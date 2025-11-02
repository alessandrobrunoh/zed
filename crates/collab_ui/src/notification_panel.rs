use crate::NotificationPanelSettings;
use anyhow::Result;
use channel::ChannelStore;
use client::{ChannelId, Client, Notification, User, UserStore};
use collections::HashMap;
use db::kvp::KEY_VALUE_STORE;
use futures::StreamExt;
use gpui::{
    AnyElement, App, AsyncWindowContext, ClickEvent, Context, DismissEvent, Element, Entity,
    EventEmitter, FocusHandle, Focusable, FontWeight, InteractiveElement, IntoElement,
    ListAlignment, ListScrollEvent, ListState, ParentElement, Render, StatefulInteractiveElement,
    Styled, Task, WeakEntity, Window, actions, div, img, px,
};
use notifications::{NotificationEntry, NotificationEvent, NotificationStore};
use project::Fs;
use rpc::proto;
use serde::{Deserialize, Serialize};
use settings::{Settings, SettingsStore};
use std::{sync::Arc, time::Duration};
use task::{RevealStrategy, SpawnInTerminal, TaskId};
use terminal_view::terminal_panel::TerminalPanel;
use time::{OffsetDateTime, UtcOffset};
use ui::{Avatar, Button, IconButton, IconName, Label, Tab, Tooltip, h_flex, prelude::*, v_flex};
use util::{ResultExt, TryFutureExt};
use workspace::notifications::{
    Notification as WorkspaceNotification, NotificationId, SuppressEvent,
};
use workspace::{
    Workspace,
    dock::{DockPosition, Panel, PanelEvent},
};
use zed_actions::RevealTarget;

const LOADING_THRESHOLD: usize = 30;
const MARK_AS_READ_DELAY: Duration = Duration::from_secs(1);
const TOAST_DURATION: Duration = Duration::from_secs(5);
const NOTIFICATION_PANEL_KEY: &str = "NotificationPanel";

pub struct NotificationPanel {
    client: Arc<Client>,
    user_store: Entity<UserStore>,
    channel_store: Entity<ChannelStore>,
    notification_store: Entity<NotificationStore>,
    fs: Arc<dyn Fs>,
    width: Option<Pixels>,
    active: bool,
    notification_list: ListState,
    pending_serialization: Task<Option<()>>,
    subscriptions: Vec<gpui::Subscription>,
    workspace: WeakEntity<Workspace>,
    current_notification_toast: Option<(u64, Task<()>)>,
    local_timezone: UtcOffset,
    focus_handle: FocusHandle,
    mark_as_read_tasks: HashMap<u64, Task<Result<()>>>,
    unseen_notifications: Vec<NotificationEntry>,
    // Docker fields
    docker_running: bool,
    docker_containers: Vec<DockerContainer>,
    docker_refresh_task: Option<Task<()>>,
    docker_event_task: Option<Task<()>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DockerContainer {
    pub id: String,
    pub name: String,
    pub image: String,
    pub status: String,
    pub state: ContainerState,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ContainerState {
    Running,
    Stopped,
    Paused,
    Unknown,
}

#[derive(Serialize, Deserialize)]
struct SerializedNotificationPanel {
    width: Option<Pixels>,
}

#[derive(Debug)]
pub enum Event {
    DockPositionChanged,
    Focus,
    Dismissed,
}

pub struct NotificationPresenter {
    pub actor: Option<Arc<client::User>>,
    pub text: String,
    pub icon: &'static str,
    pub needs_response: bool,
}

actions!(
    notification_panel,
    [
        /// Toggles focus on the notification panel.
        ToggleFocus
    ]
);

pub fn init(cx: &mut App) {
    cx.observe_new(|workspace: &mut Workspace, _, _| {
        workspace.register_action(|workspace, _: &ToggleFocus, window, cx| {
            workspace.toggle_panel_focus::<NotificationPanel>(window, cx);
        });
    })
    .detach();
}

impl NotificationPanel {
    pub fn new(
        workspace: &mut Workspace,
        window: &mut Window,
        cx: &mut Context<Workspace>,
    ) -> Entity<Self> {
        let fs = workspace.app_state().fs.clone();
        let client = workspace.app_state().client.clone();
        let user_store = workspace.app_state().user_store.clone();
        let workspace_handle = workspace.weak_handle();

        cx.new(|cx| {
            let mut status = client.status();
            cx.spawn_in(window, async move |this, cx| {
                while (status.next().await).is_some() {
                    if this
                        .update(cx, |_: &mut Self, cx| {
                            cx.notify();
                        })
                        .is_err()
                    {
                        break;
                    }
                }
            })
            .detach();

            let notification_list = ListState::new(0, ListAlignment::Top, px(1000.));
            notification_list.set_scroll_handler(cx.listener(
                |this, event: &ListScrollEvent, _, cx| {
                    if event.count.saturating_sub(event.visible_range.end) < LOADING_THRESHOLD
                        && let Some(task) = this
                            .notification_store
                            .update(cx, |store, cx| store.load_more_notifications(false, cx))
                    {
                        task.detach();
                    }
                },
            ));

            let local_offset = chrono::Local::now().offset().local_minus_utc();
            let mut this = Self {
                fs,
                client,
                user_store,
                local_timezone: UtcOffset::from_whole_seconds(local_offset).unwrap(),
                channel_store: ChannelStore::global(cx),
                notification_store: NotificationStore::global(cx),
                notification_list,
                pending_serialization: Task::ready(None),
                workspace: workspace_handle,
                focus_handle: cx.focus_handle(),
                current_notification_toast: None,
                subscriptions: Vec::new(),
                active: false,
                mark_as_read_tasks: HashMap::default(),
                width: None,
                unseen_notifications: Vec::new(),
                // Docker fields
                docker_running: false,
                docker_containers: Vec::new(),
                docker_refresh_task: None,
                docker_event_task: None,
            };

            // Avvia il refresh Docker
            this.refresh_docker_status(cx);

            let mut old_dock_position = this.position(window, cx);
            this.subscriptions.extend([
                cx.observe(&this.notification_store, |_, _, cx| cx.notify()),
                cx.subscribe_in(
                    &this.notification_store,
                    window,
                    Self::on_notification_event,
                ),
                cx.observe_global_in::<SettingsStore>(
                    window,
                    move |this: &mut Self, window, cx| {
                        let new_dock_position = this.position(window, cx);
                        if new_dock_position != old_dock_position {
                            old_dock_position = new_dock_position;
                            cx.emit(Event::DockPositionChanged);
                        }
                        cx.notify();
                    },
                ),
            ]);
            this
        })
    }

    pub fn load(
        workspace: WeakEntity<Workspace>,
        cx: AsyncWindowContext,
    ) -> Task<Result<Entity<Self>>> {
        cx.spawn(async move |cx| {
            let serialized_panel = if let Some(panel) = cx
                .background_spawn(async move { KEY_VALUE_STORE.read_kvp(NOTIFICATION_PANEL_KEY) })
                .await
                .log_err()
                .flatten()
            {
                Some(serde_json::from_str::<SerializedNotificationPanel>(&panel)?)
            } else {
                None
            };

            workspace.update_in(cx, |workspace, window, cx| {
                let panel = Self::new(workspace, window, cx);
                if let Some(serialized_panel) = serialized_panel {
                    panel.update(cx, |panel, cx| {
                        panel.width = serialized_panel.width.map(|w| w.round());
                        cx.notify();
                    });
                }
                panel
            })
        })
    }

    fn serialize(&mut self, cx: &mut Context<Self>) {
        let width = self.width;
        self.pending_serialization = cx.background_spawn(
            async move {
                KEY_VALUE_STORE
                    .write_kvp(
                        NOTIFICATION_PANEL_KEY.into(),
                        serde_json::to_string(&SerializedNotificationPanel { width })?,
                    )
                    .await?;
                anyhow::Ok(())
            }
            .log_err(),
        );
    }

    fn render_notification(
        &mut self,
        ix: usize,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) -> Option<AnyElement> {
        let entry = self.notification_store.read(cx).notification_at(ix)?;
        let notification_id = entry.id;
        let now = OffsetDateTime::now_utc();
        let timestamp = entry.timestamp;
        let NotificationPresenter {
            actor,
            text,
            needs_response,
            ..
        } = self.present_notification(entry, cx)?;

        let response = entry.response;
        let notification = entry.notification.clone();

        if self.active && !entry.is_read {
            self.did_render_notification(notification_id, &notification, window, cx);
        }

        let relative_timestamp = time_format::format_localized_timestamp(
            timestamp,
            now,
            self.local_timezone,
            time_format::TimestampFormat::Relative,
        );

        let absolute_timestamp = time_format::format_localized_timestamp(
            timestamp,
            now,
            self.local_timezone,
            time_format::TimestampFormat::Absolute,
        );

        Some(
            div()
                .id(ix)
                .flex()
                .flex_row()
                .size_full()
                .px_2()
                .py_1()
                .gap_2()
                .hover(|style| style.bg(cx.theme().colors().element_hover))
                .children(actor.map(|actor| {
                    img(actor.avatar_uri.clone())
                        .flex_none()
                        .w_8()
                        .h_8()
                        .rounded_full()
                }))
                .child(
                    v_flex()
                        .gap_1()
                        .size_full()
                        .overflow_hidden()
                        .child(Label::new(text))
                        .child(
                            h_flex()
                                .child(
                                    div()
                                        .id("notification_timestamp")
                                        .hover(|style| {
                                            style
                                                .bg(cx.theme().colors().element_selected)
                                                .rounded_sm()
                                        })
                                        .child(Label::new(relative_timestamp).color(Color::Muted))
                                        .tooltip(move |_, cx| {
                                            Tooltip::simple(absolute_timestamp.clone(), cx)
                                        }),
                                )
                                .children(if let Some(is_accepted) = response {
                                    Some(div().flex().flex_grow().justify_end().child(Label::new(
                                        if is_accepted {
                                            "You accepted"
                                        } else {
                                            "You declined"
                                        },
                                    )))
                                } else if needs_response {
                                    Some(
                                        h_flex()
                                            .flex_grow()
                                            .justify_end()
                                            .child(Button::new("decline", "Decline").on_click({
                                                let notification = notification.clone();
                                                let entity = cx.entity();
                                                move |_, _, cx| {
                                                    entity.update(cx, |this, cx| {
                                                        this.respond_to_notification(
                                                            notification.clone(),
                                                            false,
                                                            cx,
                                                        )
                                                    });
                                                }
                                            }))
                                            .child(Button::new("accept", "Accept").on_click({
                                                let notification = notification.clone();
                                                let entity = cx.entity();
                                                move |_, _, cx| {
                                                    entity.update(cx, |this, cx| {
                                                        this.respond_to_notification(
                                                            notification.clone(),
                                                            true,
                                                            cx,
                                                        )
                                                    });
                                                }
                                            })),
                                    )
                                } else {
                                    None
                                }),
                        ),
                )
                .into_any(),
        )
    }

    fn present_notification(
        &self,
        entry: &NotificationEntry,
        cx: &App,
    ) -> Option<NotificationPresenter> {
        let user_store = self.user_store.read(cx);
        let channel_store = self.channel_store.read(cx);
        match entry.notification {
            Notification::ContactRequest { sender_id } => {
                let requester = user_store.get_cached_user(sender_id)?;
                Some(NotificationPresenter {
                    icon: "icons/plus.svg",
                    text: format!("{} wants to add you as a contact", requester.github_login),
                    needs_response: user_store.has_incoming_contact_request(requester.id),
                    actor: Some(requester),
                })
            }
            Notification::ContactRequestAccepted { responder_id } => {
                let responder = user_store.get_cached_user(responder_id)?;
                Some(NotificationPresenter {
                    icon: "icons/plus.svg",
                    text: format!("{} accepted your contact invite", responder.github_login),
                    needs_response: false,
                    actor: Some(responder),
                })
            }
            Notification::ChannelInvitation {
                ref channel_name,
                channel_id,
                inviter_id,
            } => {
                let inviter = user_store.get_cached_user(inviter_id)?;
                Some(NotificationPresenter {
                    icon: "icons/hash.svg",
                    text: format!(
                        "{} invited you to join the #{channel_name} channel",
                        inviter.github_login
                    ),
                    needs_response: channel_store.has_channel_invitation(ChannelId(channel_id)),
                    actor: Some(inviter),
                })
            }
        }
    }

    fn did_render_notification(
        &mut self,
        notification_id: u64,
        notification: &Notification,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        let should_mark_as_read = match notification {
            Notification::ContactRequestAccepted { .. } => true,
            Notification::ContactRequest { .. } | Notification::ChannelInvitation { .. } => false,
        };

        if should_mark_as_read {
            self.mark_as_read_tasks
                .entry(notification_id)
                .or_insert_with(|| {
                    let client = self.client.clone();
                    cx.spawn_in(window, async move |this, cx| {
                        cx.background_executor().timer(MARK_AS_READ_DELAY).await;
                        client
                            .request(proto::MarkNotificationRead { notification_id })
                            .await?;
                        this.update(cx, |this, _| {
                            this.mark_as_read_tasks.remove(&notification_id);
                        })?;
                        Ok(())
                    })
                });
        }
    }

    fn on_notification_event(
        &mut self,
        _: &Entity<NotificationStore>,
        event: &NotificationEvent,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        match event {
            NotificationEvent::NewNotification { entry } => {
                self.unseen_notifications.push(entry.clone());
                self.add_toast(entry, window, cx);
            }
            NotificationEvent::NotificationRemoved { entry }
            | NotificationEvent::NotificationRead { entry } => {
                self.unseen_notifications.retain(|n| n.id != entry.id);
                self.remove_toast(entry.id, cx);
            }
            NotificationEvent::NotificationsUpdated {
                old_range,
                new_count,
            } => {
                self.notification_list.splice(old_range.clone(), *new_count);
                cx.notify();
            }
        }
    }

    fn add_toast(
        &mut self,
        entry: &NotificationEntry,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        let Some(NotificationPresenter { actor, text, .. }) = self.present_notification(entry, cx)
        else {
            return;
        };

        let notification_id = entry.id;
        self.current_notification_toast = Some((
            notification_id,
            cx.spawn_in(window, async move |this, cx| {
                cx.background_executor().timer(TOAST_DURATION).await;
                this.update(cx, |this, cx| this.remove_toast(notification_id, cx))
                    .ok();
            }),
        ));

        self.workspace
            .update(cx, |workspace, cx| {
                let id = NotificationId::unique::<NotificationToast>();

                workspace.dismiss_notification(&id, cx);
                workspace.show_notification(id, cx, |cx| {
                    let workspace = cx.entity().downgrade();
                    cx.new(|cx| NotificationToast {
                        actor,
                        text,
                        workspace,
                        focus_handle: cx.focus_handle(),
                    })
                })
            })
            .ok();
    }

    fn remove_toast(&mut self, notification_id: u64, cx: &mut Context<Self>) {
        if let Some((current_id, _)) = &self.current_notification_toast
            && *current_id == notification_id
        {
            self.current_notification_toast.take();
            self.workspace
                .update(cx, |workspace, cx| {
                    let id = NotificationId::unique::<NotificationToast>();
                    workspace.dismiss_notification(&id, cx)
                })
                .ok();
        }
    }

    fn respond_to_notification(
        &mut self,
        notification: Notification,
        response: bool,

        cx: &mut Context<Self>,
    ) {
        self.notification_store.update(cx, |store, cx| {
            store.respond_to_notification(notification, response, cx);
        });
    }

    // Docker methods
    fn refresh_docker_status(&mut self, cx: &mut Context<Self>) {
        let task = cx.spawn(async move |this, cx| {
            let docker_running = check_docker_running(cx).await.unwrap_or(false);
            let containers = if docker_running {
                get_docker_containers(cx).await.unwrap_or_default()
            } else {
                Vec::new()
            };

            this.update(cx, |this, cx| {
                this.docker_running = docker_running;
                this.docker_containers = containers;
                cx.notify();
            })
            .log_err();
        });

        self.docker_refresh_task = Some(task);

        // Avvia il monitoraggio eventi solo se non è già attivo
        if self.docker_event_task.is_none() {
            self.start_docker_event_monitoring(cx);
        }
    }

    fn start_docker_event_monitoring(&mut self, cx: &mut Context<Self>) {
        let task = cx.spawn(async move |this, mut cx| {
            loop {
                if let Ok(()) = monitor_docker_events(this.clone(), cx.clone()).await {
                    // Se il monitoring termina normalmente, aspetta un po' prima di riavviare
                    smol::Timer::after(std::time::Duration::from_secs(5)).await;
                } else {
                    // Se c'è un errore, aspetta più a lungo
                    smol::Timer::after(std::time::Duration::from_secs(10)).await;
                }
            }
        });

        self.docker_event_task = Some(task);
    }

    fn toggle_container(
        &mut self,
        container_id: String,
        current_state: ContainerState,
        cx: &mut Context<Self>,
    ) {
        let task = cx.spawn(async move |this, cx| {
            let result = if current_state == ContainerState::Running {
                stop_container(&container_id, cx).await
            } else {
                start_container(&container_id, cx).await
            };

            if let Err(err) = result {
                log::error!("Failed to toggle container: {}", err);
            }

            this.update(cx, |this, cx| {
                this.refresh_docker_status(cx);
            })
            .log_err();
        });

        task.detach();
    }

    fn view_container_logs(
        &mut self,
        container_id: String,
        container_name: String,
        window: &mut Window,
        cx: &mut Context<Self>,
    ) {
        log::info!(
            "Opening logs for container: {} ({})",
            container_name,
            container_id
        );

        let workspace = self.workspace.clone();

        let result = workspace.update(cx, |workspace, cx| {
            log::info!("Getting terminal panel...");

            let terminal_panel = workspace.panel::<TerminalPanel>(cx);

            if terminal_panel.is_none() {
                log::error!("Terminal panel not found!");
                return None;
            }

            let terminal_panel = terminal_panel?;

            log::info!("Creating terminal task...");

            let task = SpawnInTerminal {
                id: TaskId(format!("docker-logs-{}", container_id)),
                full_label: format!("Docker Logs: {}", container_name),
                label: format!("Logs: {}", container_name),
                command: Some("docker".to_string()),
                args: vec![
                    "logs".to_string(),
                    "-f".to_string(),
                    "--tail".to_string(),
                    "100".to_string(),
                    "--timestamps".to_string(),
                    container_id.clone(),
                ],
                command_label: format!("docker logs -f --tail 100 --timestamps {}", container_name),
                cwd: None,
                use_new_terminal: true,
                allow_concurrent_runs: true,
                reveal: RevealStrategy::Always,
                reveal_target: RevealTarget::default(),
                env: Default::default(),
                shell: Default::default(),
                hide: Default::default(),
                show_summary: false,
                show_command: false,
                show_rerun: false,
            };

            log::info!("Adding terminal task...");

            let task_result = terminal_panel.update(cx, |panel, cx| {
                panel.add_terminal_task(task, RevealStrategy::Always, window, cx)
            });

            task_result.detach();
            log::info!("Terminal task added successfully");

            Some(())
        });

        if let Err(e) = result {
            log::error!("Failed to update workspace: {}", e);
        }
    }
}

impl Render for NotificationPanel {
    fn render(&mut self, _: &mut Window, cx: &mut Context<Self>) -> impl IntoElement {
        v_flex()
            .size_full()
            .child(
                h_flex()
                    .justify_between()
                    .px_2()
                    .py_1()
                    .h(Tab::container_height(cx))
                    .border_b_1()
                    .border_color(cx.theme().colors().border)
                    .child(Label::new("Notifiche"))
                    .child(
                        IconButton::new("refresh_docker", IconName::ArrowCircle).on_click(
                            cx.listener(|this, _, _, cx| {
                                this.refresh_docker_status(cx);
                            }),
                        ),
                    ),
            )
            .map(|this| {
                if !self.docker_running {
                    this.child(
                        v_flex().p_4().gap_2().child(
                            div().flex().w_full().items_center().child(
                                Label::new("Docker non è avviato. Avvialo per vedere i container.")
                                    .color(Color::Muted)
                                    .size(LabelSize::Small),
                            ),
                        ),
                    )
                } else if self.docker_containers.is_empty() {
                    this.child(
                        v_flex().p_4().gap_2().child(
                            div().flex().w_full().items_center().child(
                                Label::new("Nessun container trovato.")
                                    .color(Color::Muted)
                                    .size(LabelSize::Small),
                            ),
                        ),
                    )
                } else {
                    let mut container_list = v_flex().size_full().p_2().gap_3();

                    for (idx, container) in self.docker_containers.iter().enumerate() {
                        let container_id = container.id.clone();
                        let container_id_for_logs = container.id.clone();
                        let container_name = container.name.clone();
                        let current_state = container.state.clone();
                        let is_running = current_state == ContainerState::Running;
                        let button_text = if is_running { "Stop" } else { "Start" };
                        let status_color = if is_running {
                            Color::Success
                        } else {
                            Color::Muted
                        };
                        let status_icon = if is_running {
                            IconName::Check
                        } else {
                            IconName::XCircle
                        };

                        container_list = container_list.child(
                            v_flex()
                                .p_3()
                                .gap_2()
                                .border_1()
                                .border_color(cx.theme().colors().border)
                                .rounded_lg()
                                .bg(cx.theme().colors().element_background)
                                .hover(|style| style.bg(cx.theme().colors().element_hover))
                                .child(
                                    h_flex()
                                        .justify_between()
                                        .items_center()
                                        .child(
                                            h_flex()
                                                .gap_2()
                                                .items_center()
                                                .child(
                                                    div().child(
                                                        ui::Icon::new(status_icon)
                                                            .size(ui::IconSize::Small)
                                                            .color(status_color),
                                                    ),
                                                )
                                                .child(
                                                    Label::new(container.name.clone())
                                                        .weight(FontWeight::BOLD)
                                                        .size(LabelSize::Default),
                                                ),
                                        )
                                        .child(
                                            Label::new(container.status.clone())
                                                .color(status_color)
                                                .size(LabelSize::Small),
                                        ),
                                )
                                .child(
                                    v_flex()
                                        .gap_1()
                                        .child(
                                            Label::new(format!("Image: {}", container.image))
                                                .size(LabelSize::Small)
                                                .color(Color::Muted),
                                        )
                                        .child(
                                            Label::new(format!(
                                                "ID: {}",
                                                &container.id[..12.min(container.id.len())]
                                            ))
                                            .size(LabelSize::XSmall)
                                            .color(Color::Muted),
                                        ),
                                )
                                .child(
                                    v_flex()
                                        .gap_2()
                                        .child(
                                            Button::new(("toggle", idx), button_text)
                                                .style(ButtonStyle::Filled)
                                                .full_width()
                                                .on_click(cx.listener(move |this, _, _, cx| {
                                                    this.toggle_container(
                                                        container_id.clone(),
                                                        current_state.clone(),
                                                        cx,
                                                    );
                                                })),
                                        )
                                        .child(
                                            Button::new(("logs", idx), "Logs")
                                                .style(ButtonStyle::Subtle)
                                                .full_width()
                                                .on_click(cx.listener(
                                                    move |this, _event, _window, cx| {
                                                        this.view_container_logs(
                                                            container_id_for_logs.clone(),
                                                            container_name.clone(),
                                                            _window,
                                                            cx,
                                                        );
                                                    },
                                                )),
                                        ),
                                ),
                        );
                    }

                    this.child(container_list)
                }
            })
    }
}

impl Focusable for NotificationPanel {
    fn focus_handle(&self, _: &App) -> FocusHandle {
        self.focus_handle.clone()
    }
}

impl EventEmitter<Event> for NotificationPanel {}
impl EventEmitter<PanelEvent> for NotificationPanel {}

impl Panel for NotificationPanel {
    fn persistent_name() -> &'static str {
        "NotificationPanel"
    }

    fn panel_key() -> &'static str {
        NOTIFICATION_PANEL_KEY
    }

    fn position(&self, _: &Window, cx: &App) -> DockPosition {
        NotificationPanelSettings::get_global(cx).dock
    }

    fn position_is_valid(&self, position: DockPosition) -> bool {
        matches!(position, DockPosition::Left | DockPosition::Right)
    }

    fn set_position(&mut self, position: DockPosition, _: &mut Window, cx: &mut Context<Self>) {
        settings::update_settings_file(self.fs.clone(), cx, move |settings, _| {
            settings.notification_panel.get_or_insert_default().dock = Some(position.into())
        });
    }

    fn size(&self, _: &Window, cx: &App) -> Pixels {
        self.width
            .unwrap_or_else(|| NotificationPanelSettings::get_global(cx).default_width)
    }

    fn set_size(&mut self, size: Option<Pixels>, _: &mut Window, cx: &mut Context<Self>) {
        self.width = size;
        self.serialize(cx);
        cx.notify();
    }

    fn set_active(&mut self, active: bool, _: &mut Window, cx: &mut Context<Self>) {
        self.active = active;

        if self.active {
            self.unseen_notifications = Vec::new();
            cx.notify();
        }

        if self.notification_store.read(cx).notification_count() == 0 {
            cx.emit(Event::Dismissed);
        }
    }

    fn icon(&self, _: &Window, cx: &App) -> Option<IconName> {
        let show_button = NotificationPanelSettings::get_global(cx).button;
        if !show_button {
            return None;
        }

        if self.unseen_notifications.is_empty() {
            return Some(IconName::Bell);
        }

        Some(IconName::BellDot)
    }

    fn icon_tooltip(&self, _window: &Window, _cx: &App) -> Option<&'static str> {
        Some("Notification Panel")
    }

    fn icon_label(&self, _window: &Window, cx: &App) -> Option<String> {
        let count = self.notification_store.read(cx).unread_notification_count();
        if count == 0 {
            None
        } else {
            Some(count.to_string())
        }
    }

    fn toggle_action(&self) -> Box<dyn gpui::Action> {
        Box::new(ToggleFocus)
    }

    fn activation_priority(&self) -> u32 {
        8
    }
}

pub struct NotificationToast {
    actor: Option<Arc<User>>,
    text: String,
    workspace: WeakEntity<Workspace>,
    focus_handle: FocusHandle,
}

impl Focusable for NotificationToast {
    fn focus_handle(&self, _cx: &App) -> FocusHandle {
        self.focus_handle.clone()
    }
}

impl WorkspaceNotification for NotificationToast {}

impl NotificationToast {
    fn focus_notification_panel(&self, window: &mut Window, cx: &mut Context<Self>) {
        let workspace = self.workspace.clone();
        window.defer(cx, move |window, cx| {
            workspace
                .update(cx, |workspace, cx| {
                    workspace.focus_panel::<NotificationPanel>(window, cx)
                })
                .ok();
        })
    }
}

impl Render for NotificationToast {
    fn render(&mut self, window: &mut Window, cx: &mut Context<Self>) -> impl IntoElement {
        let user = self.actor.clone();

        let suppress = window.modifiers().shift;
        let (close_id, close_icon) = if suppress {
            ("suppress", IconName::Minimize)
        } else {
            ("close", IconName::Close)
        };

        h_flex()
            .id("notification_panel_toast")
            .elevation_3(cx)
            .p_2()
            .justify_between()
            .children(user.map(|user| Avatar::new(user.avatar_uri.clone())))
            .child(Label::new(self.text.clone()))
            .on_modifiers_changed(cx.listener(|_, _, _, cx| cx.notify()))
            .child(
                IconButton::new(close_id, close_icon)
                    .tooltip(move |_window, cx| {
                        if suppress {
                            Tooltip::for_action(
                                "Suppress.\nClose with click.",
                                &workspace::SuppressNotification,
                                cx,
                            )
                        } else {
                            Tooltip::for_action(
                                "Close.\nSuppress with shift-click",
                                &menu::Cancel,
                                cx,
                            )
                        }
                    })
                    .on_click(cx.listener(move |_, _: &ClickEvent, _, cx| {
                        if suppress {
                            cx.emit(SuppressEvent);
                        } else {
                            cx.emit(DismissEvent);
                        }
                    })),
            )
            .on_click(cx.listener(|this, _, window, cx| {
                this.focus_notification_panel(window, cx);
                cx.emit(DismissEvent);
            }))
    }
}

impl EventEmitter<DismissEvent> for NotificationToast {}
impl EventEmitter<SuppressEvent> for NotificationToast {}

// Docker helper functions using Docker CLI

async fn check_docker_running(_cx: &mut gpui::AsyncApp) -> anyhow::Result<bool> {
    let output = smol::process::Command::new("docker")
        .arg("info")
        .arg("--format")
        .arg("{{json .}}")
        .output()
        .await;

    match output {
        Ok(output) => Ok(output.status.success()),
        Err(_) => Ok(false),
    }
}

async fn get_docker_containers(_cx: &mut gpui::AsyncApp) -> anyhow::Result<Vec<DockerContainer>> {
    let output = smol::process::Command::new("docker")
        .args(["ps", "-a", "--format", "{{json .}}"])
        .output()
        .await?;

    if !output.status.success() {
        return Ok(Vec::new());
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    let mut containers = Vec::new();

    for line in stdout.lines() {
        if line.trim().is_empty() {
            continue;
        }

        if let Ok(value) = serde_json::from_str::<serde_json::Value>(line) {
            let state_str = value["State"].as_str().unwrap_or("unknown").to_lowercase();
            let state = match state_str.as_str() {
                "running" => ContainerState::Running,
                "exited" | "stopped" => ContainerState::Stopped,
                "paused" => ContainerState::Paused,
                _ => ContainerState::Unknown,
            };

            let name = value["Names"]
                .as_str()
                .unwrap_or("unnamed")
                .trim_start_matches('/')
                .to_string();

            containers.push(DockerContainer {
                id: value["ID"].as_str().unwrap_or("").to_string(),
                name,
                image: value["Image"].as_str().unwrap_or("").to_string(),
                status: value["Status"].as_str().unwrap_or("").to_string(),
                state,
            });
        }
    }

    Ok(containers)
}

async fn start_container(container_id: &str, _cx: &mut gpui::AsyncApp) -> anyhow::Result<()> {
    let output = smol::process::Command::new("docker")
        .args(["start", container_id])
        .output()
        .await?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        anyhow::bail!("Failed to start container: {}", stderr);
    }

    Ok(())
}

async fn stop_container(container_id: &str, _cx: &mut gpui::AsyncApp) -> anyhow::Result<()> {
    let output = smol::process::Command::new("docker")
        .args(["stop", container_id])
        .output()
        .await?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        anyhow::bail!("Failed to stop container: {}", stderr);
    }

    Ok(())
}

async fn monitor_docker_events(
    panel: WeakEntity<NotificationPanel>,
    mut cx: gpui::AsyncApp,
) -> anyhow::Result<()> {
    use futures::AsyncBufReadExt;
    use futures::io::BufReader;

    let mut child = smol::process::Command::new("docker")
        .args([
            "events",
            "--format",
            "{{json .}}",
            "--filter",
            "type=container",
        ])
        .stdout(std::process::Stdio::piped())
        .spawn()?;

    if let Some(stdout) = child.stdout.take() {
        let reader = BufReader::new(stdout);
        let mut lines = reader.lines();

        while let Some(line) = lines.next().await {
            match line {
                Ok(line) => {
                    if let Ok(event) = serde_json::from_str::<serde_json::Value>(&line) {
                        if let Some(action) = event["Action"].as_str() {
                            if action == "start"
                                || action == "stop"
                                || action == "die"
                                || action == "pause"
                                || action == "unpause"
                                || action == "kill"
                            {
                                panel
                                    .update(&mut cx, |this, cx| {
                                        this.refresh_docker_status(cx);
                                    })
                                    .log_err();
                            }
                        }
                    }
                }
                Err(e) => {
                    log::error!("Docker event stream error: {}", e);
                    return Err(anyhow::anyhow!("Event stream error: {}", e));
                }
            }
        }
    }

    Ok(())
}
