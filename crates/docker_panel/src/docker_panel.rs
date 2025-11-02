use anyhow::{anyhow, Result};
use gpui::{
    actions, div, prelude::*, Action, App, AsyncApp, AsyncWindowContext, Context, Entity,
    EventEmitter, FocusHandle, Focusable, Pixels, Render, SharedString, Task, WeakEntity, Window,
};
use serde::{Deserialize, Serialize};
use ui::{h_flex, prelude::*, v_flex, Button, Icon, IconName, IconPosition, Label, Tab};
use util::ResultExt;
use workspace::{
    dock::{DockPosition, Panel, PanelEvent},
    Workspace,
};

const DOCKER_PANEL_KEY: &str = "DockerPanel";

actions!(docker_panel, [ToggleFocus]);

pub fn init(cx: &mut App) {
    log::info!("Docker panel initializing...");
    cx.observe_new(
        |workspace: &mut Workspace, _window, _cx: &mut Context<Workspace>| {
            log::info!("Docker panel registering action for workspace");
            workspace.register_action(|workspace, _: &ToggleFocus, window, cx| {
                log::info!("Docker panel toggle action triggered");
                workspace.toggle_panel_focus::<DockerPanel>(window, cx);
            });
        },
    )
    .detach();
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

impl ContainerState {
    fn icon(&self) -> IconName {
        match self {
            ContainerState::Running => IconName::PlayOutlined,
            ContainerState::Stopped => IconName::Stop,
            ContainerState::Paused => IconName::DebugPause,
            ContainerState::Unknown => IconName::Warning,
        }
    }

    fn color(&self) -> Color {
        match self {
            ContainerState::Running => Color::Success,
            ContainerState::Stopped => Color::Error,
            ContainerState::Paused => Color::Warning,
            ContainerState::Unknown => Color::Muted,
        }
    }
}

pub struct DockerPanel {
    _workspace: WeakEntity<Workspace>,
    focus_handle: FocusHandle,
    width: Option<Pixels>,
    docker_running: bool,
    containers: Vec<DockerContainer>,
    refresh_task: Option<Task<()>>,
}

impl DockerPanel {
    pub async fn load(
        workspace: WeakEntity<Workspace>,
        mut cx: AsyncWindowContext,
    ) -> anyhow::Result<Entity<Self>> {
        let panel = workspace.update(&mut cx, |_workspace, cx| {
            cx.new(|cx| {
                let focus_handle = cx.focus_handle();
                Self {
                    _workspace: workspace.clone(),
                    focus_handle,
                    width: None,
                    docker_running: false,
                    containers: Vec::new(),
                    refresh_task: None,
                }
            })
        })?;

        // Avvia il primo refresh
        panel
            .update(&mut cx, |this, cx| {
                this.refresh_docker_status(cx);
            })
            .ok();

        Ok(panel)
    }

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
                this.containers = containers;
                cx.notify();
            })
            .log_err();
        });

        self.refresh_task = Some(task);
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
}

impl Focusable for DockerPanel {
    fn focus_handle(&self, _cx: &App) -> FocusHandle {
        self.focus_handle.clone()
    }
}

impl EventEmitter<PanelEvent> for DockerPanel {}

impl Panel for DockerPanel {
    fn persistent_name() -> &'static str {
        "DockerPanel"
    }

    fn panel_key() -> &'static str {
        DOCKER_PANEL_KEY
    }

    fn position(&self, _window: &Window, _cx: &App) -> DockPosition {
        DockPosition::Right
    }

    fn position_is_valid(&self, position: DockPosition) -> bool {
        matches!(position, DockPosition::Right | DockPosition::Left)
    }

    fn set_position(
        &mut self,
        _position: DockPosition,
        _window: &mut Window,
        _cx: &mut Context<Self>,
    ) {
    }

    fn size(&self, _window: &Window, _cx: &App) -> Pixels {
        self.width.unwrap_or_else(|| Pixels::from(360.0))
    }

    fn set_size(&mut self, size: Option<Pixels>, _window: &mut Window, cx: &mut Context<Self>) {
        self.width = size;
        cx.notify();
    }

    fn icon(&self, _window: &Window, _cx: &App) -> Option<IconName> {
        Some(IconName::Server)
    }

    fn icon_tooltip(&self, _window: &Window, _cx: &App) -> Option<&'static str> {
        Some("Docker Containers")
    }

    fn toggle_action(&self) -> Box<dyn Action> {
        Box::new(ToggleFocus)
    }

    fn activation_priority(&self) -> u32 {
        100
    }
}

impl Render for DockerPanel {
    fn render(&mut self, _window: &mut Window, cx: &mut Context<Self>) -> impl IntoElement {
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
                    .child(Label::new("Docker Containers"))
                    .child(
                        Button::new("refresh", "Refresh")
                            .icon(IconName::ArrowCircle)
                            .icon_position(IconPosition::Start)
                            .on_click(cx.listener(|this, _, _window, cx| {
                                this.refresh_docker_status(cx);
                            })),
                    ),
            )
            .map(|this| {
                if !self.docker_running {
                    this.child(
                        v_flex().gap_2().p_4().child(
                            div()
                                .flex()
                                .flex_col()
                                .items_center()
                                .gap_2()
                                .child(
                                    Icon::new(IconName::Warning)
                                        .size(IconSize::Medium)
                                        .color(Color::Warning),
                                )
                                .child(Label::new("Docker non è avviato"))
                                .child(
                                    Label::new("Avvialo per vedere i container")
                                        .color(Color::Muted)
                                        .size(LabelSize::Small),
                                ),
                        ),
                    )
                } else if self.containers.is_empty() {
                    this.child(
                        v_flex().p_4().child(
                            div().flex().w_full().items_center().child(
                                Label::new("Nessun container trovato")
                                    .color(Color::Muted)
                                    .size(LabelSize::Small),
                            ),
                        ),
                    )
                } else {
                    this.child(v_flex().gap_1().p_2().children(self.containers.iter().map(
                        |container| {
                            let container_id = container.id.clone();
                            let state = container.state.clone();

                            h_flex()
                                .gap_2()
                                .p_2()
                                .rounded_md()
                                .hover(|style| style.bg(cx.theme().colors().element_hover))
                                .child(
                                    Icon::new(state.icon())
                                        .size(IconSize::Small)
                                        .color(state.color()),
                                )
                                .child(
                                    v_flex()
                                        .flex_1()
                                        .child(Label::new(container.name.clone()))
                                        .child(
                                            Label::new(format!(
                                                "{} • {}",
                                                container.image, container.status
                                            ))
                                            .size(LabelSize::Small)
                                            .color(Color::Muted),
                                        ),
                                )
                                .child(
                                    Button::new(
                                        SharedString::from(format!("toggle-{}", container.id)),
                                        if state == ContainerState::Running {
                                            "Stop"
                                        } else {
                                            "Start"
                                        },
                                    )
                                    .icon(if state == ContainerState::Running {
                                        IconName::Stop
                                    } else {
                                        IconName::PlayOutlined
                                    })
                                    .icon_position(IconPosition::Start)
                                    .on_click(cx.listener(
                                        move |this, _, _window, cx| {
                                            this.toggle_container(
                                                container_id.clone(),
                                                state.clone(),
                                                cx,
                                            );
                                        },
                                    )),
                                )
                        },
                    )))
                }
            })
    }
}

async fn check_docker_running(cx: &mut AsyncApp) -> Result<bool> {
    let output = cx
        .background_spawn(async {
            smol::process::Command::new("docker")
                .arg("info")
                .output()
                .await
        })
        .await?;

    Ok(output.status.success())
}

async fn get_docker_containers(cx: &mut AsyncApp) -> Result<Vec<DockerContainer>> {
    let output = cx
        .background_spawn(async {
            smol::process::Command::new("docker")
                .args(["ps", "-a", "--format", "{{json .}}"])
                .output()
                .await
        })
        .await?;

    if !output.status.success() {
        return Err(anyhow!("Failed to get docker containers"));
    }

    let stdout = String::from_utf8(output.stdout)?;
    let mut containers = Vec::new();

    for line in stdout.lines() {
        if line.trim().is_empty() {
            continue;
        }

        let container: serde_json::Value = serde_json::from_str(line)?;

        let status = container["Status"].as_str().unwrap_or("").to_lowercase();
        let state = if status.contains("up") {
            ContainerState::Running
        } else if status.contains("paused") {
            ContainerState::Paused
        } else if status.contains("exited") {
            ContainerState::Stopped
        } else {
            ContainerState::Unknown
        };

        containers.push(DockerContainer {
            id: container["ID"].as_str().unwrap_or("").to_string(),
            name: container["Names"].as_str().unwrap_or("").to_string(),
            image: container["Image"].as_str().unwrap_or("").to_string(),
            status: container["Status"].as_str().unwrap_or("").to_string(),
            state,
        });
    }

    Ok(containers)
}

async fn start_container(container_id: &str, cx: &mut AsyncApp) -> Result<()> {
    let container_id = container_id.to_string();
    let output = cx
        .background_spawn(async move {
            smol::process::Command::new("docker")
                .args(["start", &container_id])
                .output()
                .await
        })
        .await?;

    if !output.status.success() {
        return Err(anyhow!("Failed to start container"));
    }

    Ok(())
}

async fn stop_container(container_id: &str, cx: &mut AsyncApp) -> Result<()> {
    let container_id = container_id.to_string();
    let output = cx
        .background_spawn(async move {
            smol::process::Command::new("docker")
                .args(["stop", &container_id])
                .output()
                .await
        })
        .await?;

    if !output.status.success() {
        return Err(anyhow!("Failed to stop container"));
    }

    Ok(())
}
