use anyhow::Result;
use db::kvp::KEY_VALUE_STORE;
use gpui::{
    actions, div, list, px, AnyElement, App, AsyncWindowContext, ClickEvent, Context,
    Element, Entity, EventEmitter, FocusHandle, Focusable, InteractiveElement,
    IntoElement, ListAlignment, ListState, ParentElement, Render, StatefulInteractiveElement,
    Styled, Task, WeakEntity, Window,
};
use project::{search::SearchQuery, search::SearchResult, WorktreeId};
use serde::{Deserialize, Serialize};
use text::ToPoint;
use ui::{
    h_flex, prelude::*, v_flex, Icon, IconButton, IconName, Label, Tab, Tooltip,
};
use util::{rel_path::RelPathBuf, ResultExt};
use workspace::{
    dock::{DockPosition, Panel, PanelEvent},
    Workspace,
};

const TODO_PANEL_KEY: &str = "TodoPanel";

pub struct TodoPanel {
    width: Option<Pixels>,
    active: bool,
    todo_list: ListState,
    todos: Vec<TodoItem>,
    workspace: WeakEntity<Workspace>,
    focus_handle: FocusHandle,
    is_searching: bool,
    search_task: Task<()>,
}

#[derive(Clone, Debug)]
struct TodoItem {
    file_path: RelPathBuf,
    worktree_id: WorktreeId,
    line_number: usize,
    text: String,
}

#[derive(Serialize, Deserialize)]
struct SerializedTodoPanel {
    width: Option<Pixels>,
}

#[derive(Debug)]
pub enum Event {
    DockPositionChanged,
    Focus,
    Dismissed,
}

actions!(
    todo_panel,
    [
        /// Toggles focus on the TODO panel.
        ToggleFocus,
        /// Refreshes the TODO list.
        RefreshTodos
    ]
);

pub fn init(cx: &mut App) {
    cx.observe_new(|workspace: &mut Workspace, _, _| {
        workspace.register_action(|workspace, _: &ToggleFocus, window, cx| {
            workspace.toggle_panel_focus::<TodoPanel>(window, cx);
        });
        workspace.register_action(|workspace, _: &RefreshTodos, window, cx| {
            if let Some(panel) = workspace.panel::<TodoPanel>(cx) {
                panel.update(cx, |panel, cx| {
                    panel.search_todos(window, cx);
                });
            }
        });
    })
    .detach();
}

impl TodoPanel {
    pub fn new(
        workspace: &mut Workspace,
        window: &mut Window,
        cx: &mut Context<Workspace>,
    ) -> Entity<Self> {
        let focus_handle = cx.focus_handle();
        let workspace_handle = workspace.weak_handle();

        cx.new(|cx: &mut Context<Self>| {
            let mut panel = Self {
                width: None,
                active: false,
                todo_list: ListState::new(
                    0,
                    ListAlignment::Top,
                    px(1000.),
                ),
                todos: Vec::new(),
                workspace: workspace_handle,
                focus_handle,
                is_searching: false,
                search_task: Task::ready(()),
            };

            panel.search_todos(window, cx);
            panel
        })
    }

    pub fn load(
        workspace: WeakEntity<Workspace>,
        cx: AsyncWindowContext,
    ) -> Task<Result<Entity<Self>>> {
        cx.spawn(async move |cx| {
            let serialized_panel = if let Some(panel) = cx
                .background_spawn(async move { KEY_VALUE_STORE.read_kvp(TODO_PANEL_KEY) })
                .await
                .log_err()
                .flatten()
            {
                Some(serde_json::from_str::<SerializedTodoPanel>(&panel.clone())?)
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

    fn search_todos(&mut self, _window: &mut Window, cx: &mut Context<Self>) {
        self.is_searching = true;
        self.todos.clear();
        self.todo_list.reset(0);
        cx.notify();

        // Get the project and start the search
        let Some(workspace) = self.workspace.upgrade() else {
            self.is_searching = false;
            cx.notify();
            return;
        };

        let search_rx = workspace.update(cx, |workspace, cx| {
            let project = workspace.project().clone();
            project.update(cx, |project, cx| {
                // Create a regex search query for TODO comments
                // Matches: TODO:, FIXME:, HACK:, NOTE:, BUG:, XXX:
                let query = SearchQuery::regex(
                    r"(TODO:|FIXME:|HACK:|NOTE:|BUG:|XXX:).*",
                    false, // whole_word
                    false, // case_sensitive
                    false, // include_ignored
                    true,  // one_match_per_line
                    Default::default(), // files_to_include
                    Default::default(), // files_to_exclude
                    false, // match_full_paths
                    None,  // buffers (search all files)
                )
                .expect("Failed to create search query");

                project.search(query, cx)
            })
        });

        // Process search results asynchronously
        self.search_task = cx.spawn(async move |this, cx| {
            let mut all_todos = Vec::new();

            // Collect all search results
            while let Ok(result) = search_rx.recv().await {
                match result {
                    SearchResult::Buffer { buffer, ranges } => {
                        // Process each buffer with matches
                        let buffer_todos = buffer
                            .update(cx, |buffer, cx| {
                                let snapshot = buffer.snapshot();
                                let file_path = buffer
                                    .file()
                                    .map(|f| f.path().as_ref().to_rel_path_buf());
                                let worktree_id = buffer.file().map(|f| f.worktree_id(cx));

                                let mut todos = Vec::new();

                                if let (Some(file_path), Some(worktree_id)) = (file_path, worktree_id) {
                                    for range in ranges {
                                        let start_point = range.start.to_point(&snapshot);
                                        let line_number = (start_point.row + 1) as usize; // 1-indexed for display

                                        // Get the full line text
                                        let line_start = snapshot.anchor_before(text::Point::new(start_point.row, 0));
                                        let line_end = if start_point.row + 1 < snapshot.max_point().row {
                                            snapshot.anchor_after(text::Point::new(start_point.row + 1, 0))
                                        } else {
                                            snapshot.anchor_after(snapshot.max_point())
                                        };

                                        let line_text = snapshot
                                            .text_for_range(line_start..line_end)
                                            .collect::<String>()
                                            .trim()
                                            .to_string();

                                        todos.push(TodoItem {
                                            file_path: file_path.clone(),
                                            worktree_id,
                                            line_number,
                                            text: line_text,
                                        });
                                    }
                                }

                                todos
                            })
                            .ok();

                        if let Some(buffer_todos) = buffer_todos {
                            all_todos.extend(buffer_todos);
                        }
                    }
                    SearchResult::LimitReached => {
                        // Search limit reached, stop collecting
                        break;
                    }
                }
            }

            // Update the panel with the collected TODOs
            _ = this.update(cx, |this, cx| {
                this.todos = all_todos;
                this.todo_list.reset(this.todos.len());
                this.is_searching = false;
                cx.notify();
            });
        });
    }

    fn render_todo_item(
        &mut self,
        ix: usize,
        _window: &mut Window,
        cx: &mut Context<Self>,
    ) -> Option<AnyElement> {
        let todo = self.todos.get(ix)?.clone();
        let file_name = todo
            .file_path
            .file_name()
            .unwrap_or("unknown")
            .to_string();
        let line_number = todo.line_number;

        Some(
            div()
                .id(("todo-item", ix))
                .px_2()
                .py_1()
                .border_b_1()
                .border_color(cx.theme().colors().border)
                .hover(|style| style.bg(cx.theme().colors().element_hover))
                .cursor_pointer()
                .on_click(cx.listener(move |this, _: &ClickEvent, window, cx| {
                    let Some(workspace) = this.workspace.upgrade() else {
                        return;
                    };

                    workspace.update(cx, |workspace, cx| {
                        let project_path = project::ProjectPath {
                            worktree_id: todo.worktree_id,
                            path: todo.file_path.clone().into(),
                        };

                        // Open the file
                        // TODO: Navigate to the specific line after opening
                        workspace.open_path(project_path, None, true, window, cx)
                            .detach_and_log_err(cx);
                    });
                }))
                .child(
                    v_flex()
                        .gap_1()
                        .child(
                            h_flex()
                                .gap_2()
                                .child(
                                    Icon::new(IconName::File)
                                        .size(IconSize::Small)
                                        .color(Color::Muted),
                                )
                                .child(
                                    Label::new(format!("{}:{}", file_name, line_number))
                                        .size(LabelSize::Small)
                                        .color(Color::Muted),
                                ),
                        )
                        .child(Label::new(todo.text).size(LabelSize::Small)),
                )
                .into_any(),
        )
    }
}

impl Render for TodoPanel {
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
                    .child(Label::new("TODO List"))
                    .child(
                        h_flex()
                            .gap_2()
                            .child(
                                Label::new(format!("{} items", self.todos.len()))
                                    .size(LabelSize::Small)
                                    .color(Color::Muted),
                            )
                            .child(
                                IconButton::new("refresh_todos", IconName::ArrowCircle)
                                    .icon_size(IconSize::Small)
                                    .on_click(cx.listener(|this, _, window, cx| {
                                        this.search_todos(window, cx);
                                    }))
                                    .tooltip(|window, cx| {
                                        Tooltip::text("Refresh TODO list")(window, cx)
                                    }),
                            ),
                    ),
            )
            .child({
                let content = if self.is_searching {
                v_flex()
                    .p_4()
                    .child(
                        div()
                            .flex()
                            .w_full()
                            .items_center()
                            .justify_center()
                            .child(
                                Label::new("Searching for TODOs...")
                                    .color(Color::Muted)
                                    .size(LabelSize::Small),
                            ),
                    )
                    .into_any()
            } else if self.todos.is_empty() {
                v_flex()
                    .p_4()
                    .child(
                        div()
                            .flex()
                            .w_full()
                            .items_center()
                            .justify_center()
                            .child(
                                Label::new("No TODOs found in the project.")
                                    .color(Color::Muted)
                                    .size(LabelSize::Small),
                            ),
                    )
                    .into_any()
            } else {
                list(
                    self.todo_list.clone(),
                    cx.processor(|this, ix, window, cx| {
                        this.render_todo_item(ix, window, cx)
                            .unwrap_or_else(|| div().into_any())
                    }),
                )
                .size_full()
                .into_any()
            };
            content
            })
    }
}

impl Focusable for TodoPanel {
    fn focus_handle(&self, _: &App) -> FocusHandle {
        self.focus_handle.clone()
    }
}

impl EventEmitter<Event> for TodoPanel {}
impl EventEmitter<PanelEvent> for TodoPanel {}

impl Panel for TodoPanel {
    fn persistent_name() -> &'static str {
        "TodoPanel"
    }

    fn panel_key() -> &'static str {
        "TodoPanel"
    }

    fn position(&self, _: &Window, _cx: &App) -> DockPosition {
        DockPosition::Right
    }

    fn position_is_valid(&self, _: DockPosition) -> bool {
        true
    }

    fn set_position(&mut self, _position: DockPosition, _: &mut Window, cx: &mut Context<Self>) {
        cx.emit(Event::DockPositionChanged);
    }

    fn size(&self, _: &Window, _cx: &App) -> Pixels {
        self.width.unwrap_or(px(400.))
    }

    fn set_size(&mut self, size: Option<Pixels>, _: &mut Window, cx: &mut Context<Self>) {
        self.width = size;
        cx.notify();
    }

    fn set_active(&mut self, active: bool, window: &mut Window, cx: &mut Context<Self>) {
        self.active = active;
        if active {
            self.search_todos(window, cx);
        }
    }

    fn icon(&self, _: &Window, _cx: &App) -> Option<IconName> {
        Some(IconName::ListTodo)
    }

    fn icon_tooltip(&self, _: &Window, _cx: &App) -> Option<&'static str> {
        Some("TODO List")
    }

    fn icon_label(&self, _: &Window, _cx: &App) -> Option<String> {
        if self.todos.is_empty() {
            None
        } else {
            Some(self.todos.len().to_string())
        }
    }

    fn toggle_action(&self) -> Box<dyn gpui::Action> {
        Box::new(ToggleFocus)
    }

    fn activation_priority(&self) -> u32 {
        2
    }
}
