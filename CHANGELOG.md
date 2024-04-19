### Unreleased

- New features

  - An xref adapter to create Citre backend from an xref backend.
  - An eglot backend based on the adapter.
  - `citre-query-*` commands to find definitions/references for a user inputted
    symbol. See their docstrings for details.

- Improvements

  - New user options: `citre-identifier-list-backends` and
    `citre-auto-enable-citre-mode-backend-test-timeout`.
  - `company-docsig` and `company-kind` information are offerd by tags backend,
    so company (and corfu, kind-icon, and maybe other packages...) users could
    see more help information.
  - Tags file creating/updating tool is largely rewritten, with rarely used
    features removed, and the hacky "ctags options embedded in tags file"
    scheme is replaced by option files supported by Universal Ctags.
  - Global backend now works over TRAMP.
  - Documentations are updated to follow up the current status of Citre.
  - Other minor improvements.

- Fixes

  - Various bugfixes.

### 0.3.1 (released 2022-10-25)

- Improvements

  - `citre-global`: Now the global backend supports Imenu.

### 0.3 (released 2022-10-19)

- New features

  - Citre now takes a pluggable backend design, and the GNU Global backend now
    works for auto-completion, finding definitions and references.

### 0.2.1 (released 2022-05-23)

- Improvements

  - `citre-global`: references are sorted by nearness. See the `--nearness`
    option in global for details.
  - New user option: `citre-gtags-args`.
  - Basic asynchronous infrastructure is added for future features. The current
    synchronous process API is also rebuilt on it, making it less hachy and
    more robust.
  - Minor improvements of C language support.
  - Other Minor improvements.

- Fixes

  - Various bugfixes

## 0.2 (released 2021-09-27)

### Features

- `citre-global` is a GNU Global plugin that can find references using xref, or
  UI similar to `citre-jump` and `citre-peek`.

### 0.1.3 (released 2021-09-27)

- Various bugfixes and minor improvements.
- Some refactoring for the upcoming GNU Global plugin.

### 0.1.2 (released 2021-09-23)

- Improvements

  - New user option: `citre-auto-enable-citre-mode-modes`.
  - New user option: `citre-update-tags-file-when-no-definitions`.
  - An empty keymap, `citre-mode-map` for `citre-mode` for the user to
    customize.
  - `citre-peek`: Fringes are now filled by vertical borders for the peek
    window. The user option `citre-peek-fill-fringe` controls this behavior.
  - Imenu: Qualified tags (tags like `class.method` or `struct::member`)and
    reference tags are in their own categories now.
  - Various little improvements

- Fixes

  - Various bugfixes from the core to the UI.

- Language specific improvements

  - C: More accurate definition sorting for names after "struct", "union" or
    "enum".

### 0.1.1 (released 2021-08-02)

- Improvements

  - New user option: `citre-imenu-create-tags-file-threshold`.
  - New language support: (System) Verilog.
  - When definitions can't be found for a symbol, Citre will ask if you want to
    update the tags file and search again.
  - Scope information is shown for definitions. Now the annotation looks like
    `(kind/type@scope)`.
  - Various little improvements.

- Fixes

  - Various bugfixes for tags file generating/updating.

## 0.1 (released 2021-07-15)

### Tools

- Ctags: Tags file creating & updating.
- capf, xref, imenu: Integration with Emacs built-in tools.
- `citre-peek`: Deep code reading in a peek window.

### Language-specific supports

- C
- fileref: Find references of files (e.g., header files) in a file browser
  (e.g., dired).

### Features

- Support TRAMP out of the box.
