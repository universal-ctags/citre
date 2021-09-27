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
