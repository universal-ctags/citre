# Customization

Some of the user options are already mentioned in the user manual and
[README](../../README.md). We'll explain some more useful ones here. These are
still not all. For all user options, read the source code. They are in the
"user options" sections in each source file.

## Auto-completion

### Case sensitivity

Customize `citre-completion-case-sensitive` to tweak the case sensitivity of
auto-completion.

### Substring completion

Substring completion can be useful sometimes, especially when you don't
remember the full name of a symbol, but can remember part of it. To use
substring completion, you need to:

- Set `citre-capf-substr-completion` to non-nil.
- Add `substring` or other suitable completion styles to `completion-styles`.

I personally recommend the following scheme for auto-completion:

- Use prefix completion with popup completion UI. This is much faster since
  readtags can perform binary search. We use popup completion when we know the
  symbol name, but want to save some typing or make sure we spell it correctly.

- Use substring completion for the `completion-at-point` UI. This is for
  situations when we remember part of the name. We type the part we remember,
  and call `completion-at-point`.

To separate these 2 use cases, we need to create a customized
`completion-at-point` command, like this:

```elisp
(defun my-completion-at-point ()
  (interactive)
  (let ((citre-capf-substr-completion t)
        (completion-styles '(substring basic)))
    (completion-at-point)))
```

If you need finer customization on the completion styles, you can customize
`completion-category-overrides` for the category `citre`. For example, you
normally use `substring` completion style, but you only want prefix completion
for Citre, you can do:

```elisp
(add-to-list 'completion-category-overrides
             '(citre (styles basic)))
```

### Popup completion

Citre has a `citre-capf-optimize-for-popup` option. You should keep it as
non-nil if you use popup completion UI. Read its docstring for details.

## Finding definitions

### After jump hook

The hook `citre-after-jump-hook` that runs after you jumping to the definition.
By default, it recenters the line and blink it. You can use your own function
here to do what you want.

### Definition string format

In `citre-peek` and `citre-jump`, the definition is shown as a string in the
following format:

```
kind/type@scope<R> /path/to/source/file(line): line content
```

This is self explanatory. The mark `<R>` means the definition comes from a
reference tag. Some parts of it may be missing if it's not recorded in the tags
file (maybe ctags doesn't support it for the particular language, or some tags
just don't have a scope).

To tweak the display format, see these options:

- `citre-definition-annotation-separator`
- `citre-definition-annotation-separator-for-scope`
- `citre-definition-reference-mark`
- `citre-definition-missing-file-mark`

If the path of the tags is under current project, it's displayed using the
relative path to the project root. `citre-project-root-function` is the
function used to detect project root, and you can customize it.

### `citre-jump` UI

By default, `citre-jump` uses the `completing-read` UI. You can customize
`citre-jump-select-definition-function` to use your own UI that picks a
definition out of a list of them. See its docstring.

### imenu

When a tags file is large, running `imenu` could be slow. In this situation,
Citre let Ctags scan the current file, and create a temporary tags file for it,
which should be faster. By default, we consider a tags file larger than 50 MiB
to be "large". You could customize `citre-imenu-create-tags-file-threshold` to
change this behavior, see its docstring for details.

## Misc

The `completion-at-point`, xref and imenu integration can be enabled/disabled
by:

- `citre-enable-xref-integration`
- `citre-enable-capf-integration`
- `citre-enable-imenu-integration`

These are buffer-local variables so remember to use `setq-default` for them.
Also, they are safe to use as file-local variables, so you can configure them
on a per-directory basis.
