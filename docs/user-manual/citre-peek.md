# How To Use `citre-peek`

## Peek the definition of a symbol

The basic use of `citre-peek` is to read the definition of a symbol, without
leaving current buffer. This is often helpful when writing code.

Type `M-x citre-peek` on a symbol to peek its definition. A peek window will be
opened under current line, and it follows the cursor. You can use these
keybindings to browse in the peek window:

- `M-n`, `M-p`: Next/prev line.
- `M-N`, `M-P`: Next/prev definition.
- `M-l j`: Jump to the definition.
- `C-g`: Close the peek window.

These keys are defined in `citre-peek-keymap`, and can be customized.

Here's a little bonus: You can use `citre-peek` in an xref buffer to peek the
location of current item.

After jumping to the definition, the peek window will still be shown. This
behavior can be tweaked by `citre-peek-auto-restore-after-jump`.

When filling the arguments of a function, we often want to see the signature of
the function. We have a convenient way to do that.

For example:

```
function(arg1, arg|
```

`|` is the cursor position. Now, type `M-x citre-ace-peek`, and an "ace string"
will be attached to each symbol in the displayed part of the buffer. Now this
line will look like:

```
[sdf]ction(arg1, arg|
```

`sdf` is the ace string. Type it to peek the definition of `function`.

If you don't type the ace string, but press `RET`, than the symbol under point
will be used, like `citre-peek`.

The keys used in an ace session can be customized by `citre-peek-ace-keys`,
`citre-peek-cancel-ace-keys` and `citre-peek-ace-pick-symbol-at-point-keys`.

## Follow a function call chain

When reading a definition, we often find it calls another function, and we are
interested in the definition of that function too. Use a traditional code
reading tools to do this, and we'll soon get lost in a lot of buffers.

Not a problem in `citre-peek`. In a peek session, type `M-l p` to call
`citre-peek-through`, than an ace string will be attached to each symbol in the
peek window, and you know what to do now :) Just type it to see the definition
of that symbol, in the same peek window.

Do this several times, and the history is shown in the bottom of the peek
window, like this:

```
· → func1 → func2 → [func3]
```

The `·` is the position of the symbol you initially peeked. You can type
`<left>` and `<right>` to move in the history. Now move back to `func2`, it
becomes:

```
· → func1 → [func2] → func3
```

And you'll find the definition where you peeked through is prefixed by a `*`.
If it's not the first one in the definition list, it's also moved to the first
position.

You can type `S-<up>` and `S-<down>` to move the current definition up and
down, or `M-l f` to make it the first one.

## Browse in the tree history

Let's continue from the peek session in the last section. We are at `func2`
now, and we peek through the symbol `func4`. Now the history becomes:

```
· → func1 → func2 < [func4]
```

The `<` means you created a branch here. The current branch goes to `func4`,
and the other one goes to `func3`. Type `<left>` to move to `func2` again, and
type `<up>`/`<down>` to switch between the branches. Congratulations! You just
learned the most effective way to browse in a tree-like code reading history.

Sometimes we may want to delete a branch. Type `M-l d` to delete the current
branch after current symbol; Type `M-l D` to delete all branches after it.

## Save and restore peek sessions

After you close the peek window by `C-g`, you can restore the peek session by
`citre-peek-restore`.

TBW

## The big picture of code reading

TBW
