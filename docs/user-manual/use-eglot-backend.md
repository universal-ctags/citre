# Use Eglot Backend

## Prerequisite

See the documentation of [eglot](https://github.com/joaotavora/eglot) to know
how to configure it.

Once you get eglot to work, you can use Citre in any buffer managed by eglot.

## Capabilities

Citre only adapts the finding definition/reference abilities of Eglot, as
auto-completion and imenu integration is done by eglot itself.

Eglot backend doesn't support finding definitions/references for a user
inputted symbol, so `citre-query-*` doesn't work. Tags and global backend will
be used for them if Citre could find a tags file or global database for the
current project.
