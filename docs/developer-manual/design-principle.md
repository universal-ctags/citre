# The Design Principle of Citre

*This is an unfinished document and contains my immature ideas.*

## The goal of Citre

Citre tries to become the most advanced ctags frontend for text editors, and I
think this is very likely to come true.

Nowdays there are "intelligent" code analyzing tools, and ctags is considered
to be an old technology. But as we'll discuss below, ctags actually has many
strengths over those modern tools. *The goal of Citre is to give full play to
them.*

## Intelligence vs. fuzzy

There are 2 kinds of source code indexing tools. Some tries to be intelligent.
They can "understand" the code (e.g. they can get the AST), so they do things
accurately (e.g. auto-complete the methods from the correct classes). Typical
examples are rtags and language servers.

Others are kind of "fuzzy". We are talking about ctags here. It doesn't try to
fully understand the code, instead it scans the code and extract definitions
based on incomplete rules. Client tools based on ctags could try to be
intelligent, but that will never be on par with the first kind.

Since Citre is built on ctags, we want to find out the strengths of ctags over
"intelligent" tools, and let Citre give full play to them.

First we have some obvious ones:

- Since ctags doesn't run a daemon that consistently parse/compile your code,
  it's much more energy saving and usable for large projects.

- The fact that ctags doesn't understand the AST can also become useful
  sometimes, e.g. it's far more tolerant to syntax errors, so it's likely to
  work on files that's being edited.

  Another example is you may want to jump into an `#if 0` or `#if DEBUG` block
  in some C code. A tool that understands the syntax would just ignore them.

But from the perspective of Citre, we are looking for something more
fundamental or essential, that decides what should a frontend of a ctags works,
and how people should use it. The result of my investigation on this is: *ctags
is hackable, and ctags works for multi-language projects.*

### Ctags is hackable

Ctags could be easily extended to tag definitions in DSL code. One way of doing
this is through regex. A "real world" example is in
`ctags/Tmain/nested-subparsers.d/event.ctags`:

```
--langdef=Event{base=C}
--kinddef-Event=e,event,events
--regex-Event=/DEFINE_EVENT\((.*)\);/\1/e/)
```

It introduces `event` kind. I don't know this well, but it's a macro in the
Linux Kernel. The point is when using `DEFINE_EVENT`, the programmer thinks
about defining an `event`, rather than what the macro is eventually expanded
into.

This is even better than intelligent tools, because although they can expand
the macro at place and get the actual AST, *they have no concept of a kind that
only exists in the programmer's mind.*

We have more examples. [lw_oopc](https://github.com/Akagi201/lw_oopc) is a set
of macros that brings OOP concepts like class and interface to C.
[Cello](http://libcello.org/) goes even crazier. In DSL-heavy codes (that based
on similar libraries), only ctags (with extensions using regexp/optscript)
could produce not-so-bad indexing. This is more true for Lisp (with real
macros) and other languages that have meta-programming ability.

A ctags frontend needs to work well with such usage (since it's the unique
strength of ctags), and this contradicts with being intelligent. For example,
when filling the arguments of a function, we could check it's signature, and
only provides variables and functions that has the "right" type for
auto-completion, but what if the user wants to put an "event" here (which is
some variable with the proper type under the hood)? The signature won't tell
you about this usage. Similar difficulties happen more with DSL-heavier codes.

What we need to put in mind is: When a user defines a new kind, they expects to
auto-complete it when needed, *without further configuration*, or they will be
mad at Citre, like "I've spent my time hacking ctags to tag a new kind, and
Citre just throw those tags away?". The same for finding definitions: If we
judge the kind of the thing under point, based on its context, we'll fail in
the situation where it's a user-defined kind of thing.

### Ctags works for multi-language projects

This is simple: Most "go to definition" tools based on tags files simply search
for tags by their names, and don't restrict the language, so for example, you
can jump to the implementation of a function in another language. This may be a
more common situation than you thought. e.g., don't you want to jump to the
definition of a function from the documentation (which is written in some
markup language)?

Intelligent tools could hardly do this, since it only understands one language.

Citre could support multi-language projects easily. But at the same time, this
means it's harder (and more meaningless) to provide language specific support.
For example, Even if Citre supports C and Python respectively, which means it
could (intelligently) filter C tags when in C code, and Python tags when in
Python code, it's useless when jumping from Python to C, since you are in the
context of Python, you can't filter the C tags based on the context.

I'd like to look into these to understand multi-language programming more:

- [swig](http://www.swig.org)
- [pybind11](https://github.com/pybind/pybind11)
- [ffi](https://en.wikipedia.org/wiki/Foreign_function_interface)
- [Calling C/Fortran functions from
  Julia](https://docs.julialang.org/en/v1/manual/calling-c-and-fortran-code/)
- [PyCall.jl](https://github.com/JuliaPy/PyCall.jl)
- [Nim Backend Integrationn](https://nim-lang.org/docs/backends.html)
- The HTML/CSS/JavaScript thing.

## The UI design

So basically, we give up understanding the code by context, as it contradicts
with the strengths of ctags, which is discussed above. Rather, I want to take
the below approach, for both auto-completion and finding definitions:

1. Citre filters and sorts the tags by absolute minimum conditions. This means
   we try to offer good enough results (by filtering out unneeded tags), but it
   should never throw away a tag that may be needed in any situation,
   particularly when working with user-defined kinds and multi-language
   projects (corresponding to the 2 unique strengths of ctags).
2. The user further filters by inputting things. Then there's few left, so the
   user could pick one.

1 is required by the nature of ctags. If there are too many tags after 1, then
2 is necessary for offering a small subset of them. In other words, *logically,
this is what a "ctags frontend done right" should be*.

### Filtering/sorting by Citre

Here I'll write some principles for language specific support.

For auto-completion, it's safe to filter out:

- anonymous tags (they can't be referenced by a name)
- reference tags (definition tags already have all the names we need)
- file tags (unless we are sure the user needs a file name here)
- tags that have "file" scope, and is not in this file (so they can't be used)

What "safe" here means is: in all situations we can think of (especially in
multi-language project, and with user-defined kinds), the tags needed by the
user won't exist in these tags, so we could throw them away.

I once thought it's safe to restrict the `language` field. For example, we can
filter C/C++ tags when completing C code (since the header files are counted as
C++ code). For multi-language programming, this shouldn't be a problem. For
example, when calling a function in language B from language A, there should be
a function definition in language A that wraps the implementation in language
B. And for auto-completion, we only need the symbol name, not the location of
its definition. But the truth is:

- the wrapper code can be generated by a tool, and it may not exist in the
  project, but in some cache directory (see
  [here](https://github.com/universal-ctags/citre/pull/48) for an example).
- User-defined kinds typically involve user-defined sub-language (which is
  encouraged as it never conflicts with the kind names of the base language).

For finding definitions, it's basically safe to filter out the same set of tags
except reference tags. In Citre, we sort the tags to put definition tags above
reference tags, so when the user needs a reference tag, they could browse the
list from the end.

Sometimes we want to make "very likely to be true" assumptions. For example,
when the cursor is after a dot in C code, the user may very likely needs a
struct member, but we are not sure about this, as they may actually want a
macro. In these situations, we could make use of the sorter expression. Just
put the members on top of the list, but still keep all the other tags just in
case they are needed.

### Filtering by the user

Then the results could be further filtered by user input. For now:

- For auto-completion you can further filter by tag names.
- In `citre-jump` you can further filter by file paths.

I have 2 ideas for the existing tools:

- Filtering by file paths for auto-completion is meaningful, as the user may
  want "a symbol from a library", which must exists in a path like
  `../library-name/..`.
- `citre-peek` doesn't have such a mechanism for now, and I want to add one
  like in `citre-jump`.

These mechanisms are sufficient most of the time, but they don't meet the needs
for complex filtering rules. Here's an example I have in mind. The user inputs:

```
something kind:^member$ kind:^macro$ input:.c$
```

This searches the tags whose names contain `something`, with the kind `member`
or `macro`, and whose paths end with `.c`. We can also offer abbreviate like
`k` for `kind`, `p` for `input` (path), `t` for `typeref`, etc.

The point is:

- A tags file contains a lot of tags.
- Since there's a lot, we need to filter them before using them.
- Auto-completion and finding definitions are 2 typical tasks for programmers,
  where we can design the efficient filtering rules.
- Auto-completion and finding definitions may cover 95% of the usage of a tags
  file. But there are other usages (e.g. the user wants to know all functions
  in a module, or all methods of a class). These usages can be covered by a
  general mechanism of "filtering the tags by user input". That's where the
  "complex filtering rules" above are useful.

This is hard to implement in the existing tools. I'm thinking about a new tool
for this, see [this
discussion](https://github.com/universal-ctags/citre/discussions/47).
