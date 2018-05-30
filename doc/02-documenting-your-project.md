---
title: Documenting your project
---

# Documenting your project

## Folder structure

Begin by creating a `doc` directory in the same directory
where your `package.yaml` (or `*.cabal`) file is located.

Inside there should be **at least** two files:

- `index.md`
- Another file

It is recommended that you name your files with a number
at the beginning, e.g.:

- `01-foo.md`
- `02-bar.md`
- `03-quux.md`

The reason for this is that `tintin` sorts the pages based
on their filename and then renders them in the navigation
bar.


## File structure

All files **must** include a _front matter_ with a title.
Meaning that they all start with this header:

```markdown
---
title: Title of the page
---
```

## Conventions

* The `title` of the `index.md` file is generally _"Home"_.
* The `title` of the first file is generally _"Getting started"_.

## Generating the website

Just execute `tintin run` and you will have a website ready at `.stack-work/tintin/rendered`.

