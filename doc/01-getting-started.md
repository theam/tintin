---
title: Getting Started
---

# Getting Started with Tintin

## Requirements

### Build tool

You'll have to have installed either, the [stack](https://haskellstack.org) Haskell build tool. Or `runghc`.
The Stack installation is straightforward:

```bash
wget -qO- https://get.haskellstack.org/ | sh
```

## Installing

The recommended way to start using tintin in your project is to use stack:

```bash
stack install tintin
```

Once compilation finishes, you should be able to run tintin from the console:

```bash
tintin run
```

## Creating a new project with the Tintin template

We provide a Stack template for making your life easier when creating projects with Tintin:

```bash
stack new --resolver nightly <your-project-name> https://github.com/theam/tintin/raw/master/assets/template.hsfiles
```

_Note: The `--resolver nightly` is required, as the tintin dependencies are not in LTS yet._

You are ready to roll, proceed to [documenting your project](02-documenting-your-project.html)!
