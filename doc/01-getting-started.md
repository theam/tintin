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

You can install Tintin by issuing the following command:

```bash
wget -qO- https://github.com/theam/tintin/raw/master/assets/install | sh
```

We invite you to [check the installation script](https://github.com/theam/tintin/blob/master/assets/install)
before you run it.

If you prefer `cabal` over Stack, you can run:

```bash
cabal new-update
cabal new-install tintin
```

## Creating a new project with the Tintin template

We provide a Stack template for making your life easier when creating projects with Tintin:

```bash
stack new --resolver nightly <your-project-name> https://github.com/theam/tintin/raw/master/assets/template.hsfiles
```

_Note: The `--resolver nightly` is required, as the tintin dependencies are not in LTS yet._

You are ready to roll, proceed to [documenting your project](02-documenting-your-project.html)!
