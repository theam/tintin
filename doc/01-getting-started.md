---
title: Getting Started
---

# Getting Started with Tintin

**Note:** Currently, `tintin` only supports [stack](https://haskellstack.org) projects.


## Requirements

### Stack

You'll have to have installed the [stack](https://haskellstack.org) Haskell build tool .
The installation is straightforward:

```bash
wget -qO- https://get.haskellstack.org/ | sh
```

### Inliterate

Inliterate is the rendering mechanism for the documentation. Add the following packages
to the `extra-deps` section of your `stack.yaml` file:

```bash
- inliterate-0.1.0
- lucid-extras-0.1.0.0
- plotlyhs-0.2
```

After that perform a `stack install inliterate --stack-yaml <your stack.yaml file>`.

You are ready to roll, proceed to [documenting your project](02-documenting-your-project.html)!
