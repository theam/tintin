---
title: Home
---

# Tintin

**tintin** is a utility to create a *soft-documentation* website for your Haskell project.
All documentation blocks are compiled through GHC to ensure that there are no compilation errors.

Forget about creating a website, just write your tutorials using **markdown**.

You can evaluate Haskell code and show the results inline:

```haskell top
addInts :: Int -> Int -> Int
addInts x y = x + y
```

```haskell eval
addInts 20 22
```


## Credits

Thanks to [Tom Nielsen](https://github.com/glutamate) for developing [Inliterate](https://github.com/diffusionkinetics/open/tree/master/inliterate),
the package that compiles and renders the websites.

