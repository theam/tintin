---
title: Customization
---

An h1 header
============

Paragraphs are separated by a blank line.

2nd paragraph. *Italic*, **bold**, and `monospace`. Itemized lists
look like:

  * this one
  * that one
  * the other one

## You can have Haskell code inline:

```haskell top
data Person = Person
  { personName :: String
  , personAge  :: Int
  }

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
```

And evaluate stuff

```haskell eval
take 10 fibs
```

Note that not considering the asterisk the actual text
content starts at 4-columns in.

> Block quotes are
> written like so.
>
> They can span multiple paragraphs,
> if you like.

Use 3 dashes for an em-dash. Use 2 dashes for ranges (ex., "it's all
in chapters 12-14"). Three dots ... will be converted to an ellipsis.
Unicode is supported. ☺

