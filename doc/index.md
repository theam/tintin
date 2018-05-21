---
title: Home
---

# Let's teach about our passion

---

```text
I'm building books.

But, by teaching, I'm also building
a community.

Teaching is building.

    - Julie Moronuki, author of Haskell Programming from first principles
```

---

One day, all Haskell programmers outgrow tutorials. Haddock is more than enough, right?

Haskell is awesome enough to have some very expressive types that tell you how to
work your way through the library that you're using.

But Haddock is API documentation, which is a kind of **hard documentation**.
_(Hard as in granite)_

Let's face it, there are **a lot** of people out there trying to reach that point.
People like to get things done, and to do so, they have to use awesome **libraries like yours.**
And to do so, they consume lots of tutorials and guides, which are a kind of
**soft documentation**.

Also, not everyone has the **time** of maintaining a super great library and also a website
for hosting the documentation. Some users would like to do so, but also, why creating a
site from scratch for your package, when they can do so **in their own blog**?

## Tintin handles all of that

**Tintin** is a utility to create a **soft documentation** website for your Haskell project.
All documentation blocks are compiled through GHC to ensure that there are no compilation errors.

Forget about creating a website, just write your tutorials using **markdown**. Host them on
Github pages.

And it's a piece of cake:

---

* **1.** Put your documentation into the `doc` folder
* **2.** Install the `tintin` dependencies
* **3.** Run `tintin`

---

## Code correctness

Tintin runs the GHC Haskell compiler over all of your documentation files, ensuring that everything is
correct.

Each one of your code blocks is typechecked and compiled, meaning that your documentation won't get outdated,
as it depends on your code:

```haskell top
{-# LANGUAGE OverloadedStrings #-}
import Data.Text as Text

protocol :: Text -> Text
protocol = Text.takeWhile (/= ':')
```

## Inline examples

You can also evaluate code in your documentation, meaning that your users will know what to expect from
your examples:

```haskell eval
protocol "https://github.com/theam/tintin"
```


## Credits

Special thanks to [47deg](https://www.47deg.com/) for developing [sbt-microsites](https://47deg.github.io/sbt-microsites/),
it served as great inspiration for this project.

Technologies used throughout this project:
* [Inliterate](https://github.com/diffusionkinetics/open/tree/master/inliterate), the package that compiles and renders the websites.
* [Clay](http://fvisser.nl/clay/), a CSS preprocessor embedded in Haskell.
* [Lucid](https://github.com/chrisdone/lucid), an HTML preprocessor embedded in Haskell.
* [Universum](https://github.com/serokell/universum), the prelude used in this project.
* [Yaml](https://github.com/snoyberg/yaml/), the Yaml deserialization library
* [Bootstrap 4](https://getbootstrap.com/docs/4.0/), CSS framework
* [jQuery](https://jquery.com/)
* [AOS](https://github.com/michalsnik/aos), animate on scroll JS library
* [AnimeJS](http://animejs.com/), animation JS library

