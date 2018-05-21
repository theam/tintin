---
title: Formatting your docs
---

# Formatting your docs

Tintin is just regular markdown. In case you are not familiar with it, [here you have a cheatsheet](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet).

# Haskell code

Tintin allows you to use the special Haskell code blocks from [inliterate](https://github.com/diffusionkinetics/open/tree/master/inliterate):

## Haskell top definitions

The top definitions are enclosed in the `haskell top` code block type. They are accessible throughout all your code blocks in the documentation file:

<pre>
<code>
```haskell top
foo :: Int
foo = 42
```
</code>
</pre>

## Haskell do blocks

Do blocks are enclosed in the `haskell do` code block type. Think of them as sentences in the Haskell REPL.

If you print something (`putStrLn`) it will get appended to the website. You can use this to generate HTML widgets.

<pre>
<code>
```haskell do
x <- readFile "foo.html"
putStrLn x
```
</code>
</pre>


## Haskell eval blocks

Eval blocks are enclosed in the `haskell eval` code block type. They allow you to evaluate something and show the result.

Note that the result type must implement the `Show` type class, or else it will fail.

<pre>
<code>
```haskell eval
(21 + 21) :: Int
=> 42
```
</code>
</pre>

## Hide flag

You can add a `hide` flag after `haskell top` or `haskell do` to hide this code block, yet allow it to execute. This is useful for
setting up example or stub values for your guides.

<pre>
<code>
```haskell top hide
databaseResult :: Text
databaseResult = "{name:Tom, age: 22}"
```
</code>
</pre>

