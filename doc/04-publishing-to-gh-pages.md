---
title: Publishing to Github pages
---

# Publishing to Github pages

By default, tintin will save the website to `.stack-work/tintin/rendered`.
If yor prefer to change this, you can use the `--outputDirectory` flag:

```bash
tintin run --outputDirectory path/to/the/desired/folder
```

Now, you can publish it easily to Github Pages:

```bash
tintin publish
```

Or, if you've changed the `outputDirectory`, you can set it too:

```bash
tintin publish --documentationDirectory path/to/the/desired/folder
```

# Manual publication

Tintin just executes some `git` commands in a row, nothing fancy.
If you prefer, you can do it manually:

```bash
cd <your docs folder>
git init
git remote add origin <your repo>
git checkout -b gh-pages
git add *
git commit -m "Update docs"
git push -f origin gh-pages
```


