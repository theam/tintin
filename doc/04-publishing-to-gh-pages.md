---
title: Publishing to Github pages
---

# Publishing to Github pages

Right now tintin does not support automatic Github pages upload, but it's
a very easy process.

By default, tintin will save the website to `.stack-work/tintin/rendered`.
If yor prefer to change this, you can use the `--outputDirectory` flag:

```bash
tintin --outputDirectory path/to/the/desired/folder
```

You can add push it easily to your repo:

```bash
cd <your docs folder>
git init
git checkout -b gh-pages
git add .
git commit -m "Update docs"
git remote add origin <your repo>
git push origin gh-pages
```


