---
title: Customizing your site
---

# Customizing your site

When you run tintin for the first time with `tintin run`, it will generate a `.tintin.yml`
file with the following initial content:

```yaml
color: blue
```

This is the main tintin configuration file. By default, Tintin will try to pull the basic
information directly from your `package.yaml` or `*.cabal` configuration file if they're present,
but you can override the settings in your `.tintin.yml` file too:

```yaml
name: Wonderful project
synopsis: This is a project that does wonderful things
github: theam/wonderful-project
author: Wonder Woman
color: #AB2D1C
logo: https://upload.wikimedia.org/wikipedia/en/3/3a/Wonder_Woman_Vol_5_16.png
titleFont: Poiret One
titleFontWeight: 400
bodyFont: PT Sans
```

## Changing the color theme

The `color` setting is used by tintin to generate the theme for your website.
You can either set one of the preset color themes or set a RGB hexadecimal code:

### Use one of the preset color themes are:

#### blue

```yaml
color: blue
```

![tintin blue](https://s3-eu-west-1.amazonaws.com/worldwideapps/assets/tintin_blue.png)

#### purple

![tintin purple](https://s3-eu-west-1.amazonaws.com/worldwideapps/assets/tintin_purple.png)

#### lightGreen

![tintin lightgreen](https://s3-eu-west-1.amazonaws.com/worldwideapps/assets/tintin_lightgreen.png)

#### darkGreen

![tintin darkgreen](https://s3-eu-west-1.amazonaws.com/worldwideapps/assets/tintin_darkgreen.png)

#### darkBlue

![tintin darkblue](https://s3-eu-west-1.amazonaws.com/worldwideapps/assets/tintin_darkblue.png)

#### bronze

![tintin bronze](https://s3-eu-west-1.amazonaws.com/worldwideapps/assets/tintin_bronze.png)

#### darkOrange

![tintin darkorange](https://s3-eu-west-1.amazonaws.com/worldwideapps/assets/tintin_darkorange.png)

#### lightOrange

![tintin lightorange](https://s3-eu-west-1.amazonaws.com/worldwideapps/assets/tintin_lightorange.png)

#### red

![tintin red](https://s3-eu-west-1.amazonaws.com/worldwideapps/assets/tintin_red.png)

#### grey

![tintin grey](https://s3-eu-west-1.amazonaws.com/worldwideapps/assets/tintin_grey.png)

### Use a custom hexadecimal color code

Just write your hexadecimal color code as if you were writing it in a CSS file:

```yaml
color: #424242
```

This color will be set as the main background color and a shade of it will be generated for the menus backgrounds. Take into account that title text will remain white and content will be black text on white background, so dark or saturated colors will work better.

## Setting a custom logo

By default, tintin will show the project name in the frontpage, but it can be replaced by an image setting an absolute URL in the `logo` option:

```yaml
color: blue
logo: https://github.com/theam/tintin/raw/master/assets/logo.svg
```

For the best results, we recommend to use an image with transparent background.

## Setting custom Google Fonts

You can override the default tipography for titles and text body. Tintin uses Google Fonts, so you can set any tipography in [the Google Fonts catalog](https://fonts.google.com).

### Title font

To change the title font, set the font name and weight in your `.tintin.yml` file as follows:

```yaml
titleFont: Poiret One
titleFontWeight: 400
```

Notice that **the title font weight setting is required** for Tintin to find the right font files in google servers.

### Body font

To change the body font you can just set the `bodyFont` option with the name of your prefered font. Tintin will use the regular version of the chosen font.

```yaml
bodyFont: PT Sans
```
