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
information directly from your stack or cabal configuration file, but you can override
the settings in your `.tintin.yml` file too:

```yaml
color: blue
```

## Changing the color

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

Just write your color code as if you were writting it in a CSS file:

```yaml
color: #424242
```

This color will be set as the main background color and a shade of it will be generated for the menus backgrounds. Take into account that title text will remain white and content will be black text on white background, so dark or saturated colors will work better.

## Changing the project title for a logo

Tintin also gives you the possibility of using your own logo.

To do so use the `logo` setting:

```yaml
color: blue
logo: https://github.com/theam/tintin/raw/master/assets/logo.svg
```

It is recommended that the logo has transparent background.

