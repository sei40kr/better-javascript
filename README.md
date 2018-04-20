# better-javascript

Better JavaScript layer for Spacemacs.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
# Table of Contents

- [Description](#description)
    - [Features:](#features)
- [Install](#install)
- [Key Bindings](#key-bindings)
    - [flow-minor-mode](#flow-minor-mode)
    - [Auto-import (import-js)](#auto-import-import-js)
    - [Formatting (Prettier, ESLint)](#formatting-prettier-eslint)

<!-- markdown-toc end -->

# Description

This layer adds improvements for Spacemacs javascript layer.

## Features:

* Auto-import feature using [import-js](https://github.com/Galooshi/import-js) 
* Flow support by [flow-minor-mode](https://github.com/an-sh/flow-minor-mode)
* Formatting with [prettier](https://github.com/prettier/prettier) and [eslint_d.js](https://github.com/mantoni/eslint_d.js) 

# Install

To use this configuration layer, add it to your `~/.spacemacs`. You will need to add `better-javascript` to the existing `dotspacemacs-configuration-layers` list in this file.

You will also need to install `flow-bin` and `flow-coverage-report` to use the Flow language:

```sh
$ npm install -g flow-bin flow-coverage-report
```

To use the auto-import feature, install `import-js`:

```sh
$ npm install -g import-js
```

To use the formatting features, install `prettier` and `eslint-cli` ([eslint_d](https://github.com/mantoni/eslint_d.js) is preferred for performance):

```sh
$ npm install -g prettier eslint-cli eslint_d
```

To activate error checking using flycheck, install `eslint-cli` ([eslint_d](https://github.com/mantoni/eslint_d.js) is preferred for performance):

```sh
$ npm install -g eslint-cli eslint_d
```

# Key Bindings

## Auto-import (import-js)

| Key Binding | Description                                                         |
|-------------|---------------------------------------------------------------------|
| `SPC i i`   | import the module for the variable under the cursor                 |
| `SPC i f`   | import any missing modules and remove any modules that are not used |
| `SPC i g`   | go to the module of the variable under the cursor                   |

## Formatting (Prettier, ESLint)

| Key Binding | Description                     |
|-------------|---------------------------------|
| `SPC m =`   | format code in Prettier, ESLint |

# Special Thanks

* [@agevelt's aj-javascript layer for Spacemacs](https://github.com/agevelt/.spacemacs.d/tree/master/layers/%2Baj/aj-javascript)
* [@an-sh's flow-minor-mode](https://github.com/an-sh/flow-minor-mode/blob/master/flow-minor-mode.el)
* [@evindor's prettier-eslint-emacs](https://github.com/evindor/prettier-eslint-emacs/blob/master/prettier-eslint.el)
