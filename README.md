# ob-q

## Features

ob-q adds [org-babel](https://orgmode.org/worg/org-contrib/babel/) support for evaluating codeblocks
written in [q/kdb+](https://code.kx.com/q/) in org files, a markup language from [emacs](https://www.gnu.org/software/emacs/).

Some features:
- Session evaluation (only q console)
- Asynchronous execution
- Basic value extraction and passing
- Integration with [q-mode](https://github.com/psaris/q-mode/tree/master)

## Installation

``` emacs-lisp
(require 'ob-q)
```

# Headers

## Supported :var Header Arguments Types

`ob-q` currently does not support tables and dictionaries in `:var` header arguments.

## Sessions

`:session` headers only supports `q` interpreter sessions. It **does not support** `qcon` sessions. Please use the `:remote` header argument described below

## Asynchronous Execution

Use `:async` header to execute codeblock asynchronously.

## Remote

WIP
