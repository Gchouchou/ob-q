# ob-q

## Features

ob-q adds [Org-Babel|https://orgmode.org/worg/org-contrib/babel/] support for evaluating codeblocks
written in [q/kdb+|https://code.kx.com/q/] in org files, a markup language from [Emacs|https://www.gnu.org/software/emacs/].

Some features:
- Session evaluation
- Basic value extraction and passing
- Integration with [q-mode|https://github.com/psaris/q-mode/tree/master]

## Installation

``` emacs-lisp
(require 'ob-q)
```
[q-mode|https://github.com/psaris/q-mode/tree/master] is not necessary for this package although it is highly recommended to have it for syntax highlighting and editing .q files.


## Supported :var Header Arguments

`ob-q` currently does not support tables and dictionaries in `:var` header arguments.
