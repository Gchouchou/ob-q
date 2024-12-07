# ob-q

## Features

ob-q adds [[https://orgmode.org/worg/org-contrib/babel/][Org-Babel]] support for evaluating codeblocks
written in [[https://code.kx.com/q/][q/kdb+]] in org files, a markup language from [[https://www.gnu.org/software/emacs/][Emacs]].

Some features:
- Session evaluation
- Basic value extraction and passing
- Integration with [[https://github.com/psaris/q-mode/tree/master][q-mode]]

## Installation

``` emacs-lisp
(require 'ob-q)
```
[[https://github.com/psaris/q-mode/tree/master][q-mode]] is not necessary for this package although it is highly recommended to have it for syntax highlighting and editing .q files.


## Supported :var Header Arguments

`ob-q` currently does not support tables and dictionaries in `:var` header arguments.
