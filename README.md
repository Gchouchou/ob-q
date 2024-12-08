# ob-q

## Features

ob-q adds [org-babel](https://orgmode.org/worg/org-contrib/babel/) support for evaluating codeblocks
written in [q/kdb+](https://code.kx.com/q/) in org files, a markup language from [emacs](https://www.gnu.org/software/emacs/).

Features:
- Integration with [q-mode](https://github.com/psaris/q-mode/tree/master)
- Session evaluation (only q console)
- Remote q execution with `hopen`
- Basic value extraction and passing
- Asynchronous execution

## Installation

``` emacs-lisp
(require 'ob-q)
```

# Headers

## Supported :var Header Arguments Types

`ob-q` currently does not support tables and dictionaries in `:var` header arguments.

## Sessions

`:session` headers only supports `q` interpreter sessions. It **does not support** `qcon` sessions. Please use the `:handle` header argument to execute code on a remote session.

## Asynchronous Execution

Use `:async` header to execute codeblock asynchronously.

## Handle

The `:handle` header specifies an interprocess communication handle. This header argument tells org-babel to wrap the code body into a single line string and then executes a
[one shot request](https://code.kx.com/q/ref/hopen/#one-shot-request).
This means that it is possible to have both `:session` and `:handle` header arguments on the same codeblock. The session will execute a oneshot request to the handle.

`:handle` and `:async` do not play well together. Runing a second codeblock with async while waiting for the first query will fail since the handle will occupied.
This issue does not happen with the `:session` header since the session is also occupied and will process the second query synchronously after the first.

# Known Bugs and Issues

- Getting an error with `:session` argument with `:results value` leads to confusing output. Can be fixed using trap and error trace.
