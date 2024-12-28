# ob-q

## Features

ob-q adds [org-babel](https://orgmode.org/worg/org-contrib/babel/) support for evaluating codeblocks
written in [q/kdb+](https://code.kx.com/q/) in org files, a markup language from [emacs](https://www.gnu.org/software/emacs/).

Features:
- Integration with [`q-mode`](https://github.com/psaris/q-mode/tree/master)
- Session evaluation (only q console)
- Remote q execution with `hopen`
- Basic value extraction and passing
- Asynchronous execution

## Installation

The package depends on [`q-mode`](https://github.com/psaris/q-mode/tree/master).
Then require the package
``` emacs-lisp
(require 'ob-q)
```

# Headers

## Value vs Output

The header arguments `:results value` will wrap the body in a lambda and pass variables `:var`
as function parameters.
Due to how scoping works in `q`, you have to be careful
when using lambda inside another lambda. Non global variables are not accessible inside a lambda.
For example, the following will give an error.
``` q
{[]
 local: 2;
 f: {[] local+3};
 :f[]
 }[]
```
`local` is not accessible inside the body of `f`.

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

`:handle` also does not work as expected as with multiline commands with command line arguments. For example

``` q
\c 200 2000
1+1
```

will fail since we try to wrap the console command with 1+1 and add a semi-colon. In other words, the body would be `\c 200 2000;\n1+1`
and this is not valid. To get command line arguments to work, use only a single line without new line. In other words,
``` q
\c 200 2000
```
would work fine.

## Backtrace collection

Use the header arg `:trap yes` to enable catching errors and printing the backtrace.
This will prevent the session from entering debugger and org babel will receive the backtrace string.

This header only applies when the body is wrapped in a lambda with `:results value`.
It uses [`.Q.trp`](https://code.kx.com/q/ref/dotq/#trp-extend-trap-at) when there are 0 or 1 input parameters
through the header `:var`. This function is only available from q version 3.5 onwards.
If there are 2 or more input parameters, it uses the [`.Q.trpd`](https://code.kx.com/q/ref/dotq/#trpd-extend-trap)
which is only included in q version 4.1 and onwards.

## Supported :var Header Arguments Types

`ob-q` currently does not support tables and dictionaries in `:var` header arguments.
