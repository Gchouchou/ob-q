;;; ob-q.el --- org-babel functions for q evaluation

;; Copyright (C) your name here

;; Author: your name here
;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file adds support for evaluating q (kdb+/q) code blocks in org-babel.

;;; Requirements:

(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
;; assuming q mode exists for Emacs

(add-to-list 'org-babel-tangle-lang-exts '("q" . "q"))

(defvar org-babel-default-header-args:q '())

(defun org-babel-expand-body:q (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (org-babel--get-vars (or processed-params (org-babel-process-params params)))))
    (concat
     (mapconcat ;; define any variables
      (lambda (pair)
        (format "%s:%S"
                (car pair) (org-babel-q-var-to-q (cdr pair))))
      vars "\n")
     "\n" body "\n")))

(defun org-babel-execute:q (body params)
  "Execute a block of q code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing q source code block")
  (let* ((processed-params (org-babel-process-params params))
         (session (unless (string= value "none")
                    (org-babel-q-initiate-session
                     (cdr (assq :session processed-params)))))
         (vars (org-babel--get-vars processed-params))
         (result-params (assq :result-params processed-params))
         (result-type (assq :result-type processed-params))
         (full-body (org-babel-expand-body:q
                     body params processed-params)))
    ;; execute code block, possibly in session or via external evaluation
    ))

(defun org-babel-prep-session:q (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  )

(defun org-babel-q-var-to-q (var)
  "Convert an elisp var into a string of q source code
specifying a var of the same value."
  (format "%S" var))

(defun org-babel-q-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  )

(defun org-babel-q-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (unless (string= session "none")
    ))

(provide 'ob-q)
;;; ob-q.el ends here
