;;; ob-q.el --- org-babel functions for q evaluation

;; Copyright (C) your name here

;; Author: your name here
;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org
;; Version: 0.02

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
;;; for session support, q-mode is needed
;;; Package-Requires: ((emacs "24.1"))

;;; Code:

;; (require 'org-macs)
;;(org-assert-version)

(require 'org)
(require 'ob)
(require 'ob-eval)
;; (require 'org-macs)
(require 'q-mode) ;; Require q-mode for interactive support

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("q" . "q"))

(defvar org-babel-default-header-args:q '())

(defun org-babel-expand-body:q (body params &optional processed-params)
  "Expand BODY according to PARAMS and PROCESSED-PARAMS, return the expanded body.
To be implemented, currently just returns BODY"
  (let* ((session (cdr (assoc :session processed-params)))
         (qcommand (if session
                       (concat "h:hopen `:" session ";\nresult:{$[10h=type x; x;.Q.s x]} h")
                     "result:{$[10h=type x; x;.Q.s x]} eval parse" ))
         (qendcommand (if session "hclose h;\n" "")))
    (concat qcommand "\""
            ;; escape all " to \" so that we can turn into a string
            (replace-regexp-in-string "\"" "\\\""
                                      ;; also hack and turn newline to semicolon hacking
                                      (replace-regexp-in-string "\n" ";" (q-strip body))
                                      nil 'literal)
            "\";\n" qendcommand "result\n\\\\")))

(defun org-babel-execute:q (body params)
  "Execute q BODY according to PARAMS.
This function is called by `org-babel-execute-src-block'"
  (let* ((processed-params (org-babel-process-params params))
         ;; set the session if the value of the session keyword
         ;;(session (cdr (assoc :session processed-params)))
         (full-body (org-babel-expand-body:q body params processed-params))
         (tmp-src-file (org-babel-temp-file "q-src-" ".q"))
         (cmd (format "q %s" (org-babel-process-file-name tmp-src-file)))
         (raw-output (progn (with-temp-file tmp-src-file (insert full-body))
                            (ob-q-post-process-result (org-babel-eval cmd "")))))
    ;;(ob-q-post-process-result (org-babel-eval cmd "" )))))

    (message (format "raw-output is %s" raw-output))
    raw-output))

(defun ob-q-post-process-result (result)
  "Transform the query RESULT with read."
  (message (format "post-processing result=%s" result))
  (read result))

(provide 'ob-q)
;;; ob-q.el ends here
