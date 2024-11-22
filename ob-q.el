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
         (result-type (cdr (assoc :result-type processed-params)))
         (type-processed-body
          (if (eql result-type 'value)
              ;; only function wrap the stripped body
              (ob-q-fun-wrapper (q-strip body))
            (q-strip body))))
    ;; TODO maybe use trap to not enter debugger for no reason
    (if session
        (throw 'NotImplemented t)
      body)))

;; example params
;;((:var data . -1) (:var i . dddeaf) (:colname-names) (:rowname-names) (:result-params replace) (:result-type . value) (:results . replace) (:exports . code) (:session . none) (:cache . no) (:noweb . no) (:hlines . no) (:tangle . no))
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
    (message (format "processed-params are %s" processed-params))
    (message (format "raw-output is %s" raw-output))
    raw-output))

(defun ob-q-post-process-result (result)
  "Transform the query RESULT with read."
  (message (format "post-processing result=%s" result))
  (read result))

(defun ob-q-var-to-q (var)
  "Convert an elisp VAR into a string of q source code."
  (format "%S" var))

(defun ob-q-fun-wrapper (body)
  "Wraps BODY in a q lambda."
  (concat "{ 1 \"org-babel-q-BOF\\n\";.Q.S[x];1 \"org-babel-q-EOF\" }{[]" (replace-regexp-in-string "\n" ";" body) "}[]"))

(provide 'ob-q)
;;; ob-q.el ends here
