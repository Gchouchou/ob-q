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
;;; Package-Requires: ((emacs "26.1"))

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
  (let* ((vars (org-babel--get-vars processed-params))
         (result-type (cdr (assoc :result-type processed-params)))
         (type-processed-body
          (if (eql result-type 'value)
              ;; only function wrap the stripped body
              (ob-q-fun-wrapper (q-strip body) vars)
            (q-strip body))))
    ;; TODO maybe use trap to not enter debugger for no reason
    (message (format "expanded body %s" type-processed-body))
      type-processed-body))

;; example params
;;((:var data . -1) (:var i . dddeaf) (:colname-names) (:rowname-names) (:result-params replace) (:result-type . value) (:results . replace) (:exports . code) (:session . none) (:cache . no) (:noweb . no) (:hlines . no) (:tangle . no))
(defun org-babel-execute:q (body params)
  "Execute q BODY according to PARAMS.
This function is called by `org-babel-execute-src-block'"
  (require 'ob-q)
  (let* ((processed-params (org-babel-process-params params))
         ;; set the session if the value of the session keyword
         ;;(session (cdr (assoc :session processed-params)))
         (session (unless (string= (cdr (assq :session processed-params)) "none")
                    (ob-q-initiate-session
                     (cdr (assq :session processed-params)))))
         (full-body (org-babel-expand-body:q body params processed-params))
         (tmp-src-file (org-babel-temp-file "q-src-" ".q"))
         (cmd (format "q %s" (org-babel-process-file-name tmp-src-file)))
         (raw-output (progn (with-temp-file tmp-src-file (insert full-body))
                            (org-babel-eval cmd ""))))
    ;(ob-q-post-process-result (org-babel-eval cmd "" )))))
    (message (format "processed-params are %s" processed-params))
    (message (format "raw-output is %s" raw-output))
    raw-output))

(defun ob-q-post-process-result (result)
  "Transform the query RESULT with read."
  (message (format "post-processing result=%s" result))
  (read result))

(defun ob-q-var-to-q (var)
  "Convert an elisp VAR into a string of q source code."
  (pcase (type-of var)
    ('integer (format "%S" var))
    ('float (format "%S" var))
    ('string (format "%S" var))
    ('symbol (format "`%S" var))
    (_ (format "%S" var))))
;; TODO handle date time formats...

(defun ob-q-fun-wrapper (body &optional vars)
  "Wraps BODY in a q lambda with VARS as parameters."
  (concat "{["
          (when vars
            (substring (apply
                        #'concat
                        (mapcar
                         (lambda (pair)
                           (format ";%s" (car pair))) vars))
                       1))
          "]\n " (replace-regexp-in-string "\n" ";\n " body) "}["
          (when vars
            (substring (apply
                        #'concat
                        (mapcar
                         (lambda (pair)
                           (format ";%S" (cdr pair))) vars))
                       1))
          "]"))
;; TODO make a function to convert lists,tables etc to q lists

(defun ob-q-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (unless (string= session "none")
    (or (get-buffer session)
        (let ((buffer (prog2 (q "" "" "")
                          (get-buffer q-active-buffer)
                        (setq q-active-buffer nil))))
          (when buffer
            (with-current-buffer buffer
              (rename-buffer session)
              (current-buffer)))))))

(provide 'ob-q)
;;; ob-q.el ends here
