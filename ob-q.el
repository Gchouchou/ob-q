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
;;; Package-Requires: ((emacs "27.1"))

;;; Code:

(require 'org)
(require 'ob)
(require 'ob-eval)
(require 'ob-comint)
;; (require 'org-macs)
(require 'q-mode) ;; Require q-mode for interactive support

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("q" . "q"))

(defvar org-babel-default-header-args:q '())

(defvar ob-q-eoe-indicator "1 \"org_babel_q_eoe\\n\";"
  "String to indicate that evaluation has completed.")
(defvar ob-q-eoe-output "org_babel_q_eoe"
  "String to indicate that evaluation has completed.")

(defun org-babel-expand-body:q (body params &optional processed-params)
  "Expand BODY according to PARAMS and PROCESSED-PARAMS, return the expanded body.
To be implemented, currently just returns BODY"
  (let* ((vars (org-babel--get-vars processed-params))
         (result-type (cdr (assoc :result-type processed-params)))
         (type-processed-body
          (if (eql result-type 'value)
              (concat
               (ob-q-preprocess-fun processed-params)
               ;; only function wrap the stripped body
               (ob-q-fun-wrapper (q-strip body) vars))
            (concat (mapconcat
                     (lambda (pair)
                       (format "%s:%s;\n" (car pair) (ob-q-var-to-q (cdr pair))))
                     vars)
                    (q-strip body)))))
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
         (full-body (org-babel-expand-body:q body params processed-params))
         (session-name (cdr (assoc :session processed-params)))
         (raw-output
          (if (string= session-name "none")
              (let* ((tmp-src-file (org-babel-temp-file "q-src-" ".q"))
                     (cmd (format "q %s" (org-babel-process-file-name tmp-src-file))))
                (message "not using session")
                (with-temp-file tmp-src-file (insert full-body))
                (org-babel-eval cmd ""))
            (let* ((session (unless (string= session-name "none")
                              (ob-q-initiate-session session-name))))
            (message "using session")
              (mapconcat
               #'org-trim
               (butlast
                (org-babel-comint-with-output
                   (session ob-q-eoe-output)
                 (insert (q-strip full-body) "\n" ob-q-eoe-indicator)
                 (comint-send-input nil t))
                1)))))); bug with new line, and also bug when initializing session
    (message (format "processed-params are %s" processed-params))
    (message (format "raw-output is %s" raw-output))
    (if (eql 'value (cdr (assoc :result-type processed-params)))
        (substring
         raw-output
         (+
          (length "ob-q-output-start\\n") ;; define a string for start of value
          (string-match-p "ob-q-output-start" raw-output)))
      raw-output)))
;(substring (+ (length "ob-q-output-start") (string-match "ob-q-output-start" " ob-q-output-start hello")))
(defun ob-q-post-process-result (result)
  "Transform the query RESULT with read."
  (message (format "post-processing result=%s" result))
  (read result))

(defun ob-q-var-to-q (var)
  "Convert an elisp VAR into a string of q source code."
  (cond
   ((null var) nil)
   ((or (vectorp var)
        (proper-list-p var))
    (concat
     "("
     (mapconcat #'ob-q-var-to-q var ";") ; do it recursively
     ")"))
   ((listp var)
    (concat
     "("
     (mapconcat #'ob-q-var-to-q (list (car var) (cdr var)) ";") ; do it recursively
     ")"))
   ((symbolp var) (format "`%S" var))
   (t (format "%S" var))))
;; TODO handle date time formats...
;; TODO emacs table are still going to suck...

(defun ob-q-fun-wrapper (body &optional vars)
  "Wraps BODY in a q lambda with VARS as parameters."
  (concat "{["
          (when vars
            (mapconcat
             (lambda (pair)
               (symbol-name (car pair)))
             vars ";"))
          "]\n " (replace-regexp-in-string "\n" ";\n " body) "\n }["
          (when vars
            (mapconcat
             (lambda (pair)
               (ob-q-var-to-q (cdr pair)))
              vars ";"))
          "]"))

(defun ob-q-preprocess-fun (processed-params)
  "Outputs a q-function string depending on PROCESSED-PARAMS to preprocess output."
  ;TODO Use a let statement to find what type it expects, only care if it's verbatim or not
  (when (eql 'value (cdr (assoc :result-type processed-params)))
    (concat
     "{[result]1 \"ob-q-output-start\\n\";"
     "1 .Q.s result;"
     "}")))

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
