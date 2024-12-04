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

(defvar ob-q-soe-indicator "1 \"org_babel_q_soe\";\n "
  "String to indicate start of evaluation output.")
(defvar ob-q-soe-output "org_babel_q_soe"
  "String to indicate start of evaluation output.")
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
         (isvalue (eql 'value (cdr (assoc :result-type processed-params))))
         (raw-output
          (if (string= session-name "none")
              (let* ((tmp-src-file (org-babel-temp-file "q-src-" ".q"))
                     (cmd (format "q %s" (org-babel-process-file-name tmp-src-file))))
                (with-temp-file tmp-src-file (insert full-body))
                (org-babel-eval cmd ""))
            (let* ((session (unless (string= session-name "none")
                              (ob-q-initiate-session session-name))))
              (mapconcat
               #'org-trim
               (butlast
                (org-babel-comint-with-output
                   (session ob-q-eoe-output)
                 (insert (q-strip full-body) "\n" ob-q-eoe-indicator)
                 (comint-send-input nil t))
                1)
               "\n"))))); bug with new line, and also bug when initializing session
    (message (format "raw output is %s" raw-output))
    (if isvalue
        (let ((raw-value
               (substring
                raw-output
                (+ (length ob-q-soe-output) ;; define a string for start of value
                   (string-match-p ob-q-soe-output raw-output)))))
          (if (or (member "verbatim" (cdr (assoc :result-params processed-params)))
               (not isvalue))
              raw-value
            (ob-q-post-process-result raw-value)))
      raw-output)))

(defun ob-q-post-process-result (result)
  "Convert the RESULT to elisp list."
  (message (format "pre-proccessed-result is %s" result))
  (let* ((delim (string-match-p ";" result))
         (type (string-to-number (substring result 0 delim)))
         (split-result (substring result (+ delim 1))))
    ;(message (format "split-result is %s" split-result))
    (cond
     ((or (< type 0) (= type 10)) (ob-q-read-atom split-result type))
     ((= type 0) (split-string split-result ";"))
     ((<= 1 type 20)
      ;; it's a list
      (mapcar (lambda (q-atom)
                (ob-q-read-atom q-atom (- type))) (split-string split-result ";")))
     ((<= 98 type 99)
      ;; it's a table
      (mapcar (lambda (row)
                (split-string row ";"))
              (split-string split-result "\n" t)))
     (t split-result))))

(defun ob-q-read-atom (q-atom type)
  "Convert a Q-ATOM string of TYPE to a elisp atom."
  ;(message (format "processing %s" q-atom))
  (let ((q-atom (org-trim q-atom)))
   (pcase type
     ; -11 for symbol
     (-11 (make-symbol (substring q-atom 1)))
     ; 10 for string
     (10 (substring (replace-regexp-in-string "\\\\n" "\n" q-atom) 1 -1))
     ; -4 for byte
     (-4 (string-to-number (substring q-atom 2) 16))
     ; all the other numbers
     ((pred (<= -9)) (string-to-number q-atom))
     ; TODO handling date time
     (_ q-atom))))

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
  (when (eql 'value (cdr (assoc :result-type processed-params)))
    (concat
     "{[result]\n "
     ob-q-soe-indicator
     (if (member "verbatim" (cdr (assoc :result-params processed-params)))
         ;; when in verbatim use q string maker
         "1 .Q.s result;\n "
       ;; or else try to make something parsable
       (mapconcat
        #'identity
        '("rtype:type result;"
        "1 string rtype;"
        "1 \";\";"
        "1 $["
        "rtype=10h;.Q.s result;"
        ;; lists can be sv'ed
        "rtype within (0;20);\";\" sv .Q.s each result;"
        ;; how to parse a table?
        ".Q.qt result;"
        "\"\\n\" sv \";\" 0: result;"
        "rtype=99h;"
        "\"\\n\" sv {[d;k] (.Q.s1 k),\";\",.Q.s1 d[k] }[result;] each key result;"
        ".Q.s result"
        "];"
        "1 \"\\n\";\n ")
        "\n "))
     "}")))

(defun ob-q-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (unless (string= session "none")
    (if (get-buffer-process (get-buffer session))
        (get-buffer session)
      (prog2 (when (get-buffer session) (kill-buffer session))
          (let ((buffer (prog2 (q)
                            (get-buffer q-active-buffer)
                          (setq q-active-buffer nil))))
            (when buffer
              (with-current-buffer buffer
                (rename-buffer session)
                (current-buffer))))))))


;;;###autoload
;(defvar org-babel-default-header-args:q (list '(:results . "verbatim" )))

(provide 'ob-q)
;;; ob-q.el ends here
