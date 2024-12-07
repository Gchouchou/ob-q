;;; ob-q.el --- org-babel functions for q evaluation

;; Copyright (C) Justin Yu <jusytinyu@gmail.com>

;; Author: Justin Yu
;; Keywords: literate programming, reproducible research
;; Created 7 Dec 2024
;; Version: 0.1

;; This file is not part of GNU Emacs.

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
;;; Package-Requires: ((emacs "28.1"))

;;; Code:

(require 'ob)
(require 'ob-eval)
(require 'ob-comint)
(require 'q-mode "q-mode" t) ; not required but highly recommended

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("q" . "q"))

(defconst ob-q-soe-indicator "1 \"org_babel_q_soe\";"
  "String to indicate start of evaluation output.")
(defconst ob-q-soe-output "org_babel_q_soe"
  "String to indicate start of evaluation output.")
(defconst ob-q-eoe-indicator "1 \"org_babel_q_eoe\\n\";"
  "String to indicate that evaluation has completed.")
(defconst ob-q-eoe-output "org_babel_q_eoe"
  "String to indicate that evaluation has completed.")

(defcustom ob-q-program "q"
  "Program name for invoking an inferior q."
  :type 'file
  :group 'ob-q)

(defun org-babel-expand-body:q (body params &optional processed-params)
  "Expand BODY according to PARAMS and PROCESSED-PARAMS, return the expanded body."
  (let* ((body (ob-q-strip (concat (when-let ((prologue (cdr (assoc :prologue processed-params))))
                                     (concat prologue "\n"))
                                   body "\n"
                                   (cdr (assoc :epilogue processed-params)))))
         (vars (org-babel--get-vars processed-params))
         (type-processed-body
          (pcase (cdr (assoc :result-type processed-params))
            ('value (concat (ob-q-preprocess-fun processed-params)
                            (ob-q-fun-wrapper body vars)))
            ('output (concat (mapconcat
                              (lambda (pair)
                                (format "%s:%s;\n" (car pair) (ob-q-var-to-q (cdr pair))))
                              vars)
                             body)))))
    type-processed-body))

(defun org-babel-execute:q (body params)
  "Execute q BODY according to PARAMS.
This function is called by `org-babel-execute-src-block'"
  (unless (featurep 'ob-q) (require 'ob-q))
  (let* ((processed-params (org-babel-process-params params))
         (full-body (org-babel-expand-body:q body params processed-params))
         (session-name (cdr (assoc :session processed-params)))
         (session (unless (string= session-name "none")
                    (ob-q-initiate-session session-name)))
         (async (org-babel-comint-use-async params))
         (result-type (cdr (assoc :result-type processed-params))))
    (if async
        nil
      (let ((raw-output
             (pcase session-name
               ("none" (let* ((tmp-src-file (org-babel-temp-file "q-src-" ".q"))
                              (cmd (format "%s %s"
                                           (if (featurep 'q-mode) q-program ob-q-program)
                                           (org-babel-process-file-name tmp-src-file))))
                         (with-temp-file tmp-src-file (insert full-body))
                         (org-babel-eval cmd "")))
               (_ (mapconcat
                   #'org-trim
                   (butlast
                    (org-babel-comint-with-output
                        (session ob-q-eoe-output)
                      (insert (ob-q-strip full-body) "\n" ob-q-eoe-indicator)
                      (comint-send-input nil t))
                    1)
                   "\n")))))
        (pcase result-type
          ('value (let ((raw-value (substring
                                    raw-output
                                    (+ (length ob-q-soe-output)
                                       (string-match-p ob-q-soe-output raw-output)))))
                    (if (member "verbatim" (cdr (assoc :result-params processed-params)))
                        raw-value
                      (ob-q-post-process-result raw-value))))
          ('output raw-output))))))

(defun ob-q-post-process-result (result)
  "Convert the RESULT to elisp."
  (let* ((delim (string-match-p ";" result))
         (type (string-to-number (substring result 0 delim)))
         (split-result (substring result (+ delim 1))))
    (cond
     ((or (< type 0) (= type 10)) (ob-q-read-atom split-result type))
     ((= type 0) (split-string split-result ";"))
     ((<= 1 type 20)
      ;; it's a list
      (mapcar (lambda (q-atom)
                (ob-q-read-atom q-atom (- type)))
              (split-string split-result ";")))
     ((<= 98 type 99)
      ;; it's a table
      (let ((table
             (mapcar (lambda (row)
                       (split-string row ";"))
                     (split-string split-result "\n" t))))
        (append (list (car table)) '(hline) (cdr table))))
     (t split-result))))

(defun ob-q-read-atom (q-atom type)
  "Convert a Q-ATOM string of TYPE to a elisp atom."
  (let ((q-atom (org-trim q-atom)))
    (pcase type
      ;; -11 for symbol
      (-11 (make-symbol (substring q-atom 1)))
      ;; 10 for string
      (10 (substring (replace-regexp-in-string "\\\\n" "\n" q-atom) 1 -1))
      ;; -4 for byte
      (-4 (string-to-number (substring q-atom 2) 16))
      ;; all the other numbers
      ((pred (<= -9)) (string-to-number q-atom))
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
   ;; Match a datetime string
   ((and (stringp var)
         (or (string-match-p
              "^\\([0-9]\\{4\\}\\)\\.[0-1][0-9]\\.[0-3][0-9]\\(D\\([0-2][0-9]\\(:\\([0-5][0-9]\\(:[0-5][0-9]\\(\\.[0-9]*\\)?\\)?\\)?\\)?\\)?\\)?$"
              var)
             (string-match-p
              "[0-2][0-9]:\\([0-5][0-9]\\(:\\([0-5][0-9]\\(\\.[0-9]*\\)?\\)?\\)?\\)?"
              var)
             (string-match-p
              "\\([0-9]+\\)D\\([0-2][0-9]:\\([0-5][0-9]\\(:\\([0-5][0-9]\\(\\.[0-9]*\\)?\\)?\\)?\\)?\\)?"
              var)))
    (format "%s" var))
   (t (format "%S" var))))
;; TODO emacs table

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
  (concat
   "{[result]\n "
   ob-q-soe-indicator "\n "
   (if (member "verbatim" (cdr (assoc :result-params processed-params)))
       "1 .Q.s result;" ; when in verbatim use q string maker
        "\
rtype:type result;
 1 string rtype;
 1 \";\";
 1 $[
 rtype=10h;.Q.s result;
 rtype within (0;20);\";\" sv .Q.s each result;
 .Q.qt result;
 \"\\n\" sv \";\" 0: result;
 rtype=99h;
 \"key;value\\n\",\"\\n\" sv {[d;k] (.Q.s1 k),\";\",.Q.s1 d[k] }[result;] each key result;
 .Q.s result
 ];
 1 \"\\n\";")
   "}"))

(defun ob-q-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (unless (string= session "none")
    (let* ((session (or session "*org-babel-q*"))
           (buffer (get-buffer-create session)))
      (cond
       ((comint-check-proc buffer) buffer)                   ; already a q process
       ((featurep 'q-mode)                                   ; check if q-mode is loaded
        (let* ((process (q q-host q-user (q-default-args)))  ; start q with defaults
               (buffer2 (get-buffer q-active-buffer)))
          (kill-buffer buffer)
          (with-current-buffer buffer2
            (rename-buffer session)                          ; massage the buffer name
            (setq q-active-buffer session)
            (current-buffer))))
       (t (with-current-buffer buffer                        ; barebones q-session
            (message "Starting q with: \"%s\"" ob-q-program)
            (comint-mode)
            (comint-exec buffer "ob-q" ob-q-program nil nil)
            (setq-local comint-prompt-regexp "^q)+")
            (current-buffer)))))))

;;; Taken from github psaris/q-mode, decoupling the package
(defun ob-q-strip (text)
  "Strip TEXT of all trailing comments, newlines and excessive whitespace."
  (let* ((text (replace-regexp-in-string "^\\(?:[^\\\\].*\\)?[ \t]\\(/.*\\)\n" "" text t t 1)) ; / comments
         (text (replace-regexp-in-string "^/.+$" "" text t t))                                 ; / comments
         (text (replace-regexp-in-string "[ \t\n]+$" "" text t t))                             ; excess white space
         (text (replace-regexp-in-string "\n[ \t]+" "" text t t)))                             ; fold functions
    text))

;;;###autoload
;;;(defvar org-babel-default-header-args:q (list '(:results . "verbatim" )))

(provide 'ob-q)
;;; ob-q.el ends here
