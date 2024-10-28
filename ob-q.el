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

;;; Code:

;; (require 'org-macs)
;;(org-assert-version)

(require 'org)
(require 'ob)
(require 'ob-eval)
;; (require 'org-macs)
;; (require 'q-mode) ;; Require q-mode for interactive support

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("q" . "q"))

(defvar org-babel-default-header-args:q '())

;;(defun org-babel-expand-body:q (body params &optional processed-params)
;;  "Expand BODY according to PARAMS, return the expanded body."
;;  (let ((vars (org-babel--get-vars (or processed-params (org-babel-process-params params)))))
;;    (concat
;;     (mapconcat
;;      (lambda (pair)
;;        (format "%s:%S"
;;                (car pair) (org-babel-q-var-to-q (cdr pair))))
;;      vars "\n")
;;     "\n" body "\n")))

(defun org-babel-execute:q (body params)
  "Execute q BODY according to PARAMS.
This function is called by `org-babel-execute-src-block'"
  (let* ((tmp-src-file (org-babel-temp-file "q-src-" ".q"))
         (cmd (format "q %s" (org-babel-process-file-name tmp-src-file))))
    (with-temp-file tmp-src-file (insert body))
    (org-babel-eval cmd "" )))

;;(defun org-babel-prep-session:q (session params)
;;  "Prepare SESSION according to the header arguments specified in PARAMS."
;;  (org-babel-q-initiate-session session))
;;
;;(defun org-babel-q-var-to-q (var)
;;  "Convert an elisp var into a string of q source code
;;specifying a var of the same value."
;;  (format "%S" var))
;;
;;(defun org-babel-q-initiate-session (&optional session)
;;  "If there is not a current inferior-process-buffer in SESSION then create.
;;Return the initialized session."
;;  (unless (string= session "none")
;;    (if (not (comint-check-proc "*q*"))
;;        (progn
;;          (run-q) ;; Starts a new q process with q-mode’s run-q function
;;          (get-buffer "*q*"))
;;      (get-buffer "*q*"))))

(provide 'ob-q)
;;; ob-q.el ends here
