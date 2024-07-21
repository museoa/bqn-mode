;;; ob-bqn.el --- org-babel functions for template evaluation  -*- lexical-binding: t; -*-

;; Author: luxbock <luxbock@gmail.com>
;; Version: 0
;; URL: https://github.com/museoa/bqn-mode
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This package provides org-babel functions for template evaluation

;;; Code:

(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(require 'bqn-mode)
;; possibly require modes required for your language

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("bqn" . "bqn"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:bqn '())

;; This function expands the body of a source code block by doing things like
;; prepending argument definitions to the body, it should be called by the
;; `org-babel-execute:bqn' function below. Variables get concatenated in
;; the `mapconcat' form, therefore to change the formatting you can edit the
;; `format' form.
(defun org-babel-expand-body:bqn (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (require 'bqn-mode nil t)
  (let ((vars (org-babel--get-vars (or processed-params (org-babel-process-params params)))))
    (concat
     (mapconcat ;; define any variables
      (lambda (pair)
        (format "%s=%S"
                (car pair) (org-babel-bqn-var-to-bqn (cdr pair))))
      vars "\n")
     "\n" body "\n")))

;; This is the main function which is called to evaluate a code
;; block.
;;
;; This function will evaluate the body of the source code and
;; return the results as emacs-lisp depending on the value of the
;; :results header argument
;; - output means that the output to STDOUT will be captured and
;;   returned
;; - value means that the value of the last statement in the
;;   source code block will be returned
;;
;; The most common first step in this function is the expansion of the
;; PARAMS argument using `org-babel-process-params'.
;;
;; Please feel free to not implement options which aren't appropriate
;; for your language (e.g. not all languages support interactive
;; "session" evaluation).  Also you are free to define any new header
;; arguments which you feel may be useful -- all header arguments
;; specified by the user will be available in the PARAMS variable.
(defun org-babel-execute:bqn (body params)
  "Execute a block of Bqn code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing BQN source code block")
  ;; TODO: Add handling of parameters, session, etc.
  (bqn-comint-evaluate-command body))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:bqn (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  )

(defun org-babel-bqn-var-to-bqn (var)
  "Convert an elisp var into a string of bqn source code
specifying a var of the same value."
  (format "%S" var))

(defun org-babel-bqn-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  )

(defun org-babel-bqn-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (unless (string= session "none")
    (get-buffer-process (bqn-comint-buffer))))

(provide 'ob-bqn)
;;; ob-bqn.el ends here
