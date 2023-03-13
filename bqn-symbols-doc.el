;;; bqn-symbols-doc.el --- Documentation table for BQN symbols -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Jeff Young
;;
;; Author: Jeff Young <https://github.com/doyougnu>
;; Maintainer: Jeff Young <jeff@doyougnu.xyz>
;; Created: October 29, 2021
;; Modified: October 29, 2021
;; Version: 0.0.1
;; Keywords: convenience data docs
;; URL: https://github.com/museoa/bqn-mode
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; - used in bqn-help.el to implement eldoc for bqn-mode
;;
;;; Code:

;; Arrays and hashes are not very Lispy, however they will be employed here
;; because we want the lowest latency latency possible for an end-user-facing
;; structure. For all intents and purposes, this table should be regarded as
;; read-only; indeed, it is "cached" at byte-compile time via eval-when-compile
(defconst bqn-symbols-doc--symbol-doc-table
  (eval-when-compile
    (let ((table '(
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The format of each entry follows the model below
;;; (<symb> . [short-description long-description extra-description])
;;; where:
;;; - <symb> is the symbol to be described
;;; - short-description should be no more than 80 characters (to fit modeline)
;;; - long-description should state what symbol is and what forms symbol has
;;; - extra-description should provide examples, preferably REPL-like
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                   ("" . [""
                          ""
                          ""])))
          (ht (make-hash-table :test 'equal)))
      (dolist (entry table)
        (puthash (car entry) (cdr entry) ht))
      ht))
  "This table associates BQN symbols as hash-keys to a 3-vector of docstrings.
Position 0 is short description for eldoc, position 1 is a long description,
and position 2 is any extra description.")

(defun bqn-symbols-doc--symbols ()
  "Return a list of bqn symbols for which we have docs."
  (let (symbols)
    (maphash (lambda (sym _) (push sym symbols))
             bqn-symbols-doc--symbol-doc-table)
    symbols))

(defun bqn-symbols-doc--get-doc (symbol doc)
  "Retrieve a docstring for SYMBOL, given a stringp SYMBOL and a keywordp DOC.
Return nil if no docstring is found."
  (let ((docs (gethash symbol bqn-symbols-doc--symbol-doc-table)))
    (and docs (aref docs (cond ((eq doc :short) 0)
                               ((eq doc :long)  1)
                               ((eq doc :extra) 2))))))

(defun bqn-symbols-doc-get-short-doc (symbol)
  "Given SYMBOL as stringp, retrieve a single-line doc string for SYMBOL, or nil."
  (bqn-symbols-doc--get-doc symbol :short))

(defun bqn-symbols-doc-get-long-doc (symbol)
  "Given SYMBOL as stringp, retrieve a multi-line doc string for SYMBOL, or nil."
  (bqn-symbols-doc--get-doc symbol :long))

(defun bqn-symbols-doc-get-extra-doc (symbol)
  "Given SYMBOL as stringp, retrieve a extra doc string for SYMBOL, or nil."
  (bqn-symbols-doc--get-doc symbol :extra))

(provide 'bqn-symbols-doc)

;;; bqn-symbols-doc.el ends here
