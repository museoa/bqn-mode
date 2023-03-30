;;; bqn-syntax.el --- BQN font lock definitions -*- lexical-binding: t -*-

;; Author: Marshall Lochbaum <mwlochbaum@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/museoa/bqn-mode
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This package provides definitions for font lock syntax highlighting BQN code.

;;; Code:

(require 'cl-lib)
(require 'bqn-symbols)

(defface bqn-syntax-block-face
  '((t (:inherit font-lock-function-name-face)))
  "Face used for BQN curly braces."
  :group 'bqn)

(defface bqn-syntax-paren-face
  '((t (:inherit default)))
  "Face used for BQN parentheses."
  :group 'bqn)

(defface bqn-syntax-header-face
  '((t (:inherit default)))
  "Face used for BQN header delimiters : and ; ."
  :group 'bqn)

(defface bqn-syntax-list-face
  '((t (:inherit font-lock-builtin-face
        :family "BQN386 Unicode")))
  "Face used for BQN list characters: angle brackets and ligature."
  :group 'bqn)

(defface bqn-syntax-separator-face
  '((t (:inherit font-lock-builtin-face
        :family "BQN386 Unicode")))
  "Face used for BQN expression separators."
  :group 'bqn)

(defface bqn-syntax-arrow-face
  '((t (:inherit 'default
        :family "BQN386 Unicode")))
  "Face used for BQN assignment and return arrows."
  :group 'bqn)

(defface bqn-syntax-function-face
  '((t (:inherit font-lock-type-face
        :family "BQN386 Unicode")))
  "Face used for BQN functions."
  :group 'bqn)

(defface bqn-syntax-one-modifier-face
  '((t (:inherit font-lock-preprocessor-face
        :family "BQN386 Unicode")))
  "Face used for BQN 1-modifiers."
  :group 'bqn)

(defface bqn-syntax-two-modifier-face
  '((t (:inherit font-lock-keyword-face
        :family "BQN386 Unicode")))
  "Face used for BQN 2-modifiers."
  :group 'bqn)

(defface bqn-syntax-subject-face
  '((t (:inherit font-lock-variable-name-face
        :family "BQN386 Unicode")))
  "Face used for BQN subjects."
  :group 'bqn)

(defface bqn-syntax-nothing-face
  '((t (:inherit font-lock-constant-face
        :family "BQN386 Unicode")))
  "Face used for BQN Nothing (·)."
  :group 'bqn)

(defface bqn-syntax-number-face
  '((t (:inherit font-lock-constant-face)))
  "Face used for BQN numeric literals."
  :group 'bqn)

(defvar bqn-syntax--token-types
  '((("'.'\\|@" . font-lock-string-face)
     ("[{}]" . 'bqn-syntax-block-face)
     ("[()]" . 'bqn-syntax-paren-face)
     ("[:;?]" . 'bqn-syntax-header-face)
     ("[⟨⟩‿]" . 'bqn-syntax-list-face)
     ("[⋄,]" . 'bqn-syntax-separator-face)
     ("[←⇐↩→]" . 'bqn-syntax-arrow-face)
     ("·" . 'bqn-syntax-nothing-face)
     ("[𝔽𝔾𝕎𝕏𝕊+×÷⋆√⌊⌈|¬∧∨<>≠=≤≥≡≢⊣⊢⥊∾≍⋈↑↓↕«»⌽⍉/⍋⍒⊏⊑⊐⊒∊⍷⊔!⍕⍎-]\\|•?\\_<[A-Z][A-Z_a-z0-9π∞¯]*\\_>" . 'bqn-syntax-function-face)
     ("[˙˜˘¨⌜⁼´˝`]\\|•?\\_<_[A-Za-z][A-Z_a-z0-9π∞¯]*\\|_𝕣\\_>" . 'bqn-syntax-one-modifier-face)
     ("[∘○⊸⟜⌾⊘◶⎉⚇⍟⎊]\\|_𝕣_\\|•?\\_<_[A-Za-z][A-Z_a-z0-9π∞¯]*_\\_>" . 'bqn-syntax-two-modifier-face)
     ("[𝕗𝕘𝕨𝕩𝕤𝕣]\\|•\\|•?\\_<[a-z][A-Z_a-z0-9π∞¯]*\\_>" . 'bqn-syntax-subject-face)
     ("\\_<¯?\\(\\([0-9]+\\.\\)?[0-9]+\\(e¯?[0-9]+\\)?\\|π\\|∞\\)\\(i¯?\\(\\([0-9]+\\.\\)?[0-9]+\\(e¯?[0-9]+\\)?\\|π\\|∞\\)\\)?\\_>" . 'bqn-syntax-number-face)
     ("[^ \r\n]" . 'error))
    nil nil nil))

(defvar bqn-syntax--table
  (let ((table (make-syntax-table)))
    (cl-loop for s in bqn-symbols--list
             do (modify-syntax-entry (aref (cl-second s) 0) "." table))
    (cl-loop for s in (append "$%&*+-/<=>|" nil)
             do (modify-syntax-entry s "." table))
    (modify-syntax-entry ?#  "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?¯  "_" table)
    (modify-syntax-entry ?π  "_" table)
    (modify-syntax-entry ?∞  "_" table)
    (modify-syntax-entry ?⟩  ")⟨" table)
    (modify-syntax-entry ?⟨  "(⟩" table)
    table)
  "Syntax table for `bqn-mode'.")

(provide 'bqn-syntax)

;;; bqn-syntax.el ends here
