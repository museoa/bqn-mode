;; bqn-help.el --- Help for BQN mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Jeff Young
;;
;; Author: Jeff Young <https://github.com/doyougnu>
;; Maintainer: Jeff Young <jeff@doyougnu.xyz>
;; Created: October 29, 2021
;; Modified: October 29, 2021
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;  Description
;;  This module does two things:
;;    1. Ties documentation functions from bqn-symbols-doc to eldoc mode
;;    2. Implements simple interactive documentation functions for help-at-point
;;
;;  TODOs
;;    1. Add function that fuzzy matches a symbol by name and provides
;;       a minibuffer of candidates
;;    2. font-face for documentation
;;    3. searchable markdown documentation from bqn source repo
;;    4. Add glossary
;;; Code:

(require 'bqn-symbols)
(require 'bqn-symbols-doc)
(require 'bqn-syntax)       ; for font-lock in doc mode
(require 'cl-lib)

;; Eldoc functions
(defvar bqn-help--function-regexp
  (regexp-opt
   (let* ((symbols (mapcar #'cadr bqn--symbols))
          (others  (bqn-symbols-doc--symbols)))
     (cl-union symbols others)))
  "Regex to match BQN functions")

(defun bqn-help--eldoc ()
  "Returns doc string for thing at point, or nil."
  (when (looking-at bqn-help--function-regexp)
    (bqn-symbols-doc-get-short-doc (match-string 0))))

;;; Help functions
(defun bqn-help--close-documentation-buffer ()
  "Close the active documentation window"
  (interactive)
  (quit-window))

(defvar bqn-help-documentation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'bqn-help--close-documentation-buffer)
    map)
  "Keymap for documentation buffers")

(define-derived-mode bqn-help-documentation-mode fundamental-mode
  "BQN Documentation"
  "Major mode for displaying BQN documentation"
  (use-local-map bqn-help-documentation-mode-map)
  (setq-local font-lock-defaults bqn--token-syntax-types)
  (setq-local eldoc-documentation-function 'bqn-help--eldoc))

;; General interactive help
;; TODO: a custom face for documentation
(defvar *bqn-help-documentation-buffer-name* "*bqn-help*")

(defun bqn-help--bqn-symbols-info (test)
  "Given a functionp, TEST, find entry for THING in BQN--SYMBOLS using TEST."
  (seq-filter test bqn--symbols))

(defun bqn-help-symbol-at-point-is-called ()
  "Show the canonical name of the symbol at point in the minibuffer"
  (interactive)
  (when (looking-at bqn-help--function-regexp))
  (if-let* ((symbol (match-string 0))
            (result (bqn-help--bqn-symbols-info
                     (lambda (v) (equal (cadr v) symbol)))))
      (message (format "We call symbol %s %s" symbol (caar result)))
    (message (format "No name for %s found" symbol))))

(defun bqn-help-symbol-info-at-point ()
  "Get multi-line documentation for the thing at point, or nil"
  (interactive)
  (when (looking-at bqn-help--function-regexp)
    (if-let* ((symbol (match-string 0))
              (long   (bqn-symbols-doc-get-long-doc symbol))
              (extra  (bqn-symbols-doc-get-extra-doc symbol))
              (sep    "\n\n========================================\n\n")
              (doc-buffer (get-buffer-create
                           *bqn-help-documentation-buffer-name*)))
        (with-current-buffer doc-buffer ;; we have a hit
          (read-only-mode 0)            ; set read only
          (delete-region (point-min) (point-max))
          (insert (concat long sep extra))
          (goto-char (point-min))
          (bqn-help-documentation-mode)
          (read-only-mode 1)            ; unset read only
          (pop-to-buffer doc-buffer))
      ;; a miss
      (message (format "No help for %s found!" symbol)))))


(provide 'bqn-help)

;; Local Variables:
;; coding: utf-8-unix
;; indent-tabs-mode: nil
;; End:

;; bqn-help.el ends here
