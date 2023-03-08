;;; bqn-mode-map.el --- BQN mode keymap input helpers -*- lexical-binding: t -*-

;; Author: Marshall Lochbaum <mwlochbaum@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/museoa/bqn-mode
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This package provides functions to manage the BQN mode's keymap.

(require 'cl-lib)
(require 'bqn-symbols)

;;; Code:

(defun bqn-mode-map--command-name (name)
  "Return a symbol for the command NAME."
  (intern (concat "bqn-insert-sym-" name)))

(pcase-dolist (`(,name ,symbol ,_) bqn-symbols--list)
  (defalias (bqn-mode-map--command-name name)
    (lambda ()
      (interactive)
      (insert symbol))))

(defun bqn-mode-map--make-base (prefix)
  "Create a new keymap using the key PREFIX."
  (let ((map (make-sparse-keymap)))
    (pcase-dolist (`(,name ,_ ,key) bqn-symbols--list)
      (let ((cmd (bqn-mode-map--command-name name))
            (key (single-key-description key)))
        (define-key map (kbd (concat prefix key)) cmd)))
    (define-key map [menu-bar bqn] (cons "BQN" (make-sparse-keymap "BQN")))
    map))

;; value gets updated by initialization of `bqn-mode-map-prefix'
(defvar bqn-mode-map--keymap nil
  "The keymap for `bqn-mode'.")

(defun bqn-mode-map--set-prefix (prefix new)
  "Set the BQN mode keymap's PREFIX to NEW and recreate the keymap."
  (set-default prefix new)
  (setq bqn-mode-map--keymap (bqn-mode-map--make-base new)))

(defcustom bqn-mode-map-prefix "s-"
  "This stores the keymap prefix for `bqn-mode-map--keymap'.
It is used both to store the new value using
`set-create' and to update `bqn-mode-map--keymap' using
`bqn-mode-map--make-base'. Kill and restart your BQN buffers
to reflect the change."
  :type 'string
  :group 'bqn
  :set #'bqn-mode-map--set-prefix
  :initialize #'custom-initialize-set)

(provide 'bqn-mode-map)

;; Local Variables:
;; coding: utf-8-unix
;; indent-tabs-mode: nil
;; End:

;;; bqn-mode-map.el ends here
