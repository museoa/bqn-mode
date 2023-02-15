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

(eval-and-compile
  (defun bqn-mode-map--make-key-command-sym (n)
    (intern (concat "insert-sym-bqn-" n))))

(cl-macrolet ((make-insert-functions ()
             `(progn
                ,@(mapcar (lambda (command)
                              `(defun ,(bqn-mode-map--make-key-command-sym (car command)) ()
                                 (interactive)
                                 (insert ,(cadr command))))
                          bqn-symbols--list))))
  (make-insert-functions))

(defun bqn-mode-map--kbd (definition)
  "Function wrapper for `kbd' with arg DEFINITION."
  (if (functionp #'kbd)
      (kbd definition)
    (eval `(kbd ,definition))))

(defun bqn-mode-map--make-base (prefix)
  "Create a new keymap using the key PREFIX."
  (let ((map (make-sparse-keymap)))
    (dolist (command bqn-symbols--list)
      (let ((key (single-key-description (caddr command))))
        (define-key map (bqn-mode-map--kbd (concat prefix key)) (bqn-mode-map--make-key-command-sym (car command)))))
    (define-key map [menu-bar bqn] (cons "BQN" (make-sparse-keymap "BQN")))
    map))

;; value gets updated by initialization of bqn-mode-map-prefix
(defvar bqn-mode-map--keymap nil
  "The keymap for ‘bqn-mode’.")

(defun bqn-mode-map--set-prefix (prefix new)
  "Set the BQN mode keymap's PREFIX to NEW and recreate the keymap."
  (set-default prefix new)
  (setq bqn-mode-map--keymap (bqn-mode-map--make-base new)))

(defcustom bqn-mode-map-prefix "s-"
  "This stores the keymap prefix for ‘bqn-mode-map--keymap’.
It is used both to store the new value using
‘set-create’ and to update ‘bqn-mode-map--keymap’ using
`bqn-mode-map--make-base'. Kill and restart your BQN buffers
to reflect the change."
  :type 'string
  :group 'bqn
  :set 'bqn-mode-map--set-prefix
  :initialize 'custom-initialize-set)

(provide 'bqn-mode-map)

;; Local Variables:
;; coding: utf-8-unix
;; indent-tabs-mode: nil
;; End:

;;; bqn-mode-map.el ends here
