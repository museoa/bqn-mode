;;; bqn-key-prefix.el --- BQN input key prefix bindings -*- lexical-binding: t -*-

;; Author: Marshall Lochbaum <mwlochbaum@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/museoa/bqn-mode
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; This package provides functions to get and set the key prefix used to start
;; BQN symbol input.

(require 'cl-lib)
(require 'quail)
(require 'bqn-symbols)

;;; Code:

(quail-define-package "BQN-Z" "UTF-8" "⍉" t
                      "Input mode for BQN"
                      '(("\t" . quail-completion))
                      t                 ; forget-last-selection
                      nil               ; deterministic
                      nil               ; kbd-translate
                      t                 ; show-layout
                      nil               ; create-decode-map
                      nil               ; maximum-shortest
                      nil               ; overlay-plist
                      nil               ; update-translation-function
                      nil               ; conversion-keys
                      t                 ; simple
                      )

(defvar bqn-key-prefix--transcription-alist)
(defun bqn-key-prefix--set (prefix new)
  "Set a key PREFIX to the NEW one."
  (quail-select-package "BQN-Z")
  (quail-install-map
   (let* ((prefix (string new))
          (bqn-key-prefix--transcription-alist
           (cl-loop for command in bqn-symbols--list
                    collect (cons (concat prefix (char-to-string (cl-third command)))
                                  (cl-second command)))))
     (quail-map-from-table
      '((default bqn-key-prefix--transcription-alist)))))
  (set-default prefix new))

(defun bqn-key-prefix--initialize (prefix new)
  "Initialize the key PREFIX with the NEW one."
  (custom-initialize-default prefix new)
  (bqn-key-prefix--set prefix (eval new)))

(defcustom bqn-key-prefix ?\\
  "Set a character to serve as prefix key for BQN symbol input."
  :type 'character
  :group 'bqn
  :initialize #'bqn-key-prefix--initialize
  :set #'bqn-key-prefix--set)

(provide 'bqn-key-prefix)

;;; bqn-key-prefix.el ends here
