;;; bqn-mode.el --- Emacs mode for BQN -*- lexical-binding: t -*-

;; Emacs bqn-mode is derived from gnu-apl-mode,
;; which is copyright 2013-2015 Elias Mårtenson <lokedhs@gmail.com>.
;; Changes are copyright 2021 Marshall Lochbaum <mwlochbaum@gmail.com>.

;; Author: Marshall Lochbaum <mwlochbaum@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/museoa/bqn-mode
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Emacs mode for BQN: currently keymap only

;; There are two ways to access the BQN keymap:
;; - When editing a BQN file, use keys with the super (s-) modifier.
;; - Enable backslash prefixes by entering C-\ (`toggle-input-method')
;;   then BQN-Z. Then enter backslash \ before a key.

;;; Code:

(require 'bqn-mode-map)
(require 'bqn-key-prefix)
(require 'bqn-syntax)
(require 'bqn-comint)
(require 'bqn-help)
(require 'bqn-glyph-mode)
(require 'bqn-keymap-mode)

;;;###autoload
(defgroup bqn nil
  "Major mode for interacting with the BQN interpreter."
  :prefix 'bqn
  :group 'languages)

;;;###autoload
(define-derived-mode bqn-mode prog-mode "BQN"
  "Major mode for editing BQN files."
  :syntax-table bqn-syntax--table
  :group 'bqn
  (use-local-map bqn-mode-map--keymap)
  (setq-local font-lock-defaults bqn-syntax--token-types)
  (setq-local eldoc-documentation-function 'bqn-help--eldoc)
  (setq-local comment-start "# "))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bqn\\'" . bqn-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("bqn" . bqn-mode))

(provide 'bqn-mode)

;; Local Variables:
;; coding: utf-8-unix
;; indent-tabs-mode: nil
;; End:

;;; bqn-mode.el ends here
