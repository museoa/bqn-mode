;;; bqn-comint.el --- BQN comint mode -*- lexical-binding: t -*-

;; Author: Marshall Lochbaum <mwlochbaum@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/museoa/bqn-mode
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This package defines a comint derived mode for the BQN interpreter.

;;; Code:

(require 'comint)
(require 'bqn-key-prefix)
(require 'bqn-syntax)

(defcustom bqn-comint-interpreter-path "bqn"
  "Path to the BQN interpreter used by `bqn-comint-run-process`."
  :type 'string
  :group 'bqn)

(defcustom bqn-comint-interpreter-arguments '()
  "Commandline arguments to pass to the BQN interpreter."
  :type 'string
  :group 'bqn)

(defvar bqn-comint-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; add keymaps here
    map)
  "Basic mode to run BQN.")

(defcustom bqn-comint-prompt-regexp "^   "
  "Prompt for BQN."
  :type 'regexp
  :group 'bqn)

(defvar bqn-comint--process-name "BQN"
  "Name of BQN comint process.")

(defcustom bqn-comint-*process-buffer-name* "*BQN*"
  "Name of buffer which holds BQN process."
  :type 'string
  :group 'bqn)

(defcustom bqn-comint-flash-on-send t
  "When non-nil flash the region sent to BQN process."
  :type 'boolean
  :group 'bqn)

(defun bqn-comint--flash-region (start end &optional timeout)
  "Temporarily highlight region from START to END for TIMEOUT seconds."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))

(defun bqn-comint-process-ensure-session ()
  "Check for a running `bqn-comint-*process-buffer-name*'.
If it doesn't exist, create and return it; else, return the existing one."
  (or (get-process bqn-comint--process-name)
      (progn
        (bqn-comint-run-process)
        (get-process bqn-comint--process-name))))

;;;###autoload
(defun bqn-comint-run-process ()
  "Run an inferior BQN process inside Emacs."
  (interactive)
  (let* ((bqn-program bqn-comint-interpreter-path)
	     (buffer (comint-check-proc bqn-comint--process-name)))
    ;; pop to the "*BQN*" buffer when the process is dead, the buffer
    ;; is missing or it's got the wrong mode.
    (pop-to-buffer-same-window
     (if (or buffer (comint-check-proc (current-buffer)))
	     (get-buffer-create (or buffer bqn-comint-*process-buffer-name*))
       (current-buffer)))
    ;; create the comint process unless there is a buffer already
    (unless buffer
      (apply #'make-comint-in-buffer
             bqn-comint--process-name
             buffer
	         bqn-program bqn-comint-interpreter-arguments)
      (switch-to-buffer-other-window bqn-comint-*process-buffer-name*)
      (bqn-comint-mode)
      (set-input-method "BQN-Z"))))

(defun bqn-comint-process-execute-region (start end &optional dont-follow)
  "Send the region bounded by START and END to the bqn-comint-process-session.

When DONT-FOLLOW is non-nil, maintain focus on the buffer where
the function was called from."
  (interactive "r")
  (when (= start end)
    (error
     (concat "Attempt to send empty region to "
             bqn-comint-*process-buffer-name*)))
  (when bqn-comint-flash-on-send
    (bqn-comint--flash-region start end))
  (let ((region (buffer-substring-no-properties start end))
        (session (bqn-comint-process-ensure-session))
        (buffer (current-buffer)))
    (pop-to-buffer (process-buffer session))
    (goto-char (point-max))
    (insert (format "\n%s\n" region))
    (comint-send-input)
    (when (or dont-follow nil)
      (pop-to-buffer buffer))))

(defun bqn-comint-process-execute-line-and-follow ()
  "Send the current line to BQN process and focus BQN process buffer."
  (interactive)
  (bqn-comint-process-execute-region (line-beginning-position) (line-end-position)))

(defun bqn-comint-process-execute-buffer-and-follow ()
  "Send the current buffer to BQN process and focus BQN process buffer."
  (interactive)
  (bqn-comint-process-execute-region (point-min) (point-max)))

(defun bqn-comint-process-execute-line ()
  "Send the line containing the point to the BQN process."
  (interactive)
  (bqn-comint-process-execute-region (line-beginning-position) (line-end-position) t))

(defun bqn-comint-process-execute-buffer ()
  "Send the current buffer to BQN process."
  (interactive)
  (bqn-comint-process-execute-region (point-min) (point-max) t))

(define-derived-mode bqn-comint-mode comint-mode "BQN interactive"
  "Major mode for inferior BQN processes."
  :syntax-table bqn-syntax--table
  (setq-local font-lock-defaults bqn-syntax--token-types)
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t)
  (setq comint-prompt-regexp bqn-comint-prompt-regexp)
  (setq comint-prompt-read-only nil)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) bqn-comint-prompt-regexp))

(provide 'bqn-comint)

;; Local Variables:
;; coding: utf-8-unix
;; indent-tabs-mode: nil
;; End:

;;; bqn-comint.el ends here
