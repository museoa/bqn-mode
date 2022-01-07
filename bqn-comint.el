;;; bqn-comint --- BQN command interface -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;;
;;;
;;; Code:

(require 'comint)
(require 'bqn-syntax)

(defvar bqn-interpreter-path "BQN"
  "Path to the BQN interpreter used by `run-bqn`.")

(defvar bqn-interpreter-arguments '()
  "Commandline arguments to pass to the BQN interpreter.")

(defvar bqn-comint-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; add keymaps here
    map)
  "Basic mode to run BQN.")

(defvar bqn-comint-prompt-regexp "^   "
  "Prompt for BQN.")

(defvar bqn-keyboard-map
  "
┌────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬─────────┐
│~ ¬ │! ⎉ │@ ⚇ │# ⍟ │$ ◶ │% ⊘ │^ ⎊ │& ⍎ │* ⍕ │( ⟨ │) ⟩ │_ √ │+ ⋆ │Backspace│
│` ˜ │1 ˘ │2 ¨ │3 ⁼ │4 ⌜ │5 ´ │6 ˝ │7   │8 ∞ │9 ¯ │0 • │- ÷ │= × │         │
├────┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬──────┤
│Tab    │Q ↙ │W 𝕎 │E ⍷ │R 𝕣 │T ⍋ │Y   │U   │I ⊑ │O ⊒ │P ⍳ │{ ⊣ │} ⊢ │|     │
│       │q ⌽ │w 𝕨 │e ∊ │r ↑ │t ∧ │y   │u ⊔ │i ⊏ │o ⊐ │p π │[ ← │] → │\     │
├───────┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴─┬──┴──────┤
│Caps    │A ↖ │S 𝕊 │D   │F 𝔽 │G 𝔾 │H « │J   │K ⌾ │L » │: · │\" ˙ │Enter    │
│Lock    │a ⍉ │s 𝕤 │d ↕ │f 𝕗 │g 𝕘 │h ⊸ │j ∘ │k ○ │l ⟜ │; ⋄ │' ↩  │         │
├────────┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬──┴─────────┤
│Shift      │Z ⋈ │X 𝕏 │C   │V ⍒ │B ⌈ │N   │M ≢ │< ≤ │> ≥ │? ⇐ │Shift       │
│           │z ⥊ │x 𝕩 │c ↓ │v ∨ │b ⌊ │n   │m ≡ │, ∾ │. ≍ │/ ≠ │            │
└───────────┴────┴────┴────┴────┴────┴────┴────┴────┴────┴────┴────────────┘
                             Space: ‿
"
  "Keyboard map for BQN.")

(defvar *bqn-keymap-buffer-name* "*BQN keymap*"
  "Name of the BQN keymap buffer.")

(defvar bqn-keymap-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'bqn-keymap-mode-kill-buffer)
    map)
  "Keymap for keymap mode buffers.")

(defvar bqn--process-name "BQN"
  "Name of BQN comint process")

(defcustom *bqn-process-buffer-name* "*BQN*"
  "Name of buffer which holds BQN process."
  :type 'string
  :group 'bqn)

(defcustom bqn-flash-on-send t
  "When non-nil flash the region sent to BQN process."
  :type 'boolean
  :group 'bqn)

(defun bqn--flash-region (start end &optional timeout)
  "Temporarily highlight region from start to end."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))

(defun bqn-process-ensure-session ()
  "Check for a running *bqn-process-buffer-name*.

If it doesn't exist, create and return it; else, return the existing one."
  (or (get-process bqn--process-name)
      (progn
        (run-bqn)
        (get-process bqn--process-name))))

;;;###autoload
(defun run-bqn ()
  "Run an inferior BQN process inside Emacs."
  (interactive)
  (let* ((bqn-program bqn-interpreter-path)
	     (buffer (comint-check-proc bqn--process-name)))
    ;; pop to the "*BQN*" buffer when the process is dead, the buffer
    ;; is missing or it's got the wrong mode.
    (pop-to-buffer-same-window
     (if (or buffer (comint-check-proc (current-buffer)))
	     (get-buffer-create (or buffer *bqn-process-buffer-name*))
       (current-buffer)))
    ;; create the comint process unless there is a buffer already
    (unless buffer
      (apply 'make-comint-in-buffer
             bqn--process-name
             buffer
	         bqn-program bqn-interpreter-arguments)
      (switch-to-buffer-other-window *bqn-process-buffer-name*)
      (bqn-inferior-mode)
      (set-input-method "BQN-Z"))))

(defun bqn-process-execute-region (start end &optional dont-follow)
  "Send the current region to the bqn-process-session.

When DONT-FOLLOW is non-nil, maintain focus on the buffer where the function was called from."
  (interactive "r")
  (when (= start end)
    (error
     (concat "Attempt to send empty region to "
             *bqn-process-buffer-name*)))
  (when bqn-flash-on-send
    (bqn--flash-region start end))
  (let ((region (buffer-substring-no-properties start end))
        (session (bqn-process-ensure-session))
        (buffer (current-buffer)))
    (pop-to-buffer (process-buffer session))
    (goto-char (point-max))
    (insert (format "\n%s\n" region))
    (comint-send-input)
    (when (or dont-follow nil)
      (pop-to-buffer buffer))))

(defun bqn-process-execute-line-and-follow ()
  "Send the current line to BQN process and focus BQN process buffer."
  (interactive)
  (bqn-process-execute-region (point-at-bol) (point-at-eol)))

(defun bqn-process-execute-buffer-and-follow ()
  "Send the current buffer to BQN process and focus BQN process buffer"
  (interactive)
  (bqn-process-execute-region (point-min) (point-max)))

(defun bqn-process-execute-line ()
  "Send the line, which contains point, to the BQN process"
  (interactive)
  (bqn-process-execute-region (point-at-bol) (point-at-eol) t))

(defun bqn-process-execute-buffer ()
  "Send the current buffer to BQN process"
  (interactive)
  (bqn-process-execute-region (point-min) (point-max) t))

(defun bqn-inferior--initialize ()
  "Helper function to initialize BQN inferior process."
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(define-derived-mode bqn-inferior-mode comint-mode "BQN interactive"
  "Major mode for inferior BQN processes."
  :syntax-table bqn--syntax-table
  (setq-local font-lock-defaults bqn--token-syntax-types)

  (setq comint-prompt-regexp bqn-comint-prompt-regexp)
  (setq comint-prompt-read-only nil)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) bqn-comint-prompt-regexp))

(add-hook 'bqn-inferior-mode-hook 'bqn-inferior--initialize)

(defun bqn-keymap-mode-kill-buffer ()
  "Close the buffer displaying the keymap."
  (interactive)
  (let ((buffer (get-buffer *bqn-keymap-buffer-name*)))
    (when buffer
      (delete-windows-on buffer)
      (kill-buffer buffer))))

(define-derived-mode bqn-keymap-mode fundamental-mode "BQN-Keymap"
  "Major mode for displaying the keymap help."
  (use-local-map bqn-keymap-mode-map)
  (read-only-mode 1)
  (setq truncate-lines t))

(defun bqn-show-keyboard ()
  "Display the keyboard help."
  (interactive)
  (let ((keyboard-help (get-buffer *bqn-keymap-buffer-name*)))
    (unless (and keyboard-help (get-buffer-window keyboard-help))
      ;; The buffer is not displayed.
      (let* ((buffer (get-buffer-create *bqn-keymap-buffer-name*))
	         (window (split-window nil)))
	    (with-current-buffer buffer
	      (insert bqn-keyboard-map)
	      (goto-char (point-min))
	      (bqn-keymap-mode))
        (set-window-buffer window buffer)
        (fit-window-to-buffer window)))))

(provide 'bqn-comint)
;;; bqn-comint.el ends here
