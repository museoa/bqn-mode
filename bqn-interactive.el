;;; bqn-interactive --- Interactive BQN interpreter -*- lexical-binding: t -*-
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

(defvar bqn-cli-arguments '()
  "Commandline arguments to pass to the BQN interpreter.")

(defvar bqn-interactive-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; add keymaps here
    map)
  "Basic mode to run BQN.")

(defvar bqn-interactive-prompt-regexp "^   "
  "Prompt for BQN.")

(defvar bqn-keyboard-map
  "
┌────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬─────────┐
│~ ¬ │! ⎉ │@ ⚇ │# ⍟ │$ ◶ │% ⊘ │^ ⎊ │& ⍎ │* ⍕ │( ⟨ │) ⟩ │_ √ │+ ⋆ │Backspace│
│` ˜ │1 ˘ │2 ¨ │3 ⁼ │4 ⌜ │5 ´ │6 ˝ │7   │8 ∞ │9 ¯ │0 • │- ÷ │= × │         │
├────┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬──────┤
│Tab    │Q ↙ │W 𝕎 │E ⍷ │R 𝕣 │T ⍋ │Y   │U   │I ⊑ │O ⊒ │P ⍳ │{ ⊣ │} ⊢ │|     │
│       │q ⌽ │w 𝕨 │e ∊ │r ↑ │t ∧ │y   │u ⊔ │i ⊏ │o ⊐ │p π │[ ← │] → │\     │
├───────┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴──────┤
│Caps    │A ↖ │S 𝕊 │D   │F 𝔽 │G 𝔾 │H « │J   │K ⌾ │L » │: · │\" ˙ │Enter     │
│Lock    │a ⍉ │s 𝕤 │d ↕ │f 𝕗 │g 𝕘 │h ⊸ │j ∘ │k ○ │l ⟜ │; ⋄ │' ↩ │          │
├────────┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──────────┤
│Shift      │Z ⋈ │X 𝕏 │C   │V ⍒ │B ⌈ │N   │M ≢ │< ≤ │> ≥ │? ⇐ │Shift       │
│           │z ⥊ │x 𝕩 │c ↓ │v ∨ │b ⌊ │n   │m ≡ │, ∾ │. ≍ │/ ≠ │            │
└───────────┴────┴────┴────┴────┴────┴────┴────┴────┴────┴────┴────────────┘
                             Space: ‿
" "Keyboard map for BQN.")

(defvar *bqn-keymap-buffer-name* "*BQN keymap*"
  "Name of the BQN keymap buffer.")

(defvar bqn-keymap-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'bqn-keymap-mode-kill-buffer)
    map)
  "Keymap for keymap mode buffers.")

(defun run-bqn ()
  "Run an inferior BQN process inside Emacs."
  (interactive)
  (let* ((bqn-program bqn-interpreter-path)
	 (buffer (comint-check-proc "BQN")))
    ;; pop to the "*BQN*" buffer if the process is dead, the buffer
    ;; is missing or it's got the wrong mode.
    (pop-to-buffer-same-window
     (if (or buffer (comint-check-proc (current-buffer)))
	 (get-buffer-create (or buffer "*BQN*"))
       (current-buffer)))
    ;; create the comint process if there is no buffer
    (unless buffer
      (apply 'make-comint-in-buffer "BQN" buffer
	     bqn-program bqn-cli-arguments)
      (switch-to-buffer-other-window "*BQN*")
      (bqn-inferior-mode)
      (set-input-method "BQN-Z"))))

(defun bqn-inferior--initialize ()
  "Helper function to initialize BQN inferior process."
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(define-derived-mode bqn-inferior-mode comint-mode "BQN interactive"
  "Major mode for inferior BQN processes."
  :syntax-table bqn--syntax-table
  (setq-local font-lock-defaults bqn--token-syntax-types)

  (setq comint-prompt-regexp bqn-interactive-prompt-regexp)
  (setq comint-prompt-read-only nil)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) bqn-interactive-prompt-regexp))

(add-hook 'bqn-inferior-mode-hook 'bqn-inferior--initialize)
(add-hook 'bqn-inferior-mode-hook 'bqn-init)

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

(provide 'bqn-interactive)
;;; bqn-interactive.el ends here
