;;; bqn-keymap-mode.el --- BQN Keymap Reference

;; Author: Marshall Lochbaum <mwlochbaum@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/museoa/bqn-mode
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This package provides a major mode for displaying the BQN keymap reference.

;;; Code:

(defvar bqn-keymap-mode-reference
  "┌────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬─────────┐
│~ ¬ │! ⎉ │@ ⚇ │# ⍟ │$ ◶ │% ⊘ │^ ⎊ │& ⍎ │* ⍕ │( ⟨ │) ⟩ │_ √ │+ ⋆ │Backspace│
│` ˜ │1 ˘ │2 ¨ │3 ⁼ │4 ⌜ │5 ´ │6 ˝ │7   │8 ∞ │9 ¯ │0 • │- ÷ │= × │         │
├────┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬──────┤
│Tab    │Q ↙ │W 𝕎 │E ⍷ │R 𝕣 │T ⍋ │Y   │U   │I ⊑ │O ⊒ │P ⍳ │{ ⊣ │} ⊢ │|     │
│       │q ⌽ │w 𝕨 │e ∊ │r ↑ │t ∧ │y   │u ⊔ │i ⊏ │o ⊐ │p π │[ ← │] → │\\     │
├───────┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴──────┤
│Caps    │A ↖ │S 𝕊 │D   │F 𝔽 │G 𝔾 │H « │J   │K ⌾ │L » │: · │\" ˙ │Enter     │
│Lock    │a ⍉ │s 𝕤 │d ↕ │f 𝕗 │g 𝕘 │h ⊸ │j ∘ │k ○ │l ⟜ │; ⋄ │' ↩ │          │
├────────┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──────────┤
│Shift      │Z ⋈ │X 𝕏 │C   │V ⍒ │B ⌈ │N   │M ≢ │< ≤ │> ≥ │? ⇐ │Shift       │
│           │z ⥊ │x 𝕩 │c ↓ │v ∨ │b ⌊ │n   │m ≡ │, ∾ │. ≍ │/ ≠ │            │
└───────────┴────┴────┴────┴────┴────┴────┴────┴────┴────┴────┴────────────┘
                             Space: ‿"
  "Keyboard map for BQN.")

(defvar bqn-keymap-mode-*buffer-name* "*BQN keymap*"
  "Name of the BQN keymap buffer.")

(defvar bqn-keymap-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'bqn-keymap-mode-kill-buffer)
    map)
  "Keymap for keymap mode buffers.")

(defun bqn-keymap-mode-kill-buffer ()
  "Close the buffer displaying the keymap."
  (interactive)
  (let ((buffer (get-buffer bqn-keymap-mode-*buffer-name*)))
    (when buffer
      (delete-windows-on buffer)
      (kill-buffer buffer))))

(defun bqn-keymap-mode-show-keyboard ()
  "Display the keyboard help."
  (interactive)
  (let ((keyboard-help (get-buffer bqn-keymap-mode-*buffer-name*)))
    (unless (and keyboard-help (get-buffer-window keyboard-help))
      ;; The buffer is not displayed.
      (let* ((buffer (get-buffer-create bqn-keymap-mode-*buffer-name*))
             (window (split-window nil)))
        (with-current-buffer buffer
          (insert bqn-keymap-mode-reference)
          (goto-char (point-min))
          (bqn-keymap-mode))
        (set-window-buffer window buffer)
        (fit-window-to-buffer window)))))

(define-derived-mode bqn-keymap-mode fundamental-mode "BQN-Keymap"
  "Major mode for displaying the keymap help."
  (use-local-map bqn-keymap-mode-map)
  (read-only-mode 1)
  (setq truncate-lines t))

(provide 'bqn-keymap-mode)

;; Local Variables:
;; coding: utf-8-unix
;; indent-tabs-mode: nil
;; End:

;;; bqn-keymap-mode.el ends here
