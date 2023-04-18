;;; bqn-glyph-mode.el --- BQN glyph reference

;; Author: Marshall Lochbaum <mwlochbaum@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/museoa/bqn-mode
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This package provides a major mode to show a BQN glyphs lookup table
;; reference.

;;; Code:

(require 'bqn-mode)

(defvar bqn-glyph-mode-reference
  "┌───┬────────────────┬──────────────┬───┬──────────────────┬────────────────┐
│ @ │ Monadic        │ Dyadic       │ @ │ Monadic          │ Dyadic         │
├───┼────────────────┼──────────────┼───┼──────────────────┼────────────────┤
│ + │ Conjugate      │ Add          │ ⥊ │ Deshape          │ Reshape        │
│ ─ │ Negate         │ Subtract     │ ∾ │ Join             │ Join to        │
│ × │ Sign           │ Multiply     │ ≍ │ Solo             │ Couple         │
│ ÷ │ Reciprocal     │ Divide       │ ⋈ │ Enlist           │ Pair           │
│ ⋆ │ Exponential    │ Power        │ ↑ │ Prefixes         │ Take           │
│ √ │ Square Root    │ Root         │ ↓ │ Suffixes         │ Drop           │
│ ⌊ │ Floor          │ Minimum      │ ↕ │ Range            │ Windows        │
│ ⌈ │ Ceiling        │ Maximum      │ » │ Nudge            │ Shift Before   │
│ ∧ │ Sort Up        │ And          │ « │ Nudge Back       │ Shift After    │
│ ∨ │ Sort Down      │ Or           │ ⌽ │ Reverse          │ Rotate         │
│ ¬ │ Not            │ Span         │ ⍉ │ Transpose        │ Reorder Axes   │
│ │ │ Absolute Value │ Modulus      │ / │ Indices          │ Replicate      │
│ ≤ │                │ No More Than │ ⍋ │ Grade Up         │ Bins Up        │
│ < │ Enclose        │ Less Than    │ ⍒ │ Grade Down       │ Bins Down      │
│ > │ Merge          │ Greater Than │ ⊏ │ First Cell       │ Select         │
│ ≥ │                │ No Less Than │ ⊑ │ First            │ Pick           │
│ = │ Rank           │ Equals       │ ⊐ │ Classify         │ Index of       │
│ ≠ │ Length         │ Not Equals   │ ⊒ │ Occurrence Count │ Progressive ⊐  │
│ ≡ │ Depth          │ Match        │ ∊ │ Mark Firsts      │ Member of      │
│ ≢ │ Shape          │ Not Match    │ ⍷ │ Deduplicate      │ Find           │
│ ⊣ │ Identity       │ Left         │ ⊔ │ Group Indices    │ Group          │
│ ⊢ │ Identity       │ Right        │ ! │ Assert           │ Assert Message │
└───┴────────────────┴──────────────┴───┴──────────────────┴────────────────┘"
  "Glyph Lookup Table for BQN.")

(defvar bqn-glyph-mode-*buffer-name* "*BQN Glyphs*")

(defvar bqn-glyph-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'bqn-glyph-mode-kill-buffer)
    map)
  "Keymap for keymap mode buffers.")

(defun bqn-glyph-mode-kill-buffer ()
  "Close the buffer displaying the keymap."
  (interactive)
  (let ((buffer (get-buffer bqn-glyph-mode-*buffer-name*)))
    (when buffer
      (delete-windows-on buffer)
      (kill-buffer buffer))))

(defun bqn-glyph-mode-show-glyphs ()
  "Display a table of BQN glyphs."
  (interactive)
  (let ((glyph-buffer (get-buffer bqn-glyph-mode-*buffer-name*)))
    (unless (and glyph-buffer (get-buffer-window glyph-buffer))
      ;; The buffer is not displayed.
      (let* ((buffer (get-buffer-create bqn-glyph-mode-*buffer-name*))
             (window (split-window nil)))
        (with-current-buffer buffer
          (insert bqn-glyph-mode-reference)
          (goto-char (point-min))
          (bqn-glyph-mode))
        (set-window-buffer window buffer)
        (fit-window-to-buffer window)))))

(define-derived-mode bqn-glyph-mode fundamental-mode "BQN-Glyphs"
  "Major mode for displaying the BQN Glyph help."
  (use-local-map bqn-glyph-mode-map)
  (buffer-face-set 'bqn-default)
  (read-only-mode 1)
  (setq truncate-lines t))

(provide 'bqn-glyph-mode)

;;; bqn-glyph-mode.el ends here
