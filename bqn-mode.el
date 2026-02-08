;;; bqn-mode.el --- Emacs mode for BQN -*- lexical-binding: t -*-

;; Emacs bqn-mode is derived from gnu-apl-mode,
;; which is copyright 2013-2015 Elias Mårtenson <lokedhs@gmail.com>.
;; Changes are copyright 2021 Marshall Lochbaum <mwlochbaum@gmail.com>.

;; Author: Marshall Lochbaum <mwlochbaum@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (compat "30.0.0.0") (eros "0.1.0"))
;; URL: https://github.com/museoa/bqn-mode
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Emacs major mode for BQN programming language.

;;; Code:

(require 'comint)
(require 'quail)
(require 'pulse)
(require 'subr-x)

(defvar bqn-keymap-mode-reference
  "\
┌────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬─────────┐
│~ ¬ │! ⎉ │@ ⚇ │# ⍟ │$ ◶ │% ⊘ │^ ⎊ │&   │*   │( ⟨ │) ⟩ │_ √ │+ ⋆ │Backspace│
│` ˜ │1 ˘ │2 ¨ │3 ⁼ │4 ⌜ │5 ´ │6 ˝ │7   │8 ∞ │9 ¯ │0 • │- ÷ │= × │         │
├────┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬──────┤
│Tab    │Q   │W 𝕎 │E ⍷ │R 𝕣 │T ⍋ │Y   │U   │I ⊑ │O ⊒ │P   │{ ⊣ │} ⊢ │|     │
│       │q ⌽ │w 𝕨 │e ∊ │r ↑ │t ∧ │y   │u ⊔ │i ⊏ │o ⊐ │p π │[ ← │]   │\\     │
├───────┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴┬───┴──────┤
│Caps    │A   │S 𝕊 │D   │F 𝔽 │G 𝔾 │H « │J   │K ⌾ │L » │: · │\" ˙ │Enter     │
│Lock    │a ⍉ │s 𝕤 │d ↕ │f 𝕗 │g 𝕘 │h ⊸ │j ∘ │k ○ │l ⟜ │; ⋄ │' ↩ │          │
├────────┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──┬─┴──────────┤
│Shift      │Z ⋈ │X 𝕏 │C   │V ⍒ │B ⌈ │N   │M ≢ │< ≤ │> ≥ │? ⇐ │Shift       │
│           │z ⥊ │x 𝕩 │c ↓ │v ∨ │b ⌊ │n   │m ≡ │, ∾ │. ≍ │/ ≠ │            │
└───────────┴────┴────┴────┴────┴────┴────┴────┴────┴────┴────┴────────────┘
                             Space: ‿"
  "Keyboard map for BQN.")

(defvar bqn-keymap-mode-*buffer-name* "*BQN keymap*"
  "Name of the BQN keymap buffer.")

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

(define-derived-mode bqn-keymap-mode special-mode "BQN-Keymap"
  "Major mode for displaying the keymap help."
  (buffer-face-set 'bqn-default)
  (read-only-mode 1)
  (setq truncate-lines t))

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

(define-derived-mode bqn-glyph-mode special-mode "BQN-Glyphs"
  "Major mode for displaying the BQN Glyph help."
  (buffer-face-set 'bqn-default)
  (read-only-mode 1)
  (setq truncate-lines t))

;;; BQN Symbols documentation

;; Arrays and hashes are not very Lispy, however they will be employed here
;; because we want the lowest latency possible for an end-user-facing structure.
;; For all intents and purposes, this table should be regarded as read-only;
;; indeed, it is "cached" at byte-compile time via eval-when-compile.
(defconst bqn--symbols
  (eval-when-compile
    (let ((table '(
                   ;; top row
                   (?\` . [ nil
                            "𝔽` 𝕩: Scan | 𝕨 𝔽` 𝕩: Scan With Initial"
                            "\
𝔽` 𝕩: Scan
- Scan over 𝕩 with 𝔽 from left to right, producing intermediate values.

𝕨 𝔽` 𝕩: Scan With initial
- Monadic scan, but use 𝕨 as initial left argument."
                            "\
    +` 1‿2‿3
⟨ 1 3 6 ⟩

    ⟨1, 1+2, (1+2)+3⟩
⟨ 1 3 6 ⟩

    -` 1‿2‿3
⟨ 1 ¯1 ¯4 ⟩

    ⟨1, 1-2, (1-2)-3⟩
⟨ 1 ¯1 ¯4 ⟩

    5 +` 1‿2‿3
⟨ 6 8 11 ⟩

    ⟨5+1, (5+1)+2, ((5+1)+2)+3⟩
⟨ 6 8 11 ⟩

    5 -` 1‿2‿3
⟨ 4 2 ¯1 ⟩

    ⟨5-1, (5-1)-2, ((5-1)-2)-3⟩
⟨ 4 2 ¯1 ⟩"])
                   (?˜ . [ ?\`
                           "𝔽˜ 𝕩: Self | 𝕨 𝔽˜ 𝕩: Swap"
                           "\
𝔽˜ 𝕩: Self
- Supplies 𝕩 as a left argument to 𝔽 (𝕩 𝔽 𝕩).

𝕨 𝔽˜ 𝕩: Swap
- Swaps the arguments of 𝔽 (𝕩 𝔽 𝕨)."
                           "\
    1 + 1
2

    +˜ 1
2

    1 - 2
¯1

    1 -˜ 2
1"])
                   (?¬ . [ ?~
                           "¬ 𝕩: Logical Not | 𝕨 ¬ 𝕩: Span"
                           "\
¬ 𝕩: Logical Not
- Logical Not of 𝕩.
- Pervasive.

𝕨 ¬ 𝕩: Span
- Count of numbers in the inclusive range from 𝕩 to 𝕨.
- Pervasive."
                           "\
    ¬ 0
1

    ¬ 1‿0
⟨ 0 1 ⟩

    3 ¬ 1
3

    3‿4 ¬ 0‿2
⟨ 4 3 ⟩"])
                   (?! . [ nil
                           "! 𝕩: Assert | 𝕨 ! 𝕩: Assert With Message"
                           "\
! 𝕩: Assert
- Throw an error if 𝕩 is not 1.

𝕨 ! 𝕩: Assert With Message
- Throw an error with message 𝕨 if 𝕩 is not 1."
                           "\
    ! 1
1

    ! 2
Error: Assertion error

    ! \"hello\"
Error: hello

    \"hi\" ! 1
1

    \"two\" ! 2
Error: two

    \"hello error\" ! \"hello\"
Error: hello error"])
                   (?˘ . [ ?1
                           "𝔽˘ 𝕩, 𝕨 𝔽˘ 𝕩: Cells"
                           "\
𝔽˘ 𝕩, 𝕨 𝔽˘ 𝕩: Cells
- Apply 𝔽 to/between the major cells of the arguments. (𝔽⎉¯1)"
                           "\
    a ← 3‿3 ⥊ ↕9


    <˘ a
⟨ ⟨ 0 1 2 ⟩ ⟨ 3 4 5 ⟩ ⟨ 6 7 8 ⟩ ⟩

    a ≍˘ a
┌─
╎ 0 1 2
  0 1 2

  3 4 5
  3 4 5

  6 7 8
  6 7 8
        ┘"])
                   (?⎉ . [ ?!
                           "𝔽⎉𝕘 𝕩, 𝕨 𝔽⎉𝕘 𝕩: Rank"
                           "\
𝔽⎉𝕘 𝕩, 𝕨 𝔽⎉𝕘 𝕩: Rank
- Apply 𝔽 to cells at ranks given in 𝕘. Non-negative numbers indicate the rank
  of the cell and negative ones indicate the difference from full rank.
- The ranks applied are given by the following:
  - ⎉ c Rank-c cells of 𝕩 (monadic) or both arguments (dyadic)
  - ⎉ b‿c Rank-b cells of 𝕨 and rank-c cells of 𝕩 (dyadic)
  - ⎉ a‿b‿c Rank-a cells of 𝕩 (monadic), b-cells of 𝕨 and c-cells of 𝕩 (dyadic)"
                           "\
    a ← 3‿2‿4⥊\"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"

    ⌽⎉2 a
┌─
╎\"EFGH
  ABCD

 ·MNOP
  IJKL

 ·UVWX
  QRST\"
       ┘"])
                   (?@ . [ nil
                           "Null Character"
                           "\
@: Null Character
- Code point 0 in ASCII.
- Add to a code point number to ger that character."
                           "\
    @+50
'2'

    @
@

    @+64
'@'"])
                   (?¨ . [ ?2
                           "𝔽¨ 𝕩, 𝕨 𝔽¨ 𝕩: Each"
                           "\
𝔽¨ 𝕩, 𝕨 𝔽¨ 𝕩: Each
- Apply 𝔽 to/between the elements of the arguments. (𝔽⚇¯1)"
                           "\
    <¨ 1‿2‿3
┌─
· ┌·    ┌·    ┌·
  · 1   · 2   · 3
      ┘     ┘     ┘
                    ┘

    4‿5‿6 ∾¨ 1‿2‿3
⟨ ⟨ 4 1 ⟩ ⟨ 5 2 ⟩ ⟨ 6 3 ⟩ ⟩"])
                   (?⚇ . [ ?@
                           "𝔽⚇𝕘 𝕩, 𝕨 𝔽⚇𝕘 𝕩: Depth"
                           "\
𝔽⚇𝕘 𝕩, 𝕨 𝔽⚇𝕘 𝕩: Depth
- Apply 𝔽 to the cells of the arguments at depth given in 𝕘.
- Negative numbers count down from the top level and non-negative ones from the
  bottom up."
                           "\
    1⊸↓⚇1 ⟨⟨1,2,3⟩, ⟨4,5,6⟩⟩
⟨ ⟨ 2 3 ⟩ ⟨ 5 6 ⟩ ⟩

    1 ↓⚇1 ⟨⟨1,2,3⟩, ⟨4,5,6⟩⟩
⟨ ⟨ 2 3 ⟩ ⟨ 5 6 ⟩ ⟩

    (+´↕)⚇0 ⟨2,4‿7,3⟩  # Implements pervasion
⟨ 1 ⟨ 6 21 ⟩ 3 ⟩"])
                   (?\# . [ nil
                            "#: Comment"
                            "\
#: Comment
- Create a comment that extends to the end of the line.
- Anything written in comments is ignored.
"
                            "\
    1 + 2 # + 3 + 4
3

    \"Hello world!\" # this is ignored!
\"Hello world!\""])
                   (?⁼ . [ ?3
                           "𝔽⁼ 𝕩, 𝕨 𝔽⁼ 𝕩: Undo"
                           "\
𝔽⁼ 𝕩, 𝕨 𝔽⁼ 𝕩: Undo"
                           "\
    1 - 2
¯1

    1 -⁼ 2
¯1

    √ 16
4

    √⁼ 4
16

    ⋆ 1
2.718281828459045

    ⋆⁼ 2.718281828459045
1"])
                   (?⍟ . [ ?\#
                           "𝔽⍟𝔾 𝕩, 𝕨 𝔽⍟𝔾 𝕩: Repeat"
                           "\
𝔽⍟𝔾 𝕩, 𝕨 𝔽⍟𝔾 𝕩: Repeat
- Apply 𝔾 to 𝕨 and 𝕩, then apply 𝔽 to 𝕩 that many times.
- If 𝕨 is given, use it each time as a constant left argument.
- If 𝔾 returns an array, give 𝔽⍟𝕩 for each of its elements."
                           "\
    1 +⍟⊢ 4
8

    1 +⍟1‿2‿3 4
⟨ 5 6 7 ⟩

    3 ∾⍟{≠𝕩} ⟨4,5,6⟩
⟨ 3 3 3 4 5 6 ⟩"])
                   (?⌜ . [ ?4
                           "𝕨 𝔽⌜ 𝕩: Table"
                           "\
𝕨 𝔽⌜ 𝕩: Table
- Apply 𝔽 between every possible pair of the elements of the arguments."
                           "\
    1‿2‿3‿4 +⌜ 4‿5‿6‿7
┌─
╵ 5 6  7  8
  6 7  8  9
  7 8  9 10
  8 9 10 11
            ┘

    \"abc\" ∾⌜ \"xyz\"
┌─
╵ \"ax\" \"ay\" \"az\"
  \"bx\" \"by\" \"bz\"
  \"cx\" \"cy\" \"cz\"
                 ┘
"])
                   (?◶ . [ ?$
                           "𝔽◶𝕘 𝕩, 𝕨 𝔽◶𝕘 𝕩: Choose"
                           "\
𝔽◶𝕘 𝕩, 𝕨 𝔽◶𝕘 𝕩: Choose
- Apply 𝔽 to the arguments and use the result to pick (⊑) a function from list
  𝕘.
- Apply the picked function to the arguments."
                           "\
    F ← ⊢◶+‿-‿÷‿×

    F 0
0

    F 1
¯1

    F 2
0.5"])
                   (?´ . [ ?5
                           "𝔽´ 𝕩: Fold | 𝕨 𝔽´ 𝕩: Fold With Initial"
                           "\
𝔽´ 𝕩: Fold
- Fold over 𝕩 with 𝔽 from right to left i.e. Insert 𝔽 between the elements of 𝕩.
- 𝕩 must be a simple list (1 = =𝕩).

𝕨 𝔽´ 𝕩: Fold With Initial
- Monadic fold, but use 𝕨 as initial right argument."
                           "\
    +´ 1‿2‿3
6

    1+2+3
6

    -´ 1‿2‿3
2

    1-2-3
2

    5 +´ 1‿2‿3
11

    1+2+3+5
11

    5 -´ 1‿2‿3
¯3

    1-2-3-5
¯3"])
                   (?⊘ . [ ?%
                           "𝔽⊘𝔾 𝕩: Valences | 𝕨 𝔽⊘𝔾 𝕩: Dyadic Valences"
                           "\
𝔽⊘𝔾 𝕩: Valences
- Apply 𝔽 to 𝕩.

𝕨 𝔽⊘𝔾 𝕩: Dyadic Valences
- Apply 𝔾 to 𝕨 and 𝕩."
                           "\
    +⊘- 5
5

    -⊘+ 5
¯5

    4 +⊘- 5
¯1

    4 -⊘+ 5
9"])
                   (?˝ . [ ?6
                           "𝔽˝ 𝕩: Insert | 𝕨 𝔽˝ 𝕩: Insert With Initial"
                           "\
𝔽˝ 𝕩: Insert
- Fold over cells of 𝕩 with 𝔽 from end to start, that is, insert 𝔽 between the
  major cells of 𝕩.

𝕨 𝔽˝ 𝕩: Insert With Initial
- Monadic insert, but use 𝕨 as initial right argument."
                           "\
    a ← 3‿3 ⥊ ↕9

    +˝ a
⟨ 9 12 15 ⟩

    0‿1‿2 + 3‿4‿5 + 6‿7‿8
⟨ 9 12 15 ⟩

    b ← 3‿3 ⥊ ↕9

    1‿1‿1 +˝ b
⟨ 10 13 16 ⟩

    1 +˝ b
⟨ 10 13 16 ⟩

    0‿1‿2 + 3‿4‿5 + 6‿7‿8 + 1‿1‿1
⟨ 10 13 16 ⟩"])
                   (?⎊ . [ ?^
                           "𝔽⎊𝔾 𝕩, 𝕨 𝔽⎊𝔾 𝕩: Catch"
                           "\
𝔽⎊𝔾 𝕩, 𝕨 𝔽⎊𝔾 𝕩: Catch
- Apply 𝔽 to the arguments.
- If an error happens when 𝔽 is applied, cancel its execution, apply 𝔾 to the
  arguments and return its result.
- Otherwise, return the result of 𝔽.
"
                           "\
    ∾⎊{\"error occurred with argument: \"∾•Fmt 𝕩} 1
\"error occurred with argument: 1\"

    ∾⎊{\"error occurred with argument: \"∾•Fmt 𝕩} ⟨⟨1,2⟩, ⟨3,4⟩⟩
⟨ 1 2 3 4 ⟩
"])
                   (?∞ . [ ?8
                           "∞: Infinity"
                           "\
∞: Infinity
- Mathematical constant Infinity, a numeric literal. Can be negative (¯∞)."
                           "\
    ∞
∞

    ¯∞
¯∞

    1+∞
∞"])
                   (?\( . [ nil
                            "(: Begin Expression"
                            "\
(: Begin Expression
- Starts an expression, and only one expression.
- Must end with a corresponding ).
- ( supercedes any precedence order, so that an expression in () is evaluated
  fully before it can be used in the outer context."
                            "\
    1 + 2 - 3
0

    (1 + 2) - 3
0"])
                   (?¯ . [ ?9
                           "¯: Minus"
                           "\
¯: Minus
- Prefix before numbers to indicate that they are negative.
- Note that this is not the same as -, since it is part of the number, rather
  than a primitive that negates its value."
                           "\
    -1‿2‿3
⟨ ¯1 ¯2 ¯3 ⟩

    ¯1‿2‿3
⟨ ¯1 2 3 ⟩"])
                   (?⟨ . [ ?\(
                           "⟨: Begin list"
                           "\
⟨: Begin list
- Starts a list.
- Inner elements must be separated by , or ⋄.
- Lists can be nested in other lists.
- Must end with a corresponding ⟩."
                           "\
    ⟨1, 2, 3⟩
⟨ 1 2 3 ⟩

    ⟨+ ⋄ - ⋄ 56⟩
⟨ + - 56 ⟩"])
                   (?\) . [ nil
                            "): End Expression)"
                            "\
): End Expression
- The closing symbol for (.
- See ( documentation for more details."
                            "\
    1 + 2 - 3
0

    (1 + 2) - 3
0"])
                   (?• . [ ?0
                           "•: System"
                           "\
•: System
- A prefix for system functions.
- •listSys gives a list of defined system value names.
- • is ignored when determining the role of the system value."
                            "\
"])
                   (?⟩ . [ ?\)
                           "⟩: End list)"
                           "\
⟩: End list
- Ends a list started by a ⟨.
- See ⟨ documentation for more details."
                            "\
    ⟨1, 2, 3⟩
⟨ 1 2 3 ⟩

    ⟨+ ⋄ - ⋄ 56⟩
⟨ + - 56 ⟩"])
                   (?- . [ nil
                           "- 𝕩: Negate | 𝕨 - 𝕩: Subtract"
                           "\
- 𝕩: Negate
- Additive Inverse of 𝕩.

𝕨 - 𝕩: Subtract
- Subtract 𝕩 from 𝕨.
- 𝕨 and 𝕩 can be characters or numbers."
                            "\
    - 1
¯1

    - ¯1
1

    1 - 2
¯1

    1 - 2‿3‿4
⟨ ¯1 ¯2 ¯3 ⟩

    'a' - 4
']'

    'b' - 'a'
1"])
                   (?÷ . [ ?-
                           "÷ 𝕩: Reciprocal | 𝕨 ÷ 𝕩: Divide"
                           "\
÷ 𝕩: Reciprocal
- Gives 1 ÷ 𝕩.
- Pervasive.

𝕨 ÷ 𝕩: Divide
- 𝕨 divided by 𝕩.
- Pervasive."
                           "\
    ÷ 5
0.2

    5 ÷ 4
1.25

    14 ÷ 7
2
"])
                   (?√ . [ ?_
                           "√ 𝕩: Square root | 𝕨 √ 𝕩: Root"
                            "\
√ 𝕩: Square root
- Self-explaining.
- Pervasive.

𝕨 √ 𝕩: Root
- 𝕨 th root of 𝕩.
- Pervasive."
                            "\
    √ 2
1.4142135623730951

    2 √ 2
1.4142135623730951

    1‿2‿3‿4 √ 4
⟨ 4 2 1.5874010519681994 1.4142135623730951 ⟩"])
                   (?= . [ nil
                           "= 𝕩: Rank | 𝕨 = 𝕩: Equal To"
                           "\
= 𝕩: Rank
- Returns the number of dimensions in 𝕩.

𝕨 = 𝕩: Equal To
- Do argument atoms match?
- Pervasive."
                           "\
    = 0
0

    = 3⥊0
1

    = 3‿3⥊0
2

    3‿3‿3 ⥊ ⟨⟨0⟩⟩
┌─
╎ ⟨ 0 ⟩ ⟨ 0 ⟩ ⟨ 0 ⟩
  ⟨ 0 ⟩ ⟨ 0 ⟩ ⟨ 0 ⟩
  ⟨ 0 ⟩ ⟨ 0 ⟩ ⟨ 0 ⟩

  ⟨ 0 ⟩ ⟨ 0 ⟩ ⟨ 0 ⟩
  ⟨ 0 ⟩ ⟨ 0 ⟩ ⟨ 0 ⟩
  ⟨ 0 ⟩ ⟨ 0 ⟩ ⟨ 0 ⟩

  ⟨ 0 ⟩ ⟨ 0 ⟩ ⟨ 0 ⟩
  ⟨ 0 ⟩ ⟨ 0 ⟩ ⟨ 0 ⟩
  ⟨ 0 ⟩ ⟨ 0 ⟩ ⟨ 0 ⟩
                    ┘

    1 = 3
0

    2‿3‿0 = 3‿1‿0
⟨ 0 0 1 ⟩

    'a' = 'a'
1"])
                   (?+ . [ nil
                           "+ 𝕩: Conjugate | 𝕨 + 𝕩: Add"
                           "\
+ 𝕩: Conjugate
- Complex conjugate of 𝕩.
- BQN doesn't support complex numbers yet, so it has no effect.

𝕨 + 𝕩: Add
- 𝕨 added to 𝕩.
- Either 𝕨 or 𝕩 can be a character, and if so, the other has to be an integer.
- Pervasive."
                           "\

    + 1
1

    + ¯1
¯1

    1 + 2
3

    1 + 2‿3‿4
⟨ 3 4 5 ⟩

    'a' + 4
'e'"])
                   (?× . [ ?=
                           "× 𝕩: Sign | 𝕨 × 𝕩: Multiply"
                           "\
× 𝕩: Sign
- Sign of 𝕩.
  - ¯1 if 𝕩 < 0
  - 0 if 𝕩 = 0
  - 1 if 𝕩 > 0
- Pervasive.

𝕨 × 𝕩: Multiply
- 𝕨 multiplied by 𝕩.
- Pervasive."
                           "\
    × ¯5‿0‿5‿1
⟨ ¯1 0 1 1 ⟩

    1 × 2
2

    2 × 2‿3‿4
⟨ 4 6 8 ⟩
"])
                   (?⋆ . [ ?+
                           "⋆ 𝕩: Exponential | 𝕨 ⋆ 𝕩: Power"
                           "\
⋆ 𝕩: Exponential
- e (Euler's constant) to the power of 𝕩.
- Pervasive.

𝕨 ⋆ 𝕩: Power
- 𝕨 to the power of 𝕩.
- Pervasive."
                           "\
    ⋆ 0‿1‿2‿3
⟨ 1 2.718281828459045 7.38905609893065 20.085536923187668 ⟩

    2 ⋆ 5
32

    8‿5‿9 ⋆ 2
⟨ 64 25 81 ⟩

    2‿3 ⋆ 3‿¯4
⟨ 8 0.012345679012345678 ⟩"])
                   ;; first row
                   (?⌽ . [ ?q
                           "⌽ 𝕩: Reverse | 𝕨 ⌽ 𝕩: Rotate"
                           "\
⌽ 𝕩: Reverse
- Reverse 𝕩 along the first axis.

𝕨 ⌽ 𝕩: Rotate
- Move the first 𝕨 elements of 𝕩 to its end. Negative 𝕨 reverses the direction
  of rotation."
                           "\
    ⌽ 1‿2‿3
⟨ 3 2 1 ⟩

    a ← 3‿3 ⥊ ↕9

    ⌽ a
┌─
╵ 6 7 8
  3 4 5
  0 1 2
        ┘
    2 ⌽ 1‿2‿3
⟨ 3 1 2 ⟩

    b ← 3‿3 ⥊ ↕9

    2 ⌽ b
┌─
╵ 6 7 8
  0 1 2
  3 4 5
        ┘"])
                   (?𝕨 . [ ?w
                           "𝕨: Left Argument"
                           "\
𝕨: Left Argument
- A variable assigned to the left argument of a block.
- 𝕎 can be used to access the left argument as a function."
                           "\
    5 {𝕨} 1
5

    -‿÷ {𝕎𝕩}¨ 4
⟨ ¯4 0.25 ⟩"])
                   (?𝕎 . [ ?W
                           "𝕎: Left Argument"
                           "\
𝕨: Left Argument
- A variable assigned to the left argument of a block.
- 𝕎 can be used to access the left argument as a function."
                           "\
    5 {𝕨} 1
5

    -‿÷ {𝕎𝕩}¨ 4
⟨ ¯4 0.25 ⟩"])
                   (?∊ . [ ?e
                           "∊ 𝕩: Mark Firsts | 𝕨 ∊ 𝕩: Member Of"
                           "\
∊ 𝕩: Mark Firsts
- Mark the first occurrence of each major cell in 𝕩 with a 1, and all other
  occurrences with a 0.

𝕨 ∊ 𝕩: Member Of
- Is each cell in 𝕨 a major cell of 𝕩?"
                           "\
    ∊ 4‿5‿6‿6‿4‿7‿5
⟨ 1 1 1 0 0 1 0 ⟩

    a ← 3‿3 ⥊ ↕9

    ∊ a
⟨ 1 1 1 ⟩

    ⟨1⟩ ∊ ↕9
⟨ 1 ⟩

    b ← 3‿3 ⥊ ↕9

    ⟨0‿1‿2⟩ ∊ b
┌·
· 0
    ┘

    ⟨1‿3 ⥊ 0‿1‿2⟩ ∊ b
┌·
· 0
    ┘"])
                   (?⍷ . [ ?E
                           "⍷ 𝕩: Deduplicate | 𝕨 ⍷ 𝕩: Find"
                           "\
⍷ 𝕩: Deduplicate
- Unique major cells of 𝕩.

𝕨 ⍷ 𝕩: Find
- Mark the top left location of the occurrences of 𝕨 in 𝕩 with a 1, and other
  locations with 0.
- Result is the same shape as (≢𝕨)↕x."
                           "\
    ⍷ 4‿5‿6‿6‿4‿7‿5
⟨ 4 5 6 7 ⟩

    a ← 3‿3 ⥊ ↕6

    ⍷ a
┌─
╵ 0 1 2
  3 4 5
        ┘
    \"string\" ⍷ \"substring\"
⟨ 0 0 0 1 ⟩

    \"loooooong\" ⍷ \"short\"
⟨⟩

    b ← 7 (4|⋆˜)⌜○↕ 9

    c ← (0‿3‿0≍0‿1‿0)

    c ⍷ b
┌─
╵ 0 0 0 0 0 0 0
  0 0 0 0 0 0 0
  0 0 0 0 0 0 0
  0 0 1 0 0 0 1
  0 0 0 0 0 0 0
  0 0 1 0 0 0 1
                ┘"])
                   (?↑ . [ ?r
                           "↑ 𝕩: Prefixes | 𝕨 ↑ 𝕩: Take"
                           "\
↑ 𝕩: Prefixes
- Prefixes of array 𝕩 along its first axis.

𝕨 ↑ 𝕩: Take
- For each integer in 𝕨, take that many elements from each dimension of 𝕩.
- Negative numbers take from the end.
- If any of the elements in 𝕨 are greater than the length of their respective
  dimension, the dimension is extended with a fill value."
                           "\
    ↑ 1‿2‿3‿4
⟨ ⟨⟩ ⟨ 1 ⟩ ⟨ 1 2 ⟩ ⟨ 1 2 3 ⟩ ⟨ 1 2 3 4 ⟩ ⟩

    a ← 3‿3 ⥊ ↕9

    ↑ a
┌─
· ↕0‿3 ┌─        ┌─        ┌─
       ╵ 0 1 2   ╵ 0 1 2   ╵ 0 1 2
               ┘   3 4 5     3 4 5
                         ┘   6 7 8
                                   ┘
                                     ┘

    3 ↑ 1‿3‿5‿67
⟨ 1 3 5 ⟩

    b ← 4‿4 ⥊ ↕16

    3‿3 ↑ b
┌─
╵ 0 1  2
  4 5  6
  8 9 10
         ┘

    5‿5 ↑ b
┌─
╵  0  1  2  3 0
   4  5  6  7 0
   8  9 10 11 0
  12 13 14 15 0
   0  0  0  0 0
                ┘

    3‿¯3 ↑ b
┌─
╵ 1  2  3
  5  6  7
  9 10 11
          ┘"])
                   (?𝕣 . [ ?R
                           "𝕣: Current Modifier"
                           "\
𝕣: Current Modifier
- A variable assigned to the current modifier block.
- Add underscores to the beginning and/or end (_𝕣, _𝕣_) to use it in a modifier
  role."
                           "\
    +{𝕣⊣𝕩} 4
(1-modifier block)"])
                   (?∧ . [ ?t
                           "∧ 𝕩: Sort Up | 𝕨 ∧ 𝕩: Logical And"
                           "\
∧ 𝕩: Sort Up
- Sort array 𝕩 in ascending order.

𝕨 ∧ 𝕩: Logical And
- Logical And of 𝕨 and 𝕩.
- Pervasive."
                           "\
    ∧ 3‿1‿4‿1‿5
⟨ 1 1 3 4 5 ⟩

    1 ∧ 1
1

    1‿0 ∧ 1‿1
⟨ 1 0 ⟩
"])
                   (?⍋ . [ ?T
                           "⍋ 𝕩: Grade Up | 𝕨 ⍋ 𝕩: Bins Up"
                           "\
⍋ 𝕩: Grade Up
- Indices of 𝕩 that would sort its major cells in ascending order.

𝕨 ⍋ 𝕩: Bins Up
- Binary search for each cell of 𝕩 in 𝕨, returning the number of major cells in
  𝕨 less than or equal to that cell.
- 𝕨 must be sorted in ascending order."
                           "\
    a ← 3‿2‿1

    ⍋ a
⟨ 2 1 0 ⟩

    (⍋a) ⊏ a
⟨ 1 2 3 ⟩

    3‿4‿5‿7 ⍋ 2
┌·
· 0
    ┘

    3‿4‿5‿7 ⍋ 2‿6
⟨ 0 3 ⟩"])
                   (?⊔ . [ ?u
                           "⊔ 𝕩: Group Indices | 𝕨 ⊔ 𝕩: Group"
                           "\
⊔ 𝕩: Group Indices
- Group the indices of the major cells of 𝕩 by their respective values.
- 𝕩 must consist of integers. Groups start from 0.

𝕨 ⊔ 𝕩: Group
- Group the major cells of 𝕩 by their respective indices in 𝕨.
- If an element corresponds to ¯1, it is excluded from grouping.
- An extra element can be added to the end of 𝕨 to specify length of the
  result."
                           "\
    ⊔ 4‿5‿6‿6‿4‿7‿5
⟨ ⟨⟩ ⟨⟩ ⟨⟩ ⟨⟩ ⟨ 0 4 ⟩ ⟨ 1 6 ⟩ ⟨ 2 3 ⟩ ⟨ 5 ⟩ ⟩

    (↕8) ≍ ⊔ 4‿5‿6‿6‿4‿7‿5
┌─
╵ 0  1  2  3  4       5       6       7
  ⟨⟩ ⟨⟩ ⟨⟩ ⟨⟩ ⟨ 0 4 ⟩ ⟨ 1 6 ⟩ ⟨ 2 3 ⟩ ⟨ 5 ⟩
                                            ┘

    1‿0‿1‿2‿2‿3‿3  ⊔ 4‿5‿6‿6‿4‿7‿5
⟨ ⟨ 5 ⟩ ⟨ 4 6 ⟩ ⟨ 6 4 ⟩ ⟨ 7 5 ⟩ ⟩

    1‿0‿1‿¯1‿¯1‿3‿3  ⊔ 4‿5‿6‿6‿4‿7‿5
⟨ ⟨ 5 ⟩ ⟨ 4 6 ⟩ ⟨⟩ ⟨ 7 5 ⟩ ⟩

    1‿0‿1‿¯1‿¯1‿3‿3‿10  ⊔ 4‿5‿6‿6‿4‿7‿5
⟨ ⟨ 5 ⟩ ⟨ 4 6 ⟩ ⟨⟩ ⟨ 7 5 ⟩ ⟨⟩ ⟨⟩ ⟨⟩ ⟨⟩ ⟨⟩ ⟨⟩ ⟩"])
                   (?⊏ . [ ?i
                           "⊏ 𝕩: First Cell | 𝕨 ⊏ 𝕩: Select"
                           "\
⊏ 𝕩: First Cell
- First major cell of 𝕩.

𝕨 ⊏ 𝕩: Select
- Select the major cells of 𝕩 at the indices in 𝕨."
                           "\
    ⊏ ⟨1, 2, 3⟩
┌·
· 1
    ┘

    a ← 3‿3 ⥊ ↕9

    ⊏ a
⟨ 0 1 2 ⟩

    2‿0 ⊏ ⟨1, 2, 3⟩
⟨ 3 1 ⟩

    b ← 3‿3 ⥊ ↕9

    2‿0 ⊏ b
┌─
╵ 6 7 8
  0 1 2
        ┘"])
                   (?⊑ . [ ?I
                           "⊑ 𝕩: First | 𝕨 ⊑ 𝕩: Pick"
                           "\
⊑ 𝕩: First
- First element of 𝕩.

𝕨 ⊑ 𝕩: Pick
Pick the element of 𝕩 at index 𝕨."
                           "\
    ⊑ ⟨1, 2, 3⟩
1

    a ← 3‿3 ⥊ ↕9

    ⊑ a
0

    2 ⊑ ⟨1, 2, 3⟩
3

    b ← 3‿3 ⥊ ↕9

    2‿0 ⊑ b
6"])
                   (?⊐ . [ ?o
                           "⊐ 𝕩: Classify | 𝕨 ⊐ 𝕩: Index Of"
                           "\
⊐ 𝕩: Classify
- Translate major cells of 𝕩 to unique ID numbers based on first occurrence.

𝕨 ⊐ 𝕩: Index Of
- First index of each major cell of 𝕩 in 𝕨. Rank of 𝕩 must be at least cell rank
  of 𝕨.
- If a cell is not found in 𝕨, the length of 𝕨 (≠𝕨) is used for that position."
                           "\
    ⊐ 5‿6‿2‿2‿5‿1
⟨ 0 1 2 2 0 3 ⟩

    a ← 3‿3 ⥊ 0‿1‿2‿9‿0‿9‿0‿1‿2

    ⊐ a
⟨ 0 1 0 ⟩

    5‿6‿2‿2‿5‿1 ⊐ 5‿7‿1‿6
⟨ 0 6 5 1 ⟩

    b ← 3‿3 ⥊ 0‿1‿2‿9‿0‿9‿0‿1‿2

    b ⊐ ≍9‿0‿9
⟨ 1 ⟩"])
                   (?⊒ . [ ?O
                           "⊒ 𝕩: Occurrence Count | 𝕨 ⊒ 𝕩: Progressive Index Of"
                           "\
⊒ 𝕩: Occurrence Count
- Number of times each major cell of 𝕩 has occurred before the current position.

𝕨 ⊒ 𝕩: Progressive Index Of
- Index of the first unused match of each major cell of 𝕩 in 𝕨.
- If there are no more matches left, the length of 𝕨 is placed in that
  position."
                           "\
    ⊒   2‿7‿1‿8‿1‿7‿1‿8‿2‿8‿4
⟨ 0 0 0 0 1 1 2 1 1 2 0 ⟩

    ≍⟜⊒ 2‿7‿1‿8‿1‿7‿1‿8‿2‿8‿4
┌─
╵ 2 7 1 8 1 7 1 8 2 8 4
  0 0 0 0 1 1 2 1 1 2 0
                        ┘

    \"aaa\" ⊒ \"aaaaa\"
⟨ 0 1 2 3 3 ⟩

    \"aaabb\" ⊒ \"ababababab\"
⟨ 0 3 1 4 2 5 5 5 5 5 ⟩"])
                   (?π . [ ?p
                           "π: Pi"
                           "\
π: Pi
- The mathematical constant pi, a numeric literal.
- Can be negative (¯π)."
                           "\
    π
3.141592653589793

    ¯π
¯3.141592653589793"])
                   (?\[ . [ nil
                            "[ : Begin array"
                            "\
[: Begin array
- Starts a high-rank array.
- Entries must be separated by , or ⋄.
- These must have the same shape.
- They become major cells of the result.
- Must end with a corresponding ]."
                            "\
    [\"abc\", \"def\"]
┌─
╵\"abc
  def\"
      ┘

    [↕4, ↕5]
Error: >: Elements didn't have equal shapes (contained shapes ⟨4⟩ and ⟨5⟩)"])
                   (?{ . [ nil
                           "{: Begin Block"
                           "\
{: Begin Block
- Starts a block, which can be one of:
  - Function
  - 1-Modifier
  - 2-Modifier
  - Namespace
  - Immediate Block
- Must end with a corresponding }."
                           "\
    {𝕨 + 𝕩}   # Function
(function block)

    {𝕨‿𝔽‿𝕩}   # 1-modifier
(1-modifier block)

    {𝕨‿𝔽‿𝔾‿𝕩} # 2-modifier
(2-modifier block)

    {a ⇐ 5}   # Namespace
{a⇐}

    {5+4+6}   # Immediate block
15"])
                   (?← . [ ?\[
                           "n ← v: Define"
                           "\
n ← v: Define
- Defines a new variable with name n and value v.
- Variable n must not already exist in the scope."
                           "\
    ⊢ a ← 1
1

    ⊢ b ← 3‿3 ⥊ 5
┌─
╵ 5 5 5
  5 5 5
  5 5 5
        ┘

    C ← ↑"])
                   (?⊣ . [ ?{
                           "⊣ 𝕩: Identity | 𝕨 ⊣ 𝕩: Left"
                           "\
⊣ 𝕩: Identity
- Return 𝕩.

𝕨 ⊣ 𝕩: Left
- Return 𝕨."
                           "\
    ⊣ 5
5

    5 ⊣ 8
5

    'a' ⊣ 1‿2‿3
'a'"])
                   (?\] . [ nil
                            "]: End array"
                            "\
]: End array
- Ends an array started by a [.
- See Begin Array for more details."
                            "\
    [\"abc\", \"def\"]
┌─
╵\"abc
  def\"
      ┘

    [↕4, ↕5]
Error: >: Elements didn't have equal shapes (contained shapes ⟨4⟩ and ⟨5⟩)"])
                   (?} . [ nil
                           "}: End Block"
                           "\
}: End Block
- Starts a block, which starts with }.
- See Begin Block for more details."
                           "\
    {𝕨 + 𝕩}   # Function
(function block)

    {𝕨‿𝔽‿𝕩}   # 1-modifier
(1-modifier block)

    {𝕨‿𝔽‿𝔾‿𝕩} # 2-modifier
(2-modifier block)

    {a ⇐ 5}   # Namespace
{a⇐}

    {5+4+6}   # Immediate block
15"])
                   (?⊢ . [ ?}
                           "⊢ 𝕩: Identity | 𝕨 ⊢ 𝕩: Right"
                           "\
⊢ 𝕩: Identity
- Return 𝕩.

𝕨 ⊢ 𝕩: Right
- Return 𝕩."
                           "\
    ⊢ 5
5

    5 ⊢ 8
8

    'a' ⊢ 1‿2‿3
⟨ 1 2 3 ⟩"])
                   (?\| . [ nil
                            "| 𝕩: Absolute Value | 𝕨 | 𝕩: Modulus"
                            "\
| 𝕩: Absolute Value
- Absolute Value of 𝕩.
- Pervasive.

𝕨 | 𝕩: Modulus
- Remainder of 𝕩 divided by 𝕨.
- Pervasive."
                            "\
    | ¯2
2

    | 1‿3‿¯4‿3
⟨ 1 3 4 3 ⟩

    1 | 3
0

    2 | 3‿4‿¯5
⟨ 1 0 1 ⟩"])
                   ;; second row
                   (?⍉ . [ ?a
                           "⍉ 𝕩: Transpose | 𝕨 ⍉ 𝕩: Reorder Axes"
                           "\
⍉ 𝕩: Transpose
- Move the first axis of 𝕩 to the end.

𝕨 ⍉ 𝕩: Reorder Axes
- Rearrange the axes of 𝕩 as per the axis indices in 𝕨."
                           "\
    a ← 3‿3 ⥊ ↕9

    ⍉ a
┌─
╵ 0 3 6
  1 4 7
  2 5 8
        ┘

    b ← 1‿2‿3 ⥊ ↕6

    ≢⍉ b
⟨ 2 3 1 ⟩

    ≢ c ← 2‿3‿4‿5‿6 ⥊1
⟨ 2 3 4 5 6 ⟩

    ≢ 1‿3‿2‿0‿4 ⍉ c
⟨ 5 2 4 3 6 ⟩"])
                   (?𝕤 . [ ?s
                           "𝕤: Current Function"
                           "\
𝕊: Current Function
- A variable assigned to the current function block.
- 𝕤 accesses the same value but has a subject role.
- 𝕊 can be used for recursion."
                           "\
    F ← {𝕊 0: 1; 𝕩 × 𝕊 𝕩-1} # Factorial
    F 5
120

    {𝕤‿𝕤}4
⟨ (function block) (function block) ⟩"])
                   (?𝕊 . [ ?S
                           "𝕊: Current Function"
                           "\
𝕊: Current Function
- A variable assigned to the current function block.
- 𝕤 accesses the same value but has a subject role.
- 𝕊 can be used for recursion."
                           "\
    F ← {𝕊 0: 1; 𝕩 × 𝕊 𝕩-1} # Factorial
    F 5
120

    {𝕤‿𝕤}4
⟨ (function block) (function block) ⟩"])
                   (?↕ . [ ?d
                           "↕ 𝕩: Range | 𝕨 ↕ 𝕩: Windows"
                           "\
↕ 𝕩: Range
- Return all indices that would index into an array of shape 𝕩.
- When given a single number, range from 0 to 𝕩-1.

𝕨 ↕ 𝕩: Windows
- Overlapping slices from 𝕩 of shape 𝕨."
                           "\
    ↕ 4
⟨ 0 1 2 3 ⟩

    ↕ 4‿5
┌─
╵ ⟨ 0 0 ⟩ ⟨ 0 1 ⟩ ⟨ 0 2 ⟩ ⟨ 0 3 ⟩ ⟨ 0 4 ⟩
  ⟨ 1 0 ⟩ ⟨ 1 1 ⟩ ⟨ 1 2 ⟩ ⟨ 1 3 ⟩ ⟨ 1 4 ⟩
  ⟨ 2 0 ⟩ ⟨ 2 1 ⟩ ⟨ 2 2 ⟩ ⟨ 2 3 ⟩ ⟨ 2 4 ⟩
  ⟨ 3 0 ⟩ ⟨ 3 1 ⟩ ⟨ 3 2 ⟩ ⟨ 3 3 ⟩ ⟨ 3 4 ⟩
                                          ┘

    5 ↕ \"abcdefg\"
┌─
╵ \"abcde
   bcdef
   cdefg\"
          ┘

    a ← 3‿3⥊↕9

    2‿2 ↕ a
┌─
┆ 0 1
  3 4

  1 2
  4 5


  3 4
  6 7

  4 5
  7 8
      ┘"])
                   (?𝕗 . [ ?f
                           "𝕗: Modifier Left operand"
                           "\
𝔽: Modifier Left operand
- A variable assigned to the left operand of a modifier block.
- 𝕗 can be used to access the left operand as a subject."
                           "\
    5 +{𝕗⊣𝕨} 1
+"])
                   (?𝔽 . [ ?F
                           "𝔽: Modifier Left operand"
                           "\
𝔽: Modifier Left operand
- A variable assigned to the left operand of a modifier block.
- 𝕗 can be used to access the left operand as a subject."
                           "\
    5 +{𝕗⊣𝕨} 1
+"])
                   (?𝕘 . [ ?g
                           "𝕘: 2-Modifier Right operand"
                           "\
𝔾: 2-Modifier Right operand
- A variable assigned to the right operand of a 2-modifier block.
- 𝕘 can be used to access the right operand as a subject."
                           "\
    5 +{𝕘}3 1
3"])
                   (?𝔾 . [ ?G
                           "𝔾: 2-Modifier Right operand"
                           "\
𝔾: 2-Modifier Right operand
- A variable assigned to the right operand of a 2-modifier block.
- 𝕘 can be used to access the right operand as a subject."
                           "\
    5 +{𝕘}3 1
3"])
                   (?⊸ . [ ?h
                           "𝕗⊸𝔾 𝕩: Bind Left | 𝔽⊸𝔾 𝕩: Before | 𝕨 𝔽⊸𝔾 𝕩: Dyadic Before"
                           "\
𝕗⊸𝔾 𝕩: Bind Left
- Supply 𝕗 as a left argument to 𝔾 (𝕗 𝔾 𝕩).
- 𝕗 is a constant, 𝔾 must be dyadic.

𝔽⊸𝔾 𝕩: Before
- Apply 𝔽 to 𝕩, and supply it as a left argument to 𝔾 ((𝔽 𝕩) 𝔾 𝕩).
- 𝔽 must be monadic, 𝔾 must be dyadic.

𝕨 𝔽⊸𝔾 𝕩: Dyadic Before
- Apply 𝔽 to 𝕨, and supply it as a left argument to 𝔾 ((𝔽 𝕨) 𝔾 𝕩).
- 𝔽 must be monadic, 𝔾 must be dyadic."
                           "\
    3⊸- 9
¯6

    3 - 9
¯6

    -⊸+ 9
0

    - + 9
¯9

    (- 9) + 9
0

    2 -⊸+ 1
¯1

    2 - + 1
1

    (- 2) + 1
¯1"])
                   (?« . [ ?H
                           "« 𝕩: Nudge Back | 𝕨 « 𝕩: Shift Before"
                           "\
« 𝕩: Nudge Back
- Remove the first element of 𝕩, add a cell of fill values to the end of the
  first axis of 𝕩.

𝕨 « 𝕩: Shift Before
- Remove the first ≠𝕨 (length) major cells from 𝕩, join 𝕨 to the end of 𝕩.
- Ranks must match."
                           "\
    78 « 1‿2‿3
⟨ 2 3 78 ⟩

    « 1‿2‿3
⟨ 2 3 0 ⟩

    « 3‿3 ⥊ 9
┌─
╵ 9 9 9
  9 9 9
  0 0 0
        ┘

    8‿5 « 1‿2‿3
⟨ 3 8 5 ⟩

    a ← 3‿3 ⥊ 9

    1‿2‿3 « a
┌─
╵ 9 9 9
  9 9 9
  1 2 3
        ┘"])
                   (?∘ . [ ?j
                           "𝔽∘𝔾 𝕩: Atop | 𝕨 𝔽∘𝔾 𝕩: Dyadic Atop"
                           "\
𝔽∘𝔾 𝕩: Atop
- Apply 𝔾 to 𝕩, then apply 𝔽 (𝔽 𝔾 𝕩).
- 𝔽 and 𝔾 must be monadic.

𝕨 𝔽∘𝔾 𝕩: Dyadic Atop
- Apply 𝔾 to 𝕨 and 𝕩, then apply 𝔽 (𝔽 (𝕨 𝔾 𝕩)).
- 𝔽 must be monadic, and 𝔾 must be dyadic."
                           "\
    -∘- 5
5

    - - 5
5

    1 -∘+ 2
¯3

    1 - + 2
¯1

    - 1 + 2
¯3"])
                   (?○ . [ ?k
                           "𝔽○𝔾 𝕩: Atop | 𝕨 𝔽○𝔾 𝕩: Over"
                           "\
𝔽○𝔾 𝕩: Atop
- Apply 𝔾 to 𝕩, then apply 𝔽 (𝔽 𝔾 𝕩).
- 𝔽 and 𝔾 must be monadic.

𝕨 𝔽○𝔾 𝕩: Over
- Apply 𝔾 to 𝕨 and 𝕩, then apply 𝔽 to them ((𝔾 𝕨) 𝔽 (𝔾 𝕩)).
- 𝔽 must be dyadic, 𝔾 must be monadic."
                           "\
    -○- 5
5

    - - 5
5

    1 +○- 2
¯3

    1 + - 2
¯1

    (- 1) + (- 2)
¯3"])
                   (?⌾ . [ ?K
                           "𝔽⌾𝔾 𝕩, 𝕨 𝔽⌾𝔾 𝕩: Under"
                           "\
𝔽⌾𝔾 𝕩, 𝕨 𝔽⌾𝔾 𝕩: Under
- Apply transformation 𝔾 to all arguments
- Apply 𝔽 to the transformed arguments
- Undo transformation 𝔾
- Where 𝔾 must be
  - A function invertible by ⁼ (Undo)
  - A structural modification"
                           "\
    9⌾(1⊸⊑) 1‿2‿3
⟨ 1 9 3 ⟩

    √⁼ (√1) + (√9)
16

    1 +⌾√ 9
16"])
                   (?⟜ . [ ?l
                           "𝔽⟜𝕘 𝕩: Bind | 𝔽⟜𝔾 𝕩: After | 𝕨 𝔽⟜𝔾 𝕩: Dyadic After"
                           "\
𝔽⟜𝕘 𝕩: Bind
- Supply 𝕘 as a right argument to 𝔽 (𝕩 𝔽 𝕘).
- 𝕘 is a constant, 𝔽 must be dyadic.

𝔽⟜𝔾 𝕩: After
- Apply 𝔾 to 𝕩, and supply it as a right argument to 𝔽 (𝕩 𝔽 (𝔾 𝕩)).
- 𝔽 must be dyadic, 𝔾 must be monadic.

𝕨 𝔽⟜𝔾 𝕩: Dyadic After
- Apply 𝔾 to 𝕩, and supply it as a right argument to 𝔽 (𝕨 𝔽 (𝔾 𝕩)).

𝔽 must be dyadic, 𝔾 must be monadic."
                           "\
    -⟜3 9
6

    - 3 9
Error: Double subjects (missing ‿?)

    9 - 3
6

    ×⟜- 9
¯81

    × - 9
¯1

    9 × (- 9)
¯81

    2 ×⟜- 1
¯2

    2 × (- 1)
¯2"])
                   (?» . [ ?L
                           "» 𝕩: Nudge | 𝕨 » 𝕩: Shift After"
                           "\
» 𝕩: Nudge
- Remove the last element of 𝕩, add a cell of fill values to the start of the
  first axis of 𝕩.

𝕨 » 𝕩: Shift After
- Remove the last ≠𝕨 (length) major cells from 𝕩, join 𝕨 to the start of 𝕩.
- Ranks must match."
                           "\
    » 1‿2‿3
⟨ 0 1 2 ⟩

    » 3‿3 ⥊ 9
┌─
╵ 0 0 0
  9 9 9
  9 9 9
        ┘

    78 » 1‿2‿3
⟨ 78 1 2 ⟩

    1‿2 » 1‿2‿3
⟨ 1 2 1 ⟩

    a ← 3‿3 ⥊ 9

    1‿2‿3 » a
┌─
╵ 1 2 3
  9 9 9
  9 9 9
        ┘"])
                   (?⋄ . [ ?\;
                           "⋄: Separator"
                           "\
, or ⋄: Separator
- Separates statements in blocks, programs, and arrays.
- Characters , and ⋄ are interchangeable with each other and with newline."
                           "\
    a ← 3 , ⊢ b ← 2
2

    1 ⋄ 2 , 3
3

    ⟨1 , 2 ⋄ 3⟩
⟨ 1 2 3 ⟩

    {1 ⋄ 2 ⋄ 3}
3"])
                   (?· . [ ?:
                           "·: Nothing"
                           "\
·: Nothing
- Indicates no value.
- If a left argument is Nothing, the function is called with no left argument,
  and if the right is Nothing, it's not called and \"returns\" Nothing.

- In Trains:
  - Nothing can serve as a left argument in a train to string together multiple
    monadic functions.

- Destructuring
  - For pattern matching in assignment or a block header, Nothing indicates an
    unused value."
                           "\
    · ⌽ \"abc\"  # Reverse instead of Rotate
\"cba\"

    (-+-) 5
¯10

    (-·+-) 5
5

    F ← {𝕊 a‿·‿b: a∾b}

    F 1‿2‿3
⟨ 1 3 ⟩"])
                   (?\' . [ nil
                            "'': Character"
                            "\
'c': Character
- A character literal whose value is the character between quotes.
- Any character can be used, even ' and newline."
                            "\
    'a'‿'b'
\"ab\""])
                   (?\" . [ nil
                            "\"charseq\": String"
                            "\
\"charseq\": String
- Literal notation for a string, or list of characters.
- Double quotes must be escaped by writing them twice.
- Any other characters can be included directly."
                            "\
    2 ⊑ \"string\"
'r'

    2 ⊑ \"ab\"\"cd\"
'\"'"])
                   (?↩ . [ ?\'
                           "n ↩ v: Change | n F↩: Modify | n F↩ v: Modify"
                           "\
n ↩ v: Change
- Changes the value of variable with name n to value v.
- Variable n must already exist.

n F↩: Modify
- Apply function F to existing variable n, and assign the result back to n.

n F↩ v: Modify
- Assign n F v to n."
                           "\
    a ↩ 1
Error: Undefined identifier

    ⊢ b ← 3
3

    ⊢ b ↩ \"Be the change you wish to see in the world.\"
\"Be the change you wish to see in the world.\"

    ⊢ b ⌽↩
\".dlrow eht ni ees ot hsiw uoy egnahc eht eB\"

    ⊢ b ↓˜↩ 6
\" eht ni ees ot hsiw uoy egnahc eht eB\""])
                   (?˙ . [ ?\"
                           "𝔽˙ 𝕩, 𝕨 𝔽˙ 𝕩: Constant"
                           "\
𝔽˙ 𝕩, 𝕨 𝔽˙ 𝕩: Constant
- Returns a function that will always return 𝕗."
                           "\
    \"hello\" 1˙ 2
1

    \"hello\" {𝕨+𝕩}˙ 2
(function block)"])
                   ;; third row
                   (?⥊ . [ ?z
                           "⥊ 𝕩: Deshape | 𝕨 ⥊ 𝕩: Reshape"
                           "\
⥊ 𝕩: Deshape
- Put all elements of 𝕩 in a rank 1 array, promoting to an array if necessary.

𝕨 ⥊ 𝕩: Reshape
- Put all elements of 𝕩 in an array of shape 𝕨, removing elements or repeating
  them cyclically if necessary.
- A single element in 𝕨 can be a function, which will be replaced with an
  appropriate length:

  - ∘: Exact fit
  - ⌊: Round length down, discarding elements
  - ⌽: Round length up
  - ↑: Round length up, and use element fill to add extra elements."
                           "\
    ⥊ 1
⟨ 1 ⟩

    ⥊ 1‿2 ≍ 3‿4
⟨ 1 2 3 4 ⟩

    3‿3 ⥊ 3
┌─
╵ 3 3 3
  3 3 3
  3 3 3
        ┘

    2‿⌽‿2 ⥊ 1‿2‿3
┌─
╎ 1 2

  3 1
      ┘

    2‿↑‿2 ⥊ 1‿2‿3
┌─
╎ 1 2

  3 0
      ┘"])
                   (?⋈ . [ ?Z
                           "⋈ 𝕩: Enlist | 𝕨 ⋈ 𝕩: Pair"
                           "\
⋈ 𝕩: Enlist
- Put 𝕩 in a single element list. (⟨𝕩⟩)

𝕨 ⋈ 𝕩: Pair
- Put 𝕨 and 𝕩 in a two element list. (⟨𝕨, 𝕩⟩)"
                           "\
    ⋈ 1
⟨ 1 ⟩

    ⋈ 4‿4 ⥊ 3‿67‿8‿0
┌─
· ┌─
  ╵ 3 67 8 0
    3 67 8 0
    3 67 8 0
    3 67 8 0
             ┘
               ┘

    1 ⋈ 2
⟨ 1 2 ⟩

    1 ⋈ \"dsdasdas\"
⟨ 1 \"dsdasdas\" ⟩

    (3‿3 ⥊ 3) ⋈ 67‿'a'‿\"example\"
┌─
· ┌─        ⟨ 67 'a' \"example\" ⟩
  ╵ 3 3 3
    3 3 3
    3 3 3
          ┘
                                 ┘"])
                   (?𝕩 . [ ?x
                           "𝕩: Right Argument"
                           "\
𝕩: Right Argument
- A variable assigned to the right argument of a block.
- 𝕏 can be used to access the right argument as a function."
                           "\
    5 {𝕩} 1
1"])
                   (?𝕏 . [ ?X
                           "𝕏: Right Argument"
                           "\
𝕩: Right Argument
- A variable assigned to the right argument of a block.
- 𝕏 can be used to access the right argument as a function."
                           "\
    5 {𝕩} 1
1"])
                   (?↓ . [ ?c
                           "↓ 𝕩: Suffixes | 𝕨 ↓ 𝕩: Drop"
                           "\
↓ 𝕩: Suffixes
- Suffixes of array 𝕩 along its first axis

𝕨 ↓ 𝕩: Drop
- For each integer in 𝕨, drop that many elements from the beginning of each
  dimension of 𝕩.
- Negative numbers drop from the end."
                           "\
    ↓ 1‿2‿3‿4
⟨ ⟨ 1 2 3 4 ⟩ ⟨ 2 3 4 ⟩ ⟨ 3 4 ⟩ ⟨ 4 ⟩ ⟨⟩ ⟩

    a ← 3‿3 ⥊ ↕9

    ↓ a
┌─
· ┌─        ┌─        ┌─        ↕0‿3
  ╵ 0 1 2   ╵ 3 4 5   ╵ 6 7 8
    3 4 5     6 7 8           ┘
    6 7 8           ┘
          ┘
                                     ┘

    3 ↓ 1‿3‿5‿67
⟨ 67 ⟩

    b ← 4‿4 ⥊ ↕16

    3‿3 ↓ b
┌─
╵ 15
     ┘

    5‿5 ↓ b
┌┐
└┘


    3‿¯3 ↓ b
┌─
╵ 12
     ┘"])
                   (?∨ . [ ?v
                           "∨ 𝕩: Sort Down | 𝕨 ∨ 𝕩: Logical Or"
                           "\
∨ 𝕩: Sort Down
- Sort array 𝕩 in descending order.

𝕨 ∨ 𝕩: Logical Or
- Logical Or of 𝕨 and 𝕩.
- Pervasive."
                           "\
    ∨ 3‿1‿4‿1‿5
⟨ 5 4 3 1 1 ⟩

    1 ∨ 0
1

    1‿0 ∨ 1‿1
⟨ 1 1 ⟩

    0 ∨ 0
0"])
                   (?⍒ . [ ?V
                           "⍒ 𝕩: Grade Down | 𝕨 ⍒ 𝕩: Bins Down"
                           "\
⍒ 𝕩: Grade Down
- Indices of 𝕩 that would sort its major cells in descending order.

𝕨 ⍒ 𝕩: Bins Down
- Binary search for each cell of 𝕩 in 𝕨, returning the number of major cells in
  𝕨 greater than or equal to that cell.
- 𝕨 must be sorted in descending order.
- Right Pervasive.
"
                           "\
    a ← 1‿2‿3

    ⍒ a
⟨ 2 1 0 ⟩

    (⍒a) ⊏ a
⟨ 3 2 1 ⟩


    7‿5‿4‿3 ⍒ 2
┌·
· 4
    ┘

    7‿5‿4‿3 ⍒ 2‿6
⟨ 4 1 ⟩"])
                   (?⌊ . [ ?b
                           "⌊ 𝕩: Floor | 𝕨 ⌊ 𝕩: Minimum"
                           "\
⌊ 𝕩: Floor
- Round 𝕩 down to the nearest integer.
- Pervasive.

𝕨 ⌊ 𝕩: Minimum
- Minimum of 𝕨 and 𝕩.
- Pervasive."
                           "\
    ⌊ 1.2‿π‿3‿7.89
⟨ 1 3 3 7 ⟩

    1 ⌊ 2
1

    2‿3 ⌊ 4‿2
⟨ 2 2 ⟩"])
                   (?⌈ . [ ?B
                           "⌈ 𝕩: Ceiling | 𝕨 ⌈ 𝕩: Maximum"
                           "\
⌈ 𝕩: Ceiling
- Round 𝕩 up to the nearest integer.
- Pervasive.

𝕨 ⌈ 𝕩: Maximum
- Maximum of 𝕨 and 𝕩.
- Pervasive."
                           "\
    ⌈ 1.2‿π‿3‿7.89
⟨ 2 4 3 8 ⟩

    1 ⌈ 2
2

    2‿3 ⌈ 4‿2
⟨ 4 3 ⟩"])
                   (?≡ . [ ?m
                           "≡ 𝕩: Depth | 𝕨 ≡ 𝕩: Match"
                           "\
≡ 𝕩: Depth
- Highest level of nesting in 𝕩.

𝕨 ≡ 𝕩: Match
- Does 𝕨 exactly match 𝕩?"
                           "\
    ≡ 2‿3‿4
1

    ≡ ⟨2,<3,4,<<<5⟩
4

    ≡ 9
0

    1 ≡ ⟨1⟩
0

    ⟨1⟩ ≡ ⟨1⟩
1"])
                   (?≢ . [ ?M
                           "≢ 𝕩: Shape | 𝕨 ≢ 𝕩: Not Match"
                           "\
≢ 𝕩: Shape
- Length of each dimension of x.

𝕨 ≢ 𝕩: Not Match
- Does 𝕨 not exactly match 𝕩?"
                           "\
    ≢ 1
⟨⟩

    ≢ 1‿2
⟨ 2 ⟩

    ≢ 1‿2 ≍ 3‿4
⟨ 2 2 ⟩

    1 ≢ ⟨1⟩
1

    ⟨1⟩ ≢ ⟨1⟩
0"])
                   (?\, . [ nil
                            ",: Separator"
                            "\
, or ⋄: Separator
- Separates statements in blocks, programs, and arrays.
- Characters , and ⋄ are interchangeable with each other and with newline."
                            "\
    a ← 3 , ⊢ b ← 2
2

    1 ⋄ 2 , 3
3

    ⟨1 , 2 ⋄ 3⟩
⟨ 1 2 3 ⟩

    {1 ⋄ 2 ⋄ 3}
3"])
                   (?< . [ nil
                           "< 𝕩: Enclose | 𝕨 < 𝕩: Lesser Than"
                           "\
< 𝕩: Enclose
- Create a unit array containing 𝕩. ((≢<𝕩) ≡ ⟨⟩)

𝕨 < 𝕩: Lesser Than
- 𝕨 and 𝕩 can both be either numbers or characters.
- Pervasive."
                           "\
    <1
┌·
· 1
    ┘


    ≢<1
⟨⟩

    1 < 3
1

    2‿3‿0 < 3‿1‿0
⟨ 1 0 0 ⟩"])
                   (?∾ . [ ?\,
                           "∾ 𝕩: Join | 𝕨 ∾ 𝕩: Join To"
                           "\
∾ 𝕩: Join
- Join all elements of 𝕩 together.
- Element ranks must be compatible.

𝕨 ∾ 𝕩: Join To
- Join 𝕨 to 𝕩 along the first axis."
                           "\
    ∾ ⟨1‿2, 3, 4‿5⟩
⟨ 1 2 3 4 5 ⟩

    m ← (3‿1≍⌜4‿2‿5) ⥊¨ 2‿3⥊↕6

    ∾ m
┌─
╵ 0 0 0 0 1 1 2 2 2 2 2
  0 0 0 0 1 1 2 2 2 2 2
  0 0 0 0 1 1 2 2 2 2 2
  3 3 3 3 4 4 5 5 5 5 5
                        ┘

    \"abcd\" ∾ \"EFG\"
\"abcdEFG\"

    a ← 3‿3 ⥊ ↕9

    c ← 4‿3 ⥊ ↕12

    a∾c
┌─
╵ 0  1  2
  3  4  5
  6  7  8
  0  1  2
  3  4  5
  6  7  8
  9 10 11
          ┘"])
                   (?≤ . [ ?<
                           "𝕨 ≤ 𝕩: Lesser Than or Equal To"
                           "\
𝕨 ≤ 𝕩: Lesser Than or Equal To
- 𝕨 and 𝕩 can both be either numbers or characters.
- Pervasive."
                           "\
    1 ≤ 3
1

    2‿3‿0 ≤ 3‿1‿0
⟨ 1 0 1 ⟩"])
                   (?\. . [ nil
                            "ns . name: Namespace Field"
                            "\
ns . name: Namespace Field
- Access a field with name name in namespace ns.
- Field must have been exported with ⇐."
                            "\
    {a⇐1} . a
1

    {F⇐-}.F 5
¯5"])
                   (?> . [ nil
                           "> 𝕩: Merge | 𝕨 > 𝕩: Greater Than"
                           "\
> 𝕩: Merge
- Combine an array of arrays into one array.
- All elements of 𝕩 must have the same rank, and the result rank is that plus
  the rank of 𝕩.
- Returns and boxed atoms unchanged.

𝕨 > 𝕩: Greater Than
- 𝕨 and 𝕩 can both be either numbers or characters.
- Pervasive."
                           "\
    a ← ⟨⟨1, 2⟩, ⟨3, 4⟩⟩

    >a
┌─
╵ 1 2
  3 4
      ┘

    ≢a
⟨ 2 ⟩

    ≢>a
⟨ 2 2 ⟩

    1 > 3
0

    2‿3‿0 > 3‿1‿0
⟨ 0 1 0 ⟩

    'a' > 'b'
0"])
                   (?≍ . [ ?\.
                           "≍ 𝕩: Solo | 𝕨 ≍ 𝕩: Couple"
                           "\
≍ 𝕩: Solo
- Add a dimension to 𝕩.

𝕨 ≍ 𝕩: Couple
- Join 𝕨 and 𝕩 along a newly created axis."
                           "\
    ≍ 1
⟨ 1 ⟩

    ≍≍ 1
┌─
╵ 1
    ┘

    ≍≍≍ 1
┌─
╎ 1
    ┘

    ≍≍ 1‿2‿3‿4
┌─
╎ 1 2 3 4
          ┘

    ≍≍≍ 1‿2‿3‿4
┌─
┆ 1 2 3 4
          ┘

    1 ≍ 3
⟨ 1 3 ⟩

    1‿2 ≍ 2‿3
┌─
╵ 1 2
  2 3
      ┘"])
                   (?≥ . [ ?>
                           "𝕨 ≥ 𝕩: Lesser Than or Equal To"
                           "\
𝕨 ≤ 𝕩: Lesser Than or Equal To
- 𝕨 and 𝕩 can both be either numbers or characters.
- Pervasive."
                           "\
    1 ≥ 3
0

    2‿3‿0 ≥ 3‿1‿0
⟨ 0 1 1 ⟩"])
                   (?/ . [ nil
                           "/ 𝕩: Indices | 𝕨 / 𝕩: Replicate"
                           "\
/ 𝕩: Indices
- Repeat the index of each element in 𝕩 by the element's value. 𝕩 must be rank
  1.

𝕨 / 𝕩: Replicate
- Repeat each major cell in 𝕩 by the corresponding element in 𝕨.
- Unit 𝕨 applies to all elements."
                           "\
    / 1‿2‿3
⟨ 0 1 1 2 2 2 ⟩

    / 1‿0‿1
⟨ 0 2 ⟩

    3 / \"copy\"
\"cccooopppyyy\"

    1‿0‿1 / 1‿2‿3
⟨ 1 3 ⟩"])
                   (?? . [ nil
                           "?: Predicate"
                           "\
?: Predicate
- Follows a statement in a block, which must return 0 or 1.
- If it's 0, stop the current body and evaluate the next eligible one instead.
- Variables defined before the ? stay if execution continues (1) but don't carry
  over to other bodies (0)."
                           "\
    {0 ? 3 ; 4}
4

    Min ← {𝕨<𝕩 ? 𝕨 ; 𝕩}

    3 Min 5
3

    4 Min 2
2"])
                   (?≠ . [ ?/
                           "≠ 𝕩: Length | 𝕨 ≠ 𝕩: Not Equal To"
                           "\
≠ 𝕩: Length
- Length of the first dimension of 𝕩.

𝕨 ≠ 𝕩: Not Equal To
- Do argument atoms not match?
- Pervasive."
                           "\
    ≠ 3
1

    ≠ ⟨1, 2, 3⟩
3

    ≠ 3‿4‿5⥊0
3

    ≠ 1‿4‿5⥊0
1

    ≠ 4‿4‿5⥊0
4
    1 ≠ 3
1

    2‿3‿0 ≠ 3‿1‿0
⟨ 1 1 0 ⟩

    'a' ≠ 'a'
0
"])
                   (?⇐ . [ ??
                           "n ⇐ v: Export Definition | n ⇐: Export names"
                           "\
n ⇐ v: Export Definition
- Define a variable with name n and export it from the current namespace.

n ⇐: Export names
- Export the names given in n from the current namespace.
- Names must be defined somewhere in the scope."
                           "\
    ns ← { exported ⇐ 5, unexported ← 0}
    ns.exported
5
    ns.unexported
Error: No key found

    ns1 ← { ⟨alsoexported⟩⇐, exported ⇐ 5, alsoexported ← 0}
    ns1.exported
5
    ns1.alsoexported
0"])
                   (?‿ . [ 32
                           "‿: Strand"
                           "\
‿: Strand
- Create a list via strand notation.
- Placing ‿ between valid BQN expressions will create a list out of the end
  results of those expressions."
                           "\
    1‿2‿3
⟨ 1 2 3 ⟩

    +‿-‿56
⟨ + - 56 ⟩"])))
          (ht (make-hash-table)))
      (dolist (entry table)
        (puthash (car entry) (cdr entry) ht))
      ht))
  "Hash map from BQN symbols as keys to 4-vector of information:

- input key if(!) reachable via `bqn-glyph-prefix', else nil
- short description with no more than 80 characters (to fit echo area)
- long description states what symbol is and what forms symbol has
- extra description provides examples, preferably REPL-like

Description and examples as of
https://mlochbaum.github.io/BQN/help/index.html.")

(defun bqn--symbol (c)
  (gethash c bqn--symbols))
(defsubst bqn--symbol-prefixed (info)
  (aref info 0))
(defsubst bqn--symbol-eldoc (info)
  (aref info 1))
(defsubst bqn--symbol-description (info)
  (aref info 2))
(defsubst bqn--symbol-examples (info)
  (aref info 3))

(defun bqn--symbols-no-doc (&optional all)
  (let ((lst nil))
    (maphash
     (lambda (k v) (if (or all (aref v 0)) (push (cons (aref v 0) k) lst)))
     bqn--symbols)
    lst))

;;;###autoload
(defgroup bqn nil
  "Support for the BQN array programming language."
  :prefix 'bqn
  :group 'languages)

;;;; input method

(quail-define-package "BQN-Z" "UTF-8" "⍉"
                      t "Input mode for BQN" '(("\t" . quail-completion))
                      t nil nil t nil nil nil nil nil t)

(defvar bqn--glyph-prefix-table)
(defun bqn--glyph-prefix-set (symbol new)
  (setq bqn--glyph-prefix-table
        (mapcar (lambda (s) (cons (string new (car s)) (cdr s)))
                (bqn--symbols-no-doc)))
  ;; add input "escape" using the prefix key again:
  (push (cons (string new new) new) bqn--glyph-prefix-table)
  (quail-select-package "BQN-Z")
  (quail-install-map (quail-map-from-table '((default bqn--glyph-prefix-table))))
  (set-default symbol new))

(define-obsolete-variable-alias 'bqn-key-prefix
  'bqn-glyph-prefix "2023-04-21")
(defcustom bqn-glyph-prefix ?\\
  "Prefix character for BQN symbol input."
  :type 'character
  :group 'bqn
  :set #'bqn--glyph-prefix-set)

(defcustom bqn-use-input-method t
  "Should BQN source and comint modes auto-enable the `BQN-Z' input method?"
  :type 'boolean
  :group 'bqn)

(defcustom bqn-font-lock-eval t
  "Should evaluation results show syntax highlighting?"
  :type 'boolean
  :group 'bqn)

;;;; core functionality

(defface bqn-default
  '((t (:family "BQN386 Unicode")))
  "Default face for BQN source and inferior-process buffers."
  :group 'bqn)

(defface bqn-arrow
  '((t (:inherit default)))
  "Face used for BQN assignment and return arrows."
  :group 'bqn)

(defface bqn-nothing
  '((t (:inherit font-lock-constant-face)))
  "Face used for BQN Nothing (·)."
  :group 'bqn)

(defface bqn-subject
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for BQN subjects."
  :group 'bqn)

(defface bqn-function
  '((t (:inherit font-lock-function-name-face)))
  "Face used for BQN functions."
  :group 'bqn)

(defface bqn-one-modifier
  '((t (:inherit font-lock-preprocessor-face)))
  "Face used for BQN 1-modifiers."
  :group 'bqn)

(defface bqn-two-modifier
  '((t (:inherit font-lock-keyword-face)))
  "Face used for BQN 2-modifiers."
  :group 'bqn)

(defface bqn-primitive-function
  '((t (:inherit font-lock-builtin-face)))
  "Face used for primitive BQN functions."
  :group 'bqn)

(defface bqn-primitive-one-modifier
  '((t (:inherit font-lock-preprocessor-face)))
  "Face used for primitive BQN 1-modifiers."
  :group 'bqn)

(defface bqn-primitive-two-modifier
  '((t (:inherit font-lock-keyword-face)))
  "Face used for primitive BQN 2-modifiers."
  :group 'bqn)

(defface bqn-box
  '((t (:inherit shadow)))
  "Face used for BQN output boxes."
  :group 'bqn)

(defvar bqn--font-lock-defaults
  `((("'.'\\|@" . 'font-lock-string-face)
     ("^\\s *\\(Error\\)\\(: .*\\)" (1 'error) (2 'default)) ;for REPL output
     ("[{}]" . ,(if (facep 'font-lock-bracket-face) ''font-lock-bracket-face ''default))
     ("[()]" . ,(if (facep 'font-lock-bracket-face) ''font-lock-bracket-face ''default))
     ("[][⟨⟩]" . ,(if (facep 'font-lock-bracket-face) ''font-lock-bracket-face ''default))
     ("[←⇐↩]" . 'bqn-arrow)
     ("·" . 'bqn-nothing)
     ("[:;?]" . 'font-lock-type-face)
     ("[‿,⋄]" . ,(if (facep 'font-lock-delimiter-face) ''font-lock-delimiter-face ''default))
     ;; built-ins
     ("[∘○⊸⟜⌾⊘◶⎉⚇⍟⎊]" . 'bqn-primitive-two-modifier)
     ("[˙˜˘¨⌜⁼´˝`]" . 'bqn-primitive-one-modifier)
     ("[+×÷⋆√⌊⌈|¬∧∨<>≠=≤≥≡≢⊣⊢⥊∾≍⋈↑↓↕«»⌽⍉/⍋⍒⊏⊑⊐⊒∊⍷⊔!-]" . 'bqn-primitive-function)
     ;; note: π∞¯ may, in fact, be part of identifier names; order of clauses matters
     ("_𝕣_\\|•?\\_<_[A-Za-z][A-Z_a-z0-9π∞¯]*_\\_>" . 'bqn-two-modifier)
     ("_𝕣\\|•?\\_<_[A-Za-z][A-Z_a-z0-9π∞¯]*\\_>" . 'bqn-one-modifier)
     ("[𝔽𝔾𝕎𝕏𝕊]\\|•?\\_<[A-Z][A-Z_a-z0-9π∞¯]*\\_>" . 'bqn-function)
     ("[𝕗𝕘𝕨𝕩𝕤𝕣]\\|•?\\_<[a-z][A-Z_a-z0-9π∞¯]*\\_>" . 'bqn-subject) ;TODO had single • --- why?
     ("\\_<¯?\\(\\([0-9][0-9_]*\\.\\)?[0-9][0-9_]*\\(e¯?[0-9_]+\\)?\\|π\\|∞\\)\\(i¯?\\(\\([0-9_]+\\.\\)?[0-9_]+\\(e¯?[0-9_]+\\)?\\|π\\|∞\\)\\)?\\_>"
      . ,(if (facep 'font-lock-number-face) ''font-lock-number-face ''font-lock-constant-face))
     ;; *after* numbers have been parsed:
     ("\\." . ,(if (facep 'font-lock-punctuation-face) ''font-lock-punctuation-face ''default))
     ("[┌─╵╎┆┊┘]" . 'bqn-box)
     ;; anything else:
     ("[^ \r\n]" . 'error))
    nil nil nil))

(defvar bqn--syntax-table
  (let ((table (make-syntax-table)))
    ;; - semantically, BQN's primitives are rather "symbols" than punctuation
    ;; - but symbols are more problematic because 1) we cannot prevent them
    ;;   from being lumped together, also with user identifiers and 2) it
    ;;   removes the option to use symbol boundaries in font-lock expressions
    (dolist (s (bqn--symbols-no-doc))   ;with prefix-input == non-ASCII glyphs
      (modify-syntax-entry (cdr s) "." table))
    ;; TODO 𝔾𝕘𝔽𝕗𝕊𝕤𝕏𝕩𝕎𝕨𝕣 might use "_"?
    ;; correct syntax for system values, nothing and number parts, extra parens
    (modify-syntax-entry ?•  "'" table) ;expression prefix
    (modify-syntax-entry ?·  "." table)
    (modify-syntax-entry ?¯  "_" table)
    (modify-syntax-entry ?π  "_" table)
    (modify-syntax-entry ?∞  "_" table)
    (modify-syntax-entry ?\⟩  ")⟨" table)
    (modify-syntax-entry ?\⟨  "(⟩" table)
    ;; adjust ASCII glyph syntax relative to standard table
    ;; - fine: [{}]() are "()", \" is "\"" ,.:; are "."
    ;; - not legal BQN: $%&* (ww__)
    ;; - ?!` are already "."
    (dolist (s (string-to-list "+-/<=>|"))
      (modify-syntax-entry s "." table))
    (modify-syntax-entry ?@  "." table)
    (modify-syntax-entry ?#  "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?'  "\"" table)
    (modify-syntax-entry ?\\ "." table)
    table)
  "Syntax table for `bqn-mode'.")

(defconst bqn--syntax-propertize
  (syntax-propertize-rules
   ;; double quotes inside strings are not escaped, but repeated:
   ("\\(\"\\)\\([^\\\"]\\|\\(\"\"\\)\\)+\\(\"\\)" (1 "\"") (3 ".") (4 "\""))
   ;; character constants permit *any* single char, incl. the single quote:
   ("\\('\\)\\('\\)\\('\\)" (1 "\"") (2 ".") (3 "\""))))

(defun bqn--eldoc ()
  (let ((c (char-after (point))))
    (when-let* ((docs (bqn--symbol c)))
      (concat (bqn--symbol-eldoc docs) " | Input: "
              (if-let* ((prefixed (bqn--symbol-prefixed docs)))
                  (string bqn-glyph-prefix prefixed)
                (string c))))))

(define-derived-mode bqn-help--mode special-mode
  "BQN Documentation"
  "Major mode for displaying BQN documentation."
  (setq-local eldoc-documentation-function #'bqn--eldoc)
  (buffer-face-set 'bqn-default))

(defun bqn-help-symbol-info-at-point ()
  "Show full documentation for the primitive at point in a separate buffer."
  (interactive)
  (let* ((c (char-after (point)))
         (info (bqn--symbol c)))
    (unless info
      (user-error "No BQN primitive at point"))
    (let ((doc-buffer (get-buffer-create "*bqn-help*")))
      (with-current-buffer doc-buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (bqn--symbol-description info)
                  "\n\n==================== Examples ====================\n\n"
                  (with-temp-buffer
                    (set-syntax-table bqn--syntax-table)
                    (setq-local syntax-propertize-function bqn--syntax-propertize)
                    (setq-local font-lock-defaults bqn--font-lock-defaults)
                    (insert (bqn--symbol-examples info))
                    (font-lock-ensure)
                    (buffer-string))))
        (goto-char (point-min))
        (bqn-help--mode))
      (display-buffer doc-buffer))))

(defun bqn--make-glyph-map (modifier)
  "Create a new keymap using the string prefix MODIFIER."
  (let ((map (make-sparse-keymap)))
    (mapc
     (lambda (x)
       (define-key map
                   (kbd (concat modifier (single-key-description (car x))))
                   (lambda () (interactive) (insert (cdr x)))))
     (bqn--symbols-no-doc))
    ;; (define-key map [menu-bar bqn] (cons "BQN" (make-sparse-keymap "BQN"))) ;has not been used so far
    map))

(defvar bqn--glyph-map nil
  "Keymap for BQN special-glyph entry.")

(defun bqn--glyph-map-modifier-set (symbol new)
  "Set the BQN mode glyph keymap modifier to NEW and recreate the keymap."
  (set-default symbol new)
  (setq bqn--glyph-map (bqn--make-glyph-map new)))

(define-obsolete-variable-alias 'bqn-mode-map-prefix
  'bqn-glyph-map-modifier "2023-04-19")
(defcustom bqn-glyph-map-modifier "s-"
  "Keymap modifier for the special-glyph entry `bqn--glyph-map'.

If nil, `bqn-mode-map' and `bqn-comint-mode-map' will not be
changed at all.

For a change to be effective, rerun the mode function in existing
BQN buffers (or recreate them)."
  :type '(choice (const :tag "Off" nil)
                 (string :tag "Modifier prefix"))
  :group 'bqn
  :set #'bqn--glyph-map-modifier-set)

;;;###autoload
(define-derived-mode bqn-mode prog-mode "BQN"
  "Major mode for editing BQN files."
  :syntax-table bqn--syntax-table
  :group 'bqn
  (when bqn-glyph-map-modifier
    (set-keymap-parent bqn-mode-map
                       (make-composed-keymap prog-mode-map bqn--glyph-map)))
  (keymap-set bqn-mode-map "C-c C-z" #'bqn-comint-bring)
  (when bqn-use-input-method
    (activate-input-method "BQN-Z"))
  (setq-local syntax-propertize-function bqn--syntax-propertize)
  (setq-local font-lock-defaults bqn--font-lock-defaults)
  (setq-local eldoc-documentation-function #'bqn--eldoc)
  (setq-local comment-start "# ")
  (buffer-face-set 'bqn-default))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bqn\\'" . bqn-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("bqn" . bqn-mode))

;;;; inferior BQN process

(define-obsolete-variable-alias 'bqn-comint-interpreter-path
  'bqn-interpreter "2023-04-14")
(defcustom bqn-interpreter "bqn"
  "Executable of the BQN interpreter for interactive use."
  :type 'string
  :group 'bqn)

(define-obsolete-variable-alias 'bqn-comint-interpreter-arguments
  'bqn-interpreter-arguments "2023-04-14")
(defcustom bqn-interpreter-arguments '()
  "Command-line arguments to pass to the BQN interpreter."
  :type 'string
  :group 'bqn)

(defvar bqn-comint--process-name "BQN"
  "Name of BQN comint process.")

(defcustom bqn-comint-flash-on-send t
  "When non-nil, flash the region sent to BQN process."
  :type 'boolean
  :group 'bqn)

(defcustom bqn-comint-use-overlay nil
  "When non-nil, use overlays for all `bqn-comint-eval-*' style functions.
Else, the result in the minibuffer instead."
  :type 'boolean
  :group 'bqn
  :set (lambda (sym val)
         (when val (require 'eros))
         (set-default-toplevel-value sym val)))

(defun bqn--comint-prefix ()
  "The prefix for BQNs comint buffers."
  (concat "*" bqn-comint--process-name "-"))

(defconst bqn--comint-suffix "*"
  "The suffix for BQNs comint buffers.")

(defun bqn--comint-buffer-name ()
  "Return the name of the comint buffer associated to the current buffer.
If the comint buffer does not exist yet, please call `bqn-comint-buffer'
to create it."
  (let* ((pref (bqn--comint-prefix))
         (buf (or (buffer-file-name) (buffer-name))))
    (if (string-prefix-p pref buf)
        buf
      (concat pref buf bqn--comint-suffix))))

;;;###autoload
(defun bqn-comint-buffer ()
  "Run an inferior BQN process inside Emacs and return its buffer."
  (interactive)
  (let ((buf-name (bqn--comint-buffer-name)))
    ;; same buffer name as auto-created when passing nil below
    (if-let* ((buf (get-buffer buf-name)))
        (if (comint-check-proc buf)
            buf
          (error "Buffer '%s' exists but has no live process" buf-name))
      (let ((buf
             (apply #'make-comint-in-buffer
                    bqn-comint--process-name buf-name
                    bqn-interpreter nil bqn-interpreter-arguments)))
        (with-current-buffer buf
          (bqn-comint-mode)
          ;; Wait for prompt.
          (let ((proc (get-buffer-process buf)))
            (while (and (process-live-p proc)
                        (save-excursion
                          (goto-char (point-max))
                          (forward-line 0)
                          (not (looking-at-p comint-prompt-regexp))))
              (accept-process-output proc 0.05))))
        buf))))

(defun bqn-comint--escape (str)
  ;; At least for CBQN, newlines in the string trigger immediate evaluation, so
  ;; use its escape mechanism.
  (concat
   ")escaped \""
   (with-temp-buffer
     (insert str)
     (goto-char (point-min))
     (while (search-forward-regexp "[\\\"\r\n]" nil 'noerror)
       (let ((m (match-string 0)))
         (cond
          ((string= m "\\") (replace-match "\\\\" t t))
          ((string= m "\"") (replace-match "\\\"" t t))
          ((string= m (char-to-string ?\n)) (replace-match "\\n" t t))
          ((string= m (char-to-string ?\r)) (replace-match "\\r" t t)))))
     (buffer-string))
   "\""))

(defun bqn-comint-send-region (start end &optional follow)
  "Send the region bounded by START and END to the bqn-comint-process-session.

When FOLLOW is non-nil, switch to the inferior process buffer."
  (interactive "r")
  (when (= start end)
    (error "Attempt to send empty region to %s" bqn-comint--process-name))
  (when (and bqn-comint-flash-on-send (pulse-available-p))
    (pulse-momentary-highlight-region start end))
  (let ((region (buffer-substring-no-properties start end))
        (pbuf (bqn-comint-buffer)))
    (with-current-buffer pbuf
      ;; get rid of prompt for output alignment
      (goto-char (point-max))
      (comint-kill-whole-line 0))
    (comint-send-string (get-buffer-process pbuf)
                        (concat (bqn-comint--escape region) "\n"))
    (when follow
      (select-window (display-buffer pbuf)))))

(defun bqn-comint-send-dwim (&optional arg)
  "Send the active region, else the current line to the BQN process.

With non-nil prefix ARG, switch to the process buffer."
  (interactive "P")
  (cond
   ((use-region-p)
    (bqn-comint-send-region (region-beginning) (region-end) arg)
    (deactivate-mark))
   (t
    (bqn-comint-send-region (line-beginning-position) (line-end-position) arg))))

(defun bqn-comint-send-buffer (&optional arg)
  "Send the current buffer to BQN process.

With non-nil prefix ARG, switch to the process buffer."
  (interactive "P")
  (bqn-comint-send-region (point-min) (point-max) arg))

;; https://stackoverflow.com/questions/46631920/silently-send-command-to-comint-without-printing-prompt
(defun bqn--comint-call-process-silently (process command)
  "Send COMMAND to PROCESS. Returns nil if process does not exist."
  (when process
    (with-temp-buffer
     (comint-redirect-send-command-to-process
      (bqn-comint--escape command) (current-buffer) process nil t)
     ;; Wait for the process to complete
     (with-current-buffer (process-buffer process)
       (while (and (null comint-redirect-completed)
                 (accept-process-output process))))
     (goto-char (point-min))
     ;; Get output while skipping the next prompt
     (when bqn-font-lock-eval
       (set-syntax-table bqn--syntax-table)
       (setq-local syntax-propertize-function bqn--syntax-propertize)
       (setq-local font-lock-defaults bqn--font-lock-defaults)
       (font-lock-ensure))
     (string-trim-right (buffer-string)))))

(defun bqn-comint-evaluate-command (command)
  "Sends a COMMAND to an existin bqn comint process or start a new
one if one doesn't already exist."
  (let ((proc (get-buffer-process (bqn-comint-buffer))))
    (bqn--comint-call-process-silently proc command)))

(defun bqn-comint-eval-region (start end &optional arg)
  "Evaluate the region bounded by START and END with the
bqn-comint-process-session and echoes the result.  If ARG is non-nil or
when called with a prefix \\[universal-argument], insert the result in
the current buffer instead."
  (interactive "rP")
  (when (= start end)
    (error "Attempt to evaluate empty region to %s" bqn-comint--process-name))
  (when (and bqn-comint-flash-on-send (pulse-available-p))
    (pulse-momentary-highlight-region start end))
  (let* ((region (buffer-substring-no-properties start end))
         (process (get-buffer-process (bqn-comint-buffer)))
         (response (bqn--comint-call-process-silently process region))
         (r-lines (string-lines response))
         (single-line? (= 1 (length r-lines))))
    (cond
     (arg                               ; Insert in buffer
      (save-excursion
        (goto-char end)
        (if single-line?
            (insert " # ⇒ " response)
          (dolist (l r-lines)
            (insert "\n# " l)))))
     (bqn-comint-use-overlay            ; Use overlay
      (eros--make-result-overlay response
        :where end
        :duration eros-eval-result-duration
        :format (if single-line? " ⇒ %s" "%s")))
     (t                                 ; Insert in minibuffer
      (message "%s" response)))))

(defun bqn-comint-eval-dwim (&optional arg)
  "Evaluate the active region or the current line, displaying the result.
If ARG is non-nil or when called with a prefix \\[universal-argument],
insert the result in the current buffer instead."
  (interactive "P")
  (cond
   ((use-region-p)
    (bqn-comint-eval-region (region-beginning) (region-end) arg)
    (deactivate-mark))
   (t
    (bqn-comint-eval-region (line-beginning-position) (line-end-position) arg))))

(defun bqn-comint-eval-buffer (&optional arg)
  "Evaluate the current buffer contents, displaying the result.
If ARG is non-nil or when called with a prefix \\[universal-argument],
insert the result in the current buffer instead."
  (interactive "P")
  (bqn-comint-eval-region (point-min) (point-max) arg))

(defun bqn-comint-bring ()
  "Toggle between the comint buffer and its associated buffer."
  (interactive)
  (let* ((comint (bqn-comint-buffer))
         (buf (thread-last
                (buffer-name comint)
                (string-remove-prefix (bqn--comint-prefix))
                (string-remove-suffix bqn--comint-suffix)
                get-file-buffer)))
    (pop-to-buffer (if (equal (current-buffer) comint) buf comint))))

(define-derived-mode bqn-comint-mode comint-mode "BQN interactive"
  "Major mode for inferior BQN processes."
  :syntax-table bqn--syntax-table
  :group 'bqn
  (when bqn-glyph-map-modifier
    (set-keymap-parent bqn-comint-mode-map
                       (make-composed-keymap comint-mode-map bqn--glyph-map)))
  (keymap-set bqn-comint-mode-map "C-c C-z" #'bqn-comint-bring)
  (when bqn-use-input-method
    (activate-input-method "BQN-Z"))
  (setq-local syntax-propertize-function bqn--syntax-propertize)
  (setq-local font-lock-defaults bqn--font-lock-defaults)
  (setq-local comint-prompt-regexp "^   $")
  (setq-local comint-prompt-read-only t)
  (buffer-face-set 'bqn-default))

(provide 'bqn-mode)

;;; bqn-mode.el ends here
