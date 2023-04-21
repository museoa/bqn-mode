;;; bqn-symbols.el --- Documentation table for BQN symbols -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021-2023 Jeff Young
;; Copyright (C) 2023 bqn-mode project
;;
;; Author: Jeff Young <https://github.com/doyougnu>
;; Maintainer: bqn-mode project
;; Created: 2021-10-29
;; Modified: 2023-03-26
;; Version: 0.0.1
;; Keywords: convenience data docs
;; URL: https://github.com/museoa/bqn-mode
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

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

(provide 'bqn-symbols)

;;; bqn-symbols.el ends here
