;;; bqn-symbols-doc.el --- Documentation table for BQN symbols -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Jeff Young
;;
;; Author: Jeff Young <https://github.com/doyougnu>
;; Maintainer: Jeff Young <jeff@doyougnu.xyz>
;; Created: October 29, 2021
;; Modified: October 29, 2021
;; Version: 0.0.1
;; Keywords: convenience data docs
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; - used in bqn-help.el to implement eldoc for bqn-mode
;;
;;; Code:

(require 'subr-x)

;; a hash and array is not very emacs-lisp-y but because this will be user
;; facing, so we want the lowest latency possible. This hash should be treated
;; as read-only.
(defconst bqn-symbols-doc--symbol-doc-table
  #s(hash-table
     size 85 ;; set to number of symbols in bqn-symbols.el
     test equal
     data
     (;; Each entry is:
      ;; <symbol> [short-description long-description extra-description]
      ;; short-description should be <= 80 characters to fit on modeline
      ;; long-description should state what symbol is and what forms symbol has
      ;; extra-description should provide minimal examples
      ;; The indentation is purposefully strange for doc string presentation
      ;; ================================================
      ;; Arithmetic
      ;; Addition

      "+"

      ["Monad: Conjugate | Dyad: Addition | Input: +"

       "+ is a function.
  Its monadic form conjugates.
  Its dyadic form is addition.
  Can be applied to numbers, arrays and characters. For characters, uses an
    affine space relative to the linear space of numbers. Thus, 'a' + 2 is valid
    but 'a' + 'b' is not."

       "Examples:
## Monadic form
NOTE: Not implemented yet.

## Dyadic form
2 + 2
   4

2 + 3‿1‿0‿5
   ⟨ 5 3 2 7 ⟩

3 + \"abcde\"
   \"defgh\"

'a' + 'b'
   Error: +: Unexpected argument types
   at 'a' + 'b'
          ^"]

      ;; ================================================
      ;; Subtraction

      "-"

      ["Monad: Negation | Dyad: Subtraction | Input: -"

       "- is a function.
  Its monadic form negates
  Its dyadic form subtracts
  Can be applied to characters"

       "Examples:
## Monadic form
-0
   ¯0
- 1
   ¯1
--1
   1
- 1‿2‿3
   ⟨ ¯1 ¯2 ¯3 ⟩


## Dyadic form
0 - 1
   ¯1

0 - - 1
   1

0 -- 1
   1

1 - 1‿2‿3
   ⟨ 0 ¯1 ¯2 ⟩

'c' - 2
   'a'

'c' - \"abc\"
   ⟨ 2 1 0 ⟩
"]

      ;; ================================================
      ;; Multiplication

      "×"

      ["Monad: Sign | Dyad: Multiplication | Input: \\="

       "× is a function.
  Its monadic form returns the sign of its argument:
    no sign        =>  0
    positive sign  =>  1
    negative sign  =>  ¯1
  Its dyadic form multiplies."

       "Examples:
## Monadic form
× 0
  0

× 10
  1

× -100
  ¯1


## Dyadic form
2 × 2
   4

2‿1 × 2
   ⟨ 4 2 ⟩

2‿3‿4 × 5‿6‿7
   ⟨ 10 18 28 ⟩

2‿3‿4 × 5‿6
   Mapping: Equal-rank argument shapes don't agree

   2‿3‿4 × 5‿6
         ^


Note:
  To compute a logarithm use Undo: ⋆⁼ (\\+\\3):
    # base is e in monadic form
    euler ← ⋆1
    ⋆⁼ euler  => 1

    # 𝕨 is base in dyadic form
    2 ⋆⁼ 1024 => 10
    10 ⋆⁼ 100 => 2 "]

      ;; ================================================
      ;; Division

      "÷"

      ["Monad: Reciprocal | Dyad: Divide | Input: \\-"

       "÷ is a function.
  Its monadic form computes 1÷x, where x is ÷'s argument
  Its dyadic form is division"

       "Examples:
## Monadic form
÷ 0
  ∞

÷ 2
  0.5

÷ 1‿2‿3‿5
  ⟨ 1 0.5 0.3333333333333333 0.2 ⟩


## Dyadic form
3 ÷ 2
   1.5

0‿1‿2‿3 ÷ 1‿2‿3‿5
   ⟨ 0 0.5 0.6666666666666666 0.6 ⟩

1‿2‿3 ÷ 1‿2
   Mapping: Equal-rank argument shapes don't agree

   1‿2‿3 ÷ 1‿2
         ^ "]

      ;; ================================================
      ;; Exponentiation

      "⋆"

      ["Monad: Exponential | Dyad: Power | Input: \\+"

       "⋆ is a function.
  Its monadic form raises its argument to euler's number.
  Its dyadic form raises 𝕨 to 𝕩."

       "Examples:
## Monadic form
⋆ 0
   1

⋆ 5
   148.4131591025766

⋆ 0‿1‿2
   ⟨ 1 2.718281828459045 7.38905609893065 ⟩


## Dyadic form
0 ⋆ 3‿4‿5
   ⟨ 0 0 0 ⟩

2 ⋆ 3‿4‿5
   ⟨ 8 16 32 ⟩

0‿1‿2 ⋆ 2‿3‿4
   ⟨ 0 1 16 ⟩

0‿1‿2 ⋆ 2‿3‿4‿5
   Mapping: Equal-rank argument shapes don't agree

   0‿1‿2 ⋆ 2‿3‿4‿5
         ^ "]

      ;; ================================================
      ;; Root

      "√"

      ["Monad: Square Root | Dyad: Root | Input: \\_"

       "√ is a function.
  Its monadic form computes the square root of its argument.
  Its dyadic form computes the root of 𝕩 with the degree 𝕨."

       "Examples:
## Monadic form
√4
   2

√ 0‿1‿4‿9‿16‿25‿36‿¯49
   ⟨ 0 1 2 3 4 5 6 NaN ⟩


## Dyadic form
0‿1‿2‿3 √ 64‿64‿64‿64
   ⟨ ∞ 64 8 4 ⟩

¯0‿0‿¯2‿2‿¯2 √ 1‿¯1‿¯1‿4‿4
   ⟨ 1 1 NaN 2 0.5 ⟩
          "]

      ;; ================================================
      ;; Floor

      "⌊"

      ["Monad: Floor | Dyad: Minimum | Input: \\b"

       "⌊ is a function.
  Its monadic form returns the floor of its argument.
  Its dyadic form returns the minimum of its arguments."

       "Examples:
## Monadic form
⌊ π
   3

⌊ 2.71827
   2


## Dyadic form
0‿1‿2‿3‿4 ⌊ 4‿3‿2‿1‿0
   ⟨ 0 1 2 1 0 ⟩

0‿¯1‿¯2‿¯3‿¯4 ⌊ 4‿3‿2‿1‿0
   ⟨ 0 ¯1 ¯2 ¯3 ¯4 ⟩


Note:
  To take a minimum of an entire list, use the fold: ⌊´ (\\b\\5)
          "]

      ;; ================================================
      ;; Ceiling

      "⌈"

      ["Monad: Ceiling | Dyad: Maximum | Input: \\B"

       "⌈ is a function.
  Its monadic form returns the ceiling of its argument.
  Its dyadic form returns the maximum of its arguments."

       "Examples:
## Monadic form
 ⌈ π
   4

 ⌈ 2.71827
   3


## Dyadic form
0‿1‿2‿3‿4 ⌈ 4‿3‿2‿1‿0
   ⟨ 4 3 2 3 4 ⟩

0‿¯1‿¯2‿¯3‿¯4 ⌈ 4‿3‿2‿1‿0
   ⟨ 4 3 2 1 0 ⟩


Note:
  To take a maximum of an entire list, use the fold: ⌈´ (\\B\\5)"]

      ;; ================================================
      ;; Absolute value

      "|"

      ["Monad: Absolute Value | Dyad: Modulus | Input: |"

       "| is a function.
  Its monadic form returns the absolute value of its argument.
  Its dyadic form returns the remainder resulting from division of 𝕩 by 𝕨."

       "Examples:
## Monadic form
| ¯1
   1

| ¯1‿¯2‿¯3‿¯4
   ⟨ 1 2 3 4 ⟩


## Dyadic form
3 | 0‿1‿2‿3‿4‿5‿6‿7
   ⟨ 0 1 2 0 1 2 0 1 ⟩

0‿¯1‿¯2‿¯3‿¯4 ⌈ 4‿3‿2‿1‿0
   ⟨ 4 3 2 1 0 ⟩

∞ | 0
   0

0 | ∞
   NaN "]

      ;; ================================================
      ;; Comparisons
      ;; Equality
"="

["Monad: Rank | Dyad: Equals | Input: ="

 "= is a function.
  Its monadic form returns the rank of its input.
  Its dyadic form tests for atomic equality of its arguments:
    Found to be equal     => 1
    Not found to be equal => 0
  Note: values of different types can never be equal.
        characters are equal if they have the same code point (i.e c - @, where c is the char)."

 "Examples:

## Monadic form
= 'a'
   0

= 1
   0

= ↕100
   1

= 2‿3 ⥊ '0'+↕10
   2


## Dyadic form
(2 + 2) = 4
   1

Even ← 0=2|⊣
Even ↕10
   ⟨ 1 0 1 0 1 0 1 0 1 0 ⟩"]

      ;; ================================================
      ;; Inequality
"≠"

["Monad: Length | Dyad: Not Equals | Input: \\\/"

 "≠ is a function.
  Its monadic form returns the length of its input.
  Its dyadic form tests for atomic inequality of its arguments:
    Found to be not equal     => 1
    Not found to be not equal => 0
  Note: values of different types can never be equal."

 "Examples:

## Monadic form
≠ 'a'
   1

≠ 1
   0

≠ ↕100
   100


## Dyadic form
'b' ≠ \"abacba\"
   ⟨ 1 0 1 1 0 1 ⟩"]

      ;; ================================================
      ;; Less than or equal
"≤"

["Dyad: Less than or equal | Input: \\<"

 "≤ is a function.
  It has no monadic form.
  Its dyadic form tests for less than or equal to:
  "

 ""]

      ;; ================================================
      ;; Greater than or equal
"≥"

["Dyad: Greater than or equal | Input: \\>"

 "≥ is a function.
  It has no monadic form.
  Its dyadic form tests for greater than or equal to:
  "

 ""]

      ;; ================================================
      ;; less than
"<"

["Monad: Enclose | Dyad: Less than | Input: <"

 "< is a function.
  It monadic form returns its argument in a unit array.
  Its dyadic form returns the result comparing 𝕨 with 𝕩.
  Note: characters are always considered greater than numbers, even ∞"

 "Examples:

## Monadic form
< \"singleton\"
   ┌·
   · \"singleton\"
              ┘

<<< \"present\"
   ┌·
   · ┌·
     · ┌·
       · \"present\"
                   ┘
                     ┘
                       ┘


## Dyadic form
1 < 2
   1

4 < ↕8
   ⟨ 0 0 0 0 0 1 1 1 ⟩

↕4 < ↕8
   ↕0‿0‿0‿0‿0‿1‿1‿1

∞ < 'a'
   1

'a' < ∞
   0"]

      ;; ================================================
      ;; greater than
">"

["Monad: Merge | Dyad: Greater than | Input: >"

 "> is a function.
  It monadic form ensures that any inner arrays, in its argument,
    can fit together in an array (i.e. flatten ragged inner arrays).
  Its dyadic form returns the result comparing 𝕨 with 𝕩.
  Note: characters are always considered greater than numbers, even ∞"

 "Examples:

## Monadic form
a ← \"AB\"‿\"CD\" ∾⌜ \"rst\"‿\"uvw\"‿\"xyz\"
   ┌─
   ╵ \"ABrst\" \"ABuvw\" \"ABxyz\"
     \"CDrst\" \"CDuvw\" \"CDxyz\"
                          ┘

> a
   ┌─
   ╎\"ABrst
     ABuvw
     ABxyz

    ·CDrst
     CDuvw
     CDxyz\"
         ┘


## Dyadic form
1 > 2
   0

4 . ↕8
   ⟨ 1 1 1 1 0 0 0 0 ⟩

↕4 > ↕8
   ↕1‿1‿1‿1‿0‿0‿0‿0

'a' > ∞
   1

∞ > 'z'
   0"]

      ;; ================================================
      ;; Boolean functions
      ;; Sort Up
"∧"

["Monad: Sort Up | Dyad: Logical And | Input: \\t"

 "∧ is a function.
  Its monadic form reorders the major cells of its argument to place them in
    ascending order.
  Its dyadic form returns the result of a logical And on the input arguments.
  "

 "Examples:

## Monadic form
∧ \"delta\"‿\"alpha\"‿\"beta\"‿\"gamma\"
   ⟨ \"alpha\" \"beta\" \"delta\" \"gamma\" ⟩

∧ ⌽↕10
   ⟨ 0 1 2 3 4 5 6 7 8 9 ⟩


## Dyadic form
## Notice that dyadic ∧ is just ×
1‿2‿3‿4‿5 ∧ 5‿4‿3‿2‿1
   ⟨ 5 8 9 8 5 ⟩

## folding with ∧: if 0 is not present we take the product
∧´ 1‿2‿3‿4‿5
   120

## if present we get a logical false back
∧´ 0‿1‿2‿3‿4
   0

## similarly we can fold an array of booleans
∧´ 1‿1‿1‿1‿1
   1

## or fold with a predicate
∧´ 0≤ ↕10
   1

∧´ 'a'≤ \"purple\"
   1"]

      ;; ================================================
      ;; Sort Up
"∨"

["Monad: Sort Down | Dyad: Logical Or | Input: \\v"

 "∨ is a function.
  Its monadic form reorders the major cells of its argument to place them in
    descending order.
  Its dyadic form returns the result of a logical Or on the input arguments.
  "

 "Examples:

## Monadic form
∨ \"delta\"‿\"alpha\"‿\"beta\"‿\"gamma\"
   ⟨ \"gamma\" \"delta\" \"beta\" \"alpha\" ⟩

∨ ↕10
   ⟨ 9 8 7 6 5 4 3 2 1 0 ⟩

## Dyadic form
## Notice that dyadic ∨ is just ×
1‿2‿3‿4‿5 ∨ 5‿4‿3‿2‿1
   ⟨ 1 ¯2 ¯3 ¯2 1 ⟩

1‿2‿3‿4‿5 ×⌾¬  5‿4‿3‿2‿1
   ⟨ 1 ¯2 ¯3 ¯2 1 ⟩

## folding with ∨: tests that any element is greater than 0
∨´ ¯1‿0‿1‿2‿3‿4‿5
   1

∨´ 0‿0‿0‿0
   0

∨´ 1‿0‿0‿1
   1

## or fold with a predicate
∨´ 3≥ 1‿2‿3‿4‿5
   1

∨´ 'z'≤ \"purple\"
   0 "]

      ;; ================================================
      ;; Boolean Not
"¬"

["Monad: Not | Dyad: Span | Input: \\~"

 "¬ is a function.
  Its monadic form returns the Boolean negation of its input.
  Its dyadic form returns the number of integers separating 𝕨 from 𝕩, inclusive,
    only when 𝕩≤𝕨 and both are integers.
  Note: defined as the fork, 1+-
        considered an arithmetic function.
        considered pervasive."

 "Examples:

## Monadic form
¬ 0
   1

¬ 1
   0

¬ ˜1729
   1

¬ ˜1‿0‿1‿2‿3
   ⟨ 1 1 1 1 1 ⟩


## Dyadic form
## Notice 0 is counted
10 ¬ 0
   11

## Notice that when 𝕨≥𝕩 ¬ returns the exclusive span as a negative integer
0 ¬ 10
   ¯9

## Dyadic form can be used on characters since it is considered
## an arithmetic function
'a' ¬ 'c'
   ¯1

'a' ¬ @
   98"]

      ;; ================================================
      ;; Equality
"≡"

["Monad: Depth | Dyad: Match | Input: \\m"

 "≡ is a function.
  Its monadic form returns the depth (i.e. the level of nesting) of its input.
  Its dyadic form tests equivalency between 𝕩 and 𝕨, returns 1 if equivalent
    and 0 otherwise.
  Note: see related function ≢ (Not Match)
        always returns the same result as = (Equals) when 𝕩 and 𝕨 are atoms.
"


 "Examples:

## Monadic form
≡ 'a'
   0

≡ 1‿2‿3
   1

≡ \"a depth of 1\"
   1

≡ <<<< \"an extra four levels of nesting\"
   5

## Dyadic form
\"abc\" ≡ 'a'‿'b'‿'c'
   1  ## equivalent

4 ≡ <4
   0  ## not equivalent

## Notice ≡ returns 0 but = errors out since both arguments are not atoms.
\"abc\" ≡ 'a'‿'b'
   0

\"abc\" = \"ab\"
   Error: =: Expected equal shape prefix (⟨3⟩ ≡ ≢𝕨, ⟨2⟩ ≡ ≢𝕩)
at \"abc\" = \"ab\"
         ^"]

      ;; ================================================
      ;; Shape
"≢"

["Monad: Shape | Dyad: Not Match | Input: \\M"

 "≢ is a function.
  Its monadic form returns the shape of its input. The shape is a list of natural
    numbers.
  Its dyadic form tests in-equivalency between 𝕩 and 𝕨, returns 0 if equivalent
    and 1 otherwise.
  Note: = (Rank) and ≠ (Length) can be derived from ≢ (Shape).
        Rank can be defined as =∘≢
        Length can be defined as a fold: 1⊣´≢
        See related function ⥊ (Reshape)"


 "Examples:

## Monadic form
## Make a 4-dimensional array of length 1, rank 4
## The only element is an array of shape 3‿2‿6, i.e., an array with 3-elements
## where each element is an array of 2 elements that are length 6.
⊢ array ← 1‿3‿2‿6 ⥊ '0'+↕10
   ┌─
   ┆\"012345
     678901

    ·234567
     890123

    ·456789
     012345\"
             ┘
      
≢ array # Shape
   ⟨ 1 3 2 6 ⟩

≠ array # Length
   1

= array # Rank
   4


## Dyadic form
\"abc\" ≢ 'a'‿'b'‿'c'
   0  ## equivalent

4 ≢ <4
   1  ## not equivalent

## Notice ≢ returns 1 but = errors out since both arguments are not atoms.
\"abc\" ≢ 'a'‿'b'
   1

\"abc\" = \"ab\"
   Error: =: Expected equal shape prefix (⟨3⟩ ≡ ≢𝕨, ⟨2⟩ ≡ ≢𝕩)
at \"abc\" = \"ab\"
         ^"]

      ;; ================================================
      ;; Left Identity
"⊣"

["Monad: Identity | Dyad: Left | Input: \\{"

 "⊣ is a function.
  Its monadic form returns its input.
  Its dyadic form returns 𝕨."


 "Examples:

## Monadic form
⊣ 1
   1

⊣ 'a'
   'a'

⊣ <<↕10
   ┌·
   · ┌·
     · ⟨ 0 1 2 3 4 5 6 7 8 9 ⟩
                              ┘
                                ┘

## Dyadic form
\"left\" ⊣ \"right\"
   \"left\"

## ⊣ allows a nice way to create a matrix if combined with ⌜ (the Table modifier)
(↕4) ⊣⌜ ↕5
   ┌─
   ╵ 0 0 0 0 0
     1 1 1 1 1
     2 2 2 2 2
     3 3 3 3 3
               ┘

## The same trick applies with ¨ (the Each modifier)
(↕4) ⊣¨ ↕4‿5
   ┌─
   ╵ 0 0 0 0 0
     1 1 1 1 1
     2 2 2 2 2
     3 3 3 3 3
               ┘

## Another use case is replacing values in a nested array using dyadic ⌾ (Under).
## When 𝔽 (left operand) is ⊣, selected values come from 𝕨 (left), unselected
## values from 𝕩 (right). This works even for deeply nested arrays as long as
## you can write a function that accesses the values:
\"ABCDE\" ⊣⌾(0‿1‿1‿0‿0⊸/) \"abcde\"
   \"aBCde\"

## ∘⊣ can be used to ignore a right argument for modified assignment. It applies
## a function \"in place\" to a variable without writing the variable name twice.
## For example, we can modify a variable with its reversal (⌽) in place:
a ← \"hello I'm a\"
   \"hello I'm a\"

a ⌽∘⊣↩ @
   \"a m'I olleh\""]

      ;; ================================================
      ;; Right Identity
"⊢"

["Monad: Identity | Dyad: Right | Input: \\}"

 "⊢ is a function.
  Its monadic form returns its input.
  Its dyadic form returns 𝕩 (its right argument)."


 "Examples:

## Monadic form
⊢ 1
   1

⊢ 'a'
   'a'

⊢ <<↕10
   ┌·
   · ┌·
     · ⟨ 0 1 2 3 4 5 6 7 8 9 ⟩
                               ┘
                                 ┘


## Dyadic form
\"left\" ⊢ \"right\"
   \"right\"

## A common hack is to use ⊢ to display the value of a variable in an assignemnt
⊢ a ← \"show me right after assignment\"
   \"show me right after assignment\"

## Note: In APL a tack can be used to avoid stranding numbers together. In BQN,
## stranding is explicit so these are identical, notice the missing ⊢ in the
## second example:
÷⟜2⍟3⊢ 24
   3

÷⟜2⍟3 24
   3"]

      ;; ================================================
      ;; Reshape
"⥊"

["Monad: Deshape | Dyad: Reshape | Input: \\z"

 "⥊ is a function.
  Its monadic form removes all shape information from its input. Returning a
    list of all elements from the array in reading order.
  Its dyadic form ignores the shape information of 𝕩 and adds shape information
    based on 𝕨.
  Note: in its dyadic form, one entry of 𝕨 may be left for BQN to fill in.
        when the number of elements implied by 𝕨 is equal to the number of
          elements in 𝕩, 𝕩 is rearranged to match that shape.
        when 𝕨 implies less elements than 𝕩 has, then only as many elements
          as needed from 𝕩 are used, and the rest ignored.
        when 𝕨 implies more elements than 𝕩 has, then the elements of 𝕩 are
          reused cyclically.
        see related function ≍ (Solo)."


 "Examples:

## Monadic form
## Deshape returns a list in reading order: left to right, top to bottom.
⊢ a ← +⌜´ ⟨100‿200, 30‿40, 5‿6‿7⟩
   ┌─
   ╎ 135 136 137
     145 146 147

     235 236 237
     245 246 247
                 ┘

⥊ a
   ⟨ 135 136 137 145 146 147 235 236 237 245 246 247 ⟩

## When 𝕩 is an atom, ⥊ encloses it into a singleton list
## In this scenario, ≍ (Solo) is preferred
⥊ 2
   ⟨ 2 ⟩

≍ 2
   ⟨ 2 ⟩

## Dyadic form
## Reshape a into 6 rows of 2, notice 𝕨 implies 12 elements which is exactly
## ×´≢ a (the number of elements in 𝕩), thus every element is used.
6‿2 ⥊ a
   ┌─
   ╵ 135 136
     137 145
     146 147
     235 236
     237 245
     246 247
             ┘

## A common use case is to generate an array with a specified shape that counts
## up from 0.
2‿7 ⥊ ↕14
   ┌─
   ╵ 0 1 2  3  4  5  6
     7 8 9 10 11 12 13
                       ┘

## prefer the phrase ⥊⟜(↕×´) 2‿7 for this use case, it only requires the shape
⥊⟜(↕×´) 2‿7
   ┌─
   ╵ 0 1 2  3  4  5  6
     7 8 9 10 11 12 13
                       ┘

## Reshape into 3 rows of 3 elements, notice 𝕨 implies 9 elements but 𝕩 has 12
## thus only the first 9 elements in reading order of 𝕩 are returned
3‿3 ⥊ a
   ┌─
   ╵ 135 136 137
     145 146 147
     235 236 237
                 ┘

## Reshape into a list of 15 elements, notice 3 elements are reused since 𝕨
## implies more elements than 𝕩 has.
15 ⥊ a
   ⟨ 135 136 137 145 146 147 235 236 237 245 246 247 135 136 137 ⟩

## A common use case for ⥊ is to create an array filled with a constant value
3‿4 ⥊ 0
   ┌─
   ╵ 0 0 0 0
     0 0 0 0
     0 0 0 0
             ┘

## For an atom, first enclose the atom, then reshape
5 ⥊ < \"I'm an atom\"
   ⟨ \"I'm an atom\" \"I'm an atom\" \"I'm an atom\" \"I'm an atom\" \"I'm an atom\" ⟩

## Combine ⥊, with ∘ (Atop), ⌊ (Floor), ⌽ (Reverse), ↑ (Take), to omit an
## argument in 𝕨

## with ∘, lengths must match, that is 2| ×´≢ 𝕩 = 0
2‿∘ ⥊ \"abcde\"
   Error: ⥊: Shape must be exact when reshaping with ∘
at 2‿∘ ⥊ \"abcde\"
       ^

## with ⌊, the length is rounded down, so some elements are discarded
2‿⌊ ⥊ \"abcde\"
   ┌─
   ╵\"ab
     cd\"
        ┘

## with ⌽, the length is rounded up, thus elements are repeatedly used
2‿⌽ ⥊ \"abcde\"
   ┌─
   ╵\"abc
     dea\"
         ┘

## with ↑, the fill element is used to pad the array to the proper shape
2‿↑ ⥊ \"abcde\"
   ┌─
   ╵\"abc
     de \"
         ┘"]

      ;; ================================================
      ;; Join
"∾"

["Monad: Join | Dyad: Join to | Input: \,"

 "∾ is a function.
  Its monadic form concatenates the elements of its input.
  Its dyadic form returns an array whose major cells are the major cells from
     𝕨 followed by the major cells of 𝕩."


 "Examples:

## Monadic form
∾ \"time\"‿\"to\"‿\"join\"‿\"some\"‿\"words\"
   \"timetojoinsomewords\"

## Result must be rank 0
∾ \"abcd\"
   Error: ∾𝕩: 𝕩 must have an element with rank at least =𝕩
at ∾ \"abcd\"
   ^

## join with a space separator, then remove the leading space after joining
1↓∾' '∾¨\"time\"‿\"to\"‿\"join\"‿\"some\"‿\"words\"
   \"time to join some words\"

## join can be used to merge higher-dimensional arrays, as long as m≤n, where
## m is the rank of 𝕨, and n the rank of 𝕩.
⊢ m ← (3‿1≍⌜4‿2‿5) ⥊¨ 2‿3⥊↕6
   ┌─
   ╵ ┌─          ┌─      ┌─
     ╵ 0 0 0 0   ╵ 1 1   ╵ 2 2 2 2 2
       0 0 0 0     1 1     2 2 2 2 2
       0 0 0 0     1 1     2 2 2 2 2
               ┘       ┘             ┘
     ┌─          ┌─      ┌─
     ╵ 3 3 3 3   ╵ 4 4   ╵ 5 5 5 5 5
               ┘       ┘             ┘
                                       ┘

## now join the array
∾ m
   ┌─
   ╵ 0 0 0 0 1 1 2 2 2 2 2
     0 0 0 0 1 1 2 2 2 2 2
     0 0 0 0 1 1 2 2 2 2 2
     3 3 3 3 4 4 5 5 5 5 5
                           ┘

## Axes with length 1 can be left out, but must be consistently left out
⊢ n ← 2‿4‿6 ×{⟨𝕗,𝕩⟩≍⟨𝕨,𝕨𝔽⌜𝕩⟩} 5‿6‿7‿8
   ┌─
   ╵ ×         ⟨ 5 6 7 8 ⟩
     ⟨ 2 4 6 ⟩ ┌─
               ╵ 10 12 14 16
                 20 24 28 32
                 30 36 42 48
                             ┘
                               ┘

## return the shape of each element, notice we have different shapes but
## compatible ranks
≢¨ n
   ┌─
   ╵ ⟨⟩    ⟨ 4 ⟩
     ⟨ 3 ⟩ ⟨ 3 4 ⟩
                   ┘

## and so we can join, and the length 1 axes are used consistently as borders of
## the multiplication table
∾ n
   ┌─
   ╵ × 5  6  7  8
     2 10 12 14 16
     4 20 24 28 32
     6 30 36 42 48
                   ┘


## Dyadic form
\"abcd\" ∾ \"EFG\"
   \"abcdEFG\"

## arrays of rank 2 or more are joined vertically
⊢ a ← 3 +⌜○↕ 4
   ┌─
   ╵ 0 1 2 3
     1 2 3 4
     2 3 4 5
             ┘

⊢ b ← 2‿4 ⥊ ↕8
   ┌─
   ╵ 0 1 2 3
     4 5 6 7
             ┘

a ∾ b
   ┌─
   ╵ 0 1 2 3
     1 2 3 4
     2 3 4 5
     0 1 2 3
     4 5 6 7
             ┘

## Edge case: can be applied to units to make a list
## Why: rank of the result is greater than either argument.
'a' ∾ 0
   ⟨ 'a' 0 ⟩

## 𝕨 (left) and 𝕩 (right) must have the same shape
a ∾ 2‿5⥊b  # Shapes don't fit
   Error: ∾: Lengths not matchable (3‿4 ≡ ≢𝕨, 2‿5 ≡ ≢𝕩)
at a ∾ 2‿5⥊b  # Shapes don't fit
     ^

## however, ranks can be at most one apart
4‿2‿3‿0 ∾ a
   ┌─
   ╵ 4 2 3 0
     0 1 2 3
     1 2 3 4
     2 3 4 5
             ┘"]

      ;; ================================================
      ;; 𝕩
"𝕩"

["Right argument of a block or function | Input: \\x or \\X"

 "𝕩 and 𝕏 is a reserved name.
  It always refers to the right argument of a function.
  See related form, 𝕨 (left argument)."


 "Examples:

## Use in a block
{𝕩+1} 2
   3

×{𝕩𝔽𝕩} 4
   16

## In a function
F ← {𝕩 × 𝕩}
   (function block)

F 2
   4 "]

      ;; ================================================
      ;; 𝕏
"𝕏"

["Right argument of a block or function | Input: \\x or \\X"

 "𝕩 and 𝕏 is a reserved name.
  It always refers to the right argument of a function.
  See related form, 𝕨 (left argument)."


 "Examples:

## Use in a block
{𝕩+1} 2
   3

×{𝕩𝔽𝕩} 4
   16

## In a function
F ← {𝕩 × 𝕩}
   (function block)

F 2
   4"]

      ;; ================================================
      ;; 𝕨
"𝕨"

["Left argument of a block or function | Input: \\w or \\W"

 "𝕨 and 𝕎 is a reserved name.
  It always refers to the left argument of a function.
  See related form, 𝕩 (right argument)."


 "Examples:

## Use in a block
'c' {𝕨=𝕩} \"abcd\"
   ⟨ 0 0 1 0 ⟩

3 { (2×𝕨)-𝕩 } 1
   5


## When 𝕨 occurs in a function called with one argument, it is filled
## with · (Nothing). This use of 𝕨 is discouraged.
3 { (2×𝕨)-𝕩 } 1
   5

{ (2×𝕨)-𝕩 } 1          # 𝕨 is · so (2×𝕨) is not evaluated, then - is monadic
   ¯1

## Note: this may lead to surprisingly different behavior for ⊸ and ⟜
{ 𝕨 ⋆⊸- 𝕩 } 5
   143.4131591025766   # · ⋆⊸- 𝕩, expands to, ⋆⊸- 𝕩, which is, (⋆𝕩)-𝕩, not -𝕩"]

      ;; ================================================
      ;; 𝕎
"𝕎"

["Left argument of a block or function | Input: \\w or \\W"

 "𝕨 and 𝕎 is a reserved name.
  It always refers to the left argument of a function.
  See related form, 𝕩 (right argument)."


 "Examples:

## Use in a block
'c' {𝕨=𝕩} \"abcd\"
   ⟨ 0 0 1 0 ⟩

3 { (2×𝕨)-𝕩 } 1
   5


## When 𝕨 occurs in a function called with one argument, it is filled
## with · (Nothing). This use of 𝕨 is discouraged.
3 { (2×𝕨)-𝕩 } 1
   5

{ (2×𝕨)-𝕩 } 1          # 𝕨 is · so (2×𝕨) is not evaluated, then - is monadic
   ¯1

## Note: this may lead to surprisingly different behavior for ⊸ and ⟜
{ 𝕨 ⋆⊸- 𝕩 } 5
   143.4131591025766   # · ⋆⊸- 𝕩, expands to, ⋆⊸- 𝕩, which is (⋆𝕩)-𝕩, not -𝕩"]

      ;; ================================================
      ;; Solo
"≍"

["Monad: Solo | Dyad: Couple | Input: \\."

 "≍ is a function.
  Its monadic form returns an array with its input as the only major cell.
  Its dyadic form returns an array with elements 𝕩 and 𝕨, and outer axis of
    length-2
  See related form, > (Merge). Merge is considered a generalized form of ≍.
  See related form, ⋈ (Pair).
  Note: ≍ ←→ >{⟨𝕩⟩;⟨𝕨,𝕩⟩} or in other words: Solo is {>⟨𝕩⟩}, Couple is {>⟨𝕨,𝕩⟩}"


 "Examples:

## Monadic form
## Notice that ≍ always adds an axis, thus applied to unit values returns a list
≍ 2
   ⟨ 2 ⟩

≍ 'a'
   \"a\"

## a length-1 axis is always added
≢ ↕5
   ⟨ 5 ⟩

≢ ≍ ↕5
   ⟨ 1 5 ⟩

## Dyadic form
## Couple two arrays of shape 2‿3
 ⊢ p ← 3‿5×⌜↕3
   ┌─
   ╵ 0 3  6
     0 5 10
            ┘

⊢ q ← 2‿3⥊\"abcdef\"
   ┌─
   ╵\"abc
     def\"
         ┘

p ≍ q   # p coupled to q
   ┌─
   ╎ 0   3   6
     0   5   10

     'a' 'b' 'c'
     'd' 'e' 'f'
                 ┘

## Notice that the outer axis is length 2 because ≍ had two arguments
≢ p ≍ q
   ⟨ 2 2 3 ⟩"]

      ;; ================================================
      ;; Pair
"⋈"

["Monad: Enlist | Dyad: Pair | Input: \\Z"

 "⋈ is a function.
  Its monadic form returns a singleton list containing its input.
  Its dyadic form a list containing both 𝕨 and 𝕩.
  See related form, > (Merge).
  See related form, ≍ (Solo).
  Note: ⋈ ←→ ≍○<, and ≍ ←→ >∘⋈"


 "Examples:

## Monadic form
⋈ \"enlist\"    # ⟨𝕩⟩
   ⟨ \"enlist\" ⟩

⋈ ↕5
   ⟨ ⟨ 0 1 2 3 4 ⟩ ⟩

## Dyadic form
## A common pattern is to use ⋈ in a train, to give the results of applying each
## of two functions
'c' (+⋈-)  1‿2       # capture the result of + and - using ⋈
   ⟨ \"de\" \"ba\" ⟩

# This pattern can be extended with <⊸∾ (prepend a single element to a list)
# Lispers would call <⊸∾ \"cons\"
\"e0\" <⊸∾ \"e1\" <⊸∾ \"e2\" ⋈ \"e3\"
   ⟨ \"e0\" \"e1\" \"e2\" \"e3\" ⟩

# or use a list of functions
6 (+ <⊸∾ - <⊸∾ × ⋈ ÷) 3
   ⟨ 9 3 18 2 ⟩

## ⋈ vs ≍ (Couple)
## the crucial difference is ⋈ always returns a list (rank 1) while ≍ always
## returns an array _of at least_ rank 1. Consider:
\"abc\" ⋈ \"def\"
   ⟨ \"abc\" \"def\" ⟩

\"abc\" ≍ \"def\"
   ┌─
   ╵\"abc
     def\"
         ┘"]

      ;; ================================================
      ;; Prefixes
"↑"

["Monad: Prefixes | Dyad: Take | Input: \\r"

 "↑ is a function.
  Its monadic form returns a list of all prefixes of its argument along the
    first axis.
  Its dyadic form returns the first 𝕨 elements of 𝕩.
  Note: Prefix is defined as (↕1+≠)↑¨<
        (Take) when 𝕩 is an atom, or array of any rank, the result will be an array.
        when 𝕨 is negative, elements are returned from the end rather than the
          beginning of the array.
        if 𝕨 ≥ ≠𝕩, then fills are added to the result.
        𝕨 may also have many numbers, corresponding to the leading axes of 𝕩.
        𝕨 can be longer than the rank of 𝕩, in such a case 𝕩 is extended to fit.
        See related form, ↓ (Drop)."


 "Examples:

## Monadic form
↑ \"hello\"              # notice the empty array and input is in the result
   ⟨ ⟨⟩ \"h\" \"he\" \"hel\" \"hell\" \"hello\" ⟩

## return the prefix of a range
↑ 1+↕6
   ⟨ ⟨⟩ ⟨ 1 ⟩ ⟨ 1 2 ⟩ ⟨ 1 2 3 ⟩ ⟨ 1 2 3 4 ⟩ ⟨ 1 2 3 4 5 ⟩ ⟨ 1 2 3 4 5 6 ⟩ ⟩

## with ↓, we can get all list slices along the first axis by taking suffix of
## each prefix.
↓¨↑ \"abc\"
   ┌─
   · ⟨ ⟨⟩ ⟩ ⟨ \"a\" ⟨⟩ ⟩ ⟨ \"ab\" \"b\" ⟨⟩ ⟩ ⟨ \"abc\" \"bc\" \"c\" ⟨⟩ ⟩
                                                             ┘

## Dyadic form
4 ↑ \"take and drop\"
   \"take\"

1 ↑ >\"maj\"‿\"orc\"‿\"ell\"
   ┌─
   ╵\"maj\"
         ┘

10 ↑ ↕5
   ⟨ 0 1 2 3 4 0 0 0 0 0 ⟩

¯2 ↑ ↕5
   ⟨ 3 4 ⟩

## Multiple axes
⊢ m ← (10×↕5) +⌜ ↕7
   ┌─
   ╵  0  1  2  3  4  5  6
     10 11 12 13 14 15 16
     20 21 22 23 24 25 26
     30 31 32 33 34 35 36
     40 41 42 43 44 45 46
                          ┘

¯4‿2 ↑ m  # Take the last four rows; first two columns
   ┌─
   ╵ 10 11
     20 21
     30 31
     40 41
           ┘

## when =𝕩 < ≠𝕨, length-1 axes are added to the beginning to fit.
## In this case, the return is 𝕩 with a lot of fills.
3‿4 ↑ <1‿1
   ┌─
   ╵ ⟨ 1 1 ⟩ ⟨ 0 0 ⟩ ⟨ 0 0 ⟩ ⟨ 0 0 ⟩
     ⟨ 0 0 ⟩ ⟨ 0 0 ⟩ ⟨ 0 0 ⟩ ⟨ 0 0 ⟩
     ⟨ 0 0 ⟩ ⟨ 0 0 ⟩ ⟨ 0 0 ⟩ ⟨ 0 0 ⟩
                                     ┘

## when that isn't the case, here = 3‿¯12 ≡ 1, and ≠m ≡ 5, the result has shape
## |𝕨 and trailing axes from 𝕩
3‿¯12 ↑ m
   ┌─
   ╵ 0 0 0 0 0  0  1  2  3  4  5  6
     0 0 0 0 0 10 11 12 13 14 15 16
     0 0 0 0 0 20 21 22 23 24 25 26
                                    ┘"]

      ;; ================================================
      ;; Suffixes
"↓"

["Monad: Suffixes | Dyad: Drop | Input: \\c"

 "↓ is a function.
  Its monadic form returns a list of all suffixes of its argument along the
    first axis.
  Its dyadic form drops the first 𝕨 elements of 𝕩 and returns the rest.
  Note: Suffix is defined as (↕1+≠)↓¨<
        (Drop) when 𝕩 is an atom, or array of any rank, the result will be an array.
        when 𝕨 is negative, elements are dropped from the end rather than the
          beginning of the array.
        if 𝕨 ≥ ≠𝕩, then the result is empty.
        𝕨 may also have many numbers, corresponding to the leading axes of 𝕩.
        See related form, ↑ (Take)."


 "Examples:

## Monadic form
↓ \"hello\"                # notice the empty array and input is in the result
   ⟨ \"hello\" \"ello\" \"llo\" \"lo\" \"o\" ⟨⟩ ⟩

## return the suffix of a range
↓ 1+↕6
   ⟨ ⟨ 1 2 3 4 5 6 ⟩ ⟨ 2 3 4 5 6 ⟩ ⟨ 3 4 5 6 ⟩ ⟨ 4 5 6 ⟩ ⟨ 5 6 ⟩ ⟨ 6 ⟩ ⟨⟩ ⟩

## with ↓, we can get all list slices along the first axis by taking suffix of
## each prefix.
↓¨↑ \"abc\"
   ┌─
   · ⟨ ⟨⟩ ⟩ ⟨ \"a\" ⟨⟩ ⟩ ⟨ \"ab\" \"b\" ⟨⟩ ⟩ ⟨ \"abc\" \"bc\" \"c\" ⟨⟩ ⟩
                                                             ┘


## Dyadic form
4 ↓ \"take and drop\"
   \" and drop\"

1 ↓ >\"maj\"‿\"orc\"‿\"ell\"
   ┌─
   ╵\"orc
     ell\"
         ┘

10 ↓ ↕5
   ⟨⟩

¯2 ↓ ↕5
   ⟨ 0 1 2 ⟩

## Multiple axes
⊢ m ← (10×↕5) +⌜ ↕7
   ┌─
   ╵  0  1  2  3  4  5  6
     10 11 12 13 14 15 16
     20 21 22 23 24 25 26
     30 31 32 33 34 35 36
     40 41 42 43 44 45 46
                          ┘

¯4‿2 ↓ m  # Drop the last four rows; first two columns
   ┌─
   ╵ 2 3 4 5 6
               ┘

## when =𝕩 < ≠𝕨, if 𝕨 is a list of zeros, ↓ will do nothing but extend the rank
## of 𝕩
≢ (3⥊0) ↓ 3         # the pattern (r⥊0)↓a, ensures array a with rank at least r
   ⟨ 1 1 1 ⟩

≢ (3⥊0) ↓ ↕3
   ⟨ 1 1 3 ⟩

≢ (3⥊0) ↓ ↕5‿4‿3‿2
   ⟨ 5 4 3 2 ⟩"]

      ;; ================================================
      ;; Range
"↕"

["Monad: Range | Dyad: Windows | Input: \\d"

 "↕ is a function.
  Its monadic form returns an array where each element's value is its own index.
  Its dyadic form returns ≠𝕩 contiguous slices of 𝕩 that are of length 𝕨.
  Note: (Range) the result always has depth (≡) one more than the argument.
        (Window) 𝕨 must be between 0 and 1+≠𝕩
        (Window) slices always have the same rank as the argument array (𝕩)"


 "Examples:

## Monadic form, all results are length 6, but elements differ
## 𝕩 must be a natural number, notice the result is ≠𝕩, but 𝕩 is not in the result
↕6
   ⟨ 0 1 2 3 4 5 ⟩

(↕6) ⊏ \"select\"
   \"select\"

(↕⟨6⟩) ⊑ \" pick \"
   \" pick \"

## when 𝕩 is a list of numbers, the result is an array of lists
## this can also be read as all possible numbers of a mixed based number system
## in this case, three digit numbers, the lowest digit is base 4
## the next in base 3, and the highest in base 2
↕ 2‿3‿4
   ┌─
   ╎ ⟨ 0 0 0 ⟩ ⟨ 0 0 1 ⟩ ⟨ 0 0 2 ⟩ ⟨ 0 0 3 ⟩
     ⟨ 0 1 0 ⟩ ⟨ 0 1 1 ⟩ ⟨ 0 1 2 ⟩ ⟨ 0 1 3 ⟩
     ⟨ 0 2 0 ⟩ ⟨ 0 2 1 ⟩ ⟨ 0 2 2 ⟩ ⟨ 0 2 3 ⟩

     ⟨ 1 0 0 ⟩ ⟨ 1 0 1 ⟩ ⟨ 1 0 2 ⟩ ⟨ 1 0 3 ⟩
     ⟨ 1 1 0 ⟩ ⟨ 1 1 1 ⟩ ⟨ 1 1 2 ⟩ ⟨ 1 1 3 ⟩
     ⟨ 1 2 0 ⟩ ⟨ 1 2 1 ⟩ ⟨ 1 2 2 ⟩ ⟨ 1 2 3 ⟩
                                             ┘

## ↕≠a returns the indices of the major cells of a
a ← 4‿2⥊@
↕≠a
   ⟨ 0 1 2 3 ⟩

## ↕≢a returns the indices of all elements
↕≢a
   ┌─
   ╵ ⟨ 0 0 ⟩ ⟨ 0 1 ⟩
     ⟨ 1 0 ⟩ ⟨ 1 1 ⟩
     ⟨ 2 0 ⟩ ⟨ 2 1 ⟩
     ⟨ 3 0 ⟩ ⟨ 3 1 ⟩
                     ┘

## get the first b numbers, starting at a, with a+↕b
5+↕4
   ⟨ 5 6 7 8 ⟩

## or get the first b natural numbers, with a↓↕b (swap Plus for Take)
2↓↕4
   ⟨ 2 3 ⟩

## add a character to create a range of characters
'a'+↕26
   \"abcdefghijklmnopqrstuvwxyz\"

## combine with ↑ to pad with 0's, or combine with »⍟ to pad with empty spaces
4↑↕3
   ⟨ 0 1 2 0 ⟩

»⍟3 'b'+↕8
   \"   bcdef\"

## use ↕ to find, at each position in a Boolean list, the most recent index
## that has a 1
⊢ b ← 0‿1‿1‿0‿0‿0‿1‿0
   ⟨ 0 1 1 0 0 0 1 0 ⟩       # a Boolean list

b ≍ ↕≠b                      # get indices of b (↕≠b) and Couple with b
   ┌─
   ╵ 0 1 1 0 0 0 1 0
     0 1 2 3 4 5 6 7
                     ┘

b × ↕≠b                      # now multiply with b
   ⟨ 0 1 2 0 0 0 6 0 ⟩

# now the index of the most recent 1 is given by the maximum of the previous
# elements, which is the scan: ⌈`
⌈` b × ↕≠b
   ⟨ 0 1 2 2 2 2 6 6 ⟩       # the last 1 in the input list was at index 6
                            # similarly the closest, previous 1 was at index 2
                            # until it was at index 6, similarly for index 1

# use ↕ with ⥊ to get fixed base numbers, for example all 3 digit binary numbers
↕ 3⥊2
   ┌─
   ╎ ⟨ 0 0 0 ⟩ ⟨ 0 0 1 ⟩
     ⟨ 0 1 0 ⟩ ⟨ 0 1 1 ⟩

     ⟨ 1 0 0 ⟩ ⟨ 1 0 1 ⟩
     ⟨ 1 1 0 ⟩ ⟨ 1 1 1 ⟩
                         ┘


## Dyadic form
5↕\"abcdefg\"                # get contiguous slices of 𝕩 with length 𝕨
   ┌─
   ╵\"abcde
     bcdef
     cdefg\"
           ┘

## Get 2 row slices of a shape 3‿4 array
<⎉2 2↕\"0123\"∾\"abcd\"≍\"ABCD\"
   ┌─
   · ┌─       ┌─
     ╵\"0123   ╵\"abcd
       abcd\"    ABCD\"
            ┘        ┘
                       ┘

## when 𝕨 has length 0, 𝕩 is not sliced along any dimension
⟨⟩↕\"abc\"
   \"abc\"

## Use window with reductions to get windowed reductions.
## For example sums of 3 values
+˝˘3↕ ⟨2,6,0,1,4,3⟩
   ⟨ 8 7 5 8 ⟩

## Add two zeros to keep the length constant
(+˝≠↕(2⥊0)⊸∾) ⟨2,6,0,1,4,3⟩
   ⟨ 2 8 8 7 5 8 ⟩"]

      ;; ================================================
      ;; Nudge
"»"

["Monad: Nudge | Dyad: Shift Before | Input: \\L"

 "» is a function.
  Its monadic form returns its input where each element has shifted one major
    cell to the right, and the new cell is filled with 0s or \" \".
  Its dyadic form adds 𝕨 to the beginning of 𝕩, while maintiaing the length of 𝕩.
  Note: 𝕩 must have rank 1 or more.
        𝕨 can have rank equal to or less than the rank of 𝕩.
        𝕨 must be join compatible, i.e., 𝕨∾𝕩 must not error.
        (Nudge) default argument is a cell of fills: 1↑0↑𝕩
        Nudge        is defined as (1↑0↑⊢)⊸»
        Shift Before is defined as {(≠𝕩)↑𝕨∾𝕩}
        See related form, « (Nudge Back/Shift After)"

 "Examples:

## Monadic form
» \"abc\"
   \" ab\"    # notice that the length of the result equals the length of 𝕩

»» 1‿2‿3
   ⟨ 0 0 1 ⟩

»»» \"abc\"
   \"   \"

## higher rank, Shift Before adds a major cell (row) of fills
⊢ a ← ⥊⟜(↕×´) 4‿3
   ┌─
   ╵ 0  1  2
     3  4  5
     6  7  8
     9 10 11
             ┘

» a
   ┌─
   ╵ 0 0 0    # new major cell of fills
     0 1 2
     3 4 5
     6 7 8
           ┘


## Dyadic form, » and « are useful for sequence processing
s ← 1‿2‿2‿4‿3‿5‿6
   ⟨ 1 2 2 4 3 5 6 ⟩

## join s with »s
s ≍ »s
   ┌─
   ╵ 1 2 2 4 3 5 6
     0 1 2 2 4 3 5
                   ┘

## now compare each element with the previous with -⟜»
-⟜»s
   ⟨ 1 1 0 2 ¯1 2 1 ⟩

## this is equivalent to inverse of plus scan (+)
 +` -⟜» s
   ⟨ 1 2 2 4 3 5 6 ⟩

## we can use 𝕨 to fill s instead of the default array's fill
∞ » s
   ⟨ ∞ 1 2 2 4 3 5 ⟩

## when a number is in big-endian form, a right shift might be logical, shifting
## in zeros (the most significant bit). For little endian, this applies to left
## shifts «
⊢ i ← \"10011011\"-'0'
   ⟨ 1 0 0 1 1 0 1 1 ⟩

1 ⥊⟜0⊸» i    # Logical right shift, notice the most sig. bit becomes a 0
   ⟨ 0 1 0 0 1 1 0 1 ⟩

1 (⥊⟜⊏»⊢) i  # Arithmetic right shift, notice the most sig. bit remains a 1
   ⟨ 1 1 0 0 1 1 0 1 ⟩

## we can also shift by more than 1
3 ⥊⟜0⊸» i    # Logical right shift by 3
   ⟨ 0 0 0 1 0 0 1 1 ⟩

3 (⥊⟜⊏»⊢) i  # Arithmetic right shift by 3
   ⟨ 1 1 1 1 0 0 1 1 ⟩

## higher rank dyadic shifts
⊢ a ← ⥊⟜(↕×´) 4‿3
   ┌─
   ╵ 0  1  2
     3  4  5
     6  7  8
     9 10 11
             ┘
\"one\" » a            # Shift in a cell
   ┌─
   ╵ 'o' 'n' 'e'
     0   1   2
     3   4   5
     6   7   8
                 ┘

(\"two\"≍\"cel\") » a  # Shift in multiple cells
   ┌─
   ╵ 't' 'w' 'o'
     'c' 'e' 'l'
     0   1   2
     3   4   5
                 ┘"]

      ;; ================================================
      ;; Nudge Back
"«"

["Monad: Nudge Back | Dyad: Shift After | Input: \\H"

 "« is a function.
  Its monadic form returns its input where each element has shifted one major
    cell to the left, and the new cell is filled with 0s or \" \".
  Its dyadic form adds 𝕨 to the end of 𝕩, while maintaining the length of 𝕩.
  Note: 𝕩 must have rank 1 or more.
        𝕨 can have rank equal to or less than the rank of 𝕩.
        𝕨 must be join compatible, i.e., 𝕨∾𝕩 must not error.
        (Nudge Back) default argument is a cell of fills: 1↑0↑𝕩
        Nudge Back  is defined as (1↑0↑⊢)⊸«
        Shift After is defined as {(-≠𝕩)↑𝕩∾𝕨}
        See related form, » (Nudge/Shift Before)"

 "Examples:

## Monadic form
« \"abc\"
   \"bc \"    # notice that the length of the result equals the length of 𝕩

«« 1‿2‿3
   ⟨ 3 0 0 ⟩

««« \"abc\"
   \"   \"

## higher rank, Shift After adds a major cell (row) of fills to the end
⊢ a ← ⥊⟜(↕×´) 4‿3
   ┌─
   ╵ 0  1  2
     3  4  5
     6  7  8
     9 10 11
             ┘

« a
   ┌─
   ╵ 3  4  5
     6  7  8
     9 10 11
     0  0  0    # new major cell of fills
             ┘


## Dyadic form, » and « are useful for sequence processing
## in this example we get a difference between pairs of elements
s ← 1‿2‿2‿4‿3‿5‿6
   ⟨ 1 2 2 4 3 5 6 ⟩

## join s with «s
s ≍ «s
   ┌─
   ╵ 1 2 2 4 3 5 6
     2 2 4 3 5 6 0
                   ┘

## now compare each element with the previous with -⟜»
«⊸- s
   ⟨ 1 0 2 ¯1 2 1 ¯6 ⟩     # notice 2-1=1, 2-2=0, 4-2=2

## we can also get a symmetric difference, i.e., subtracting the previous element
## from the next and dividing by two.
2÷˜ (»-«) s
   ⟨ ¯1 ¯0.5 ¯1 ¯0.5 ¯0.5 ¯1.5 2.5 ⟩

# Repeat at the ends instead of using fills, notice the length never changes
2÷˜ (⊣˝⊸» - ⊢˝⊸«) s
   ⟨ ¯0.5 ¯0.5 ¯1 ¯0.5 ¯0.5 ¯1.5 ¯0.5 ⟩

## when a number is in big-endian form, a right shift might be logical, shifting
## in zeros (the most significant bit). For little endian, this applies to left
## shifts «
⊢ i ← \"10011011\"-'0'
   ⟨ 1 0 0 1 1 0 1 1 ⟩

1 ⥊⟜0⊸« i    # Logical left shift, notice the least sig. bit becomes a 0
   ⟨ 0 0 1 1 0 1 1 0 ⟩

1 (⥊⟜⊏«⊢) i  # Arithmetic left shift, notice the least sig. bit remains a 1
   ⟨ 0 0 1 1 0 1 1 1 ⟩

## we can also shift by more than 1
3 ⥊⟜0⊸« i    # 3 Logical left shifts
   ⟨ 1 1 0 1 1 0 0 0 ⟩

«⍟3 i         # An alternative left shift form, 3 left shifts in this case
   ⟨ 1 1 0 1 1 0 0 0 ⟩

3 (⥊⟜⊏«⊢) i  # 3 Arithmetic left shifts
   ⟨ 1 1 0 1 1 1 1 1 ⟩

## higher rank dyadic shifts
⊢ a ← ⥊⟜(↕×´) 4‿3
   ┌─
   ╵ 0  1  2
     3  4  5
     6  7  8
     9 10 11
             ┘
\"one\" « a            # Shift in a cell to the back
   ┌─
   ╵ 3   4   5
     6   7   8
     9   10  11
     'o' 'n' 'e'
                 ┘

(\"two\"≍\"cel\") « a  # Shift in multiple cells
   ┌─
   ╵ 6   7   8
     9   10  11
     't' 'w' 'o'
     'c' 'e' 'l'
                 ┘"]

      ;; ================================================
      ;; Reverse
"⌽"

["Monad: Reverse | Dyad: Rotate | Input: \\q"

 "⌽ is a function.
  Its monadic form returns an array whose major cells are reverse from the input.
  Its dyadic form cycles or rotates the major cells in 𝕩, according to 𝕨.
  Note: Both Reverse and Rotate return an array with the same shape and elements
          as 𝕩.
        Avoid Rotate if there is no reason to treat data in 𝕩 as cyclic or
          periodic."

 "Examples:

## Monadic form
⌽ \"abcdefg\"
   \"gfedcba\"

⌽ >\"ab\"‿\"cd\"‿\"ef\"
   ┌─
   ╵\"ef
     cd
     ab\"
        ┘

## atoms or rank-0 arrays have no axes to reverse along, or no ordering
## thus an error
⌽ 'c'
   Error: ⌽: Argument cannot be a unit
at ⌽ 'c'
   ^

## to Reverse along an axis other than the first axis use ˘ (Cells) or ⎉ (Rank)
⌽˘ >\"ab\"‿\"cd\"‿\"ef\"
   ┌─
   ╵\"ba
     dc
     fe\"
        ┘

2‿4⥊↕8
   ┌─
   ╵ 0 1 2 3
     4 5 6 7
             ┘

⌽⎉ 2‿4⥊↕8
   ⟨ 7 6 5 4 3 2 1 0 ⟩

## Reverse is useful for folding from the left rather than the right
⋈´   \"abcd\"  # Right to left
   ⟨ 'a' ⟨ 'b' \"cd\" ⟩ ⟩

⋈˜´ ⌽ \"abcd\"  # Left to right
   ⟨ ⟨ \"ab\" 'c' ⟩ 'd' ⟩

## Similarly for ` (Scan)
∨`   0‿0‿1‿0‿0‿1‿0
   ⟨ 0 0 1 1 1 1 1 ⟩    # change all bits after first 1 to 1s

∨`⌾⌽ 0‿0‿1‿0‿0‿1‿0
   ⟨ 1 1 1 1 1 1 0 ⟩    # change all bits before the last 1 in the bitstring to 1s


## Dyadic form, for single axis 𝕨 must be an number, 𝕩 must be an array with
## at least one axis.
2 ⌽ \"rotate\"
   \"tatero\"

## rotation with a negative 𝕨, rotates from right to left
¯2 ⌽ \"rotate\"
   \"terota\"

2 (⊢ ⋈ ⌽) 5‿2⥊\"rotateCELL\"
   ┌─
   · ┌─     ┌─
     ╵\"ro  ╵\"te
       ta     CE
       te     LL
       CE     ro
       LL\"    ta\"
          ┘       ┘
                   ┘

2 ⌽ 'c'  # No axes to rotate
   Error: ⌽: 𝕩 must have rank at least 1 for atom 𝕨
at 2 ⌽ 'c'  # No axes to rotate

## by default elements are rotated to the left, so entry i of the result is entry
## 𝕨+i of the argument.
2 ⌽ ↕6
   ⟨ 2 3 4 5 0 1 ⟩

## multiple axes
⊢ tab ← 3‿4⥊\"abcdABCD0123\"
   ┌─
   ╵\"abcd
     ABCD
     0123\"
          ┘

1 ⌽˘ tab        # Rotate the second axis
   ┌─
   ╵\"bcda
     BCDA
     1230\"
          ┘

## 𝕨 can be a list or unit array of integers, which are matched with the leading
## axes of 𝕩. This means that 𝕨 cannot be larger than rank of 𝕩.
3‿4‿2 ⌽ \"just a list\"

   Error: 𝕨⌽𝕩: Length of compound 𝕨 must be at most rank of 𝕩
at 3‿4‿2 ⌽ \"just a list\"
         ^

## rotate the first (vertical) axis of tab by 1, then the second axis by 2
## so the capitalized row rotates two positions up to the top, and the column
## with 2 rotates from horizontal index 2, to index 0
1‿2 ⌽ tab
   ┌─
   ╵\"CDAB
     2301
     cdab\"
          ┘

## the rotations are independent, thus this is equivalent to a sequence of ⌽s
## and a ˘
1 ⌽ 2 ⌽˘ tab     # Note: Rotate in this case should be preferred as it can be
   ┌─                    evaluated more quickly than multiple independent rotations
   ╵\"CDAB
     2301
     cdab\"
          ┘"]

      ;; ================================================
      ;; Transpose
"⍉"

["Monad: Transpose | Dyad: Reorder axes | Input: \\a"

 "⍉ is a function.
  Its monadic form returns an array whose first axis has been moved to the end.
  Its dyadic form generalizes the monadic form for arbritrary arrangement of 𝕩,
    according to 𝕨.
  Note: ≢⍉⍟k a ←→ k⌽≢a for any whole number k, and any array a
        Transpose ⍉ is equivalent to Reorder axes, with a default 𝕨: (=-1˙)⊸⍉
        (Reorder axes) 𝕨 is a number or numeric array of rank 1 or less
                       the result rank, r, is equal to  r←(=𝕩)-+´¬∊𝕨.
                       Invariant: ∧´𝕨<r
        see related function, ⌽ (Rotate)"

 "Examples:

## Monadic form
## mat is a 2‿3 matrix
⊢ mat ← 2‿3 ⥊ ↕6
   ┌─
   ╵ 0 1 2
     3 4 5
           ┘

## we transpose it to a 3‿2 matrix
⍉ mat
   ┌─
   ╵ 0 3
     1 4
     2 5
         ┘

## transpose a rank 3 matrix
a322 ← 3‿2‿2⥊↕12
   ┌─
   ╎  0  1
      2  3

      4  5
      6  7

      8  9
     10 11
           ┘

⋈⟜⍉ a322
   ┌─                      
   · ┌─        ┌─
     ╎  0  1   ╎ 0 4  8
        2  3     1 5  9

        4  5     2 6 10
        6  7     3 7 11
                        ┘
        8  9
       10 11
             ┘
                          ┘

## monadic ⍉ takes the first axis and moves it to the end, see how 2 changes here
≢ a23456 ← ↕2‿3‿4‿5‿6
   ⟨ 2 3 4 5 6 ⟩

≢ ⍉ a23456
   ⟨ 3 4 5 6 2 ⟩

## to exchange multiple axes, use ⍟ (Repeat); notice we've moved 3 axes here
≢ ⍉⍟3 a23456
   ⟨ 5 6 2 3 4 ⟩

## use a negative number to move axis in the other direction, similar to ⌽
≢ ⍉⍟¯3 a23456
   ⟨ 4 5 6 2 3 ⟩

## to move the last axis to the front, use ⁼ (Undo)
≢ ⍉⁼ a23456
   ⟨ 6 2 3 4 5 ⟩

## to move axes other than the first, use the rank modifier to leave inital axes
## untouched. Here k ≡ 3, a k>0 transposes only the last k axes, k<0 ignores the
## first |k axes.
≢ ⍉⎉3 a23456
   ⟨ 2 3 5 6 4 ⟩         # notice 2 (1st axis) and 3 (2nd axis) are untouched

## finally, combine Rank and Repeat for more compilcated transpositions
## such as move a set of contiguous axes with any starting point and length to
## the end
≢ ⍉⁼⎉¯1 a23456
   ⟨ 2 6 3 4 5 ⟩

## Dyadic form, 𝕨 specifies a permutation over 𝕩's axes. For each index p←i⊑𝕨
## in 𝕨, axis i of 𝕩 is used for axis p of the result. Mutliple argument axes
## can be sent to the same result axis, in this case the axis goes along the
## diagonal of 𝕩, and the result will have a lower rank than 𝕩.
≢ 1‿3‿2‿0‿4 ⍉ a23456
   ⟨ 5 2 4 3 6 ⟩

## often times it is easier to use ⍉⁼ when specifying all axes.
## Note that if we have p≡○≠≢a, then ≢p⍉⁼a ←→ p⊏≢a
≢ 1‿3‿2‿0‿4 ⍉⁼ a23456
   ⟨ 3 5 4 2 6 ⟩

## when only some axes are specified in 𝕨, 𝕨 will be matched up to the leading
## axes of 𝕩. The matched axes are moved according to 𝕨, the unmatched maxes
## are moved to fill the gaps between the moved axes
≢ 0‿2‿4 ⍉ a23456
   ⟨ 2 5 3 6 4 ⟩

≢ 2 ⍉ a23456  # Restrict Transpose to the first three axes
   ⟨ 3 4 2 5 6 ⟩"]

      ;; ================================================
      ;; Indices
"/"

["Monad: Indices | Dyad: Replicate | Input: \/"

 "/ is a function.
  Its monadic form returns a list of natural numbers that are the indices of 𝕩.
  Its dyadic form repeats each major cell of 𝕩, the corresponding 𝕨 times.
  Note: (Replicate) Invariant: (𝕨≠) ≡ (𝕩≠)
                    Invariant: (≠𝕨) ≤  =𝕩
                    result inludes i⊑𝕨 copies of each cell i⊏𝕩, in order.
                    when 𝕨 has rank 1, (𝕨/𝕩) ≡ (𝕨/⊸⊏𝕩)

        (Indices) 𝕩 must be a list of natural numbers, then /𝕩 is 𝕩/↕≠𝕩"

 "Examples:

## Monadic form
/ 3‿0‿1‿2
   ⟨ 0 0 0 2 3 3 ⟩

## combine with ⊔ to group 𝕩 according to a list of lengths 𝕨.
## use (/∾⟜1)⊸⊔ to include trailing empty arrays
2‿5‿0‿1 /⊸⊔ \"ABCDEFGH\"
   ⟨ \"AB\" \"CDEFG\" ⟨⟩ \"H\" ⟩

## when 𝕩 is boolean, /𝕩 contains all indices where a 1 appears in 𝕩
/ 0‿1‿0‿1‿0‿0‿0‿0‿1‿0
   ⟨ 1 3 8 ⟩

## use -⟜» to get the distance from each 1 to the previous or to the start of
## the list, notice the first 1 has a distance of 1 (1 element from beginning)
-⟜» / 0‿1‿0‿1‿0‿0‿0‿0‿1‿0
   ⟨ 1 2 5 ⟩

## we can use / to analyze groups of 1s (or 0s via flipping values with ¬)
## first highlight the start and end of each group by comparing with a shifted copy
## To do this we place a 0 at the front and at the end of the group to detech the shift
0 (∾≍∾˜) 0‿1‿1‿1‿0‿0‿1‿0‿1‿1‿0
   ┌─
   ╵ 0 0 1 1 1 0 0 1 0 1 1 0
     0 1 1 1 0 0 1 0 1 1 0 0
                             ┘

## notice the 1s here now correspond to each group's boundaries
## note you can also do this with a shift: ≠⟜«0∾𝕩
0 (∾≠∾˜) 0‿1‿1‿1‿0‿0‿1‿0‿1‿1‿0
   ⟨ 0 1 0 0 1 0 1 1 1 0 1 0 ⟩

## now get the Indices of the transition points
/ 0(∾≠∾˜) 0‿1‿1‿1‿0‿0‿1‿0‿1‿1‿0
   ⟨ 1 4 6 7 8 10 ⟩

## we know the first transition must be a 0 to 1, then the next 1 to 0 and so on
## thus the transitions come in pairs, so we can Reshape with ∘‿2 groups of
## these pairs, and then scan -˜`˘ to convert the start/end format to start/length
-˜`˘ ∘‿2⥊/ 0(∾≠∾˜) 0‿1‿1‿1‿0‿0‿1‿0‿1‿1‿0
   ┌─
   ╵ 1 3
     6 1
     8 2
         ┘

## Indices returns a list of natural numbers, where the number i appears i⊑𝕩 times
## Given a list of k numbers, the inverse of indices returns a corresponding 𝕩.
## one where the value i⊑𝕩 is the number of times i appears in k.
/ 3‿2‿1
   ⟨ 0 0 0 1 1 2 ⟩

/⁼ 0‿0‿0‿1‿1‿2
   ⟨ 3 2 1 ⟩

## there are several ways to find how many times each index appears in a list
## of indices
+˝˘ (↕5) =⌜ 2‿2‿4‿1‿2‿0  # Inefficient
   ⟨ 1 1 3 0 1 ⟩

≠¨⊔ 2‿2‿4‿1‿2‿0
   ⟨ 1 1 3 0 1 ⟩

/⁼∧ 2‿2‿4‿1‿2‿0          # note that for /⁼ to work 𝕩 must be sorted, hence ∧
   ⟨ 1 1 3 0 1 ⟩          # this is also typically faster than ≠¨⊔


## Dyadic form
2‿1‿0‿2 / \"abcd\"
   \"aabdd\"

⊢ a ← >\"aa0\"‿\"bb1\"‿\"cc2\"‿\"dd3\"
   ┌─
   ╵\"aa0
     bb1
     cc2
     dd3\"
         ┘

2‿1‿0‿2 / a
   ┌─
   ╵\"aa0
     aa0
     bb1
     dd3
     dd3\"
         ┘

3 / \"copy\"
   \"cccooopppyyy\"

## if 𝕨 is a list of booleans, then we have a filter
1‿1‿0‿0‿1‿0 / \"filter\"
   \"fie\"

## similarly we can filter by any function which returns a Boolean with the
## pattern Fn¨⊸/
≤⟜'i' \"filter\"        # Fn, ≤⟜'i' is pervasive so we don't need ¨ (Each)
   ⟨ 1 1 0 0 1 0 ⟩       # similarly use Fn˘⊸/ to filter each major cell

≤⟜'i'⊸/ \"filter\"
   \"fie\"

## when 𝕨 has depth 2, then its elements give the amounts to copy along each
## leading axis of 𝕩
⊢ b ← 2‿5 ⥊ ↕10
   ┌─
   ╵ 0 1 2 3 4
     5 6 7 8 9
               ┘

⟨2‿0, 1‿0‿0‿1‿1⟩ / b       # 2‿0 indicates to copy the first row twice
   ┌─                     # then elements from the row are selected via 1‿0‿0‿1‿1
   ╵ 0 3 4
     0 3 4
           ┘

2‿0 / 1‿0‿0‿1‿1⊸/˘ b
   ┌─
   ╵ 0 3 4
     0 3 4
           ┘

## each element has to have the same length as the correspond axis, or is a unit
⟨<2,<3⟩ / b               # notice that both 2 and 3 are enclosed
   ┌─
   ╵ 0 0 0 1 1 1 2 2 2 3 3 3 4 4 4
     0 0 0 1 1 1 2 2 2 3 3 3 4 4 4
     5 5 5 6 6 6 7 7 7 8 8 8 9 9 9
     5 5 5 6 6 6 7 7 7 8 8 8 9 9 9
                                   ┘

## if none of the elements in 𝕨 are enclosed, then ≡𝕨 is 1, and will be
## interpreted as relicating along the first axis only
⟨2,3⟩ / b                # notice (⟨<2,<3⟩ / b) ≢ (⟨2,3⟩ / b)
   ┌─
   ╵ 0 1 2 3 4
     0 1 2 3 4
     5 6 7 8 9
     5 6 7 8 9
     5 6 7 8 9
               ┘

## when 𝕨 is ⟨⟩ we have the base case b ≡ ⟨⟩ / b
b ≡ ⟨⟩ / b
   1"]

      ;; ================================================
      ;; Grade Up
"⍋"

["Monad: Grade Up | Dyad: Bins Up | Input: \\T"

 "⍋ is a function.
  Its monadic form returns a list of natural numbers that are an ascending ording
    of the input.
  Its dyadic form returns a list of natural numbers, where each number indicates
    the rank of 𝕨 that the corresponding element in 𝕩 is ≥ than.
  Note: (Bins Up) Invariant: 𝕨 is already sorted according to some ordering.
                  Result is always in ascending sorted order.
        see related function, ⍒ (Grade Down/Bins Down)"

 "Examples:

## Monadic form
⊢ l ← \"planet\"‿\"moon\"‿\"star\"‿\"asteroid\"
   ⟨ \"planet\" \"moon\" \"star\" \"asteroid\" ⟩

∧ l                           # sort alphabetically
   ⟨ \"asteroid\" \"moon\" \"planet\" \"star\" ⟩

⍋ l                           # ⍋ returns the indices of the elements of 𝕩
   ⟨ 3 1 0 2 ⟩                 # in ascending sorted order

## thus
(⍋l) ⊏ l
   ⟨ \"asteroid\" \"moon\" \"planet\" \"star\" ⟩  # sorted

## and
((⍋l) ⊏l) ≡ ∧l
   1


## Dyadic form
5‿6‿2‿4‿1 ⍋ 3                  # notice 𝕨 is not strictly sorted due to 6‿2‿4
   Error: ⍋: 𝕨 must be sorted
at 5‿6‿2‿4‿1 ⍋ 3
             ^

scores ← 3‿5‿17‿11‿23          # notice this is not sorted
   ⟨ 3 5 17 11 23 ⟩

other_scores ← 5‿6‿23          # notice this is sorted due to 𝕨 sorted invariant
   ⟨ 5 6 23 ⟩

# Notice that 3 (which is at index 0 in 𝕩) is ≤ 5 (which is at rank 1 in 𝕨),
# hence if we were to insert 3 into 𝕨, and preserve 𝕨's ordering we would insert
# 3 at index 0. Thus at index 0 in the result we return a 0. Similarly, 5≥5 but
# 5<6, so we get a 1 in the result at 5's position since every element in 𝕨 at
# rank > 1 is ≥ 5 and if we were to insert 5 and preserve ordering we would do
# so at index 1
other_scores ⍋ scores
   ⟨ 0 1 2 2 3 ⟩"]

      ;; ================================================
      ;; Grade Down
"⍒"

["Monad: Grade Down | Dyad: Bins Down | Input: \\V"

 "⍒ is a function.
  Its monadic form returns a list of natural numbers that are a descending ording
    of the input.
  Its dyadic form returns a list of natural numbers, where each number indicates
    the rank of 𝕨 that the corresponding element in 𝕩 is ≤ than.
  Note: (Bins Up) Invariant: 𝕨 is already sorted according to some ordering.
                  Result is always in descending sorted order.
        see related function, ⍒ (Grade Down/Bins Down)"

 "Examples:

## Monadic form
⊢ l ← \"planet\"‿\"moon\"‿\"star\"‿\"asteroid\"
   ⟨ \"planet\" \"moon\" \"star\" \"asteroid\" ⟩

∨ l                           # sort alphabetically
   ⟨ \"star\" \"planet\" \"moon\" \"asteroid\" ⟩

⍒ l                           # ⍒ returns the indices of the elements of 𝕩
   ⟨ 2 0 1 3 ⟩                 # in descending sorted order

## thus
(⍒l) ⊏ l
   ⟨ \"star\" \"planet\" \"moon\" \"asteroid\" ⟩

## and
((⍒l) ⊏l) ≡ ∨l
   1


## Dyadic form
5‿6‿2‿4‿1 ⍒ 3                  # notice 𝕨 is not strictly sorted due to 6‿2‿4
   Error: ⍒: 𝕨 must be sorted in descending order
at 5‿6‿2‿4‿1 ⍒ 3
             ^

scores ← 3‿5‿17‿11‿23          # notice this is not sorted
   ⟨ 3 5 17 11 23 ⟩

other_scores ← 23‿6‿5          # notice this is sorted due to 𝕨 sorted invariant
   ⟨ 23 6 5 ⟩

# Notice that 3 (which is at index 0 in 𝕩) is ≤ 5 (which is at rank 3 in 𝕨),
# hence if we were to insert 3 into 𝕨, and preserve 𝕨's ordering we would insert
# 3 at index 3. Thus at index 0 in the result we return a 3. Similarly, 5≥5 but
# 5<6, so we get a 3 in the result at 5's position since every element in 𝕨 at
# rank < 3 is ≥ 5 and if we were to insert 5 and preserve ordering we would do
# so at index 3
other_scores ⍒ scores
   ⟨ 3 3 1 1 1 ⟩"]

      ;; ================================================
      ;; First Cell
"⊏"

["Monad: First Cell | Dyad: Select | Input: \\i"

 "⊏ is a function.
  Its monadic form returns the major cell of 𝕩 at index 0.
  Its dyadic form reorganizes 𝕩 along one or more axes according to the indices
    given by 𝕨.
  Note: (First Cell) is defined using Select: 0⊏𝕩
        (Select) 𝕨 must be an integer or array of integers (including empty array)
                 when 𝕨 is an atom, Select returns a major cell whose shape is 1↓≢𝕩
                 indices of 𝕨 must be < ≠𝕩
                 indices of 𝕨 can be negative but must be ≥ -≠𝕩
                 if ≠𝕩 is 0, then no index is valid for selection
                 the shape of the result is equivalent to (≢𝕨)∾1↓≢𝕩
        see related function, ⊐ (Classify)
        see related function, ⊑ (Pick)"


 "Examples:

## Monadic form
⊏ \"abc\"
   ┌·
   ·'a'
       ┘

⊏ \"abc\"≍\"def\"
   \"abc\"

⊏ ≍ \"abc\"
   \"abc\"

⊏ 'a'
   Error: ⊏: Argument cannot be an atom
at ⊏ 'a'
   ^

## Dyadic form
2 ⊏ \"abcdef\"  # An enclosed element
   ┌·
   ·'c'
       ┘

2 ⊑ \"abcdef\"  # Pick (not Select) gets a non-enclosed element
   'c'

¯2 ⊏ \"abcdef\" # negative indices in 𝕨 select from the end of 𝕩
   ┌·
   ·'e'
       ┘

4‿0‿1‿3‿2 ⊏ \"elolh\"  # a list of number returns a result with same rank
   \"hello\"

4‿0‿1‿3 ⊏ \"elolh\"  # but the length of the result may differ from 𝕩
   \"hell\"


⟨⟩ ⊏ \"elolh\"  # an empty 𝕨 returns and empty array
   ⟨⟩

⊢ m ← 3‿5‿7‿11 |⌜ ×˜↕7
   ┌─
   ╵ 0 1 1 0 1 1 0
     0 1 4 4 1 0 1
     0 1 4 2 2 4 1
     0 1 4 9 5 3 3
                   ┘

0‿¯1 ⊏ m
   ┌─
   ╵ 0 1 1 0 1 1 0
     0 1 4 9 5 3 3
                   ┘

## when 𝕩 is a list the result has the same shape as 𝕨, where elements of 𝕨
## are replaced one-by-one with elements of 𝕩
2|m
   ┌─
   ╵ 0 1 1 0 1 1 0
     0 1 0 0 1 0 1
     0 1 0 0 0 0 1
     0 1 0 1 1 1 1
                   ┘

(2|m) ⊏ \" ⋆\"
   ┌─
   ╵\" ⋆⋆ ⋆⋆
      ⋆  ⋆ ⋆
      ⋆    ⋆
      ⋆ ⋆⋆⋆⋆\"
             ┘

## when 𝕨 is a unit, the result shape will be the major cell shape of 𝕩
## remember that the initial axes come from 𝕨 while later ones come from 𝕩
\"awA0\" +⌜ ↕4
   ┌─
   ╵\"abcd
     wxyz
     ABCD
     0123\"
          ┘

2 ↕ ↕4
   ┌─
   ╵ 0 1
     1 2
     2 3
         ┘

(2 ↕ ↕4) ⊏ \"awA0\" +⌜ ↕4
   ┌─
   ╎\"abcd
     wxyz

    ·wxyz
     ABCD

    ·ABCD
     0123\" 
          ┘

## 𝕨 can apply to mulitple axes of 𝕩 simultaneously, only if 𝕨 is a non-empty
## list or array
⟨2‿1, 3‿0‿0⟩ ⊏ ↕3‿4
   ┌─
   ╵ ⟨ 2 3 ⟩ ⟨ 2 0 ⟩ ⟨ 2 0 ⟩
     ⟨ 1 3 ⟩ ⟨ 1 0 ⟩ ⟨ 1 0 ⟩
                             ┘"]

      ;; ================================================
      ;; Pick
"⊑"

["Monad: First | Dyad: Pick | Input: \\I"

 "⊑ is a function.
  Its monadic form returns the first element of 𝕩 in index order.
  Its dyadic form returns elements from 𝕩 based on index lists from 𝕨.
  Note: (First) is Pick where 𝕨 is 0¨≢𝕩
        (Pick) 𝕨 can be a plain list, a single number, array of index lists,
                 or have deeper structure.
               a number in 𝕨 must be an integer, i, where -≠𝕩 < i < ≠𝕩
               using Pick to repeatedly select multiple elements from 𝕩 is likely
                 slower than using ⊏. Prefer ⊏ in this case or rearrange your data.
        see related function, ⊏ (Select)
        see related function, ⊐ (Classify)"

 "Examples:

## Monadic form
⊑ 'a'
   'a'

⊑ \"First\"
   'F'

⊑ ↕4‿2‿5‿1
   ⟨ 0 0 0 0 ⟩

⊑ \"\"
   Error: ⊑: Argument cannot be empty
at ⊑ \"\"
   ^

## Dyadic form
## when 𝕨 is a single number, Pick gets an element from 𝕩
2 ⊑ 0‿1‿2‿3‿4
   2

2 ⊑ \"abc\"
   'c'

2 ⊑ ⟨@, 0‿1‿2‿3, \"abc\"⟩
   \"abc\"

## when 𝕩 is a unit, the only possible value for 𝕨 is ⟨⟩
⟨⟩ ⊑ <'a'
   'a'

⟨⟩ ⊑ 'a'
   'a'

## negative numbers start from the end of 𝕩, where the last element is at ¯1
¯2 ⊑ 0‿1‿2‿3‿4
   3

## In general, 𝕨 can be a list of numbers whose length is 𝕩 rank
## when =𝕩 is 1, 𝕨 can be a length-1 list
⟨2,0⟩ ⊑ ↕4‿5      # Picking the result of Range, gives the index
   ⟨ 2 0 ⟩

⊢ a ← 'a' + ⥊⟜(↕×´) 4‿5
   ┌─
   ╵\"abcde
     fghij
     klmno
     pqrst\"
           ┘

2‿0 ⊑ a
   'k'

1‿¯1 ⊑ a
   'j'

## Pick also accepts a list of indices, these must be lists otherwise 𝕨 looks
## like a single list index
⟨2‿0, 1‿¯1, 3‿1, ¯1‿¯1⟩ ⊑ a
   \"kjqt\"

⟨2,1,0,¯1⟩ ⊑ \"abc\"  # 𝕩 doesn't have rank 4!
   Error: 𝕨⊑𝕩: Index length in 𝕨 must match rank of 𝕩
at ⟨2,1,0,¯1⟩ ⊑ \"abc\"
              ^

⟨2,1,0,¯1⟩ ⥊¨⊸⊑ \"abc\"
   \"cbac\"

⟨2,1,0,¯1⟩ ⊏ \"abc\"  # Better way
   \"cbac\"

## as long as your indices are in lists, you can arrange them in any array
## structure with arbritrary nesting
⟨⟨2,3⟩,1⟩ ⊑ a  # 1 isn't a valid index
   Error: 𝕨⊑𝕩: Indices in compound 𝕨 must be lists
at ⟨⟨2,3⟩,1⟩ ⊑ a
             ^

⟨2‿0, ⟨⟨1‿¯1, 3‿1⟩, ¯1‿¯1⟩⟩ ⊑ a
   ⟨ 'k' ⟨ \"jq\" 't' ⟩ ⟩

(⟨2‿0, 1‿¯1⟩≍⟨3‿1, ¯1‿¯1⟩) ⊑ a
   ┌─
   ╵\"kj
     qt\"
        ┘

(⟨2‿0, <1‿¯1⟩≍⟨<3‿1, ¯1‿¯1⟩) ⊑ a
   ┌─
   ╵ 'k'   ┌·
           ·'j'
               ┘
     ┌·    't'
     ·'q'
         ┘
                 ┘

## a more convienient way is to use the ⚇ (Depth). Pick applies to Depth-1
## componenets of 𝕨 and all of 𝕩, which corresponds to a depth operand of 1‿∞
(⟨2‿0, <1‿¯1⟩≍⟨<3‿1, ¯1‿¯1⟩) ⊑⚇1‿∞ a
   ┌─
   ╵ 'k'   ┌·
           ·'j'
               ┘
     ┌·    't'
     ·'q'
         ┘
                 ┘"]

      ;; ================================================
      ;; Classify
"⊐"

["Monad: Classify | Dyad: Index of | Input: \\o"

 "⊐ is a function.
  Its monadic form returns a list of natural numbers, where each number
    corresponds to the index of first appearance of the corresponding value in 𝕩.
  Its dyadic form returns a list of indices, where each index is the first
    occurrence of each entry in 𝕨, in 𝕩.
  Note: (Classify) is idempotent.
        see related function, ⍷ (Deduplicate)
        see related function, ⊒ (Occurence Count)"


 "Examples:

## Monadic form
# notice that 5 is at index 0, and so 0's are in 5's position in the result
⊐ 5‿6‿2‿2‿5‿1
   ⟨ 0 1 2 2 0 3 ⟩

## We can couple the argument to the result to observe this more easily
≍⟜⊐ 5‿6‿2‿2‿5‿1
   ┌─
   ╵ 5 6 2 2 5 1
     0 1 2 2 0 3
                 ┘

## Classify is an inverse of ⍷ (Deduplicate)
⊢ c ← >\"yellow\"‿\"orange\"‿\"yellow\"‿\"purple\"‿\"orange\"‿\"yellow\"
   ┌─
   ╵\"yellow
     orange
     yellow
     purple
     orange
     yellow\"
            ┘

⍷ ⊐ c
   ⟨ 0 1 2 ⟩

⊐ ⍷ c
   ⟨ 0 1 2 ⟩

## notice the Deduplicate indicates which cells are retained
⍷ c
   ┌─
   ╵\"yellow
     orange
     purple\"
            ┘

## while Classify indicates where the cell is located
⊐ c
   ⟨ 0 1 0 2 1 0 ⟩

## Clasify is idempotent: repeated applications return the same result
⊐ ⊐ ⊐ c
   ⟨ 0 1 0 2 1 0 ⟩


## Dyadic form
\"zero\"‿\"one\"‿\"two\"‿\"three\" ⊐ \"one\"‿\"eight\"‿\"two\"
   ⟨ 1 4 2 ⟩"]

      ;; ================================================
      ;; Occurrence Count
"⊒"

["Monad: Occurrence Count | Dyad: Progressive Index of | Input: \\O"

 "⊒ is a function.
  Its monadic form returns a list of natural numbers, where each number
    is the number of previous cells that match the current cell.
  Its dyadic form returns a list of indices, where each index is either the first
    occurrence of each entry in 𝕨, in 𝕩, or the first unused match if there is one.
  Note: (Progressive Index of) no index except ≠𝕨 can be repeated.
                               use ⊒˜<≠∘⊢ for Progressive Membership of
                               ⊒˜ is the same as ↕∘≠
        see related function, ⊐ (Classify)"


 "Examples:

## Monadic form
⊒   2‿7‿1‿8‿1‿7‿1‿8‿2‿8‿4
   ⟨ 0 0 0 0 1 1 2 1 1 2 0 ⟩    # notice a 1 at the next occurrence of 1 in 𝕩

## or more succinctly, notice at each 8 in 𝕩 the count of previous 8's increases
≍⟜⊒ 2‿7‿1‿8‿1‿7‿1‿8‿2‿8‿4
   ┌─
   ╵ 2 7 1 8 1 7 1 8 2 8 4
     0 0 0 0 1 1 2 1 1 2 0
                           ┘

## use Occurrence Count to return exactly one duplicate form a list of duplicates
(1=⊒)⊸/ \"aaaabcddcc\"
   \"adc\"

## an interesting use case is to apply Occurrence Count to / (Indices), this
## returns a list of numbers, that is a sequence of counting up to each number
## in 𝕩. Here the result is three sequences counting up to 2, 3, and then 4 in
## that order. Note
⊒ / 2‿3‿4

   ⟨ 0 1 0 1 2 0 1 2 3 ⟩

## /(¯1⊸⊑↕⊸-⊏⟜»)+` is identical to ⊒/ but is more efficient and more complicated
(/(¯1⊸⊑↕⊸-⊏⟜»)+`) 2‿3‿4
   ⟨ 0 1 0 1 2 0 1 2 3 ⟩


## Dyadic form
\"aaa\" ⊒ \"aaaaa\"      # the first 3 'a's match, but the last two are unused
   ⟨ 0 1 2 3 3 ⟩

## in this example the first 3 'a's of 𝕩 are matched to indices 0 1 and 2 in 𝕨
## and the first two 'b's to indices 3 and 4 of 𝕨, then we only have unused
## matches so ≠𝕨 is used.
\"aaabb\" ⊒ \"ababababab\"
   ⟨ 0 3 1 4 2 5 5 5 5 5 ⟩"]

      ;; ================================================
      ;; Mark Firsts
"∊"

["Monad: Mark Firsts | Dyad: Member of | Input: \\e"

 "∊ is a function.
  Its monadic form returns a list of booleans, where each number is either a 0,
    if the major cell of 𝕩 is a duplicate of a previous cell, or 1 otherwise.
  Its dyadic form returns a list of numbers of length ≠𝕨, each number is either
    a 0 or 1. A 1 indicates an entry of 𝕨 matches some entry in 𝕩, a 0 otherwise.
  Note: see related function, ⍷ (Deduplicate)"


 "Examples:

## Monadic form
∊   3‿1‿4‿1‿5‿9‿2‿6‿5
   ⟨ 1 1 1 0 1 1 1 1 0 ⟩     # notice the first duplicate 1, corresponds to a 0

## once can implement ⍷ (Deduplicate) in terms of ∊
∊⊸/ 3‿1‿4‿1‿5‿9‿2‿6‿5
   ⟨ 3 1 4 5 9 2 6 ⟩

⍷ 3‿1‿4‿1‿5‿9‿2‿6‿5
   ⟨ 3 1 4 5 9 2 6 ⟩

## use ¬∘∊⊸/ to remove the first of each value, leaving only the duplicates
¬∘∊⊸/ 3‿1‿4‿1‿5‿9‿2‿6‿5‿5
   ⟨ 1 5 5 ⟩

## use ∧´∊ to check if an array does not have duplicates
∧´∊ 3‿1‿4‿1‿5‿9‿2‿6‿5‿5
   0                         # thus we have duplicates in the input

## use +´∊ to count the number of unique cells
≠ 3‿1‿4‿1‿5‿9‿2‿6‿5‿5
   10                        # 10 elements

+´∊ 3‿1‿4‿1‿5‿9‿2‿6‿5‿5
   7                         # with 7 uniques

## use ∊∧∊⌾⌽ to mark the elements that appear exactly once
(∊∧∊⌾⌽) \"duck\"‿\"duck\"‿\"teal\"‿\"duck\"‿\"goose\"
   ⟨ 0 0 1 0 1 ⟩


## Dyadic form
## results are independent of the ordering of 𝕩
\"green\"‿\"bricks\"‿\"cow\"‿\"blue\" ∊ \"red\"‿\"green\"‿\"blue\"
   ⟨ 1 0 0 1 ⟩

## use ∊ in a train for set difference and intersection
\"initial set\" (∊/⊣) \"intersect\"     # Keep 𝕩
   \"initiset\"

\"initial set\" (¬∘∊/⊣) \"difference\"  # Remove 𝕩
   \"tal st\""]

      ;; ================================================
      ;; Deduplicate
"⍷"

["Monad: Deduplicate | Dyad: Find | Input: \\E"

 "⍷ is a function.
  Its monadic form removes every major cell that matches an earlier cell.
  Its dyadic form searches for occurrences of an array 𝕨, in 𝕩. The result is a
    list of booleans for each possible location.
  Note: (Deduplicate) can be implemented as ∊⊸/
                      see related function, ⊐ (Classify)
        (Find)        𝕨 needs to match a contiguous section of 𝕩
                      there is no guarantee the result maintains the shape of 𝕩
                      if ≠𝕨 > ≠𝕩 then the result is empty"


 "Examples:

## Monadic form
⍷ >\"take\"‿\"drop\"‿\"drop\"‿\"pick\"‿\"take\"‿\"take\"
   ┌─
   ╵\"take
     drop
     pick\"
          ┘

## use ⍷⌾⌽ to reverse the ordering
⍷⌾⌽ >\"take\"‿\"drop\"‿\"drop\"‿\"pick\"‿\"take\"‿\"take\"
   ┌─
   ╵\"drop
     pick
     take\"
          ┘


## Dyadic form
\"xx\" ⍷ \"xxbdxxxcx\"        # a contiguous match for strings is a substring
   ⟨ 1 0 0 0 1 1 0 0 ⟩

## The subarrays that are searched are the cells in the result of ↕ (Windows)
## so we can use Windows to see the arrays 𝕨 will be compared against.
2 ↕ \"xxbdxxxcx\"
   ┌─
   ╵\"xx
     xb
     bd
     dx
     xx
     xx
     xc
     cx\"
        ┘

\"xx\"⊸≡˘ 2 ↕ \"xxbdxxxcx\"
   ⟨ 1 0 0 0 1 1 0 0 ⟩

## shape of 𝕩 is not maintained
\"string\" ⍷ \"substring\"
   ⟨ 0 0 0 1 ⟩

## shape is maintained in APL style
\"string\" (≢∘⊢↑⍷) \"substring\"  # APL style
   ⟨ 0 0 0 1 0 0 0 0 0 ⟩

## when ≠𝕨 > ≠𝕩 then the result is empty
\"loooooong\" ⍷ \"short\"
   ⟨⟩

9 ↕ \"short\"
   Error: 𝕨↕𝕩: Window length 𝕨 must be at most axis length plus one
at 9 ↕ \"short\"
     ^

## use ⊑⍷ to test whether 𝕨 is a prefix of 𝕩, and thus isn't longer than 𝕩
## use a fold if this may be the case to return a 0, rather than an Error
0 ⊣´ \"loooooong\" ⍷ \"short\"
   0

## when 𝕩 and 𝕨 are multi-dimensional, Find will do a multi-dimentsional search
## use 𝕨≢⊸↕𝕩 to view the cells that will be matched to 𝕨
⊢ a ← 7 (4|⋆˜)⌜○↕ 9   # Array with patterns
   ┌─
   ╵ 1 1 1 1 1 1 1 1 1
     0 1 2 3 0 1 2 3 0
     0 1 0 1 0 1 0 1 0
     0 1 0 3 0 1 0 3 0
     0 1 0 1 0 1 0 1 0
     0 1 0 3 0 1 0 3 0
     0 1 0 1 0 1 0 1 0
                       ┘

## notice the bottom right, 0 1 0 of a (𝕩) matches the 0‿1‿0 of 𝕨, hence the 1
## in the bottom right corner of the result
(0‿3‿0≍0‿1‿0) ⍷ a
   ┌─
   ╵ 0 0 0 0 0 0 0
     0 0 0 0 0 0 0
     0 0 0 0 0 0 0
     0 0 1 0 0 0 1
     0 0 0 0 0 0 0
     0 0 1 0 0 0 1
                   ┘

## 𝕨 is allowed to be smaller rank than 𝕩, in this case the leading axes of 𝕩
## are mapped over so that axes of 𝕨 correspond to trailing axes of 𝕩
0‿1‿0‿1 ⍷ a
   ┌─
   ╵ 0 0 0 0 0 0
     0 0 0 0 0 0
     1 0 1 0 1 0
     0 0 0 0 0 0
     1 0 1 0 1 0
     0 0 0 0 0 0
     1 0 1 0 1 0
                 ┘"]

      ;; ================================================
      ;; Group
"⊔"

["Monad: Group Indices | Dyad: Group | Input: \\u"

 "⊔ is a function.
  Its monadic form returns a list of lists of indices, where each sublist
    contains indices of equal elements of 𝕩.
  Its dyadic form returns a list of groups, each containing cells from 𝕩,
    according to a list of atomic indices in 𝕨.
  Note: (Group) 𝕨 and 𝕩 must have the same length"


 "Examples:

## Monadic form
⊔ 0‿2‿5‿3‿2
   ⟨ ⟨ 0 ⟩ ⟨⟩ ⟨ 1 4 ⟩ ⟨ 3 ⟩ ⟨⟩ ⟨ 2 ⟩ ⟩     # 2 is at index 1 and 4, 3 is unique hence ⟨⟩

⊔ \"abcdab\"
   Error: ⊔: Grouping argument must consist of integers
at ⊔ \"abcdab\"
   ^


## Dyadic form
0‿1‿2‿0‿1 ≍ \"abcde\"  # Corresponding indices and values
   ┌─
   ╵ 0   1   2   0   1
     'a' 'b' 'c' 'd' 'e'
                         ┘

0‿1‿2‿0‿1 ⊔ \"abcde\"  # Values grouped by index
   ⟨ \"ad\" \"be\" \"c\" ⟩

## use a ¯1 in 𝕨 to drop cells
0‿¯1‿2‿2‿¯1 ⊔ \"abcde\"  # Drop c and e
   ⟨ \"a\" ⟨⟩ \"cd\" ⟩

## add a single extra argument to 𝕨 to set the length of the result
## in this case we add a 6 to produce a length 6 result filled with empty arrays
0‿1‿2‿2‿1‿6 ⊔ \"abcde\"
   ⟨ \"a\" \"be\" \"cd\" ⟨⟩ ⟨⟩ ⟨⟩ ⟩

## in this case an 8 for a length-8 result
0‿1‿2‿2‿1‿8 ⊔ \"abcde\"
   ⟨ \"a\" \"be\" \"cd\" ⟨⟩ ⟨⟩ ⟨⟩ ⟨⟩ ⟨⟩ ⟩

## when 𝕨 is not a list, ⊔ groups -=𝕨 cells of 𝕩, rather than ¯1 cells.
## this is not compatible with the length-extension. One can use this behavior
## to group diagonals of a table
⊢ a ← 'a'+⥊⟜(↕×´)3‿5
   ┌─
   ╵\"abcde
     fghij
     klmno\"
           ┘

(+⌜´·↕¨≢)⊸⊔ a
   ⟨ \"a\" \"bf\" \"cgk\" \"dhl\" \"eim\" \"jn\" \"o\" ⟩

## or group a list of words by length
phrase ← \"BQN\"‿\"uses\"‿\"notation\"‿\"as\"‿\"a\"‿\"tool\"‿\"of\"‿\"thought\"
   ⟨ \"BQN\" \"uses\" \"notation\" \"as\" \"a\" \"tool\" \"of\" \"thought\" ⟩

≍˘ ≠¨⊸⊔ phrase
   ┌─
   ╵ ⟨⟩
     ⟨ \"a\" ⟩
     ⟨ \"as\" \"of\" ⟩
     ⟨ \"BQN\" ⟩
     ⟨ \"uses\" \"tool\" ⟩
     ⟨⟩
     ⟨⟩
     ⟨ \"thought\" ⟩
     ⟨ \"notation\" ⟩
                       ┘

## one can Group according to a computed property, for example with ⊐ (Classify)
ln ← \"Phelps\"‿\"Latynina\"‿\"Bjørgen\"‿\"Andrianov\"‿\"Bjørndalen\"
   ⟨ \"Phelps\" \"Latynina\" \"Bjørgen\" \"Andrianov\" \"Bjørndalen\" ⟩

co ← \"US\"    ‿\"SU\"      ‿\"NO\"     ‿\"SU\"       ‿\"NO\"
   ⟨ \"US\" \"SU\" \"NO\" \"SU\" \"NO\" ⟩

≍˘ co ⊐⊸⊔ ln
   ┌─
   ╵ ⟨ \"Phelps\" ⟩
     ⟨ \"Latynina\" \"Andrianov\" ⟩
     ⟨ \"Bjørgen\" \"Bjørndalen\" ⟩
                                ┘

## or change the left argument of Index of to changed to index to key correspondance
## this will fail if there are trailing keys with no values
countries ← \"IT\"‿\"JP\"‿\"NO\"‿\"SU\"‿\"US\"
   ⟨ \"IT\" \"JP\" \"NO\" \"SU\" \"US\" ⟩

countries ≍˘ co countries⊸⊐⊸⊔ ln
   ┌─
   ╵ \"IT\" ⟨⟩
     \"JP\" ⟨⟩
     \"NO\" ⟨ \"Bjørgen\" \"Bjørndalen\" ⟩
     \"SU\" ⟨ \"Latynina\" \"Andrianov\" ⟩
     \"US\" ⟨ \"Phelps\" ⟩
                                     ┘

## To force the result to have a particular length you can append that length to
## the left argument
countries ≍˘ co countries⊸(⊐∾≠∘⊣)⊸⊔ ln
   ┌─
   ╵ \"IT\" ⟨⟩
     \"JP\" ⟨⟩
     \"NO\" ⟨ \"Bjørgen\" \"Bjørndalen\" ⟩
     \"SU\" ⟨ \"Latynina\" \"Andrianov\" ⟩
     \"US\" ⟨ \"Phelps\" ⟩
                                     ┘"]

      ;; ================================================
      ;; Assert
"!"

["Monad: Assert | Dyad: Assert with message | Input: !"

 "! is a function.
  Its monadic form tests that 𝕩 is 1, if it is then it returns 𝕩, otherwise it
    throws an Error.
  Its dyadic form returns a message with the error thrown.
  Note: (Assert) the right argument must be exactly 1, or 0."


 "Examples:

## Monadic form
! 2=2  # Passed
   1

! 2=3  # Failed
   Error: Assertion error
at ! 2=3
   ^

## an array or list of booleans is not a valid input
! 1‿1‿1‿1
   Error: ⟨1, 1, 1, 1⟩
at ! 1‿1‿1‿1
   ^

## use ∧´⥊ to convert a boolean array to a single boolean
! ∧´⥊ (∧=∨⌾¬)⌜˜ ↕2
   1

## Dyadic form
\"Message\" ! 0
   Error: Message
at \"Message\" ! 0

## 𝕨 is computed before ! is called, so if 𝕨 is costly then you may want to
## store it in a function or use a control structure
MyError ← {𝕨 \"My custom error\"⊸!⍟(1⊸≢) 𝕩}

\"hello\" MyError 0
   Error: My custom error
at MyError ← {𝕨 \"My custom error\"⊸!⍟(1⊸≢) 𝕩}
                ^^^^^^^^^^^^^^^^^^^^^^^^^
at \"hello\" MyError 0
           ^^^^^^^"]
))


  "Table which associates BQN symbols as hash-keys to a 3-vector of docstrings
  where: position 0 is short description for eldoc, position 1 is a long
  description, position 2 is any extra description")

(defun bqn-symbols-doc--symbols ()
  "Return a list of bqn symbols we have docs for"
  (hash-table-keys bqn-symbols-doc--symbol-doc-table))

(defun bqn-symbols-doc--get-doc (symbol doc)
  "Given a stringp SYMBOL, and a keywordp DOC, retrieve a docstring for SYMBOL,
   or nil if no docstring is found"
  (when-let (docs (gethash symbol bqn-symbols-doc--symbol-doc-table))
    (cond ((equal doc :short) (aref docs 0))
          ((equal doc :long)  (aref docs 1))
          ((equal doc :extra) (aref docs 2)))))

(defun bqn-symbols-doc-get-short-doc (symbol)
  "Given SYMBOL as stringp, retrieve a single-line doc string for SYMBOL, or nil"
  (bqn-symbols-doc--get-doc symbol :short))

(defun bqn-symbols-doc-get-long-doc (symbol)
  "Given SYMBOL as stringp, retrieve a multi-line doc string for SYMBOL, or nil"
  (bqn-symbols-doc--get-doc symbol :long))

(defun bqn-symbols-doc-get-extra-doc (symbol)
  "Given SYMBOL as stringp, retrieve a extra doc string for SYMBOL, or nil"
  (bqn-symbols-doc--get-doc symbol :extra))

(provide 'bqn-symbols-doc)

;; Local Variables:
;; coding: utf-8-unix
;; indent-tabs-mode: nil
;; End:

;;; bqn-symbols-doc.el ends here
