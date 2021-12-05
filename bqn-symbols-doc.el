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
;;; bqn-symbols-doc.el ends here
