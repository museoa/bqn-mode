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

;; There are two ways to access the BQN keymap:
;; - When editing a BQN file, use keys with the super (s-) modifier.
;; - Enable backslash prefixes by entering C-\ (`toggle-input-method')
;;   then BQN-Z. Then enter backslash \ before a key.

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'quail)
(require 'bqn-symbols-doc)

;;;###autoload
(defgroup bqn nil
  "Major mode for interacting with the BQN interpreter."
  :prefix 'bqn
  :group 'languages)

(defvar bqn-symbols--list
  '(;; Top row
    ;; `
    ("high-tilde" "˜" ?`)
    ("logical-not" "¬" ?~)
    ;; 1
    ("breve" "˘" ?1)
    ("circle-notch" "⎉" ?!)
    ;; 2
    ("diaeresis" "¨" ?2)
    ("circled-diaeresis" "⚇" ?@)
    ;; 3
    ("high-equals" "⁼" ?3)
    ("circle-star" "⍟" ?#)
    ;; 4
    ("corner" "⌜" ?4)
    ("circle-corner" "◶" ?$)
    ;; 5
    ("acute" "´" ?5)
    ("circle-slash" "⊘" ?%)
    ;; 6
    ("double-acute" "˝" ?6)
    ("circle-triangle" "⎊" ?^)
    ;; 7
    ("hydrant" "⍎" ?&)
    ;; 8
    ("infinity" "∞" ?8)
    ("thorn" "⍕" ?*)
    ;; 9
    ("high-minus" "¯" ?9)
    ("open-angle" "⟨" ?\()
    ;; 0
    ("bullet" "•" ?0)
    ("close-angle" "⟩" ?\))
    ;; -
    ("division-sign" "÷" ?-)
    ("root" "√" ?_)
    ;; =
    ("multiplication-sign" "×" ?=)
    ("star" "⋆" ?+)

    ;; First row
    ;; q
    ("circle-stile" "⌽" ?q)
    ("down-left-arrow" "↙" ?Q)
    ;; w
    ("double-w" "𝕨" ?w)
    ("double-upper-w" "𝕎" ?W)
    ;; e
    ("epsilon" "∊" ?e)
    ("epsilon-underbar" "⍷" ?E)
    ;; r
    ("up-arrow" "↑" ?r)
    ("double-r" "𝕣" ?R)
    ;; t
    ("logical-and" "∧" ?t)
    ("up-grade" "⍋" ?T)
    ;; y
    ;; u
    ("square-cup" "⊔" ?u)
    ;; i
    ("open-square" "⊏" ?i)
    ("open-square-underbar" "⊑" ?I)
    ;; o
    ("close-square" "⊐" ?o)
    ("close-square-underbar" "⊒" ?O)
    ;; p
    ("pi" "π" ?p)
    ("iota" "⍳" ?P)
    ;; [
    ("left-arrow" "←" ?\[)
    ("left-tack" "⊣" ?{)
    ;; ]
    ("right-arrow" "→" ?\])
    ("right-tack" "⊢" ?})
    ;; \
    ("backslash" "\\" ?\\)

    ;; Second row
    ;; a
    ("circle-backslash" "⍉" ?a)
    ("up-left-arrow" "↖" ?A)
    ;; s
    ("double-s" "𝕤" ?s)
    ("double-upper-s" "𝕊" ?S)
    ;; d
    ("up-down-arrow" "↕" ?d)
    ;; f
    ("double-f" "𝕗" ?f)
    ("double-upper-f" "𝔽" ?F)
    ;; g
    ("double-g" "𝕘" ?g)
    ("double-upper-g" "𝔾" ?G)
    ;; h
    ("left-loop" "⊸" ?h)
    ("left-chevron" "«" ?H)
    ;; j
    ("jot" "∘" ?j)
    ;; k
    ("circle" "○" ?k)
    ("circle-jot" "⌾" ?K)
    ;; l
    ("right-loop" "⟜" ?l)
    ("right-chevron" "»" ?L)
    ;; ;
    ("diamond" "⋄" ?\;)
    ("middle-dot" "·" ?:)
    ;; '
    ("left-hook-arrow" "↩" ?')
    ("high-dot" "˙" ?\")

    ;; Third row
    ;; z
    ("zigzag" "⥊" ?z)
    ("bowtie" "⋈" ?Z)
    ;; x
    ("double-x" "𝕩" ?x)
    ("double-upper-x" "𝕏" ?X)
    ;; c
    ("down-arrow" "↓" ?c)
    ;; v
    ("logical-or" "∨" ?v)
    ("down-grade" "⍒" ?V)
    ;; b
    ("left-floor" "⌊" ?b)
    ("left-ceiling" "⌈" ?B)
    ;; n
    ;; m
    ("identical-to" "≡" ?m)
    ("not-identical-to" "≢" ?M)
    ;; ,
    ("join" "∾" ?,)
    ("less-than-or-equal-to" "≤" ?<)
    ;; .
    ("couple" "≍" ?.)
    ("greater-than-or-equal-to" "≥" ?>)
    ;; /
    ("not-equal-to" "≠" ?/)
    ("left-double-arrow" "⇐" ??)

    ;; Space bar
    ("ligature" "‿" ? )))

;;;; input method

(quail-define-package "BQN-Z" "UTF-8" "⍉" t
                      "Input mode for BQN"
                      '(("\t" . quail-completion))
                      t                 ; forget-last-selection
                      nil               ; deterministic
                      nil               ; kbd-translate
                      t                 ; show-layout
                      nil               ; create-decode-map
                      nil               ; maximum-shortest
                      nil               ; overlay-plist
                      nil               ; update-translation-function
                      nil               ; conversion-keys
                      t                 ; simple
                      )

(defvar bqn-key-prefix--transcription-alist)
(defun bqn-key-prefix--set (prefix new)
  "Set a key PREFIX to the NEW one."
  (quail-select-package "BQN-Z")
  (quail-install-map
   (let* ((prefix (string new))
          (bqn-key-prefix--transcription-alist
           (cl-loop for command in bqn-symbols--list
                    collect (cons (concat prefix (char-to-string (cl-third command)))
                                  (cl-second command)))))
     (quail-map-from-table
      '((default bqn-key-prefix--transcription-alist)))))
  (set-default prefix new))

(defun bqn-key-prefix--initialize (prefix new)
  "Initialize the key PREFIX with the NEW one."
  (custom-initialize-default prefix new)
  (bqn-key-prefix--set prefix (eval new)))

(defcustom bqn-key-prefix ?\\
  "Set a character to serve as prefix key for BQN symbol input."
  :type 'character
  :group 'bqn
  :initialize #'bqn-key-prefix--initialize
  :set #'bqn-key-prefix--set)

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
  "Face used for BQN functions."
  :group 'bqn)

(defface bqn-primitive-one-modifier
  '((t (:inherit font-lock-preprocessor-face)))
  "Face used for BQN 1-modifiers."
  :group 'bqn)

(defface bqn-primitive-two-modifier
  '((t (:inherit font-lock-keyword-face)))
  "Face used for BQN 2-modifiers."
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
     ("[←⇐↩→]" . 'bqn-arrow)
     ("·" . 'bqn-nothing)
     ("[:;?]" . 'font-lock-type-face)
     ("[‿,⋄]" . ,(if (facep 'font-lock-delimiter-face) ''font-lock-delimiter-face ''default))
     ;; built-ins
     ("[∘○⊸⟜⌾⊘◶⎉⚇⍟⎊]" . 'bqn-primitive-two-modifier)
     ("[˙˜˘¨⌜⁼´˝`]" . 'bqn-primitive-one-modifier)
     ("[+×÷⋆√⌊⌈|¬∧∨<>≠=≤≥≡≢⊣⊢⥊∾≍⋈↑↓↕«»⌽⍉/⍋⍒⊏⊑⊐⊒∊⍷⊔!⍕⍎-]" . 'bqn-primitive-function)
     ;; note: π∞¯ may, in fact, be part of identifier names; order of clauses matters
     ("_𝕣_\\|•?\\_<_[A-Za-z][A-Z_a-z0-9π∞¯]*_\\_>" . 'bqn-two-modifier)
     ("_𝕣\\|•?\\_<_[A-Za-z][A-Z_a-z0-9π∞¯]*\\_>" . 'bqn-one-modifier)
     ("[𝔽𝔾𝕎𝕏𝕊]\\|•?\\_<[A-Z][A-Z_a-z0-9π∞¯]*\\_>" . 'bqn-function)
     ("[𝕗𝕘𝕨𝕩𝕤𝕣]\\|•?\\_<[a-z][A-Z_a-z0-9π∞¯]*\\_>" . 'bqn-subject) ;TODO had single • --- why?
     ("\\_<¯?\\(\\([0-9]+\\.\\)?[0-9]+\\(e¯?[0-9]+\\)?\\|π\\|∞\\)\\(i¯?\\(\\([0-9]+\\.\\)?[0-9]+\\(e¯?[0-9]+\\)?\\|π\\|∞\\)\\)?\\_>"
      . ,(if (facep 'font-lock-number-face) ''font-lock-number-face ''font-lock-constant-face))
     ;; *after* numbers have been parsed:
     ("\\." . ,(if (facep 'font-lock-punctuation-face) ''font-lock-punctuation-face ''default))
     ("[┌─╵╎┆┊┘]" . 'bqn-box)
     ;; anything else:
     ("[^ \r\n]" . 'error))
    nil nil nil))

(defvar bqn-syntax--table
  (let ((table (make-syntax-table)))
    (cl-loop for s in bqn-symbols--list
             do (modify-syntax-entry (aref (cl-second s) 0) "." table))
    (cl-loop for s in (append "$%&*+-/<=>|" nil)
             do (modify-syntax-entry s "." table))
    (modify-syntax-entry ?#  "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?¯  "_" table)
    (modify-syntax-entry ?π  "_" table)
    (modify-syntax-entry ?∞  "_" table)
    (modify-syntax-entry ?⟩  ")⟨" table)
    (modify-syntax-entry ?⟨  "(⟩" table)
    table)
  "Syntax table for `bqn-mode'.")

;; Eldoc functions
(defvar bqn-help--function-regexp
  (regexp-opt
   (let* ((symbols (mapcar #'cadr bqn-symbols--list))
          (others  (bqn-symbols-doc--symbols)))
     (cl-union symbols others)))
  "Regex to match BQN functions.")

(defun bqn-help--eldoc ()
  "Return the doc string for the thing at point, or nil."
  (when (looking-at bqn-help--function-regexp)
    (bqn-symbols-doc-get-short-doc (match-string 0))))

;; Help functions
(defun bqn-help--close-documentation-buffer ()
  "Close the active documentation window."
  (interactive)
  (quit-window))

(defvar bqn-help-documentation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'bqn-help--close-documentation-buffer)
    map)
  "Keymap for documentation buffers.")

(define-derived-mode bqn-help-documentation-mode fundamental-mode
  "BQN Documentation"
  "Major mode for displaying BQN documentation."
  (use-local-map bqn-help-documentation-mode-map)
  (setq-local font-lock-defaults bqn--font-lock-defaults)
  (setq-local eldoc-documentation-function 'bqn-help--eldoc)
  (buffer-face-set 'bqn-default))

;; General interactive help
(defvar bqn-help-*documentation-buffer-name* "*bqn-help*")

(defun bqn-help--bqn-symbols-info (test)
  "Find entry in `bqn-symbols--list' using the filter functionp TEST."
  (seq-filter test bqn-symbols--list))

(defun bqn-help-symbol-at-point-is-called ()
  "Show the canonical name of the symbol at point in the minibuffer."
  (interactive)
  (when (looking-at bqn-help--function-regexp)
    (if-let* ((symbol (match-string 0))
              (result (bqn-help--bqn-symbols-info
                       (lambda (v) (equal (cadr v) symbol)))))
        (message "We call symbol %s %s" symbol (caar result))
      (message "No name for %s found" symbol))))

(defun bqn-help-symbol-info-at-point ()
  "Get multi-line documentation for the thing at point, or nil."
  (interactive)
  (when (looking-at bqn-help--function-regexp)
    (if-let* ((symbol (match-string 0))
              (long   (bqn-symbols-doc-get-long-doc symbol))
              (extra  (bqn-symbols-doc-get-extra-doc symbol))
              (sep    "\n\n========================================\n\n")
              (doc-buffer (get-buffer-create
                           bqn-help-*documentation-buffer-name*)))
        (with-current-buffer doc-buffer ;; we have a hit
          (read-only-mode 0)            ; set read only
          (delete-region (point-min) (point-max))
          (insert long sep extra)
          (goto-char (point-min))
          (bqn-help-documentation-mode)
          (read-only-mode 1)            ; unset read only
          (pop-to-buffer doc-buffer))
      (message "No help for %s found!" symbol))))

(defun bqn-mode-map--command-name (name)
  "Return a symbol for the command NAME."
  (intern (concat "bqn-insert-sym-" name)))

(pcase-dolist (`(,name ,symbol ,_) bqn-symbols--list)
  (defalias (bqn-mode-map--command-name name)
    (lambda ()
      (interactive)
      (insert symbol))))

(defun bqn-mode-map--make-base (prefix)
  "Create a new keymap using the key PREFIX."
  (let ((map (make-sparse-keymap)))
    (pcase-dolist (`(,name ,_ ,key) bqn-symbols--list)
      (let ((cmd (bqn-mode-map--command-name name))
            (key (single-key-description key)))
        (define-key map (kbd (concat prefix key)) cmd)))
    (define-key map [menu-bar bqn] (cons "BQN" (make-sparse-keymap "BQN")))
    map))

;; value gets updated by initialization of `bqn-mode-map-prefix'
(defvar bqn-mode-map--keymap nil
  "The keymap for `bqn-mode'.")

(defun bqn-mode-map--set-prefix (prefix new)
  "Set the BQN mode keymap's PREFIX to NEW and recreate the keymap."
  (set-default prefix new)
  (setq bqn-mode-map--keymap (bqn-mode-map--make-base new)))

(defcustom bqn-mode-map-prefix "s-"
  "This stores the keymap prefix for `bqn-mode-map--keymap'.
It is used both to store the new value using
`set-create' and to update `bqn-mode-map--keymap' using
`bqn-mode-map--make-base'. Kill and restart your BQN buffers
to reflect the change."
  :type 'string
  :group 'bqn
  :set #'bqn-mode-map--set-prefix
  :initialize #'custom-initialize-set)

;;;###autoload
(define-derived-mode bqn-mode prog-mode "BQN"
  "Major mode for editing BQN files."
  :syntax-table bqn-syntax--table
  :group 'bqn
  (use-local-map bqn-mode-map--keymap)
  (setq-local font-lock-defaults bqn--font-lock-defaults)
  (setq-local eldoc-documentation-function 'bqn-help--eldoc)
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
  "Commandline arguments to pass to the BQN interpreter."
  :type 'string
  :group 'bqn)

(defvar bqn-comint-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; add keymaps here
    map)
  "Basic mode to run BQN.")

(defvar bqn-comint--process-name "BQN"
  "Name of BQN comint process.")

(defcustom bqn-comint-flash-on-send t
  "When non-nil flash the region sent to BQN process."
  :type 'boolean
  :group 'bqn)

(defun bqn-comint--flash-region (start end &optional timeout)
  "Temporarily highlight region from START to END for TIMEOUT seconds."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))

(defun bqn-comint--ensure-process ()
  "Check for a running BQN process and return its buffer.
If the process does not exist, create it.

FIXME: we do not actually check that the process is alive."
  (if-let ((proc (get-process bqn-comint--process-name)))
      (process-buffer proc)
    (bqn-comint-run-process)))

;;;###autoload
(defun bqn-comint-run-process ()
  "Run an inferior BQN process inside Emacs and return its buffer."
  (interactive)
  (let ((buf-name (concat "*" bqn-comint--process-name "*")))
    ;; same buffer name as auto-created when passing nil below
    (if-let ((buf (get-buffer buf-name)))
        (if (comint-check-proc buf)
            buf
          (error "Buffer '%s' exists but has no live process" buf-name))
      (let ((buf
             (apply #'make-comint-in-buffer
                    bqn-comint--process-name buf-name
                    bqn-interpreter nil bqn-interpreter-arguments)))
        (with-current-buffer buf
          (bqn-comint-mode))
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

(defun bqn-comint-process-execute-region (start end &optional follow)
  "Send the region bounded by START and END to the bqn-comint-process-session.

When FOLLOW is non-nil, switch to the inferior process buffer."
  (interactive "r")
  (when (= start end)
    (error "Attempt to send empty region to %s" bqn-comint--process-name))
  (when bqn-comint-flash-on-send
    (bqn-comint--flash-region start end))
  (let ((region (buffer-substring-no-properties start end))
        (pbuf (bqn-comint--ensure-process)))
    (with-current-buffer pbuf
      ;; get rid of prompt for output alignment
      (goto-char (point-max))
      (comint-kill-whole-line 0))
    (comint-send-string (get-buffer-process pbuf)
                        (concat (bqn-comint--escape region) "\n"))
    (when follow
      (select-window (display-buffer pbuf)))))

(defun bqn-comint-process-execute-line (&optional arg)
  "Send the active region, else the current line to the BQN process."
  (interactive "P")
  (cond
   ((use-region-p)
    (bqn-comint-process-execute-region (region-beginning) (region-end) arg))
   (t
    (bqn-comint-process-execute-region (line-beginning-position) (line-end-position) arg))))

(defun bqn-comint-process-execute-buffer (&optional arg)
  "Send the current buffer to BQN process."
  (interactive "P")
  (bqn-comint-process-execute-region (point-min) (point-max) arg))

(define-derived-mode bqn-comint-mode comint-mode "BQN interactive"
  "Major mode for inferior BQN processes."
  :syntax-table bqn-syntax--table
  (setq-local font-lock-defaults bqn-syntax--token-types)
  (buffer-face-set 'bqn-default))

(provide 'bqn-mode)

;;; bqn-mode.el ends here
