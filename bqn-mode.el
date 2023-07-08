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

;; Emacs major mode for BQN programming language.

;;; Code:

(require 'comint)
(require 'quail)
(require 'pulse)
(require 'bqn-symbols)

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
     ("\\_<¯?\\(\\([0-9]+\\.\\)?[0-9]+\\(e¯?[0-9]+\\)?\\|π\\|∞\\)\\(i¯?\\(\\([0-9]+\\.\\)?[0-9]+\\(e¯?[0-9]+\\)?\\|π\\|∞\\)\\)?\\_>"
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
    (modify-syntax-entry ?·  "_" table)
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
    (modify-syntax-entry ?@  "_" table) ;like "nothing" above
    (modify-syntax-entry ?#  "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?'  "\"" table)
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
    (when-let ((docs (bqn--symbol c)))
      (concat (bqn--symbol-eldoc docs) " | Input: "
              (if-let ((prefixed (bqn--symbol-prefixed docs)))
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

;;;###autoload
(defun bqn-comint-buffer ()
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
                 (accept-process-output process 0.1))))
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

(defun bqn-comint-eval-region (start end)
  "Evaluate the region bounded by START and END with the
bqn-comint-process-session and echoes the result."
  (interactive "r")
  (when (= start end)
    (error "Attempt to evaluate empty region to %s" bqn-comint--process-name))
  (when (and bqn-comint-flash-on-send (pulse-available-p))
    (pulse-momentary-highlight-region start end))
  (let ((region (buffer-substring-no-properties start end))
        (process (get-buffer-process (bqn-comint-buffer))))
    (message "%s" (bqn--comint-call-process-silently process region))))

(defun bqn-comint-eval-dwim ()
  "Evaluate the active region or the current line, displaying the result."
  (interactive)
  (cond
   ((use-region-p)
    (bqn-comint-eval-region (region-beginning) (region-end))
    (deactivate-mark))
   (t
    (bqn-comint-eval-region (line-beginning-position) (line-end-position)))))

(defun bqn-comint-eval-buffer ()
  "Evaluate the current buffer contents, displaying the result."
  (interactive)
  (bqn-comint-eval-region (point-min) (point-max)))

(define-derived-mode bqn-comint-mode comint-mode "BQN interactive"
  "Major mode for inferior BQN processes."
  :syntax-table bqn--syntax-table
  :group 'bqn
  (when bqn-glyph-map-modifier
    (set-keymap-parent bqn-comint-mode-map
                       (make-composed-keymap comint-mode-map bqn--glyph-map)))
  (when bqn-use-input-method
    (activate-input-method "BQN-Z"))
  (setq-local syntax-propertize-function bqn--syntax-propertize)
  (setq-local font-lock-defaults bqn--font-lock-defaults)
  (setq-local comint-prompt-regexp "^   $")
  (setq-local comint-prompt-read-only t)
  (buffer-face-set 'bqn-default))

(provide 'bqn-mode)

;;; bqn-mode.el ends here
