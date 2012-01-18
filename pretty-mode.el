;;; pretty-mode.el --- redisplay parts of the buffer as pretty symbols
;;; -*- coding: utf-8 -*-

;;; Commentary:
;;
;; Minor mode for redisplaying parts of the buffer as pretty symbols
;; originally modified from Trent Buck's version at http://paste.lisp.org/display/42335,2/raw
;; Also includes code from `sml-mode'
;; See also http://www.emacswiki.org/cgi-bin/wiki/PrettyLambda
;;
;; Released under the GPL. No implied warranties, etc. Use at your own risk.
;; Arthur Danskin <arthurdanskin@gmail.com>, March 2008
;;
;; to install:
;; (require 'pretty-mode)
;; and
;; (global-pretty-mode 1)
;; or
;; (add-hook 'my-pretty-language-hook 'turn-on-pretty-mode)

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar pretty-syntax-types '(?_ ?w ?\\))

;; modified from `sml-mode'
(defun pretty-font-lock-compose-symbol (alist)
  "Compose a sequence of ascii chars into a symbol."
  ;; Check that the chars should really be composed into a symbol.
  (let* ((start (match-beginning 0))
         (end (match-end 0))
         (syntax (char-syntax (char-after start))))
    (if (or (if (memq syntax pretty-syntax-types)
               (or (memq (char-syntax (char-before start)) pretty-syntax-types)
                  (memq (char-syntax (char-after end)) pretty-syntax-types))
             (memq (char-syntax (char-before start)) '(?. ?\\)))
           (memq (get-text-property start 'face)
                 '(font-lock-doc-face font-lock-string-face
                                      font-lock-comment-face)))
        ;; No composition for you. Let's actually remove any composition
        ;; we may have added earlier and which is now incorrect.
        (remove-text-properties start end '(composition))
      ;; That's a symbol alright, so add the composition.
      (compose-region start end (cdr (assoc (match-string 0) alist)))
;;;       (add-text-properties start end `(display ,repl))
      ))
  ;; Return nil because we're not adding any face property.
  nil)

(defvar pretty-interaction-mode-alist
  '((inferior-scheme-mode . scheme-mode)
    (lisp-interaction-mode . emacs-lisp-mode)
    (inferior-lisp-mode . lisp-mode)
    (inferior-ess-mode . ess-mode)
    (inf-haskell-mode . haskell-mode)
    (tuareg-interactive-mode . tuareg-mode)
    (inferior-python-mode . python-mode)
    (inferior-octave-mode . octave-mode)
    (inferior-ruby-mode . ruby-mode))
  "Alist mapping from inferior process interaction modes to their
  corresponding script editing modes.")


(defun pretty-font-lock-keywords (alist)
  "Return a `font-lock-keywords' style entry for replacing
regular expressions with symbols. ALIST has the form ((STRING .
REPLACE-CHAR) ...)."
  (when alist
    `((,(regexp-opt (mapcar 'car alist))
       (0 (pretty-font-lock-compose-symbol
           ',alist))))))

(defun pretty-keywords (&optional mode)
  "Return the font-lock keywords for MODE, or the current mode if
MODE is nil. Return nil if there are no keywords."
  (let* ((mode (or mode major-mode))
         (kwds (cdr-safe
                (or (assoc mode pretty-patterns)
                    (assoc (cdr-safe
                            (assoc mode pretty-interaction-mode-alist))
                           pretty-patterns)))))
    (pretty-font-lock-keywords kwds)))

(defgroup pretty nil "Minor mode for replacing text with symbols "
  :group 'faces)

(define-minor-mode pretty-mode
  "Toggle Pretty minor mode.
With arg, turn Pretty minor mode on if arg is positive, off otherwise.

Pretty mode builds on `font-lock-mode'. Instead of highlighting
keywords, it replaces them with symbols. For example, lambda is
displayed as λ in lisp modes."
  :group 'pretty
  :lighter " λ"
  (if pretty-mode
      (progn
        (font-lock-add-keywords nil (pretty-keywords) t)
        (font-lock-fontify-buffer))
    (font-lock-remove-keywords nil (pretty-keywords))
    (remove-text-properties (point-min) (point-max) '(composition nil))))

(defun turn-on-pretty-if-desired ()
  "Turn on `pretty-mode' if the current major mode supports it."
  (if (pretty-keywords)
      (pretty-mode 1)))

(define-globalized-minor-mode global-pretty-mode
  pretty-mode turn-on-pretty-if-desired
  :init-value t)

(defun turn-off-pretty-mode ()
  (interactive)
  (pretty-mode -1))


(defun turn-on-pretty-mode ()
  (interactive)
  (pretty-mode +1))

(defun pretty-compile-patterns (patterns)
  "Set pretty patterns in a convenient way.

PATTERNS should be of the form ((GLYPH (REGEXP MODE ...) ...)
...). GLYPH should be a character. MODE should be the name of a
major mode without the \"-mode\". Returns patterns in the form
expected by `pretty-patterns'"
  (let ((pretty-patterns))
    (loop for (glyph . pairs) in patterns do
          (loop for (regexp . major-modes) in pairs do
                (loop for mode in major-modes do
                      (let* ((mode (intern (concat (symbol-name mode)
                                                   "-mode")))
                             (assoc-pair (assoc mode pretty-patterns))

                             (entry (cons regexp glyph)))
                        (if assoc-pair
                            (push entry (cdr assoc-pair))
                          (push (cons mode (list entry))
                                pretty-patterns))))))
    pretty-patterns))

(defvar pretty-patterns
  (let* ((lispy '(scheme emacs-lisp lisp))
         (mley '(tuareg haskell sml coq))
         (c-like '(c c++ perl sh python java ess ruby))
         (all `(,@lispy ,@mley ,@c-like octave latex)))
    (pretty-compile-patterns
     `(
       (?≠ ("!=" ,@c-like scheme octave coq)
           ("<>" tuareg octave)
           ("~=" octave)
           ("/=" haskell emacs-lisp)
           ("\\neq" latex))
       (?≤ ("<=" ,@all)
           ("\\leq" latex))
       (?≥ (">=" ,@all)
           ("\\geq" latex))
       (?≡ ("\\equiv" latex))
       (?⟵ ("<-" ,@mley ess)
           ("\\leftarrow" latex))
       (?⟶ ("->" ,@mley ess c c++ perl)
           ("\\rightarrow" latex))
       (?↑ ("\\^" tuareg)
           ("^+" coq))
       (?⟹ ("=>" sml perl ruby haskell coq)
           ("\\Rightarrow" latex))
       (?⟷ ("<->" coq)
           ("\leftrightarrow" latex))
       (?↣ (">->" coq))
       (?↦ ("\\mapsto" latex))
       (?∅ ("nil" emacs-lisp ruby)
           ("null" scheme java)
           ("NULL" c c++)
           ("None" python)
           ("set0" coq)
           ("()" ,@mley)
           ("\\emptyset" latex)
           ("\\varnothing" latex))
       (?… ("\.\.\." scheme)
           ("\.\." haskell)
           ("\\ldots" latex))
       (?∈ ("in" python))
       (?∉ ("not in" python)
           ("\\notin" coq latex))
       (?⊲ ("<|" coq))
       (?√ ("sqrt" ,@all))
       (?∑ ("sum" python)
           ("\\sum" coq latex)
           ("\\Sigma" latex))
       (?∪ (":|:" coq))
       (?∩ (":&:" coq))
       (?∁ ("~:" coq))
       (?α ("alpha" ,@all)
           ("'a" ,@mley)
           ("\\alpha" latex))
       (?β ("beta" ,@all)
           ("'b" ,@mley)
           ("\\beta" latex))
       (?γ ("gamma" ,@all)
           ("'c" ,@mley)
           ("\\gamma" latex))
       (?Δ ("delta" ,@all)
           ("'d" ,@mley)
           ("\\Delta" latex))
       (?ε ("epsilon" ,@all)
           ("\\epsilon" latex))
       (?ι ("iota" ,@all)
           ("\\iota" latex))
       (?θ ("theta" ,@all)
           ("\\theta" latex))
       (?σ ("sigma" ,@all))
       (?μ ("mu" ,@all))
       (?λ ("lambda" ,@all)
           ("fn" sml)
           ("fun" tuareg)
           ("\\" haskell)
           ("\\lambda" latex))
       (?π ("pi" ,@all)
           ("M_PI" c c++)
           ("\\pi" latex))
       (?Π ("Pi" @all)
           ("\\prod" latex)
           ("\\Pi" latex))
       (?ω ("omega" @all)
           ("\\omega" latex))
       (?Φ ("Phi" @all)
           ("\\Phi" latex))
       (?Ω ("Ohm" @all)
           ("\\ohm" latex)
           ("\\Omega" latex))
       (?℧ ("Mho" @all)
           ("\\mho" latex))
       (?φ ("phi" ,@all)
           ("\\varphi" latex))
       (?η ("eta" ,@all)
           ("\\eta" latex))

       (?² ("**2" python tuareg octave)
           ("^2" octave haskell))
       (?³ ("**3" python tuareg octave)
           ("^3" octave haskell))
       (?ⁿ ("**n" python tuareg octave)
           ("^n" octave haskell coq))

       (?∞ ("HUGE_VAL" c c++))
       (?∎ ("Qed." coq))

       (?∙ ("*" python haskell))

       (?⋂ ("\\bigcap" coq)
           ("\\cap" latex))
       (?∏ ("\\prod" coq))
       (?⋃ ("\\bigcup" coq)
           ("\\cup" latex))
       (?⊎ ("\\uplus" latex))
       (?ℕ ("nat" coq))
       (?∣  ("%|" coq))

;;;    (?⋂ "\\<intersection\\>"   (,@lispen))
;;;    (?⋃ "\\<union\\>"          (,@lispen))

       (?∧ ("and"     emacs-lisp lisp python)
           ("&&" haskell c c++ java perl coq)
           ("\\wedge" latex)
           ("\\land" latex))
       (?∨ ("or"      emacs-lisp lisp python)
           ("||" haskell c c++ java perl coq)
           ("\\vee" latex)
           ("\\lor" latex))

       (?¬ ("not" python lisp emacs-lisp haskell)
           ("!" c c++ java)
           ("~~" coq)
           ("\\neg" latex))
       (?⊻ ("(+)" coq))
       (?∀ ("all" python)
           ("forall" haskell coq)
           ("\\forall" latex))
       (?∃ ("any" python)
           ("exists" coq)
           ("\\exists" latex))
       (?⊂ ("\\proper" coq)
           ("\\subset" latex))
       (?⊆ ("\\subset" coq)
           ("\\subseteq" latex))
       (?∖ (":\\:" coq)
           ("\\setminus" latex))
       (?⋊ ("><|" coq))

       (?× ("\\times" latex))
       (?〈 ("\\langle" latex))
       (?〉 ("\\rangle" latex))
       (?≻ ("\\succ" latex))
       (?≺ ("\\prec" latex)))))
;;;    (?∧ ("\\<And\\>"     emacs-lisp lisp python)
;;;        ("\\<andalso\\>" sml)
;;;        ("&&"            c c++ perl haskell))
;;;    (?∨ ("\\<or\\>"      emacs-lisp lisp)
;;;        ("\\<orelse\\>"  sml)
;;;        ("||"            c c++ perl haskell))
;;;    (?¬ ("\\<!\\>"       c c++ perl sh)
;;;        ("\\<not\\>"     lisp emacs-lisp scheme haskell sml))
    "*List of pretty patterns.

Should be a list of the form ((MODE ((REGEXP . GLYPH) ...)) ...)")


(defun pretty-add-keywords (mode keywords)
  "Add pretty character KEYWORDS to MODE

MODE should be a symbol, the major mode command name, such as
`c-mode' or nil. If nil, pretty keywords are added to the current
buffer. KEYWORDS should be a list where each element has the
form (REGEXP . CHAR). REGEXP will be replaced with CHAR in the
relevant buffer(s)."
  (font-lock-add-keywords
   mode (mapcar (lambda (kw) `(,(car kw)
                          (0 (prog1 nil
                               (compose-region (match-beginning 0)
                                               (match-end 0)
                                               ,(cdr kw))))))
                keywords)))

(defun pretty-regexp (regexp glyph)
  "Replace REGEXP with GLYPH in buffer."
  (interactive "MRegexp to replace:
MCharacter to replace with: ")
  (pretty-add-keywords nil `((,regexp . ,(string-to-char glyph))))
  (font-lock-fontify-buffer))

(provide 'pretty-mode)
