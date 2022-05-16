;;; laas.el --- A bundle of as-you-type LaTeX snippets -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020-2022 Yoav Marco, TEC, Hyunggyu Jang
;;
;; Authors: Yoav Marco <https://github.com/yoavm448> TEC <https://github.com/tecosaur> Hyunggyu Jang <https://github.com/HyunggyuJang>
;; Maintainer: Yoav Marco <yoavm448@gmail.com>
;; Created: September 22, 2020
;; Modified: April 17, 2021
;; Version: 1.0
;; Keywords: tools, tex
;; Homepage: https://github.com/tecosaur/LaTeX-auto-activating-snippets
;; Package-Requires: ((emacs "26.3") (aas "0.2") (yasnippet "0.14"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Make use of the auto-activating-snippets engine to provide an expansive
;; collection of LaTeX snippets. Primaraly covering: operators, symbols,
;; accents, subscripts, and a few fraction forms.
;;
;;; Code:

(require 'aas)
(require 'yasnippet)

(defgroup laas nil
  "Snippet expansions mid-typing."
  :prefix "laas-"
  :group 'aas)

(defun laas-current-snippet-insert-post-space-if-wanted ()
  "Insert a space at point, if it seems warranted."
  (when (and (stringp aas-transient-snippet-expansion)
             (= ?\\ (aref aas-transient-snippet-expansion 0))
             (not (memq (char-after) '(?\) ?\]))))
    (insert " ")))

(defun laas-auto-script-condition ()
  "Condition used for auto-sub/superscript snippets."
  (let ((c (char-before)))
      (and (/= (char-syntax c) 32)
           (/= (char-syntax c) 40))))

(defun laas-numeric-script-condition ()
  "Condition used for numeric-sub/superscript snippets."
  (let ((c (char-before)))
    (or (= (char-syntax c) 119)
        (= (char-syntax c) 41))))

(defun laas-identify-adjacent-tex-object (&optional point)
  "Return the starting position of the left-adjacent TeX object from POINT."
  (save-excursion
    (if point (goto-char point))
    (let (result)
      (while
          (catch 'recursion
            (setq
             result
             (pcase (char-before)
               ((or ?\) ?\])
                (backward-sexp)
                (point))
               (?\}
                (cl-loop do (backward-sexp)
                         while (= (char-before) ?}))
                ;; try to catch the marco if the braces belong to one
                (when (looking-back "\\\\[A-Za-z@*]+" (line-beginning-position))
                  (goto-char (match-beginning 0)))
                (when (memq (char-before) '(?_ ?^ ?.))
                  (backward-char)
                  (throw 'recursion t)) ; yay recursion
                (point))
               ((pred
                 (lambda (c)
                   (and (/= (char-syntax c) 32)
                        (/= (char-syntax c) 40))))
                (backward-word)
                (when (= (char-syntax (char-before)) 119)
                  (backward-word))
                (when (= (char-before) ?\\) (backward-char))
                (when (memq (char-before) '(?_ ?^ ?.))
                  (backward-char)
                  (throw 'recursion t)) ; yay recursion
                (point))))
            nil))
      result)))

(defun laas-wrap-previous-object (tex-command)
  "Wrap previous TeX object in TEX-COMMAND."
  (lambda ()
    (interactive)
    (let ((start aas-transient-snippet-condition-result)
          needbracket?)
      (save-excursion
        (goto-char start)
        (setq needbracket? (char-equal (char-after) ?{))
        (insert (concat "\\" tex-command (unless needbracket? "{"))))
      (unless needbracket? (insert-char ?})))))

;; HACK Smartparens runs after us on the global `post-self-insert-hook' and
;;      thinks that since a { was inserted after a self-insert event, it
;;      should insert the matching } - even though we took care of that.
;;      laas--shut-up-smartparens removes
(defun laas--restore-smartparens-hook ()
  "Restore `sp--post-self-insert-hook-handler' to `post-self-insert-hook'.

Remove ourselves, `laas--restore-smartparens-hook', as well, so
it is restored only once."
  (remove-hook 'post-self-insert-hook #'laas--restore-smartparens-hook)
  (add-hook 'post-self-insert-hook #'sp--post-self-insert-hook-handler))

(declare-function sp--post-self-insert-hook-handler "smartparens")
(defun laas--shut-up-smartparens ()
  "Remove Smartparens' hook temporarily from `post-self-insert-hook'."
  (when (memq #'sp--post-self-insert-hook-handler
              (default-value 'post-self-insert-hook))
    (remove-hook 'post-self-insert-hook #'sp--post-self-insert-hook-handler)
    ;; push rather than add-hook so it doesn't run right after this very own
    ;; hook, but next time
    (unless (memq #'laas--restore-smartparens-hook
                  (default-value 'post-self-insert-hook))
      (push #'laas--restore-smartparens-hook (default-value 'post-self-insert-hook)))))

(defun laas-inline-snippet (format-string)
  (lambda ()
    (interactive)
    (let* ((tex-obj aas-transient-snippet-condition-result)
           (start (save-excursion
                    ;; if bracketed, delete outermost brackets
                    (if (memq (char-before) '(?\) ?\]))
                        (progn
                          (backward-delete-char 1)
                          (goto-char tex-obj)
                          (delete-char 1))
                      (goto-char tex-obj))
                    (point)))
           (end (point))
           (content (buffer-substring-no-properties start end)))
      (yas-expand-snippet (format format-string content)
                          start end))
    (laas--shut-up-smartparens)))

(defvar laas-basic-snippets
  '(
    "!="    "\\neq"
    "!>"    "\\mapsto"
    "?>"    "\\hookrightarrow"
    "**"    "\\cdot"
    "+-"    "\\pm"
    "-+"    "\\mp"
    "->"    "\\to"
    "~>"    "\\leadsto"
    "..."   "\\dots"
    ",,"    ",~"
    "<<"    "\\ll"
    "<="    "\\leq"
    "<>"    "\\diamond"
    "><"    "\\lessgtr"
    "=<"    "\\impliedby"
    "=="    "\\equiv"
    "=>"    "\\implies"
    ">="    "\\geq"
    ">>"    "\\gg"
    "AA"    "\\forall"
    "EE"    "\\exists"
    "tp"    "^\\intercal"
    "inv"   "^{-1}"
    "cb"    "^3"
    "iff"   "\\iff"
    "inn"   "\\in"
    "notin" "\\notin"
    "sr"    "^2"
    "xx"    "\\times"
    "|-"    "\\vdash"
    "|="    "\\models"
    "||"    "\\mid"
    "~="    "\\approx"
    "~~"    "\\sim"
    "|>"    "\\rhd"
    "<|"    "\\lhd"

    "arccos" "\\arccos"
    "arccot" "\\arccot"
    "arccot" "\\arccot"
    "arccsc" "\\arccsc"
    "arcsec" "\\arcsec"
    "arcsin" "\\arcsin"
    "arctan" "\\arctan"
    "cos"    "\\cos"
    "cot"    "\\cot"
    "csc"    "\\csc"
    "exp"    "\\exp"
    "ln"     "\\ln"
    "log"    "\\log"
    "perp"   "\\perp"
    "sin"    "\\sin"
    "star"   "\\star"
    "gcd"    "\\gcd"
    "min"    "\\min"
    "max"    "\\max"
    "sup"    "\\sup"
    "inf"    "\\inf"
    "det"    "\\det"
    "prec"   "\\prec"
    "succ"   "\\succ"
    "deg"    "\\deg"

    "CC" "\\mathbb{C}"
    "FF" "\\mathbb{F}"
    "HH" "\\mathbb{H}"
    "NN" "\\mathbb{N}"
    "PP" "\\mathbb{P}"
    "QQ" "\\mathbb{Q}"
    "RR" "\\mathbb{R}"
    "ZZ" "\\mathbb{Z}"

    ";a"  "\\alpha"
    ";A"  "\\forall"        ";;A" "\\aleph"
    ";b"  "\\beta"
    ";;;c" "\\cos"
    ";;;C" "\\arccos"
    ";d"  "\\delta"         ";;d" "\\partial"
    ";D"  "\\Delta"         ";;D" "\\nabla"
    ";e"  "\\epsilon"       ";;e" "\\varepsilon"   ";;;e" "\\exp"
    ";E"  "\\exists"                               ";;;E" "\\ln"
    ";f"  "\\phi"           ";;f" "\\varphi"
    ";F"  "\\Phi"
    ";g"  "\\gamma"                                ";;;g" "\\lg"
    ";G"  "\\Gamma"                                ";;;G" "10^{?}"
    ";h"  "\\eta"           ";;h" "\\hbar"
    ";i"  "\\in"            ";;i" "\\imath"
    ";I"  "\\iota"          ";;I" "\\Im"
    ";;j" "\\jmath"
    ";k"  "\\kappa"
    ";l"  "\\lambda"        ";;l" "\\ell"          ";;;l" "\\log"
    ";L"  "\\Lambda"
    ";m"  "\\mu"
    ";n"  "\\nu"            ";;n" "\\cap"          ";;;n" "\\ln"
    ";N"  "\\nabla"                                ";;;N" "\\exp"
    ";o"  "\\omega"
    ";O"  "\\Omega"         ";;O" "\\mho"
    ";p"  "\\pi"            ";;p" "\\varpi"
    ";P"  "\\Pi"
    ";q"  "\\theta"         ";;q" "\\vartheta"
    ";Q"  "\\Theta"
    ";r"  "\\rho"           ";;r" "\\varrho"
    ";;R" "\\Re"
    ";s"  "\\sigma"         ";;s" "\\varsigma"    ";;;s" "\\sin"
    ";S"  "\\Sigma"                               ";;;S" "\\arcsin"
    ";t"  "\\tau"                                 ";;;t" "\\tan"
    ";;;T" "\\arctan"
    ";u"  "\\upsilon"       ";;u" "\\cup"
    ";U"  "\\Upsilon"
    ";v"  "\\vee"
    ";V"  "\\Phi"
    ";w"  "\\xi"
    ";W"  "\\Xi"
    ";x"  "\\chi"
    ";y"  "\\psi"
    ";Y"  "\\Psi"
    ";z"  "\\zeta"
    ";0"  "\\emptyset"
    ";8"  "\\infty"
    ";!"  "\\neg"
    ";^"  "\\uparrow"
    ";&"  "\\wedge"
    ";|"  "\\vee"
    ";~"  "\\approx"        ";;~" "\\simeq"
    ";_"  "\\downarrow"
                            ";;+" "\\oplus"
    ";-"  "\\leftrightarrow"";;-" "\\longleftrightarrow"
    ";*"  "\\times"         ";;*" "\\otimes"
    ";/"  "\\not"
    ";:"  "\\mapsto"        ";;:" "\\longmapsto"
    ";\\" "\\setminus"
    ";="  "\\Leftrightarrow"";;=" "\\Longleftrightarrow"
    ";(" "\\langle"
    ";)" "\\rangle"
    ";[" "\\Leftarrow"     ";;[" "\\Longleftarrow"
    ";]" "\\Rightarrow"    ";;]" "\\Longrightarrow"
    ";{"  "\\subset"
    ";}"  "\\supset"
    ";<"  "\\leftarrow"    ";;<" "\\longleftarrow"  ";;;<" "\\min"
    ";>"  "\\rightarrow"   ";;>" "\\longrightarrow" ";;;>" "\\max"
    ";'"  "\\prime"
    ";."  "\\cdot"         ";;." "\\circ"
    "; "  "\\quad"         ";; " "\\qquad")
  "Basic snippets. Expand only inside maths.")

(defvar laas-subscript-snippets
  `(:cond laas-auto-script-condition
    "jj" ,(lambda () (interactive)
            (doom-snippets-expand :uuid "subscript"))
    "kk" ,(lambda () (interactive)
            (doom-snippets-expand :uuid "superscript"))
    :cond laas-numeric-script-condition
    "0"   "_0"
    "1"   "_1"
    "2"   "_2"
    "3"   "_3"
    "4"   "_4"
    "5"   "_5"
    "6"   "_6"
    "7"   "_7"
    "8"   "_8"
    "9"   "_9")
  "Automatic subscripts! Expand In math and after a single letter.")

(defvar laas-smart-snippets
  `(:cond laas-identify-adjacent-tex-object
    ,@(cl-loop for (key desc format) in '(("/" "frac" "\\frac{%s}{$1}$0")
                                          ("'O" "stackrel" "\\stackrel{$1}{%s}$0")
                                          ("'U" "underset" "\\underset{$1}{%s}$0"))
               collect :expansion-desc
               collect (format "Wrap object on the left with \\%s{}{}" desc)
               collect key
               collect (laas-inline-snippet format)))
  "Smart snippet. Expand in maths when there's something to expand on on the left.")


(defvar laas-accent-snippets
  `(:cond laas-identify-adjacent-tex-object
    ,@(cl-loop for (key exp) in '(("'." "dot")
                                  ("':" "ddot")
                                  ("'u"  "breve")
                                  ("'~" "tilde")
                                  ("'h" "hat")
                                  ("'-" "bar")
                                  ("'T"  "overline")
                                  ("'_"  "underline")
                                  ("'{"  "overbrace")
                                  ("'}"  "underbrace")
                                  ("'>"  "vec")
                                  ("'/"  "grave")
                                  ("'\"" "acute")
                                  ("'v"  "check")
                                  ("'u"  "breve")
                                  ("'s" "mathscr")
                                  ("'S" "mathsf")
                                  ("'f" "mathfrak")
                                  ("'c" "mathcal")
                                  ("'b" "mathbf")
                                  ("'B" "boldsymbol")
                                  ("'r" "mathrm")
                                  ("'t" "text"))
               collect :expansion-desc
               collect (format "Wrap in \\%s{}" exp)
               collect key
               collect (laas-wrap-previous-object exp)))
  "A simpler way to apply accents. Expand If LaTeX symbol immidiately before point.")

(defun laas--no-backslash-before-point? ()
  "Check that the char preceding the snippet key is not backslash."
  ;; conditions are already run right at the start of the snippet key, no need
  ;; to move point
  (not (eq (char-before) ?\\)))


(apply #'aas-set-snippets 'laas-mode laas-basic-snippets)
(apply #'aas-set-snippets 'laas-mode laas-subscript-snippets)
(apply #'aas-set-snippets 'laas-mode laas-smart-snippets)
(apply #'aas-set-snippets 'laas-mode laas-accent-snippets)


;;;###autoload
(define-minor-mode laas-mode
  "Minor mode for enabling a ton of auto-activating LaTeX snippets."
  :init-value nil
  (if laas-mode
      (progn
        (aas-mode +1)
        (aas-activate-keymap 'laas-mode)
        (add-hook 'aas-global-condition-hook
                  #'laas--no-backslash-before-point?
                  nil 'local))
    (aas-deactivate-keymap 'laas-mode)
    (remove-hook 'aas-global-condition-hook #'laas--no-backslash-before-point?
                 'local)
    (remove-hook 'aas-post-snippet-expand-hook
                 #'laas-current-snippet-insert-post-space-if-wanted
                 'local)
    (unless aas-active-keymaps
      (aas-mode -1))))

(provide 'laas)
;;; laas.el ends here
