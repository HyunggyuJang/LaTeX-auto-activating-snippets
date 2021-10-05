;;; laas.el --- A bundle of as-you-type LaTeX snippets -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020-2021 Yoav Marco, TEC
;;
;; Authors: Yoav Marco <https://github.com/yoavm448> TEC <https://github.com/tecosaur>
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
  (cond ((or (bobp) (= (1- (point)) (point-min)))
         nil)
        ((let ((look-before (char-before)))
            (or (<= ?a look-before ?z)
                (<= ?A look-before ?Z)
                (memq look-before (list ?\] ?\} ?\))))))))

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
                   (or (<= ?a c ?z)
                       (<= ?A c ?Z)
                       (<= ?0 c ?9))))
                (backward-word)
                (when (= (char-before) ?\\) (backward-char))
                (when (memq (char-before) '(?_ ?^ ?.))
                  (backward-char)
                  (throw 'recursion t)) ; yay recursion
                (point))))
            nil))
      result)))

(defun laas-wrap-previous-object (tex-command)
  "Wrap previous TeX object in TEX-COMMAND."
  (interactive)
  (let ((start aas-transient-snippet-condition-result))
    (insert "}")
    (save-excursion
      (goto-char start)
      (insert (concat "\\" tex-command "{")))))

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

(defun laas-smart-fraction ()
  "Expansion function used for auto-subscript snippets."
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
    (yas-expand-snippet (format "\\frac{%s}{$1}$0" content)
                        start end))
  (laas--shut-up-smartparens))

(defvar laas-basic-snippets
  '(
    "!="    "\\neq"
    "!>"    "\\mapsto"
    "**"    "\\cdot"
    "+-"    "\\pm"
    "-+"    "\\mp"
    "->"    "\\to"
    "..."   "\\dots"
    "<<"    "\\ll"
    "<="    "\\leq"
    "<>"    "\\diamond"
    "><" "\\lessgtr"
    "=<"    "\\impliedby"
    "=="    "&="
    "=>"    "\\implies"
    ">="    "\\geq"
    ">>"    "\\gg"
    "AA"    "\\forall"
    "EE"    "\\exists"
    "tp"    "^T"
    "inv"   "^{-1}"
    "cb"    "^3"
    "iff"   "\\iff"
    "inn"   "\\in"
    "notin" "\\notin"
    "sr"    "^2"
    "xx"    "\\times"
    "|->"   "\\mapsto"
    "|="    "\\models"
    "||"    "\\mid"
    "~="    "\\approx"
    "~~"    "\\sim"

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
    "; "  "\\quad"         ";; " "\\qqaud")
  "Basic snippets. Expand only inside maths.")

(defvar laas-subscript-snippets
  `(:cond laas-auto-script-condition
    "ii"  "_i"
    "Ii"  "^i"
    "ip1" "_{i+1}"
    "Ip1" "^{i+1}"
    "im1" "_{i-1}"
    "Im1" "^{i-1}"
    "jj"  "_j"
    "Jj"  "^j"
    "jp1" "_{j+1}"
    "Jp1" "^{j+1}"
    "jm1" "_{j-1}"
    "Jm1" "^{j-1}"
    "nn"  "_n"
    "Nn"  "^n"
    "np1" "_{n+1}"
    "Np1" "^{n+1}"
    "nm1" "_{n-1}"
    "Nm1" "^{n-1}"
    "kk"  "_k"
    "Kk"  "^k"
    "kp1" "_{k+1}"
    "Kp1" "^{k+1}"
    "km1" "_{k-1}"
    "Km1" "^{k-1}"
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

(defvar laas-frac-snippet
  '(:cond laas-identify-adjacent-tex-object
    :expansion-desc "Wrap object on the left with \\frac{}{}, leave `point' in the denuminator."
    "/" laas-smart-fraction)
  "Frac snippet. Expand in maths when there's something to frac on on the left.")


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
                                  ("'f" "mathfrak")
                                  ("'c" "mathcal")
                                  ("'b" "mathbf")
                                  ("'r" "mathrm")
                                  ("'t" "text"))
               collect :expansion-desc
               collect (format "Wrap in \\%s{}" exp)
               collect key
               ;; re-bind exp so its not changed in the next iteration
               collect (let ((expp exp)) (lambda () (interactive)
                                           (laas-wrap-previous-object expp)))))
  "A simpler way to apply accents. Expand If LaTeX symbol immidiately before point.")

(defun laas--no-backslash-before-point? ()
  "Check that the char preceding the snippet key is not backslash."
  ;; conditions are already run right at the start of the snippet key, no need
  ;; to move point
  (not (eq (char-before) ?\\)))


(apply #'aas-set-snippets 'laas-mode laas-basic-snippets)
(apply #'aas-set-snippets 'laas-mode laas-subscript-snippets)
(apply #'aas-set-snippets 'laas-mode laas-frac-snippet)
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
