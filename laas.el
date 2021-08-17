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
;; Package-Requires: ((emacs "26.3") (auctex "11.88") (aas "0.2") (yasnippet "0.14"))
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
(require 'texmathp)
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

(defun laas-mathp ()
  "Determine whether point is within a LaTeX maths block."
  (cond
   ((derived-mode-p 'latex-mode) (texmathp))
   ((derived-mode-p 'org-mode) (laas-org-mathp))
   ((message "LaTeX auto-activated snippets does not currently support any of %s"
             (aas--modes-to-activate major-mode))
    nil)))

(defun laas-org-mathp ()
  "Determine whether the point is within a LaTeX fragment or environment."
  (and (texmathp)
       (not (string= (car texmathp-why) "$")))) ;dollar sign is deprecated in org mode.

(defun laas-auto-script-condition ()
  "Condition used for auto-sub/superscript snippets."
  (cond ((or (bobp) (= (1- (point)) (point-min)))
         nil)
        ((and
          ;; Before is some indexable char
          (let ((look-before (char-before)))
            (or (<= ?a look-before ?z)
                (<= ?A look-before ?Z)
                (= ?\} look-before)))
          ;; Inside math
          (laas-mathp)))))

(defun laas-identify-adjacent-tex-object (&optional point)
  "Return the starting position of the left-adjacent TeX object from POINT."
  (save-excursion
    (goto-char (or point (point)))
    (cond
     ((memq (char-before) '(?\) ?\]))
      (backward-sexp)
      (point))
     ((= (char-before) ?})
      (cl-loop do (backward-sexp)
               while (= (char-before) ?}))
      ;; try to catch the marco if the braces belong to one
      (when (looking-back "\\\\[A-Za-z@*]+" (line-beginning-position))
        (goto-char (match-beginning 0)))
      (when (memq (char-before) '(?_ ?^ ?.))
        (backward-char)
        (goto-char (laas-identify-adjacent-tex-object))) ; yay recursion
      (point))
     ((or (<= ?a (char-before) ?z)
          (<= ?A (char-before) ?Z)
          (<= ?0 (char-before) ?9))
      (backward-word)
      (when (= (char-before) ?\\) (backward-char))
      (when (memq (char-before) '(?_ ?^ ?.))
        (backward-char)
        (goto-char (laas-identify-adjacent-tex-object))) ; yay recursion
      (point)))))

(defun laas-wrap-previous-object (tex-command)
  "Wrap previous TeX object in TEX-COMMAND."
  (interactive)
  (let ((start (laas-identify-adjacent-tex-object)))
    ;; Remove smartparen inserted single quote if ' is used for expansion prefix
    (if (= (char-after) ?\')
        (delete-char 1))
    (insert "}")
    (save-excursion
      (goto-char start)
      (insert (concat "\\" tex-command "{")))))

(defun laas-object-on-left-condition ()
  "Return t if there is a TeX object imidiately to the left."
  ;; TODO use `laas-identify-adjacent-tex-object'
  (and (or (<= ?a (char-before) ?z)
           (<= ?A (char-before) ?Z)
           (<= ?0 (char-before) ?9)
           (memq (char-before) '(?\) ?\] ?})))
       (laas-mathp)))

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
  (let* ((tex-obj (laas-identify-adjacent-tex-object))
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
  '(:cond laas-mathp
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
    "><"    "\\lessgtr"
    "=<"    "\\impliedby"
    "=="    "&="
    "=>"    "\\implies"
    ">="    "\\geq"
    ">>"    "\\gg"
    "tp"    "^T"
    "inv"   "^{-1}"
    "cb"    "^3"
    "sr"    "^2"
    "|->"   "\\mapsto"
    "|="    "\\models"
    "||"    "\\mid"
    "~="    "\\approx"
    "~~"    "\\sim"
    ";0"  "\\emptyset"
    ";8"  "\\infty"
    ";!"  "\\neg"
    ";^"  "\\uparrow"
    ";&"  "\\wedge"
    ";~"  "\\approx"        ";;~" "\\simeq"
    ";_"  "\\downarrow"
    ";+"  "\\cup"
    ";-"  "\\leftrightarrow"";;-" "\\longleftrightarrow"
    ";*"  "\\times"         ";;*" "\\otimes"
    ";/"  "\\not"
    ";|"  "\\mapsto"        ";;|" "\\longmapsto"
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
  '(:cond laas-object-on-left-condition
    :expansion-desc "Wrap object on the left with \\frac{}{}, leave `point' in the denuminator."
    "/" laas-smart-fraction)
  "Frac snippet. Expand in maths when there's something to frac on on the left.")


(defvar laas-accent-snippets
  `(:cond laas-object-on-left-condition
    ,@(cl-loop for (key exp) in '(("'. " "dot")
                                  ("'.." "ddot")
                                  ("'v " "vec")
                                  ("'u"  "breve")
                                  ("'~" "tilde")
                                  ("'h" "hat")
                                  ("'-" "overline")
                                  ("'_" "underline")
                                  ("'s" "mathscr")
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

(defcustom laas-enable-auto-space t
  "If non-nil, hook intelligent space insertion onto snippet expansion."
  :type 'boolean
  :group 'laas)

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
                  nil 'local)
        (when laas-enable-auto-space
          (add-hook 'aas-post-snippet-expand-hook
                    #'laas-current-snippet-insert-post-space-if-wanted
                    nil 'local)))
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
