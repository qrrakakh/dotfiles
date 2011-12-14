;;; ff++-mode.el --- FreeFem++ Programming Language mode for (X)Emacs
;;;               Requires a cc-mode of version 5.30 or greater

;; Author:     2008 Rafael Rodríguez Galván
;; Maintainer: Rafael Rodríguez Galván
;; Created:    September 2008
;; Version:    0.1
;; Based on:   D programming language mode for (X)Emacs, by William Baxter
;; Keywords:   FreeFem++ finite element programming language emacs cc-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Usage: 
;; Put these lines in your .emacs startup file.
;;  (autoload 'ff++-mode "ff++-mode" "Major mode for editing FreeFem++ code." t)
;;  (add-to-list 'auto-mode-alist '("\\.edp$" . ff++-mode))
;;
;; cc-mode version 5.30 or greater is required.
;; You can check your cc-mode with the command M-x c-version.
;; You can get the latest version of cc-mode at http://cc-mode.sourceforge.net
;;
;;
;; TODO:
;;   * Assure all FreeFem++ keywords are highlighted.
;;   * ¿Use client/server scheme for communicating emacs/freefem++?
;;
;; History:
;;   * 2008 September 23 - Released version 0.2
;;      Changelog:
;;        - Set up of comment/uncomment region menu entries.
;;        - Added syntax highligting for more keywords.
;;        - Run buffer function was improved: 
;;            + After running, window is split into two frames and FreeFem++ 
;;              output is displayed in the bottom one.
;;            + If FreeFem++ returned with an error: the error line is 
;;              highlighted in output buffer; in code buffer, cursor goes to 
;;              error line and error keyword is highlighted
;;   * 2008 September 9 - Released version 0.1
;;      Changelog:
;;        - First version: Syntax higlighting of several keywords, 
;;          FreeFem++ menú, possibility of running FreFem++ code buffer
;;----------------------------------------------------------------------------
;; Code:

(require 'cc-mode)
(require 'hi-lock)

;; These are only required at compile time to get the sources for the
;; language constants.  (The cc-fonts require and the font-lock
;; related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)
;; Coment out 'when-compile part for debugging
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts)
  )

(eval-and-compile
  ;; Make our mode known to the language constant system.  Use Java
  ;; mode as the fallback for the constants we don't change here.
  ;; This needs to be done also at compile time since the language
  ;; constants are evaluated then.
  (c-add-language 'ff++-mode 'java-mode))

;;----------------------------------------------------------------------------

;; Built-in basic types
(c-lang-defconst c-primitive-type-kwds
  ff++ '("bool" "border" "Cmatrix" "complex" "element" "fespace"
	 "func" "ifstream" "int" "macro" "matrix" "mesh" "ofstream" "problem"
	 "real" "R3" "solve" "string" "varf" "vertex" ))


;; Keywords that can prefix normal declarations of identifiers
(c-lang-defconst c-modifier-kwds 
  ff++  nil)

(c-lang-defconst c-class-decl-kwds
  ;; Keywords introducing declarations where the following block (if any)
  ;; contains another declaration level that should be considered a class.
  ff++  nil)

(c-lang-defconst c-brace-list-decl-kwds
  ff++  nil)

(c-lang-defconst c-type-modifier-kwds
  ff++  nil)

(c-lang-defconst c-type-prefix-kwds
  ;; Keywords where the following name - if any - is a type name, and
  ;; where the keyword together with the symbol works as a type in
  ;; declarations.  In this case, like "mixin foo!(x) bar;"
  ff++  nil)

(c-lang-defconst c-typedef-decl-kwds
  ff++  nil)

(c-lang-defconst c-decl-hangon-kwds
  ff++  nil)

(c-lang-defconst c-protection-kwds
  ;; Access protection label keywords in classes.
  ff++  nil)

(c-lang-defconst c-type-list-kwds
  ff++  nil)

(c-lang-defconst c-ref-list-kwds
  ff++  nil)

(c-lang-defconst c-colon-type-list-kwds
  ;; Keywords that may be followed (not necessarily directly) by a colon
  ;; and then a comma separated list of type identifiers.
  ff++  nil)

(c-lang-defconst c-paren-nontype-kwds
  ;;Keywords that may be followed by a parenthesis expression that doesn't
  ;; contain type identifiers.
  ff++  nil)

(c-lang-defconst c-paren-type-kwds
  ;; Keywords that may be followed by a parenthesis expression containing
  ;; type identifiers separated by arbitrary tokens.
  ff++  nil)

(c-lang-defconst c-block-stmt-1-kwds
  ;; Statement keywords followed directly by a substatement.
  ;; 'static' is there for the "else static if (...) {}" usage.
  ff++ '("else" "try"))

(c-lang-defconst c-block-stmt-2-kwds
  ;; Statement keywords followed by a paren sexp and then by a substatement.
  ff++ '("catch" "for" "if" "while"))

(c-lang-defconst c-simple-stmt-kwds
  ;; Statement keywords followed by an expression or nothing.
  ff++ '("break" "continue" "return" "throw"))

(c-lang-defconst c-paren-stmt-kwds
  ;; Statement keywords followed by a parenthesis expression that
  ;; nevertheless contains a list separated with ';' and not ','."
  ff++ '("adaptmesh" "assert" "buildmesh"  "clock" "convect" 
         "dx" "dy" "intalledges" "int1d" "int2d" "jump" "mean"
         "movemesh" "on" "plot" "savemesh" "square" 
         "sin" "cos" "tan" "atan" "asin" "acos"
         "cotan" "sinh" "cosh" "tanh""cotanh"
         "exp" "log" "log10" "sqrt"
         "abs" "max" "min"))

(c-lang-defconst c-asm-stmt-kwds
  ;; Statement keywords followed by an assembler expression.
  ff++ nil)

(c-lang-defconst c-label-kwds
  ;; Keywords introducing colon terminated labels in blocks.
  ff++ nil)

(c-lang-defconst c-before-label-kwds
  ;; Keywords that might be followed by a label identifier.
  ff++ nil)

(c-lang-defconst c-constant-kwds
  ;; Keywords for constants.
  ff++ '("false" "i" "pi" "true"
         "anisomax" "area" "CG" "cin" "cout"  "endl" "eps"
         "hTriangle""init" "label" 
         "lenEdge" "N"  "nTonEdge" "nuTriangle" "nuEdge" 
         "precon""region" "split" "solver" "strategy" 
         "tgv" "tolpivot" "tolpivotsym"
         "x" "y" "z"
					; Finite elements
         "P" "P0" "P1" "P2" "RT0" "P1nc" "P1dc" "P2dc" "P1b"
					; Solvers
         "LU" "Cholesky" "Crout" "CG" "GMRES" "UMFPACK" "sparsesolver"
					; Quadrature keywords
         "qfe" "qforder" "qf1pE" "qf2pE" "qf3pE" "qf4pE" "qf5pE" "qf1pElump"
         "qf1pT" "qf2pT" "qf5pT" "qf1pTlump" "qf2pT4P1" "qf7pT" "qf9pT"
					; Plot options
         "fill" "wait"
					; Adaptmesh
         "hmax" "hmin" "err" "errg" "nbvx" "nbsmooth" "nbjacobi"
         "ratio" "omega" "iso" "abserror" "cutoff" "verbosity" 
         "inquire" "splitpbedge" "maxsubdiv" "rescaling" "keepbackvertices"
         "isMetric" "power" "thetamax" "splitin2" "metric"
         "nomeshgeneration" "periodic"
	 ))

(c-lang-defconst c-primary-expr-kwds
  ;; Keywords besides constants and operators that start primary expressions.
  ff++ nil)

(c-lang-defconst c-inexpr-class-kwds
  ;; Keywords that can start classes inside expressions.
  ff++    nil)

(c-lang-defconst c-inexpr-brace-list-kwds
  ;; Keywords that can start brace list blocks inside expressions.
  ff++    nil)

(c-lang-defconst c-other-decl-kwds
  P0 P1
  ff++ nil)

(c-lang-defconst c-other-kwds
  ;; Keywords not accounted for by any other `*-kwds' language constant.
  ff++ nil )

;; (defcustom ff++-font-lock-extra-types nil
;;   "*List of extra types (aside from the type keywords) to recognize in FreeFem++ mode. 
;;    Each list item should be a regexp matching a single identifier."
;;   )

(defconst ff++-font-lock-keywords-1 (c-lang-const c-matchers-1 ff++)
  "Minimal highlighting for FreeFem++ mode.")

(defconst ff++-font-lock-keywords-2 (c-lang-const c-matchers-2 ff++)
  "Fast normal highlighting for FreeFem++ mode.")

(defconst ff++-font-lock-keywords-3 (c-lang-const c-matchers-3 ff++)
  "Accurate normal highlighting for FreeFem++ mode.")

(defvar ff++-font-lock-keywords ff++-font-lock-keywords-3
  "Default expressions to highlight in FreeFem++ mode.")

(defvar ff++-mode-syntax-table nil
  "Syntax table used in ff++-mode buffers.")
(or ff++-mode-syntax-table
    (setq ff++-mode-syntax-table
	  (let ((table (funcall (c-lang-const c-make-mode-syntax-table ff++))))
	    ;; Make it recognize D `backquote strings`
					;(modify-syntax-entry ?` "\"" table)
	    ;; Make it recognize D's nested /+ +/ comments 
	    ;; You'll definitely need an elisp manual for this:
	    ;; http://www.delorie.com/gnu/docs/elisp-manual-21/
					;(modify-syntax-entry ?+  ". 23n"   table)
	    table)))

(defvar ff++-mode-abbrev-table nil
  "Abbreviation table used in ff++-mode buffers.")
(c-define-abbrev-table 'ff++-mode-abbrev-table
  ;; Use the abbrevs table to trigger indentation actions 
  ;; on keywords that, if they occur first on a line, might alter the
  ;; syntactic context.
  ;; Syntax for abbrevs is:
  ;; ( pattern replacement command initial-count)
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)
    ("finally" "finally" c-electric-continued-statement 0)))


(defvar freefempp-program "FreeFem++")
(defvar freefempp-output-buffer "*FreeFem++ Output*")

(defun freefempp-run-buffer() 
  "Send current buffer to FreeFem++."
  (interactive)
  (save-some-buffers)
  (let 
      ((freefempp-command 
	(concatenate 'string freefempp-program " " buffer-file-name))
       (freefempp-code-buffer (file-name-nondirectory buffer-file-name)))
					;     (freefempp-code-buffer (buffer-true-name)))
    ;; Clean freefempp-output-buffer, split window and change to it
    (if (get-buffer freefempp-output-buffer)
        (kill-buffer freefempp-output-buffer))
    (switch-to-buffer-other-window freefempp-output-buffer)
    ;; Send code to FreeFem++, saving the return value(0 = no error)
    (let 
	((freefempp-return-value
	  (call-process-shell-command freefempp-command
				      nil (get-buffer-create freefempp-output-buffer) t))
	 (freefempp-error-info (list 0 nil)))
      (cond ((/= freefempp-return-value 0)
	     ;; Save information about error (error-line, invalid-token)
	     (setq freefempp-error-info (freefempp-get-error-info))
	     ;; Highlight error line in output buffer
	     (freefempp-output-buffer-show-error freefempp-error-info)))
      ;; Switch to code buffer. If error, go to error line and highlight wrong token
      (switch-to-buffer-other-window freefempp-code-buffer)
      (freefempp-code-buffer-show-error freefempp-error-info)))
  )

(defun freefempp-get-error-info () 
  "Look at FreeFem++ output and return a list (err-line, err-string),
  where err-line is the error line (or 0 if no information about error
  line was detected) and err-string is the information presented by
  FreeFem++ (usually, a wrong token)."
  (let ((line-number "0") (wrong-toke nil))
    (save-excursion 
       (cond ((search-backward-regexp
              "line number :\\([0-9]*\\),[ ]*\\([a-zA-Z0-9]*\\)" nil t)
              (setq line-number (match-string 1)
                    wrong-token (match-string 2)))))
    (list (string-to-number line-number) wrong-token))
)

(defun freefempp-output-buffer-show-error(freefempp-error-info)
  "If an error was detected, set point in error line in FreeFem++ output"
  (let ((freefempp-error-line (car freefempp-error-info)))
    (cond ((> freefempp-error-line 0)
     (setq freefempp-error-regexp 
       (concatenate 'string "^[ \t]*" (number-to-string freefempp-error-line) " :"))
;;     (insert ">>>" freefempp-error-regexp "<<<")  
     (search-backward-regexp freefempp-error-regexp nil t)
     (freefempp-remove-hi-lock)
     (hi-lock-line-face-buffer freefempp-error-regexp)))))
   
(defun freefempp-code-buffer-show-error(freefempp-error-info)
  "If an error was detected, set point in error line an highligt the wrong token"
  (let ((freefempp-error-line (car freefempp-error-info))
        (freefempp-invalid-token (car (cdr freefempp-error-info))))
    (cond ((> freefempp-error-line 0)
      (freefempp-remove-hi-lock)
      (goto-line freefempp-error-line) 
      (highlight-regexp freefempp-invalid-token)))))

(defun freefempp-remove-hi-lock ()
  "Redraw highlighted expressions, using default face"
  (while hi-lock-interactive-patterns
    (hi-lock-unface-buffer (car(car hi-lock-interactive-patterns)))))

(defvar ffpp-mode-map ()
  "Keymap used in ff++-mode buffers.")
(if ffpp-mode-map
    nil
  (setq ffpp-mode-map (c-make-inherited-keymap))
  ;; Add bindings which are only useful for FreeFem++
  (define-key ffpp-mode-map "\C-c\C-c"  'freefempp-run-buffer)
  )

(c-lang-defconst c-mode-menu
  ;; The definition for the mode menu.  The menu title is prepended to
  ;; this before it's fed to `easy-menu-define'.
  t `(["Run buffer"     freefempp-run-buffer t]
      ["Kill FreeFem++ process" keyboard-quit t]
      "---"
      ["Comment Out Region"     comment-dwim
       (c-fn-region-is-active-p)]
      ["Uncomment Region"       comment-dwim
       (c-fn-region-is-active-p)]
      ["Indent Expression"      c-indent-exp
       (memq (char-after) '(?\( ?\[ ?\{))]
      ["Indent Line or Region"  c-indent-line-or-region t]
      ["Fill Comment Paragraph" c-fill-paragraph t]
      "----"
      ["Backward Statement"     c-beginning-of-statement t]
      ["Forward Statement"      c-end-of-statement t]
      "----"
      ("Toggle..."
       ["Syntactic indentation" c-toggle-syntactic-indentation
	:style toggle :selected c-syntactic-indentation]
       ["Electric mode"         c-toggle-electric-state
	:style toggle :selected c-electric-flag]
       ["Auto newline"          c-toggle-auto-newline
	:style toggle :selected c-auto-newline]
       ["Hungry delete"         c-toggle-hungry-state
	:style toggle :selected c-hungry-delete-key]
       ["Subword mode"          c-subword-mode
	:style toggle :selected (and (boundp 'c-subword-mode)
				     c-subword-mode)])))

(easy-menu-define ff++-menu ffpp-mode-map "FreeFem++ Mode Commands"
  (cons "FreeFem++" (c-lang-const c-mode-menu ff++)))


;;----------------------------------------------------------------------------

;;;###autoload
(defun ff++-mode ()
  "Major mode for editing code written in the FreeFem++ Programming Language.
See http://www.freefem.org/ff++/ for more information about the FreeFem++ language.
The hook `c-mode-common-hook' is run with no args at mode
initialization, then `ff++-mode-hook'.

Key bindings:
\\{ffpp-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table ff++-mode-syntax-table)
  (setq major-mode 'ff++-mode
	mode-name "FreeFem++"
	local-abbrev-table ff++-mode-abbrev-table
	abbrev-mode t)

  (make-local-variable 'comment-start)
  (setq comment-end   "")
  (make-local-variable 'comment-end)
  (setq comment-start "// ")
  (use-local-map ffpp-mode-map)
  (c-init-language-vars ff++-mode)
  (c-common-init 'ff++-mode)
  (easy-menu-add ff++-menu)
  (c-run-mode-hooks 'c-mode-common-hook 'ff++-mode-hook)
  (c-update-modeline))

(provide 'ff++-mode)

;;; ff++-mode.el ends here
