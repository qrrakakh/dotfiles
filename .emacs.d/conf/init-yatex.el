;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YaTeX

(when (locate-library "yatex")
  (autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)

  (setq auto-mode-alist (cons '("\\.tex$" . yatex-mode) auto-mode-alist))
  (setq YaTeX-use-AMS-LaTeX t)
  (setq YaTeX-kanji-code 4)
  (setq tex-command "pdfplatex")
  (setq dvi2-command "evince")
  (setq YaTeX-dvi2-command-ext-alist
        '(("evince\\|pdf\\|Preview\\|TeXShop" . ".pdf")
          ));
  (setq bibtex-command "pbibtex -kanji=utf8")
  (setq section-name "documentclass")
  (setq YaTeX-nervous nil)
  (setq dviprint-command-format "dvipdfmx %s")
  ;;数式モードの";"補間の強化
  (setq YaTeX-math-sign-alist-private
        '(("q"         "quad"          "__")
          ("qq"        "qquad"         "____")
          ("ls"        "varlimsup"     "___\nlim")
          ("li"        "varliminf"     "lim\n---")
          ("il"        "varinjlim"     "lim\n-->")
          ("st"        "text{ s.~t. }" "s.t.")
          ("bigop"     "bigoplus"      "_\n(+)~")
          ("bigot"     "bigotimes"     "_\n(x)\n ~")
          ("pl"        "varprojlim"    "lim\n<--")
          ("di"        "mathrm{d}"    "d (roman)")
          ))
  ;;数式モードの","補間
  (setq YaTeX-math-funcs-list
        '(("s"	"sin"           "sin")
          ("c"  "cos"          "cos") 
          ("t"  "tan"           "tan")
          ("hs"	"sinh"          "sinh")
          ("hc"   "cosh"          "cosh")
          ("ht"   "tanh"          "tanh")
          ("S"	"arcsin"        "arcsin")
          ("C"    "arccos"        "arccos")
          ("T"    "arctan"        "arctan")
          ("se"   "sec"           "sec")
          ("cs"   "csc"           "csc")
          ("cot"  "cot"           "cot")
          ("l"    "ln"            "ln")
          ("L"    "log"           "log")
          ("e"    "exp"           "exp")
          ("M"    "max"           "max")
          ("m"    "min"           "min")
          ("su"   "sup"           "sup")
          ("in"   "inf"           "inf")
          ("di"   "dim"           "dim")
          ("de"   "det"           "det")
          ("i"    "imath"         "i")
          ("j"    "jmath"         "j")
          ("I"    "Im"            "Im")
          ("R"    "Re"            "Re")
          ))
  (setq YaTeX-math-key-list-private
        '(("," . YaTeX-math-funcs-list)
          ))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
