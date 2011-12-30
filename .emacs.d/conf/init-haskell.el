;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; haskell-mode
;; http://www.haskell.org/haskell-mode/
                                        ;(add-to-load-path "~/.emacs.d/elisp/haskell-mode/")
(load "haskell-site-file" t)
(when (fboundp 'haskell-program-name)
  (setq haskell-program-name "/usr/bin/ghci")
  (setq auto-mode-alist
        (append auto-mode-alist
                '(("\\.[hg]s$"  . haskell-mode)
                  ("\\.hi$"     . haskell-mode)
                  ("\\.l[hg]s$" . literate-haskell-mode))))
  (autoload 'haskell-mode "haskell-mode"
    "Major mode for editing Haskell scripts." t)
  (autoload 'literate-haskell-mode "haskell-mode"
    "Major mode for editing literate Haskell scripts." t)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)

  (setq haskell-literate-default 'latex)
  (setq haskell-doc-idle-delay 0)
  )
