;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Meadow用設定集
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 言語系

;; Windows IME
(mw32-ime-initialize)
(setq default-input-method "MW32-IME")
(set-keyboard-coding-system 'cp932)

;; IME ON/OFF mode-line
(setq mw32-ime-show-mode-line t)
;;; OFF : [--]
;;; ON  : [あ]
(setq-default mw32-ime-mode-line-state-indicator "[--]")
(setq mw32-ime-mode-line-state-indicator "[--]")
(setq mw32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 外観設定

;; load ttfont-setup.el
(when (require 'ttfont-setup nil t)
  (ttfont-setup))

;; transparency
(modify-all-frames-parameters (list (cons 'alpha '(97 50 50 30))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; シェル設定 (主にcygwinのbash用)

;; cygwinのpathを有効にする
(require 'mw32script)
(mw32script-init)
(require 'cygwin-mount)
(cygwin-mount-activate)

;; shell-mode の設定 (bash)
;; Cygwinのbashを利用 
(setq explicit-shell-file-name (concat (getenv "CYGWIN_PATH") "/bin/bash.exe"))
(setq shell-file-name (concat (getenv "CYGWIN_PATH") "/bin/sh.exe"))
(setq shell-command-switch "-c")

;; xdviに渡すパスがCygwinのものになるようなShellScriptを用意
(setq dvi2-command "/usr/local/bin/xdvi-win2cyg.sh")

(if 1 () 
  (setq tex-command "platex --kanji=utf8")
                                        ; tex の実行コマンド
  (setq dviprint-command-format "/usr/local/teTex/bin/dvipdfmx %s")
  (setq scheme-program-name "/usr/local/gauche/bin/gosh -i")
  (setq bibtex-command "/usr/local/teTex/bin/jbibtex")
  )
;; shell-modeでの補完 (for drive letter)
(setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@`'.,:()-")

;; shell-modeでエコー回避
(add-hook 'comint-mode-hook 
          (lambda () 
            (setq comint-process-echoes t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

