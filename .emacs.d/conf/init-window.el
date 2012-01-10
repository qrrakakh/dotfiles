;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Window環境用の設定
(menu-bar-mode -1)

(cond (window-system
       ;; ツールバー削除
       (tool-bar-mode -1)
  
       ;; disable scroll-bar
       (set-scroll-bar-mode nil)

       ;; Title bar customize
       (setq frame-title-format (format "%%b - Emacs@%s" (system-name)))
       
       ;; shell-mode でエスケープを綺麗に表示
       (autoload 'ansi-color-for-comint-mode-on "ansi-color"
         "Set `ansi-color-for-comint-mode' to t." t)
       (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

       (setq initial-frame-alist
             ;;(append '((font . "VL Gothic:style=Regular:size=16") 
             (append '((font . "Ricty:style=Regular:size=16") 
                       (width . 80)
                       (height . 35))
                     initial-frame-alist))
       (setq default-frame-alist initial-frame-alist)

       
       (add-to-list 'default-frame-alist '(alpha . 95))

       ;; (cond ( (string-match "^23\." emacs-version)
       ;;         (set-default-font "VL Gothic-11")
       ;;          (set-fontset-font (frame-parameter nil 'font)
       ;;                            'japanese-jisx0208
       ;;                            '("IPA Gothic" . "unicode-bmp"))
       ;;         ))
       
       ))
