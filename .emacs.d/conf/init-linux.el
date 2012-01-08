(cond (window-system
       ;; iBus/Mozc
       (when (require 'ibus nil t)
         ;; Turn on ibus-mode automatically after loading .emacs
         (add-hook 'after-init-hook 'ibus-mode-on)
         ;; key settings
         (ibus-define-common-key ?\C-\s nil)
         (ibus-define-common-key ?\C-/ nil)
         (setq ibus-cursor-color '("limegreen" "white" "blue"))
         (global-set-key "\C-\\" 'ibus-toggle)
         (global-set-key "\M-`" 'ibus-toggle)
         )
       ;; sync kill-ring with clipboard on X
       (setq x-select-enable-clipboard t)

       (setq browse-url-browser-function 'browse-url-generic
             browse-url-generic-program "firefox")
       )
      (;; Anthy
       ;; (require 'anthy)
       ;; (setq default-input-method "japanese-anthy")
       ;; (setq anthy-accept-timeout 1)

       ;; (anthy-load-hiragana-map anthy-alt-char-map)
       ;; (setq anthy-wide-space " ")
       ;; (anthy-change-hiragana-map "0" "0")
       ;; (anthy-change-hiragana-map "1" "1")
       ;; (anthy-change-hiragana-map "2" "2")
       ;; (anthy-change-hiragana-map "3" "3")
       ;; (anthy-change-hiragana-map "4" "4")
       ;; (anthy-change-hiragana-map "5" "5")
       ;; (anthy-change-hiragana-map "6" "6")
       ;; (anthy-change-hiragana-map "7" "7")
       ;; (anthy-change-hiragana-map "8" "8")
       ;; (anthy-change-hiragana-map "9" "9")
       ;; (global-set-key "\C-\\" 'anthy-mode)
       ;; (global-set-key "\M-`" 'anthy-mode)
       ))
