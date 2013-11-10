(cond
 ((require 'server nil t)
  (unless (server-running-p)
    (server-start))))

(cond 
 ((require 'ibus nil t)
  (
   ;; Turn on ibus-mode automatically after loading .emacs
   (setq default-input-method "ibus")
   (add-hook 'after-init-hook 'ibus-mode-on)
   ;; key settings
   (ibus-define-common-key ?\C-\s nil)
   (ibus-define-common-key ?\C-/ nil)
   (setq ibus-cursor-color '("limegreen" "white" "blue"))
   (global-set-key "\C-\\" 'ibus-toggle)
   (global-set-key "\M-`" 'ibus-toggle)
   ))
 ((require 'mozc nil t)
  (setq default-input-method "japanese-mozc")   
  ))

(cond (window-system
       ;; sync kill-ring with clipboard on X
       (setq x-select-enable-clipboard t)
       (setq browse-url-browser-function 'browse-url-generic
             browse-url-generic-program "firefox")
       )
      )

