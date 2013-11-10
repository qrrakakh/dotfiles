;; Strength corresponded parenthesis
(show-paren-mode t)

;; Color Theme

(cond
 ((require 'color-theme nil t)
  (color-theme-initialize)
  (color-theme-charcoal-black)
  ) 
 
 ( (string-match "^24\." emacs-version)
        (custom-set-variables
         '(custom-enabled-themes (quote (wombat))))
        (custom-set-faces
         )
        ))
