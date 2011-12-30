;; Color Theme
(when (require 'color-theme nil t)
  (color-theme-initialize)
  (color-theme-charcoal-black)
  ;(color-theme-blue-sea)
)  

;; disable comment-italicize
(set-face-italic-p 'font-lock-comment-face nil) 

;; Strength corresponded parenthesis
(show-paren-mode t)

