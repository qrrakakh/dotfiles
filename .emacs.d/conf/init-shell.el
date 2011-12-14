;;; Shell setting

;; display the escape in shell-mode
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)

;; auto-fill from the history with up-down keys
(add-hook 'shell-mode-hook
          (function (lambda ()
                      (define-key shell-mode-map [up] 'comint-previous-input)
                      (define-key shell-mode-map [down] 'comint-next-input)
                      (ansi-color-for-comint-mode-on))))
          
