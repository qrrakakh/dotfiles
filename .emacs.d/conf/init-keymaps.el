;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Settings
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-%" 'query-replace-regexp)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\M-h" 'help-for-help)
(global-set-key "\C-xa" 'anything)
(global-set-key "\C-j" 'nil)

(global-set-key "\C-\M-s" 'isearch-forward-regexp)
(global-set-key "\C-\M-S" 'isearch-backward-regexp)

(global-set-key "\C-r" 'replace-string)
(global-set-key "\C-\M-r" 'replace-regexp)
(global-set-key "\C-j" 'nil)

(global-set-key "\C-xn" 'new-frame)
(global-set-key "\C-xO" 'other-frame)
(global-set-key "\C-xK" 'kill-buffer-and-window)

;; Let auto-indent when enter-key typed (same as the prev. line)
(global-set-key "\C-m" 'newline-and-indent)
(setq indent-line-function 'indent-relative-maybe)

;; revert buffer
(defun revert-buffer-force ()
  (interactive)
  (revert-buffer t t))
;;(define-key global-map "\C-c\C-x\C-j" 'revert-buffer-force)
;;(define-key global-map "\C-x\C-j" 'revert-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
