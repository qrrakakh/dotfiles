;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Settings
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-%" 'query-replace-regexp)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\M-h" 'help-for-help)
(global-set-key "\C-x\C-a" 'anything)
(global-unset-key "\C-o") ; for GNU screen
(global-set-key "\M-j" 'edebug-eval-print-last-sexp)

(global-set-key "\C-\M-s" 'isearch-forward-regexp)
(global-set-key "\C-\M-S" 'isearch-backward-regexp)

(global-set-key "\C-r" 'replace-string)
(global-set-key "\C-\M-r" 'replace-regexp)

(global-set-key "\C-xn" 'new-frame)
(global-set-key "\C-xO" 'other-frame)
(global-set-key "\C-xK" 'kill-buffer-and-window)

(global-set-key "\C-xE" 'flymake-display-err-menu-for-current-line)

;; Let auto-indent when enter-key typed (same as the prev. line)
;(global-set-key "\C-m" 'newline-and-indent)
(setq indent-line-function 'indent-relative-maybe)

;; revert buffer
(defun revert-buffer-force ()
  (interactive)
  (revert-buffer t t))
;;(define-key global-map "\C-c\C-x\C-j" 'revert-buffer-force)
;;(define-key global-map "\C-x\C-j" 'revert-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
