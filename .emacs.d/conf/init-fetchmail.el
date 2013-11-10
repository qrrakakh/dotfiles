;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; fetchmail を Emacs から使用する

(autoload 'fetchmail "fetchmail" nil t)
(setq fetchmail-server-omit-passwd-list '("pop.gmail.com"))
(setq fetchmail-default-server "pop.gmail.com")
(setq fetchmail-server-param-alist '())

;; キーバインド
(autoload 'fetchmail "fetchmail" nil t)
(global-set-key "\C-xf" 'fetchmail)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
