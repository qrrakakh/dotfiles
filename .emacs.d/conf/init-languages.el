;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 言語系

(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
;;(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8-unix)
(add-hook 'shell-mode-hook
	  (lambda ()
	    (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
