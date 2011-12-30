;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; for ESS-R
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ess-site)
(setq ess-ask-for-ess-directory nil)
(setq ess-pre-run-hook
      '((lambda ()
	  (setq default-process-coding-system '(utf-8 . utf-8))
	  )))
