;;; init-completion.el --- 

(setq completion-ignored-extensions
      (append completion-ignored-extensions
              '("#")))
(defadvice completion-file-name-table (after ignoring-backups-f-n-completion activate)
    "filter out results when the have completion-ignored-extensions"
      (let ((res ad-return-value))
            (if (and (listp res)
                               (stringp (car res))
                                         (cdr res)) ; length > 1, don't ignore sole match
                        (setq ad-return-value
                                        (completion-pcm--filename-try-filter res)))))
(eval-after-load "dired"
    '(require 'dired-x))
(add-hook 'dired-mode-hook
                    (lambda ()
                                  (dired-omit-mode 1)))
