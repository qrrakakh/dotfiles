(when (require 'scala-mode-auto nil t)
  (add-to-list 'auto-mode-alist
               '("\\.scala$" . scala-mode))
  (defvar my-scala-current-buffer nil)

  (defun my-scala-switch-to-interpreter ()
    (interactive)
    (setq my-scala-switch-to-interpreter (current-buffer))
    (unless (scala-interpreter-running-p-1)
      (scala-run-scala "scala"))
    (scala-switch-to-interpreter))

  (defun my-scala-back-to-editing-buffer ()
    (interactive)
    (switch-to-buffer my-scala-current-buffer))
  (add-hook 'scala-mode-hook
            (lambda ()
              (define-key scala-mode-map "\C-c\C-z" 'my-scala-switch-to-interpreter)
              (define-key scala-mode-inf-map "\C-c\C-z" 'my-scala-back-to-editing-buffer)
              ))
  )
