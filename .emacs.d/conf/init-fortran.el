(add-hook 'f90-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (setq f90-beginning-ampersand t)
             (setq f90-font-lock-keywords f90-font-lock-keywords-3)
             (setq fill-column 132)
             (auto-fill-mode 1)
             (abbrev-mode 1)
             (turn-on-font-lock)))
