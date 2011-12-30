(when (require 'anything nil t)
  (require 'anything-config)
  (defvar org-direcotry "")
  (global-set-key [?\C-,] 'anything)
  (setq anything-sources
        (list anything-c-source-buffers
              anything-c-source-google-suggest
              anything-c-source-bookmarks
              anything-c-source-file-name-history
              anything-c-source-man-pages
              anything-c-source-emacs-commands
              anything-c-source-emacs-functions
              anything-c-source-info-pages
              anything-c-source-calculation-result
              anything-c-source-locate))
  )
