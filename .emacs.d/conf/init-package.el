(when (require 'package nil t)
  (add-to-list 'package-archives '
               ("marmalade" . "http://marmalade-repo.org/packages/"))
  (setq package-user-dir "~/local/lib/emacs/package/")
  (package-initialize)
  )
