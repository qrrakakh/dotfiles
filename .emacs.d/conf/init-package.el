(when (require 'package nil t)
  (add-to-list 'package-archives '
               ("gnu" . "http://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '
               ("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '
               ("marmalade" . "http://marmalade-repo.org/packages/"))
  (setq package-user-dir "~/local/lib/emacs/package/")
  (package-initialize)
  )
