;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Miscellaneous

;; set indent size
(setq tab-width 4)

;; disable backup
(setq make-backup-files nil)

;; autosave
;;(setq auto-save-default nil)
(setq auto-save-file-name-transforms
      `((".*/Dropbox/.*" ,temporary-file-directory t)))
(setq auto-save-timeout 30) 
(setq auto-save-interval 100)
(setq delete-auto-save-files t)


;; always font-lock-mode enabled
(global-font-lock-mode t)
;;  (setq font-lock-support-mode 'jit-lock-mode)

;; display line and column number on the modeline
(line-number-mode t) 
(column-number-mode t)

;; display where the function are we on the modeline
(which-function-mode t)

;; highlight the line on which the cursor is
;;(setq hl-line-face 'underline)
(setq hl-line-face 'nil)
(global-hl-line-mode t)

;; disable blinking cursor
(blink-cursor-mode 0)

;; hide cursors on non-active window
(setq default-cursor-in-non-selected-windows nil)

;; display time
(display-time)
(setq display-time-24hr-format t)
(setq display-time-string-forms '(24-hours ":" minutes))

;; modeline format configure
(setq-default mode-line-format
              '("-" mode-line-mule-info mode-line-modified
                mode-line-frame-identification
                mode-line-buffer-identification " " global-mode-string
                " %[(" mode-name mode-line-process minor-mode-alist
                "%n" ")%]-" (which-func-mode ("" which-func-format "-"))
                (line-number-mode "L%l-") (column-number-mode "C%c-")
                (-3 . "%p") "-%-"))
(setq mode-line-frame-identification " ")

;; disable truncate 
(setq truncate-lines nil)
(setq truncate-partial-width-windows t)

;; enable to choose yes/no by y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; delete the EOL break withkill-line
(setq kill-whole-line t)

;; delete region with \C-h
(delete-selection-mode 1)

;; insert indent with the space
(setq-default indent-tabs-mode nil)

;; hilight the corresponding parenthesis
(setq show-paren-mode t)

;; requiring the new line on the EOF
(setq require-final-newline t)

;; disable inserting new lines on the EOF
(setq next-line-add-newlines nil)

;; hilight the region
(setq transient-mark-mode t)

;; linum-mode: show line number

(when
    (require 'linum nil t)
  (setq linum-format "%5d|")
  (dolist (linum-hook (list
                       'c-mode-hook
                       'emacs-lisp-mode-hook
                       'lisp-interaction-mode-hook
                       'lisp-mode-hook
                       'java-mode-hook
                       'sh-mode-hook
                       ))
    (add-hook linum-hook (lambda () (linum-mode t))))
  (global-linum-mode)
  )
;; Auto complete
(when (require 'auto-complete nil t)
  (require 'auto-complete-config)
  (global-auto-complete-mode t)
  )

;; Let Lambda on LISP cute
(when (require 'pretty-lambdada nil t)
  (global-pretty-lambda-mode t)
  )

;; uniquify
(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*")
  )

;; autoinsert
(when (require 'autoinsert nil t)
  (setq auto-insert-directory "~/.emacs.d/template/")
  (setq auto-insert-alist
        (append '(("\\.cc$" . "template.cc")
                  ("\\.c$" . "template.c")
                  ("\\.cpp" . "template.cc")
                  ("\\.py" . "template.py")
                  ("\\.tex" . "template.tex")
                  ("\\.rb" . "template.rb")
                  ("[Mm]akefile" . "Makefile.template"))
                auto-insert-alist))
  (add-hook 'find-file-hooks 'auto-insert)
  (setq auto-insert-query nil)
  )

;; highlight-indentation
(when (require 'highlight-indentation nil)
  t)
