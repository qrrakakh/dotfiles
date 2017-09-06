(add-to-list 'exec-path (expand-file-name "/home/qrrakakh/local/go/bin"))


(when (require 'go-mode nil t)
  (add-hook 'go-mode-hook (lambda()
                            (local-set-key (kbd "M-.") 'godef-jump)
                            (setq indent-tabs-mode nil)    ; タブを利用
                            (setq c-basic-offset 4)        ; tabサイズを4にする
                            (setq tab-width 4)))
    )
