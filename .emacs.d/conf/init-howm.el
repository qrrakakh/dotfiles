(require 'howm)
(setq howm-menu-lang 'ja)
(global-set-key "\C-c,," 'howm-menu)

;;(put 'downcase-region 'disabled nil)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(howm-directory "~/Dropbox/howm/")
 '(howm-history-file "~/Dropbox/.howm-history")
 '(howm-keyword-file "~/Dropbox/.howm-keys"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(mapc
 (lambda (f)
   (autoload f
     "howm" "Hitori Otegaru Wiki Modoki" t))
 '(howm-menu howm-list-all howm-list-recent
             howm-list-gerp howm-create
             howm-keyword-to-kill-ring))

;; リンクを TAB で辿る
(eval-after-load "howm-mode"
  '(progn
     (define-key howm-mode-map [tab] 'action-lock-goto-next-link)
     (define-key howm-mode-map [(meta tab)] 'action-lock-goto-previous-link)))
;; 「最近のメモ」一覧時にタイトル表示
(setq howm-list-recent-title t)
;; 全メモ一覧時にタイトル表示
(setq howm-list-all-title t)
;; メニューを 2 時間キャッシュ
(setq howm-menu-expiry-hours 2)

;; howm の時は auto-fill で
(add-hook 'howm-mode-on-hook 'auto-fill-mode)

;; RET でファイルを開く際, 一覧バッファを消す
;; C-u RET なら残る
(setq howm-view-summary-persistent nil)

;; メニューの予定表の表示範囲
;; 10 日前から
(setq howm-menu-schedule-days-before 10)
;; 3 日後まで
(setq howm-menu-schedule-days 3)

;; howm のファイル名
;; 以下のスタイルのうちどれかを選んでください
;; で，不要な行は削除してください
;; 1 メモ 1 ファイル (デフォルト)
(setq howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.howm")
;; 1 日 1 ファイルであれば
;(setq howm-file-name-format "%Y/%m/%Y-%m-%d.howm")

(setq howm-view-grep-parse-line
      "^\\(\\([a-zA-Z]:/\\)?[^:]*\\.howm\\):\\([0-9]*\\):\\(.*\\)$")
;; 検索しないファイルの正規表現
(setq
 howm-excluded-file-regexp
 "/\\.#\\|[~#]$\\|\\.bak$\\|/CVS/\\|\\.doc$\\|\\.pdf$\\|\\.ppt$\\|\\.xls$")

;; いちいち消すのも面倒なので
;; 内容が 0 ならファイルごと削除する
(if (not (memq 'delete-file-if-no-contents after-save-hook))
    (setq after-save-hook
          (cons 'delete-file-if-no-contents after-save-hook)))
(defun delete-file-if-no-contents ()
  (when (and
         (buffer-file-name (current-buffer))
         (string-match "\\.howm" (buffer-file-name (current-buffer)))
         (= (point-min) (point-max)))
    (delete-file
     (buffer-file-name (current-buffer)))))

;; http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?SaveAndKillBuffer
;; C-cC-c で保存してバッファをキルする
(defun my-save-and-kill-buffer ()
  (interactive)
  (when (and
         (buffer-file-name)
         (string-match "\\.howm"
                       (buffer-file-name)))
    (save-buffer)
    (kill-buffer nil)))
(eval-after-load "howm"
  '(progn
     (define-key howm-mode-map
       "\C-c\C-c" 'my-save-and-kill-buffer)))

