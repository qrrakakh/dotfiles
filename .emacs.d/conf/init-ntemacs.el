;;; init-ntemacs.el --- 

;; Copyright (C) 2013  

;; Author:  <qrrakakh@LEIBNIZ>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

;; SHELL で ^M が付く場合は ^M を削除します。
(add-hook 'shell-mode-hook
	  (lambda ()
	    (set-buffer-process-coding-system 'undecided-dos 'sjis-unix)))
;; shell-mode での保管(for drive letter)
(setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@`'.,:()-")

;; バックアップを残さない
(setq make-backup-files nil)

;;; init-ntemacs.el ends here
