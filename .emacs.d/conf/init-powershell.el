;;; init-powershell.el --- Configuration of Powershell mode

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

(autoload 'powershell "powershell"
  "Run powershell as a shell within emacs." t)

(require 'powershell-mode)
(setq auto-mode-alist
      (append '(("\\.ps1$" . powershell-mode))
              auto-mode-alist))

(setq powershell-indent 4)
(setq powershell-continuation-indent 2)

;;; init-powershell.el ends here
