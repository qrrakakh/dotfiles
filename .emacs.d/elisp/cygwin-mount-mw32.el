;; -*- Mode: Emacs-Lisp -*-
;; cygwin-mount-mw32.el -- Teach Meadow2 about mount points

;; Copyright (C) 2003 Masayuki Fujii <boochang@m4.kcn.ne.jp>

;; Author: Masayuki Fujii
;; Keywords: files, mount, cygwin

;; This is not part of Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; What's cygwin-mount-mw32?:
;;
;; cygwin-mount calls mount command to get information about mounted
;; filesystems. cygwin-mount-mw32 uses registry API instead of mount
;; command and gets information about mounted filesystems faster than
;; cygwin-mount.
;;

;;; Requirement:
;;
;; cygwin-mount-mw32 works only on Meadow 2.00b1 or later.
;; (it doesn't work on NTEmacs)
;; cygwin-mount-mw32 requires cygwin-mount.el 
;; (I tested cygwin-mount-w32.el with cygwin-mount 1.4.5)
;;

;;; Usage:
;;
;; (require 'cygwin-mount-mw32)
;; (cygwin-mount-activate)
;;


(require 'cygwin-mount)

;; user options

(defvar cygwin-mount-mw32-disabled-drives '("a:")
  "A list of drives which cygwin-mount-mw32 ignores. When
`cygwin-mount-mw32-build-table-internal' checks drives, it skips any
drives in this list. Drive name must be a drive letter followed by a
colon. Drive letter is not case sensitive.")

;; constants

(defconst cygwin-mount-mw32-version "0.9.0")

(defconst cygwin-mount-mw32-subkey "SOFTWARE\\Cygnus Solutions\\Cygwin\\mounts v2"
  "A sub key of cygwin mount table (v2).")

(defconst cygwin-mount-mw32-sysroot "HKEY_LOCAL_MACHINE"
  "A root key of cygwin system-wide mount table.")

(defconst cygwin-mount-mw32-userroot "HKEY_CURRENT_USER"
  "A root key of cygwin user-local mount table.")

;; functions

(defun cygwin-mount-mw32-get-native-path (cygwin-path &optional user)
  (car (mw32-registry-get (concat (if user
				      cygwin-mount-mw32-userroot
				    cygwin-mount-mw32-sysroot)
				  "\\"
				  cygwin-mount-mw32-subkey
				  "\\" cygwin-path)
			  "native")))

(defun cygwin-mount-mw32-build-mount-table-internal (&optional user)
  (mapcar (lambda (cygwin-path)
	    (cons (file-name-as-directory
		   (cygwin-mount-mw32-get-native-path cygwin-path user))
		  (file-name-as-directory cygwin-path)))
	  (mw32-registry-list-keys (concat (if user
					       cygwin-mount-mw32-userroot
					     cygwin-mount-mw32-sysroot)
					   "\\"
					   cygwin-mount-mw32-subkey))))

(defun cygwin-mount-mw32-get-cygdrive-prefix ()
  (or (car (mw32-registry-get (concat cygwin-mount-mw32-userroot "\\"
				      cygwin-mount-mw32-subkey)
			      "cygdrive prefix"))
      (car (mw32-registry-get (concat cygwin-mount-mw32-sysroot "\\"
				      cygwin-mount-mw32-subkey)
			      "cygdrive prefix"))))

(defun cygwin-mount-mw32-get-cygdrive-list ()
  (let ((drive-letter ?a)
	(disabled-drives (mapcar 'downcase cygwin-mount-mw32-disabled-drives))
	(prefix (cygwin-mount-mw32-get-cygdrive-prefix))
	drive-name
	drive-root
	result)
    (while (<= drive-letter ?z)
      (setq drive-name (format "%c:" drive-letter))
      (unless (member drive-name disabled-drives)
	(setq drive-root (file-name-as-directory drive-name))
	(when (file-exists-p drive-root)
	  (setq result (cons (cons drive-root
				   (format "%s/%c/" prefix drive-letter))
			     result))))
      (setq drive-letter (1+ drive-letter)))
    result))

;;
;; cygwin-mount-build-table-internal is replaced.
;;
(defun cygwin-mount-mw32-build-table-internal ()
  (let ((sys-table (cygwin-mount-mw32-build-mount-table-internal))
	(usr-table (cygwin-mount-mw32-build-mount-table-internal t))
	(cygdrive-table (cygwin-mount-mw32-get-cygdrive-list))
	table)
    (setq table (append sys-table usr-table cygdrive-table))
    (setq table (sort table (lambda (a b) 
			      (< (length (cdr b)) (length (cdr a))))))
    (setq cygwin-mount-table--internal table)))

(fset 'cygwin-mount-build-table-internal-orig 
      'cygwin-mount-build-table-internal)
(defalias 'cygwin-mount-build-table-internal
  'cygwin-mount-mw32-build-table-internal)

(provide 'cygwin-mount-mw32)

;;; cygwin-mount-mw32.el ends here
