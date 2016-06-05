;;; ogmerge.el --- easier way to merge changes in shared Emacs/Orgzly Org notes

;; Copyright (C) 2016  Glenn Barry

;; Author: Glenn Barry <gaak99@gmail.com>
;; Keywords: ediff, merge, org-mode, orgzly, dropbox, cloud, sync
;; Version: 0.5.0
;; Package-Requires: ((s "1.8.0")  (f "0.16.0"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Put a description of the package here

;;; Code:

(require 'f)
(require 's)

;; User setable vars
;;
(defvar ogmerge-local-dir (f-canonical org-directory)
  "Local org dir aka Emacs org dir.")
(defvar ogmerge-remote-dir (f-canonical org-orgzly-directory)
  "Remote org dir aka upstream in cloud (aka Orgzly Save path in Dropbox).")

(defvar ogmerge-backup? t
  "Pre destructive op, make an emergency backup copy in subdir ogmerge-backup-dir.")
(defvar ogmerge-backup-dir ".ogmerge-bkup"
  "Name of subdir for emergency copies.")

(defvar ogmerge-verbose t
  "Get more output to *messages*.")

;; User commands
;;
(defun ogmerge-ediff (orgf)
  "Ediff merge remote repo orgzly file ORGF with local mac copy.

xxx flow?"
  (interactive "sEnter org file name: ")
  (let* ((ld ogmerge-local-dir);  local dir "repo"
	 (rd ogmerge-remote-dir); remote dir "repo" (Orgzly app will sync here)
	 (lf (concat ld "/" orgf))
	 (rf (concat rd "/" orgf)))
    (ogmerge--pr-debug
     (concat "start -ediff\n  lf=" lf "\n  rf=" rf))
    (if (not (f-exists? lf)) 
	(message (concat "ogmerge: file no exist: " lf))
      (if (not (f-exists? rf))
	  (message (concat "ogmerge: file no exist: " rf))
	(if (string= (funcall ogmerge--hash-it (f-read-text lf))
		     (funcall ogmerge--hash-it (f-read-text rf)))
	      (message (concat "ogmerge: files are same: " (f-filename lf)))
	    (ogmerge--ediff-files lf rf))))))

;; xxx testme. really needed? not in usual flow yo.
(defalias 'ogmerge-pull 'ogmerge-load)
(defun ogmerge-load (orgf)
  "Pull a copy of file ORGF from remote repo."
  (interactive "sEnter org file name: ")
  (let ((locf (concat ogmerge-local-dir "/" orgf))
	(remf (concat ogmerge-remote-dir "/" orgf)))
    (and ogmerge-backup?
	 (ogmerge--savecopyf locf (f-dirname locf)))
    (ogmerge--copyf remf locf)))

(defalias 'ogmerge-push 'ogmerge-save)
(defun ogmerge-save (orgf)
  "Push a copy of file ORGF to remote repo."
  (interactive "sEnter org file name: ")
  (let ((locf (concat ogmerge-local-dir "/" orgf))
	(remf (concat ogmerge-remote-dir "/" orgf)))
    (and ogmerge-backup?
       (ogmerge--savecopyf remf (f-dirname remf)))
  (ogmerge--copyf locf remf)))

;;; private funcs&vars
;;
(setq ogmerge--debug nil)
(defun ogmerge--pr-debug (msg)
  (and ogmerge--debug
       (message (concat "ogdebug: " msg))))

(defun ogmerge--pr-verbose (msg)
  (and ogmerge-verbose
       (message (concat "ogmerge: " msg))))

(setq ogmerge--hash-algo 'sha1)
(setq ogmerge--hash-it
      (lambda (o)
	(secure-hash ogmerge--hash-algo o)))

(defun ogmerge--ediff-buffers (bufa bufb)
  (let ((savemsg "ogmerge: saving backup copies...")
	(donemsg "ogmerge: files merged, pls pull/sync from Orgzly app"))
      ;; make backups here
      (ogmerge--pr-debug "-backup start")
      (ediff-merge-buffers bufa bufb 
      			   (make-temp-file
      			    (file-name-nondirectory
      			     localf))); 3rd arg seems like no-op :-(
      ))

(defun ogmerge--ediff-files (localf remotef)
  (let* ((ffa (find-file localf))
	 (bufa (current-buffer))
	 (ffb (find-file remotef))
	 (bufb (current-buffer)))
    (progn (ogmerge--pr-debug
	    (concat "--ediff-files start\n  bufa="
		    (buffer-file-name bufa)
		    "\n  bufb=" (buffer-file-name bufb)))
	   (and ogmerge-backup?
		(ogmerge--savecopyf localf (f-dirname localf)))
	   (ogmerge--ediff-buffers bufa bufb))))

(defun ogmerge--copyf (fro to)
  (if (not (f-exists? fro)) 
	(message (concat "ogmerge: file no exist: " fro))
      (if (f-exists? to)
	  (if (string= (funcall ogmerge--hash-it (f-read-text fro))
		       (funcall ogmerge--hash-it (f-read-text to)))
	      (message (concat "ogmerge: files are same: " (f-filename fro)))
	    (progn (f-delete to) (f-copy fro to))) 
	(f-copy fro to))))

(defun ogmerge--savecopyf (from_f to_d)
  "Pre file mod, make emergency copy of file FROM_F in subdir of TO_D dir."
  (let* ((subd ogmerge-backup-dir)
	 (to_d (if (s-ends-with? "/" to_d) to_d (concat to_d "/")))
	 (to_d (concat to_d subd))
	 (r1 (f-mkdir to_d)); xxx chmod?
	 (to_f (concat to_d "/"
		       (f-filename from_f)
		       "~"
		       (format-time-string "%Y%m%dT%H%M%S")
		       "~" ))
	 (r2 (ogmerge--pr-debug (concat "--savecopyf: " to_f)))
	 (r3 (f-copy from_f to_f)))
    to_f))

(provide 'ogmerge)
;;; ogmerge.el ends here
