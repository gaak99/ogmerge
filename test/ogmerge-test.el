;;; ogmege test suite -- test it!

;;; Code:
(require 'f)
(require 'mocker)

;; .emacs org (set for other pkgs aka org/orgzly)
(setq tog-org-orgzly-directory "/tmp/ogmerge-test/dbox/orgzly")
(setq tog-org-directory "/tmp/ogmerge-test/org")

;; .emacs ogmerge user setable vars
(setq ogmerge-local-dir (f-canonical tog-org-directory))
(setq ogmerge-remote-dir (f-canonical tog-org-orgzly-directory))
(setq ogmerge-backup? t)
(setq ogmerge-backup-dir ".test-ogmerge-bkup")
(setq ogmerge-verbose nil)

(setq load-path
      (cons (expand-file-name ".") load-path))
(require 'ogmerge)


;; setup test files
(defun ogmerge-test--setup (orgf)
  (let* ((r0c (f-mkdir "/tmp/ogmerge-test"))
	 (r0c (f-mkdir "/tmp/ogmerge-test/dbox"))
	 (r0a (f-mkdir tog-org-directory))
	 (r0b (f-mkdir tog-org-orgzly-directory))
	 (r1 (f-write "fooz" 'utf-8
		      (concat tog-org-directory "/" orgf)))
	 (r2 (f-write "fooz\nbazz" 'utf-8
		      (concat tog-org-orgzly-directory "/" orgf))))
    r2))

;; prolly dont need to kill bufs for --batch (but do rm files)
(defun ogmerge-test--teardown (orgf)
  (let* ((f0 "mergememaybe:org")
	 (r0 (kill-buffer f0))
	 (f1 "mergememaybe"); trust me
	 (r1 (kill-buffer f1)))
    r1))

;; misc tests

(defun ogmerge-test-ediff (orgf)
  (let* ((yo 'yo)
	 (r0 (ogmerge-test--setup orgf))
	 (r1 (mocker-let ((ediff-merge-buffers
			   (b1 b2 f)
			   ((:input-matcher
			     '(lambda (x y z)
				(and (bufferp x) (bufferp y)))
			     :output t))))
	       (ogmerge-ediff orgf)))
	 )
    r1))

(defun ogmerge-test-push (orgf)
  (let* ((lf (concat tog-org-directory "/" orgf))
	 (rf (concat tog-org-orgzly-directory "/" orgf))
	 (r0 (ogmerge-test--setup orgf))
	 (r1 (ogmerge-push orgf))
	 (feqp (string= (funcall ogmerge--hash-it (f-read-text lf))
			(funcall ogmerge--hash-it (f-read-text rf)))))
    (if feqp "PASS" "FAIL")))


(defun ogmerge-test-pull (orgf)
  (let* ((lf (concat tog-org-directory "/" orgf))
	 (rf (concat tog-org-orgzly-directory "/" orgf))
	 (r0 (ogmerge-test--setup orgf))
	 (r1 (ogmerge-pull orgf))
	 (feqp (string= (funcall ogmerge--hash-it (f-read-text lf))
			(funcall ogmerge--hash-it (f-read-text rf)))))
    (if feqp "PASS" "FAIL")))


(defun ogmerge-test-savecopy (orgf)
  (let* ((r0 (ogmerge-test--setup orgf))
	 (lf (concat tog-org-directory "/" orgf))
	 (bf (ogmerge--savecopyf lf (f-dirname lf)))
	 (feqp (string= (funcall ogmerge--hash-it (f-read-text lf))
			(funcall ogmerge--hash-it (f-read-text bf)))))
    (if feqp "PASS" "FAIL")))

;; runtests

(let* ((orgf "mergememaybe")
       (r1 (ogmerge-test-ediff orgf))
       )
  (message "ogmerge-test-ediff ... PASS."))

(let* ((orgf "pushmemaybe")
       (r (ogmerge-test-push orgf))
       )
  (message (concat "ogmerge-test-push ... " r ".")))

(let* ((orgf "pullmemaybe")
       (r (ogmerge-test-pull orgf)))
  (message (concat "ogmerge-test-pull ... " r ".")))

(let* ((orgf "savememaybe")
       (r (ogmerge-test-savecopy orgf)))
  (message (concat "ogmerge-test-savecopy ... " r ".")))


