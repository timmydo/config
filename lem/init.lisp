
(in-package :lem-core)
;; do nothing
(defun check-already-defined-command (name source-location))

(lem-core:load-site-init)

(in-package :lem-user)

(lem-lisp-mode/implementation:set-default-command "sbcl")

(define-command open-init-file () ()
  (lem:find-file
   (merge-pathnames "init.lisp" (lem-home))))

(log:config :info)

#+nil(defun wipe-region (start end)
  (let ((repeat-command (continue-flag :kill)))
	  (let ((killed-string (delete-character start (count-characters start end))))
	    (with-killring-context (:appending repeat-command)
	      (copy-to-clipboard-with-killring killed-string)))))

#+nil(define-command kill-region-or-line () ()
  "Kill the text of region or the current line if no region."
  (if (buffer-mark (current-buffer))
      (with-point ((start (cursor-region-beginning (current-point)))
                   (end (cursor-region-end (current-point))))
	(wipe-region start end))
      (with-point ((start (current-point))
		   (end (current-point)))
	(line-start start)
	(line-start end)
	(line-offset end 1)
	(when (point= start end)
	  (line-end end))
	(wipe-region start end)
	)))


#+nil(define-command kill-region-with-newline (start end) ("r")
  "Kill the text of region."
  (when (point< end start)
    (rotatef start end))
  (let ((repeat-command (continue-flag :kill)))
    (let ((killed-string (delete-character start (count-characters start end))))
      (with-killring-context (:appending repeat-command)
        (copy-to-clipboard-with-killring (concatenate 'string killed-string (format nil "~%")))))))

#+nil(define-command kill-whole-line-with-newline () ()
  "Kill the entire line and the remaining whitespace"
   (with-point ((start (current-point))
                (end (current-point)))
     (line-end end)
     (kill-region-with-newline start end))
  (if (start-buffer-p (current-point))
      (delete-next-char)
      (delete-previous-char)))

#+nil(define-command kill-region-or-line () ()
  "Kill the text of region or the current line if no region."
  (if (buffer-mark (current-buffer))
        (kill-region (region-beginning)
                     (region-end))
        (progn (line-start (current-point))
	       (kill-whole-line-with-newline)
	       (unless (start-buffer-p (current-point))
		 (line-offset (current-point) 1))
	       (line-start (current-point)))))

#+nil(defmethod execute :around (mode (command kill-region) argument)
  (if (buffer-mark-p (current-buffer))
      (call-next-method)
      (with-point ((start (current-point))
                   (end (current-point)))
        (line-start start)
        (line-offset end 1)
        (kill-region start end))))

(define-key lem:*global-keymap* "C-o" 'lem:next-window)
;(define-key lem:*global-keymap* "C-w" 'kill-region-or-line)


(load "/home/timmy/src/lem-notmuch/package.lisp")
(load "/home/timmy/src/lem-notmuch/notmuch.lisp")

(setf lem-notmuch:*notmuch-saved-searches* '(
			       (:name "inbox"
                                      :query "tag:inbox and not tag:list and not tag:github"
				      :key "i")
			       (:name "btrfs"
                                      :query "tag:btrfs and tag:inbox")
			       (:name "save"
                                      :query "tag:save")
			       (:name "alpine"
                                      :query "tag:alpine and tag:inbox")
			       (:name "guix"
                                      :query "tag:guix and tag:inbox")
			       (:name "git"
                                      :query "tag:git and tag:inbox")
			       (:name "rss"
                                      :query "folder:rss and tag:inbox")
			       (:name "github"
                                      :query "folder:github and tag:inbox or tag:github and tag:inbox")
			       (:name "notmuch"
                                      :query "tag:notmuch and tag:inbox"
				      :key "n")
			       (:name "emacs-devel"
                                      :query "tag:emacs-devel and tag:inbox")
			       (:name "deals"
                                      :query "tag:deals and tag:inbox")
			       ))
