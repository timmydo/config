(use-modules (shepherd service))



(register-services
  (make <service>
        #:provides '(timmy-notmuch)
;        #:requires '(notmuch)
        #:start (make-forkexec-constructor
		 (list "/home/timmy/.config/notmuch/new.sh"))
        #:stop (make-kill-destructor)))
