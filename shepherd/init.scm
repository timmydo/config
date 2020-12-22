(use-modules (shepherd service)
	     (ice-9 match))

;; some good examples at:
;; https://git.dthompson.us/dotfiles.git/tree/dotfiles/.config/shepherd/init.scm

(define-syntax-rule (with-fork body ...)
  (match (primitive-fork)
    (0 (begin body ...))
    (pid pid)))

(define (run-command command)
  (zero? (status:exit-val (apply system* command))))

(register-services

 (make <service>
        #:provides '(notmuch)
        #:requires '()
        #:start
	(lambda args
          (with-fork
           (while #t
            (system* "/home/timmy/.config/notmuch/new.sh")
            (sleep 100))))
        #:stop (make-kill-destructor))


  (make <service>
    #:provides '(emacsd)
    #:requires '()
    #:start (lambda _ (run-command '("emacs" "--daemon")))
    #:environment-variables
    (list
     "HOME=/home/timmy"
     "SSH_AUTH_SOCK=/run/user/1000/keyring/ssh")
    #:stop (lambda _ (not (run-command '("emacsclient" "--eval" "(kill-emacs)")))))


  )

(action 'shepherd 'daemonize)

