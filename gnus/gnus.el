(setq gnus-select-method '(nnmaildir "td" (directory "~/mail/timmydouglascom")))
(setq gnus-secondary-select-methods
      '((nnmaildir "info" (directory "~/mail/timmydinfo"))
	(nnmaildir "gmail" (directory "~/mail/gmail"))))

(setq mail-host-address "timmydouglas.com")
(setq user-full-name "Timmy Douglas")
(setq user-mail-address "mail@timmydouglas.com")

(setq send-mail-function 'smtpmail-send-it)
(setq message-send-mail-function 'smtpmail-send-it)

(setq send-mail-function    'smtpmail-send-it
          smtpmail-smtp-server  "smtp.mxes.net"
          smtpmail-stream-type  'ssl
          smtpmail-smtp-service 465)
