(setq make-backup-files nil)
(global-display-line-numbers-mode)
(menu-bar-mode -1)
					;(tool-bar-mode -1)

(unless window-system
  (require 'mouse)
  (xterm-mouse-mode 0)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode nil)
)

(global-set-key "\C-k" 'kill-whole-line)
(global-set-key (kbd "C-s") 'swiper)  ;; replaces i-search with swiper
(global-set-key (kbd "M-x") 'counsel-M-x) ;; Gives M-x command counsel features
(global-set-key (kbd "C-x C-f") 'counsel-find-file) ;; gives C-x C-f counsel features
(global-set-key (kbd "C-x f") 'counsel-file-jump) ;; gives C-x C-f counsel features
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c C-r") 'ivy-resume)

(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag) ;; add counsel/ivy features to ag package
(global-set-key (kbd "C-x l") 'counsel-locate)




(require 'package)
(add-to-list 'package-archives
     	     '("melpa" . "https://melpa.org/packages/") t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (spinner magit ivy counsel go-rename go-mode yasnippet company-lsp company lsp-ui lsp-mode use-package notmuch notmuch-counsel))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(package-initialize)


(load "~/.config/emacs.d/lsp-mode-init.el")


(ivy-mode 1)
(whitespace-mode 1)

(setq notmuch-saved-searches '(
			       (:name "all"
                                      :query "*"
				      :sort-order newest-first
				      :key "a")
			       (:name "inbox"
                                      :query "tag:inbox and not tag:list and not tag:from-me"
				      :key "i")
			       (:name "sent"
                                      :query "tag:from-me"
				      :sort-order newest-first
				      :key "s")
			       (:name "btrfs"
                                      :query "tag:btrfs and tag:inbox")
			       (:name "rss"
                                      :query "folder:rss and tag:inbox")
			       (:name "github"
                                      :query "folder:github and tag:inbox")
			       (:name "notmuch"
                                      :query "tag:notmuch and tag:inbox"
				      :key "n")
			       (:name "emacs-devel"
                                      :query "tag:emacs-devel and tag:inbox")
			       (:name "deals"
                                      :query "tag:deals and tag:inbox")
			       (:name "friend"
                                      :query "tag:friend and tag:inbox and not tag:from-me"
				      :sort-order newest-first)
			       ))



(setq mail-host-address "timmydouglas.com")
(setq user-full-name "Timmy Douglas")
(setq user-mail-address "mail@timmydouglas.com")

(setq send-mail-function 'smtpmail-send-it)
(setq message-send-mail-function 'smtpmail-send-it)

(setq send-mail-function    'smtpmail-send-it
          smtpmail-smtp-server  "smtp.mxes.net"
          smtpmail-stream-type  'ssl
          smtpmail-smtp-service 465)
(defun my-message-mode-setup ()
       (setq fill-column 72)
       (turn-on-auto-fill))
(add-hook 'message-mode-hook 'my-message-mode-setup)
