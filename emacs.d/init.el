(setq make-backup-files nil)
(global-display-line-numbers-mode)
(menu-bar-mode -1)
					;(tool-bar-mode -1)

(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
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

(setq notmuch-folders '(("inbox" . "tag:inbox")
			("tome" . "tag:inbox and (tag:to-me or tag:friends)")
			("btrfs" . "tag:btrfs")
			("emacs-devel" . "tag:emacs-devel")
			
			))

(setq notmuch-saved-searches '((:name "inbox"
                                      :query "tag:inbox"
                                      :count-query "tag:inbox and tag:unread"
                                      :sort-order oldest-first)
			       (:name "btrfs"
                                      :query "tag:btrfs and tag:inbox"
                                      :sort-order oldest-first)
			       (:name "emacs-devel"
                                      :query "tag:emacs-devel and tag:inbox"
                                      :sort-order oldest-first)
			       (:name "friend"
                                      :query "tag:friend and tag:inbox"
                                      :sort-order oldest-first)

			       ))


(require 'package)
(add-to-list 'package-archives
     	     '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (spinner magit ivy counsel go-rename go-mode yasnippet company-lsp company lsp-ui lsp-mode use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load "~/.config/emacs.d/lsp-mode-init.el")


(ivy-mode 1)
(whitespace-mode 1)
