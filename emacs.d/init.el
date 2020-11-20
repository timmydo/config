(setq make-backup-files nil)
(global-display-line-numbers-mode)
(menu-bar-mode -1)
(tool-bar-mode -1)

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

(global-set-key (kbd "<f5>") 'deadgrep)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(defun org-timmy ()
  (interactive)
  (find-file "~/org/home.org"))

(global-set-key (kbd "C-c o") 'org-timmy)
(setq org-agenda-files (list "~/org/home.org"))

(setq exec-path '("/home/timmy/.cargo/bin" "/home/timmy/bin" "/home/timmy/go/bin" "/usr/local/go/bin" "/usr/local/bin" "/usr/local/sbin" "/usr/local/bin" "/usr/sbin" "/usr/bin" "/sbin" "/bin" "/usr/games" "/usr/local/games" "/home/timmy/.fzf/bin" "/usr/local/libexec/emacs/28.0.50/x86_64-pc-linux-gnu"))

(require 'package)
(add-to-list 'package-archives
     	     '("melpa" . "https://melpa.org/packages/") t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(package-selected-packages
   '(deadgrep rg with-editor elpher spinner magit ivy counsel go-rename go-mode yasnippet company-lsp company lsp-ui lsp-mode use-package notmuch notmuch-counsel)))
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
;			       (:name "sent"
;                                      :query "tag:from-me"
;				      :sort-order newest-first
;				      :key "s")
			       (:name "btrfs"
                                      :query "tag:btrfs and tag:inbox")
			       (:name "gemini"
                                      :query "tag:gemini and tag:inbox")
			       (:name "git"
                                      :query "tag:git and tag:inbox")
			       (:name "golang"
                                      :query "tag:golang and tag:inbox")
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


(defun eshell/-buffer-as-args (buffer separator command)
  "Takes the contents of BUFFER, and splits it on SEPARATOR, and
runs the COMMAND with the contents as arguments. Use an argument
`%' to substitute the contents at a particular point, otherwise,
they are appended."
  (let* ((lines (with-current-buffer buffer
                  (split-string
                   (buffer-substring-no-properties (point-min) (point-max))
                   separator)))
         (subcmd (if (-contains? command "%")
                     (-flatten (-replace "%" lines command))
                   (-concat command lines)))
         (cmd-str  (string-join subcmd " ")))
    (message cmd-str)
    (eshell-command-result cmd-str)))

(defun eshell/bargs (buffer &rest command)
  "Passes the lines from BUFFER as arguments to COMMAND."
  (eshell/-buffer-as-args buffer "\n" command))

(defun eshell/sargs (buffer &rest command)
  "Passes the words from BUFFER as arguments to COMMAND."
  (eshell/-buffer-as-args buffer nil command))
