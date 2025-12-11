(setq make-backup-files nil)
(global-display-line-numbers-mode)
(column-number-mode)
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

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq inhibit-startup-message t)


(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
;                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(use-package all-the-icons)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-n" . ivy-next-line)
         ("C-p" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-p" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-p" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))


(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))


(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))


(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))


;; from lsp-doctor
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024 5))
(setq lsp-idle-delay 0.500)
(setq lsp-ui-sideline-delay 2)
(setq lsp-ui-doc-enable nil)
(setq lsp-lens-enable nil)
;(use-package treemacs)
;
;(use-package lsp-treemacs
;  :after lsp)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.5))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "SPC") nil))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package avy)
(setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n))
(setq avy-dispatch-alist
  '((?M . avy-action-kill-move)
    (?m . avy-action-kill-stay)
    (?T . avy-action-teleport)
    (?p . avy-action-mark)
    (?c . avy-action-copy)
    (?y . avy-action-yank)
    (?Y . avy-action-yank-line)
    (?s . avy-action-ispell)
    (?z . avy-action-zap-to-char)))


(eval-after-load
  'company
  '(add-to-list 'company-backends #'company-omnisharp))

(use-package solarized-theme)

(load-theme 'solarized-dark t)

;(whitespace-mode 1)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package term
  :config
  (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
)

(use-package go-mode)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package pass
  :pin melpa
  :config
  (setf epa-pinentry-mode 'loopback))

(use-package python-mode)

(add-hook 'python-mode-hook
  (lambda ()
    (setq indent-tabs-mode t)
    (setq python-indent 8)
    (setq tab-width 4)))

(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

(setq ccls-executable "~/.guix-profile/bin/ccls")

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

(put 'kill-region 'interactive-form      
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))



(use-package hide-mode-line)

(defun efs/presentation-setup ()
  ;; Hide the mode line
  (hide-mode-line-mode 1)
)

(defun efs/presentation-end ()
  ;; Show the mode line again
  (hide-mode-line-mode 0)
)

(use-package org-tree-slide
  :hook ((org-tree-slide-play . efs/presentation-setup)
         (org-tree-slide-stop . efs/presentation-end))
  :custom
  (org-tree-slide-slide-in-effect t)
  (org-tree-slide-activate-message "Presentation started!")
  (org-tree-slide-deactivate-message "Presentation finished!")
  (org-tree-slide-header t)
  (org-tree-slide-breadcrumbs " > ")
  (org-image-actual-width nil))


; (add-hook 'after-init-hook 'global-company-mode)

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))



(require 'recentf)

;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; enable recent files mode.
(recentf-mode t)

; 50 files ought to be enough.
(setq recentf-max-saved-items 50)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;;
;; reverse isearch eshell history
;;

(defun timmy/counsel-eshell-history-action (cmd)
  "Insert cmd into the buffer"
  (interactive)
  (insert cmd))

(defun timmy/counsel-eshell-history (&optional initial-input)
  "Find command from eshell history.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
    (ivy-read "Find cmd: " (timmy/eshell-history-list)
              :initial-input initial-input
              :action #'timmy/counsel-eshell-history-action
              :caller 'timmy/counsel-eshell-history))

(defun timmy/eshell-history-list ()
  "return the eshell history as a list"
  (and (or (not (ring-p eshell-history-ring))
	   (ring-empty-p eshell-history-ring))
       (error "No history"))
  (let* ((index (1- (ring-length eshell-history-ring)))
	 (ref (- (ring-length eshell-history-ring) index))
	 (items (list)))
    (while (>= index 0)
      (setq items (cons (format "%s" (eshell-get-history index)) items)
	    index (1- index)
	    ref (1+ ref)))
    items))

(use-package esh-mode
  :ensure nil
  :bind (:map eshell-mode-map
	      ("C-r" . timmy/counsel-eshell-history)))

(use-package geiser)
(use-package geiser-guile)
(with-eval-after-load 'geiser-guile
  (add-to-list 'geiser-guile-load-path "~/src/guix"))

(use-package notmuch)

(use-package deadgrep)

;(use-package paredit)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))


(use-package frog-jump-buffer :ensure t)

(defcustom frog-menu-avy-keys (append (string-to-list "aoeuidhtns")
                                      (string-to-list "1234567890")
                                      (string-to-list "',.pyfgcrl")
                                      (string-to-list ";qjkxbmwvz")
                                      (number-sequence ?, ?@))
  "Frog menu keys used for `avy-keys'.
By default uses a large collection of keys, so that the hints can
be drawn by single characters."
  :type '(repeat character))

;(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(add-hook 'lisp-mode-hook 'sly-editing-mode)
(remove-hook 'lisp-mode-hook 'slime-lisp-mode-hook)

(use-package sly)


(use-package elpher)


(setq inferior-lisp-program "sbcl")
;(use-package slime)
;(setq slime-load-failed-fasl 'never)

(defun timmy/kill-buffer-file-name ()
  "add the current file name to the kill ring"
  (interactive)
  (message buffer-file-name)
  (kill-new buffer-file-name))

(defun timmy/kill-buffer-path ()
  "add the current file name to the kill ring"
  (interactive)
  (let ((d (directory-file-name (file-name-directory (or (buffer-file-name) default-directory)))))
    (message d)
    (kill-new d)))
  

(defun timmy/eshell ()
  "add the current file name to the kill ring"
  (interactive)
  (let ((fn (directory-file-name (file-name-directory (or (buffer-file-name) default-directory)))))
    (with-current-buffer "*eshell*"
      (cd fn)
      (eshell-emit-prompt))
    (eshell)))

(defun timmy/findemacsconfig ()
  "edit the emacs config"
  (interactive)
  (find-file "/home/timmy/.config/emacs/init.el"))


;;
;; Global key bindings
;;

(global-set-key (kbd "C-!") 'delete-other-windows)
(global-set-key (kbd "C-s") 'swiper)  ;; replaces i-search with swiper
(global-set-key (kbd "M-x") 'counsel-M-x) ;; Gives M-x command counsel features
(global-set-key (kbd "C-x C-f") 'counsel-find-file) ;; gives C-x C-f counsel features
(global-set-key (kbd "C-x f") 'counsel-file-jump) ;; gives C-x C-f counsel features
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c C-r") 'ivy-resume)

(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag) ;; add counsel/ivy features to ag package
(global-set-key (kbd "C-x l") 'counsel-locate)

(global-set-key (kbd "<f1>") 'frog-jump-buffer)
(global-set-key (kbd "<f2>") 'recompile)
(global-set-key (kbd "<f3>") 'counsel-git-grep)
(global-set-key (kbd "<f5>") 'deadgrep)
(global-set-key (kbd "<f6>") 'counsel-git)
(global-set-key (kbd "<f9>") 'dired)
(global-set-key (kbd "<f10>") 'magit-status)
(global-set-key (kbd "<f11>") 'bury-buffer)
(global-set-key (kbd "<f12>") 'timmy/findemacsconfig)

(global-set-key (kbd "<pause>") 'mode-line-other-buffer)
(global-set-key (kbd "M-o") 'mode-line-other-buffer)
(global-set-key (kbd "C-o") 'other-window)
(define-key dired-mode-map (kbd "C-o") 'other-window)

(global-set-key (kbd "C-x o") 'open-line)

(global-set-key (kbd "<C-tab>") 'tab-next)
(global-set-key (kbd "<C-iso-lefttab>") 'tab-previous)

(global-set-key (kbd "<C-left>") 'previous-buffer)
;(define-key paredit-mode-map (kbd "<C-left>") 'previous-buffer)
(global-set-key (kbd "<C-right>") 'next-buffer)
;(define-key paredit-mode-map (kbd "<C-right>") 'next-buffer)
(global-set-key (kbd "M-s") 'avy-goto-char)
(global-set-key (kbd "M-j") 'avy-goto-char-timer)

;;
;; Org-mode related
;;

;(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
;(global-set-key (kbd "C-c c") 'org-capture)

(defun org-timmy ()
  (interactive)
  (find-file "~/org/home.org"))

(global-set-key (kbd "C-c o") 'org-timmy)
(global-set-key (kbd "<f8>") 'org-tree-slide-mode)

(setq org-agenda-files (list "~/org"))



(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode t)

;;
;; pcomplete
;;


;;;; sudo completion
(defun pcomplete/sudo ()
  "Completion rules for the `sudo' command."
  (let ((pcomplete-ignore-case t))
    (pcomplete-here (funcall pcomplete-command-completion-function))
    (while (pcomplete-here (pcomplete-entries)))))

;;;; systemctl completion
(defcustom pcomplete-systemctl-commands
  '("disable" "enable" "status" "start" "restart" "stop" "reenable"
    "list-units" "list-unit-files")
  "p-completion candidates for `systemctl' main commands"
  :type '(repeat (string :tag "systemctl command"))
  :group 'pcomplete)

(defvar pcomplete-systemd-units
  (split-string
   (shell-command-to-string
    "(systemctl list-units --all --full --no-legend;systemctl list-unit-files --full --no-legend)|while read -r a b; do echo \" $a\";done;"))
  "p-completion candidates for all `systemd' units")

(defvar pcomplete-systemd-user-units
  (split-string
   (shell-command-to-string
    "(systemctl list-units --user --all --full --no-legend;systemctl list-unit-files --user --full --no-legend)|while read -r a b;do echo \" $a\";done;"))
  "p-completion candidates for all `systemd' user units")

(defun pcomplete/systemctl ()
  "Completion rules for the `systemctl' command."
  (pcomplete-here (append pcomplete-systemctl-commands '("--user")))
  (cond ((pcomplete-test "--user")
         (pcomplete-here pcomplete-systemctl-commands)
         (pcomplete-here pcomplete-systemd-user-units))
        (t (pcomplete-here pcomplete-systemd-units))))



(defun pcmpl-git-commands ()
  "Return the most common git commands by parsing the git output."
  (with-temp-buffer
    (call-process-shell-command "git" nil (current-buffer) nil "help" "--all")
    (goto-char 0)
    (search-forward "Main Porcelain Commands")
    (let (commands)
      (while (re-search-forward
	      "^[[:blank:]]+\\([[:word:]-.]+\\)[[:blank:]]*\\([[:word:]-.]+\\)?"
	      nil t)
	(push (match-string 1) commands)
	(when (match-string 2)
	  (push (match-string 2) commands)))
      (sort commands #'string<))))

(defconst pcmpl-git-commands (pcmpl-git-commands)
  "List of `git' commands.")

(defvar pcmpl-git-ref-list-cmd "git for-each-ref refs/ --format='%(refname)'"
  "The `git' command to run to get a list of refs.")

(defun pcmpl-git-get-refs (type)
  "Return a list of `git' refs filtered by TYPE."
  (with-temp-buffer
    (insert (shell-command-to-string pcmpl-git-ref-list-cmd))
    (goto-char (point-min))
    (let (refs)
      (while (re-search-forward (concat "^refs/" type "/\\(.+\\)$") nil t)
	(push (match-string 1) refs))
      (nreverse refs))))

(defun pcmpl-git-remotes ()
  "Return a list of remote repositories."
  (split-string (shell-command-to-string "git remote")))

(defun pcomplete/git ()
  "Completion for `git'."
  ;; Completion for the command argument.
  (pcomplete-here* pcmpl-git-commands)
  (cond
   ((pcomplete-match "help" 1)
    (pcomplete-here* pcmpl-git-commands))
   ((pcomplete-match (regexp-opt '("pull" "push")) 1)
    (pcomplete-here (pcmpl-git-remotes)))
   ;; provide branch completion for the command `checkout'.
   ((pcomplete-match "checkout" 1)
    (pcomplete-here* (append (pcmpl-git-get-refs "heads")
			     (pcmpl-git-get-refs "tags"))))
   (t
    (while (pcomplete-here (pcomplete-entries))))))

;;
;; Email
;;

					;(setq notmuch-identities '("Timmy Douglas <mail@timmydouglas.com>"))

(defadvice notmuch-mua-reply (around notmuch-fix-sender)
  (let ((sender "Timmy Douglas <mail@timmydouglas.com>"))
    ad-do-it))
(ad-activate 'notmuch-mua-reply)

(setq notmuch-saved-searches '(
			       (:name "inbox"
                                      :query "tag:inbox and not tag:list and not tag:github and not tag:html"
				      :key "i")
;			       (:name "sent"
;                                      :query "tag:from-me"
;				      :sort-order newest-first
;				      :key "s")
			       (:name "html"
                                      :query "tag:inbox and tag:html and not tag:list")
			       (:name "btrfs"
                                      :query "tag:btrfs and tag:inbox")
			       (:name "hare"
                                      :query "tag:hare and tag:inbox")
			       (:name "save"
                                      :query "tag:save")
			       (:name "sourcehut"
                                      :query "tag:sourcehut and tag:inbox")
			       (:name "alpine"
                                      :query "tag:alpine and tag:inbox")
			       (:name "guix"
                                      :query "tag:guix and tag:inbox")
			       (:name "git"
                                      :query "tag:git and tag:inbox")
			       (:name "golang"
                                      :query "tag:golang and tag:inbox")
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

(setq mail-host-address "timmydouglas.com")
(setq user-full-name "Timmy Douglas")
(setq user-mail-address "mail@timmydouglas.com")

(setq send-mail-function 'smtpmail-send-it)
(setq message-send-mail-function 'smtpmail-send-it)

(setq send-mail-function    'smtpmail-send-it
      smtpmail-smtp-server  "smtp.migadu.com"
      ;;smtpmail-smtp-server  "smtp.sendgrid.net"
          smtpmail-stream-type  'ssl
          smtpmail-smtp-service 465)
(defun my-message-mode-setup ()
  (setq fill-column 72)
  (flyspell-mode)
  (turn-on-auto-fill))
(add-hook 'message-mode-hook 'my-message-mode-setup)

(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(setq erc-default-server "10.18.11.2")
(defun timmy/erc ()
  (interactive)
  (setq erc-email-userid "timmydo")
  (erc-tls :server "chat.sr.ht"
       :nick "timmydo"
       :port 6697
       :full-name "timmydo"
       :password
       (let ((secret (plist-get (car (auth-source-search :host "chat.sr.ht" :user "timmydo" :max 1)) :secret)))
	 (if (functionp secret)
	     (funcall secret)
	   secret))))

(defun browse-url-netsurf (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply #'start-process
           (concat "netsurf-gtk3" url) nil
           "netsurf-gtk3"
           (append
            (list url)))))

(defun browse-url-visurf (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply #'start-process
           (concat "visurf" url) nil
           "/gnu/store/sdlgyfaxwp86apmky4jqj0ipkqq75g0g-visurf-0.1.1/bin/netsurf-vi"
           (append
            (list url)))))

(defun browse-url-chromium (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply #'start-process
           (concat "chromium" url) nil
           "/home/timmy/.guix-profile/bin/chromium"
           (append
            (list url)))))

(setq
 browse-url-handlers
 '(
;  ("news.ycombinator.com" . eww-browse-url)
;  (".*" . browse-url-chromium)
  (".*" . eww-browse-url)
;  (".*" . browse-url-visurf)
  ))
(put 'upcase-region 'disabled nil)


;(load-file "~/.config/emacs/hare-mode.el")

(let ((haremode "~/src/hare-mode/hare-mode.el"))
 (when (file-exists-p haremode)
   (load-file haremode))
)

;(setq-default show-trailing-whitespace nil)

;; apply patches
(defun mbox-open-notmuch-messages ()
  "When this function is executed in notmuch-show buffer all the \"open\"
messages will be written to the file ~/tmp-mbox (overwriting it)."
  (interactive)
  (let ((search-terms-list (notmuch-show-get-message-ids-for-open-messages))
	(buffer (get-buffer-create "* Contents of ~/tmp-mbox *")))
    (set-buffer buffer)
    (setq buffer-read-only nil)
    (buffer-disable-undo)
    (pop-to-buffer buffer)
    (goto-char (point-max))
    (if (> (buffer-size) 0)
	(insert "\n\n"))
    (insert (format-time-string
	     "%c: Writing the following messages to ~/tmp-mbox:\n ")
	    (mapconcat 'identity search-terms-list "\n ") "\n")
    (with-temp-file "~/tmp-mbox"
      (call-process notmuch-command nil t nil "show" "--format=mbox"
		    (mapconcat 'identity search-terms-list " OR ")))
    (insert "\nMessages in ~/tmp-mbox:\n")
    (call-process "mail" nil t nil
		  "-H" "-f" (expand-file-name "~/tmp-mbox"))))





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(all-the-icons-dired ccls company-box counsel deadgrep elpher
			 eterm-256color frog-jump-buffer geiser-guile
			 go-mode helpful hide-mode-line ivy-rich
			 lsp-ui magit notmuch omnisharp org-tree-slide
			 pass python-mode rainbow-delimiters sly
			 solarized-theme yasnippet))
 '(safe-local-variable-values
   '((lisp-fill-paragraphs-as-doc-string nil)
     (geiser-insert-actual-lambda)
     (eval cl-flet
	   ((enhance-imenu-lisp (&rest keywords)
				(dolist (keyword keywords)
				  (let
				      ((prefix
					(when (listp keyword)
					  (cl-second keyword)))
				       (keyword
					(if (listp keyword)
					    (cl-first keyword)
					  keyword)))
				    (add-to-list
				     'lisp-imenu-generic-expression
				     (list
				      (purecopy
				       (concat (capitalize keyword)
					       (if
						   (string=
						    (substring-no-properties
						     keyword -1)
						    "s")
						   "es"
						 "s")))
				      (purecopy
				       (concat "^\\s-*("
					       (regexp-opt
						(list
						 (if prefix
						     (concat prefix
							     "-"
							     keyword)
						   keyword)
						 (concat prefix "-"
							 keyword))
						t)
					       "\\s-+\\("
					       lisp-mode-symbol-regexp
					       "\\)"))
				      2))))))
	   (enhance-imenu-lisp '("bookmarklet-command" "define")
			       '("class" "define")
			       '("command" "define")
			       '("ffi-method" "define")
			       '("ffi-generic" "define")
			       '("function" "define")
			       '("internal-page-command" "define")
			       '("internal-page-command-global"
				 "define")
			       '("mode" "define")
			       '("parenscript" "define") "defpsmacro"))
     (eval with-eval-after-load 'tempel
	   (if (stringp tempel-path)
	       (setq tempel-path (list tempel-path)))
	   (let
	       ((guix-tempel-snippets
		 (concat
		  (expand-file-name "etc/snippets/tempel"
				    (locate-dominating-file
				     default-directory
				     ".dir-locals.el"))
		  "/*.eld")))
	     (unless (member guix-tempel-snippets tempel-path)
	       (add-to-list 'tempel-path guix-tempel-snippets))))
     (eval with-eval-after-load 'git-commit
	   (add-to-list 'git-commit-trailers "Change-Id"))
     (eval with-eval-after-load 'geiser-guile
	   (let
	       ((root-dir
		 (file-name-directory
		  (locate-dominating-file default-directory
					  ".dir-locals.el"))))
	     (unless (member root-dir geiser-guile-load-path)
	       (setq-local geiser-guile-load-path
			   (cons root-dir geiser-guile-load-path)))))
     (eval progn (require 'lisp-mode)
	   (defun emacs27-lisp-fill-paragraph (&optional justify)
	     (interactive "P")
	     (or (fill-comment-paragraph justify)
		 (let
		     ((paragraph-start
		       (concat paragraph-start
			       "\\|\\s-*\\([(;\"]\\|\\s-:\\|`(\\|#'(\\)"))
		      (paragraph-separate
		       (concat paragraph-separate
			       "\\|\\s-*\".*[,\\.]$"))
		      (fill-column
		       (if
			   (and
			    (integerp emacs-lisp-docstring-fill-column)
			    (derived-mode-p 'emacs-lisp-mode))
			   emacs-lisp-docstring-fill-column
			 fill-column)))
		   (fill-paragraph justify))
		 t))
	   (setq-local fill-paragraph-function
		       #'emacs27-lisp-fill-paragraph))
     (eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'")
     (geiser-repl-per-project-p . t)
     (eval with-eval-after-load 'yasnippet
	   (let
	       ((guix-yasnippets
		 (expand-file-name "etc/snippets/yas"
				   (locate-dominating-file
				    default-directory ".dir-locals.el"))))
	     (unless (member guix-yasnippets yas-snippet-dirs)
	       (add-to-list 'yas-snippet-dirs guix-yasnippets)
	       (yas-reload-all))))
     (eval setq-local guix-directory
	   (locate-dominating-file default-directory ".dir-locals.el"))
     (eval add-to-list 'completion-ignored-extensions ".go"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
