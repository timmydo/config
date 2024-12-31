;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (gnu home)
             (gnu packages)
             (gnu services)
             (guix gexp)
             (gnu home services shells))

(home-environment
  ;; Below is the list of packages that will show up in your
  ;; Home profile, under ~/.guix-home/profile.
 (packages (specifications->packages (list "texinfo"
					   "procps"
                                            "mpv"
                                            "xdg-desktop-portal-wlr"
                                            "python-yubikey-manager"
                                            "emacs-next-pgtk"
                                            "eog"
                                            "obs-wlrobs"
                                            "obs"
                                            "pipewire"
                                            "git-lfs"
                                            "evince"
                                            "imv"
                                            "virt-manager"
                                            "qemu"
                                            "gst-plugins-good"
                                            "rhythmbox"
                                            "flatpak"
                                            "gst-plugins-base"
                                            "pulseaudio"
                                            "i3status"
                                            "pavucontrol"
                                            "git:credential-netrc"
                                            "git:send-email"
                                            "pinentry"
                                            "password-store"
                                            "notmuch"
                                            "git"
                                            "sbcl"
                                            "emacs-pgtk"
                                            "perf"
                                            "clang-toolchain"
                                            "wireplumber-sans-elogind"
                                            "opensmtpd"
                                            "bind:utils"
                                            "gcc-toolchain"
                                            "pandoc"
                                            "swayr"
                                            "runc"
                                            "smartmontools"
                                            "go"
                                            "catimg"
                                            "w3m"
                                            "libxkbcommon"
                                            "imagemagick"
                                            "mesa"
                                            "btrfs-progs"
                                            "foot"
                                            "slurp"
                                            "xdg-utils"
                                            "fontconfig"
                                            "grim"
                                            "wayland-protocols"
                                            "font-gnu-freefont"
                                            "cifs-utils"
                                            "python-pygobject"
                                            "htop"
                                            "gstreamer"
                                            "gnupg"
                                            "gdb"
                                            "python"
                                            "ccls"
                                            "rsync"
                                            "cpio"
                                            "bzip2"
                                            "ntp"
                                            "curl"
                                            "ghostscript"
                                            "groff"
                                            "lsof"
                                            "ispell"
                                            "alsa-utils"
                                            "xxd"
                                            "isync"
                                            "jmtpfs"
                                            "strace"
                                            "zsh"
                                            "mailutils-timmy"
                                            "util-linux"
                                            "unzip"
                                            "execline"
                                            "s6"
                                            "patchelf"
                                            "scdoc"
                                            "expat"
                                            "utf8proc"
                                            "ncurses"
                                            "font-wqy-zenhei"
                                            "font-dejavu"
                                            "man-pages"
                                            )))

  ;; Below is the list of Home services.  To search for available
  ;; services, run 'guix home search KEYWORD' in a terminal.
  (services
   (list (service home-bash-service-type
                  (home-bash-configuration
                   (aliases '())
                   (bashrc (list (local-file
                                  "/home/timmy/.config/guix-home/.bashrc"
                                  "bashrc")))
                   (bash-profile (list (local-file
                                        "/home/timmy/.config/guix-home/.bash_profile"
                                        "bash_profile")))))
	 (service home-zsh-service-type
		  (home-zsh-configuration
                   (zshrc (list (local-file
                                 "/home/timmy/.config/zsh-startup.sh")))
		   (zprofile (list ))
		   )))))
