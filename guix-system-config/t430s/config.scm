;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu))
(use-service-modules desktop networking ssh xorg)

(operating-system
  (locale "en_US.utf8")
  (timezone "America/Los_Angeles")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "timmy-t430s")
  (users (cons* (user-account
                  (name "timmy")
                  (comment "Timmy")
                  (group "users")
		  (shell "/home/timmy/.guix-profile/bin/zsh")
                  (home-directory "/home/timmy")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video" "input")))
                %base-user-accounts))
  (packages
    (append
     (map specification->package
	  '("nss-certs"
	    "alacritty"
	    "emacs-next-pgtk"
	    "git"
	    "notmuch"
	    "icecat"
	    "zsh"
	    "i3status"
	    "sway"
	    "curl"))
      %base-packages))
  (services
    (append
      (list (service openssh-service-type)
            (service network-manager-service-type)
            (service wpa-supplicant-service-type))
      %base-services))
  (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (target "/boot/efi")
      (keyboard-layout keyboard-layout)))
  (swap-devices
    (list (uuid "3a8b19bd-f826-4d3f-8c48-521d748108a6")))
  (file-systems
    (cons* (file-system
             (mount-point "/boot/efi")
             (device (uuid "0FEC-651D" 'fat32))
             (type "vfat"))
           (file-system
             (mount-point "/")
             (device
               (uuid "3565f88e-5cb9-4369-ba31-f69b74fb9bff"
                     'ext4))
             (type "ext4"))
           %base-file-systems)))
