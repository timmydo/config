;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu)
	     (gnu packages shells)
	     (nongnu packages linux)
             (nongnu system linux-initrd))

(use-service-modules desktop networking ssh xorg)

(operating-system
  (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (locale "en_US.utf8")
  (timezone "America/Los_Angeles")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "t5700g")
  (users (cons* (user-account
                 (name "timmy")
		 (shell (file-append zsh "/bin/zsh"))
                  (comment "timmy")
                  (group "users")
                  (home-directory "/home/timmy")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video" "input")))
                %base-user-accounts))
  (packages
    (append
      (map specification->package (list "nss-certs" "zsh"))
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
    (list (uuid "166071b2-30d2-4d64-a0c8-40c308a7f16c")))
  (file-systems
    (cons* (file-system
             (mount-point "/")
             (device
               (uuid "5e3cf9e9-ebfd-4e2b-b39d-ebc097a36dd8"
                     'btrfs))
             (type "btrfs"))
           (file-system
             (mount-point "/boot/efi")
             (device (uuid "DE22-AB01" 'fat32))
             (type "vfat"))
           %base-file-systems)))
