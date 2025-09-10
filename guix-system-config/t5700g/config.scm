;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu)
	     (gnu packages mail)
	     (gnu packages shells)
	     (gnu services mail)
	     (gnu services virtualization)
	     (gnu services desktop)
	     (gnu system accounts)
	     (gnu system setuid)
	     (nongnu packages linux)
             (nongnu system linux-initrd)
	     )

(use-service-modules base containers desktop networking ssh xorg networking)
(use-package-modules security-token)
(operating-system
  (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (locale "en_US.utf8")
  (timezone "America/Los_Angeles")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "t5700g")
  (kernel-arguments
   (append (list "user_namespace.enable=1" "systemd.unified_cgroup_hierarchy=1")
           %default-kernel-arguments))
  (setuid-programs
   (append (list (setuid-program
                  (program (file-append opensmtpd "/sbin/smtpctl"))
		  (setuid? #f)
		  (setgid? #t)
		  (user "root")
		  (group "smtpq"))
		 (setuid-program
                  (program (file-append opensmtpd "/sbin/sendmail"))
		  (setuid? #f)
		  (setgid? #t)
		  (user "root")
		  (group "smtpq")))
           %setuid-programs))
  
  (users (cons* (user-account
                 (name "timmy")
		 (shell (file-append zsh "/bin/zsh"))
                  (comment "timmy")
                  (group "users")
                  (home-directory "/home/timmy")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video" "input" "libvirt" "kvm" "plugdev")))
                %base-user-accounts))
  (packages
    (append
      (map specification->package (list "zsh"))
      %base-packages))
  (services
    (append
     (list 
	   (service rootless-podman-service-type
              (rootless-podman-configuration
                (subgids (list (subid-range (name "timmy"))))
                (subuids (list (subid-range (name "timmy"))))))
           (service openssh-service-type)
	   (service seatd-service-type)
	   (service network-manager-service-type)
	   (service wpa-supplicant-service-type)
	   (udev-rules-service 'fido2 libfido2 #:groups '("plugdev"))
	   (service opensmtpd-service-type
		    (opensmtpd-configuration
		     (config-file (local-file "/etc/opensmtpd.conf"))))
	   (service ntp-service-type)
	    (service libvirt-service-type
		     (libvirt-configuration
		      (unix-sock-group "libvirt")
		      (tls-port "16555"))))
      %base-services))
  (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets (list "/boot/efi"))
      (keyboard-layout keyboard-layout)))
  (file-systems
    (cons* (file-system
             (mount-point "/")
	     (flags '(lazy-time))
             (device
               (uuid "5e3cf9e9-ebfd-4e2b-b39d-ebc097a36dd8"
                     'btrfs))
             (type "btrfs"))
	   (file-system
             (mount-point "/mnt/backup")
	     (flags '(lazy-time))
             (device
               (uuid "69d03e6e-55c3-4c52-8602-1100b9db0feb"
                     'btrfs))
             (type "btrfs"))
           (file-system
             (mount-point "/boot/efi")
             (device (uuid "DE22-AB01" 'fat32))
             (type "vfat"))
           %base-file-systems)))
