;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu))
(use-modules (gnu services admin))
(use-modules (gnu services monitoring))
(use-modules (gnu services networking))
(use-modules (gnu services sysctl))
(use-modules (timmy coredns))
(use-service-modules desktop networking ssh xorg)

(define %my-nftables-ruleset
  (plain-file "nftables.conf"
	      "
flush ruleset

table ip nat {
  chain prerouting {
    type nat hook prerouting priority 0; policy accept;
    iif enp2s0 udp dport 53 counter redirect to :1053
  }
  chain postrouting {
    type nat hook postrouting priority 100; policy accept;
    iif enp2s0 ip saddr 10.18.0.0/16 oifname enp4s0 masquerade
    
  }
  chain output {
    type nat hook output priority 100; policy accept;
  }
}

table inet filter {
  chain input {
    type filter hook input priority 0; policy drop;

    # early drop of invalid connections
    ct state invalid counter drop

    # allow established/related connections
    ct state { established, related } counter accept

    # allow from loopback
    iifname lo counter accept

    # allow icmp
    ip protocol icmp counter accept
    ip6 nexthdr icmpv6 counter accept

    # allow ssh
    ip saddr 10.18.0.0/16 tcp dport ssh counter accept

    # allow dns
    ip saddr 10.18.0.0/16 udp dport { 53, 1053 } counter accept

    # DHCPv6-PD
    udp dport dhcpv6-client counter accept

    # reject everything else
    reject with icmpx type port-unreachable
  }
  chain forward {
    type filter hook forward priority 0; policy accept;
  }
  chain output {
    type filter hook output priority 0; policy accept;
  }
}
"))

(define %corefile
  (plain-file "Corefile"
	      "
. {
  bind 10.18.11.4
  prometheus
  errors
  cache
  forward . tls://9.9.9.9 tls://[2620:fe::fe]:853 {
    tls_servername dns.quad9.net
    health_check 15s
  }

  hosts {
    10.18.11.4 timmydns
    fallthrough
  }
}
"))


(define %my-dhcpd-config
  (plain-file "dhcpd.conf"
	      "
option domain-name-servers 10.18.11.4, 10.18.11.2;
default-lease-time 14400;
max-lease-time 28800;


subnet 10.18.0.0 netmask 255.255.0.0 {
  option routers 10.18.11.4;
  pool {
    range 10.18.12.10 10.18.12.250;
  }
}
"))

(modify-services
 %base-services
 (sysctl-service-type config =>
		      (sysctl-configuration
		       (settings (append '(("net.ipv4.ip_forward" . "1")
					   ("net.ipv6.conf.all.forwarding" . "1")
					   ("net.ipv6.conf.enp4s0.accept_ra" . "2"))
					 %default-sysctl-settings)))))

(operating-system
  (locale "en_US.utf8")
  (timezone "America/Los_Angeles")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "timmy-m90n")
  (users (cons* (user-account
                  (name "timmy")
                  (comment "Timmy")
                  (group "users")
                  (home-directory "/home/timmy")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))
  (packages
    (append
     (map specification->package
	  '("nss-certs"
	    "emacs-next-pgtk"
	    "git"
	    "gnupg"
	    "zsh"
	    "coredns"
	    "btrfs-progs"
	    "nftables"
	    "curl"))
      %base-packages))
  (services
    (append
      (list (service openssh-service-type)
	    (service unattended-upgrade-service-type)
	    (service network-manager-service-type)
	    (simple-service '%my-nftables-ruleset etc-service-type (list `("nftables.conf" ,%my-nftables-ruleset)))
	    (simple-service '%corefile etc-service-type (list `("Corefile" ,%corefile)))
	    (service dhcpd-service-type
		     (dhcpd-configuration
		      (config-file %my-dhcpd-config)
		      (version "4")
		      (interfaces '("enp2s0"))))
	    (service coredns-service-type
		     (coredns-configuration (config-file "/etc/Corefile")))
	    (service nftables-service-type
		     (nftables-configuration (ruleset %my-nftables-ruleset)))
            (service wpa-supplicant-service-type))
      %base-services))
  (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (target "/boot/efi")
      (keyboard-layout keyboard-layout)))
  (swap-devices
    (list (uuid "5c18bc14-16e5-4e76-b1b4-9ad71bececbb")))
  (file-systems
    (cons* (file-system
             (mount-point "/")
             (device
               (uuid "c70fdb86-0828-4ddf-a275-21613e9c2c1b"
                     'btrfs))
             (type "btrfs"))
           (file-system
             (mount-point "/boot/efi")
             (device (uuid "BA8F-DDF3" 'fat32))
             (type "vfat"))
           %base-file-systems)))
