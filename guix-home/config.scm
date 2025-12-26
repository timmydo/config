;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

;; guix home reconfigure ~/.config/guix-home/config.scm

(use-modules (gnu home)
             (gnu home services guix)
             (guix channels)
             (gnu packages)
             (gnu services)
             (guix gexp)
             (gnu home services shells)
             (srfi srfi-1)
             ;; Modules required for custom packages
             (guix packages)
             (guix download)
             (guix git-download)
             (guix build-system gnu)
             (guix build-system go)
             ((guix licenses) #:prefix license:)
             (gnu packages base)
             (gnu packages gcc)
             (gnu packages commencement)
             (gnu packages elf)
             (gnu packages compression)
             (gnu packages golang-crypto)
             (gnu packages golang-web)
             (gnu packages golang-xyz)
             (gnu packages golang-build))

;; --- Custom Rust Nightly Definition ---

(define rust-nightly-date "2025-10-03")

(define rust-src-nightly
  (origin
    (method url-fetch)
    (uri (string-append
          "https://static.rust-lang.org/dist/"
          rust-nightly-date
          "/rust-src-nightly.tar.xz"))
    (sha256
     (base32
      "11bzbil0crzq6p9jq3a78bz0g3hhdcwin8gxk2d6f6kzs63mgd41"))))

(define-public rust-nightly
  (package
    (name "rust-nightly")
    (version rust-nightly-date)
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://static.rust-lang.org/dist/"
             rust-nightly-date
             "/rust-nightly-x86_64-unknown-linux-gnu.tar.gz"))
       (sha256
        (base32
         "1ww9mpcp314q4nk7ykp2blkvw66zmiy4c01v02fg0asrdh17vspr"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:validate-runpath? #f
      #:strip-binaries? #f
      #:modules '((guix build gnu-build-system)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'build)
          (add-after 'unpack 'unpack-rust-src
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((rust-src (assoc-ref inputs "rust-src")))
                (invoke "tar" "-xf" rust-src))))
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (invoke "./install.sh"
                        (string-append "--prefix=" out)
                        "--components=rustc,cargo,rust-std-x86_64-unknown-linux-gnu,rustfmt-preview,clippy-preview")
                (chdir "rust-src-nightly")
                (invoke "./install.sh"
                        (string-append "--prefix=" out)
                        "--components=rust-src")
                (chdir ".."))))
          (add-after 'install 'patch-rust-src-checksums
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (library-lock (string-append out "/lib/rustlib/src/rust/library/Cargo.lock"))
                     (dummy-hash "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
                (when (file-exists? library-lock)
                  (substitute* library-lock
                    (("(checksum = )\"[a-f0-9]+\"" all prefix)
                     (string-append prefix "\"" dummy-hash "\"")))))))
          (add-after 'install 'patch-binaries
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (gcc-toolchain (assoc-ref inputs "gcc-toolchain"))
                     (zlib (assoc-ref inputs "zlib"))
                     (rpath (string-append out "/lib:"
                                          gcc-toolchain "/lib:"
                                          zlib "/lib")))
                (for-each
                 (lambda (file)
                   (when (and (file-exists? file)
                              (not (file-is-directory? file))
                              (elf-file? file))
                     (invoke "patchelf" "--set-rpath" rpath file)
                     (unless (string-contains file ".so")
                       (invoke "patchelf" "--set-interpreter"
                               (string-append gcc-toolchain
                                              "/lib/ld-linux-x86-64.so.2")
                               file))))
                 (find-files out ".*")))))
          (add-after 'patch-binaries 'wrap-programs
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (gcc-toolchain (assoc-ref inputs "gcc-toolchain"))
                     (lib-path (string-append gcc-toolchain "/lib")))
                (wrap-program (string-append out "/bin/rustc")
                  `("LIBRARY_PATH" ":" suffix (,lib-path)))
                (wrap-program (string-append out "/bin/cargo")
                  `("LIBRARY_PATH" ":" suffix (,lib-path))
                  `("RUSTFLAGS" " " suffix (,(string-append "-C link-arg=-Wl,-rpath," lib-path))))))))))
    (native-inputs
     `(("patchelf" ,patchelf)
       ("rust-src" ,rust-src-nightly)))
    (inputs
     (list gcc-toolchain zlib))
    (home-page "https://www.rust-lang.org")
    (synopsis "Rust nightly toolchain")
    (description "Rust nightly binary")
    (license (list license:asl2.0 license:expat))))

;; --- Custom Go Package: gofeed ---

(define-public go-github-com-mmcdole-gofeed
  (package
    (name "go-github-com-mmcdole-gofeed")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mmcdole/gofeed")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "03cmj4wk6yicv5pqxwa3sbqxxbw3srx2j5c9938yv0ydkccnlyhq"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/mmcdole/gofeed"
       #:tests? #f))
    (propagated-inputs
     (list go-github-com-puerkitobio-goquery
           go-github-com-json-iterator-go
           go-github-com-mmcdole-goxpp
           go-golang-org-x-net
           go-golang-org-x-text))
    (home-page "https://github.com/mmcdole/gofeed")
    (synopsis "Parse RSS, Atom and JSON feeds in Go")
    (description "The gofeed library is a robust feed parser that supports
parsing both RSS, Atom and JSON feeds.")
    (license license:expat)))

;; --- Custom Go Package: feed2maildir ---

(define-public feed2maildir
  (package
    (name "feed2maildir")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/timmydo/feed2maildir.git")
             (commit "01f3e72d380b5ab11dbcd7cda846397ea38c4cfb")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0b25a9wkvw4qmsijhq6avrbm82rmp04vpp3631lpn2x9zwa2mjdn"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/timmydo/feed2maildir"))
    (propagated-inputs
     (list go-github-com-cespare-xxhash
           go-github-com-mmcdole-gofeed))
    (home-page "https://github.com/timmydo/feed2maildir")
    (synopsis "Convert RSS/Atom feeds to maildir format")
    (description "A Go program that fetches RSS/Atom feeds and stores them in maildir format.")
    (license license:expat)))

;; --- Home Environment ---

(home-environment
 (packages
  (cons* rust-nightly feed2maildir ;; Custom packages
         (specifications->packages 
         (list "aerc" "texinfo" "procps" "bubblewrap" "node" "mpv"
               "xdg-desktop-portal-gtk" "xdg-desktop-portal-wlr"
               "python-yubikey-manager" "emacs-next-pgtk" "eog" "obs-wlrobs"
               "obs" "pipewire" "git-lfs" "evince" "imv" "virt-manager"
               "qemu" "gst-plugins-good" "flatpak" "gst-plugins-base"
               "pulseaudio" "i3status" "pavucontrol" "git:credential-netrc"
               "git:send-email" "pinentry" "password-store" "notmuch"
               "git" "perl" "sbcl" "emacs-pgtk" "perf" "clang-toolchain"
               "wireplumber-minimal" "openssh-sans-x" "opensmtpd" "bind:utils"
               "gcc-toolchain" "pandoc" "sway" "swayr" "swayidle" "dbus"
               "runc" "smartmontools" "go" "catimg" "w3m" "imagemagick"
               "mesa" "btrfs-progs" "foot" "slurp" "xdg-utils" "fontconfig"
               "grim" "font-gnu-freefont" "cifs-utils" "python-pygobject"
               "htop" "gstreamer" "gnupg" "gdb" "python" "ccls" "rsync"
               "cpio" "bzip2" "ntp" "curl" "ghostscript" "groff" "lsof"
               "ispell" "alsa-utils" "xxd" "isync" "jmtpfs" "strace" "zsh"
               "util-linux" "unzip" "execline" "s6" "patchelf" "scdoc"
               "expat" "utf8proc" "ncurses" "font-wqy-zenhei" "font-dejavu"
               "man-pages" "less" "lem" "grep" "msmtp" "thunar" "whisper-cpp"
               "wl-clipboard" "ydotool" "podman" "coreutils" "sed" "findutils"
               "inetutils" "ripgrep" "shepherd" "zstd" "pkg-config" "make"
               "sshfs" "which" "wget" "tar" "gzip" "iproute2" "file" "hugo"))))

 (services
  (append
   (list
   (service home-channels-service-type
            (list (channel
                   (name 'guix)
                   (url "https://git.guix.gnu.org/guix.git")
                   (branch "master")
                   (introduction
                    (make-channel-introduction
                     "9edb3f66fd807b096b48283debdcddccfea34bad"
                     (openpgp-fingerprint
                      "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
                  (channel
                   (name 'nonguix)
                   (url "https://gitlab.com/nonguix/nonguix.git")
                   (branch "master"))))
   (service home-bash-service-type
            (home-bash-configuration
             (environment-variables
              '(("PATH" . "/run/privileged/bin:/home/timmy/.config/guix/current/bin:/home/timmy/bin:/home/timmy/npm/bin:$PATH")
                ("EDITOR" . "emacs")
                ("MOZ_ENABLE_WAYLAND" . "1")
                ("XDG_CONFIG_HOME" . "/home/timmy/.config")
                ("XDG_RUNTIME_DIR" . "/tmp/timmy-xdg")
                ("QT_QPA_PLATFORM" . "wayland")
                ("USER" . "timmy")))
             (aliases
              '(("ls" . "ls --color=auto")
                ("lal" . "ls -al")
                ("ll" . "ls -l")
                ("krmevicted" . "kubectl get po | grep Evicted| awk '{print $1}'| xargs -n 1 kubectl delete pod")
                ("dfh" . "df -h -x squashfs -x tmpfs -x devtmpfs")
                ("claude" . "node /home/timmy/npm/bin/claude --allow-dangerously-skip-permissions")
                ("gemini" . "node /home/timmy/npm/bin/gemini")
                ("copilot" . "node /home/timmy/npm/bin/copilot")
                ("dev" . "~/.config/dev.sh")))
             (bashrc (list (local-file "/home/timmy/.config/guix-home/bashrc-extra.sh")))))
   (service home-zsh-service-type
            (home-zsh-configuration
             (zshrc (list (local-file "/home/timmy/.config/guix-home/zshrc-extra.sh")))
             (zprofile (list )))))
   %base-home-services)))


