;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

;; guix home reconfigure ~/.config/guix-home/config.scm

(use-modules (gnu home)
             (gnu home services)
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
             (gnu packages golang-build)
             (gnu packages linux)
             (gnu packages tls)
             (gnu packages xorg)
             (gnu packages xdisorg)
             (gnu packages gl)
             (gnu packages fontutils)
             (gnu packages vulkan)
             (gnu packages freedesktop)
             (gnu packages sqlite))

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

;; Prebuilt static-musl std batteries (self-contained libc.a + crt objects) so a
;; --target x86_64-unknown-linux-musl build links fully static with no external libc.
(define rust-std-musl-nightly
  (origin
    (method url-fetch)
    (uri (string-append
          "https://static.rust-lang.org/dist/"
          rust-nightly-date
          "/rust-std-nightly-x86_64-unknown-linux-musl.tar.gz"))
    (sha256
     (base32
      "0abjc649p546hf8cqz8lvyb543flw7j1jzyg3ypln4smpbhzljyj"))))

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
          ;; Extract the musl std BEFORE patch-source-shebangs so its install.sh
          ;; `/usr/bin/env bash` shebang gets patched; extracting it in the install
          ;; phase (post-patch) leaves an unresolvable interpreter → execvp 127.
          (add-after 'unpack-rust-src 'unpack-musl-std
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((musl-std (assoc-ref inputs "rust-std-musl")))
                (invoke "tar" "-xf" musl-std))))
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (invoke "./install.sh"
                        (string-append "--prefix=" out)
                        "--components=rustc,cargo,rust-std-x86_64-unknown-linux-gnu,rustfmt-preview,clippy-preview")
                ;; Add the static-musl std target so rustc can build fully-static musl binaries.
                (chdir "rust-std-nightly-x86_64-unknown-linux-musl")
                (invoke "./install.sh"
                        (string-append "--prefix=" out)
                        "--components=rust-std-x86_64-unknown-linux-musl")
                (chdir "..")
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
                              ;; musl self-contained/*.o are ET_REL objects;
                              ;; patchelf refuses them ("wrong ELF type"). Skip.
                              (not (string-suffix? ".o" file))
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
       ("rust-src" ,rust-src-nightly)
       ("rust-std-musl" ,rust-std-musl-nightly)))
    (inputs
     (list gcc-toolchain zlib))
    (home-page "https://www.rust-lang.org")
    (synopsis "Rust nightly toolchain")
    (description "Rust nightly binary")
    (license (list license:asl2.0 license:expat))))

;; --- Home Environment ---

(home-environment
 (packages
  (cons* rust-nightly ;; Custom packages
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
               "sshfs" "which" "wget" "tar" "gzip" "iproute2" "file" "hugo"
	       "postgresql" "bubblewrap" "udiskie"))))

 (services
  (append
   (list
   ;; td's stage0 resolves its seed toolchain from PATH, taking `cc' ahead of
   ;; `gcc': that finds clang-toolchain's clang, which records no RUNPATH for
   ;; the libgcc_s.so.1 rustc asks for, so host build scripts fail to load
   ;; under stage0's cleared environment. Name both toolchains instead. Also
   ;; TD_RUST_HOME, not just TD_CC_HOME: stage0 searches the rust bin dir
   ;; before the cc one, and the profile's bin has clang's `cc'.
   (simple-service 'td-toolchain-env
                   home-environment-variables-service-type
                   `(("TD_RUST_HOME" . ,(file-append rust-nightly))
                     ("TD_CC_HOME" . ,(file-append gcc-toolchain))))
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
              '(("PATH" . "/run/privileged/bin:/home/timmy/.local/bin:/home/timmy/.config/guix/current/bin:/home/timmy/bin:/home/timmy/npm/bin:$PATH")
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
                ("claude" . "/home/timmy/.local/bin/claude")
                ("gemini" . "node /home/timmy/npm/bin/gemini")
                ("copilot" . "node /home/timmy/npm/bin/copilot")
                ("codex" . "node /home/timmy/npm/bin/codex --dangerously-bypass-approvals-and-sandbox")
                ("dev" . "~/.config/dev.sh")
                ("fixclaude" . "~/.config/guix-home/fix-claude.sh")
                ("z8sync" . "~/.config/guix-home/z8sync.sh")))
             (bashrc (list (local-file "/home/timmy/.config/guix-home/bashrc-extra.sh")))))
   (service home-zsh-service-type
            (home-zsh-configuration
             (zshrc (list (local-file "/home/timmy/.config/guix-home/zsh.sh")))
             (zprofile (list )))))
   %base-home-services)))


