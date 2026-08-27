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
             (guix build-system trivial)
             ((guix licenses) #:prefix license:)
             (gnu packages bash)
             (gnu packages base)
             (gnu packages containers)
             (gnu packages gcc)
             (gnu packages commencement)
             (gnu packages elf)
             (gnu packages compression)
             (gnu packages cups)
             (gnu packages golang-crypto)
             (gnu packages golang-web)
             (gnu packages golang-xyz)
             (gnu packages golang-build)
             (gnu packages glib)
             (gnu packages gnome)
             (gnu packages gtk)
             (gnu packages libusb)
             (gnu packages linux)
             (gnu packages nss)
             (gnu packages pulseaudio)
             (gnu packages tls)
             (gnu packages version-control)
             (gnu packages xorg)
             (gnu packages xdisorg)
             (gnu packages gl)
             (gnu packages fontutils)
             (gnu packages vulkan)
             (gnu packages freedesktop)
             (gnu packages sqlite)
             (gnu packages xml))

;; --- ChatGPT Podman launcher ---

;; This bootstrap package installs OpenAI's signed apt repository.  Container
;; rebuilds then upgrade ChatGPT from that repository, so the launcher does not
;; have to patch or execute Debian binaries in the Guix store.
(define chatgpt-bootstrap-version "26.820.60940")

(define chatgpt-bootstrap-deb
  (origin
    (method url-fetch)
    (uri (string-append
          "https://persistent.oaistatic.com/codex-app-prod/linux/deb/"
          "pool/main/c/chatgpt/chatgpt_"
          chatgpt-bootstrap-version
          "_amd64.deb"))
    (sha256
     (base32 "1myzmx9zhidajx5incsrpa2ydxqrr6gcsyhbgvcgh5f5qsl5dn9i"))))

(define chatgpt-containerfile
  (plain-file
   "Containerfile"
   "FROM docker.io/library/debian:13-slim

ARG CHATGPT_UID=1000
ARG CHATGPT_GID=1000
ENV DEBIAN_FRONTEND=noninteractive

COPY chatgpt.deb /tmp/chatgpt.deb

RUN set -eu; \
    apt-get update; \
    apt-get install -y --no-install-recommends \
      ca-certificates git libasound2-plugins libegl1 libpulse0 \
      mesa-va-drivers /tmp/chatgpt.deb; \
    apt-get update; \
    apt-get install -y --no-install-recommends --only-upgrade chatgpt; \
    apt-get clean; \
    rm -f /tmp/chatgpt.deb; \
    rm -rf /var/lib/apt/lists/*

COPY xdg-open /usr/local/bin/xdg-open
RUN chmod 0755 /usr/local/bin/xdg-open

RUN set -eu; \
    group_name=chatgpt; \
    existing_group=\"$(getent group \"${CHATGPT_GID}\" | cut -d: -f1 || true)\"; \
    if [ -n \"${existing_group}\" ]; then group_name=\"${existing_group}\"; \
    else groupadd --gid \"${CHATGPT_GID}\" \"${group_name}\"; fi; \
    useradd --uid \"${CHATGPT_UID}\" --gid \"${group_name}\" \
      --home-dir /home/chatgpt --create-home --shell /bin/sh chatgpt; \
    install -d -o \"${CHATGPT_UID}\" -g \"${CHATGPT_GID}\" \
      /home/chatgpt /run/user/\"${CHATGPT_UID}\" /src

ENV HOME=/home/chatgpt \
    XDG_CONFIG_HOME=/home/chatgpt/.config \
    XDG_CACHE_HOME=/home/chatgpt/.cache \
    XDG_DATA_HOME=/home/chatgpt/.local/share

USER chatgpt
WORKDIR /src
ENTRYPOINT [\"/usr/bin/chatgpt\"]
"))

(define chatgpt-xdg-open
  (plain-file
   "xdg-open"
   "#!/bin/sh
set -eu

if [ \"$#\" -ne 1 ]; then
  printf 'xdg-open: expected exactly one URI or path\n' >&2
  exit 2
fi

exec /usr/bin/gio open \"$1\"
"))

(define chatgpt-launcher-template
  (plain-file
   "chatgpt.in"
   "#!@BASH@
set -euo pipefail

readonly PODMAN=\"@PODMAN@\"
readonly DBUS_PROXY=\"@DBUS_PROXY@\"
readonly COREUTILS=\"@COREUTILS@\"
readonly CONTEXT=\"@CONTEXT@\"
readonly IMAGE=\"localhost/chatgpt-desktop:debian13-v1\"

die() {
  printf 'chatgpt: %s\\n' \"$*\" >&2
  exit 1
}

podman_ready() {
  if ! \"$PODMAN\" info >/dev/null; then
    die 'Podman is unavailable. Check /etc/subuid and /etc/subgid, then run podman system migrate.'
  fi
}

build_image() {
  local -a build_args=(build --pull=always)
  if [[ \"${1:-}\" == force ]]; then
    build_args+=(--no-cache)
  fi
  build_args+=(
    --build-arg \"CHATGPT_UID=${EUID}\"
    --build-arg \"CHATGPT_GID=$(\"$COREUTILS/bin/id\" -g)\"
    --tag \"$IMAGE\"
    --file \"$CONTEXT/Containerfile\"
    \"$CONTEXT\"
  )
  \"$PODMAN\" \"${build_args[@]}\"
}

podman_ready

readonly launcher_name=\"${0##*/}\"

if [[ \"$launcher_name\" == chatgpt-update || \"${1:-}\" == --rebuild-container ]]; then
  [[ \"${1:-}\" == --rebuild-container ]] && shift
  [[ $# -eq 0 ]] || die 'chatgpt-update does not accept application arguments'
  build_image force
  exit 0
fi

if ! \"$PODMAN\" image exists \"$IMAGE\"; then
  build_image
fi

readonly host_runtime=\"${XDG_RUNTIME_DIR:-/run/user/${EUID}}\"
readonly wayland_display=\"${WAYLAND_DISPLAY:-wayland-0}\"
readonly wayland_socket=\"${host_runtime}/${wayland_display}\"
readonly source_dir=\"${CHATGPT_SOURCE_DIR:-${HOME}/src}\"
readonly service_dir=/srv
readonly state_root=\"${CHATGPT_CONTAINER_STATE_DIR:-${XDG_DATA_HOME:-${HOME}/.local/share}/chatgpt-container}\"
readonly state_home=\"${state_root}/home\"
readonly proxy_dir=\"${host_runtime}/chatgpt-podman\"
readonly container_runtime=\"/run/user/${EUID}\"

[[ -d \"$host_runtime\" ]] || die \"XDG_RUNTIME_DIR does not exist: $host_runtime\"
[[ -S \"$wayland_socket\" ]] || die \"Wayland socket does not exist: $wayland_socket\"
[[ \"$wayland_display\" != */* ]] || die \"invalid Wayland display name: $wayland_display\"
[[ -d \"$source_dir\" ]] || die \"source directory does not exist: $source_dir\"
[[ -d \"$service_dir\" ]] || die \"service directory does not exist: $service_dir\"
[[ -n \"${DBUS_SESSION_BUS_ADDRESS:-}\" ]] || die 'DBUS_SESSION_BUS_ADDRESS is unset'

\"$COREUTILS/bin/mkdir\" -p \"$state_home\" \"$proxy_dir\"
\"$COREUTILS/bin/chmod\" 700 \"$state_root\" \"$state_home\" \"$proxy_dir\"

runtime_dir=
proxy_pid=
cleanup() {
  if [[ -n \"$proxy_pid\" ]]; then
    kill \"$proxy_pid\" 2>/dev/null || true
    wait \"$proxy_pid\" 2>/dev/null || true
  fi
  if [[ -n \"$runtime_dir\" && \"$runtime_dir\" == \"${proxy_dir}/runtime.\"* ]]; then
    \"$COREUTILS/bin/rm\" -rf -- \"$runtime_dir\"
  fi
}
trap cleanup EXIT

runtime_dir=\"$(\"$COREUTILS/bin/mktemp\" -d \"${proxy_dir}/runtime.XXXXXX\")\"
readonly runtime_dir
readonly proxy_socket=\"${runtime_dir}/bus\"
\"$COREUTILS/bin/chmod\" 700 \"$runtime_dir\"
\"$COREUTILS/bin/touch\" \"${runtime_dir}/${wayland_display}\"

proxy_args=(
  \"$DBUS_SESSION_BUS_ADDRESS\"
  \"$proxy_socket\"
  --filter
  --talk=org.freedesktop.portal.Desktop
  --talk=org.freedesktop.portal.Documents
  --talk=org.freedesktop.portal.FileTransfer
  --talk=org.freedesktop.Notifications
)
\"$DBUS_PROXY\" \"${proxy_args[@]}\" &
proxy_pid=$!

for ((attempt = 0; attempt < 100; attempt++)); do
  [[ -S \"$proxy_socket\" ]] && break
  kill -0 \"$proxy_pid\" 2>/dev/null || {
    wait \"$proxy_pid\" || true
    die 'the filtered D-Bus proxy exited before creating its socket'
  }
  \"$COREUTILS/bin/sleep\" 0.02
done
[[ -S \"$proxy_socket\" ]] || die 'timed out waiting for the filtered D-Bus proxy'

readonly host_gid=\"$(\"$COREUTILS/bin/id\" -g)\"
run_args=(
  run --rm
  --name \"chatgpt-${EUID}-$$\"
  --hostname chatgpt
  --userns keep-id
  --user \"${EUID}:${host_gid}\"
  --group-add keep-groups
  --cap-drop ALL
  --security-opt no-new-privileges
  --read-only
  --pids-limit 4096
  --shm-size 1g
  --tmpfs /tmp:rw,nosuid,nodev,size=2g
  --env \"HOME=/home/chatgpt\"
  --env \"XDG_RUNTIME_DIR=${container_runtime}\"
  --env \"WAYLAND_DISPLAY=${wayland_display}\"
  --env \"DBUS_SESSION_BUS_ADDRESS=unix:path=${container_runtime}/bus\"
  --env GTK_USE_PORTAL=1
  --env GIO_USE_PORTALS=1
  --env ELECTRON_DISABLE_SANDBOX=1
  --env ELECTRON_OZONE_PLATFORM_HINT=wayland
  --env \"LANG=${LANG:-C.UTF-8}\"
  --env \"XDG_CURRENT_DESKTOP=${XDG_CURRENT_DESKTOP:-}\"
  --volume \"${state_home}:/home/chatgpt:rw\"
  --volume \"${source_dir}:/src:rw\"
  --volume \"${service_dir}:${service_dir}:rw\"
  --volume \"${runtime_dir}:${container_runtime}:rw\"
  --volume \"${wayland_socket}:${container_runtime}/${wayland_display}:ro\"
  --workdir /src
)

if [[ -S \"${host_runtime}/pipewire-0\" ]]; then
  \"$COREUTILS/bin/touch\" \"${runtime_dir}/pipewire-0\"
  run_args+=(
    --env PIPEWIRE_REMOTE=pipewire-0
    --volume \"${host_runtime}/pipewire-0:${container_runtime}/pipewire-0:ro\"
  )
fi

if [[ -S \"${host_runtime}/pulse/native\" ]]; then
  \"$COREUTILS/bin/mkdir\" -p \"${runtime_dir}/pulse\"
  run_args+=(
    --env \"PULSE_SERVER=unix:${container_runtime}/pulse/native\"
    --volume \"${host_runtime}/pulse:${container_runtime}/pulse:ro\"
  )
fi

if [[ -d \"${host_runtime}/doc\" ]] &&
   \"$COREUTILS/bin/stat\" -f \"${host_runtime}/doc\" >/dev/null 2>&1; then
  \"$COREUTILS/bin/mkdir\" -p \"${runtime_dir}/doc\"
  run_args+=(--volume \"${host_runtime}/doc:${container_runtime}/doc:rw\")
elif [[ -d \"${host_runtime}/doc\" ]]; then
  printf 'chatgpt: document portal filesystem rejects statfs; continuing without its direct mount\n' >&2
fi

for render_node in /dev/dri/renderD*; do
  [[ -c \"$render_node\" ]] && run_args+=(--device \"$render_node\")
done

if [[ \"$launcher_name\" == chatgpt-login ]]; then
  printf 'chatgpt: login mode shares the host network for the localhost OAuth callback\n' >&2
  run_args+=(--network host)
fi

run_args+=(\"$IMAGE\" --ozone-platform=wayland --password-store=basic --no-sandbox)
\"$PODMAN\" \"${run_args[@]}\" \"$@\"
"))

(define-public chatgpt
  (package
    (name "chatgpt-container")
    (version chatgpt-bootstrap-version)
    (source chatgpt-bootstrap-deb)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((out #$output)
                 (bin (string-append out "/bin"))
                 (share (string-append out "/share/chatgpt-container"))
                 (applications (string-append out "/share/applications"))
                 (launcher (string-append bin "/chatgpt")))
            (mkdir-p bin)
            (mkdir-p share)
            (mkdir-p applications)
            (copy-file #$chatgpt-launcher-template launcher)
            (substitute* launcher
              (("@BASH@") #$(file-append bash-minimal "/bin/bash"))
              (("@PODMAN@") #$(file-append podman "/bin/podman"))
              (("@DBUS_PROXY@")
               #$(file-append xdg-dbus-proxy "/bin/xdg-dbus-proxy"))
              (("@COREUTILS@") #$(file-append coreutils))
              (("@CONTEXT@") share))
            (chmod launcher #o555)
            (symlink "chatgpt" (string-append bin "/chatgpt-update"))
            (symlink "chatgpt" (string-append bin "/chatgpt-login"))
            (copy-file #$chatgpt-containerfile
                       (string-append share "/Containerfile"))
            (copy-file #$chatgpt-bootstrap-deb
                       (string-append share "/chatgpt.deb"))
            (copy-file #$chatgpt-xdg-open
                       (string-append share "/xdg-open"))
            (call-with-output-file
                (string-append applications "/chatgpt.desktop")
              (lambda (port)
                (display
                 "[Desktop Entry]\nType=Application\nName=ChatGPT\nComment=Run ChatGPT in a rootless Podman container\nExec=chatgpt\nTryExec=chatgpt\nTerminal=false\nCategories=Development;Utility;\nStartupNotify=true\n"
                 port)))))))
    (home-page "https://chatgpt.com")
    (synopsis "Rootless Podman launcher for the ChatGPT Linux app")
    (description
     "This package builds and launches OpenAI's Debian ChatGPT application in
a rootless Podman container.  It exposes only the selected source directory,
private application state, filtered portal D-Bus access, Wayland, audio, and
DRM render nodes.  The chatgpt-update command rebuilds from OpenAI's signed apt
repository.  The chatgpt-login command temporarily uses host networking so a
host browser can return the OAuth callback to the application.")
    (license #f)))

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
  (cons* rust-nightly chatgpt ;; Custom packages
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
   (simple-service 'flatpak-xdg-data-dirs
                   home-environment-variables-service-type
                   '(("XDG_DATA_DIRS"
                      . "$HOME/.local/share/flatpak/exports/share:/var/lib/flatpak/exports/share:$XDG_DATA_DIRS")))
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
