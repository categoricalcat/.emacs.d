# 福福的emacs配置文件
<small>_lucky emacs configuration_</small>

## What is this?

A [Prelude](https://github.com/bbatsov/prelude)-based Emacs config with TRAMP, Treemacs,
Magit integration and a network-accessible Emacs server for multi-machine editing.

## Features

- Based on **Prelude** (sensible defaults, curated packages).
- **Vertico** completion.
- Theme: **modus-vivendi** / modus-operandi (toggle with `modus-themes-toggle`).
- Font: **Maple Mono NF CN** ultra-condensed (ideal for CJK).
- `C-c r` reloads the whole config in-place.
- Network Emacs server — edit on machine A from machine B over LAN.
- Systemd / launchctl service files to run Emacs daemon.
- GNU Stow for linking config into Prelude's `personal/` directory.

## Repository Layout

```
.emacs.d/
├── early-init.el          → GC/frame setup before init
├── init.el                → Entry point — loads Prelude & personal modules
├── prelude-modules.el     → Which Prelude modules to enable
├── lisp/                  → Personal elisp modules (stowed into prelude/personal/)
│   ├── path.el            → Path helpers
│   ├── femacs-tramp.el    → TRAMP + Magit config
│   ├── femacs-server.el   → TCP server for LAN access
│   ├── femacs-shell.el    → EAT terminal setup
│   └── treemacs-config.el → Treemacs + projectile/magit/tramp integration
├── prelude/               → Git submodule — upstream Prelude (fork)
├── scripts/
│   └── service-emacs.sh   → Daemon start/stop/attach helpers
├── services/              → systemd & launchctl service files
├── connect-remote-emacs.sh→ SSH tunnel + emacsclient to remote daemon
├── Makefile               → install / link / start / stop targets
└── docs/                  → Notes
```

## Installation

### Prerequisites

- **Emacs 30.1+**
- **Git** with submodule support
- **GNU Stow**

### Bootstrap

```bash
git clone https://github.com/<you>/.emacs.d ~/.emacs.d
cd ~/.emacs.d
./install-prelude.sh
```

This runs `git submodule update --init --recursive` and uses `stow` to link
`lisp/` and `prelude-modules.el` into `prelude/personal/`.

Start Emacs normally or run the daemon:

```bash
make start        # emacs --daemon --debug-init
emacsclient -c    # open a frame
```

## Network Server (two machines on the same LAN)

### Machine A — server

```bash
make start        # starts daemon with TCP server on 0.0.0.0:13245
```

The server writes an auth file to `~/.emacs.d/server/femacs`.

### Machine B — client

**Option 1: Direct (LAN, no firewall)**

```bash
# Copy the auth file from machine A
scp A:~/.emacs.d/server/femacs ~/.emacs.d/server/femacs

# Connect
emacsclient -f ~/.emacs.d/server/femacs -c
```

**Option 2: SSH tunnel**

```bash
# Set env vars (or edit defaults in the script)
export REMOTE_USER=fufu
export REMOTE_HOST=192.168.31.18

./connect-remote-emacs.sh           # tunnels + connects
./connect-remote-emacs.sh /some/file
```

The server port defaults to `13245`. Override with `EMACS_SERVER_PORT` env var
(set it on both server and client).

## Everyday Usage

| Task              | Key / Command                      |
|-------------------|------------------------------------|
| Reload config     | `C-c r`                            |
| Find file         | `C-x C-f`                          |
| Switch buffer     | `C-x b`                            |
| Magit status      | `C-x g`                            |
| Treemacs          | `F8` or `C-x t t`                  |
| Toggle theme      | `M-x modus-themes-toggle`          |
| Describe key      | `C-h k`                            |

## Makefile Targets

```
make install    Full bootstrap (submodule + link)
make link       Stow lisp/ into prelude/personal
make unlink     Remove stowed symlinks
make start      Start daemon
make stop       Stop daemon
make restart    Restart daemon
make clean      Unlink + nuke elpa/eln-cache/etc.
make help       Show all targets
```

## Running as a Service

### Linux (systemd)

```bash
cd services && ./install.sh
emacsclient -c
```

### macOS (launchctl)

```bash
cd services && ./install.sh
emacsclient -c
```

## Updating

```bash
cd ~/.emacs.d
git pull
git submodule update --remote --merge
make link
```

## Troubleshooting

- `M-x package-refresh-contents` if packages fail to install.
- Delete `elpa/` to force a clean reinstall.
- `emacs --debug-init` to pinpoint startup errors.
- Check `*Messages*` buffer for TRAMP / server diagnostics.
- TRAMP verbose logging is enabled (`tramp-verbose 10`).

## References

- [Emacs Manual](https://www.gnu.org/software/emacs/manual/)
- [Prelude docs](https://github.com/bbatsov/prelude#readme)
- [Maple Mono Nerd Font](https://github.com/subframe7536/maple-font)
