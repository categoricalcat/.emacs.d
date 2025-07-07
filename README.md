# 福福的emacs配置文件
<small>_lucky emacs configuration_</small>

## What is this?

This is my emacs configuration

## Features

- Based on [Prelude](https://github.com/bbatsov/prelude) (syntax, packages, sensible defaults).
- **Helm** completion everywhere.
- Theme: **`leuven-dark`**.
- Font: **`Maple Mono NF CN`** ultra-condensed (ideal for CJK).
- Key-binding **`C-c r`** reloads the whole config in-place.
- Extra helpers in `rc.el` (package bootstrap, window layout, etc.).
- Systemd / `launchctl` ready service files to run Emacs daemon.
- Scripted installer & symlink manager for dot-files.

## Repository Layout

```
.emacs.d/
 ├ docs/                 → Misc docs & blog posts
 ├ init.el               → Entry point – loads Prelude & personal code
 ├ rc.el                 → Custom helper functions & package setup
 ├ path.el               → Path helpers (XDG, cache dirs, etc.)
 ├ treemacs-config.el    → Treemacs face tweaks
 ├ prelude/              → **Git submodule** with upstream Prelude
 ├ prelude-modules.el    → List of Prelude modules to enable
 ├ scripts/              → Various shell helpers (see below)
 │   ├ service-emacs.sh  → Wrapper to start/stop Emacs server
 │   └ symlinks.sh       → Declarative dot-file linker (reads symlinks.csv)
 ├ symlinks.csv          → `<abs-src>,<abs-dst>` tuples for scripts/symlinks.sh
 └ docs/, vendor/, etc.
```

Feel free to browse – files are short & documented.

## Installation

### 1. Prerequisites

- built on **Emacs 30.1**.
- **Git** + `submodule` support.

### 2. Clone & bootstrap

```bash
# clone into the canonical location so Emacs picks it up automatically
$ git clone https://github.com/<your-fork>/emacs.d ~/.emacs.d
$ cd ~/.emacs.d

# grab Prelude sub-module & other deps
$ ./install-prelude.sh         # ⇢ runs `git submodule update --init --recursive`

# optional – create all declared dot-file symlinks
$ ./scripts/symlinks.sh        # reads symlinks.csv
```

That's it – start Emacs and enjoy.

## Every-day Usage

| Task                     | Key / Command            |
|--------------------------|--------------------------|
| Reload config            | `C-c r`                  |
| Find file (Helm)         | `C-x C-f`                |
| Switch buffer            | `C-x b`                  |
| Magit status             | `C-x g` (if magit)       |
| Describe key             | `C-h k`                  |
| Package refresh          | `M-x package-refresh-contents` |

Most Prelude goodies apply – see its README for full cheatsheet.

## Updating

Prelude and this config evolve. To update **both**:

```bash
cd ~/.emacs.d
# pull personal config
git pull

# pull upstream Prelude into the submodule
git submodule update --remote --merge

# finally re-compile every *.el (optional but keeps things snappy)
emacs --batch -l ~/.emacs.d/init.el -f batch-byte-compile ~/.emacs.d/**/*.el
```

## Running Emacs as a Service

### macOS (launchctl)

```bash
ln -sf ~/.emacs.d/emacs.service ~/Library/LaunchAgents/
launchctl load -w ~/Library/LaunchAgents/emacs.service
```

### systemd (Linux)

```bash
systemctl --user enable ~/.emacs.d/emacs.service
systemctl --user start  emacs
```

Afterwards start GUI/TTY frames with `emacsclient -c` or `-t`.

Scripts under `scripts/service-emacs.sh` wrap common operations.

## Troubleshooting

- If packages fail to install, run `M-x package-refresh-contents`.
- Delete the `elpa/` dir to force a clean reinstall.
- Invoke Emacs with `--debug-init` to pinpoint startup errors.
- Still stuck? – Check `*Messages*` buffer and the issue tracker.

## Contributing

Make an issue? idk

## References

- [Emacs Manual](https://www.gnu.org/software/emacs/manual/)
- [Prelude docs](https://github.com/bbatsov/prelude#readme)
- [Helm project](https://github.com/emacs-helm/helm)
- [Maple Mono Nerd Font](https://github.com/subframe7536/maple-font)
- My notes: see `docs/` directory inside this repo.

---
