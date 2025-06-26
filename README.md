# My Emacs Configuration (福福的emacs配置文件)

This configuration is based on [Emacs Prelude](https://github.com/bbatsov/prelude) with personal customizations.

## Overview

- **Base:** Emacs Prelude
- **Completion:** Helm
- **Theme:** `leuven-dark`
- **Font:** `Maple Mono NF` (ultra-condensed)
- **Keybinding:** `C-c r` reloads the init file (`~/.emacs.d/init.el`).

## Structure

- `init.el`: Main entry point, loads Prelude, custom files, and sets basic theme/font.
- `rc.el`: Contains custom functions and package requirements (e.g., `rc/require 'helm`).
- `path.el`: Defines relevant directory paths.
- `prelude/`: The core Emacs Prelude distribution (managed as a git submodule).
- `elpa/`: Installed packages.

## Prerequisites

- Git (for managing submodules)
- Emacs 28 or later (recommended)

## Installation

Clone this repository and initialize the submodules:

```bash
git clone <your-repo-url> ~/.emacs.d
cd ~/.emacs.d
git submodule update --init --recursive
```

## Recommended Additional Packages

Consider installing these popular packages for enhanced functionality:
- `magit`: Powerful Git integration
- `org`: Extensive organizational and note-taking tool
- `company`: Modular text completion framework

## Troubleshooting

- **Package issues:** If packages fail to install or load, refresh your package contents by running `M-x package-refresh-contents`.
- **Theme issues:** If the theme doesn't load, ensure it's installed with `M-x list-packages` or `M-x package-install`.

## Quick Reference

| Command             | Keybinding          | Description                         |
|---------------------|---------------------|-------------------------------------|
| Reload init file    | `C-c r`             | Reload your Emacs configuration     |
| Find file           | `C-x C-f`           | Open a file                         |
| Save buffer         | `C-x C-s`           | Save changes                        |
| Quit Emacs          | `C-x C-c`           | Exit Emacs                          |
| List buffers        | `C-x C-b`           | Show all open buffers               |
| Switch buffer       | `C-x b`             | Switch to another buffer            |
| Helm completion     | (various commands)  | Enhanced completion interface       |

## Customizing Your Environment

### Changing Theme

To try a new theme interactively:
- Run `M-x customize-themes` and select your preferred theme.

To permanently set a theme, add to your `init.el`:

```elisp
(load-theme 'your-theme-name t)
```

### Changing Font

Change your default font by editing the `rc.el` or adding the following line to your `init.el`:

```elisp
(add-to-list 'default-frame-alist '(font . "Your Preferred Font-Size"))
```

## Emacs Initialization

Emacs reads configuration instructions from an initialization file (`init.el`) located typically at `~/.emacs.d/init.el`. If it doesn't exist, create it with:

```bash
mkdir ~/.emacs.d
touch ~/.emacs.d/init.el
```

You can organize your configuration by splitting it into multiple `.el` files and loading them from your `init.el`:

```elisp
(load-file (expand-file-name "my-settings.el" user-emacs-directory))
```

## Essential Commands

- `C-p`: Move cursor up one line
- `C-n`: Move cursor down one line
- `C-a`: Move cursor to the beginning of the line
- `C-e`: Move cursor to the end of the line
- `M-x`: Execute command by name
- `C-g`: Quit/cancel current operation or minibuffer prompt

## Running Emacs as a Daemon (Server)

For faster startup times, run Emacs as a daemon:

```bash
emacs --daemon
```

Then use `emacsclient` to connect quickly:

```bash
emacsclient -c  # graphical
emacsclient -t  # terminal
```

## References

- [Emacs Official Documentation](https://www.gnu.org/software/emacs/manual/)
- [Emacs Prelude GitHub](https://github.com/bbatsov/prelude)
- [Helm Documentation](https://emacs-helm.github.io/helm/)

