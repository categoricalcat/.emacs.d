# Emacs Daemon Services

## Installation

```bash
./install.sh
```

## Usage

Connect to the daemon:
```bash
emacsclient -c
```

## Files

- `install.sh` - Cross-platform installer
- `install-linux.sh` - Linux installer
- `install-macos.sh` - macOS installer
- `emacs-daemon.service` - Linux service file
- `org.gnu.emacs.daemon.plist` - macOS service file
