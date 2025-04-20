# Emacs Initialization

Emacs reads configuration instructions from an initialization file on startup. This file dictates its behavior, appearance, and loaded packages.

## Locating Your Init File

The main configuration file is typically `init.el`, located in `~/.emacs.d/`.

*   `~/.emacs.d/init.el`: This is the standard, preferred location (within your home directory's `.emacs.d` folder). It uses a dedicated directory to help organize your configuration (`init.el`) and other Emacs-related files (like packages).
*   `~/.emacs`: This is an older alternative, placing a single configuration file directly in your home directory. While potentially simpler for minimal setups, it can clutter the home directory and makes organization harder. `init.el` within `.emacs.d/` is generally recommended and usually takes precedence if both files exist.

## Understanding .el Files

The `.el` extension signifies a file containing Emacs Lisp (Elisp) source code. Emacs reads these files for instructions. This includes:

*   Your configuration file (`init.el`).
*   Code for Emacs packages and extensions.
*   Any custom functions you might write.

## Creating Your Init File

If `~/.emacs.d/` or `init.el` doesn't exist, create them:

*   In a terminal, run `mkdir ~/.emacs.d` (if the directory is missing).
*   Create the file, e.g., using the terminal command `touch ~/.emacs.d/init.el` or within Emacs itself (`C-x C-f ~/.emacs.d/init.el`).

## Organizing Your Configuration

As your configuration grows, you might want to split it into multiple files for better organization. You can load other `.el` files from your main `init.el`.

*   **Loading Files:** A common way to load another configuration file (e.g., `my-settings.el`) located in the same directory (`~/.emacs.d/`) is to add this line to your `init.el`:
    ```elisp
    (load-file (expand-file-name "my-settings.el" user-emacs-directory))
    ```
    This executes the code in `my-settings.el` as if it were part of `init.el`.

## Your First Configuration

Let's add a simple setting to `init.el` to disable the startup message screen.

Open `~/.emacs.d/init.el` in Emacs (`C-x C-f`, type the path, Enter) and add:

```elisp
(setq inhibit-startup-message t)
```

*   **Explanation:** This Lisp code uses `setq` to assign a value to a variable. `inhibit-startup-message` controls the startup screen, and `t` represents "true", so this disables the screen.

## Applying Configuration Changes

After modifying your `init.el`, you need to apply the changes. There are several ways:

*   **Restart Emacs (`C-x C-c`)**:
    *   **Strength:** Most reliable, guarantees a clean state.
    *   **Weakness:** Slowest method.
*   **Evaluate the Buffer (`M-x eval-buffer`)**:
    *   **Prerequisite:** The `init.el` file must be the active buffer.
    *   **Strength:** Faster than restarting. Applies changes from the current buffer.
    *   **Weakness:** Requires file open, might not perfectly mimic restart, only acts on the current buffer's code.
*   **Load the Init File (`M-x load-user-init-file`)**:
    *   **Strength:** Fast, specifically targets your `init.el` regardless of the active buffer.
    *   **Weakness:** Might not perfectly mimic restart, command might not be available on all systems (custom function workaround exists if needed).

## Extending Emacs: Package Management & Themes

Most additional features in Emacs, such as advanced language support, custom themes, and specialized tools, are added via packages. Emacs includes a built-in package management system, `package.el`, which allows you to find, install, and manage these packages from online repositories (like MELPA or GNU ELPA). Configuring `package.el` early in your `init.el` is a common next step after basic setup.

*   **Themes:** Themes control colors and styles. They are often installed as packages.
    *   **Finding/Trying:** Use `M-x customize-themes` to interactively preview all installed themes. Use `M-x load-theme` (with TAB completion) to try loading a specific theme by name. Find new themes via `M-x list-packages` (requires package archives like MELPA to be configured) or online browsing.
    *   **Setting:** Add `(load-theme 'theme-name t)` to your `init.el` to load a theme automatically.

## Understanding Emacs Modes

Modes are fundamental to how Emacs adapts its behavior:

*   **Major Modes:** Define the primary behavior for a buffer, usually based on file type (e.g., `python-mode`, `text-mode`, `org-mode`). A buffer typically has one active major mode, providing syntax highlighting, indentation, and relevant commands.
*   **Minor Modes:** Provide additional, optional features layered on top of any major mode (e.g., `display-line-numbers-mode`, `show-paren-mode`, `evil-mode` for Vim emulation). Multiple minor modes can be active globally or per-buffer. They are toggled on/off, often via function calls in `init.el`.
*   **Perspective:** Modes make Emacs highly adaptable (**strength**) but learning which modes exist and how they interact takes time (**weakness**).

## Basic Interaction Concepts

Understanding these core Emacs concepts is helpful:

*   **Minibuffer:** The line at the bottom of the frame used for command input (`M-x`), file paths (`C-x C-f`), prompts, and messages.
*   **Buffer vs. Window:** A buffer holds text content (like a file or `*scratch*`). A window is a viewport displaying a buffer within the main Emacs frame (OS window). You can have many buffers open, but view them through one or more windows.
*   **Managing Windows and Buffers:**
    *   `C-x b`: Switch which buffer is shown in the current window (`switch-to-buffer`).
    *   `C-x o`: Move focus to the next window (`other-window`).
    *   `C-x 2`: Split the current window vertically (one above, one below) (`split-window-below`).
    *   `C-x 3`: Split the current window horizontally (side-by-side) (`split-window-right`).
    *   `C-x 0`: Delete the current window (`delete-window`).
    *   `C-x 1`: Delete all windows except the current one (`delete-other-windows`).
    *   `C-x C-b`: List all open buffers (`list-buffers`).
*   **Kill/Yank (Copy/Paste):** Emacs uses "killing" (cutting `C-w` or copying `M-w`) text to the "kill ring", and "yanking" (`C-y`) to paste. The kill ring stores a history, accessible via `M-y` after `C-y`.
    *   **Perspective:** Kill ring history is powerful (**strength**) but uses different terminology/workflow than standard OS clipboards (**adjustment needed**).
*   **Undo and Redo:**
    *   `C-/` or `C-_` or `C-x u`: Undo the last change (`undo`). Repeat to undo further back.
    *   **Redo:** Emacs has no single redo key. To undo an undo: perform a non-undo action (like moving the cursor with `C-f`) and then immediately press `C-/` again. This undoes the interruption, effectively redoing the change.
*   **Using the Scratch Buffer:**
    *   The `*scratch*` buffer is a temporary buffer (usually in `lisp-interaction-mode`) not tied to a file.
    *   `C-x C-e`: Evaluate the single Lisp expression before the cursor (`eval-last-sexp`). Result appears in the minibuffer.
    *   `C-j`: Evaluate the expression before the cursor and insert the result into the buffer (`eval-print-last-sexp`). Useful for interactive Elisp.
*   **Getting Help (`describe-function`, `describe-variable`):** Emacs has built-in documentation.
    *   Press `C-h f` (or `M-x describe-function`), type a function name (e.g., `set-face-attribute`), press Enter to see its documentation.
    *   Press `C-h v` (or `M-x describe-variable`), type a variable name (e.g., `line-spacing`), press Enter to see its documentation.

## Essential Commands:

*   `C-p`: Move cursor up one line (`previous-line`).
*   `C-n`: Move cursor down one line (`next-line`).
*   `C-a`: Move cursor to the beginning of the current line (`beginning-of-line`).
*   `C-e`: Move cursor to the end of the current line (`end-of-line`).
*   `C-x C-f`: Find file.
*   `C-x C-s`: Save buffer.
*   `C-x C-c`: Quit Emacs.
*   `M-x`: Execute command by name.
*   `C-g`: Quit/cancel current operation or minibuffer prompt.
    *   *(Note: Arrow keys and Home/End keys often work as expected too)*

## Running Emacs as a Daemon (Server)

For faster startup times, especially with larger configurations, Emacs can run as a background daemon (or server) process (`emacs --daemon` or `emacs --fg-daemon`).

*   **Concept:** The daemon starts once and loads your full configuration. You then connect instantly using `emacsclient` (`emacsclient -c` for graphical window, `emacsclient -t` for terminal).
*   **Perspective:** Near-instant startup for editing sessions after the initial daemon launch (**strength**), but requires managing the daemon process separately and restarting it after configuration changes (**weakness**).