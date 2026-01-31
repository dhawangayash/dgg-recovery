## Project `dgg-recovery`

`dgg-recovery` is an Emacs Lisp utility package that streamlines the common “autosave recovery” workflow into a single command.

### Problem it solves

When Emacs crashes or a buffer is left in a bad state, you may have:

* an on-disk file (the “original”), and
* an auto-save file (`#filename#`) containing newer or divergent edits.

Manually recovering and comparing these versions is repetitive and error-prone.

### What the package does

`dgg-recovery` provides one interactive entrypoint:

* **`dgg-recovery-run-all`**:

  1. runs Emacs’s built-in recovery (`recover-this-file`) to load the auto-save content into the current buffer,
  2. snapshots the recovered contents into a dedicated temporary “recovered” buffer,
  3. compares the recovered snapshot against the on-disk file using **Ediff**.

### Usage
#### Install / load ####

Put `dgg-recovery.el` on your `load-path`, then:

```elisp
(require 'dgg-recovery)
```

#### Run the workflow ####

1. Open the file buffer that has an auto-save (`#filename#`) available.
2. Run:

```elisp
M-x dgg-recovery-run-all
```

This will:

* recover from the auto-save into the current buffer,
* snapshot the recovered contents into a temporary “recovered” buffer,
* open **Ediff** between the recovered snapshot and a read-only snapshot of the file on disk.

#### Optional keybinding ####

```elisp
(global-set-key (kbd "C-c r") #'dgg-recovery-run-all)
```

#### Notes ####

* Auto-save files look like `#file#` (often in the same directory or redirected to `/tmp` depending on your Emacs config).
* You exit Ediff with `q`; the temporary “disk snapshot” buffer is cleaned up automatically on exit.
### Key behavior choices

* **Non-destructive comparison**: it does not require overwriting the on-disk file to see what changed.
* **Ediff-based review**: the recovered content is compared interactively, allowing you to inspect and selectively apply changes.
* **Buffer-based diffing (no prompts)**: the Ediff comparison is done between:

  * the recovered snapshot buffer, and
  * a read-only “disk snapshot” buffer loaded from the file,
    so Ediff won’t prompt you to save modified visiting buffers.

### Internal architecture

* Maintains a mapping from origin file path → recovered snapshot buffer.
* Uses internal helper functions (private `dgg-recovery--*` functions) to keep the public surface area small.
* Designed for testability via ERT by stubbing the built-in recovery step and asserting orchestration behavior.

### Intended use cases

* Recovering work after crashes.
* Comparing auto-save state vs disk state before deciding whether to overwrite.
* Auditing “out of sync” situations without forcing a save.

If you want a short README-style paragraph (for MELPA/GitHub), say “write README blurb” and I’ll format it accordingly.

