;;; dgg-recovery.el --- Recover autosave -> temp buffer -> diff -*- lexical-binding: t; -*-

;; Package: dgg-recovery
;; Public entrypoint: `dgg-recovery-run-all`
;;
;; Suggested usage:
;;   (require 'dgg-recovery)
;;   (global-set-key (kbd "C-c r") #'dgg-recovery-run-all)

(require 'cl-lib)
(require 'ediff)

(defgroup dgg-recovery nil
  "Recover from auto-save, snapshot recovered contents, revert to disk, and ediff."
  :group 'files
  :prefix "dgg-recovery-")

(defcustom dgg-recovery-make-temp-buffer-read-only t
  "If non-nil, mark the recovered snapshot buffer read-only."
  :type 'boolean
  :group 'dgg-recovery)

(defcustom dgg-recovery-kill-recovered-buffer-on-exit t
  "If non-nil, kill the recovered snapshot buffer when Ediff quits."
  :type 'boolean
  :group 'dgg-recovery)

(defvar dgg-recovery--origin->buffer (make-hash-table :test 'equal)
  "Map origin file (absolute path) -> recovered snapshot buffer.")

(defvar-local dgg-recovery--origin-file nil
  "Absolute path of the original on-disk file for this recovered snapshot buffer.")

(defun dgg-recovery--recover-this-file-no-save ()
  "Recover from auto-save, but never save back to disk; ignore expected out-of-sync user-error."
  (let ((orig-y (symbol-function 'y-or-n-p))
        (orig-yn (symbol-function 'yes-or-no-p)))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (prompt)
                 (cond
                  ((string-match-p "\\`Recover auto save file" prompt) t)
                  ((string-match-p "Save it in file" prompt) nil)
                  (t (funcall orig-y prompt)))))
              ((symbol-function 'yes-or-no-p)
               (lambda (prompt)
                 (cond
                  ((string-match-p "\\`Recover auto save file" prompt) t)
                  ((string-match-p "Save it in file" prompt) nil)
                  (t (funcall orig-yn prompt))))))
      (condition-case e
          (recover-this-file)
        (user-error
         (let ((msg (error-message-string e)))
           ;; Expected after recovery if you declined saving to disk.
           (unless (string-match-p "Buffer is out of sync for file" msg)
             (signal (car e) (cdr e)))))))))

(defun dgg-recovery--snapshot-current-buffer-to-recovered-buffer (origin &optional make-read-only)
  "Copy current buffer contents into a new recovered buffer tracked for ORIGIN."
  (let* ((origin (expand-file-name origin))
         (base   (file-name-nondirectory origin))
         (name   (format "*recovered: %s*" base))
         (src    (current-buffer))
         (buf    (generate-new-buffer name)))
    (with-current-buffer buf
      (setq dgg-recovery--origin-file origin)
      ;; Make it a pure snapshot buffer (not visiting a file).
      (setq-local buffer-file-name nil)
      (setq-local buffer-offer-save nil)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-buffer-substring src)
        (goto-char (point-min))
        (set-buffer-modified-p nil))
      (when make-read-only
        (read-only-mode 1)))
    (puthash origin buf dgg-recovery--origin->buffer)
    buf))

(defun dgg-recovery--ediff-recovered-vs-current (recovered-buf current-buf)
  "Ediff RECOVERED-BUF against CURRENT-BUF.
Optionally kill RECOVERED-BUF when Ediff quits (see `dgg-recovery-kill-recovered-buffer-on-exit`)."
  (let ((cleanup nil))
    (setq cleanup
          (lambda ()
            (remove-hook 'ediff-after-quit-hook-internal cleanup)
            (when (and dgg-recovery-kill-recovered-buffer-on-exit
                       (buffer-live-p recovered-buf))
              (kill-buffer recovered-buf))))
    (add-hook 'ediff-after-quit-hook-internal cleanup)
    (ediff-buffers recovered-buf current-buf)))

;;;###autoload
(defun dgg-recovery-run-all ()
  "Recover from auto-save, snapshot recovered contents, revert file buffer to disk, then ediff.

Flow:
1) Recover current buffer from auto-save (never save to disk).
2) Copy recovered contents to a recovered snapshot buffer.
3) Revert current buffer to match the on-disk file (no prompts).
4) Ediff recovered snapshot buffer vs current (disk) buffer."
  (interactive)
  (let ((origin (buffer-file-name)))
    (unless origin
      (user-error "Current buffer is not visiting a file"))
    (setq origin (expand-file-name origin))

    ;; 1) Recover into current buffer (auto-confirm recover, never save-to-disk).
    (dgg-recovery--recover-this-file-no-save)

    ;; 2) Snapshot recovered contents into recovered buffer.
    (let ((recovered-buf
           (dgg-recovery--snapshot-current-buffer-to-recovered-buffer
            origin dgg-recovery-make-temp-buffer-read-only)))
      ;; 3) Revert current buffer to what's on disk (no prompts).
      (revert-buffer t t) ;; IGNORE-AUTO=t, NOCONFIRM=t

      ;; 4) Ediff recovered vs current (disk).
      (dgg-recovery--ediff-recovered-vs-current recovered-buf (current-buffer))

      (message "Recovery workflow complete for: %s" origin))))

(provide 'dgg-recovery)
;;; dgg-recovery.el ends here

