;;; recovered-file-tools.el --- helpers for Emacs auto-save recovery -*- lexical-binding: t; -*-

;; Usage:
;;  1) M-x recover-this-file
;;  2) M-x save-recovered-file-to-temp-buffer
;;  3) M-x diff-recovered-file-with-original

(defvar recovered-file-tools--origin->buffer (make-hash-table :test 'equal)
  "Map origin file (absolute path) -> temp recovered buffer.")

(defvar-local recovered-file-tools--origin-file nil
  "Absolute path of the original on-disk file for this recovered temp buffer.")

(defun save-recovered-file-to-temp-buffer (&optional make-read-only)
  "Copy current buffer contents into a temp \"recovered\" buffer.

Intended flow:
  1) M-x recover-this-file
  2) M-x save-recovered-file-to-temp-buffer
  3) M-x diff-recovered-file-with-original

With prefix arg MAKE-READ-ONLY, mark the recovered buffer read-only."
  (interactive "P")
  (let* ((origin (buffer-file-name))
         (src    (current-buffer)))
    (unless origin
      (user-error "Current buffer is not visiting a file"))
    (setq origin (expand-file-name origin))
    (let* ((name (format "*recovered: %s*" (file-name-nondirectory origin)))
           (buf  (generate-new-buffer name)))
      (with-current-buffer buf
        (setq-local recovered-file-tools--origin-file origin)
        (insert-buffer-substring src)
        (goto-char (point-min))
        (setq-local buffer-offer-save nil)
        (when make-read-only (read-only-mode 1)))
      (puthash origin buf recovered-file-tools--origin->buffer)
      (pop-to-buffer buf)
      (message "Recovered contents saved to %s" (buffer-name buf)))))


(defun recovered-file-tools--ensure-recovered-buffer ()
  "Return a recovered temp buffer, or signal a helpful error."
  (cond
   (recovered-file-tools--origin-file
    (current-buffer))
   ((and (buffer-file-name)
         (gethash (expand-file-name (buffer-file-name)) recovered-file-tools--origin->buffer))
    (gethash (expand-file-name (buffer-file-name)) recovered-file-tools--origin->buffer))
   (t
    (let* ((candidates
            (let (xs)
              (maphash (lambda (_k v) (push v xs)) recovered-file-tools--origin->buffer)
              (nreverse xs))))
      (unless candidates
        (user-error "No recovered temp buffers tracked yet. Run M-x save-recovered-file-to-temp-buffer first"))
      (or (get-buffer (completing-read
                       "Recovered buffer: "
                       (mapcar #'buffer-name candidates)
                       nil t))
          (user-error "No buffer selected"))))))

(defun diff-recovered-file-with-original (&optional use-ediff)
  "Diff recovered temp buffer against the original file on disk.

By default, opens a unified diff buffer.
With prefix arg USE-EDIFF, use `ediff-files` instead."
  (interactive "P")
  (let* ((rec-buf (recovered-file-tools--ensure-recovered-buffer))
         (origin  (with-current-buffer rec-buf recovered-file-tools--origin-file)))
    (unless (and origin (file-exists-p origin))
      (user-error "Original file does not exist on disk: %s" (or origin "<unknown>")))

    (if use-ediff
        ;; Write recovered buffer to a temp file and ediff it with origin.
        (let ((tmp (make-temp-file "emacs-recovered-" nil ".tmp")))
          (with-current-buffer rec-buf
            (write-region (point-min) (point-max) tmp nil 'silent))
          (ediff-files tmp origin)
          (message "Ediff: recovered temp file %s vs %s (delete temp manually if desired)" tmp origin))
      ;; Diff (unified) using `diff-no-select`.
      (let* ((tmp (make-temp-file "emacs-recovered-" nil ".tmp"))
             (diffbuf nil))
        (with-current-buffer rec-buf
          (write-region (point-min) (point-max) tmp nil 'silent))
        (setq diffbuf (diff-no-select tmp origin "-u" t))
        ;; Cleanup tmp when diff buffer is killed.
        (with-current-buffer diffbuf
          (setq-local recovered-file-tools--diff-temp-file tmp)
          (add-hook
           'kill-buffer-hook
           (lambda ()
             (when (and (boundp 'recovered-file-tools--diff-temp-file)
                        recovered-file-tools--diff-temp-file
                        (file-exists-p recovered-file-tools--diff-temp-file))
               (delete-file recovered-file-tools--diff-temp-file)))
           nil t))
        (pop-to-buffer diffbuf)))))

(defun recovered-file-tools-help ()
  "Show short instructions for the recovered-file workflow."
  (interactive)
  (message
   "1) M-x recover-this-file  2) M-x save-recovered-file-to-temp-buffer  3) M-x diff-recovered-file-with-original"))

(provide 'recovered-file-tools)
;;; recovered-file-tools.el ends here
