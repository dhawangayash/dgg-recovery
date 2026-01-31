;;; dgg-recovery-test.el --- ERT tests for dgg-recovery -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'dgg-recovery)

(ert-deftest dgg-recovery-run-all-errors-when-not-visiting-file ()
  (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
    (should-error
     (with-temp-buffer (dgg-recovery-run-all))
     :type 'error)))

(ert-deftest dgg-recovery-run-all-wires-steps-in-order ()
  (let* ((tmp (make-temp-file "dgg-recovery-" nil ".txt" "on-disk\n"))
         (origin (expand-file-name tmp))
         (calls '())
         (recovered-buf (generate-new-buffer "*dgg-test-recovered*")))
    (unwind-protect
        (cl-letf (((symbol-function 'dgg-recovery--recover-this-file-no-save)
                   (lambda () (push 'recover calls)))

                  ((symbol-function 'dgg-recovery--snapshot-current-buffer-to-recovered-buffer)
                   (lambda (o ro)
                     (push (list 'snapshot (expand-file-name o) ro) calls)
                     recovered-buf))

                  ((symbol-function 'revert-buffer)
                   (lambda (&rest args)
                     (push (cons 'revert args) calls)
                     nil))

                  ((symbol-function 'dgg-recovery--ediff-recovered-vs-current)
                   (lambda (rb cb)
                     (push (list 'ediff rb cb (buffer-file-name cb)) calls)
                     nil))

                  ((symbol-function 'message) (lambda (&rest _) nil)))

          (let ((b (find-file-noselect tmp)))
            (unwind-protect
                (with-current-buffer b
                  (setq dgg-recovery-make-temp-buffer-read-only t)
                  (dgg-recovery-run-all))
              (when (buffer-live-p b) (kill-buffer b))))

          (setq calls (nreverse calls))

          (should (equal (nth 0 calls) 'recover))
          (should (equal (nth 1 calls) (list 'snapshot origin t)))

          (let ((rev (nth 2 calls)))
            (should (eq (car rev) 'revert))
            (should (equal (cdr rev) '(t t))))

          (let ((e (nth 3 calls)))
            (should (equal (car e) 'ediff))
            (should (eq (nth 1 e) recovered-buf))
            (should (bufferp (nth 2 e)))
            (should (equal (expand-file-name (nth 3 e)) origin)))

          (should (= (length calls) 4)))
      (when (buffer-live-p recovered-buf) (kill-buffer recovered-buf))
      (ignore-errors (delete-file tmp)))))

(ert-deftest dgg-recovery--recover-no-save-answers-prompts-and-ignores-out-of-sync ()
  (let (answers)
    (cl-letf (((symbol-function 'recover-this-file)
               (lambda ()
                 (push (list 'recover? (y-or-n-p "Recover auto save file /tmp/#x#? ")) answers)
                 (push (list 'save?    (y-or-n-p "Save it in file /tmp/x? ")) answers)
                 (signal 'user-error (list "Buffer is out of sync for file /tmp/x"))))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (dgg-recovery--recover-this-file-no-save)
      (setq answers (nreverse answers))
      (should (equal answers '((recover? t) (save? nil)))))))

(ert-deftest dgg-recovery--recover-no-save-propagates-other-user-error ()
  (cl-letf (((symbol-function 'recover-this-file)
             (lambda () (signal 'user-error (list "Some other user-error")))))
    (should-error
     (dgg-recovery--recover-this-file-no-save)
     :type 'user-error)))

(ert-deftest dgg-recovery--snapshot-copies-contents-and-sets-buffer-locals ()
  (let* ((tmp (make-temp-file "dgg-recovery-" nil ".txt" "disk\n"))
         (origin (expand-file-name tmp))
         (text "hello\nworld\n")
         (snap nil))
    (unwind-protect
        (with-temp-buffer
          (insert text)
          (setq snap (dgg-recovery--snapshot-current-buffer-to-recovered-buffer origin t))
          (should (buffer-live-p snap))
          (should (eq (gethash origin dgg-recovery--origin->buffer) snap))
          (with-current-buffer snap
            (should (equal dgg-recovery--origin-file origin))
            (should (null buffer-file-name))
            (should (eq buffer-offer-save nil))
            (should (equal (buffer-string) text))
            (should (eq buffer-read-only t))))
      (when (buffer-live-p snap) (kill-buffer snap))
      (ignore-errors (delete-file tmp)))))

(ert-deftest dgg-recovery--ediff-kills-recovered-buffer-on-quit-when-enabled ()
  (let* ((rec (generate-new-buffer "*dgg-rec*"))
         (cur (generate-new-buffer "*dgg-cur*"))
         (dgg-recovery-kill-recovered-buffer-on-exit t)
         (ediff-after-quit-hook-internal nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ediff-buffers)
                   (lambda (_a _b) nil)))
          (dgg-recovery--ediff-recovered-vs-current rec cur)
          (should (consp ediff-after-quit-hook-internal))
          (run-hooks 'ediff-after-quit-hook-internal)
          (should-not (buffer-live-p rec))
          (should (null ediff-after-quit-hook-internal)))
      (when (buffer-live-p rec) (kill-buffer rec))
      (when (buffer-live-p cur) (kill-buffer cur)))))

(provide 'dgg-recovery-test)
;;; dgg-recovery-test.el ends here
