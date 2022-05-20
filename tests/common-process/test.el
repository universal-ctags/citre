;; Process infrastructure tests

;;; Helper function

(defun poll-process (citre-proc)
  "Poll for citre-process CITRE-PROC."
  (let ((proc (citre-process-proc citre-proc))
        (stderr-buffer (citre-process-stderr-buffer citre-proc))
        stderr-proc)
    (while (accept-process-output proc))
    (when (and (buffer-live-p stderr-buffer)
               (setq stderr-proc (get-buffer-process stderr-buffer)))
      (while (accept-process-output stderr-proc)))))

;;; Async process

(ert-deftest test-async-process-output ()
  "Test output handling of async processes."
  (let* ((success nil)
         (output "")
         (callback (lambda (status msg)
                     (pcase status
                       ('output (setq output (concat output msg)))
                       (0 nil)
                       (s (error (format "Status %s shouldn't occur" s))))))
         (proc-data (citre-make-async-process
                     '("sh" "-c" "sleep 0.1 && echo \"line 1\nline 2\"")
                     callback)))
    (poll-process proc-data)
    (should (equal output "line 1\nline 2\n"))))

(ert-deftest test-async-process-exit-0 ()
  "Test handling of exit status 0 of async processes."
  (let* ((success nil)
         (callback (lambda (status msg)
                     (pcase status
                       ('output nil)
                       (0 (should (equal msg nil)))
                       (s (error (format "Status %s shouldn't occur" s))))))
         (proc-data (citre-make-async-process
                     '("sh" "-c" "sleep 0.1 && echo \"msg\" 1>&2 && exit 0")
                     callback)))
    (poll-process proc-data)))

(ert-deftest test-async-process-exit-1 ()
  "Test handling of exit status 0 of async processes."
  (let* ((success nil)
         (callback (lambda (status msg)
                     (pcase status
                       ('output nil)
                       ;; I think this may fail when there's large chunks of
                       ;; output to stderr.
                       (1 (should (equal msg "msg\n")))
                       (s (error (format "Status %s shouldn't occur" s))))))
         (proc-data (citre-make-async-process
                     '("sh" "-c" "sleep 0.1 & echo \"msg\" 1>&2 && exit 1")
                     callback)))
    (poll-process proc-data)))

(ert-deftest test-async-process-signal ()
  "Test signal handling of async processes."
  (let* ((success nil)
         (callback (lambda (status msg)
                     (should (eq status 'signal))))
         (proc-data (citre-make-async-process '("sh" "-c" "sleep 1")
                                              callback)))
    (sleep-for 0.1)
    (interrupt-process (citre-process-proc proc-data))))

;;; Synchronous process

(ert-deftest test-synchronous-process-output ()
  "Test output handling of synchronous processes."
  (should (equal (citre-get-output-lines
                  '("sh" "-c" "sleep 0.1 && echo \"line 1\nline 2\""))
                 '("line 1" "line 2"))))

(ert-deftest test-synchronous-process-exit-1 ()
  "Test synchronous processes that exit abnormally."
  (should-error (citre-get-output-lines
                 '("sh" "-c" "sleep 0.1 && exit 1"))))

(ert-deftest test-synchronous-process-quit ()
  "Test keyboard quit on synchronous processes."
  (run-at-time 0.1 nil (lambda () (keyboard-quit)))
  (let ((inhibit-quit t)
        (start-time (current-time))
        end-time)
    (with-local-quit
      (citre-get-output-lines
       '("sh" "-c" "sleep 1")))
    (should (< (time-to-seconds (time-since start-time)) 0.5))
    (setq quit-flag nil)))
