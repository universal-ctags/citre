(defmacro with-clean-global-envs (&rest body)
  "Run BODY with some GNU Global related environment variables bind to nil.
These variables deal with configuration file path, database path,
etc.  See the implementation for details.  Normally you should
wrap `citre-global' tests in this macro."
  (declare (indent 0))
  `(let ((process-environment-orig process-environment))
     (unwind-protect
         (progn
           (setenv "GTAGSCONF" nil)
           (setenv "GTAGSLABEL" nil)
           (setenv "GTAGSOBJDIR" nil)
           (setenv "MAKEOBJDIR" nil)
           (setenv "GTAGSOBJDIRPREFIX" nil)
           (setenv "MAKEOBJDIRPREFIX" nil)
           ,@body)
       (setq process-environment process-environment-orig))))

(ert-deftest test-global-get-references ()
  "Test `citre-global-get-references'."
  (with-clean-global-envs
    (unless (eq (call-process (or citre-gtags-program "gtags")
                              nil "*gtags*" nil
                              "--compact"
                              "--file=src.list")
                0)
      (error "gtags failed to run:\n%s" (buffer-string "*gtags*")))
    (let ((refs (citre-global-get-references "func")))
      (should (equal (map-get-field 'name refs)
                     '("func" "func")))
      (should (equal (map-get-field 'line refs)
                     '(6 10))))
    (dolist (f '("GPATH" "GTAGS" "GRTAGS"))
      (delete-file f))))
