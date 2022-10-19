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

(defun generate-global-database ()
  "Generate global database at current directory.
Signal an error if it fails."
  (unless (eq (call-process (or citre-gtags-program "gtags")
                            nil "*gtags*" nil
                            "--compact"
                            "--file=src.list")
              0)
    (error "gtags failed to run:\n%s" (buffer-string "*gtags*"))))

(defun delete-global-database ()
  "Delete global database in current directory."
  (dolist (f '("GPATH" "GTAGS" "GRTAGS"))
    (delete-file f)))

(ert-deftest test-global-get-completions ()
  "Test getting completions using `citre-global-get-tags'."
  (with-clean-global-envs
    (generate-global-database)
    (let ((completions (citre-global-get-tags "ma" 'completion)))
      (should (equal (map-get-field 'name completions)
                     '("main"))))
    (delete-global-database)))

(ert-deftest test-global-get-definitions ()
  "Test getting definitions using `citre-global-get-tags'."
  (with-clean-global-envs
    (generate-global-database)
    (let ((defs (citre-global-get-tags "func" 'definition)))
      (should (equal (map-get-field 'name defs)
                     '("func")))
      (should (equal (map-get-field 'line defs)
                     '(1))))
    (delete-global-database)))

(ert-deftest test-global-get-references ()
  "Test getting references using `citre-global-get-tags'."
  (with-clean-global-envs
    (generate-global-database)
    (let ((refs (citre-global-get-tags "func" 'reference)))
      (should (equal (map-get-field 'name refs)
                     '("func" "func")))
      (should (equal (map-get-field 'line refs)
                     '(6 10))))
    (delete-global-database)))
