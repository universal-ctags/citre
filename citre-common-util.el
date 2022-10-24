;;; citre-common-util.el --- Common functions and utilities -*- lexical-binding: t -*-

;; Copyright (C) 2021 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 22 Aug 2021
;; Keywords: convenience, tools
;; Homepage: https://github.com/universal-ctags/citre
;; Version: 0.3.1
;; Package-Requires: ((emacs "26.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Common functions and utilities for Citre.

;;; Code:

;;;; Libraries

(require 'project)
(require 'subr-x)

;;;; User Options

(defcustom citre-project-root-function #'citre--project-root
  "A function that returns project root in current buffer.
It takes no arguments.  It's used for:

- Displaying the path of a tag relatively.
- Expanding relative paths in `citre-tags-file-alist' and
  `citre-tags-file-cache-dirs'.

The function can return a string or nil."
  :type 'function
  :group 'citre)

;;;; Backports

;; Backported from Emacs 28.2, as a dependency of `citre--file-name-quote'.
(defsubst citre--file-name-quoted-p (name &optional top)
  "Whether NAME is quoted with prefix \"/:\".
If NAME is a remote file name and TOP is nil, check the local part of NAME."
  (let ((file-name-handler-alist (unless top file-name-handler-alist)))
    (string-prefix-p "/:" (file-local-name name))))

;; Backported from Emacs 28.2, as a dependency of `citre-executable-find'.
(defsubst citre--file-name-quote (name &optional top)
  "Add the quotation prefix \"/:\" to file NAME.
If NAME is a remote file name and TOP is nil, the local part of
NAME is quoted.  If NAME is already a quoted file name, NAME is
returned unchanged."
  (let ((file-name-handler-alist (unless top file-name-handler-alist)))
    (if (citre--file-name-quoted-p name top)
        name
      (concat (file-remote-p name) "/:" (file-local-name name)))))

;; Backported from Emacs 28.2.  Versions below 27 doesn't have the REMOTE
;; argument.
(defun citre-executable-find (command &optional remote)
  "Search COMMAND in variable `exec-path' and return the absolute file name.
Return nil if COMMAND is not found anywhere in variable
`exec-path'.  If REMOTE is non-nil, search on the remote host
indicated by `default-directory' instead."
  (if (and remote (file-remote-p default-directory))
      (let* ((handler (find-file-name-handler default-directory 'exec-path))
             (exec-path (if handler (funcall handler 'exec-path) exec-path))
             (res (locate-file
                   command
                   (mapcar
                    (lambda (x) (concat (file-remote-p default-directory) x))
                    exec-path)
                   exec-suffixes 'file-executable-p)))
        (when (stringp res) (file-local-name res)))
    (let ((default-directory (citre--file-name-quote default-directory 'top)))
      (locate-file command exec-path exec-suffixes 1))))

;;;; String

(defun citre-string-after-1st-colon (string)
  "Return the part in STRING after the first colon in it.
If STRING doesn't contain a colon, return it directly."
  (if-let ((sep (string-match ":" string)))
      (substring string (1+ sep))
    string))

(defun citre-string-match-all-escaping-backslash
    (string &optional start)
  "Find all occurence of escaping backslashes in STRING.
If START is non-nil, start search at that index in STRING.
Return a list of indexes of them.

This assumes the only escape sequence containing a second
backslash is \"\\\\\", and a single \"\\\" never appeas at the
end of STRING."
  (let ((result nil)
        (start (or start 0))
        (idx nil))
    (while (setq idx (string-match "\\\\" string start))
      (push idx result)
      ;; Jump over the char after the backslash to search for next escaping
      ;; sequence.  NOTE: This may cause an "args out of range" error, but only
      ;; on string containing invalid trailing backslashes.  We don't check it
      ;; for performance.
      (setq start (+ idx 2)))
    (nreverse result)))

(defun citre-upcase-first-letter (str)
  "Return STR with the first letter upcased."
  (if (zerop (length str))
      str
    (concat (upcase (substring str 0 1))
            (substring str 1))))

;;;; Regexp / String match

(defun citre-find-nearest-regexp
    (regexp &optional limit case-fold)
  "Find the nearest occurence of REGEXP from current position.
By \"nearar\" we mean there are fewer lines between current
position and the occurence.

This goes to the beginning of line position of the occurence, and
returns the position there.  If it's not found, return nil and
don't go anywhere.

When LIMIT is non-nil, it's the limit of chars that the search
goes.  CASE-FOLD decides case-sensitivity."
  (let ((start (line-beginning-position))
        (case-fold-search case-fold)
        after after-lines
        before before-lines)
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward
             regexp (when limit (+ start limit)) t)
        (beginning-of-line)
        (setq after (point))
        (setq after-lines (count-lines start after))))
    (unless (and after (<= after-lines 1))
      (save-excursion
        (beginning-of-line)
        (when (re-search-backward
               regexp (when limit (- start limit)) t)
          (beginning-of-line)
          (setq before (point))
          (setq before-lines (count-lines before start)))))
    (cond
     ((and after before)
      (goto-char (if (< before-lines after-lines) before after)))
     ((or after before)
      (goto-char (or after before))))))

(defun citre-csv-contain (part string)
  "Check if PART is an item in STRING, a comma-separated list."
  (string-match (concat "\\(^\\|,\\)[[:space:]]*"
                        (regexp-quote part)
                        "\\(,\\|$\\)")
                string))

;;;; Text property

(defun citre-get-property (field str)
  "Get the text property corresponding to FIELD in STR.
STR should be propertized by `citre-put-property'.

What it actually does is prefix FIELD by `citre-', and get that
text property."
  (get-text-property 0 (intern (concat "citre-" (symbol-name field))) str))

(defun citre-put-property (str &rest properties)
  "Set the text property of STR.
STR is the string to be modified.  PROPERTIES form a sequence of
PROPERTY VALUE pairs for test properties to add.  Each PROPERTY
is prefixed by \"citre-\".  Propertized STR is returned."
  (let ((i 0)
        (len (length properties)))
    (while (< (1+ (* 2 i)) len)
      (let ((prop (nth (* 2 i) properties))
            (val (nth (1+ (* 2 i)) properties)))
        (put-text-property 0 (length str)
                           (intern (concat "citre-" (symbol-name prop)))
                           val str))
      (cl-incf i)))
  str)

;;;; Project

;; Suppress the compilation warning that `project-root' is not defined.
(declare-function project-root "project")

(defun citre--project-root ()
  "Full path of project root of current buffer.
This uses `project-current' internally."
  (when-let ((project (project-current nil)))
    (if (fboundp #'project-root)
        (project-root project)
      ;; Suppress the warning in Emacs master that `project-roots' is
      ;; deprecated.
      (car (with-no-warnings (project-roots project))))))

(defun citre-project-root ()
  "Return the project root of current buffer.
Customizable by `citre-project-root-function'.

This may return nil when it can't decide the project root."
  (funcall citre-project-root-function))

;;;; File

;; NOTE: On Windows, ctags uses slash as the default directory separator, and
;; it can be handled by Emacs, so for now we don't care about backslash.
(defun citre-file-name-extension (file)
  "Return the extension of FILE.
If it doesn't have an extension, return the file name without
directory.

This is faster than `file-name-extension'."
  (or (string-match "\\.\\([^./]+\\)$" file)
      (string-match "/\\([^/]+\\)$" file))
  (match-string 1 file))

(defun citre-non-dir-file-exists-p (file)
  "Return t if FILE exists and is not a directory."
  (and (file-exists-p file)
       (not (file-directory-p file))))

(defun citre-dir-exists-p (dir)
  "Return t if DIR exists and is a directory."
  (and (file-exists-p dir)
       (file-directory-p dir)))

(defun citre-current-dir ()
  "Full current directory of the buffer.
This means the directory of the buffer file, or expanded
`default-directory' if it's not a file buffer."
  (expand-file-name
   (if-let (file (buffer-file-name))
       (file-name-directory file)
     default-directory)))

(defun citre-relative-path (path &optional root)
  "Return PATH but relative to ROOT.
If PATH is not under ROOT, it's directly returned.  If ROOT is
nil, use project in current buffer instead."
  (let ((root (or root (citre-project-root))))
    (if (and root (file-in-directory-p path root))
        (file-relative-name path root)
      path)))

(defmacro citre-with-file-buffer (file &rest body)
  "Run BODY in the buffer of FILE.
When FILE is already opened, use that buffer, otherwise create a
temporary buffer.  If FILE doesn't exist, do nothing and return
nil.

BODY is wrapped in `save-excursion' and `save-restriction', with
the buffer being widened."
  (declare (indent 1))
  `(let* ((buf-opened (find-buffer-visiting ,file)) buf)
     (when (citre-non-dir-file-exists-p path)
       (if buf-opened
           (setq buf buf-opened)
         (setq buf (generate-new-buffer (format " *citre-%s*" ,file)))
         (with-current-buffer buf
           (insert-file-contents ,file)))
       (unwind-protect
           (with-current-buffer buf
             (save-excursion
               (save-restriction
                 (widen)
                 ,@body)))
         (unless buf-opened
           (kill-buffer buf))))))

(defun citre-directory-of (file)
  "Return the directory that contains FILE.
FILE can be a file or directory.

If FILE is already the root directory, return nil."
  (let* ((dirname (directory-file-name file))
         (dir (file-name-directory dirname)))
    (unless (equal dir file)
      dir)))

;;;; Visual

(defun citre-recenter-and-blink ()
  "Recenter point and blink after point.
This is suitable to run after jumping to a location."
  (recenter)
  (pulse-momentary-highlight-one-line (point)))

;;;; Process

(cl-defstruct (citre-process
               (:constructor nil)
               (:constructor
                citre-process-create)
               (:copier nil))
  "Helper data structure for async processes.
Use `citre-make-async-process' to create an instance.  This does
not fully cover Emacs async process functionalities and is for
Citre's internal use only."
  (proc
   nil
   :documentation
   "The process object."
   :type "process")
  (callback
   nil
   :documentation
   "The callback function.
See `citre-make-async-process' for details."
   :type "function")
  (stderr-buffer
   nil
   :documentation
   "Stderr buffer."
   :type "buffer")
  (remote-p
   nil
   :documentation
   "Whether the process is a remote one."
   :type "boolean")
  (-stdout-str
   ""
   :documentation
   "Internal variable for storing a part of stdout output."
   :type "string"))

;; NOTE: In Emacs 28 we could use INHIBIT-BUFFER-HOOKS argument in
;; `get-buffer-create' so we don't need this.
(defun citre-kill-process-buffer (buffer)
  "Delete the process in BUFFER and kill BUFFER.
This doesn't run `kill-buffer-hook' and
`kill-buffer-query-functions' so it should be faster."
  (let ((kill-buffer-hook nil)
        (kill-buffer-query-functions nil))
    (when-let ((proc (get-buffer-process buffer)))
      (delete-process proc))
    (kill-buffer buffer)))

(defun citre-destruct-process (citre-proc)
  "Destruct citre-process CITRE-PROC.
This means terminating the process if it's running, and cleaning
temporary buffers and maybe other resources."
  (let ((proc (citre-process-proc citre-proc)))
    ;; Based on my experiment reading a large tags file using readtags,
    ;; `interrupt-process' doesn't work reliably on Windows, while sighup seems
    ;; does.
    (when (process-live-p proc)
      (if (and (eq system-type 'windows-nt)
               (not (citre-process-remote-p citre-proc)))
          (signal-process proc 'sighup)
        (interrupt-process proc))))
  (let ((stderr-buffer (citre-process-stderr-buffer citre-proc)))
    (when (buffer-live-p stderr-buffer)
      (citre-kill-process-buffer stderr-buffer))))

(defun citre-make-async-process (cmd callback &optional name)
  "Run CMD in an async process.
A `citre-process' instance is returned.

The process is killed when its status changes, so this function
is not suited for process that needs to be
stopped/continued/connected, etc.  The process is assumed to just
run, and then exit or be terminated by a signal.

NAME is the name of the process.  When it's nil, the first
element in CMD is used as the name.  The name may be uniquified.

CALLBACK is called when the output of the process is received, or
when the status of it changed.  It receives 2 arguments: STATUS
and MSG.  STATUS can be:

- output: We've received a chunk from stdout of the process.  MSG
  is this chunk, and is guaranteed to end in a newline char.
- an integer: The process exited with STATUS.  If it's 0, MSG is
  nil; otherwise MSG is the stderr output.
- signal: The process is terminated by a signal.  MSG is nil.
- other status: See `process-status' for details.  This is the
  abnormal case as we assume the process is either running,
  exited or terminated by a signal.

There's no guarantee that `output' status doesn't occur after the
process exits or be terminated as the output is buffered.  Refer
to `citre-get-output-lines' for how to deal with it if it matters
for your callback function."
  (let* ((name (or name (car cmd)))
         (stderr-buffer (generate-new-buffer
                         (concat " *" name "-stderr*")))
         (remote-p (when (file-remote-p default-directory) t))
         (proc-data (citre-process-create
                     :callback callback
                     :stderr-buffer stderr-buffer
                     :remote-p remote-p))
         (inhibit-message remote-p)
         (proc
          (make-process
           :name name
           :buffer nil
           :command cmd
           :connection-type 'pipe
           :stderr stderr-buffer
           :file-handler t
           :filter
           (lambda (_proc str)
             (let* ((chunk-end
                     ;; Find last newline char.
                     (pcase (string-match (rx "\n" (* (not (any "\n")))
                                              string-end)
                                          str)
                       ((and i (guard i)) (1+ i))
                       ('nil nil))))
               (cl-symbol-macrolet ((stdout-cache
                                     (citre-process--stdout-str proc-data)))
                 (if chunk-end
                     (progn
                       (funcall (citre-process-callback proc-data)
                                'output (concat stdout-cache
                                                (substring str 0 chunk-end)))
                       (setf stdout-cache (substring str chunk-end)))
                   (setf stdout-cache (concat stdout-cache str))))))
           :sentinel
           (lambda (proc _msg)
             (let ((stderr-buffer (citre-process-stderr-buffer proc-data))
                   (callback (citre-process-callback proc-data)))
               (unwind-protect
                   (pcase (process-status proc)
                     ('exit
                      (pcase (process-exit-status proc)
                        (0 (funcall callback 0 nil))
                        (s (if (buffer-live-p stderr-buffer)
                               (funcall callback s
                                        (with-current-buffer stderr-buffer
                                          (buffer-string)))
                             ""))))
                     (s (funcall callback s nil)))
                 (when (buffer-live-p stderr-buffer)
                   (citre-kill-process-buffer stderr-buffer))))))))
    (setf (citre-process-proc proc-data) proc)
    proc-data))

;; This is a synchronous function, but we use async process in it internally,
;; rather than using the synchronous `call-process', is quitting (pressing
;; `C-g') during `call-process' tries to terminate the process using SIGINT,
;; and only when the process doesn't end immediately, and quitting happens
;; again, it uses SIGKILL.  This may cause lagging for popup completion.
;;
;; The detailed explanation is: The completion UI may wrap this function in
;; `while-no-input' for not blocking the UI, which sends a quit signal when
;; user input arrives, which triggers SIGINT or SIGKILL.  But SIGINT may not
;; kill the process immediately, which freezes the UI.  This is the case when
;; reading a large tags file using readtags in Windows.
;;
;; Citre before (and including) commit "093722a: ctags, fix: wrong usage of
;; read-file-name" uses a different trick which works well for me,
;; unfortunately Windows users often report bugs related to processes.
(defun citre-get-output-lines (cmd)
  "Run CMD and return its output in a list of lines.
Keyboard quit is allowed to terminate the process.  When the
process exits abnormally or run into abnormal status, an error is
signaled."
  (let* ((result nil)
         (err-msg nil)
         (finished nil)
         (success nil)
         (callback
          (lambda (status msg)
            (pcase status
              ('output (setq result
                             (nconc result (split-string msg "\n" t))))
              (0 (setq success t))
              ((and s (pred integerp))
               (setq err-msg (format "Process %s exits %s:\n%s"
                                     (car cmd) s msg)))
              ('signal nil)
              (s (setq err-msg (format "Abnormal status of process %s:\n%s"
                                       (car cmd) s))))
            (unless (eq status 'output)
              (setq finished t))))
         (proc-data (citre-make-async-process cmd callback))
         (proc (citre-process-proc proc-data)))
    (unwind-protect
        ;; We need to poll the process in a non-blocking way (i.e., allow
        ;; quitting).  In order to understand this, we need to keep in mind 2
        ;; facts about `accept-process-output':
        ;;
        ;; 1. user input could not be processed during `accept-process-output'
        ;;    (so it blocks, see
        ;;    https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32986).
        ;;
        ;; 2. When it accepts output from a certain process, it waits till the
        ;;    process outputs something or finishes.  Try:
        ;;
        ;;    ;; This blocks
        ;;    (let ((proc (make-process :name "test" :command '("sleep" "1"))))
        ;;      (accept-process-output proc))
        ;;
        ;;    ;; This doesn't block as `accept-process-output' returns quickly
        ;;    ;; and keyboard input is handled in between calls to
        ;;    ;; `accept-process-output'.
        ;;    (let ((proc (make-process :name "test" :command '("sleep" "1"))))
        ;;      (accept-process-output))
        (progn
          ;; Wait for the process to finish.  This trick is borrowed from
          ;; emacs-aio (https://github.com/skeeto/emacs-aio).  This doesn't
          ;; block.
          (while (not finished) (accept-process-output))
          ;; The process is finished, but there may still be buffered output
          ;; that's pending, so we `accept-process-output' from the process,
          ;; and the related stderr pipe process.  This blocks, but doesn't
          ;; cause a problem, as the process is finished, and the remaining
          ;; data should be consumed rather quickly.  No need to wait for the
          ;; stderr pipe process as the error message is already set when the
          ;; process exits, and in practice this lags popup completion.
          (when success
            (while (accept-process-output proc)))
          (cond
           (success result)
           (err-msg (error err-msg))
           (t nil)))
      (citre-destruct-process proc-data))))

(provide 'citre-common-util)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-common-util.el ends here
