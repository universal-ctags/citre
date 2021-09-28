;;; citre-common.el --- Common functions and utilities -*- lexical-binding: t -*-

;; Copyright (C) 2021 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 22 Aug 2021
;; Keywords: convenience, tools
;; Homepage: https://github.com/universal-ctags/citre
;; Version: 0.2
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

(defcustom citre-after-jump-hook '(citre-recenter-and-blink)
  "Hook to run after jumping to a location."
  :type 'hook
  :group 'citre)

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

(defun citre--project-root ()
  "Full path of project root of current buffer.
This uses `project-current' internally."
  (when-let ((project (project-current nil)))
    (expand-file-name (cdr project))))

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

;;;; Visual

(defun citre-recenter-and-blink ()
  "Recenter point and blink after point.
This is suitable to run after jumping to a location."
  (recenter)
  (pulse-momentary-highlight-one-line (point)))

;;;; Process

(defvar citre-stop-process-on-input nil
  "Non-nil allows user input to stop the process in `citre-get-output-lines'.
Let-bind this to non-nil for situations like popup completions to
make a responsive UI.")

(defun citre-get-output-lines (cmd buffer &optional get-lines)
  "Run CMD and write its output to BUFFER.
CMD is a list of the program name and its arguments.

This will return when the process is finished, and local quit is
allowed to terminate the process.

BUFFER is cleaned first.

When GET-LINES is non-nil, return the output lines in a list.

When the process exits abnormally or run into abnormal status,
this signals an error."
  (let* ((name (car cmd))
         inhibit-message proc exit-msg)
    (with-current-buffer buffer
      (erase-buffer))
    ;; Suppress TRAMP messages
    (when (file-remote-p default-directory)
      (setq inhibit-message t))
    ;; We allow keyboard quit when waiting for the process to finish, by
    ;; `with-local-quit'.  This line is to make sure the following cleanup
    ;; can't be breaked by user input, especially quickly swapping the sentinel
    ;; function, see comments later.
    (let ((inhibit-quit t))
      ;; Credit: This technique is developed from
      ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32986.
      (pcase (with-local-quit
               (catch 'citre-done
                 (setq proc
                       (make-process
                        :name name
                        :buffer buffer
                        :command cmd
                        :connection-type 'pipe
                        ;; NOTE: Using a buffer or pipe for :stderr has caused
                        ;; a lot of troubles on Windows.
                        :stderr nil
                        :sentinel (lambda (_proc _msg)
                                    ;; While we use `sleep-for' for pending,
                                    ;; throw/catch can stop the pending.
                                    (throw 'citre-done t))
                        :file-handler t))
                 ;; Poll for the process to finish.  Once it's finished,
                 ;; the sentinel function throws a tag which breaks the
                 ;; sleeping.  `sleep-for' can be waken up by `C-g'.
                 (if citre-stop-process-on-input
                     ;; `sit-for' wakes up when input is avaliable.
                     (while (sit-for 30))
                   ;; `sleep-for' doesn't bother with input (except `C-g').
                   (while (sleep-for 30)))))
        ;; Since we throw t, this can only receive nil when `C-g' arrives.
        ;; When this happens, we immediately replace the sentinel function, or
        ;; it will throw to the wild and cause a "No catch for tag: citre-done"
        ;; error.
        ('nil (set-process-sentinel proc #'ignore)
              (if (eq system-type 'windows-nt)
                  ;; Based on my experiment on a large tags file,
                  ;; `interrupt-process' doesn't work reliably on Windows,
                  ;; while sighup seems does.  When using a remote Unix machine
                  ;; on Windows, this may send a SIGHUP to the remote process,
                  ;; but shouldn't be a problem since SIGHUP is not a harsh
                  ;; signal.
                  (signal-process proc 'sighup)
                (interrupt-process proc))
              nil)
        (_ (pcase (process-status proc)
             ('exit
              (pcase (process-exit-status proc)
                (0 nil)
                (s (setq exit-msg (format "%s exits %s\n" name s)))))
             (s (setq exit-msg
                      (format "abnormal status of %s: %s\n" name s))))
           (cl-symbol-macrolet
               ((output (with-current-buffer buffer (buffer-string)))
                (output-list (split-string output "\n" t))
                (output-list-while-no-input
                 (pcase (while-no-input output-list) ('t nil) (val val))))
             (if exit-msg
                 (error (concat exit-msg output))
               (when get-lines
                 ;; Allow user input to stop the post-processing part as it can
                 ;; also take some time.
                 (if citre-stop-process-on-input
                     output-list-while-no-input
                   output-list)))))))))

(provide 'citre-common)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-common.el ends here
