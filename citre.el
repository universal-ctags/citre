;;; citre.el --- Superior code reading & auto-completion tool with pluggable backends. -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 05 Feb 2020
;; Keywords: convenience, tools
;; Homepage: https://github.com/universal-ctags/citre
;; Version: 0.4
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

;; Citre is a superior code reading & auto-completion tool with pluggable
;; backends.  Tags file backend, GNU global backend and eglot backend are
;; bundled with Citre.

;; Read README.md to know more about Citre.  It also points you to a detailed
;; user manual.  If you haven't received such a file, please visit
;; https://github.com/universal-ctags/citre.

;;; Code:

;;;; Libraries

(require 'citre-backend-interface)
(require 'citre-ui-jump)
(require 'citre-ui-peek)
(require 'citre-tags)
(require 'citre-global)
(require 'citre-xref-adapter)

;;;; User options

(defcustom citre-enable-xref-integration t
  "Enable xref integration."
  :type 'boolean
  :group 'citre)

;;;###autoload
(put 'citre-enable-xref-integration 'safe-local-variable #'booleanp)
(make-variable-buffer-local 'citre-enable-xref-integration)

(defcustom citre-enable-capf-integration t
  "Enable auto-completion by `completion-at-point'."
  :type 'boolean
  :group 'citre)

;;;###autoload
(put 'citre-enable-capf-integration 'safe-local-variable #'booleanp)
(make-variable-buffer-local 'citre-enable-capf-integration)

(defcustom citre-enable-imenu-integration t
  "Enable imenu integration."
  :type 'boolean
  :group 'citre)

;;;###autoload
(put 'citre-enable-imenu-integration 'safe-local-variable #'booleanp)
(make-variable-buffer-local 'citre-enable-imenu-integration)

(defcustom citre-auto-enable-citre-mode-backends '(tags global)
  "Backends for which `citre-auto-enable-citre-mode' works.
If one of these backends are usable in current file,
`citre-auto-enable-citre-mode' will enable `citre-mode'."
  :type '(repeat symbol)
  :group 'citre)

(defcustom citre-auto-enable-citre-mode-modes 'all
  "The major modes where `citre-auto-enable-citre-mode' works.
If you require `citre-config' in your configuration, then these
are the major modes where `citre-mode' is automatically enabled
if a tags file can be found.

This should be a list of major modes, or `all' for it to work in
all major modes."
  :type '(choice (repeat symbol)
                 (const :tag "All major modes" all))
  :group 'citre)

(defcustom citre-auto-enable-citre-mode-backend-test-timeout 1
  "Time limit in seconds for testing if a backend is usable.
This could prevent a test from running too long when opening a
file.  A nil value disables the time limit."
  :type '(choice (number :tag "Time in seconds")
                 (const :tag "Disable the time limit" nil))
  :group 'citre)

;;;; Helpers

(defun citre--query-symbol (prompt &optional completion)
  "Query the user for a symbol name with PROMPT.
If COMPLETION is non-nil, the identifiers in the project is used
as completion if any backend supports it."
  (let (id-list)
    (if (and completion (setq id-list (citre-get-id-list)))
        (completing-read prompt id-list)
      (read-string prompt))))

;;;; citre-jump

(defun citre--symbol-at-point-prompt (backends)
  "Get symbol at point using BACKENDS and return a string of them.
The returned string looks like:

    \"symbol\" (A backend), no symbol at point (C backend)..."
  (concat (string-join
           (mapcar (lambda (backend)
                     (let ((symbol
                            (if-let ((s (citre-backend-symbol-at-point
                                         backend)))
                                (format "\"%s\"" s)
                              "no symbol at point")))
                       (format "%s (%s backend)"
                               symbol backend)))
                   backends)
           ", ")
          "."))

;;;###autoload
(defun citre-jump (&optional reference)
  "Jump to the definition of the symbol at point.
When there's multiple definitions, it lets you pick one using the
`completing-read' UI, or you could use your own UI by customizing
`citre-select-definition-function'.

If REFERENCE is non-nil, find references instead."
  (interactive)
  (pcase-let ((`(,backend . ,tags)
               (if reference
                   (citre-get-backend-and-references)
                 (citre-get-backend-and-definitions))))
    (when (null tags)
      ;; TODO: Customizable fallback action (e.g. update tags file and try
      ;; again).  I don't know if it's necessary.
      (user-error "Can't find %s: %s"
                  (if reference "reference" "definition")
                  (citre--symbol-at-point-prompt
                   (if reference citre-find-reference-backends
                     citre-find-definition-backends))))
    (citre-jump-show tags)
    (citre-backend-after-jump backend)))

;;;###autoload
(defun citre-jump-to-reference ()
  "Jump to the definition of the symbol at point.
When there's multiple definitions, it lets you pick one using the
`completing-read' UI, or you could use your own UI by customizing
`citre-select-definition-function'."
  (interactive)
  (citre-jump 'reference))

;;;###autoload
(defun citre-query-jump (&optional completion reference)
  "Jump to the definition of user inputted symbol.
If called with a prefix argument, or COMPLETION is non-nil, then
the identifiers in the project is used as completion if backend
supports it.

If REFERENCE is non-nil, find references instead."
  (interactive "P")
  (pcase-let* ((id (citre--query-symbol
                    (format "Find %s of: "
                            (if reference "reference" "definition"))
                    completion))
               (`(,backend . ,tags)
                (if reference
                    (citre-get-backend-and-references-of-id id)
                  (citre-get-backend-and-definitions-of-id id))))
    (when (null tags)
      (user-error "Can't find %s of %s"
                  (if reference "reference" "definition") id))
    (citre-jump-show tags)
    (citre-backend-after-jump backend)))

;;;###autoload
(defun citre-query-jump-to-reference (&optional completion)
  "Jump to the reference of user inputted symbol.
If called with a prefix argument, or COMPLETION is non-nil, then
the identifiers in the project is used as completion if backend
supports it."
  (interactive "P")
  (citre-query-jump completion 'reference))

;;;; citre-peek

;;;;; Internals

(defun citre-peek--get-tags (&optional reference)
  "Return definitions or references of symbol under point.
If REFERENCE is non-nil, references are returned.

When in an xref buffer, return a single-element list of the tag
of the xref item under point, with the `name' field being
`citre-peek-root-symbol-str'.

A `citre-backend' field in tags will be set to the symbol of the
citre backend, unless when in an xref buffer."
  (pcase-let ((`(,backend . ,tags)
               (if (derived-mode-p 'xref--xref-buffer-mode)
                   (let ((tag (citre-make-tag-of-current-xref-item
                               citre-peek-root-symbol-str)))
                     (cons nil (when tag (list tag))))
                 (if reference (citre-get-backend-and-references)
                   (citre-get-backend-and-definitions)))))
    (if (null tags)
        (user-error
         (if reference (concat "Can't find references: "
                               (citre--symbol-at-point-prompt
                                citre-find-reference-backends))
           (concat "Can't find definition: "
                   (citre--symbol-at-point-prompt
                    citre-find-definition-backends))))
      ;; Keep backend info in private field for `citre-peek-through'.
      (dolist (tag tags) (citre-set-tag-field 'citre-backend backend tag))
      tags)))

;;;;; Peek definitions

;;;###autoload
(defun citre-peek (&optional buf point reference)
  "Peek the definition of the symbol in BUF at POINT.
Or, When BUF or POINT is non-nil, peek the symbol at POINT (or
current point) in BUF (or the current buffer).

When REFERENCE is non-nil, peek the references."
  (interactive)
  (let* ((buf (or buf (current-buffer)))
         (point (or point (point)))
         (tags (save-excursion
                 (with-current-buffer buf
                   (goto-char point)
                   (citre-peek--get-tags reference))))
         (marker (when (buffer-file-name) (point-marker))))
    (citre-peek-show tags marker)))

;;;###autoload
(defun citre-query-peek (&optional completion reference)
  "Peek the definition of user inputted symbol.
If called with a prefix argument, or COMPLETION is non-nil, then
the identifiers in the project is used as completion if backend
supports it.

If REFERENCE is non-nil, find references instead."
  (interactive "P")
  (pcase-let* ((id (citre--query-symbol
                    (format "Find %s of: "
                            (if reference "reference" "definition"))
                    completion))
               (`(,backend . ,tags)
                (if reference
                    (citre-get-backend-and-references-of-id id)
                  (citre-get-backend-and-definitions-of-id id)))
               (marker (when (buffer-file-name) (point-marker))))
    (when (null tags)
      (user-error "Can't find %s of %s"
                  (if reference "reference" "definition") id))
    (dolist (tag tags) (citre-set-tag-field 'citre-backend backend tag))
    (citre-peek-show tags marker)))

;;;###autoload
(defun citre-ace-peek (&optional reference)
  "Peek the definition of a symbol on screen using ace jump.
Press a key in `citre-peek-ace-pick-symbol-at-point-keys' to pick
the symbol under point.

This command is useful when you want to see the definition of a
function while filling its arglist.  When REFERENCE is non-nil,
peek the references."
  (interactive)
  (when-let ((pt (citre-ace-pick-point)))
    (when (region-active-p) (deactivate-mark))
    (citre-peek (current-buffer) pt reference)))

(defun citre-peek-through (&optional reference)
  "Peek through a symbol in current peek window.
If REFERENCE is non-nil, peek its references, otherwise peek its
definitions."
  (interactive)
  (citre-peek--error-if-not-peeking)
  (when-let* ((buffer-point (citre-ace-pick-point-in-peek-window)))
    (let* ((peeked-tag (citre-peek-peeked-tag))
           (backend (citre-get-tag-field 'citre-backend peeked-tag))
           (peeked-file (citre-get-tag-field 'ext-abspath peeked-tag))
           tags)
      (citre-with-file-buffer peeked-file 'visit nil
        (citre-backend-after-jump backend)
        (when (region-active-p) (deactivate-mark))
        (goto-char (cdr buffer-point))
        (setq tags (citre-peek--get-tags reference)))
      (citre-peek-make-current-tag-first)
      (citre-peek--make-branch tags))))

;;;;; Peek references

;;;###autoload
(defun citre-peek-reference ()
  "Peek the definitions of the symbol at point."
  (interactive)
  (citre-peek nil nil 'reference))

;;;###autoload
(defun citre-query-peek-reference (&optional completion)
  "Peek the references of user inputted symbol.
If called with a prefix argument, or COMPLETION is non-nil, then
the identifiers in the project is used as completion if backend
supports it."
  (interactive "P")
  (citre-query-peek completion 'reference))

;;;###autoload
(defun citre-ace-peek-reference ()
  "Peek the references of a symbol on screen using ace jump."
  (interactive)
  (citre-ace-peek 'reference))

(defun citre-peek-through-reference ()
  "Peek through a symbol in current peek window for references."
  (interactive)
  (citre-peek-through 'reference))

;;;; Capf

;;;;; Internals

(defun citre-capf--make-candidate (tag)
  "Make auto-completion candidate of TAG."
  (citre-put-property
   (citre-make-tag-str tag nil '(name))
   'annotation
   (citre-make-tag-str tag nil '(annotation :prefix " ("
                                            :suffix ")"))
   'signature
   (or (citre-get-tag-field 'signature tag)
       (when-let ((str (citre-get-tag-field 'extra-matched-str tag)))
         (string-trim str)))
   'kind
   (when-let ((kind (citre-get-tag-field 'ext-kind-full tag)))
     (intern kind))))

(defun citre-capf--make-collection (tags start end)
  "Make completion table from TAGS.
START and END are the boundaries of the region to complete, see
`completion-at-point-functions' for details.  The returned value
is a valid return value of `completion-at-point-functions'."
  (when-let* ((cands (mapcar #'citre-capf--make-candidate tags))
              (collection
               (lambda (str pred action)
                 (if (eq action 'metadata)
                     '(metadata
                       (category . citre-completion)
                       (cycle-sort-function . identity)
                       (display-sort-function . identity))
                   (complete-with-action action cands str pred))))
              (get-annotation
               (lambda (cand) (citre-get-property 'annotation cand)))
              (get-docsig
               (lambda (cand) (citre-get-property 'signature cand)))
              (get-kind
               (lambda (cand) (citre-get-property 'kind cand))))
    (list start end collection
          :annotation-function get-annotation
          :company-docsig get-docsig
          :company-kind get-kind
          ;; This makes our completion function a "non-exclusive" one, which
          ;; means to try the next completion function when current completion
          ;; table fails to match the text at point (see the docstring of
          ;; `completion-at-point-functions').  This is the desired behavior
          ;; but actually it breaks our substring completion.  This is a bug of
          ;; Emacs, see the FIXME comment in the code of
          ;; `completion--capf-wrapper'.  I believe I've fixed it, so let's
          ;; leave this line commented rather than delete it, and see if my
          ;; patch will get itself into Emacs
          ;; (https://debbugs.gnu.org/cgi/bugreport.cgi?bug=39600).

          ;; It actually doesn't cause much inconvenience.  Our completion
          ;; function works well, and the only problem is it won't fallback to
          ;; the next one when no tags are matched, which I believe to also
          ;; happen in other completion functions.

          ;; :exclusive 'no
          )))

;;;;; Entry point

(defun citre-completion-at-point ()
  "Function used for `completion-at-point-functions'."
  (when-let* ((cands (if citre-capf-optimize-for-popup
                         (pcase (while-no-input (citre-get-completions))
                           ('t nil)
                           (val val))
                       (citre-get-completions)))
              (beg (nth 0 cands))
              (end (nth 1 cands))
              (cands (nth 2 cands)))
    (if citre-capf-optimize-for-popup
        (pcase (while-no-input (citre-capf--make-collection cands beg end))
          ('t nil)
          (val val))
      (citre-capf--make-collection cands beg end))))

;;;; Xref

;;;;; Internals

(declare-function xref-make "xref" (summary location))
(declare-function xref-make-file-location "xref" (file line column))

;; NOTE: In the worst situation, this will create and kill a temporary buffer
;; when processing every tag.  If we get bug report on the performance, we
;; could use the temp buffer technique in citre-peek, so we only need to do
;; this once for every file.
(defun citre-xref--get-linum (tag)
  "Get the line number of TAG.
If there's no buffer visiting the file containing the tag, this
openes it temporarily, and clean it up on exit.

When the file pointed to by TAG doesn't exist, this returns the
line number in TAG, or 0 if it doesn't record the line number.
This is because we don't want to fail an xref session only
because one file is lost, and users may manually use the line
number if they know the file is renamed/moved to which file."
  (citre-with-file-buffer (citre-get-tag-field 'ext-abspath tag) nil nil
    (citre-locate-tag tag 'use-linum)))

(defun citre-xref--make-object (tag)
  "Make xref object of TAG."
  (let* ((path (citre-get-tag-field 'ext-abspath tag))
         (file-existance
          (if (citre-non-dir-file-exists-p path) ""
            citre-tag-missing-file-mark))
         (line (citre-xref--get-linum tag)))
    (xref-make
     (citre-make-tag-str tag nil
                         '(annotation :prefix "(" :suffix ")"
                                      ;; In xref buffer, we may want to jump to
                                      ;; the tags with these anonymous names.
                                      :full-anonymous-name t)
                         '(content :ensure t))
     (xref-make-file-location (concat file-existance path) line 0))))

(defun citre-xref--make-collection (tags)
  "Make xref object collection of TAGS.
The returned value is a valid return value for
`xref-backend-definitions' and `xref-backend-references'."
  (mapcar #'citre-xref--make-object tags))

;;;;; Implementation

(defun citre-xref-backend ()
  "The Citre backend for xref."
  'citre)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql 'citre)))
  "Define method for xref to get symbol at point."
  (when-let ((symbol (symbol-at-point)))
    ;; The symbol name doesn't matter for us.  We record the buffer in the text
    ;; property so Citre backends could goto the buffer and find
    ;; definitions/references for symbol at point.
    (citre-put-property (symbol-name symbol)
                        'xref-symbol-buffer (current-buffer))))

(cl-defmethod xref-backend-identifier-completion-table
  ((_backend (eql 'citre)))
  "Return a function for xref to find all completions of a prefix."
  (lambda (str pred action)
    ;; We need this since Xref calls this function in minibuffer.
    (let* ((result (with-selected-window (or (minibuffer-selected-window)
                                             (selected-window))
                     (citre-get-id-list))))
      (complete-with-action action result str pred))))

(cl-defmethod xref-backend-definitions ((_backend (eql 'citre)) symbol)
  "Method for xref to find definitions of SYMBOL."
  (if-let ((buf (citre-get-property 'xref-symbol-buffer symbol)))
      ;; If true, the symbol is grabbed from a buffer, not identifier
      ;; completion table.
      (with-current-buffer buf
        (citre-xref--make-collection (citre-get-definitions)))
    (let ((defs (citre-get-definitions-of-id symbol)))
      (citre-xref--make-collection defs))))

(cl-defmethod xref-backend-references ((_backend (eql 'citre)) symbol)
  "Method for xref to find references of SYMBOL."
  (if-let ((buf (citre-get-property 'xref-symbol-buffer symbol)))
      (with-current-buffer buf
        (citre-xref--make-collection (citre-get-references)))
    (user-error "Finding references of completed symbol is not supported \
by Citre")))

;;;; Imenu

;;;;; Internals

(defvar-local citre-imenu--create-index-function-orig nil
  "Original value of `imenu-create-index-function' in buffer.")

(defun citre-imenu--classify-tags (tags)
  "Classify TAGS based on the `ext-kind-full' field.
This creates an alist, its key is `kind' field value, and value
is a list of tags of that kind."
  (let ((result nil))
    (dolist (tag tags)
      (cl-symbol-macrolet ((place (alist-get class
                                             result nil nil #'equal)))
        (let* ((kind (citre-get-tag-field 'ext-kind-full tag))
               (extras (when-let ((extras (citre-get-tag-field 'extras tag)))
                         (split-string extras ","
                                       t (rx (+ " ")))))
               (classes (or (mapcar
                             (lambda (extra) (concat "<" extra ">"))
                             (cl-remove-if-not
                              (lambda (s)
                                (member s '("reference" "qualified")))
                              extras))
                            (when kind (list kind))
                            (list "<unclassified>"))))
          (dolist (class classes)
            (unless place
              (setf place nil))
            (push tag place)))))
    (dotimes (i (length result))
      (setf (cdr (nth i result))
            (nreverse (cdr (nth i result)))))
    (cl-sort result (lambda (str1 str2)
                      (compare-strings str1 nil nil str2 nil nil))
             :key #'car)))

(defun citre-imenu--make-index-item (tag)
  "Create Imenu index item for TAG.
The returned value is a valid element of the return value of
`imenu-create-index-function'."
  (cons (citre-make-tag-str
         tag nil
         '(name)
         `(annotation :no-kind ,(not (member (citre-get-tag-field 'extras tag)
                                             '("reference" "qualified")))
                      :prefix "(" :suffix ")")
         '(location :no-path t))
        (citre-locate-tag tag)))

(defun citre-imenu--make-index (tags)
  "Create imenu index of TAGS."
  (let ((tags-tree (citre-imenu--classify-tags tags)))
    (dotimes (i (length tags-tree))
      (setf (cdr (nth i tags-tree))
            (mapcar #'citre-imenu--make-index-item (cdr (nth i tags-tree)))))
    tags-tree))

;;;;; Entry point

(defun citre-imenu-create-index-function ()
  "Create imenu index."
  (when-let ((tags (citre-get-tags-in-buffer)))
    (citre-imenu--make-index tags)))

;;;; citre-mode

(defvar citre-mode-map (make-sparse-keymap)
  "Keymap used in `citre-mode'.")

;;;###autoload
(define-minor-mode citre-mode
  "Enable `completion-at-point', xref and imenu integration."
  :lighter " Citre"
  :keymap citre-mode-map
  (cond
   (citre-mode
    ;; Make sure we can find a tags file first.
    (when citre-enable-xref-integration
      (add-hook 'xref-backend-functions #'citre-xref-backend nil t))
    (when citre-enable-capf-integration
      (add-hook 'completion-at-point-functions
                #'citre-completion-at-point nil t))
    (when citre-enable-imenu-integration
      (setq citre-imenu--create-index-function-orig
            imenu-create-index-function)
      (setq imenu-create-index-function #'citre-imenu-create-index-function)))
   (t
    (remove-hook 'xref-backend-functions #'citre-xref-backend t)
    (remove-hook 'completion-at-point-functions
                 #'citre-completion-at-point t)
    (when citre-enable-imenu-integration
      (setq imenu-create-index-function
            citre-imenu--create-index-function-orig)))))

;;;###autoload
(defun citre-auto-enable-citre-mode ()
  "Enable `citre-mode' if appropriate.
This means the current major mode satisfies
`citre-auto-enable-citre-mode-modes', and one of
`citre-auto-enable-citre-mode-backends' is usable.

Put this in `find-file-hook' to automatically enable `citre-mode'
when opening a file."
  (when (or (eq citre-auto-enable-citre-mode-modes 'all)
            (apply #'derived-mode-p citre-auto-enable-citre-mode-modes))
    (let ((timeout citre-auto-enable-citre-mode-backend-test-timeout))
      (cl-dolist (backend citre-auto-enable-citre-mode-backends)
        (when (if timeout
                  (with-timeout (timeout) (citre-backend-usable-p backend))
                (citre-backend-usable-p backend))
          (citre-mode)
          (cl-return))))))

(provide 'citre)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre.el ends here
