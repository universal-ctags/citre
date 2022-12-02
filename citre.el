;;; citre.el --- Ctags IDE on the True Editor -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 05 Feb 2020
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

;; Citre is an advanced Ctags (or actually, readtags) frontend for Emacs.  It
;; offers:

;; - auto-completion (by `completion-at-point').
;; - xref and imenu integration.
;; - `citre-jump', a `completing-read' UI for jumping to definition.
;; - `citre-peek', a powerful code reading tool that lets you go down the
;;   rabbit hole without leaving current buffer.

;; Read README.md to know more about Citre.  It also points you to a detailed
;; user manual.  If you haven't received such a file, please visit
;; https://github.com/universal-ctags/citre.

;;; Code:

;;;; Libraries

(require 'citre-backend-interface)
(require 'citre-tags)
(require 'citre-global)

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

;;;; citre-jump

(defun citre--symbol-at-point-prompt (backends)
  "Get symbol at point using BACKENDS and return a string of them.
The returned string looks like:

    \"symbol\" (A backend), no symbol at point (C backend)..."
  (concat (string-join
           (mapcar (lambda (backend)
                     (let ((symbol
                            (if-let ((s (citre-get-symbol-at-point-for-backend
                                         backend)))
                                (format "\"%s\"" s)
                              "no symbol at point")))
                       (format "%s (%s backend)"
                               symbol backend)))
                   backends)
           ", ")
          "."))

;;;###autoload
(defun citre-jump ()
  "Jump to the definition of the symbol at point.
When there's multiple definitions, it lets you pick one using the
`completing-read' UI, or you could use your own UI by customizing
`citre-select-definition-function'."
  (interactive)
  (let* ((defs (citre-get-definitions))
         (buf (current-buffer)))
    (if (null defs)
        ;; TODO: Customizable fallback action (e.g. update tags file and try
        ;; again).  I don't know if it's necessary.
        (user-error (concat "Can't find definition: "
                            (citre--symbol-at-point-prompt
                             citre-find-definition-backends))))
    (citre-jump-show defs)
    (citre-after-jump-action buf)))

;;;###autoload
(defun citre-jump-to-reference ()
  "Jump to the definition of the symbol at point.
When there's multiple definitions, it lets you pick one using the
`completing-read' UI, or you could use your own UI by customizing
`citre-select-definition-function'."
  (interactive)
  (let* ((refs (citre-get-references))
         (buf (current-buffer)))
    (if (null refs)
        ;; TODO: Customizable fallback action.
        (user-error (concat "Can't find references: "
                            (citre--symbol-at-point-prompt
                             citre-find-reference-backends))))
    (citre-jump-show refs)
    (citre-after-jump-action buf)))

;;;; citre-peek

;;;;; Internals

(defun citre-peek--get-tags (&optional reference)
  "Return definitions or references of symbol under point.
If REFERENCE is non-nil, references are returned.  This works for
temporary buffer created by `citre-peek'.  The backend and
definitions are returned in a cons pair.

When in an xref buffer, return a single-element list of the tag
of the xref item under point, with the `name' field being
`citre-peek-root-symbol-str'."
  (citre-peek--hack-buffer-file-name
    (let* ((tags (if (derived-mode-p 'xref--xref-buffer-mode)
                     (let ((tag (citre-make-tag-of-current-xref-item
                                 citre-peek-root-symbol-str)))
                       (cons nil (when tag (list tag))))
                   (if reference (citre-get-references)
                     (citre-get-definitions)))))
      (if (null tags)
          (user-error
           (if reference (concat "Can't find references: "
                                 (citre--symbol-at-point-prompt
                                  citre-find-reference-backends))
             (concat "Can't find definition: "
                     (citre--symbol-at-point-prompt
                      citre-find-definition-backends))))
        tags))))

;;;;; Peek definitions

;;;###autoload
(defun citre-peek (&optional buf point reference)
  "Peek the definition of symbol at pointthe symbol in BUF at POINT.
Or, When BUF or POINT is non-nil, peek the symbol at POINT (or
current point) in BUF (or the current buffer).

When REFERENCE is non-nil, peek the references."
  (interactive)
  (let* ((buf (or buf (current-buffer)))
         (point (or point (point)))
         (defs (save-excursion
                 (with-current-buffer buf
                   (goto-char point)
                   (citre-peek--get-tags reference))))
         (marker (when (buffer-file-name) (point-marker))))
    (citre-peek-show defs marker)))

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

;;;###autoload
(defun citre-peek-through (&optional reference)
  "Peek through a symbol in current peek window.
If REFERENCE is non-nil, peek its references, otherwise peek its
definitions."
  (interactive)
  (citre-peek--error-if-not-peeking)
  (let ((prev-buf (current-buffer)))
    (when-let* ((buffer-point (citre-ace-pick-point-in-peek-window))
                (tags (save-excursion
                        (with-current-buffer (car buffer-point)
                          ;; TODO: this is not totally reliable, e.g., when the
                          ;; user restores the peek session in a buffer that's
                          ;; not in the current project.  We should add a
                          ;; command to set tags file for current buffer or
                          ;; current peek window.  AND, we should make
                          ;; `citre--backend-after-jump-functions' to let all
                          ;; backends work (think about peek definition then
                          ;; peek through reference, the backend has changed).
                          (citre-after-jump-action prev-buf)
                          (when (region-active-p) (deactivate-mark))
                          (goto-char (cdr buffer-point))
                          (citre-peek--get-tags reference)))))
      (citre-peek-make-current-tag-first)
      (citre-peek--make-branch tags))))

;;;;; Peek references

;;;###autoload
(defun citre-peek-reference ()
  "Peek the definitions of the symbol at point."
  (interactive)
  (citre-peek nil nil 'reference))

;;;###autoload
(defun citre-ace-peek-reference ()
  "Peek the references of a symbol on screen using ace jump."
  (interactive)
  (citre-peek-through 'reference))

;;;###autoload
(defun citre-peek-through-reference ()
  "Peek through a symbol in current peek window for references."
  (interactive)
  (citre-peek-through 'reference))

;;;;; Jump

(defun citre-peek-jump ()
  "Jump to the definition that is currently peeked."
  (interactive)
  (citre-peek--error-if-not-peeking)
  (let ((prev-buf (current-buffer)))
    (citre-peek-abort)
    (citre-goto-tag
     (citre-peek--tag-node-tag
      (citre-peek--current-tag-node)))
    (citre-after-jump-action prev-buf))
  (when citre-peek-auto-restore-after-jump
    (citre-peek-restore)
    (when citre-peek-backward-in-chain-after-jump
      (citre-peek-chain-backward))))

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
  (let* ((path (citre-get-tag-field 'ext-abspath tag))
         (buf-opened (find-buffer-visiting path))
         buf linum)
    (if (not (citre-non-dir-file-exists-p path))
        (or (citre-get-tag-field 'extra-line tag) 0)
      (if buf-opened
          (setq buf buf-opened)
        (setq buf (generate-new-buffer (format " *citre-xref-%s*" path)))
        (with-current-buffer buf
          (insert-file-contents path)))
      (with-current-buffer buf
        (setq linum (citre-locate-tag tag 'use-linum)))
      (unless buf-opened
        (kill-buffer buf))
      linum)))

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
                     (citre-get-backend-and-id-list))))
      (complete-with-action action (cdr result) str pred))))

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
    (citre-imenu--make-index (cdr tags))))

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
    (cl-dolist (backend citre-auto-enable-citre-mode-backends)
      (when (citre-backend-usable-p backend)
        (citre-mode)
        (cl-return)))))

(provide 'citre)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre.el ends here
