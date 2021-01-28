;;; citre-basic-tools.el --- Integration of Citre with Emacs built-in tools -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 23 Nov 2020
;; Keywords: convenience, tools
;; Homepage: https://github.com/AmaiKinono/citre
;; Version: 0

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

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;;; Libraries

(require 'citre-util)
(require 'ring)
(require 'subr-x)

;;;; User options

;;;;; Options: Enabled tools

(defcustom citre-enable-xref-integration t
  "Enable xref integration."
  :type 'boolean
  :group 'citre)

(make-variable-buffer-local 'citre-enable-xref-integration)

(defcustom citre-enable-capf-integration t
  "Enable auto-completion by `completion-at-point'."
  :type 'boolean
  :group 'citre)

(make-variable-buffer-local 'citre-enable-capf-integration)

(defcustom citre-enable-imenu-integration t
  "Enable imenu integration."
  :type 'boolean
  :group 'citre)

(make-variable-buffer-local 'citre-enable-imenu-integration)

;;;;; Options: `citre-jump' related

(defcustom citre-jump-select-definition-function
  #'citre-jump-completing-read
  "The function for the user to select a definition from a list.
It receives 2 arguments:

- A list of one or more strings to show the definitions.  The
  function should let the user choose one in it.  The list is
  guaranteed to have one or more elements.  When there are only
  one element, the function can decide to let the user confirm,
  or return it directly.
- A string of the symbol name that's interested in.  The function
  can show it to the user.

See `citre-jump-completing-read' for an example of
implementation."
  :type 'function
  :group 'citre)

;;;;; Options: capf related

(defcustom citre-capf-substr-completion nil
  "Whether do substring completion.
Non-nil means to match tags *containing* the symbol to be
completed, Otherwise match tags *start with* the symbol to be
completed.

Notice that when listing the candidates, Emacs itself will
further filter the completions we supply, and its behavior is
controlled by `completion-styles'.  If you want substring
completion, you need to set `citre-capf-substr-completion' to
non-nil, *and* add `substring' to `completion-styles' (for Emacs
27, there is also a `flex' style that will work)."
  :type 'boolean
  :group 'citre)

(defcustom citre-capf-optimize-for-popup t
  "Non-nil means optimize for popup completion.
This caches latest completion result, and allows typing while
calculating completions, making it slicker to use.

`company' and `auto-complete' users should leave this as t.  For
other users, set this to nil may be slightly better, since a
completion session can be interrupted when you call
`completion-at-point', and while it's calculating, you press some
key by mistake, but that doesn't happen much."
  :type 'boolean
  :group 'citre)

;;;; Tool: Xref integration

(declare-function xref-make "xref" (summary location))
(declare-function xref-make-file-location "xref" (file line column))

(defvar citre-xref--filter
  `(not (or ,(citre-core-filter 'extras '("anonymous inputFile") 'csv-contain)
            ,(citre-core-filter-kind "file")))
  "Filter for finding definitions when the symbol is inputted by user.")

(defvar citre-xref--completion-table-cache
  '(:tage-file nil :time nil :collection nil)
  "Plist for caching identifier completions.
Its props and vals are:

- `:tags-file': Canonical path of tags file.
- `:time': Last modified time of tags file.
- `:collection': The completions.")

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
  (let* ((path (citre-core-get-field 'ext-abspath tag))
         (buf-opened (find-buffer-visiting path))
         buf linum)
    (if (not (file-exists-p path))
        (or (citre-core-get-field 'extra-line tag) 0)
      (if buf-opened
          (setq buf buf-opened)
        (setq buf (generate-new-buffer (format " *citre-xref-%s*" path)))
        (with-current-buffer buf
          (insert-file-contents path)))
      (with-current-buffer buf
        (setq linum (citre-core-locate-tag tag 'use-linum)))
      (unless buf-opened
        (kill-buffer buf))
      linum)))

(defun citre-xref--make-object (tag)
  "Make xref object of TAG."
  (let* ((path (citre-core-get-field 'ext-abspath tag))
         (file-existance
          (if (file-exists-p path) "" citre-definition-missing-file-mark))
         (line (citre-xref--get-linum tag)))
    (xref-make
     (citre-make-tag-str tag nil
                         '(annotation :prefix "(" :suffix ")")
                         '(content))
     (xref-make-file-location (concat file-existance path) line 0))))

(defun citre-xref--get-definition-for-completed-symbol (symbol)
  "Get definition for SYMBOL without text property.
When xref prompts for user input for the symbol, we can't get
information from the environment of the symbol at point, so we
have to bypass the whole filter/sort mechanism of Citre and use
simple tag name matching.  This function is for it."
  (citre-get-tags nil symbol 'exact
                  :filter citre-xref--filter
                  :sorter citre-definition-default-sorter
                  :require '(name ext-abspath pattern)
                  :optional '(ext-kind-full line typeref extras)))

(defun citre-xref--find-definition (symbol)
  "Return the xref object of the definition information of SYMBOL."
  (mapcar #'citre-xref--make-object
          (if (citre-get-property 'xref-get-at-point symbol)
              (citre-get-definitions symbol)
            (citre-xref--get-definition-for-completed-symbol symbol))))

(defun citre-xref-backend ()
  "Define the Citre backend for xref."
  'citre)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql citre)))
  "Define method for xref to get symbol at point."
  (citre-put-property (citre-get-symbol)
                      'xref-get-at-point t))

(cl-defmethod xref-backend-definitions ((_backend (eql citre)) symbol)
  "Define method for xref to find definition of SYMBOL."
  (citre-xref--find-definition symbol))

(cl-defmethod xref-backend-identifier-completion-table
  ((_backend (eql citre)))
  "Return a function for xref to find all completions of a prefix."
  (lambda (str pred action)
    (let* ((tagsfile (citre-tags-file-path))
           (update-time (gethash 'time (citre-core-tags-file-info tagsfile)))
           (collection
            (if (and (equal tagsfile
                            (plist-get citre-xref--completion-table-cache
                                       :tags-file))
                     (equal update-time
                            (plist-get citre-xref--completion-table-cache
                                       :time)))
                (plist-get citre-xref--completion-table-cache :collection)
              (let ((collection
                     (cl-remove-duplicates
                      (mapcar
                       (lambda (tag) (citre-core-get-field 'name tag))
                       (citre-get-tags
                        nil str nil
                        :filter citre-xref--filter
                        :sorter (citre-core-sorter '(length name +) 'name)
                        :require '(name)))
                      :test #'equal)))
                (plist-put citre-xref--completion-table-cache
                           :tags-file tagsfile)
                (plist-put citre-xref--completion-table-cache
                           :time update-time)
                (plist-put citre-xref--completion-table-cache
                           :collection collection)
                collection))))
      (complete-with-action action collection str pred))))

;;;; Tool: `citre-jump'

;;;;; Internals

(defvar citre--marker-ring (make-ring 50)
  "The marker ring used by `citre-jump'.")

(defun citre-jump-completing-read (definitions symbol)
  "Select an element in DEFINITIONS, with SYMBOL as a prompt.
This uses the `completing-read' interface.  See
`citre-jump-select-definition-function' for the use of this function."
  (pcase (length definitions)
    (1 (car definitions))
    (_ (completing-read (format "%s: " symbol) definitions nil t))))

;;;;; Commands

(defun citre-jump ()
  "Jump to the definition of the symbol at point.
During an active `citre-peek' session, this jumps to the
definition that is currently peeked."
  (interactive)
  (let* ((marker (point-marker))
         (symbol (citre-get-symbol))
         (definitions (citre-get-definitions))
         (root (funcall citre-project-root-function))
         (loc-alist
          (mapcar (lambda (def)
                    (cons
                     (citre-make-tag-str
                      def nil
                      '(annotation)
                      `(location :suffix ":" :root ,root)
                      '(content))
                     def))
                  definitions))
         (locations (mapcar #'car loc-alist)))
    (if (null locations)
        (user-error "Can't find definition for %s" symbol)
      (citre-goto-tag (alist-get
                       (funcall citre-jump-select-definition-function
                                locations symbol)
                       loc-alist nil nil #'equal)))
    (ring-insert citre--marker-ring marker)))

(defun citre-jump-back ()
  "Go back to the position before last `citre-jump'."
  (interactive)
  (let ((ring citre--marker-ring))
    (when (ring-empty-p ring)
      (user-error "No more previous history"))
    (let ((marker (ring-remove ring 0)))
      (switch-to-buffer
       (or (marker-buffer marker)
           (user-error "The previous buffer has been deleted")))
      (goto-char (marker-position marker))
      (set-marker marker nil)
      (run-hooks 'citre-after-jump-hook))))

;;;; Tool: Capf integration

;;;;; Internals

(defvar citre--completion-cache
  `(:file nil :symbol nil :bounds nil :substr nil :collection nil)
  "A plist for completion cache.
Its props are:

- `:file': The file where the completion happens.
- `:symbol': The symbol that's been completed.
- `:bounds': The bound positions of `:symbol'.
- `:substr': Whether substring completion is used.  This is
  needed since in the same position, user may use popup
  completion that does prefix completion, and use their own
  command that let binds `citre-capf-substr-completion' to t and
  call `completion-at-point'.
- `:collection': The completion string collection.")

(defun citre--completion-get-annotation (str)
  "Generate annotation for STR.
STR is a candidate in a capf session.  See the implementation of
`citre-completion-at-point'."
  (let* ((kind (citre-get-property 'kind str))
         (type (citre-get-property 'type str))
         (face 'citre-definition-annotation-face))
    (when (or kind type)
      (concat
       (propertize " (" 'face face)
       (propertize (or kind "") 'face face)
       (if (and kind type) citre-definition-annotation-separator "")
       (propertize (or type "") 'face face)
       (propertize ")" 'face face)))))

(defun citre--completion-make-collection (tags)
  "Make collection for auto-completion of TAGS."
  (let* ((collection
          (mapcar
           (lambda (tag)
             (citre-put-property
              (citre-make-tag-str tag nil '(name))
              'kind
              (citre-core-get-field 'ext-kind-full tag)
              'type
              (citre-core-get-field 'typeref tag 'after-colon)
              'signature
              (citre-core-get-field 'signature tag 'after-colon)))
           tags))
         ;; `equal-including-properties' doesn't work. I don't know why, maybe
         ;; it uses `eq' to compare the properties.
         (str-equal
          (lambda (str1 str2)
            (and (equal str1 str2)
                 (null (cl-position
                        nil
                        (mapcar (lambda (prop)
                                  (equal (citre-get-property prop str1)
                                         (citre-get-property prop str2)))
                                '(kind type signature))))))))
    (cl-remove-duplicates
     collection :test str-equal)))

(defun citre--capf-get-completions (symbol)
  "Get completions of SYMBOL for capf.
This may return nil when `citre-capf-optimize-for-popup' is
non-nil, and the calculation is interrupted by user input."
  (if citre-capf-optimize-for-popup
      (pcase (while-no-input
               (citre-get-completions
                symbol nil citre-capf-substr-completion))
        ('t nil)
        (val val))
    (citre-get-completions symbol nil citre-capf-substr-completion)))

(defun citre--capf-get-collection (symbol)
  "Get completion collection of SYMBOL for capf."
  (if citre-capf-optimize-for-popup
      (let* ((cache citre--completion-cache)
             (file (buffer-file-name))
             (bounds (citre-get-property 'bounds symbol)))
        (if (and citre-capf-optimize-for-popup
                 (equal (plist-get cache :file) file)
                 (string-prefix-p (plist-get cache :symbol) symbol)
                 ;; We also need to make sure we are in the process of
                 ;; completing the same whole symbol, since same symbol in
                 ;; different positions can produce different results,
                 ;; depending on the language support implementation.
                 (eq (car (plist-get cache :bounds)) (car bounds))
                 ;; Just in case the user set `citre-capf-substr-completion' to
                 ;; something can't compare by `eq', we use `null' to make sure
                 ;; we are comparing t or nil.
                 (eq (null (plist-get cache :substr))
                     (null citre-capf-substr-completion)))
            (plist-get cache :collection)
          ;; Make sure we get a non-nil collection first, then setup the cache,
          ;; since the calculation can be interrupted by user input, and we get
          ;; nil, which aren't the actual completions.
          (when-let ((collection (citre--completion-make-collection
                                  (citre--capf-get-completions symbol))))
            (plist-put cache :file file)
            (plist-put cache :symbol (substring-no-properties symbol))
            (plist-put cache :bounds bounds)
            (plist-put cache :substr citre-capf-substr-completion)
            (plist-put cache :collection collection)
            collection)))
    (citre--completion-make-collection
     (citre--capf-get-completions symbol))))

;;;;; Entry point

(defun citre-completion-at-point ()
  "Function used for `completion-at-point-functions'."
  (when-let* ((symbol (citre-get-symbol))
              (bounds (citre-get-property 'bounds symbol))
              (start (car bounds))
              (end (cdr bounds))
              (collection (citre--capf-get-collection symbol))
              (get-docsig
               (lambda (cand)
                 (citre-get-property 'signature cand))))
    (list start end collection
          :annotation-function #'citre--completion-get-annotation
          :company-docsig get-docsig
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

;;;; Tool: Imenu integration

(defvar-local citre--imenu-create-index-function-orig nil
  "Original value of `imenu-create-index-function' in buffer.")

(defun citre--classify-tags (tags)
  "Classify TAGS based on the `ext-kind-full' field.
This creates an alist, its key is `kind' field value, and value
is a list of tags of that kind."
  (let ((result nil))
    (dolist (tag tags)
      (cl-symbol-macrolet ((place (alist-get kind result nil nil #'equal)))
        (let ((kind (citre-core-get-field 'ext-kind-full tag)))
          (unless place
            (setf place nil))
          (push tag place))))
    (dotimes (i (length result))
      (setf (cdr (nth i result))
            (nreverse (cdr (nth i result)))))
    (cl-sort result (lambda (str1 str2)
                      (compare-strings str1 nil nil str2 nil nil))
             :key #'car)))

(defun citre--make-imenu-index (tag)
  "Create Imenu index for TAG."
  (cons (citre-make-tag-str tag nil
                            '(name)
                            '(annotation :no-kind t :prefix "(" :suffix ")")
                            '(location :no-path t))
        (citre-core-locate-tag tag)))

(defun citre-imenu-create-index-function ()
  "Create imenu index."
  (let* ((file (buffer-file-name))
         (tags-file (citre-tags-file-path))
         (tags (citre-get-tags
                nil nil nil
                :filter
                `(and ,(citre-core-filter-input file tags-file)
                      (not ,(citre-core-filter
                             'extras
                             '("anonymous" "reference" "inputFile")
                             'csv-contain)
                           ,(citre-core-filter-kind "file" tags-file)))
                :sorter (citre-core-sorter 'line)
                :require '(name pattern)
                :optional '(ext-kind-full line typeref extras)))
         (tags (citre--classify-tags tags)))
    (dotimes (i (length tags))
      (setf (cdr (nth i tags))
            (mapcar #'citre--make-imenu-index (cdr (nth i tags)))))
    tags))

;;;; Tool: Citre mode

;;;###autoload
(define-minor-mode citre-mode
  "Ctags IDE on the True Editor"
  :lighter " Citre"
  (cond
   (citre-mode
    (when citre-enable-xref-integration
      (add-hook 'xref-backend-functions #'citre-xref-backend nil t))
    (when citre-enable-capf-integration
      (add-hook 'completion-at-point-functions
                #'citre-completion-at-point nil t))
    (when citre-enable-imenu-integration
      (setq citre--imenu-create-index-function-orig
            imenu-create-index-function)
      (setq imenu-create-index-function #'citre-imenu-create-index-function)))
   (t
    (remove-hook 'xref-backend-functions #'citre-xref-backend t)
    (remove-hook 'completion-at-point-functions
                 #'citre-completion-at-point t)
    (when citre-enable-imenu-integration
      (setq imenu-create-index-function
            citre--imenu-create-index-function-orig)))))

(defun citre-auto-enable-citre-mode ()
  "Enable `citre-mode' when a tags file can be found.
Put this in `find-file-hook' to automatically enable `citre-mode'
when opening a file."
  (when (citre-tags-file-path) (citre-mode)))

(provide 'citre-basic-tools)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-basic-tools.el ends here
