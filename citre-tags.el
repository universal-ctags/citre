;;; citre-tags.el --- Tags file backend -*- lexical-binding: t -*-

;; Copyright (C) 2022 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 25 May 2022
;; Keywords: convenience, tools
;; Homepage: https://github.com/universal-ctags/citre
;; Version: 0.2.1
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

;;; Code:

;; Tags file backend.  For now it only contains code of `citre-peek'.

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;;; Libraries

(require 'citre-ctags)
(require 'citre-ui-peek)
(require 'citre-util)

(defun citre-peek--current-tagsfile ()
  "Return the tagsfile for currently peeked file."
  (with-current-buffer
      (car (citre-peek--get-buf-and-pos
            (citre-peek--current-tag-node)))
    (citre-peek--hack-buffer-file-name
      (citre-tags-file-path))))

(defun citre-peek--set-current-tagsfile (tagsfile &optional maybe)
  "Set the tagsfile for currently peeked file to TAGSFILE.
When MAYBE is non-nil, don't do anything if the tags file of
currently peeked file can be detected.  Otherwise, always set
it."
  (with-current-buffer
      (car (citre-peek--get-buf-and-pos
            (citre-peek--current-tag-node)))
    (when (or (not maybe)
              (null (citre-peek--hack-buffer-file-name
                      (citre-tags-file-path))))
      (setq citre--tags-file tagsfile))))

;;;;; Find definitions

(defun citre-peek--get-definitions ()
  "Return definitions of symbol under point.
This works for temporary buffer created by `citre-peek'.

When in an xref buffer, return a single-element list of the tag
of the xref item under point, with the `name' field being
`citre-peek-root-symbol-str'."
  (citre-peek--hack-buffer-file-name
    (if-let* ((symbol (if (derived-mode-p 'xref--xref-buffer-mode)
                          (symbol-at-point)
                        (citre-get-symbol)))
              (definitions
                (if (derived-mode-p 'xref--xref-buffer-mode)
                    (when-let ((tag (citre-make-tag-of-current-xref-item
                                     citre-peek-root-symbol-str)))
                      (list tag))
                  (citre-get-definitions-maybe-update-tags-file
                   symbol))))
        definitions
      (if symbol
          (user-error "Can't find definitions for %s" symbol)
        (user-error "No symbol at point")))))

;;;; Commands

;;;###autoload
(defun citre-peek (&optional buf point)
  "Peek the definition of the symbol in BUF at POINT.
When BUF or POINT is nil, it's set to the current buffer and
point."
  (interactive)
  (let* ((buf (or buf (current-buffer)))
         (point (or point (point)))
         (defs (save-excursion
                 (with-current-buffer buf
                   (goto-char point)
                   (citre-peek--get-definitions))))
         (tagsfile (with-current-buffer buf
                     (citre-tags-file-path)))
         (marker (if (buffer-file-name) (point-marker))))
    (citre-peek-show defs marker)
    (citre-peek--set-current-tagsfile tagsfile 'maybe)))

;;;###autoload
(defun citre-ace-peek ()
  "Peek the definition of a symbol on screen using ace jump.
Press a key in `citre-peek-ace-pick-symbol-at-point-keys' to pick
the symbol under point.

This command is useful when you want to see the definition of a
function while filling its arglist."
  (interactive)
  (when-let ((pt (citre-ace-pick-point)))
    (citre-peek (current-buffer) pt)))

(defun citre-peek-through ()
  "Peek through a symbol in current peek window."
  (interactive)
  (when-let* ((buffer-point (citre-ace-pick-point-in-peek-window))
              (defs (save-excursion
                      (with-current-buffer (car buffer-point)
                        (goto-char (cdr buffer-point))
                        (citre-peek--get-definitions))))
              (tagsfile (citre-peek--current-tagsfile)))
    (citre-peek-make-current-def-first)
    (citre-peek--make-branch defs)
    (citre-peek--set-current-tagsfile tagsfile 'maybe)))

;; TODO: This should be in citre-ui-peek.el.  For now we keep it here as it
;; uses `citre--tags-file' (in citre-util.el)
(defun citre-peek-jump ()
  "Jump to the definition that is currently peeked."
  (interactive)
  (citre-peek--error-if-not-peeking)
  (let ((tagsfile (citre-peek--current-tagsfile)))
    (citre-peek-abort)
    (citre-goto-tag
     (citre-peek--tag-node-tag
      (citre-peek--current-tag-node)))
    (unless (citre-tags-file-path)
      (setq citre--tags-file tagsfile)))
  (when citre-peek-auto-restore-after-jump
    (citre-peek-restore)
    (when citre-peek-backward-in-chain-after-jump
      (citre-peek-chain-backward))))

(provide 'citre-tags)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-tags.el ends here
