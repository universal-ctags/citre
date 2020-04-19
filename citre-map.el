;;; citre-map.el --- A map in the maze of code reading -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 28 Feb 2020
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

(require 'citre)

;;;; User options

(defcustom citre-code-map-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "f") 'citre-code-map-forward)
    (define-key map (kbd "RET") 'citre-code-map-forward)
    (define-key map (kbd "b") 'citre-code-map-backward)
    (define-key map (kbd "m") 'citre-code-map-mark)
    (define-key map (kbd "M") 'citre-code-map-unmark-all)
    (define-key map (kbd "h") 'citre-code-map-hide)
    (define-key map (kbd "d") 'citre-code-map-delete)
    (define-key map (kbd "S") 'citre-code-map-show-all)
    (define-key map [remap save-buffer] 'citre-save-code-map)
    (define-key map [remap find-file] 'citre-load-code-map)
    map)
  "Keymap used in citre-code-map-mode."
  :group 'citre
  :type 'keymap)

;;;; Helpers

(defun citre--buffer-relative-file-name (&optional buffer)
  "Return the path relative to project root of file in current buffer.
If the file is not under its project, return the absolute path.
When BUFFER is specified, use filename and project in BUFFER
instead."
  (let* ((buf (or buffer (current-buffer))))
    (citre--relative-path (buffer-file-name buf) (citre--project-root buf))))

(defun citre--key-in-alist (elt alist)
  "Non-nil if ELT is a key in ALIST.
The test is done using `equal'."
  (cl-member elt alist :key #'car :test #'equal))

(defun citre--print-value-to-file (filename value)
  "Print VALUE to file FILENAME."
  (with-temp-file filename
    (prin1 value (current-buffer))))

(defun citre--read-value-from-file (filename)
  "Read value from file FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (read (current-buffer))))

;;;; Action: map

;;;;; Data Structures

(defvar citre--code-map-alist nil
  "Alist for code maps.
The key is project path, value is its code map.  The code map is
another nested alist with a structure like:

  (alist of:
   file-name -> (alist of:
                 symbol -> (list of:
                            (definition-record . hide-or-not))))

Once you know how to use the code map, this structure will make
sense.  See README for the user guide.")

(defvar citre--code-map-position-alist nil
  "Alist for latest positions in code maps.
The key is project path, value is the latest position in its code
map.  The position means the state after your last
`citre-code-map-forward', `citre-code-map-backward', or opening
the code map with commands like
`citre-see-symbol-in-code-map'.  It is a list of:

  (file-name symbol definition-location current-depth)

The meaning of current-depth is:

  0: In the file list.
  1: In the symbol list.
  2: In the definition list.")

(defvar citre--code-map-disk-state-alist nil
  "Alist for the disk state of code maps.
The key is project path, value is a cons pair.  The car is t or
nil, indicating whether the code map has been modified since last
save.  Cdr is the location on disk.")

(defmacro citre--get-in-code-map (&optional file symbol project)
  "Return the place form of file list in code map in current project.
If FILE is given, return the place form of the symbol list under it.

If SYMBOL is also given, return the place form of definition list under it.

If project root PROJECT is given, use that project instead.

Notice: Since this is a macro, the arguments are considered to be
non-nil as long as a form that is non-nil is presented, even when
its value is nil."
  (let* ((project (or project (citre--project-root))))
    (if file
        (if symbol
            `(alist-get ,symbol (citre--get-in-code-map ,file nil ,project)
                        nil nil #'equal)
          `(alist-get ,file (citre--get-in-code-map nil nil ,project)
                      nil nil #'equal))
      `(alist-get ,project citre--code-map-alist
                  nil nil #'equal))))

(defmacro citre--code-map-position (&optional project)
  "Return the place form of code map position in current project.
If project root PROJECT is given, use that project instead.

Notice: Since this is a macro, the arguments are considered to be
non-nil as long as a form that is non-nil is presented, even when
its value is nil."
  (let ((project (or project (citre--project-root))))
    `(alist-get ,project citre--code-map-position-alist
                nil nil #'equal)))

(defun citre--current-list-in-code-map (&optional project)
  "Return the current list in code map in current project.
\"Current list\" is determined by `citre--code-map-position-alist'.

If project root PROJECT is given, use that project instead."
  (let* ((project (or project (citre--project-root)))
         (pos (citre--code-map-position project)))
    (pcase (nth 3 pos)
      (0 (citre--get-in-code-map nil nil project))
      (1 (citre--get-in-code-map (car pos) nil project))
      (2 (citre--get-in-code-map (car pos) (nth 1 pos) project))
      (_ (error
          "Current depth in code map position should be an integer in 0~3")))))

(defun citre--get-current-item-in-code-map (&optional project)
  "Get current item in code map in current project.
If project root PROJECT is given, use that project instead."
  (let* ((project (or project (citre--project-root)))
         (pos (citre--code-map-position project))
         (depth (nth 3 pos)))
    (when (> depth 0)
      (nth (1- depth) pos))))

(defun citre--set-hide-state (record state)
  "Set the hide state of RECORD in definition list to STATE.
This uses `citre--code-map-position-alist' to know where is the
record."
  (let ((pos (citre--code-map-position)))
    (unless (= (nth 3 pos) 2)
      (error "Not browsing an definition list"))
    (let* ((definition-list (citre--get-in-code-map (car pos) (nth 1 pos)))
           (idx (cl-position record definition-list
                             :key #'car :test #'equal)))
      (unless idx
        (error "RECORD not found in current definition list"))
      (setf (cdr (nth idx
                      (citre--get-in-code-map (car pos) (nth 1 pos))))
            state))))

(defun citre--delete-item-in-code-map (item)
  "Remove ITEM in the current list in code map."
  (let* ((pos (citre--code-map-position))
         (pos-depth (nth 3 pos)))
    (pcase pos-depth
      (0 (setf (citre--get-in-code-map)
               (cl-delete item (citre--get-in-code-map)
                          :key #'car :test #'equal)))
      (1 (setf (citre--get-in-code-map (nth 0 pos))
               (cl-delete item (citre--get-in-code-map (nth 0 pos))
                          :key #'car :test #'equal)))
      (2 (error "Definitions can't be deleted")))))

(defun citre--set-code-map-position (&optional filename symbol definition project)
  "Set the code map position in current project.
This is based on FILENAME, SYMBOL and DEFINITION.

If project root PROJECT is given, use that project instead."
  (let ((project (or project (citre--project-root))))
    (cl-symbol-macrolet ((pos (citre--code-map-position project)))
      (let* ((file-presented (when filename (string= filename (car pos))))
             (symbol-presented (when symbol (string= symbol (nth 1 pos)))))
        ;; If map position is empty, set it first.
        (unless (consp pos)
          (setf pos '(nil nil nil 0)))
        (when (and (not filename) (not symbol) (not definition))
          (setf (nth 3 pos) 0))
        (when filename
          (unless file-presented
            (setf (car pos) filename))
          (setf (nth 3 pos) 1))
        (when (and filename symbol)
          (unless (and file-presented symbol-presented)
            (setf (nth 1 pos) symbol))
          (setf (nth 3 pos) 2))
        (when (and filename symbol definition)
          (setf (nth 2 pos) definition)
          (setf (nth 3 pos) 2))))))

(defun citre--set-code-map-disk-state (modified &optional location project)
  "Set the disk state of current code map to LOCATION and MODIFIED.
See the docstring of `citre--code-map-disk-state-alist' to know
their meaning.

When a path PROJECT is given, set the state of the code map of
PROJECT.

If called without LOCATION, this function will only have effect
when LOCATION already exists in the disk state.  The typical
usage is call with LOCATION after load/save a code map, and
without when changing its state in other situations."
  (let ((project (or project (citre--project-root))))
    (if location
        (setf (alist-get project
                         citre--code-map-disk-state-alist
                         nil nil #'equal)
              (cons modified location))
      (when (and (cl-member project
                            citre--code-map-disk-state-alist
                            :key #'car :test #'equal)
                 (car (alist-get project citre--code-map-alist
                                 nil nil #'equal)))
        (setf (car (alist-get project
                              citre--code-map-disk-state-alist
                              nil nil #'equal))
              modified)))))

(defun citre--get-code-map-disk-state (&optional project)
  "Get the disk state of current project.
When a path PROJECT is given, use that project instead."
  (let ((project (or project (citre--project-root))))
    (alist-get project citre--code-map-disk-state-alist
               nil nil #'equal)))

;;;;; Helpers: tabulated-list-mode extensions

;; TODO: customize mark
(defun citre--tabulated-list-mark ()
  "A wrapper around `tabulated-list-put-tag'.
This gives the mark a special text property so it can be detected
by `citre--tabulated-list-marked-p'."
  (tabulated-list-put-tag (propertize ">" 'face 'error 'citre-map-mark t)))

(defun citre--tabulated-list-unmark ()
  "Remove the mark in current line."
  (save-excursion
    (beginning-of-line)
    (when (tabulated-list-get-entry)
      (let ((inhibit-read-only t)
            (beg (point)))
        (forward-char tabulated-list-padding)
        (insert-and-inherit (make-string tabulated-list-padding ?\s))
        (delete-region beg (+ beg tabulated-list-padding))))))

(defun citre--tabulated-list-marked-p ()
  "Check if current line is marked."
  (save-excursion
    (beginning-of-line)
    (get-text-property (point) 'citre-map-mark)))

(defun citre--clamp-region (beg end)
  "Shrink the region from BEG to END.
BEG and END are two positions in the buffer.  A cons pair is
returned, its car is the next beginning of line after or at BEG,
and cdr is the previous beginning of line before END.

When such shrinked region doesn't exist (like BEG is in the last
line, or the car in the result is larger than cdr), nil will be
returned.

See `citre-code-map-mark' to get an idea about what's the purpose
of this function."
  (let ((result-beg nil)
        (result-end nil)
        (fail-flag nil))
    (save-excursion
      (goto-char beg)
      (unless (bolp)
        (forward-line)
        (when (eobp) (setq fail-flag t))
        (beginning-of-line))
      (setq result-beg (point))
      (goto-char end)
      (when (bobp) (setq fail-flag t))
      (if (bolp) (forward-line -1)
        (beginning-of-line))
      (setq result-end (point)))
    (when (and (not fail-flag) (<= result-beg result-end))
      (cons result-beg result-end))))

(defun citre--tabulated-list-marked-positions (&optional beg end)
  "Return positions of marked items in a tabulated list buffer.
The \"position of item\" means the beginning of its line.
When BEG and/or END are specified, use them as inclusive
boundaries of search.  That is, the lines at BEG and END are also
checked."
  (let ((positions nil)
        (beg-limit (or beg (point-min)))
        (end-limit (or end (point-max))))
    (save-excursion
      (goto-char beg-limit)
      (beginning-of-line)
      (while (and (<= (point) end-limit) (not (eobp)))
        (when (get-text-property (point) 'citre-map-mark)
          (push (point) positions))
        (forward-line)))
    positions))

(defun citre--tabulated-list-selected-positions ()
  "Return positions of items selected by an active region.
The \"position of item\" means the beginning of its line.

An item is \"selected by an active region\" means the beginning
of its line is in the \"clamped\" active region (including at its
boundaries), which is done by `citre--clamp-region'."
  (when (use-region-p)
    (let* ((positions nil)
           (region (citre--clamp-region (region-beginning) (region-end)))
           (beg (car region))
           (end (cdr region)))
      (goto-char beg)
      (while (<= (point) end)
        (push (point) positions)
        (forward-line))
      positions)))

;;;;; Code map mode and its helpers

(define-derived-mode citre-code-map-mode tabulated-list-mode
  "Code map"
  "Major mode for code map."
  (setq tabulated-list-padding 2))

(defun citre--find-position-near-region ()
  "Find a position near region.
See `citre--tabulated-list-print' to know its use."
  (when (region-active-p)
    (let ((upper nil)
          (lower nil)
          (pos nil)
          (region (citre--clamp-region (region-beginning)
                                       (region-end))))
      (save-excursion
        (goto-char (car region))
        (unless (bobp)
          (forward-line -1)
          (setq upper (point)))
        (goto-char (cdr region))
        (forward-line)
        (unless (eobp)
          (setq lower (point))))
      (if (= (point) (region-beginning))
          (setq pos (or upper lower))
        (setq pos (or lower upper)))
      pos)))

(defun citre--find-position-near-marked-items ()
  "Find a position near marked items when there is one at point.
See `citre--tabulated-list-print' to know its use."
  (when (citre--tabulated-list-marked-p)
    (let ((pos nil))
      (save-excursion
        (while (and (citre--tabulated-list-marked-p) (not (eobp)))
          (forward-line))
        (unless (citre--tabulated-list-marked-p)
          (setq pos (point))))
      (unless pos
        (save-excursion
          (while (and (citre--tabulated-list-marked-p) (not (bobp)))
            (forward-line -1))
          (unless (citre--tabulated-list-marked-p)
            (setq pos (point)))))
      pos)))

(defun citre--find-position-near-line ()
  "Find a position near this line.
See `citre--tabulated-list-print' to know its use."
  (let ((pos nil))
    (if (and (citre--tabulated-list-marked-positions)
             (not (citre--tabulated-list-marked-p)))
        (setq pos (point))
      (save-excursion
        (forward-line)
        (unless (eobp)
          (setq pos (point))))
      (unless pos
        (save-excursion
          (forward-line -1)
          (unless (bobp)
            (setq pos (point))))))
    pos))

(defun citre--code-map-print (&optional style)
  "A wrapper around `tabulated-list-print'.
This tries to always put the point and scroll the window to a
position that feels not intrusive and makes sense when browsing a
code map.

About the argument STYLE, see the docstring of
`citre--code-map-refresh'."
  (cond
   ((eq style 'remove-item)
    ;; Find the nearest item that's not to be removed, and restore its
    ;; position.
    (let ((pos (or (citre--find-position-near-region)
                   (citre--find-position-near-marked-items)
                   (citre--find-position-near-line))))
      (if pos (progn (goto-char pos)
                     (tabulated-list-print 'remember-pos 'update))
        (tabulated-list-print nil 'update))))
   ((eq style 'add-item)
    ;; When the added items are above the current line, some items will be push
    ;; beyond the start of window.  In a code map, items can fit in one screen
    ;; most of the time, so this can be confusing (where do my item goes?).  We
    ;; try to scroll down to restore the original line at the start of window,
    ;; but doesn't push current point below the end of window.
    (let ((window-start-linum-orig (line-number-at-pos (window-start)))
          (window-start-linum-new nil)
          (linum-in-window nil)
          (scroll-amount nil))
      (tabulated-list-print 'remember-pos 'update)
      (setq window-start-linum-new (line-number-at-pos (window-start)))
      (setq linum-in-window (1+ (- (line-number-at-pos)
                                   window-start-linum-new)))
      (setq scroll-amount (min (- (window-body-height) linum-in-window)
                               (max 0 (- window-start-linum-new
                                         window-start-linum-orig))))
      (scroll-down scroll-amount)))
   ((eq style 'switch-page)
    ;; We try to goto the line when we visit the page last time.
    (tabulated-list-print)
    (let* ((pos (citre--code-map-position))
           (idx (cl-position (nth (nth 3 pos) pos)
                             (citre--current-list-in-code-map)
                             :key #'car)))
      (when idx
        (goto-char (point-min))
        (forward-line idx))))
   (t
    (tabulated-list-print 'remember-pos 'update))))

(defun citre--code-map-make-entry-string (str)
  "Make entry for file names and symbols in the code map buffer.
STR is the file name or symbol name."
  (list str (vector str)))

(defun citre--code-map-make-entry-definition (record)
  "Make entry for definition locations in the code map buffer.
RECORD is the record of the definition."
  (list record
        (vector (format "%s: %s"
                        (propertize (citre--relative-path
                                     (citre-get-field 'path record))
                                    'face 'warning)
                        (citre-get-field 'line record)))))

(defun citre--code-map-refresh (&optional style)
  "Refresh code map in current buffer.
This is based on the position information in
`citre--code-map-position-alist'.

STYLE determines how should we put the point and scroll the
window.  Its value can be:

  - remove-item: Use this if any item will be hidden/removed
    after refresh.

  - add-item: Use this if any item will be added after refresh.

  - switch-page: Use this if the page will be switched after
    refresh.

  - nil: Don't use STYLE if we are on the same page and no fancy
    things happen."
  (let* ((pos (citre--code-map-position))
         (pos-depth (nth 3 pos))
         (list (citre--current-list-in-code-map))
         (header (pcase pos-depth
                   (0 '[("File" 0 nil)])
                   (1 '[("Symbol" 0 nil)])
                   (_ '[("Definition" 0 nil)])))
         (locations (when (>= 2 pos-depth)
                      (cl-delete nil
                                 (mapcar
                                  (lambda (location)
                                    (unless (cdr location) (car location)))
                                  list))))
         (entries (if (<= pos-depth 1)
                      (mapcar #'citre--code-map-make-entry-string
                              (mapcar #'car list))
                    (mapcar #'citre--code-map-make-entry-definition
                            locations))))
    (setq tabulated-list-format header)
    (setq tabulated-list-entries entries)
    (tabulated-list-init-header)
    (citre--code-map-print style)))

(defun citre--open-code-map (&optional project)
  "Open code map for current project.
If project root PROJECT is given, use that project instead."
  (let* ((project (or project (citre--project-root)))
         (map-buf-name (format "*Code map: %s*"
                               (abbreviate-file-name project))))
    (if (get-buffer map-buf-name)
        (progn
          (pop-to-buffer (get-buffer map-buf-name))
          (citre--code-map-refresh 'switch-page))
      (pop-to-buffer (generate-new-buffer map-buf-name))
      (citre-code-map-mode)
      (setq citre-project-root project)
      (citre--code-map-refresh 'switch-page))))

(defun citre--error-if-not-in-code-map ()
  "Signal an error if not in a code map."
  (unless (derived-mode-p 'citre-code-map-mode)
    (user-error "This command is for code map only")))

;;;;; Commands

(defun citre-see-symbol-in-code-map ()
  "See the definition locations of symbol at point in code map.
If the symbol is not in the symbol list, add it to the list."
  (interactive)
  (let ((sym (thing-at-point 'symbol 'no-properties)))
    (unless sym
      (user-error "No symbol at point"))
    (unless (citre--key-in-alist sym
                                 (citre--get-in-code-map
                                  (citre--buffer-relative-file-name)))
      (let ((locations (citre-get-records sym 'exact)))
        (unless locations
          (user-error "Can't find definition"))
        (setf (citre--get-in-code-map
               (citre--buffer-relative-file-name) sym)
              (mapcar #'list locations))
        (citre--set-code-map-disk-state t)))
    (citre--set-code-map-position (citre--buffer-relative-file-name) sym)
    (citre--open-code-map)))

(defun citre-see-file-in-code-map ()
  "See current file in code map."
  (interactive)
  (let ((file (citre--buffer-relative-file-name)))
    (unless (citre--key-in-alist file
                                 (citre--get-in-code-map))
      (user-error
       "File not in code map.  Add a symbol in this file to the map first"))
    (citre--set-code-map-position file)
    (citre--open-code-map)))

(defun citre-see-code-map ()
  "See the code map.
This will restore the status when you leave the map."
  (interactive)
  (let* ((map-buf-name (format "*Code map: %s*"
                               (abbreviate-file-name
                                (citre--project-root)))))
    ;; Don't refresh buffer (by calling `citre--open-code-map') if we have an
    ;; existing code map buffer.
    (if (get-buffer map-buf-name)
        (pop-to-buffer (get-buffer map-buf-name))
      (citre--open-code-map))))

(defun citre-code-map-backward ()
  "Go \"back\" in the code map.
This means to go from the definition list to the symbol list, and
further to the file list."
  (interactive)
  (citre--error-if-not-in-code-map)
  (let* ((pos-depth (nth 3 (citre--code-map-position))))
    (when (>= pos-depth 1)
      (setf (nth 3 (citre--code-map-position)) (1- pos-depth))
      (citre--code-map-refresh 'switch-page))))

;; TODO: If call this on a file, and the file is not visited by any buffer, it
;; should be opened.
(defun citre-code-map-forward ()
  "Go \"forward\" in the code map.
This means to go from the file list into the symbol list, or
further to the definition list, and finally to the location of a
definition."
  (interactive)
  (citre--error-if-not-in-code-map)
  (let* ((id (tabulated-list-get-id))
         (pos-depth (nth 3 (citre--code-map-position))))
    (when id
      (setf (nth pos-depth (citre--code-map-position)) id)
      (if (< pos-depth 2)
          (progn
            (setf (nth 3 (citre--code-map-position)) (1+ pos-depth))
            (citre--code-map-refresh 'switch-page))
        (citre--open-file-and-goto-line (citre-get-field 'path id)
                                        (citre-get-field 'linum id)
                                        'other-window-noselect)))))

(defun citre-code-map-mark ()
  "Mark or unmark current item.
When a region is active, mark all items in the region, or unmark
if they are already all marked.

An item is considered to be in the region if its beginning of
line is inside, or at the beginning, but not at the end of the
region.  This should be intuitive to use."
  (interactive)
  (citre--error-if-not-in-code-map)
  (if (use-region-p)
      (let* ((pos-in-region (citre--tabulated-list-selected-positions))
             (marked-pos (citre--tabulated-list-marked-positions))
             (region-all-marked-p (cl-subsetp pos-in-region marked-pos)))
        (save-excursion
          (dolist (pos pos-in-region)
            (goto-char pos)
            (if region-all-marked-p
                (citre--tabulated-list-unmark)
              (citre--tabulated-list-mark)))))
    (if (citre--tabulated-list-marked-p)
        (citre--tabulated-list-unmark)
      (citre--tabulated-list-mark))
    (forward-line)))

(defun citre-code-map-unmark-all ()
  "Unmark all items."
  (interactive)
  (citre--error-if-not-in-code-map)
  (save-excursion
    (goto-char (point-min))
    (while (and (<= (point) (point-max)) (not (eobp)))
      (when (get-text-property (point) 'citre-map-mark)
        (citre--tabulated-list-unmark))
      (forward-line))))

;; TODO: refactor this like `citre-code-map-delete', so that a region that
;; doesn't contain an item produces an empty "items to hide" list, so the disk
;; state is not modified.
(defun citre-code-map-hide ()
  "Hide some of the definitions.
If there's an active region, hide definitions in the region; If
there are no active regions, but marked definitions, hide them.
Otherwise hide current definition.

An item is considered to be in the region if its beginning of
line is inside, or at the beginning, but not at the end of the
region.  This should be intuitive to use."
  (interactive)
  (citre--error-if-not-in-code-map)
  (let* ((pos-depth (nth 3 (citre--code-map-position)))
         (pos-to-hide (or (citre--tabulated-list-selected-positions)
                          (citre--tabulated-list-marked-positions)
                          (list (point)))))
    (when (< pos-depth 2)
      (user-error "Hide can only be used on definitions"))
    (when pos-to-hide
      (save-excursion
        (dolist (pos pos-to-hide)
          (goto-char pos)
          (citre--set-hide-state (tabulated-list-get-id) t)))
      (citre--set-code-map-disk-state t)
      (citre--code-map-refresh 'remove-item))))

(defun citre-code-map-delete ()
  "Delete some of the items in the current list.
If there's an active region, delete items in the region; if there
are no active regions, but marked items, delete them.  Otherwise
delete current item.

This can't be undone, so it will ask whether you really want to
delete them."
  (interactive)
  (citre--error-if-not-in-code-map)
  (let* ((pos-depth (nth 3 (citre--code-map-position)))
         (pos-to-delete (or (citre--tabulated-list-selected-positions)
                            (citre--tabulated-list-marked-positions)
                            (list (point)))))
    (when (= pos-depth 2)
      (user-error "Only symbols or files can be removed"))
    (when (and pos-to-delete
               (y-or-n-p "This can't be undone.  Really delete the item(s)? "))
      (save-excursion
        (dolist (pos pos-to-delete)
          (goto-char pos)
          (citre--delete-item-in-code-map (tabulated-list-get-id))))
      (citre--set-code-map-disk-state t)
      (citre--code-map-refresh 'remove-item))))

(defun citre-code-map-show-all ()
  "Show hidden definitions."
  (interactive)
  (citre--error-if-not-in-code-map)
  (let* ((pos (citre--code-map-position))
         (pos-depth (nth 3 (citre--code-map-position)))
         (definition-list (citre--get-in-code-map (car pos) (nth 1 pos)))
         (added-ids nil))
    (when (< pos-depth 2)
      (user-error "Hide is only for definitions"))
    (dotimes (n (length definition-list))
      (when (cdr (nth n definition-list))
        (push (car (nth n definition-list)) added-ids))
      (setf (cdr (nth n (citre--get-in-code-map (car pos) (nth 1 pos)))) nil))
    (citre--set-code-map-disk-state t)
    (citre--code-map-refresh 'add-item)
    (save-excursion
      (goto-char (point-min))
      (while (and (<= (point) (point-max)) (not (eobp)))
        (when (member (tabulated-list-get-id) added-ids)
          (citre--tabulated-list-mark))
        (forward-line)))))

(defun citre-code-map-keep ()
  "Keep marked items.
This means hide unmarked items in definition list, or remove
unmarked items in symbol list or file list.

The remove operation can't be undone, so it will ask whether you
really want to remove them.")

(defun citre-code-map-refresh ()
  "Refresh current item, or marked items if there are any.
This means to rescan definitions of current/marked symbols, or
rescan definitions of all symbols in current/marked files.")

(defun citre-save-code-map (&optional project)
  "Save map to a file.
When PROJECT is specified, save the code map of PROJECT."
  (interactive)
  (let* ((project (or project (citre--project-root)))
         (file (cdr (citre--get-code-map-disk-state project)))
         (dir (if file (file-name-directory file) project))
         (filename (if file (file-name-nondirectory file) ".codemap"))
         (saveto (read-file-name "Save to: " dir nil nil filename)))
    (unless (string-empty-p saveto)
      (citre--print-value-to-file
       saveto
       `(:project-root
         ,project
         :map
         ,(citre--get-in-code-map nil nil project)
         :position
         ,(citre--code-map-position project)))
      (citre--set-code-map-disk-state nil saveto project)
      (message "Code map of %s saved" project))))

(defun citre-load-code-map ()
  "Load map from file."
  (interactive)
  (let* ((file (cdr (citre--get-code-map-disk-state)))
         (dir (if file (file-name-directory file) (citre--project-root)))
         (filename (when (and dir
                              (file-readable-p (concat dir ".codemap")))
                     ".codemap"))
         (readfrom (read-file-name "Read from: " dir nil t filename)))
    (unless (string-empty-p readfrom)
      (let* ((data (citre--read-value-from-file readfrom))
             (project (plist-get data :project-root)))
        (when (or (not (car (citre--get-code-map-disk-state project)))
                  (y-or-n-p
                   (format "Current code map of %s is not saved.  Continue? "
                           project)))
          (setf (citre--get-in-code-map nil nil project) (plist-get data :map))
          (setf (citre--code-map-position project) (plist-get data :position))
          (citre--set-code-map-disk-state nil readfrom project)
          (citre--open-code-map project)
          (message "Code map of %s loaded" project))))))

;;;; Setup

(defun citre--ask-for-save-code-map ()
  "Ask the user to save unsaved code maps.
This is run when you quit Emacs with
`save-buffers-kill-terminal'.  It only deals with code maps you
read from disk or saved once."
  (when citre--code-map-disk-state-alist
    (dolist (pair citre--code-map-disk-state-alist)
      (when (cadr pair)
        (when (y-or-n-p (format "Save code map of %s? " (car pair)))
          (citre-save-code-map (car pair))))))
  t)

(add-hook 'kill-emacs-query-functions #'citre--ask-for-save-code-map)

(provide 'citre-map)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; citre-map.el ends here
