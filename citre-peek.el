;;; citre-peek.el --- Go down the rabbit hole without leaving current buffer -*- lexical-binding: t -*-

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

;;; TODOs:

;; - Cross reading
;; - History management
;; - Peek through (see the first line of this file)
;; - Ace peek
;;   (for situations like peeking function definition when inside its arglist)

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;;; Libraries

(require 'citre-util)
(require 'color)

;;;; User options

(defcustom citre-peek-file-content-height 12
  "Number of lines displaying file contents in the peek window."
  :type 'integer
  :group 'citre)

(defcustom citre-peek-locations-height 3
  "Number of locations displayed in the peek window."
  :type 'integer
  :group 'citre)

(defcustom citre-peek-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") 'citre-peek-next-line)
    (define-key map (kbd "M-p") 'citre-peek-prev-line)
    (define-key map (kbd "M-N") 'citre-peek-next-location)
    (define-key map (kbd "M-P") 'citre-peek-prev-location)
    (define-key map [remap keyboard-quit] 'citre-peek-abort)
    map)
  "Keymap used for `citre-peek' sessions."
  :type 'keymap
  :group 'citre)

(defface citre-peek-border-face
  '((((background light))
     :height 15 :background "#ffcbd3" :extend t)
    (t
     :height 15 :background "#db8e93" :extend t))
  "Face used for borders of peek windows.
You can customize the appearance of the borders by setting the
height and background properties of this face.

In the terminal version of Emacs, a dashed pattern is used as the
border, and only the background property of this face is used, as
the color of the dashes."
  :group 'citre)

(defface citre-definition-annotation-face
  '((((background light))
     :foreground "#666666" :slant italic)
    (t
     :foreground "#c0c0c0" :slant italic))
  "Face used for annotations when presenting a definition.
Annotations include kind, type, etc."
  :group 'citre)

;;;; Helpers

(defun citre--subseq (seq interval)
  "Return the subsequence of SEQ in INTERVAL.
INTERVAL is a cons pair of non-negative integers.  Its car is the
starting index, cdr is the ending index (not included).  Cdr can
be smaller than car, then the result will go from the index car,
to the end of SEQ, then back to the start of SEQ, and end before
the index cdr."
  (let ((start (car interval))
        (end (cdr interval)))
    (if (<= start end)
        (cl-subseq seq start end)
      (append
       (cl-subseq seq start)
       (cl-subseq seq 0 end)))))

(defun citre--index-in-interval (num interval wrapnum)
  "Return the index of NUM inside INTERVAL, or nil if it's not inside.
INTERVAL is a cons pair of integers.  The car is included, and
cdr is not included.  Cdr can be smaller than car, which means
the interval goes from car to WRAPNUM (not included), then from 0
to cdr (not included)."
  (let* ((start (car interval))
         (end (cdr interval))
         (len (if (<= start end)
                  (- end start)
                (+ (- wrapnum start) end)))
         (index (- num start)))
    (when (< num wrapnum)
      (when (< index 0)
        (setq index (+ index wrapnum)))
      (when (< index len)
        index))))

(defun citre--file-total-lines (path)
  "Return the total number of lines of file PATH."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (line-number-at-pos (point-max)))))

(defun citre--fit-line (str)
  "Fit STR in current window.
When STR is too long, it will be truncated, and \"...\" is added
at the end."
  ;; Depending on the wrapping behavior, in some terminals, a line with exact
  ;; (window-body-width) characters can be wrapped. So we minus it by one.
  (let ((limit (1- (window-body-width))))
    (if (> (length str) limit)
        (concat (substring str 0 (- limit 3))
                "...")
      str)))

;; Ref: https://www.w3.org/TR/WCAG20/#relativeluminancedef
(defun citre--color-srgb-to-rgb (c)
  "Convert an sRGB component C to an RGB one."
  (if (<= c 0.03928)
      (/ c 12.92)
    (expt (/ (+ c 0.055) 1.055) 2.4)))

(defun citre--color-rgb-to-srgb (c)
  "Convert an RGB component C to an sRGB one."
  (if (<= c (/ 0.03928 12.92))
      (* c 12.92)
    (- (* 1.055 (expt c (/ 1 2.4))) 0.055)))

(defun citre--color-blend (c1 c2 alpha)
  "Blend two colors C1 and C2 with ALPHA.
C1 and C2 are hexadecimal strings.  ALPHA is a number between 0.0
and 1.0 which is the influence of C1 on the result.

The blending is done in the sRGB space, which should make ALPHA
feels more linear to human eyes."
  (pcase-let ((`(,r1 ,g1 ,b1)
               (mapcar #'citre--color-rgb-to-srgb
                       (color-name-to-rgb c1)))
              (`(,r2 ,g2 ,b2)
               (mapcar #'citre--color-rgb-to-srgb
                       (color-name-to-rgb c2)))
              (blend-and-to-rgb
               (lambda (x y)
                 (citre--color-srgb-to-rgb
                  (+ (* alpha x)
                     (* (- 1 alpha) y))))))
    (color-rgb-to-hex
     (funcall blend-and-to-rgb r1 r2)
     (funcall blend-and-to-rgb g1 g2)
     (funcall blend-and-to-rgb b1 b2)
     2)))

(defun citre--add-face (str face)
  "Add FACE to STR, and return it.
This is mainly for displaying STR in an overlay.  For example, if
FACE specifies background color, then STR will have that
background color, with all other face attributes preserved.

`default' face is appended to make sure the display in overlay is
not affected by its surroundings."
  (let ((len (length str)))
    (add-face-text-property 0 len face nil str)
    (add-face-text-property 0 len 'default 'append str)
    str))

;;;; Internals

(define-minor-mode citre-peek-mode
  "Mode for `citre-peek'."
  :keymap citre-peek-keymap)

(defvar-local citre-peek--ov nil
  "Current overlay used for peeking.")

(defvar-local citre-peek--locations nil
  "List of definition locations used when peeking.
Each element is a string to be displayed, with text properties
`citre-ext-abspath' being the absolute path, and `citre-line'
being the line number.")

(defvar-local citre-peek--displayed-locations-interval nil
  "The interval of displayed locations in `citre-peek--locations'.
This is a cons pair, its car is the index of the first displayed
location, and cdr is the index of the last one plus one.")

(defvar-local citre-peek--location-index nil
  "The index of current location in `citre-peek--locations'.")

(defvar-local citre-peek--temp-buffer-alist nil
  "Files and their temporary buffers that don't exist before peeking.
Its keys are file paths, values are buffers.  The buffers will be
killed after `citre-peek-abort'.")

(defvar citre-peek--bg nil
  "Background color used for file contents when peeking.")

(defvar citre-peek--bg-alt nil
  "Background color used for unselected locations when peeking.")

(defvar citre-peek--bg-selected nil
  "Background color used for selected locations when peeking.")

;; Actually we can make Emacs believe our temp buffer is visiting FILENAME (by
;; setting `buffer-file-name' and `buffer-file-truename'), but then the buffer
;; is not hidden (Emacs hides buffers whose name begin with a space, but those
;; visiting a file are not hidden), and Emacs ask you to confirm when killing
;; it because its content are modified.  Rather than trying to workaround these
;; issues, it's better to create this function instead.
(defun citre-peek--find-buffer-visiting (filename)
  "Return the buffer visiting file FILENAME.
This is like `find-buffer-visiting', but it also searches
`citre-peek--temp-buffer-alist', so it can handle temporary
buffers created during peeking."
  (or (alist-get filename citre-peek--temp-buffer-alist)
      (find-buffer-visiting filename)))

(defun citre-peek--get-linum (record)
  "Get the line number of tag RECORD.
If there's no buffer visiting PATH currently, create a new
temporary buffer for it.  It will be killed by `citre-abort'.

If the file pointed to by RECORD doesn't exist, returns 1.  This
is because we want to display a one-line message about the
missing file in the peek window."
  ;; TODO: is this `delay-mode-hooks' needed?
  (delay-mode-hooks
    (let* ((path (citre-core-get-field 'ext-abspath record))
           (buf-opened (citre-peek--find-buffer-visiting path))
           (buf nil))
      (if (not (file-exists-p path))
          1
        (if buf-opened
            (setq buf buf-opened)
          (setq buf (generate-new-buffer (format " *citre-peek-%s*" path)))
          (with-current-buffer buf
            (insert-file-contents path)
            ;; `set-auto-mode' checks `buffer-file-name' to set major mode.
            (let ((buffer-file-name path))
              (delay-mode-hooks
                (set-auto-mode))))
          (push (cons path buf) citre-peek--temp-buffer-alist))
        (with-current-buffer buf
          (citre-core-locate-tag record 'use-linum))))))

(defun citre-peek--get-content (path line n)
  "Get file contents for peeking.
PATH is the path of the file.  LINE is the starting line.  N is
the number of lines.

This must be called when a record pointing to PATH is already
processed by `citre-peek--get-linum' earlier in a `citre-peek'
session, or it may think the file doesn't exist and returns a
message about the missing file."
  (if-let ((buf (citre-peek--find-buffer-visiting path)))
      (with-current-buffer buf
        (let ((beg nil)
              (end nil))
          (save-excursion
            (goto-char (point-min))
            (forward-line (1- line))
            (setq beg (point))
            (forward-line (1- n))
            (setq end (point-at-eol))
            (font-lock-fontify-region beg end)
            (concat (buffer-substring beg end) "\n"))))
    (propertize "This file doesn't exist.\n" 'face 'error)))

(defun citre-peek--location-index-forward (n)
  "In a peek window, move current location N steps forward.
N can be negative."
  (let ((start (car citre-peek--displayed-locations-interval))
        (end (cdr citre-peek--displayed-locations-interval))
        (len (length citre-peek--locations)))
    (setq citre-peek--location-index
          (mod (+ n citre-peek--location-index) len))
    (unless (citre--index-in-interval
             citre-peek--location-index
             citre-peek--displayed-locations-interval len)
      (setcar citre-peek--displayed-locations-interval
              (mod (+ n start) len))
      (setcdr citre-peek--displayed-locations-interval
              (mod (+ n end) len)))))

(defun citre-peek--line-forward (n)
  "In a peek window, scroll N lines forward.
N can be negative."
  (let* ((loc (nth citre-peek--location-index
                   citre-peek--locations))
         (target (+ n (citre-get-property loc 'peek-line)))
         (total-lines (citre-get-property loc 'total-lines))
         (target (cond
                  ((< target 1) 1)
                  ((> target total-lines) total-lines)
                  (t target))))
    (citre-put-property loc 'peek-line target)))

(defun citre-peek--make-border ()
  "Return the border to be used in peek windows."
  (if (display-graphic-p)
      (propertize "\n" 'face 'citre-peek-border-face)
    (propertize
     (concat (make-string (1- (window-body-width)) ?-) "\n")
     'face (list :inherit 'default
                 :foreground
                 (face-attribute 'citre-peek-border-face
                                 :background)))))

(defun citre-peek--post-command-function ()
  "Deal with the update of contents in peek windows."
  (unless (minibufferp)
    (let ((overlay-pos (min (point-max) (1+ (point-at-eol)))))
      (move-overlay citre-peek--ov overlay-pos overlay-pos))
    (let* ((loc (nth citre-peek--location-index citre-peek--locations))
           (loc-numbers (length citre-peek--locations))
           (initial-newline (if (eq (line-end-position) (point-max))
                                "\n" ""))
           (border (citre-peek--make-border))
           (peek-line (or (citre-get-property loc 'peek-line)
                          (citre-get-property
                           (citre-put-property
                            loc 'peek-line
                            (citre-peek--get-linum
                             (citre-get-property loc nil 'from-record)))
                           'peek-line)))
           (file-content (citre-peek--get-content
                          (citre-get-property loc 'ext-abspath 'from-record)
                          peek-line
                          citre-peek-file-content-height))
           (displayed-locs (citre--subseq
                            citre-peek--locations
                            citre-peek--displayed-locations-interval))
           (count-info (format "(%s/%s)\n"
                               (1+ citre-peek--location-index) loc-numbers))
           (displayed-index
            (citre--index-in-interval citre-peek--location-index
                                      citre-peek--displayed-locations-interval
                                      loc-numbers)))
      ;; Trim the location strings.
      (setq displayed-locs
            (mapcar #'citre--fit-line displayed-locs))
      ;; Add faces.
      (citre--add-face file-content
                       (list :background citre-peek--bg
                             :extend t))
      (dotimes (n (length displayed-locs))
        (let ((line (concat (nth n displayed-locs) "\n")))
          (if (eq n displayed-index)
              (setf (nth n displayed-locs)
                    (citre--add-face line
                                     (list :background citre-peek--bg-selected
                                           :extend t)))
            (setf (nth n displayed-locs)
                  (citre--add-face line
                                   (list :background citre-peek--bg-alt
                                         :extend t))))))
      (citre--add-face count-info
                       (list :background citre-peek--bg-alt
                             :extend t))
      ;; And peek it!
      (overlay-put citre-peek--ov 'after-string
                   (concat initial-newline border file-content
                           (string-join displayed-locs) count-info
                           border)))))

;;;; Commands

(defun citre-peek ()
  "Peek the definition of the symbol at point."
  (interactive)
  ;; Quit existing peek sessions.
  (when (overlayp citre-peek--ov)
    (citre-peek-abort))
  ;; Fetch informations to show.
  (setq citre-peek--locations (mapcar #'citre-make-location-str
                                      (citre-get-definitions)))
  (when (null citre-peek--locations)
    (user-error "Can't find definition"))
  (dolist (loc citre-peek--locations)
    (citre-put-property loc 'total-lines
                        (or (citre--file-total-lines
                             (citre-get-property
                              loc 'ext-abspath 'from-record))
                            ;; Display 1 line when the file doesn't exist.
                            1))
    (citre-put-property loc 'buffer-exist-p
                        (find-buffer-visiting
                         (citre-get-property
                          loc 'ext-abspath 'from-record))))
  ;; Setup environment for peeking.
  (citre-peek-mode)
  (setq citre-peek--ov (make-overlay (1+ (point-at-eol)) (1+ (point-at-eol))))
  (setq citre-peek--displayed-locations-interval
        (cons 0 (min citre-peek-locations-height
                     (length citre-peek--locations))))
  (setq citre-peek--location-index 0)
  (let* ((bg-mode (frame-parameter nil 'background-mode))
         (bg-unspecified-p (string= (face-background 'default)
                                    "unspecified-bg"))
         (bg (cond
              ((and bg-unspecified-p (eq bg-mode 'dark)) "#333333")
              ((and bg-unspecified-p (eq bg-mode 'light)) "#dddddd")
              (t (face-background 'default)))))
    (cond
     ((eq bg-mode 'dark)
      (setq citre-peek--bg (citre--color-blend "#ffffff" bg 0.03))
      (setq citre-peek--bg-alt (citre--color-blend "#ffffff" bg 0.2))
      (setq citre-peek--bg-selected (citre--color-blend "#ffffff" bg 0.4)))
     (t
      (setq citre-peek--bg (citre--color-blend "#000000" bg 0.02))
      (setq citre-peek--bg-alt (citre--color-blend "#000000" bg 0.12))
      (setq citre-peek--bg-selected (citre--color-blend "#000000" bg 0.06)))))
  (add-hook 'post-command-hook #'citre-peek--post-command-function nil 'local))

(defun citre-peek-next-line ()
  "Scroll to the next line in a peek window."
  (interactive)
  (citre-peek--line-forward 1))

(defun citre-peek-prev-line ()
  "Scroll to the previous line in a peek window."
  (interactive)
  (citre-peek--line-forward -1))

(defun citre-peek-next-location ()
  "Peek the next location of definition."
  (interactive)
  (unless (citre-get-property (nth citre-peek--location-index
                                   citre-peek--locations)
                              'buffer-exist-p))
  (citre-peek--location-index-forward 1))

(defun citre-peek-prev-location ()
  "Peek the previous location of definition."
  (interactive)
  (unless (citre-get-property (nth citre-peek--location-index
                                   citre-peek--locations)
                              'buffer-exist-p))
  (citre-peek--location-index-forward -1))

(defun citre-peek-abort ()
  "Abort peeking."
  (interactive)
  (delete-overlay citre-peek--ov)
  (mapc (lambda (pair)
          (kill-buffer (cdr pair)))
        citre-peek--temp-buffer-alist)
  (setq citre-peek--temp-buffer-alist nil)
  (setq citre-peek--ov nil)
  (setq citre-peek--locations nil)
  (setq citre-peek--displayed-locations-interval nil)
  (setq citre-peek--location-index nil)
  (setq citre-peek--bg nil)
  (setq citre-peek--bg-alt nil)
  (setq citre-peek--bg-selected nil)
  (citre-peek-mode -1)
  (remove-hook 'post-command-hook #'citre-peek--post-command-function 'local))

(defun citre-peek-jump ()
  "Jump to the definition that is currently peeked."
  (interactive)
  (citre-goto-tag (citre-get-property
                   (nth citre-peek--location-index
                        citre-peek--locations)
                   nil 'from-record)))

(provide 'citre-peek)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-peek.el ends here
