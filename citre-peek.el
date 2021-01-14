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

;; - History management
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

;;;;; Size of peek window

(defcustom citre-peek-file-content-height 12
  "Number of lines displaying file contents in the peek window."
  :type 'integer
  :group 'citre)

(defcustom citre-peek-locations-height 3
  "Number of locations displayed in the peek window."
  :type 'integer
  :group 'citre)

;;;;; Keybindings

(defcustom citre-peek-ace-keys '(?a ?s ?d ?f ?j ?k ?l ?\;)
  "Keys used for `citre-peek-through'."
  :type '(repeat :tag "Keys" character)
  :group 'citre)

(defcustom citre-peek-cancel-ace-keys '(?\C-g ?q)
  "Keys used for cancel an ace session."
  :type '(repeat :tag "Keys" character)
  :group 'citre)

(defcustom citre-peek-keymap
  (let ((map (make-sparse-keymap)))
    ;; Browse file
    (define-key map (kbd "M-n") 'citre-peek-next-line)
    (define-key map (kbd "M-p") 'citre-peek-prev-line)
    ;; Browse in the definition list
    (define-key map (kbd "M-N") 'citre-peek-next-definition)
    (define-key map (kbd "M-P") 'citre-peek-prev-definition)
    ;; Browse in the history
    (define-key map (kbd "<M-right>") 'citre-peek-chain-forward)
    (define-key map (kbd "<M-left>") 'citre-peek-chain-backward)
    (define-key map (kbd "<M-up>") 'citre-peek-prev-branch)
    (define-key map (kbd "<M-down>") 'citre-peek-prev-branch)
    ;; Modify history
    (define-key map (kbd "M-l p") 'citre-peek-through)
    (define-key map (kbd "M-l d") 'citre-peek-delete-branch)
    (define-key map (kbd "M-l D") 'citre-peek-delete-branches)
    ;; Rearrange definition list
    (define-key map (kbd "<M-S-up>") 'citre-peek-move-current-def-up)
    (define-key map (kbd "<M-S-down>") 'citre-peek-move-current-def-down)
    (define-key map (kbd "M-l f") 'citre-peek-make-current-def-first)
    ;; Jump
    (define-key map (kbd "M-l j") 'citre-peek-jump)
    ;; Abort
    (define-key map [remap keyboard-quit] 'citre-peek-abort)
    map)
  "Keymap used for `citre-peek' sessions."
  :type 'keymap
  :group 'citre)

;;;;; Faces

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

(defface citre-peek-ace-str-face
  '((((background light))
     :foreground "#dddddd" :background "#666666")
    (t
     :foreground "#222222" :background "#c0c0c0"))
  "Face used for ace strings."
  :group 'citre)

;;;;; Annotations

(defcustom citre-peek-ellipsis
  "…"
  "Ellipsis used when truncating long definition lines."
  :type 'string
  :group 'citre)

(defcustom citre-peek-peeked-through-definition-prefix
  (propertize "* " 'face 'warning)
  "The prefix for definitions where you peeked through."
  :type 'string
  :group 'citre)

(defcustom citre-peek-root-symbol-str
  "·"
  "The string used to represent root symbol in the chain."
  :type 'string
  :group 'citre)

(defcustom citre-peek-chain-separator
  (propertize " → " 'face 'font-lock-function-name-face)
  "The separator in the chain where it doesn't branch."
  :type 'string
  :group 'citre)

(defcustom citre-peek-branch-separator
  (propertize " < " 'face 'font-lock-function-name-face)
  "The separator in the chain where it branches."
  :type 'string
  :group 'citre)

(defface citre-peek-current-symbol-face
  '((t :inherit warning :bold t))
  "The face used for the current symbol in the chain."
  :group 'citre)

(defcustom citre-peek-current-symbol-prefix
  "["
  "The prefix for the current symbol in the chain."
  :type 'string
  :group 'citre)

(defcustom citre-peek-current-symbol-suffix
  "]"
  "The suffix for the current symbol in the chain."
  :type 'string
  :group 'citre)

(defface citre-peek-symbol-face
  '((t :inherit default))
  "The face used for non-current symbols in the chain."
  :group 'citre)

;;;; Helpers

;;;;; List functions

(defun citre--delete-nth (n list)
  "Delete Nth element in LIST and return it."
  (setf (nthcdr n list) (nthcdr (1+ n) list))
  list)

(defun citre--insert-nth (newelt n list)
  "Insert NEWELT into the Nth position in LIST and return it."
  (setf (nthcdr n list) (push newelt (nthcdr n list)))
  list)

;;;;; "Circular" sequences

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

;;;;; Recording positions of lines/symbols

(defun citre--make-tag-of-current-location (name)
  "Make a record of the current line, with the name field being NAME.
This is for generating the \"entry\" point of the symbol chain."
  (let* ((pat (string-trim
               (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position))))
         (tag (make-hash-table :test #'eq)))
    (setq pat (replace-regexp-in-string "\\\\\\|/\\|\\$$" "\\\\\\&" pat))
    (setq pat (concat "/" pat "/;\""))
    (puthash 'name name tag)
    (puthash 'ext-abspath (buffer-file-name) tag)
    (puthash 'pattern pat tag)
    (puthash 'line (number-to-string (line-number-at-pos)) tag)
    tag))

;;;;; Ace jump

(defun citre--after-comment-or-str-p ()
  "Non-nil if current position is after/in a comment or string."
  (unless (bobp)
    (let* ((pos (1- (point))))
      ;; `syntax-ppss' is not always reliable, so we only use it when font lock
      ;; mode is disabled.
      (if font-lock-mode
          (let ((pos-faces (get-text-property pos 'face)))
            (unless (listp pos-faces)
              (setq pos-faces (list pos-faces)))
            (cl-intersection '(font-lock-comment-face
                               font-lock-comment-delimiter-face
                               font-lock-doc-face
                               font-lock-string-face)
                             pos-faces))
        (save-excursion
          (or (nth 4 (syntax-ppss pos))
              (nth 3 (syntax-ppss pos))))))))

(defun citre--search-symbols (line)
  "Search for symbols from current position to LINEs after.
The search jumps over comments/strings.

The returned value is a list of cons pairs (START . END), the
start/end position of each symbol.  Point will not be moved."
  (let ((bound (save-excursion
                 (forward-line (1- line))
                 (line-end-position)))
        (symbol-list))
    (save-excursion
      (cl-loop
       while
       (forward-symbol 1)
       do
       (when (> (point) bound)
         (cl-return))
       (unless (citre--after-comment-or-str-p)
         (push (cons (save-excursion
                       (forward-symbol -1)
                       (point))
                     (point))
               symbol-list))))
    (nreverse symbol-list)))

(defun citre--ace-key-seqs (n)
  "Make ace key sequences for N symbols.
N can be the length of the list returned by
`citre--search-symbols'.  The keys used are
`citre-peek-ace-keys'."
  (unless (and (listp citre-peek-ace-keys)
               (null (cl-remove-if #'integerp citre-peek-ace-keys))
               (eq (cl-remove-duplicates citre-peek-ace-keys)
                   citre-peek-ace-keys))
    (user-error "Invalid `citre-peek-ace-keys'"))
  (let* ((key-num (length citre-peek-ace-keys))
         (key-seq-length (pcase n
                           (0 0)
                           (1 1)
                           ;; Though `log' is a float-point operation, this is
                           ;; accurate for sym-num in a huge range.
                           (_ (ceiling (log n key-num)))))
         (key-seq (make-list n nil))
         nth-ace-key)
    (dotimes (nkey key-seq-length)
      (setq nth-ace-key -1)
      (dotimes (nsym n)
        (when (eq (% nsym (expt key-num nkey)) 0)
          (setq nth-ace-key (% (1+ nth-ace-key) key-num)))
        (push (nth nth-ace-key citre-peek-ace-keys) (nth nsym key-seq))))
    key-seq))

(defun citre--pop-ace-key-seqs (seqs char)
  "Modify ace key sequences SEQS as CHAR is pressed.
This sets elements in SEQS which not begin with CHAR to nil, and
pop the element which begin with CHAR.  When the only non-nil
element in seqs is poped, this returns its index, as the element
is hit by user input.

The modified SEQS is returned.  When CHAR is not the car of any
element in SEQS, this does nothing, and returns the original
list."
  (if (not (memq char (mapcar #'car seqs)))
      seqs
    (let (last-poped-idx)
      (dotimes (n (length seqs))
        (if (eq (car (nth n seqs)) char)
            (progn
              (pop (nth n seqs))
              (setq last-poped-idx n))
          (setf (nth n seqs) nil)))
      (if (null (cl-remove-if #'null seqs))
          last-poped-idx
        seqs))))

(defun citre--attach-ace-str (str sym-bounds bound-offset ace-seqs)
  "Return a copy of STR with ace strings attached.
SYM-BOUNDS specifies the symbols in STR, as returned by
`citre--search-symbols'.  BOUND-OFFSET is the starting point of
STR in the buffer.  ACE-SEQS is the ace key sequences, as
returned by `citre--ace-key-seqs' or `citre--pop-ace-key-seqs'.
The beginnings of each symbol are replaced by ace strings with
`citre-ace-string-face' attached."
  (let* ((nsyms (length sym-bounds))
         (new-str (copy-sequence str)))
    (dotimes (n nsyms)
      (when (nth n ace-seqs)
        (let* ((beg (- (car (nth n sym-bounds)) bound-offset))
               (end (- (cdr (nth n sym-bounds)) bound-offset))
               (ace-seq (nth n ace-seqs))
               (ace-str-len (min (length ace-seq) (- end beg))))
          (dotimes (idx ace-str-len)
            (aset new-str (+ beg idx) (nth idx ace-seq)))
          (put-text-property beg (+ beg ace-str-len)
                             'face 'citre-peek-ace-str-face new-str))))
    new-str))

;;;;; Display

(defun citre--fit-line (str)
  "Fit STR in current window.
When STR is too long, it will be truncated, and \"...\" is added
at the end."
  ;; Depending on the wrapping behavior, in some terminals, a line with exact
  ;; (window-body-width) characters can be wrapped. So we minus it by one.
  (let ((limit (1- (window-body-width))))
    ;; TODO: When the line contains tabs, sometimes it's not trimmed properly,
    ;; even if we replace this `length' by `string-width'.
    (if (> (length str) limit)
        (concat (substring str 0 (- limit (length citre-peek-ellipsis)))
                citre-peek-ellipsis)
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

;;;;; Data structures

;; These are data structures that handles code reading history in citre-peek.
;; Let's have a brief description of the basic concepts first:

;; When you peek a symbol, you create a *def list* ("def" stands for
;; "definition") of its definition locations.  Each element in a def list is
;; called a *def entry*, which has 2 important slots: one for the tag of that
;; definition, and another is a list called *branches*, which we'll soon
;; explain.

;; In a peek session, you are always browsing a def entry.  When you peek
;; through a symbol, a new def list is created.  To keep a complete history of
;; your code reading session, this def list is pushed into the branches of the
;; current browsed def entry.

;; The history of a peek session is a tree like this:

;; - def list
;;   - def entry 1
;;     - def list A
;;     - def list B
;;   - def entry 2
;;     - def list C
;;   - def entry 3
;;   ...

(cl-defstruct (citre-peek--def-entry
               (:constructor nil)
               (:constructor citre-peek--def-entry-create
                             (tag))
               (:copier nil))
  "Definition entry in code reading history in citre-peek."
  (tag
   nil
   :documentation
   "The tag of this entry."
   :type "tag (a hash table)")
  (branches
   nil
   :documentation
   "A list of tag lists under this entry.
We don't keep a record of current branch because we make sure
it's always the first one, by scrolling this list."
   :type "list of citre-peek-def-list")
  (base-marker
   nil
   :documentation
   "The marker at the start of the line of the tag location.
We use a marker to deal with possible buffer update (e.g., peek
the definition of a symbol, while writing code above its
position.)"
   :type "marker")
  (line-offset
   0
   :documentation
   "The offset of current peeked position to base-marker."
   :type "integer"))

(cl-defstruct (citre-peek--def-list
               (:constructor nil)
               (:constructor
                citre-peek--def-list-create
                (tags
                 &optional symbol
                 &aux (entries (mapcar #'citre-peek--def-entry-create tags))))
               (:copier nil))
  "List of definitions used in citre-peek."
  (index
   0
   :documentation
   "The index of current browsed def entry in this list."
   :type "integer")
  (symbol
   nil
   :documentation
   "The symbol whose definitions are this def list.
Can be nil for the root def list of a session, since it only has
one entry to keep where we start, and that's not a definition of
any symbol."
   :type "nil or string")
  (entries
   nil
   :documentation
   "A list of def entries in this def list."
   :type "list of citre-peek--def-entry"))

;;;;; State variables

;; NOTE: Here's a list of when should each state variable be changed.  Keep
;; these in mind when you are developing citre-peek related commands.

;; - `citre-peek--session-root-list': Set this only for a new peek session.
;;
;; - `citre-peek--depth-in-root-list': Set this to 1 for a new peek session,
;;   and modify it when moving forward/backward in the chain (including peeking
;;   through).  Make sure it's >= 0 and <= maximum possible depth.
;;
;; - `citre-peek--displayed-defs-interval': Set this for a new peek session,
;;   and when browsing the def list, and when moving in the chain.
;;
;; - `citre-peek--temp-buffer-alist': Set this when doing ace jump (peeking
;;   through for now), and clean it up after a succeeded/canceled jump.
;;
;; - `citre-peek--symbol-bounds', `citre-peek--ace-seqs': Set these when
;;   peeking through, and clean it up after
;;
;; - `citre-peek--ov', `citre-peek--bg'(-alt, -selected): You shouldn't use
;;   them directly.  These are controlled by `citre-peek--mode', and it makes
;;   sure that the overlay is cleaned up correctly.  When other state variables
;;   are set up, enable `citre-peek-mode' sets up the UI, and disable
;;   `citre-peek--mode' hides the UI.

(defvar citre-peek--session-root-list nil
  "The root def list of current peek session.")

(defvar citre-peek--depth-in-root-list nil
  "The depth of currently browsed def list in the root list.")

(defvar citre-peek--displayed-defs-interval nil
  "The interval of displayed def entries in currently browsed def list.
This is a cons pair, its car is the index of the first displayed
entry, and cdr is one plus the index of the last one.")

(defvar citre-peek--temp-buffer-alist nil
  "Files and their temporary buffers that don't exist before peeking.
Its keys are file paths, values are buffers.  The buffers will be
killed after `citre-peek-abort'.")

(defvar citre-peek--symbol-bounds nil
  "Symbol bounds for ace jump.
Its car is the bound offset, i.e., the starting point of the
region to perform ace jump on.  Its cdr is a list of the symbol
bounds as returned by `citre--search-symbols'.")

(defvar citre-peek--ace-seqs nil
  "Ace key sequences for ace jump.")

(defvar citre-peek--ov nil
  "Overlay used to display the citre-peek UI.")

(defvar citre-peek--bg nil
  "Background color used for file contents when peeking.")

(defvar citre-peek--bg-alt nil
  "Background color for unselected definitions when peeking.")

(defvar citre-peek--bg-selected nil
  "Background color for selected definitions when peeking.")

(define-minor-mode citre-peek--mode
  "Mode for `citre-peek'.
This mode is created merely for handling the UI (display, keymap,
etc.), and is not for interactive use.  Users should use commands
like `citre-peek', `citre-peek-abort', `citre-peek-restore',
which take care of setting up other things."
  :keymap citre-peek-keymap
  (cond
   (citre-peek--mode
    (when citre-peek--ov (delete-overlay citre-peek--ov))
    (setq citre-peek--ov
          (make-overlay (1+ (point-at-eol)) (1+ (point-at-eol))))
    (overlay-put citre-peek--ov 'window (selected-window))
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
        (setq citre-peek--bg-selected
              (citre--color-blend "#000000" bg 0.06)))))
    (add-hook 'post-command-hook #'citre-peek--update-display nil 'local))
   (t
    (when citre-peek--ov (delete-overlay citre-peek--ov))
    (mapc (lambda (pair)
            (kill-buffer (cdr pair)))
          citre-peek--temp-buffer-alist)
    (setq citre-peek--temp-buffer-alist nil)
    (setq citre-peek--ov nil)
    (setq citre-peek--bg nil)
    (setq citre-peek--bg-alt nil)
    (setq citre-peek--bg-selected nil)
    ;; In case they are not cleaned up by `citre-peek-through'
    (setq citre-peek--ace-seqs nil)
    (setq citre-peek--symbol-bounds nil)
    ;; We don't clean up `citre-peek--session-root-list',
    ;; `citre-peek--depth-in-root-list' and
    ;; `citre-peek--displayed-defs-interval', so we can restore a session
    ;; later.
    (remove-hook 'post-command-hook #'citre-peek--update-display 'local))))

;;;;; Handle temp file buffer

;; Actually we can make Emacs believe our temp buffer is visiting FILENAME (by
;; setting `buffer-file-name' and `buffer-file-truename'), but then the buffer
;; is not hidden (Emacs hides buffers whose name begin with a space, but those
;; visiting a file are not hidden), and Emacs ask you to confirm when killing
;; it because its content are modified.  Rather than trying to workaround these
;; issues, it's better to create this function instead.

(defun citre-peek--find-file-buffer (path)
  "Return the buffer visiting file PATH.
PATH is a canonical path.  This is like `find-buffer-visiting',
but it also searches `citre-peek--temp-buffer-alist', so it can
handle temporary buffers created during peeking.

When the file is not opened, this creates a temporary buffer for
it, sets its project root to current project root (for
`citre-peek-through' to work) and major mode.  These buffers will
be killed afterwards by `citre-abort'.

When PATH doesn't exist, this returns nil."
  (if (not (file-exists-p path))
      nil
    (or (alist-get path citre-peek--temp-buffer-alist
                   nil nil #'equal)
        (find-buffer-visiting path)
        (let ((buf (generate-new-buffer (format " *citre-peek-%s*" path)))
              (current-project (citre-project-root)))
          (with-current-buffer buf
            (insert-file-contents path)
            ;; `set-auto-mode' checks `buffer-file-name' to set major mode.
            (let ((buffer-file-name path))
              (delay-mode-hooks
                (set-auto-mode)))
            ;; NOTE: For some weird reason, if you put this before the above
            ;; form, `citre-project-root' will be cleared.
            (setq citre-project-root current-project))
          (push (cons path buf) citre-peek--temp-buffer-alist)
          buf))))

;;;;; Methods for `citre-peek--def-entry'

(defun citre-peek--get-base-marker (entry)
  "Return the base marker of ENTRY.
ENTRY is an instance of `citre-peek--def-entry'.  The value of
the `base-marker' slot is returned, or if it doesn't exist,
calculate the mark using the tag, write it to the `base-marker'
slot, then return it.

Nil is returned when the file in the tag doesn't exist."
  (or (let ((marker (citre-peek--def-entry-base-marker entry)))
        ;; When the buffer of the file is killed, `marker' could point to
        ;; nowhere, so we check it by `marker-position'.
        (when (and marker (marker-position marker))
          marker))
      (let* ((tag (citre-peek--def-entry-tag entry))
             (buf (citre-peek--find-file-buffer
                   (citre-core-get-field 'ext-abspath tag)))
             (marker (when buf
                       (with-current-buffer buf
                         (save-excursion
                           (goto-char (citre-core-locate-tag tag))
                           (point-marker))))))
        (when marker
          (setf (citre-peek--def-entry-base-marker entry) marker)
          marker))))

(defun citre-peek--get-buf-and-pos (entry)
  "Get the buffer and position from ENTRY.
ENTRY is an instance of `citre-peek--def-entry'.  Its
`base-marker' and `line-offset' slots are used, or initialized if
they are unset.  `line-offset' is limited so it doesn't go beyond
the beinning/end of buffer.

A cons pair (BUF . POINT) is returned.  If the file in the tag
doesn't exist, these 2 fields are all nil."
  (let* ((path (citre-core-get-field 'ext-abspath
                                     (citre-peek--def-entry-tag entry)))
         (base-marker (citre-peek--get-base-marker entry))
         (line-offset (citre-peek--def-entry-line-offset entry))
         (buf (citre-peek--find-file-buffer path))
         offset-overflow
         point)
    (when buf
      (with-current-buffer buf
        (save-excursion
          (save-restriction
            ;; TODO: is this still necessary with `base-marker'?
            (widen)
            ;; If `buf' is non-nil, base-marker will be a valid marker, so we
            ;; don't check it.
            (goto-char base-marker)
            (setq offset-overflow
                  (forward-line line-offset))
            ;; If the buffer doesn't have trailing newline, `forward-line' in
            ;; the last line will take us to the end of line and count it as a
            ;; successful move.  No, we want to always be at the beginning of
            ;; line.
            (when (and (eolp) (not (bolp)))
              (beginning-of-line)
              (cl-incf offset-overflow))
            (when (not (eq offset-overflow 0))
              (setf (citre-peek--def-entry-line-offset entry)
                    (+ line-offset (- offset-overflow))))
            (setq point (point))))))
    (cons buf point)))

(defun citre-peek--get-content (entry)
  "Get file contents for peeking from ENTRY.
ENTRY is an instance of `citre-peek--def-entry'.
`citre-peek-file-content-height' specifies the number of lines to
get.

This also modifies the `line-offset' slot of ENTRY for letting it
not go beyond the start/end of the file."
  (pcase-let ((`(,buf . ,pos) (citre-peek--get-buf-and-pos entry)))
    (if buf
        (with-current-buffer buf
          (save-excursion
            (let ((beg nil)
                  (end nil))
              (save-excursion
                ;; If `buf' is non-nil, base-marker will be a valid marker, so
                ;; we don't check it.
                (goto-char pos)
                (setq beg (point))
                (forward-line (1- citre-peek-file-content-height))
                (setq end (point-at-eol))
                (font-lock-fontify-region beg end)
                (concat (buffer-substring beg end) "\n")))))
      (propertize "This file doesn't exist.\n" 'face 'error))))

(defun citre-peek--line-forward (n &optional entry)
  "Scroll the current browsed position in ENTRY N lines forward.
ENTRY is an instance of `citre-peek--def-entry', N can be
negative.  When ENTRY is nil, the currently browsed entry is
used.

Note: `citre-peek--get-buf-and-pos' could further modify the
`citre-line-offset' property of current location.  That function
is responsible for not letting it get beyond the start/end of the
file."
  (let* ((entry (or entry (citre-peek--current-def-entry)))
         (line-offset (citre-peek--def-entry-line-offset entry)))
    (setf (citre-peek--def-entry-line-offset entry)
          (+ line-offset n))))

;;;;; Methods for `citre-peek--def-list'

(defun citre-peek--def-list-length (deflist)
  "Return the number of entries in DEFLIST.
DEFLIST is an instance of `citre-peek--def-list'."
  (length (citre-peek--def-list-entries deflist)))

(defun citre-peek--current-entry-in-def-list (deflist)
  "Return the currently browsed def entry in DEFLIST.
DEFLIST is an instance of `citre-peek--def-list'."
  (nth (citre-peek--def-list-index deflist)
       (citre-peek--def-list-entries deflist)))

(defun citre-peek--push-branch-in-current-entry-in-def-list
    (deflist branch)
  "Push BRANCH into the `branches' slot of current browsed entry in DEFLIST.
DEFLIST and BRANCH are instances of `citre-peek--def-list'."
  (push branch
        (citre-peek--def-entry-branches
         (citre-peek--current-entry-in-def-list deflist))))

;;;;; Find currently browsed item in the tree

(defun citre-peek--current-def-list ()
  "Return the currently browsed def list."
  (let ((deflist citre-peek--session-root-list))
    (dotimes (_ citre-peek--depth-in-root-list)
      (let* ((entry (citre-peek--current-entry-in-def-list deflist))
             (branches (citre-peek--def-entry-branches entry)))
        (setq deflist (car branches))))
    deflist))

(defun citre-peek--current-def-entry ()
  "Return the currently browsed def entry."
  (citre-peek--current-entry-in-def-list
   (citre-peek--current-def-list)))

;;;;; Create def lists

(defun citre-peek--make-def-list-of-current-location (name)
  "Return a def list of current location.
The def list is an instance of `citre-peek--def-list', with its
`entries' slot only contains one def entry pointing to the
current line, and NAME being the name field of the tag.

The returned def list is the root def list of the peek session."
  (let* ((tag (citre--make-tag-of-current-location name))
         (deflist (citre-peek--def-list-create (list tag) nil)))
    deflist))

(defun citre-peek--get-def-list ()
  "Return the def list of symbol under point."
  (let* ((symbol (thing-at-point 'symbol 'no-properties))
         (definitions (citre-get-definitions))
         (deflist (citre-peek--def-list-create definitions symbol)))
    (when (null definitions)
      (user-error "Can't find definition"))
    deflist))

(defun citre-peek--create-branch (buf point)
  "Create new branch in the history.
It grabs the definitions of the symbol in BUF under POINT, and
push its def list into the branches of current def entry."
  (with-current-buffer buf
    (save-excursion
      (goto-char point)
      (let* ((branch (citre-peek--get-def-list)))
        (citre-peek--push-branch-in-current-entry-in-def-list
         (citre-peek--current-def-list) branch)
        (cl-incf citre-peek--depth-in-root-list)
        (citre-peek--setup-displayed-defs-interval branch)))))

;;;;; Manage session state

(defun citre-peek--setup-displayed-defs-interval (&optional deflist)
  "Set `citre-peek--displayed-defs-interval' based on DEFLIST.
DEFLIST is an instance of `citre-peek--def-list'.  The interval
is set so that it doesn't exceeds `citre-peek-locations-height',
and also fits the number of entries in DEFLIST.

When DEFLIST is nil, the currently browsed deflist is used."
  (let ((deflist (or deflist (citre-peek--current-def-list))))
    (setq citre-peek--displayed-defs-interval
          (cons 0 (min citre-peek-locations-height
                       (citre-peek--def-list-length deflist))))))

(defun citre-peek--setup-session (buf point)
  "Set up state variables for peek session.
It grabs the definitions of the symbol in BUF under POINT, and
set variables according to it."
  (with-current-buffer buf
    (save-excursion
      (goto-char point)
      ;; TODO: could we make the get definitions API also return the symbol?
      (let* ((symbol (thing-at-point 'symbol 'no-properties))
             (root-list (citre-peek--make-def-list-of-current-location
                         symbol))
             (branch (citre-peek--get-def-list)))
        (citre-peek--push-branch-in-current-entry-in-def-list
         root-list branch)
        (setq citre-peek--session-root-list root-list)
        (setq citre-peek--depth-in-root-list 1)
        (citre-peek--setup-displayed-defs-interval branch)))))

(defun citre-peek--def-index-forward (n)
  "In a peek window, move N steps forward in the definition list.
N can be negative."
  (let* ((deflist (citre-peek--current-def-list))
         (start (car citre-peek--displayed-defs-interval))
         (end (cdr citre-peek--displayed-defs-interval))
         (idx (citre-peek--def-list-index deflist))
         (len (citre-peek--def-list-length deflist)))
    (setq idx (mod (+ n idx) len))
    (setf (citre-peek--def-list-index deflist) idx)
    (unless (citre--index-in-interval idx
                                      citre-peek--displayed-defs-interval
                                      len)
      (let ((offset (if (> n 0)
                        (mod (- idx (1- end)) len)
                      (- (mod (- start idx) len)))))
        (setcar citre-peek--displayed-defs-interval
                (mod (+ offset start) len))
        (setcdr citre-peek--displayed-defs-interval
                (mod (+ offset end) len))))))

;;;;; Display

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

(defun citre-peek--file-content (deflist)
  "Return a string for displaying file content.
DEFLIST is the currently browsed def list."
  (let* ((entry (citre-peek--current-entry-in-def-list deflist))
         (file-content
          (citre-peek--get-content entry)))
    (citre--add-face file-content
                     (list :background citre-peek--bg
                           :extend t))
    (when (and citre-peek--symbol-bounds citre-peek--ace-seqs)
      (setq file-content
            (citre--attach-ace-str file-content
                                   (cdr citre-peek--symbol-bounds)
                                   (car citre-peek--symbol-bounds)
                                   citre-peek--ace-seqs)))
    file-content))

(defun citre-peek--displayed-defs-str (deflist)
  "Return a string for displaying definitions.
DEFLIST is the currently browsed def list."
  (let* ((idx (citre-peek--def-list-index deflist))
         (defs (citre-peek--def-list-entries deflist))
         (displayed-defs
          (citre--subseq defs
                         citre-peek--displayed-defs-interval))
         (displayed-tags
          (mapcar #'citre-peek--def-entry-tag displayed-defs))
         (displayed-defs-strlist
          (mapcar #'citre-make-location-str displayed-tags))
         (displayed-idx
          (citre--index-in-interval idx
                                    citre-peek--displayed-defs-interval
                                    (length defs))))
    (dotimes (n (length displayed-defs))
      (let ((line (citre-make-location-str (nth n displayed-tags))))
        (when (citre-peek--def-entry-branches (nth n displayed-defs))
          (setq line (concat citre-peek-peeked-through-definition-prefix
                             line)))
        (setq line (concat (citre--fit-line line) "\n"))
        (if (eq n displayed-idx)
            (setf (nth n displayed-defs-strlist)
                  (citre--add-face line
                                   (list :background citre-peek--bg-selected
                                         :extend t)))
          (setf (nth n displayed-defs-strlist)
                (citre--add-face line
                                 (list :background citre-peek--bg-alt
                                       :extend t))))))
    (string-join displayed-defs-strlist)))

(defun citre-peek--session-info (deflist)
  "Return a string for displaying session info.
DEFLIST is the currently browsed def list.  Session info means
the index of currently browsed definition, the total number of
definitions, and the current chain in the code reading history."
  (let* ((idx (citre-peek--def-list-index deflist))
         (len (citre-peek--def-list-length deflist))
         ;; We need to traverse the tree from the beginning to create the
         ;; chain.
         (deflist citre-peek--session-root-list)
         (depth 0)
         chain
         session-info)
    (while deflist
      (let* ((entry (citre-peek--current-entry-in-def-list deflist))
             (symbol (citre-peek--def-list-symbol deflist))
             (branches (citre-peek--def-entry-branches entry)))
        (unless symbol
          (setq symbol citre-peek-root-symbol-str))
        (if (eq depth citre-peek--depth-in-root-list)
            (setq symbol
                  (concat
                   citre-peek-current-symbol-prefix
                   (propertize symbol 'face 'citre-peek-current-symbol-face)
                   citre-peek-current-symbol-suffix))
          (setq symbol (propertize symbol 'face 'citre-peek-symbol-face)))
        (push symbol chain)
        (pcase (length branches)
          (0 nil)
          (1 (push citre-peek-chain-separator chain))
          (_ (push citre-peek-branch-separator chain)))
        (setq deflist (car branches)))
      (cl-incf depth))
    (setq session-info
          (concat (format "(%s/%s) " (1+ idx) len)
                  (string-join (nreverse chain))
                  "\n"))
    (citre--add-face session-info
                     (list :background citre-peek--bg-alt
                           :extend t))
    session-info))

(defun citre-peek--update-display ()
  "Deal with the update of contents in peek windows."
  (unless (minibufferp)
    (let ((overlay-pos (min (point-max) (1+ (point-at-eol)))))
      (move-overlay citre-peek--ov overlay-pos overlay-pos))
    (let* ((deflist (citre-peek--current-def-list))
           (initial-newline (if (eq (line-end-position) (point-max))
                                "\n" ""))
           (border (citre-peek--make-border)))
      (overlay-put citre-peek--ov 'after-string
                   (concat initial-newline border
                           (citre-peek--file-content deflist)
                           (citre-peek--displayed-defs-str deflist)
                           (citre-peek--session-info deflist)
                           border)))))

;;;; Commands

;;;;; Create/end/restore peek sessions

(defun citre-peek (&optional buf point)
  "Peek the definition of the symbol at point.
If BUF and POINT is given, peek the definition of the symbol in
BUF under POINT."
  (interactive)
  ;; Quit existing peek sessions.
  (when citre-peek--mode
    (citre-peek-abort))
  (let ((buf (or (when (and buf point) buf)
                 (current-buffer)))
        (point (or (when (and buf point) point)
                   (point))))
    ;; Setup location related variables.
    (citre-peek--setup-session buf point))
  ;; Setup environment for peeking.
  (citre-peek--mode))

(defun citre-peek-abort ()
  "Abort peeking."
  (interactive)
  (citre-peek--mode -1))

(defun citre-peek-restore ()
  "Restore recent peek session."
  (interactive)
  (unless citre-peek--mode
    (citre-peek--mode)))

;;;;; Browse in file

;; TODO: Throw error when not in a peek session.
(defun citre-peek-next-line ()
  "Scroll to the next line in a peek window."
  (interactive)
  (citre-peek--line-forward 1))

(defun citre-peek-prev-line ()
  "Scroll to the previous line in a peek window."
  (interactive)
  (citre-peek--line-forward -1))

;;;;; Browse in def list

(defun citre-peek-next-definition ()
  "Peek the next definition in list."
  (interactive)
  (citre-peek--def-index-forward 1))

(defun citre-peek-prev-definition ()
  "Peek the previous definition in list."
  (interactive)
  (citre-peek--def-index-forward -1))

;;;;; Browse in the tree history

(defun citre-peek-chain-forward ()
  "Move forward in the currently browsed chain.
This adds 1 to the currently browsed depth.  It's ensured that
the depth is not greater than the maximum depth."
  (interactive)
  (let ((max-depth-p (null (citre-peek--def-entry-branches
                            (citre-peek--current-def-entry)))))
    (unless max-depth-p
      (cl-incf citre-peek--depth-in-root-list)
      (citre-peek--setup-displayed-defs-interval))))

(defun citre-peek-chain-backward ()
  "Move backward in the currently browsed chain.
This subtracts 1 from the currently browsed depth.  It's ensured
that the depth is not less than 0."
  (interactive)
  (unless (eq citre-peek--depth-in-root-list 0)
    (cl-incf citre-peek--depth-in-root-list -1)
    (citre-peek--setup-displayed-defs-interval)))

;; NOTE: The direction of branch switching commands are decided so that when
;; the user created a new branch (by peeking through), and call `prev-branch',
;; they should see the branch that's previously browsed.

(defun citre-peek-next-branch ()
  "Switch to the next branch under current symbol."
  (interactive)
  (let* ((entry (citre-peek--current-def-entry))
         (branches (citre-peek--def-entry-branches entry)))
    (when branches
      (setf (citre-peek--def-entry-branches entry)
            (nconc (last branches) (butlast branches))))))

(defun citre-peek-prev-branch ()
  "Switch to the previous branch under current symbol."
  (interactive)
  (let* ((entry (citre-peek--current-def-entry))
         (branches (citre-peek--def-entry-branches entry)))
    (when branches
      (setf (citre-peek--def-entry-branches entry)
            (nconc (cdr branches) (list (car branches)))))))

;;;;; Edit definition list

(defun citre-peek-make-current-def-first ()
  "Put the current def entry in the first position."
  (interactive)
  (let* ((deflist (citre-peek--current-def-list))
         (idx (citre-peek--def-list-index deflist))
         (entries (citre-peek--def-list-entries deflist))
         (entry (nth idx entries)))
    (setf (citre-peek--def-list-entries deflist)
          (nconc (list entry) (citre--delete-nth idx entries)))
    (citre-peek--def-index-forward (- idx))))

(defun citre-peek-move-current-def-up ()
  "Move the current def entry up."
  (interactive)
  (let* ((deflist (citre-peek--current-def-list))
         (idx (citre-peek--def-list-index deflist))
         (entries (citre-peek--def-list-entries deflist))
         (entry (nth idx entries)))
    (when (> idx 0)
      (setf (citre-peek--def-list-entries deflist)
            (citre--insert-nth entry (1- idx) (citre--delete-nth idx entries)))
      (citre-peek--def-index-forward -1))))

(defun citre-peek-move-current-def-down ()
  "Move the current def entry down."
  (interactive)
  (let* ((deflist (citre-peek--current-def-list))
         (idx (citre-peek--def-list-index deflist))
         (entries (citre-peek--def-list-entries deflist))
         (entry (nth idx entries)))
    (when (< idx (1- (length entries)))
      (setf (citre-peek--def-list-entries deflist)
            (citre--insert-nth entry (1+ idx) (citre--delete-nth idx entries)))
      (citre-peek--def-index-forward 1))))

;;;;; Edit history

(defun citre-peek-delete-branch ()
  "Delete the first branch in currently browsed def entry."
  (interactive)
  (let* ((entry (citre-peek--current-def-entry))
         (branches (citre-peek--def-entry-branches entry)))
    (when (and branches
               (y-or-n-p
                "Deleting the current branch under this symbol.  Continue? "))
      (pop (citre-peek--def-entry-branches entry)))))

(defun citre-peek-delete-branches ()
  "Delete all branchs in currently browsed def entry."
  (interactive)
  (let* ((entry (citre-peek--current-def-entry))
         (branches (citre-peek--def-entry-branches entry)))
    (when (and branches
               (y-or-n-p
                "Deleting all branches under this symbol.  Continue? "))
      (setf (citre-peek--def-entry-branches entry) nil))))

;;;;; Others

(defun citre-peek-through ()
  "Peek through a symbol in current peek window."
  (interactive)
  (pcase-let ((`(,buf . ,pos) (citre-peek--get-buf-and-pos
                               (citre-peek--current-def-entry)))
              (key nil))
    (unless buf
      (user-error "The file doesn't exist"))
    (setq citre-peek--symbol-bounds
          (with-current-buffer buf
            (save-excursion
              (goto-char pos)
              (cons (point)
                    (citre--search-symbols citre-peek-file-content-height)))))
    (setq citre-peek--ace-seqs (citre--ace-key-seqs
                                (length (cdr citre-peek--symbol-bounds))))
    (citre-peek--update-display)
    (cl-block nil
      (while (setq key (read-key "Ace char:"))
        (when (memq key citre-peek-cancel-ace-keys)
          (setq citre-peek--symbol-bounds nil)
          (setq citre-peek--ace-seqs nil)
          (citre-peek--update-display)
          (cl-return))
        (pcase (citre--pop-ace-key-seqs citre-peek--ace-seqs key)
          ((and (pred integerp) i)
           (citre-peek-make-current-def-first)
           (let ((pos (car (nth i (cdr citre-peek--symbol-bounds)))))
             (citre-peek--create-branch buf pos))
           (setq citre-peek--symbol-bounds nil)
           (setq citre-peek--ace-seqs nil)
           (cl-return))
          (_ (citre-peek--update-display)))))))

(defun citre-peek-jump ()
  "Jump to the definition that is currently peeked."
  (interactive)
  (citre-peek-abort)
  (citre-goto-tag
   (citre-peek--def-entry-tag
    (citre-peek--current-def-entry)))
  (citre-peek-restore))

(provide 'citre-peek)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-peek.el ends here
