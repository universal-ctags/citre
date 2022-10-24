;;; citre-ui-peek.el --- Go down the rabbit hole without leaving current buffer -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 23 Nov 2020
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

;; This is the UI library for `citre-peek' UI.  `citre-peek' is a powerful code
;; reading tool.  Whether you just want to quickly peek the implementation of a
;; function, or need to understand a long and branched call chain to solve a
;; complex problem, citre-peek is the best tool for you.

;; Read the following docs to know how to use citre-peek:
;;
;; - README.md
;; - docs/user-manual/citre-peek.md

;; If you haven't received these docs, please visit
;; https://github.com/universal-ctags/citre.

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;;; Libraries

(require 'citre-common-tag)
(require 'color)
(require 'fringe)

;; We use this function for integration with the package "clue".  We don't
;; require it as it's not necessary for citre-peek to work.
(declare-function clue-copy-location "clue" (file line &optional project))
(declare-function clue-paste-location "clue" (file line &optional project))

;;;; User options

;;;;; Behavior

(defcustom citre-peek-auto-restore-after-jump t
  "Non-nil means restore session after `citre-peek-jump'."
  :type 'boolean
  :group 'citre)

(defcustom citre-peek-backward-in-chain-after-jump nil
  "Non-nil means move backward in the chain after `citre-peek-jump'.
This means after you jump to a tag, the peek window will show
where it's used/called/referenced.

This only works when `citre-peek-auto-restore-after-jump' is
non-nil."
  :type 'boolean
  :group 'citre)

;;;;; Size of peek window

(defcustom citre-peek-file-content-height 12
  "Number of lines displaying file contents in the peek window."
  :type 'integer
  :group 'citre)

(defcustom citre-peek-tag-list-height 3
  "Number of tags displayed in the peek window."
  :type 'integer
  :group 'citre)

(defalias 'citre-peek-definitions-height 'citre-peek-tag-list-height)
(make-obsolete 'citre-peek-definitions-height 'citre-peek-tag-list-height
               "0.3")

;;;;; Keybindings

(defcustom citre-peek-ace-keys '(?a ?s ?d ?f ?j ?k ?l ?\;)
  "Ace keys used for `citre-peek-through' and `citre-ace-peek'."
  :type '(repeat :tag "Keys" character)
  :group 'citre)

(defcustom citre-peek-ace-cancel-keys '(?\C-g ?q)
  "Keys used for cancel an ace session."
  :type '(repeat :tag "Keys" character)
  :group 'citre)

(defcustom citre-peek-ace-pick-symbol-at-point-keys '(?\C-m)
  "Keys used for pick symbol at point in `citre-ace-peek'."
  :type '(repeat :tag "Keys" character)
  :group 'citre)

(defcustom citre-peek-keymap
  (let ((map (make-sparse-keymap)))
    ;; Browse file
    (define-key map (kbd "M-n") 'citre-peek-next-line)
    (define-key map (kbd "M-p") 'citre-peek-prev-line)
    ;; Browse in the tag list
    (define-key map (kbd "M-N") 'citre-peek-next-tag)
    (define-key map (kbd "M-P") 'citre-peek-prev-tag)
    ;; Browse in the history
    (define-key map (kbd "<right>") 'citre-peek-chain-forward)
    (define-key map (kbd "<left>") 'citre-peek-chain-backward)
    (define-key map (kbd "<up>") 'citre-peek-prev-branch)
    (define-key map (kbd "<down>") 'citre-peek-next-branch)
    ;; Modify history
    (define-key map (kbd "M-l p") 'citre-peek-through)
    (define-key map (kbd "M-l r") 'citre-peek-through-reference)
    (define-key map (kbd "M-l d") 'citre-peek-delete-branch)
    (define-key map (kbd "M-l D") 'citre-peek-delete-branches)
    ;; Rearrange tag list
    (define-key map (kbd "S-<up>") 'citre-peek-move-current-tag-up)
    (define-key map (kbd "S-<down>") 'citre-peek-move-current-tag-down)
    (define-key map (kbd "M-l f") 'citre-peek-make-current-tag-first)
    ;; Jump
    (define-key map (kbd "M-l j") 'citre-peek-jump)
    ;; Abort
    (define-key map [remap keyboard-quit] 'citre-peek-abort)
    map)
  "Keymap used for `citre-peek' sessions."
  :type 'keymap
  :group 'citre)

;;;;; Appearance

(defcustom citre-peek-fill-fringe t
  "Non-nil means fill the fringes with a vertical border of the peek window."
  :type 'boolean
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

In terminal Emacs, a dashed pattern is used as the border, and
only the background property of this face is used, as the color
of the dashes."
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

(defcustom citre-peek-peeked-through-tag-prefix
  (propertize "* " 'face 'warning)
  "The prefix for tag nodes in which you peeked through."
  :type 'string
  :group 'citre)

(defalias 'citre-peek-peeked-through-definition-prefix
  'citre-peek-peeked-through-tag-prefix)
(make-obsolete 'citre-peek-peeked-through-definition-prefix
               'citre-peek-peeked-through-tag-prefix
               "0.3")

(defcustom citre-peek-root-symbol-str
  "·"
  "The string used to represent root symbol in the chain."
  :type 'string
  :group 'citre)

(defcustom citre-peek-chain-separator
  ;; NOTE: In terminal Emacs, sometimes the chain is not updated properly when
  ;; there's unicode char in it.  I think it's probably not a problem of Emacs
  ;; but the display update of the terminal itself.  Put a `redraw-frame' call
  ;; in `citre-peek--update-display' solves the problem, but it creates a
  ;; visual flick.  I believe unicode chars in the middle of a line cause the
  ;; problem, so this may also happen to the file content and tag list.
  (propertize (if (display-graphic-p) " → " " - ")
              'face 'font-lock-function-name-face)
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

;;;;; Lists

(defun citre--delete-nth (n list)
  "Delete Nth element in LIST and return the modified list."
  (setf (nthcdr n list) (nthcdr (1+ n) list))
  list)

(defun citre--insert-nth (newelt n list)
  "Insert NEWELT to the Nth position in LIST.
The modified list is returned."
  (setf (nthcdr n list) (push newelt (nthcdr n list)))
  list)

;;;;; Ace jump

(defun citre--after-comment-or-str-p ()
  "Non-nil if current position is after/in a comment or string."
  (unless (bobp)
    (let* ((pos (1- (point))))
      ;; `syntax-ppss' is not always reliable, so we only use it when font lock
      ;; mode is disabled.
      (if font-lock-mode
          (when-let ((pos-faces (get-text-property pos 'face)))
            (unless (listp pos-faces)
              (setq pos-faces (list pos-faces)))
            ;; Use `cl-subsetp' rather than `cl-intersection', so that
            ;; highlighted symbols in docstrings/comments can be captured. For
            ;; example, symbols in Elisp comments/docstrings has both
            ;; `font-lock-constant-face' and `font-lock-comment/doc-face'.
            (cl-subsetp pos-faces
                        '(font-lock-comment-face
                          font-lock-comment-delimiter-face
                          font-lock-doc-face
                          font-lock-string-face)))
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

(defvar citre--ace-ov nil
  "Overlays for ace strings.")

(defun citre--clean-ace-ov ()
  "Cleanup overlays for ace jump."
  (dolist (ov citre--ace-ov)
    (delete-overlay ov))
  (setq citre--ace-ov nil))

(defun citre--attach-ace-overlay (sym-bounds ace-seqs)
  "Setup overlay in current buffer for ace strings.
SYM-BOUNDS specifies the annotated symbols, as returned by
`citre--search-symbols'.  ACE-SEQS is the ace key sequences, as
returned by `citre--ace-key-seqs' or `citre--pop-ace-key-seqs'."
  (let* ((nsyms (length sym-bounds))
         ov)
    (citre--clean-ace-ov)
    (dotimes (n nsyms)
      (when (nth n ace-seqs)
        (let* ((beg (car (nth n sym-bounds)))
               (end (cdr (nth n sym-bounds)))
               (ace-seq (nth n ace-seqs))
               (end (min end (+ beg (length ace-seq)))))
          (setq ov (make-overlay beg end))
          (overlay-put ov 'window (selected-window))
          (overlay-put ov 'face 'citre-peek-ace-str-face)
          (overlay-put ov 'display (substring
                                    (mapconcat #'char-to-string ace-seq "")
                                    0 (- end beg)))
          (push ov citre--ace-ov))))))

;;;;; Display

(defun citre--fit-line (str)
  "Fit STR in current window.
When STR is too long, it will be truncated, and
`citre-peek-ellipsis' is added at the end."
  ;; Depending on the wrapping behavior, in some terminals, a line with exact
  ;; (window-body-width) characters can be wrapped. So we minus it by one.
  (let ((limit (1- (window-body-width))))
    (if (> (length str) limit)
        (concat (truncate-string-to-width
                 str
                 (- limit (string-width citre-peek-ellipsis)))
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

;; When you peek a symbol, you create a *tag list* of tags related to it (they
;; may be its definitions/references/etc.).  Each element in a tag list is
;; called a *tag node*, which has 2 important slots: one is the tag itself, and
;; another is a list called *branches*, which we'll soon explain.

;; In a *peek session*, you are always browsing a tag node.  When you peek
;; through a symbol, a new tag list is created.  To keep a complete history of
;; your code reading session, this tag list is pushed into the branches of the
;; current browsed tag node.

;; The history of a peek session is a tree like this:

;; - tag list
;;   - tag node 1
;;     - tag list A
;;     - tag list B
;;   - tag node 2
;;     - tag list C
;;   - tag node 3
;;   ...

(cl-defstruct (citre-peek--session
               (:constructor nil)
               (:constructor
                citre-peek--session-create
                (root-list
                 depth
                 &optional name))
               (:copier nil))
  "Citre-peek session."
  (root-list
   nil
   :documentation
   "The root tag list in this peek session."
   :type "citre-peek--tag-list")
  (depth
   nil
   :documentation
   "The depth of currently browsed tag list in this session."
   :type "integer")
  (name
   nil
   :documentation
   "The name of this session."
   :type "nil or string"))

(cl-defstruct (citre-peek--tag-node
               (:constructor nil)
               (:constructor citre-peek--tag-node-create
                             (tag))
               (:copier nil))
  "Tag node in code reading history in citre-peek."
  (tag
   nil
   :documentation
   "The tag of this node."
   :type "tag (a hash table)")
  (branches
   nil
   :documentation
   "A list of tag lists under this node.
We don't keep a record of current branch because we make sure
it's always the first one, by scrolling this list."
   :type "list of citre-peek-tag-list")
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

(cl-defstruct (citre-peek--tag-list
               (:constructor nil)
               (:constructor
                citre-peek--tag-list-create
                (tags
                 &aux (nodes (mapcar #'citre-peek--tag-node-create tags))))
               (:copier nil))
  "List of tags used in citre-peek."
  (index
   0
   :documentation
   "The index of current browsed tag node in this list."
   :type "integer")
  (nodes
   nil
   :documentation
   "A list of tag nodes in this tag list."
   :type "list of citre-peek--tag-node"))

;;;;; State variables

;; NOTE: Here's a list of when should each state variable be changed.  Keep
;; these in mind when you are developing citre-peek related commands.
;;
;; - `citre-peek--displayed-tags-interval': Set this for a new peek session,
;;   when browsing the tag list, and when moving in the chain.
;;
;; - `citre-peek--symbol-bounds', `citre-peek--ace-seqs': Set these for ace
;;   jump, and clean it up afterwards.
;;
;; - `citre-peek--content-update': Set this to t when the content in the peek
;;   window is updated.  This variable is for preventing recalculate the
;;   content after every command.
;;
;; We know our currently browsed tag list in a session by the `depth' slot in
;; the `citre-peek--session'.  It can be modified by
;; `citre-peek--session-depth-add':
;;
;; - Set it to 0 or 1 for a new peek session (see `citre-peek--make-session').
;;
;; - Modify it when moving forward/backward in the chain (including peeking
;;   through).  Make sure it's >= 0 and <= maximum possible depth.
;;
;; These variables are not going to change much, and newly added commands may
;; not need to care about them:
;;
;; - `citre-peek--session-root-list': Set this only for a new peek session.
;;
;; - `citre-peek--temp-buffer-alist': Add pairs to this when peeking a new
;;   non-visiting file, and clean it up when abort current peek session (i.e.,
;;   when disabling `citre-mode').
;;
;; - `citre-peek--ov', `citre-peek--bg'(-alt, -selected): You shouldn't use
;;   them directly.  These are controlled by `citre-peek--mode', and it makes
;;   sure that the overlay is cleaned up correctly.  When other state variables
;;   are set up, enable `citre-peek--mode' sets up the UI, and disable
;;   `citre-peek--mode' hides the UI.
;;
;; - `citre-peek--buffer-file-name': It's automatically set by
;;   `citre-peek--find-file-buffer'.

(defvar citre-peek--current-session nil
  "Current peek session.")

(defvar citre-peek--saved-sessions nil
  "Saved peek sessions.")

(defvar citre-peek--displayed-tags-interval nil
  "The interval of displayed tags in currently browsed tag list.
This is a cons pair, its car is the index of the first displayed
tag, and cdr the index of the last one.")

(defvar citre-peek--temp-buffer-alist nil
  "Files and their temporary buffers that don't exist before peeking.
Its keys are file paths, values are buffers.  The buffers will be
killed after disabling `citre-peek--mode'.")

(defvar citre-peek--symbol-bounds nil
  "Symbol bounds for ace jump.
Its car is the bound offset, i.e., the starting point of the
region to perform ace jump on.  Its cdr is a list of the symbol
bounds as returned by `citre--search-symbols'.")

(defvar citre-peek--ace-seqs nil
  "Ace key sequences for ace jump.")

(defvar citre-peek--ov nil
  "Overlay used to display the citre-peek UI.")

(defvar citre-peek--content-update nil
  "Non-nil means the content in the peek window is updated.")

(defvar citre-peek--bg nil
  "Background color used for file contents when peeking.")

(defvar citre-peek--bg-alt nil
  "Background color for unselected tags when peeking.")

(defvar citre-peek--bg-selected nil
  "Background color for selected tags when peeking.")

(defvar-local citre-peek--buffer-file-name nil
  "File name in non-file buffers.
`citre-peek' needs to open files in non-file temporary buffers,
where the function `buffer-file-name' doesn't work.  `citre-peek'
uses hacks to make it work when peeking through symbols inside
such buffers, which may need the file name.  For this to work,
`citre-peek--buffer-file-name' must be set in these buffers.")

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
          (let ((ov-pos (line-end-position)))
            (make-overlay ov-pos ov-pos)))
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
    (setq citre-peek--content-update t)
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
    ;; `citre-peek--displayed-tags-interval', so we can restore a session
    ;; later.
    (remove-hook 'post-command-hook #'citre-peek--update-display 'local))))

(defun citre-peek--error-if-not-peeking ()
  "Throw an error if not in a peek session."
  (unless citre-peek--mode
    (user-error "Not in a peek session")))

;;;;; Handle temp file buffer

;; Actually we can make Emacs believe our temp buffer is visiting FILENAME (by
;; setting `buffer-file-name' and `buffer-file-truename'), but then the buffer
;; is not hidden (Emacs hides buffers whose name begin with a space, but those
;; visiting a file are not hidden), and Emacs ask you to confirm when killing
;; it because its content are modified.  Rather than trying to workaround these
;; issues, it's better to create this function instead.

(defmacro citre-peek--hack-buffer-file-name (&rest body)
  "Override function `buffer-file-name' in BODY.
This makes it work in non-file buffers where
`citre-peek--buffer-file-name' is set."
  (declare (indent 0))
  `(cl-letf* ((buffer-file-name-orig (symbol-function 'buffer-file-name))
              ((symbol-function 'buffer-file-name)
               (lambda (&optional buffer)
                 (or (with-current-buffer (or buffer (current-buffer))
                       citre-peek--buffer-file-name)
                     (funcall buffer-file-name-orig buffer)))))
     ,@body))

(defun citre-peek--find-file-buffer (path)
  "Return the buffer visiting file PATH.
PATH is an absolute path.  This is like `find-buffer-visiting',
but it also searches `citre-peek--temp-buffer-alist', so it can
handle temporary buffers created during peeking.

When the file is not opened, this creates a temporary buffer for
it.  These buffers will be killed afterwards by `citre-abort'.

When PATH doesn't exist, this returns nil."
  (when (citre-non-dir-file-exists-p path)
    (setq path (file-truename path))
    (or (alist-get path citre-peek--temp-buffer-alist
                   nil nil #'equal)
        (find-buffer-visiting path)
        (let ((buf (generate-new-buffer (format " *citre-peek-%s*" path))))
          (with-current-buffer buf
            (insert-file-contents path)
            ;; `set-auto-mode' checks `buffer-file-name' to set major mode.
            (let ((buffer-file-name path))
              (delay-mode-hooks
                (set-auto-mode)))
            (setq citre-peek--buffer-file-name path)
            ;; In case language-specific `:get-symbol' function uses
            ;; `default-directory'.
            (setq default-directory (file-name-directory path))
            (hack-dir-local-variables-non-file-buffer))
          (push (cons path buf) citre-peek--temp-buffer-alist)
          buf))))

;;;;; Methods for `citre-peek--tag-node'

(defun citre-peek--get-base-marker (node)
  "Return the base marker of NODE.
NODE is an instance of `citre-peek--tag-node'.  The value of
the `base-marker' slot is returned, or if it doesn't exist,
calculate the mark using the tag, write it to the `base-marker'
slot, then return it.

Nil is returned when the file in the tag doesn't exist."
  (or (let ((marker (citre-peek--tag-node-base-marker node)))
        ;; When the buffer of the file is killed, `marker' could point to
        ;; nowhere, so we check it by `marker-position'.
        (when (and marker (marker-position marker))
          marker))
      (let* ((tag (citre-peek--tag-node-tag node))
             (buf (citre-peek--find-file-buffer
                   (citre-get-tag-field 'ext-abspath tag)))
             (marker (when buf
                       (with-current-buffer buf
                         (save-excursion
                           (goto-char (citre-locate-tag tag))
                           (point-marker))))))
        (when marker
          (setf (citre-peek--tag-node-base-marker node) marker)
          marker))))

(defun citre-peek--get-buf-and-pos (node)
  "Get the buffer and position from NODE.
NODE is an instance of `citre-peek--tag-node'.  Its
`base-marker' and `line-offset' slots are used, or initialized if
they are unset.  `line-offset' is limited so it doesn't go beyond
the beginning/end of buffer.

A cons pair (BUF . POINT) is returned.  If the file in the tag
doesn't exist, these 2 fields are all nil."
  (let* ((path (citre-get-tag-field 'ext-abspath
                                    (citre-peek--tag-node-tag node)))
         (base-marker (citre-peek--get-base-marker node))
         (line-offset (citre-peek--tag-node-line-offset node))
         (buf (citre-peek--find-file-buffer path))
         offset-overflow
         point)
    (when buf
      (with-current-buffer buf
        (save-excursion
          (save-restriction
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
              (setf (citre-peek--tag-node-line-offset node)
                    (+ line-offset (- offset-overflow))))
            (setq point (point))))))
    (cons buf point)))

(defun citre-peek--get-content (node)
  "Get file contents for peeking NODE.
NODE is an instance of `citre-peek--tag-node'.
`citre-peek-file-content-height' specifies the number of lines to
get.

This also modifies the `line-offset' slot of NODE for letting it
not go beyond the start/end of the file."
  (pcase-let ((`(,buf . ,pos) (citre-peek--get-buf-and-pos node)))
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
                (setq end (line-end-position))
                (font-lock-fontify-region beg end)
                (concat (buffer-substring beg end) "\n")))))
      (propertize "This file doesn't exist.\n" 'face 'error))))

(defun citre-peek--line-forward (n &optional node)
  "Scroll the current browsed position in NODE N lines forward.
NODE is an instance of `citre-peek--tag-node', N can be
negative.  When NODE is nil, the currently browsed node is
used.

Note: `citre-peek--get-buf-and-pos' could further modify the
`citre-line-offset' property of current location.  That function
is responsible for not letting it get beyond the start/end of the
file."
  (let* ((node (or node (citre-peek--current-tag-node)))
         (line-offset (citre-peek--tag-node-line-offset node)))
    (setf (citre-peek--tag-node-line-offset node)
          (+ line-offset n))
    (setq citre-peek--content-update t)))

;;;;; Methods for `citre-peek--tag-list'

(defun citre-peek--tag-list-length (taglist)
  "Return the number of nodes in TAGLIST.
TAGLIST is an instance of `citre-peek--tag-list'."
  (length (citre-peek--tag-list-nodes taglist)))

(defun citre-peek--current-node-in-tag-list (taglist)
  "Return the currently browsed tag node in TAGLIST.
TAGLIST is an instance of `citre-peek--tag-list'."
  (nth (citre-peek--tag-list-index taglist)
       (citre-peek--tag-list-nodes taglist)))

(defun citre-peek--current-symbol-in-tag-list (taglist)
  "Return the name field of currently browsed tag node in TAGLIST.
TAGLIST is an instance of `citre-peek--tag-list'."
  (citre-get-tag-field 'name
                       (citre-peek--tag-node-tag
                        (citre-peek--current-node-in-tag-list taglist))))

(defun citre-peek--push-branch-in-current-node-in-tag-list
    (taglist branch)
  "Push BRANCH into the `branches' slot of current browsed node in TAGLIST.
TAGLIST and BRANCH are instances of `citre-peek--tag-list'."
  (push branch
        (citre-peek--tag-node-branches
         (citre-peek--current-node-in-tag-list taglist))))

;;;;; Methods for `citre-peek--session'

(defun citre-peek--session-depth-add (n &optional session)
  "Offset the current depth of peek session SESSION by N.
If SESSION is nil, use the currently active session."
  (let* ((session (or session citre-peek--current-session))
         (depth (citre-peek--session-depth session)))
    (setf (citre-peek--session-depth session) (+ depth n))))

;;;;; Find currently browsed item in the tree

;; NOTE: Find the current session using the variable
;; `citre-peek--current-session'.

(defun citre-peek--current-root-list (&optional session)
  "Return the root list in a peek session SESSION.
If SESSION is nil, use the currently active session."
  (let* ((session (or session citre-peek--current-session)))
    (citre-peek--session-root-list session)))

(defun citre-peek--current-tag-list (&optional session)
  "Return the currently browsed tag list in a peek session SESSION.
If SESSION is nil, use the currently active session."
  (let* ((session (or session citre-peek--current-session))
         (taglist (citre-peek--session-root-list session))
         (depth (citre-peek--session-depth session)))
    (dotimes (_ depth)
      (let* ((node (citre-peek--current-node-in-tag-list taglist))
             (branches (citre-peek--tag-node-branches node)))
        (setq taglist (car branches))))
    taglist))

(defun citre-peek--current-tag-node ()
  "Return the currently browsed tag node."
  (citre-peek--current-node-in-tag-list
   (citre-peek--current-tag-list)))

;;;;; Create tag lists & sessions

(defun citre-peek--make-tag-list-of-current-location (name)
  "Return a tag list of current location.
The tag list is an instance of `citre-peek--tag-list', with its
`nodes' slot only contains one def node pointing to the
current line, and NAME being the name field of the tag.

The returned tag list can be used as the root tag list of the
peek session."
  (let* ((tag (citre-make-tag-of-current-location name))
         (taglist (citre-peek--tag-list-create (list tag))))
    taglist))

(defun citre-peek--make-branch (tags)
  "Create a new branch in the peek history.
This maked TAGS a branch of the current tag node."
  (unless tags (error "TAGS is nil"))
  (let ((branch (citre-peek--tag-list-create tags)))
    (citre-peek--push-branch-in-current-node-in-tag-list
     (citre-peek--current-tag-list) branch)
    (citre-peek--session-depth-add 1)
    (citre-peek--setup-displayed-tags-interval branch)))

(defun citre-peek--make-session (tags &optional marker)
  "Create a peek session from TAGS.
When MARKER is non-nil, and is in a file buffer, make a tag for
it using the name `citre-peek-root-symbol-str', and make it the
root tag list, then make TAGS a branch of it."
  (let ((taglist (citre-peek--tag-list-create tags)))
    (if (and marker (buffer-local-value 'buffer-file-name
                                        (marker-buffer marker)))
        (save-excursion
          (goto-char marker)
          (let ((root-list (citre-peek--make-tag-list-of-current-location
                            citre-peek-root-symbol-str)))
            (if taglist
                (progn
                  (citre-peek--push-branch-in-current-node-in-tag-list
                   root-list taglist)
                  (citre-peek--session-create root-list 1))
              (citre-peek--session-create root-list 0))))
      (if taglist (citre-peek--session-create taglist 0)
        (error "TAGS is nil")))))

;;;;; Manage session state

(defun citre-peek--setup-displayed-tags-interval (&optional taglist)
  "Set `citre-peek--displayed-tags-interval' based on TAGLIST.
TAGLIST is an instance of `citre-peek--tag-list'.  The interval
is set so that it doesn't exceeds
`citre-peek-definitions-height', and also fits the number of
nodes in TAGLIST.

When TAGLIST is nil, the currently browsed taglist is used."
  (let* ((taglist (or taglist (citre-peek--current-tag-list)))
         (len (citre-peek--tag-list-length taglist))
         (idx (citre-peek--tag-list-index taglist))
         start end lines overflow)
    (setq lines (min citre-peek-tag-list-height len))
    (setq start idx)
    (setq end (+ idx (1- lines)))
    (setq overflow (max 0 (- end (1- len))))
    (cl-incf start (- overflow))
    (cl-incf end (- overflow))
    (setq citre-peek--displayed-tags-interval
          (cons start end))))

(defun citre-peek--setup-session (session)
  "Set up state variables for browsing a peek session SESSION."
  (setq citre-peek--current-session session)
  (citre-peek--setup-displayed-tags-interval
   (citre-peek--current-tag-list session))
  (setq citre-peek--content-update t))

(defun citre-peek--def-index-forward (n)
  "In a peek window, move N steps forward in the definition list.
N can be negative."
  (let* ((taglist (citre-peek--current-tag-list))
         (start (car citre-peek--displayed-tags-interval))
         (end (cdr citre-peek--displayed-tags-interval))
         (idx (citre-peek--tag-list-index taglist))
         (len (citre-peek--tag-list-length taglist)))
    (setq idx (mod (+ n idx) len))
    (setf (citre-peek--tag-list-index taglist) idx)
    (unless (<= start idx end)
      (let ((offset (if (> start idx)
                        (- idx start)
                      (- idx end))))
        (setcar citre-peek--displayed-tags-interval
                (+ offset start))
        (setcdr citre-peek--displayed-tags-interval
                (+ offset end))))
    (setq citre-peek--content-update t)))

;;;;; Display

(defun citre-peek--make-definition-str (tag root)
  "Generate str of TAG to show in peek window.
ROOT is the project root."
  (citre-make-tag-str tag nil '(annotation) `(location :root ,root)))

(defun citre-peek--make-border ()
  "Return the border to be used in peek windows."
  (if (display-graphic-p)
      (propertize
       (citre-peek--maybe-decorate-fringes "\n")
       'face 'citre-peek-border-face)
    (propertize
     (concat (make-string (1- (window-body-width)) ?-) "\n")
     'face (list :inherit 'default
                 :foreground
                 (face-attribute 'citre-peek-border-face
                                 :background)))))

(defun citre-peek--file-content (taglist)
  "Return a string for displaying file content.
TAGLIST is the currently browsed tag list."
  (let* ((node (citre-peek--current-node-in-tag-list taglist))
         (file-content
          (citre-peek--get-content node)))
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

(defun citre-peek--displayed-tags-str (taglist)
  "Return a string for displaying TAGLIST.
TAGLIST is the currently browsed tag list."
  (let* ((idx (citre-peek--tag-list-index taglist))
         (defs (citre-peek--tag-list-nodes taglist))
         (root (funcall citre-project-root-function))
         (displayed-defs
          (cl-subseq defs
                     (car citre-peek--displayed-tags-interval)
                     (1+ (cdr citre-peek--displayed-tags-interval))))
         (displayed-tags
          (mapcar #'citre-peek--tag-node-tag displayed-defs))
         (displayed-tags-strlist
          (make-list (length displayed-tags) nil))
         (displayed-idx
          (- idx (car citre-peek--displayed-tags-interval)))
         (bg-selected (if (< (display-color-cells) 88)
                          (list :inverse-video t :extend t)
                        (list :background citre-peek--bg-selected :extend t)))
         (bg-alt (if (< (display-color-cells) 88)
                     (list :extend t)
                   (list :background citre-peek--bg-alt :extend t))))
    (dotimes (n (length displayed-defs))
      (let ((line (citre-peek--make-definition-str
                   (nth n displayed-tags) root)))
        (when (citre-peek--tag-node-branches (nth n displayed-defs))
          (setq line (concat citre-peek-peeked-through-tag-prefix
                             line)))
        (setq line (concat (citre--fit-line line) "\n"))
        (if (eq n displayed-idx)
            (setf (nth n displayed-tags-strlist)
                  (citre--add-face line bg-selected))
          (setf (nth n displayed-tags-strlist)
                (citre--add-face line bg-alt)))))
    (string-join displayed-tags-strlist)))

(defun citre-peek--session-info (taglist)
  "Return a string for displaying session info.
TAGLIST is the currently browsed tag list.  Session info means
the index of currently browsed definition, the total number of
definitions, and the current chain in the code reading history."
  (let* ((idx (citre-peek--tag-list-index taglist))
         (len (citre-peek--tag-list-length taglist))
         ;; We need to traverse the tree from the beginning to create the
         ;; chain.
         (taglist (citre-peek--current-root-list))
         (depth 0)
         (current-depth (citre-peek--session-depth
                         citre-peek--current-session))
         chain
         session-info)
    (while taglist
      (let* ((node (citre-peek--current-node-in-tag-list taglist))
             (symbol (citre-peek--current-symbol-in-tag-list taglist))
             (branches (citre-peek--tag-node-branches node)))
        (unless symbol
          (setq symbol citre-peek-root-symbol-str))
        (if (eq depth current-depth)
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
        (setq taglist (car branches)))
      (cl-incf depth))
    (setq session-info
          (concat (format "(%s/%s) " (1+ idx) len)
                  (string-join (nreverse chain))
                  "\n"))
    (citre--add-face session-info
                     (list :background citre-peek--bg-alt
                           :extend t))
    session-info))

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'citre-peek-fringe [0]))

(defun citre-peek--maybe-decorate-fringes (str)
  "Decorate STR with left and right fringes.
This is only done wihen `citre-peek-fill-fringe' is non-nil, and
bitmap can be used in the display.  Otherwise STR is returned
directly."
  (if (and (fringe-bitmap-p 'citre-peek-fringe)
           citre-peek-fill-fringe)
      (replace-regexp-in-string
       "\n"
       (concat
        (propertize " "
                    'display
                    '(left-fringe citre-peek-fringe
                                  citre-peek-border-face))
        (propertize " "
                    'display
                    '(right-fringe citre-peek-fringe
                                   citre-peek-border-face))
        ;; Use "\\&" rather than "\n" to keep the original face.
        "\\&")
       str)
    str))

(defun citre-peek--update-display (&optional force)
  "Deal with the update of contents in peek windows.
When FORCE is non-nil, the content of the peek window is
recalculated."
  (unless (minibufferp)
    (let ((overlay-pos (min (point-max) (1+ (line-end-position)))))
      (move-overlay citre-peek--ov overlay-pos overlay-pos))
    (when (or citre-peek--content-update force)
      (let* ((taglist (citre-peek--current-tag-list))
             (initial-newline (if (eq (line-end-position) (point-max))
                                  "\n" ""))
             (border (citre-peek--make-border)))
        (overlay-put citre-peek--ov 'after-string
                     (concat initial-newline
                             border
                             (citre-peek--maybe-decorate-fringes
                              (concat
                               (citre-peek--file-content taglist)
                               (citre-peek--displayed-tags-str taglist)
                               (citre-peek--session-info taglist)))
                             border)))
      (setq citre-peek--content-update nil))))

;;;;; Pick point in "ace" style

(defun citre-ace-pick-point ()
  "Pick a point in the showed part of current buffer using \"ace\" operation.
If a key in `citre-peek-ace-pick-symbol-at-point-keys' is
pressed, the current point is returned."
  (let* ((sym-bounds (save-excursion
                       (goto-char (window-start))
                       (citre--search-symbols
                        (ceiling (window-screen-lines)))))
         (ace-seqs (citre--ace-key-seqs
                    (length sym-bounds)))
         key pt)
    (cl-block nil
      (while (progn
               (citre--attach-ace-overlay sym-bounds ace-seqs)
               (setq key (read-key "Ace char:")))
        (when (memq key citre-peek-ace-cancel-keys)
          (citre--clean-ace-ov)
          (cl-return))
        (when (memq key citre-peek-ace-pick-symbol-at-point-keys)
          (setq pt (point))
          (citre--clean-ace-ov)
          (cl-return))
        (let ((i (citre--pop-ace-key-seqs ace-seqs key)))
          (when (integerp i)
            (setq pt (car (nth i sym-bounds)))
            (citre--clean-ace-ov)
            (cl-return)))))
    pt))

(defun citre-ace-pick-point-in-peek-window ()
  "Pick a point in the buffer shown in peek window using \"ace\" operation.
The buffer and the point is returned in a cons cell."
  (citre-peek--error-if-not-peeking)
  (pcase-let ((`(,buf . ,pos) (citre-peek--get-buf-and-pos
                               (citre-peek--current-tag-node)))
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
    (citre-peek--update-display 'force)
    (cl-block nil
      (while (setq key (read-key "Ace char:"))
        (when (memq key citre-peek-ace-cancel-keys)
          (setq citre-peek--symbol-bounds nil)
          (setq citre-peek--ace-seqs nil)
          (citre-peek--update-display 'force)
          (cl-return))
        (pcase (citre--pop-ace-key-seqs citre-peek--ace-seqs key)
          ((and (pred integerp) i)
           (let ((pos (car (nth i (cdr citre-peek--symbol-bounds)))))
             (setq citre-peek--symbol-bounds nil)
             (setq citre-peek--ace-seqs nil)
             (citre-peek--update-display 'force)
             (cl-return (cons buf pos))))
          (_ (citre-peek--update-display 'force)))))))

;;;; APIs

(defun citre-peek-show (tags &optional marker)
  "Show TAGS using `citre-peek' UI.

When MARKER is non-nil, and is in a file buffer, record it as the
root of the peek history so we can go back to it after
`citre-peek-jump'."
  (when citre-peek--mode
    (citre-peek-abort))
  (citre-peek--setup-session (citre-peek--make-session tags marker))
  (citre-peek--mode))

;;;; Commands

;;;;; Create/end/restore peek sessions

(defun citre-peek-abort ()
  "Abort peeking."
  (interactive)
  (citre-peek--mode -1))

(defun citre-peek-restore ()
  "Restore recent peek session."
  (interactive)
  (unless citre-peek--mode
    (if citre-peek--current-session
        (citre-peek--mode)
      (user-error "No peek session to restore"))))

;;;;; Browse in file

(defun citre-peek-next-line ()
  "Scroll to the next line in a peek window."
  (interactive)
  (citre-peek--error-if-not-peeking)
  (citre-peek--line-forward 1))

(defun citre-peek-prev-line ()
  "Scroll to the previous line in a peek window."
  (interactive)
  (citre-peek--error-if-not-peeking)
  (citre-peek--line-forward -1))

;;;;; Browse in tag list

(defun citre-peek-next-tag ()
  "Peek the next tag in tag list."
  (interactive)
  (citre-peek--error-if-not-peeking)
  (citre-peek--def-index-forward 1))

(define-obsolete-function-alias 'citre-peek-next-definition
  'citre-peek-next-tag "0.3")

(defun citre-peek-prev-tag ()
  "Peek the previous tag in tag list."
  (interactive)
  (citre-peek--error-if-not-peeking)
  (citre-peek--def-index-forward -1))

(define-obsolete-function-alias 'citre-peek-prev-definition
  'citre-peek-prev-tag "0.3")

;;;;; Browse in the tree history

(defun citre-peek-chain-forward ()
  "Move forward in the currently browsed chain.
This adds 1 to the currently browsed depth.  It's ensured that
the depth is not greater than the maximum depth."
  (interactive)
  (citre-peek--error-if-not-peeking)
  (let ((max-depth-p (null (citre-peek--tag-node-branches
                            (citre-peek--current-tag-node)))))
    (unless max-depth-p
      (citre-peek--session-depth-add 1 citre-peek--current-session)
      (citre-peek--setup-displayed-tags-interval)
      (setq citre-peek--content-update t))))

(defun citre-peek-chain-backward ()
  "Move backward in the currently browsed chain.
This subtracts 1 from the currently browsed depth.  It's ensured
that the depth is not less than 0."
  (interactive)
  (citre-peek--error-if-not-peeking)
  (unless (eq (citre-peek--session-depth citre-peek--current-session) 0)
    (citre-peek--session-depth-add -1 citre-peek--current-session)
    (citre-peek--setup-displayed-tags-interval)
    (setq citre-peek--content-update t)))

;; NOTE: The direction of branch switching commands are decided so that when
;; the user created a new branch (by peeking through), and call `prev-branch',
;; they should see the branch that's previously browsed.

(defun citre-peek-next-branch ()
  "Switch to the next branch under current symbol."
  (interactive)
  (citre-peek--error-if-not-peeking)
  (let* ((node (citre-peek--current-tag-node))
         (branches (citre-peek--tag-node-branches node)))
    (when branches
      (setf (citre-peek--tag-node-branches node)
            (nconc (last branches) (butlast branches)))
      (setq citre-peek--content-update t))))

(defun citre-peek-prev-branch ()
  "Switch to the previous branch under current symbol."
  (interactive)
  (citre-peek--error-if-not-peeking)
  (let* ((node (citre-peek--current-tag-node))
         (branches (citre-peek--tag-node-branches node)))
    (when branches
      (setf (citre-peek--tag-node-branches node)
            (nconc (cdr branches) (list (car branches))))
      (setq citre-peek--content-update t))))

;;;;; Edit definition list

(defun citre-peek-make-current-tag-first ()
  "Put the current tag node in the first position."
  (interactive)
  (citre-peek--error-if-not-peeking)
  (let* ((taglist (citre-peek--current-tag-list))
         (idx (citre-peek--tag-list-index taglist))
         (nodes (citre-peek--tag-list-nodes taglist))
         (node (nth idx nodes)))
    (setf (citre-peek--tag-list-nodes taglist)
          (nconc (list node) (citre--delete-nth idx nodes)))
    (citre-peek--def-index-forward (- idx))))

(defun citre-peek-move-current-tag-up ()
  "Move the current tag up."
  (interactive)
  (citre-peek--error-if-not-peeking)
  (let* ((taglist (citre-peek--current-tag-list))
         (idx (citre-peek--tag-list-index taglist))
         (nodes (citre-peek--tag-list-nodes taglist))
         (node (nth idx nodes)))
    (if (> idx 0)
        (setf (citre-peek--tag-list-nodes taglist)
              (citre--insert-nth node (1- idx)
                                 (citre--delete-nth idx nodes)))
      (setf (citre-peek--tag-list-nodes taglist)
            (nconc (cdr nodes) (list node))))
    (citre-peek--def-index-forward -1)))

(define-obsolete-function-alias 'citre-peek-move-current-def-up
  'citre-peek-move-current-tag-up "0.3")

(defun citre-peek-move-current-tag-down ()
  "Move the current tag down."
  (interactive)
  (citre-peek--error-if-not-peeking)
  (let* ((taglist (citre-peek--current-tag-list))
         (idx (citre-peek--tag-list-index taglist))
         (nodes (citre-peek--tag-list-nodes taglist))
         (node (nth idx nodes)))
    (if (< idx (1- (length nodes)))
        (setf (citre-peek--tag-list-nodes taglist)
              (citre--insert-nth node (1+ idx)
                                 (citre--delete-nth idx nodes)))
      (setf (citre-peek--tag-list-nodes taglist)
            (nconc (list node) (butlast nodes))))
    (citre-peek--def-index-forward 1)))

(define-obsolete-function-alias 'citre-peek-move-current-def-down
  'citre-peek-move-current-tag-down "0.3")

;;;;; Edit history

(defun citre-peek-delete-branch ()
  "Delete the first branch in currently browsed def node."
  (interactive)
  (citre-peek--error-if-not-peeking)
  (let* ((node (citre-peek--current-tag-node))
         (branches (citre-peek--tag-node-branches node)))
    (when (and branches
               (y-or-n-p
                "Deleting the current branch under this symbol.  Continue? "))
      (pop (citre-peek--tag-node-branches node))
      (setq citre-peek--content-update t))))

(defun citre-peek-delete-branches ()
  "Delete all branchs in currently browsed def node."
  (interactive)
  (citre-peek--error-if-not-peeking)
  (let* ((node (citre-peek--current-tag-node))
         (branches (citre-peek--tag-node-branches node)))
    (when (and branches
               (y-or-n-p
                "Deleting all branches under this symbol.  Continue? "))
      (setf (citre-peek--tag-node-branches node) nil)
      (setq citre-peek--content-update t))))

;;;;; Save/load peek session

(defun citre-peek-save-session ()
  "Save current peek session.
This doesn't mean to save it on the disk, but to keep it alive
during the current Emacs session.

A Saved session can be loaded by `citre-peek-load-session'."
  ;; The only way for a session to have a name is to save it.
  (interactive)
  (unless citre-peek--current-session
    (user-error "No active session"))
  (if (citre-peek--session-name citre-peek--current-session)
      (message "Session already saved.")
    (let* ((names (mapcar #'citre-peek--session-name
                          citre-peek--saved-sessions))
           name)
      (setq name (read-string "Give it a name: "))
      (while (member name names)
        (setq name (read-string
                    (format "%s already exists. Give it another name: "
                            name))))
      (setf (citre-peek--session-name citre-peek--current-session) name)
      (push citre-peek--current-session citre-peek--saved-sessions))))

(defun citre-peek-load-session ()
  "Load a peek session."
  (interactive)
  (let ((name-session-alist
         (mapcar (lambda (session)
                   (cons (citre-peek--session-name session) session))
                 citre-peek--saved-sessions)))
    (citre-peek--setup-session
     (alist-get (completing-read "Session: " name-session-alist nil t)
                name-session-alist
                nil nil #'equal))
    (citre-peek--mode)))

;;;;; Clue integration

(defun citre-peek--current-line-clue-location ()
  "Return the location of the first line in peek window.
This location is for using in Clue API calls."
  (unless citre-peek--current-session
    (user-error "No active peek session"))
  (pcase-let* ((node (citre-peek--current-tag-node))
               (tag (citre-peek--tag-node-tag node))
               (`(,buf . ,pos) (citre-peek--get-buf-and-pos node))
               (file (citre-get-tag-field 'ext-abspath tag))
               (line) (project))
    (if buf
        (with-current-buffer buf
          (save-excursion
            (goto-char pos)
            (setq line (line-number-at-pos)))
          (citre-peek--hack-buffer-file-name
            (setq project (funcall citre-project-root-function)))
          `(,file ,line ,project))
      (user-error "The file doesn't exist"))))

(defun citre-peek-copy-clue-link ()
  "Copy currently browsed line in the peek window as a clue link.
The copied link can then be pasted by `clue-paste'.

This depends on the package
\"Clue\" (https://github.com/AmaiKinono/clue)."
  (interactive)
  (pcase-let* ((`(,file ,line ,project)
                (citre-peek--current-line-clue-location)))
    (clue-copy-location file line project)))

(defun citre-peek-paste-clue-link ()
  "Paste currently browsed line in the peek window as a clue link.
This depends on the package
\"Clue\" (https://github.com/AmaiKinono/clue)."
  (interactive)
  (pcase-let* ((`(,file ,line ,project)
                (citre-peek--current-line-clue-location)))
    (clue-paste-location file line project)))

(provide 'citre-ui-peek)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; fill-column: 79
;; emacs-lisp-docstring-fill-column: 65
;; sentence-end-double-space: t
;; End:

;;; citre-ui-peek.el ends here
