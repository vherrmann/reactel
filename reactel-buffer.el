;;; reactel-buffer.el --- Emacs buffer render backend for Reactel -*- lexical-binding: t -*-

;; Copyright (C) 2026 Valentin Herrmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A `reactel-backend' that paints the standard components (`reactel-c.el')
;; as text in an Emacs buffer, or in a string.  It is built on
;; `reactel-node.el': the core keeps a tree of `reactel-buffer-node's up
;; to date and `reactel-buffer--finish' repaints the buffer after every
;; commit, keeping point and window positions.
;;
;; Text with faces and text properties, lines, columns, boxes, tables and
;; `button.el' buttons (TAB, RET, mouse, `push-button' all work):
;;
;;   (reactel-show "*counter*" (reactel-el 'counter))      ; interactive buffer
;;   (reactel-render-to-string (reactel-el 'counter))      ; a propertized string
;;   (reactel-buffer-string-root)                          ; for tests, see `reactel-click'
;;
;; Every component of `reactel-c.el' is supported.  Lines drawn by a box or
;; table are plain text, cells must be single-line, and a box or table that
;; starts a list item begins on the line after the bullet.

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'outline)
(require 'reactel)
(require 'reactel-node)
(require 'reactel-c)

(cl-defstruct (reactel-buffer-node (:include reactel-node)
                                   (:constructor reactel-buffer-node--make)
                                   (:copier nil))
  "A node of the buffer backend.
REGION is (START . END), the text of the node in the painted buffer.
BUFFER is, for the container, the buffer painted into (nil: a string)."
  region buffer)

;;;; Faces and options

(defgroup reactel-buffer nil "Reactel buffer backend." :group 'reactel)

(defcustom reactel-buffer-bullet "•"
  "Default bullet of `reactel-c-list'."
  :type 'string)

(defcustom reactel-buffer-separator-char ?─
  "Default character of `reactel-c-separator' and table header rules."
  :type 'character)

(defface reactel-buffer-field '((t :inherit button))
  "Face of `reactel-c-field' elements.")
(defface reactel-buffer-placeholder '((t :inherit shadow))
  "Face of the placeholder of empty fields.")
(defface reactel-buffer-table-header '((t :inherit bold))
  "Face of table header rows.")

;;;; Printing

(defvar reactel-buffer--indent "" "Prefix inserted at the start of every non-empty line.")
(defvar reactel-buffer--hanging nil
  "Position just after a list bullet, where a block may start on the same line.")

(defun reactel-buffer--insert (string)
  "Insert STRING at point, applying the current indentation to its lines."
  (let ((indent reactel-buffer--indent))
    (if (string-empty-p indent)
        (insert string)
      ;; Insert at once, then indent each non-empty line that starts in
      ;; STRING (no substring per line).
      (let* ((beg (point))
             (end (progn (insert string) (point))))
        (goto-char beg)
        (while (< (point) end)
          (when (and (bolp) (not (eq (char-after) ?\n)))
            (insert indent)
            (setq end (+ end (length indent))))
          (unless (search-forward "\n" end t)
            (goto-char end)))))))

(defun reactel-buffer--end-line ()
  "Finish the current line unless at the beginning of one."
  (unless (bolp) (insert "\n")))

(defun reactel-buffer--block-start (&optional lenient)
  "Start a block on a fresh line; return its start.
With LENIENT, a block directly after a list bullet stays on that line."
  (unless (or (bolp) (and lenient (eql (point) reactel-buffer--hanging)))
    (insert "\n"))
  (point))

(defun reactel-buffer--face-region (beg face)
  "Add FACE to the text from BEG to point."
  (when (and face (> (point) beg))
    (add-face-text-property beg (point) face t)))

(defun reactel-buffer--print-children (node)
  "Print the children of NODE."
  (mapc #'reactel-buffer--print (reactel-node-children node)))

(defun reactel-buffer--print (node)
  "Insert the text of NODE and its subtree at point and record its region.
Regions are markers until `reactel-buffer--freeze-regions', so that edits
made later by ancestors (box borders, table padding) keep them correct."
  (let* ((beg (point))
         (props (reactel-node-props node))
         (start
          (if (eq (reactel-node-type node) :string)
              (progn (reactel-buffer--insert (plist-get props :value)) nil)
            (reactel-buffer--print-host node (reactel-node-type node) props))))
    (setf (reactel-buffer-node-region node)
          (cons (copy-marker (or start beg) t) (copy-marker (point))))))

(defun reactel-buffer--freeze-regions (node)
  "Turn the region markers in NODE's subtree into plain positions."
  (let ((r (reactel-buffer-node-region node)))
    (when (and r (markerp (car r)))
      (setf (reactel-buffer-node-region node)
            (cons (marker-position (car r)) (marker-position (cdr r))))
      (set-marker (car r) nil)
      (set-marker (cdr r) nil)))
  (mapc #'reactel-buffer--freeze-regions (reactel-node-children node)))

(defun reactel-buffer--make-button (beg thunk &optional help face)
  "Make the text from BEG to point a button running THUNK.
HELP is the tooltip.  FACE replaces the default `button' face."
  (when thunk
    (apply #'make-text-button beg (point)
           'action (lambda (_button) (reactel-batch (funcall thunk)))
           'follow-link t
           'help-echo (or help "RET: activate")
           (and face (list 'face face)))))

(defun reactel-buffer--print-host (node type props)
  "Print host NODE of TYPE with PROPS; return its start if not the point."
  (pcase type
    (:text
     (let ((beg (point)))
       (reactel-buffer--print-children node)
       (reactel-buffer--face-region beg (plist-get props :face))
       (when (and (plist-get props :help) (> (point) beg))
         (put-text-property beg (point) 'help-echo (plist-get props :help)))
       (when (and (plist-get props :props) (> (point) beg))
         (add-text-properties beg (point) (plist-get props :props)))
       nil))
    (:newline (insert (make-string (or (plist-get props :count) 1) ?\n)) nil)
    (:space (reactel-buffer--insert (make-string (or (plist-get props :width) 1) ?\s)) nil)
    (:vstack (reactel-buffer--print-vstack node props))
    (:hstack (reactel-buffer--print-hstack node props) nil)
    (:heading
     (let* ((beg (reactel-buffer--block-start t))
            (level (max 1 (min 8 (or (plist-get props :level) 1)))))
       (reactel-buffer--print-children node)
       (reactel-buffer--face-region beg (intern (format "outline-%d" level)))
       (reactel-buffer--end-line)
       beg))
    (:separator
     (let ((beg (reactel-buffer--block-start t)))
       (reactel-buffer--insert (make-string (or (plist-get props :width) 40)
                                            (or (plist-get props :char) reactel-buffer-separator-char)))
       (reactel-buffer--end-line)
       beg))
    (:list (reactel-buffer--print-list node props))
    (:box (reactel-buffer--print-box node props))
    (:table (reactel-buffer--print-table node props))
    (:row (error "A reactel-c-row can only be a child of reactel-c-table"))
    (:button
     (let ((beg (point))
           (thunk (cadr (car (reactel-c-actions node)))))
       (reactel-buffer--insert "[")
       (reactel-buffer--print-children node)
       (reactel-buffer--insert "]")
       (if thunk
           (reactel-buffer--make-button beg thunk (plist-get props :help))
         (add-face-text-property beg (point) 'shadow))
       nil))
    (:link
     (let ((beg (point)))
       (reactel-buffer--print-children node)
       (reactel-buffer--make-button beg (cadr (car (reactel-c-actions node)))
                                    (or (plist-get props :help) (plist-get props :url)) 'link)
       nil))
    (:checkbox
     (let ((beg (point)))
       (reactel-buffer--insert (if (plist-get props :checked) "[x] " "[ ] "))
       (reactel-buffer--print-children node)
       (reactel-buffer--make-button beg (cadr (car (reactel-c-actions node))) nil)
       nil))
    (:select (reactel-buffer--print-select node props) nil)
    (:field (reactel-buffer--print-field props) nil)
    (_ (error "Unknown host element type: %S" type))))

(defun reactel-buffer--print-vstack (node props)
  "Print a vstack NODE with PROPS; return its start."
  (let* ((beg (reactel-buffer--block-start t))
         (spacing (or (plist-get props :spacing) 0))
         (reactel-buffer--indent (concat reactel-buffer--indent
					 (make-string (or (plist-get props :indent) 0) ?\s)))
         (printed nil))
    (dolist (c (reactel-node-children node))
      (let ((b (point)))
        (reactel-buffer--print c)
        (when (> (point) b)
          (setq printed t)
          (reactel-buffer--end-line)
          (insert (make-string spacing ?\n)))))
    (when (and printed (> spacing 0))
      (delete-region (- (point) spacing) (point)))
    (reactel-buffer--face-region beg (plist-get props :face))
    beg))

(defun reactel-buffer--print-hstack (node props)
  "Print an hstack NODE with PROPS."
  (let ((sep (make-string (or (plist-get props :spacing) 1) ?\s))
        (first t))
    (dolist (c (reactel-node-children node))
      (let ((before-sep (point)))
        (unless first (reactel-buffer--insert sep))
        (let ((b (point)))
          (reactel-buffer--print c)
          (if (= (point) b)
              (delete-region before-sep b)
            (setq first nil)))))))

(defun reactel-buffer--print-list (node props)
  "Print a list NODE with PROPS; return its start."
  (let ((beg (reactel-buffer--block-start t))
        (n 0))
    (dolist (item (reactel-node-children node))
      (reactel-buffer--end-line)
      (let ((marker (concat (if (plist-get props :ordered)
                                (format "%d." (cl-incf n))
                              (or (plist-get props :bullet) reactel-buffer-bullet))
                            " ")))
        (reactel-buffer--insert marker)
        (let ((reactel-buffer--hanging (point))
              (reactel-buffer--indent (concat reactel-buffer--indent
                                              (make-string (string-width marker) ?\s))))
          (reactel-buffer--print item))))
    (reactel-buffer--end-line)
    beg))

(defun reactel-buffer--print-select (node props)
  "Print a radio group NODE with PROPS."
  (let ((selected (plist-get props :value))
        (vertical (eq (plist-get props :layout) 'vertical))
        (first t)
        (actions (reactel-c-actions node)))
    (when vertical (reactel-buffer--block-start))
    (cl-loop for (value label) in (reactel-c-select-options props)
             for (_ thunk) in actions
             do (cond (first (setq first nil))
                      (vertical (reactel-buffer--end-line))
                      (t (reactel-buffer--insert "  ")))
             (let ((beg (point)))
               (reactel-buffer--insert (format "(%s) %s" (if (equal value selected) "*" " ") label))
               (reactel-buffer--make-button beg thunk)))
    (when vertical (reactel-buffer--end-line))))

(defun reactel-buffer--print-field (props)
  "Print a field with PROPS."
  (let* ((beg (point))
         (value (or (plist-get props :value) ""))
         (width (plist-get props :width))
         (shown (if (string-empty-p value) (or (plist-get props :placeholder) "") value)))
    (reactel-buffer--insert "[")
    (let ((b (point)))
      (reactel-buffer--insert shown)
      (when (string-empty-p value)
        (add-face-text-property b (point) 'reactel-buffer-placeholder)))
    (when (and width (< (string-width shown) width))
      (reactel-buffer--insert (make-string (- width (string-width shown)) ?_)))
    (reactel-buffer--insert "]")
    (reactel-buffer--make-button beg (reactel-c-field-thunk props) "RET: edit" 'reactel-buffer-field)))

;;;; Box

(defconst reactel-buffer--box-styles
  '((single "┌" "─" "┐" "│" "└" "┘")
    (double "╔" "═" "╗" "║" "╚" "╝")
    (rounded "╭" "─" "╮" "│" "╰" "╯")
    (ascii "+" "-" "+" "|" "+" "+"))
  "Border characters: top-left, horizontal, top-right, vertical, bottom-left,
bottom-right.")

(defun reactel-buffer--print-box (node props)
  "Print a box NODE with PROPS; return its start."
  (pcase-let* ((outer reactel-buffer--indent)
               (`(,tl ,h ,tr ,v ,bl ,br)
                (or (alist-get (or (plist-get props :border) 'single) reactel-buffer--box-styles)
                    (error "Unknown box border %S" (plist-get props :border))))
               (pad (or (plist-get props :padding) 1))
               (title (plist-get props :title))
               (beg (reactel-buffer--block-start))
               (lines nil)
               (after nil))
    (let ((reactel-buffer--indent "")) (reactel-buffer--print-children node) (reactel-buffer--end-line))
    (let ((end (point-marker)))
      (goto-char beg)
      (while (< (point) end)
        (push (cons (copy-marker (point)) (copy-marker (line-end-position))) lines)
        (forward-line 1))
      (let* ((content-w (apply #'max 0 (mapcar (lambda (l)
                                                 (string-width (buffer-substring-no-properties
                                                                (car l) (cdr l))))
                                               lines)))
             (inner (max (+ content-w (* 2 pad)) (if title (+ 4 (string-width title)) 0)))
             (width (- inner (* 2 pad))))
        ;; `lines' is bottom-up, so earlier edits do not disturb later ones.
        (dolist (l lines)
          (let ((w (string-width (buffer-substring-no-properties (car l) (cdr l)))))
            (goto-char (cdr l))
            (insert (make-string (+ (- width w) pad) ?\s) v)
            (goto-char (car l))
            (insert outer v (make-string pad ?\s))))
        (goto-char end)
        (insert outer bl (apply #'concat (make-list inner h)) br "\n")
        (setq after (point-marker))
        (goto-char beg)
        (insert outer tl
                (if title
                    (concat h " " title " "
                            (apply #'concat (make-list (- inner (string-width title) 3) h)))
                  (apply #'concat (make-list inner h)))
                tr "\n")
        (goto-char after)
        (set-marker after nil)
        (set-marker end nil)))
    beg))

;;;; Table

(defun reactel-buffer--print-table (node props)
  "Print a table NODE with PROPS; return its start."
  (let* ((beg (reactel-buffer--block-start))
         (sep (or (plist-get props :separator) " │ "))
         (aligns (plist-get props :align))
         (header (if (plist-member props :header) (plist-get props :header) t))
         (rows nil))
    (dolist (row (reactel-node-children node))
      (unless (eq (reactel-node-type row) :row)
        (error "A reactel-c-table can only contain reactel-c-row children"))
      (let ((row-beg (point)) (cells nil) (first t))
        (dolist (cell (reactel-node-children row))
          (unless first (reactel-buffer--insert sep))
          (setq first nil)
          (let ((b (point)))
            (reactel-buffer--print cell)
            (push (cons (copy-marker b t) (copy-marker (point))) cells)))
        (setf (reactel-buffer-node-region row) (cons (copy-marker row-beg t) (copy-marker (point))))
        (reactel-buffer--end-line)
        (push (nreverse cells) rows)))
    (setq rows (nreverse rows))
    (let* ((ncols (apply #'max 0 (mapcar #'length rows)))
           (widths (cl-loop for j below ncols
                            collect (apply #'max 0
                                           (mapcar (lambda (r)
                                                     (if-let ((c (nth j r)))
                                                         (string-width (buffer-substring-no-properties
                                                                        (car c) (cdr c)))
                                                       0))
                                                   rows)))))
      (dolist (r rows)
        (cl-loop for c in r
                 for w in widths
                 for j from 0
                 for last = (= j (1- (length r)))
                 do (let* ((cw (string-width (buffer-substring-no-properties (car c) (cdr c))))
                           (fill (make-string (- w cw) ?\s)))
                      (cond ((eq (nth j aligns) 'right)
                             (goto-char (car c)) (insert fill))
                            ((not last)
                             (goto-char (cdr c)) (insert fill))))))
      (goto-char (point-max))
      (when (and header rows)
        (let* ((anchor (car (car (car rows))))
               (line-start (save-excursion (goto-char anchor) (line-beginning-position)))
               (row-end (save-excursion (goto-char anchor) (line-end-position)))
               (width (apply #'max
                             (mapcar (lambda (r)
                                       (save-excursion
                                         (goto-char (car (car r)))
                                         (string-width
                                          (buffer-substring-no-properties
                                           (line-beginning-position) (line-end-position)))))
                                     rows))))
          (add-face-text-property line-start row-end 'reactel-buffer-table-header)
          (goto-char (1+ row-end))
          (insert (make-string (max 0 width) reactel-buffer-separator-char) "\n")
          (goto-char (point-max)))))
    (dolist (r rows) (dolist (c r) (set-marker (car c) nil) (set-marker (cdr c) nil)))
    beg))

;;;; Whole trees

(defun reactel-buffer--print-root (container)
  "Print the tree below CONTAINER at point and freeze the regions."
  (let ((reactel-buffer--indent "")
        (reactel-buffer--hanging nil))
    (reactel-buffer--print-children container)
    (mapc #'reactel-buffer--freeze-regions (reactel-node-children container))))

;;;; The backend

(defconst reactel-buffer--host-types
  '(:text :newline :space :vstack :hstack :heading :separator :list :box
	  :table :row :button :link :checkbox :select :field)
  "The host types this backend paints.")

(defvar-local reactel-buffer--root nil
  "The root rendered into this buffer.")

(defvar reactel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'backward-button)
    (define-key map (kbd "RET") #'push-button)
    map)
  "Keymap of `reactel-mode'.  Buttons also react to the mouse.")

(define-derived-mode reactel-mode special-mode "Reactel"
  "Major mode of buffers rendered by `reactel-render-buffer'."
  (buffer-disable-undo))

(defun reactel-buffer--line-col (pos)
  "Return (LINE . COLUMN) of POS in the current buffer."
  (save-excursion (goto-char pos)
                  (cons (line-number-at-pos) (current-column))))

(defun reactel-buffer--goto-line-col (lc)
  "Go to the (LINE . COLUMN) position LC, clamped to the buffer."
  (goto-char (point-min))
  (forward-line (1- (car lc)))
  (move-to-column (cdr lc)))

(defun reactel-buffer--paint (container buffer)
  "Repaint BUFFER from the tree below CONTAINER, keeping point and windows."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((inhibit-read-only t)
             ;; A repaint is not an edit: recording the erased and
             ;; reprinted text as undo dominated allocation.
             (buffer-undo-list t)
             (inhibit-modification-hooks t)
             (point (reactel-buffer--line-col (point)))
             (windows (mapcar (lambda (w)
                                (list w (reactel-buffer--line-col (window-point w))
                                      (reactel-buffer--line-col (window-start w))))
                              (get-buffer-window-list buffer nil t))))
        (erase-buffer)
        (reactel-buffer--print-root container)
        (reactel-buffer--goto-line-col point)
        (pcase-dolist (`(,w ,wp ,ws) windows)
          (when (window-live-p w)
            (set-window-start w (save-excursion (reactel-buffer--goto-line-col ws) (point)) t)
            (set-window-point w (save-excursion (reactel-buffer--goto-line-col wp) (point)))))
        (set-buffer-modified-p nil)))))

(defun reactel-buffer--finish (container)
  "Paint the tree below CONTAINER, into its buffer or, for a string root, into
its data."
  (if-let ((buffer (reactel-buffer-node-buffer container)))
      (reactel-buffer--paint container buffer)
    (setf (reactel-node-data container)
          (list :output (with-temp-buffer
                          (reactel-buffer--print-root container)
                          (buffer-string))))))

(defvar reactel-buffer-backend
  (reactel-node-make-backend
   :name 'buffer
   :valid-p (lambda (type) (memq type reactel-buffer--host-types))
   :make-node #'reactel-buffer-node--make
   :finish #'reactel-buffer--finish)
  "The `reactel-backend' painting into Emacs buffers and strings.")

;;;; Buffers

(defun reactel-render-buffer (buffer-or-name element)
  "Render ELEMENT into BUFFER-OR-NAME and return the root.
Calling this again for the same buffer renders the new ELEMENT into
the existing root (state of matching components is preserved).  The
tree is unmounted, running effect cleanups, when the buffer is killed.
The buffer is not displayed; see `reactel-show'."
  (let* ((buffer (get-buffer-create buffer-or-name))
         (root (with-current-buffer buffer
                 (unless (derived-mode-p 'reactel-mode) (reactel-mode))
                 (or reactel-buffer--root
                     (let ((r (reactel-create-root
                               reactel-buffer-backend
                               (reactel-buffer-node--make :type :root :buffer buffer))))
                       (setq reactel-buffer--root r)
                       (add-hook 'kill-buffer-hook
                                 (lambda ()
                                   (when reactel-buffer--root
                                     (let ((r reactel-buffer--root))
                                       (setf (reactel-buffer-node-buffer (reactel-root-container r)) nil
                                             reactel-buffer--root nil)
                                       (reactel-unmount r))))
                                 nil t)
                       r)))))
    (reactel-root-render root element)))

(defun reactel-root-buffer (root)
  "Return the buffer ROOT paints into, or nil (string roots, killed buffers)."
  (let ((b (reactel-buffer-node-buffer (reactel-root-container root))))
    (and (buffer-live-p b) b)))

(defun reactel-show (buffer-or-name element)
  "Render ELEMENT into BUFFER-OR-NAME (see `reactel-render-buffer') and display it."
  (let ((root (reactel-render-buffer buffer-or-name element)))
    (pop-to-buffer (reactel-root-buffer root))
    root))

(defun reactel-buffer-goto-node (node)
  "Move point, in the buffer that shows NODE, to the start of NODE.
NODE is what a `:ref' receives.  Windows showing the buffer follow.
Returns the position, or nil when NODE is not painted in a buffer."
  (let* ((container (reactel-node-root node))
         (buffer (and (reactel-buffer-node-p container)
                      (buffer-live-p (reactel-buffer-node-buffer container))
                      (reactel-buffer-node-buffer container)))
         (region (and (reactel-buffer-node-p node) (reactel-buffer-node-region node))))
    (when (and buffer region (not (eq container node)))
      (with-current-buffer buffer (goto-char (car region)))
      (dolist (w (get-buffer-window-list buffer nil t))
        (set-window-point w (car region)))
      (car region))))

;;;; Strings

(defun reactel-buffer-string-root ()
  "Create a root whose text is available from `reactel-root-text'."
  (reactel-create-root reactel-buffer-backend (reactel-buffer-node--make :type :root)))

(defalias 'reactel-create-string-root #'reactel-buffer-string-root)

(defun reactel-render-to-string (element)
  "Render ELEMENT once and return the resulting propertized string.
The tree is unmounted afterwards, so effect cleanups run."
  (let ((root (reactel-buffer-string-root)))
    (reactel-root-render root element)
    (prog1 (substring (or (reactel-root-output root) ""))
      (reactel-unmount root))))

(defun reactel-root-output (root)
  "The propertized text last painted for the string ROOT."
  (plist-get (reactel-node-data (reactel-root-container root)) :output))

(defun reactel-root-text (root)
  "Current text of string ROOT without text properties."
  (substring-no-properties (or (reactel-root-output root) "")))

(provide 'reactel-buffer)
;;; reactel-buffer.el ends here
