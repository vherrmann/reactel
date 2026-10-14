;;; reactel-ex-tree.el --- Reactel example: a file tree with preview -*- lexical-binding: t -*-

;; M-x reactel-demo-tree
;;
;; A directory browser that reads the real file system.  It shows:
;;
;;   - a recursive component: every directory is a `reactel-ex-tree-node'
;;     that renders its subdirectories as more of itself, each keeping its
;;     own expanded/collapsed state;
;;   - effects that do the I/O (listing a directory, reading a file) so that
;;     rendering stays pure, with results kept in state and stale loads
;;     discarded by the effect's cleanup;
;;   - state shared by lifting it up: the selected path and the filters live
;;     in the root and reach the nodes through props and stable callbacks;
;;   - an error shown in place when a directory cannot be read, and a
;;     preview pane that follows the selection.

(require 'dash)
(require 'reactel)
(require 'reactel-c)
(require 'reactel-buffer)

(defcustom reactel-ex-tree-preview-lines 6
  "Number of lines of a file shown in the preview."
  :type 'integer
  :group 'reactel)

;;;; File system access (impure: only ever called from effects and handlers)

(defun reactel-ex-tree--list (directory)
  "The entries of DIRECTORY as (NAME . IS-DIRECTORY), directories first.
Returns (:error MESSAGE) when DIRECTORY cannot be read."
  (condition-case err
      (let ((entries (--map (cons (car it) (eq (file-attribute-type (cdr it)) t))
                            (--remove (member (car it) '("." ".."))
                                      (directory-files-and-attributes directory nil nil t)))))
        (-sort (lambda (a b) (if (eq (cdr a) (cdr b))
                                 (string< (car a) (car b))
                               (cdr a)))
               entries))
    (error (list :error (error-message-string err)))))

(defun reactel-ex-tree--preview (file)
  "The first lines of FILE and its size, as (:size N :lines LIST), or (:error MSG)."
  (condition-case err
      (let ((size (file-attribute-size (file-attributes file))))
        (list :size size
              :lines (with-temp-buffer
                       (insert-file-contents file nil 0 2000)
                       (-take reactel-ex-tree-preview-lines
                              (split-string (buffer-string) "\n")))))
    (error (list :error (error-message-string err)))))

(defun reactel-ex-tree--visible (entries filter show-hidden)
  "ENTRIES without hidden files (unless SHOW-HIDDEN) and files not matching FILTER.
Directories are always kept, so that a match inside them can be found."
  (--filter (and (or show-hidden (not (string-prefix-p "." (car it))))
                 (or (cdr it) (string-empty-p filter)
                     (string-match-p (regexp-quote filter) (car it))))
            entries))

;;;; Components

(reactel-defcomponent reactel-ex-tree-node
    (&key path name depth selected on-select filter show-hidden)
  "The directory PATH shown as NAME; DEPTH 0 is the root and starts open."
  (-let* (((open set-open) (reactel-use-state (zerop depth)))
          ((entries set-entries) (reactel-use-state nil)))
    ;; Load the entries once the directory is opened.  The cleanup marks a
    ;; load as stale, so a result arriving after PATH changed is dropped.
    (reactel-use-effect* (open path)
      (let ((stale nil))
        (when open
          (let ((listing (reactel-ex-tree--list path)))
            (unless stale (funcall set-entries listing))))
        (lambda () (setq stale t))))
    (reactel-c-vstack
      (reactel-c-button :on-click (lambda () (funcall set-open reactel-toggle))
        (format "%s %s/" (if open "▾" "▸") name))
      (when open
        (cond
         ((null entries) (reactel-c-text :face 'shadow "  loading…"))
         ((eq (car entries) :error)
          (reactel-c-text :face 'error (format "  %s" (cadr entries))))
         (t
          (reactel-c-vstack :indent 2
            (--map (let ((child (expand-file-name (car it) path)))
                     (if (cdr it)
                         (reactel-el 'reactel-ex-tree-node :key child
                           :path child :name (car it) :depth (1+ depth)
                           :selected selected :on-select on-select
                           :filter filter :show-hidden show-hidden)
                       (reactel-c-checkbox :key child :checked (equal selected child)
                         :on-change (lambda (_) (funcall on-select child))
                         (car it))))
                   (reactel-ex-tree--visible entries filter show-hidden)))))))))

(reactel-defcomponent reactel-ex-tree-preview (&key path)
  "Details of the file PATH, read in an effect."
  (-let (((info set-info) (reactel-use-state nil)))
    (reactel-use-effect* (path)
      (let ((stale nil))
        (funcall set-info nil)
        (when path
          (let ((preview (reactel-ex-tree--preview path)))
            (unless stale (funcall set-info preview))))
        (lambda () (setq stale t))))
    (reactel-c-box :title (if path (file-name-nondirectory path) "preview")
      (cond
       ((null path) (reactel-c-text :face 'shadow "select a file"))
       ((null info) (reactel-c-text :face 'shadow "reading…"))
       ((plist-get info :error) (reactel-c-text :face 'error (plist-get info :error)))
       (t (reactel-c-vstack
            (reactel-c-text :face 'shadow (format "%d bytes" (plist-get info :size)))
            (--map-indexed (reactel-c-text :key it-index it) (plist-get info :lines))))))))

(reactel-defcomponent reactel-ex-tree (&key root)
  (-let* (((selected set-selected) (reactel-use-state nil))
          ((filter set-filter) (reactel-use-state ""))
          ((show-hidden set-show-hidden) (reactel-use-state nil))
          (open (reactel-use-callback* (selected) ()
                  (when selected (find-file selected)))))
    (reactel-c-vstack :spacing 1
      (reactel-c-hstack
        (reactel-c-field :label "Filter" :placeholder "file name" :width 14
          :value filter :on-change set-filter)
        (reactel-c-checkbox :checked show-hidden :on-change set-show-hidden "hidden files")
        (reactel-c-button :on-click open :disabled (null selected) "open"))
      (reactel-el 'reactel-ex-tree-node :path root :name (file-name-nondirectory
                                                          (directory-file-name root))
        :depth 0 :selected selected :on-select set-selected
        :filter filter :show-hidden show-hidden)
      (reactel-el 'reactel-ex-tree-preview :path selected))))

(defun reactel-demo-tree ()
  "Show the file tree example for the Reactel sources."
  (interactive)
  (reactel-show "*reactel-tree*"
                (reactel-el 'reactel-ex-tree
                  :root (file-name-directory (or load-file-name buffer-file-name
                                                 (locate-library "reactel")
                                                 default-directory)))))

(provide 'reactel-ex-tree)
;;; reactel-ex-tree.el ends here
