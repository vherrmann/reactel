;;; reactel-c.el --- The backend-neutral Reactel components -*- lexical-binding: t -*-

;; Copyright (C) 2026 Valentin Herrmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The standard vocabulary of host components, all named `reactel-c-*'.
;; They only describe *what* to show (`reactel-el' with a keyword type);
;; how it looks, and which of them are supported, is up to the render
;; backend (see `reactel-backend' in `reactel.el').  A backend lists the
;; types it handles in its VALID-TYPE-P function, and rendering an
;; unsupported type is reported as an error.
;;
;;   Content    text newline space heading separator
;;   Layout     vstack hstack list box table row
;;   Controls   button link checkbox select field
;;   Structural fragment error-boundary (handled by the core, every backend)
;;
;; Text and numbers are valid children everywhere.
;;
;; | backend            | file                | supports                    |
;; |--------------------|---------------------|-----------------------------|
;; | Emacs buffer/string| reactel-buffer.el   | everything, interactive     |
;; | Markdown           | reactel-markdown.el | static subset (no field...) |
;; | node tree (tests)  | reactel-node.el     | everything, no painting     |
;;
;; Your own components are ordinary functions that return these elements,
;; so they work unchanged on every backend that supports what they use.
;;
;; Indentation: the constructors and `reactel-el' declare Emacs indentation
;; rules, so children are indented by two columns, not aligned under the
;; first argument:
;;
;;   (reactel-c-vstack :spacing 1
;;     (reactel-c-heading "Title")
;;     (reactel-el 'my-component :key id
;;       :item item))
;;
;; Give the element-building functions of your own the same rule with
;; `(declare (indent defun))' (or `(put \='my-fn \='lisp-indent-function \='defun)').

;;; Code:

(require 'cl-lib)
(require 'reactel)
(require 'reactel-node)

(defun reactel-c-fragment (&rest args)
  "Group ARGS (key/value props, then children) without a wrapper."
  (declare (indent defun))
  (apply #'reactel-el :fragment args))
(put 'reactel-c-fragment 'reactel-constructor t)

(reactel--define-host reactel-c-error-boundary :error-boundary
		      "Catch errors thrown while rendering children.
Props: `:fallback' (element or function of ERROR and RESET returning one),
`:on-error' (function of ERROR) and `:reset-keys'.")

(reactel--define-host reactel-c-text :text
		      "Inline text.
Props: `:face', `:help' (tooltip), `:props' (text property plist).")
(reactel--define-host reactel-c-vstack :vstack
		      "Children one per line.
Props: `:spacing' (blank lines between), `:indent', `:face'.")
(reactel--define-host reactel-c-hstack :hstack
		      "Children on one line.  Props: `:spacing' (spaces between, default 1).")
(reactel--define-host reactel-c-box :box
		      "Children inside a border.
Props: `:border' (single, double, rounded, ascii), `:padding'
\(default 1), `:title'.")
(reactel--define-host reactel-c-table :table
		      "Aligned table of `reactel-c-row' children (components may render rows).
Props: `:header' (first row is a header, default t), `:align' (list of
left or right per column), `:separator'.")
(reactel--define-host reactel-c-row :row
		      "A table row; each child is one cell.  Only valid inside `reactel-c-table'.")
(reactel--define-host reactel-c-list :list
		      "Each child is an item.  Props: `:ordered', `:bullet'.")
(reactel--define-host reactel-c-heading :heading
		      "Heading on its own line.  Props: `:level' (1-8, default 1).")
(reactel--define-host reactel-c-separator :separator
		      "Horizontal rule.  Props: `:width' (default 40), `:char'.")
(reactel--define-host reactel-c-button :button
		      "Push button labelled by its children.
Props: `:on-click', `:disabled', `:help'.")
(reactel--define-host reactel-c-link :link
		      "Link.  Props: `:url' (opened with `browse-url') or `:on-click'.")
(reactel--define-host reactel-c-checkbox :checkbox
		      "Checkbox labelled by its children.
Props: `:checked', `:on-change' (receives the new state).")
(reactel--define-host reactel-c-select :select
		      "Radio group.  Props: `:options', `:value', `:on-change' (chosen value),
`:layout' (`horizontal' or `vertical').")
(reactel--define-host reactel-c-field :field
		      "Text field edited in the minibuffer.
Props: `:value', `:on-change' (receives the new string), `:label'
\(prompt), `:placeholder', `:width', `:completions'.")

(defun reactel-c-newline (&optional count)
  "Element for COUNT (default 1) line breaks."
  (reactel-el :newline :count (or count 1)))
(put 'reactel-c-newline 'reactel-constructor t)

(defun reactel-c-space (&optional width)
  "Element for WIDTH (default 1) spaces."
  (reactel-el :space :width (or width 1)))
(put 'reactel-c-space 'reactel-constructor t)

;;;; Interaction
;;
;; What the controls do when activated.  Backends turn these functions
;; into whatever their users act on (buttons, key bindings, clicks), and
;; the test helpers below use them to act as the user would.


(defun reactel-c-select-options (props)
  "The options of a select with PROPS as a list of (VALUE LABEL)."
  (mapcar (lambda (o) (if (consp o)
                          (list (car o) (format "%s" (cdr o)))
                        (list o (format "%s" o))))
          (plist-get props :options)))

(defun reactel-c-link-thunk (props)
  "Function activating a link with PROPS."
  (or (plist-get props :on-click)
      (let ((url (plist-get props :url)))
        (and url (lambda () (browse-url url))))))

(defun reactel-c-field-thunk (props)
  "Function that edits a field with PROPS in the minibuffer."
  (let ((on-change (plist-get props :on-change))
        (value (or (plist-get props :value) ""))
        (completions (plist-get props :completions))
        (prompt (format "%s: " (or (plist-get props :label)
                                   (plist-get props :placeholder) "Value"))))
    (and on-change
         (lambda ()
           (funcall on-change
                    (if completions
                        (completing-read prompt completions nil nil value)
                      (read-string prompt value)))))))

(defun reactel-c-actions (node)
  "List of (LABEL THUNK) for what can be activated in the host element NODE.
THUNK is nil when the control is disabled or has no handler."
  (let ((props (reactel-node-props node))
        (label (reactel-node-text node)))
    (pcase (reactel-node-type node)
      (:button (list (list label (and (not (plist-get props :disabled))
                                      (plist-get props :on-click)))))
      (:link (list (list label (reactel-c-link-thunk props))))
      (:checkbox (let ((f (plist-get props :on-change)))
                   (list (list label (and f (lambda ()
                                              (funcall f (not (plist-get props :checked)))))))))
      (:select (let ((f (plist-get props :on-change)))
                 (mapcar (lambda (o) (list (cadr o) (and f (lambda () (funcall f (car o))))))
                         (reactel-c-select-options props)))))))


(defun reactel-find-action (root label)
  "Find in ROOT the first activatable component labelled LABEL.
Return (NODE LABEL THUNK) or nil."
  (cl-loop for node in (reactel-node-find-all
                        (reactel-root-container root)
                        (lambda (n) (memq (reactel-node-type n)
                                          '(:button :link :checkbox :select))))
           do (when-let ((a (assoc label (reactel-c-actions node))))
                (cl-return (cons node a)))))

(defun reactel-click (root label)
  "Activate the control labelled LABEL in ROOT, as the user would.
Works for buttons, links, checkboxes and radio options, on any backend
whose nodes are `reactel-node's."
  (let ((found (or (reactel-find-action root label)
                   (error "No control labelled %S" label))))
    (if-let ((thunk (nth 2 found)))
        (reactel-batch (funcall thunk))
      (error "Control %S is disabled" label))))

(defun reactel-find-field (root label)
  "Find the `:field' node in ROOT whose `:label' is LABEL."
  (car (reactel-node-find-all
        (reactel-root-container root)
        (lambda (n) (and (eq (reactel-node-type n) :field)
                         (equal (plist-get (reactel-node-props n) :label) label))))))

(defun reactel-type (root label text)
  "Replace the value of the field labelled LABEL in ROOT by TEXT.
Calls its `:on-change' like the minibuffer prompt would."
  (let* ((field (or (reactel-find-field root label)
                    (error "No field labelled %S" label)))
         (handler (plist-get (reactel-node-props field) :on-change)))
    (reactel-batch (when handler (funcall handler text)))))

(provide 'reactel-c)
;;; reactel-c.el ends here
