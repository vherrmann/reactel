;;; reactel-markdown.el --- Markdown render backend for Reactel -*- lexical-binding: t -*-

;; Copyright (C) 2026 Valentin Herrmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A second `reactel-backend', to show that components are independent of
;; how they are displayed: the same function components that render into
;; an Emacs buffer (`reactel-buffer.el') render here into Markdown text.
;;
;;   (reactel-markdown-render (reactel-el 'my-component))   ; => "# Title\n\n..."
;;
;; For a root that keeps state and re-renders, use `reactel-markdown-root'
;; and read the current text with `reactel-markdown-output'; `reactel-click'
;; and `reactel-type' work on it as well.
;;
;; Mapping: vstack -> paragraphs, heading -> `#', list -> `-'/`1.', table ->
;; pipe table, box -> blockquote, link -> `[x](url)', face bold/italic ->
;; `**x**'/`*x*', checkbox -> `[x] label', select -> `(*) a  ( ) b',
;; field -> a code span, separator -> `---'.  Buttons are shown as `[label]'.
;; It is a static format: nothing is interactive.

;;; Code:

(require 'cl-lib)
(require 'reactel)
(require 'reactel-node)
(require 'reactel-c)

(defconst reactel-markdown--block-types '(:vstack :heading :separator :list :box :table)
  "Host types that are blocks; the others flow inline.")

(defconst reactel-markdown--host-types
  (append reactel-markdown--block-types
          '(:text :newline :space :hstack :row :button :link :checkbox :select :field))
  "The host types this backend renders.")

(defun reactel-markdown--face-p (face name)
  "Non-nil if FACE (a face or a list of faces) includes NAME."
  (or (eq face name) (and (consp face) (memq name face))))

(defun reactel-markdown--inline (node)
  "Render NODE as inline Markdown."
  (let ((props (reactel-node-props node)))
    (pcase (reactel-node-type node)
      (:string (plist-get props :value))
      (:text
       (let ((s (mapconcat #'reactel-markdown--inline (reactel-node-children node) ""))
             (face (plist-get props :face)))
         (cond ((string-empty-p s) s)
               ((reactel-markdown--face-p face 'bold) (format "**%s**" s))
               ((reactel-markdown--face-p face 'italic) (format "*%s*" s))
               (t s))))
      (:hstack (mapconcat #'reactel-markdown--inline
                          (cl-remove "" (reactel-node-children node)
                                     :key #'reactel-markdown--inline :test #'equal)
                          (make-string (or (plist-get props :spacing) 1) ?\s)))
      (:newline (mapconcat #'identity (make-list (or (plist-get props :count) 1) "  \n") ""))
      (:space (make-string (or (plist-get props :width) 1) ?\s))
      (:link (let ((label (mapconcat #'reactel-markdown--inline (reactel-node-children node) ""))
                   (url (plist-get props :url)))
               (if url (format "[%s](%s)" label url) label)))
      (:button (format "[%s]" (reactel-node-text node)))
      (:checkbox (format "%s %s" (if (plist-get props :checked) "[x]" "[ ]")
                         (mapconcat #'reactel-markdown--inline (reactel-node-children node) "")))
      (:select (mapconcat (lambda (o)
                            (format "(%s) %s" (if (equal (car o) (plist-get props :value)) "*" " ")
                                    (cadr o)))
                          (reactel-c-select-options props) "  "))
      (:field (let ((v (or (plist-get props :value) "")))
                (if (string-empty-p v)
                    (format "*%s*" (or (plist-get props :placeholder) ""))
                  (format "`%s`" v))))
      (_ (if (memq (reactel-node-type node) reactel-markdown--block-types)
             (reactel-markdown--block node)
           (mapconcat #'reactel-markdown--inline (reactel-node-children node) ""))))))

(defun reactel-markdown--prefix-lines (text first rest)
  "Prefix the first line of TEXT with FIRST and the others with REST."
  (let ((lines (split-string text "\n")))
    (mapconcat #'identity
               (cons (concat first (car lines))
                     (mapcar (lambda (l) (if (string-empty-p l) (string-trim-right rest) (concat rest l)))
                             (cdr lines)))
               "\n")))

(defun reactel-markdown--flow (children)
  "Render CHILDREN as paragraphs: inline runs and blocks, separated by blank lines."
  (let (paragraphs run)
    (cl-flet ((flush () (when run
                          (let ((s (string-trim (apply #'concat (nreverse run)))))
                            (unless (string-empty-p s) (push s paragraphs)))
                          (setq run nil))))
      (dolist (c children)
        (if (memq (reactel-node-type c) reactel-markdown--block-types)
            (progn (flush) (push (reactel-markdown--block c) paragraphs))
          (push (reactel-markdown--inline c) run)))
      (flush))
    (mapconcat #'identity (nreverse paragraphs) "\n\n")))

(defun reactel-markdown--table (node)
  "Render the table NODE as a pipe table."
  (let* ((props (reactel-node-props node))
         (header (if (plist-member props :header) (plist-get props :header) t))
         (rows (mapcar (lambda (row) (mapcar #'reactel-markdown--inline (reactel-node-children row)))
                       (reactel-node-children node)))
         (ncols (apply #'max 0 (mapcar #'length rows)))
         (aligns (plist-get props :align)))
    (cl-flet ((line (cells) (concat "| " (mapconcat #'identity
                                                    (cl-loop for j below ncols
                                                             collect (or (nth j cells) ""))
                                                    " | ")
                                    " |")))
      (when rows
        (mapconcat
         #'identity
         (append (list (line (if header (car rows) (make-list ncols "")))
                       (concat "|" (mapconcat (lambda (j) (if (eq (nth j aligns) 'right) " ---: " " --- "))
                                              (number-sequence 0 (1- ncols)) "|")
                               "|"))
                 (mapcar #'line (if header (cdr rows) rows)))
         "\n")))))

(defun reactel-markdown--block (node)
  "Render NODE as a Markdown block."
  (let ((props (reactel-node-props node))
        (kids (reactel-node-children node)))
    (pcase (reactel-node-type node)
      (:vstack (mapconcat (lambda (c) (reactel-markdown--flow (list c))) kids "\n\n"))
      (:heading (format "%s %s" (make-string (max 1 (min 6 (or (plist-get props :level) 1))) ?#)
                        (mapconcat #'reactel-markdown--inline kids "")))
      (:separator "---")
      (:list (let ((n 0))
               (mapconcat
                (lambda (item)
                  (let ((marker (if (plist-get props :ordered)
                                    (format "%d. " (cl-incf n))
                                  "- ")))
                    (reactel-markdown--prefix-lines (reactel-markdown--flow (list item))
                                                    marker (make-string (length marker) ?\s))))
                kids "\n")))
      (:box (let ((body (reactel-markdown--flow kids))
                  (title (plist-get props :title)))
              (reactel-markdown--prefix-lines
               (if title (format "**%s**\n\n%s" title body) body) "> " "> ")))
      (:table (or (reactel-markdown--table node) ""))
      (_ (mapconcat #'reactel-markdown--inline kids "")))))

(defun reactel-markdown--finish (container)
  "Store the Markdown for the tree below CONTAINER in its data."
  (setf (reactel-node-data container)
        (list :output (let ((s (reactel-markdown--flow (reactel-node-children container))))
                        (if (string-empty-p s) "" (concat s "\n"))))))

(defvar reactel-markdown-backend
  (reactel-node-make-backend
   :name 'markdown
   :valid-p (lambda (type) (memq type reactel-markdown--host-types))
   :finish #'reactel-markdown--finish)
  "The `reactel-backend' producing Markdown text.")

(defun reactel-markdown-root ()
  "Create a root on `reactel-markdown-backend'."
  (reactel-create-root reactel-markdown-backend))

(defun reactel-markdown-output (root)
  "The Markdown text last produced for ROOT."
  (or (plist-get (reactel-node-data (reactel-root-container root)) :output) ""))

(defun reactel-markdown-render (element)
  "Render ELEMENT once and return the Markdown text.
The tree is unmounted afterwards, so effect cleanups run."
  (let ((root (reactel-markdown-root)))
    (reactel-root-render root element)
    (prog1 (reactel-markdown-output root)
      (reactel-unmount root))))

(provide 'reactel-markdown)
;;; reactel-markdown.el ends here
