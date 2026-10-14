;;; reactel-node.el --- A generic node-tree render backend for Reactel -*- lexical-binding: t -*-

;; Copyright (C) 2026 Valentin Herrmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The core (`reactel.el') talks to a `reactel-backend' that creates and
;; mutates nodes.  Most backends only need a retained tree of nodes that
;; they can walk and paint after each commit; this library provides it:
;;
;;   (reactel-node-make-backend :name 'my-backend
;;                              :finish (lambda (container) ...paint...))
;;
;; Nodes are `reactel-node' structs (type, props, children, parent) kept
;; up to date by the core, so FINISH just walks `reactel-node-children'
;; from the CONTAINER node.  A backend with extra per-node state includes
;; the struct (`:include reactel-node') and passes its constructor as
;; `:make-node'; see `reactel-buffer.el' and `reactel-markdown.el'.
;;
;; `reactel-node-test-backend' is a ready-made headless backend: render
;; into it and inspect the tree with `reactel-node-snapshot' to test
;; components without any display.

;;; Code:

(require 'cl-lib)
(require 'reactel)

(cl-defstruct (reactel-node (:constructor reactel-node--make) (:copier nil))
  "A node of a retained tree.  TYPE is a host keyword, or `:string' for
text (prop `:value') or `:root' for the container.  DATA is free for backends."
  type props children parent data)

(cl-defun reactel-node-make-backend (&key name finish valid-p (make-node #'reactel-node--make))
  "Make a `reactel-backend' that maintains a tree of `reactel-node's.
FINISH is called with the container node after every commit.  VALID-P
tells which host types are supported (all, by default).  MAKE-NODE is a
constructor taking :type and :props keywords, for node structs that
include `reactel-node'."
  (reactel-make-backend
   :name (or name 'node)
   :valid-type-p valid-p
   :create-container (lambda () (funcall make-node :type :root))
   :create-node (lambda (type props) (funcall make-node :type type :props props))
   :update-node (lambda (node _type _old new) (setf (reactel-node-props node) new))
   :set-children (lambda (node children)
                   (dolist (c (reactel-node-children node))
                     (when (eq (reactel-node-parent c) node)
                       (setf (reactel-node-parent c) nil)))
                   (dolist (c children) (setf (reactel-node-parent c) node))
                   (setf (reactel-node-children node) children))
   :destroy-node (lambda (node) (setf (reactel-node-parent node) nil))
   :finish-commit finish))

;;;; Walking

(defun reactel-node-walk (node fn)
  "Call FN on NODE and all its descendants, depth first."
  (funcall fn node)
  (dolist (c (reactel-node-children node)) (reactel-node-walk c fn)))

(defun reactel-node-find-all (node predicate)
  "List the nodes in the tree of NODE (NODE included) satisfying PREDICATE."
  (let (out)
    (reactel-node-walk node (lambda (n) (when (funcall predicate n) (push n out))))
    (nreverse out)))

(defun reactel-node-text (node)
  "The text of the subtree of NODE, without any formatting."
  (if (eq (reactel-node-type node) :string)
      (plist-get (reactel-node-props node) :value)
    (mapconcat #'reactel-node-text (reactel-node-children node) "")))

(defun reactel-node-root (node)
  "The topmost ancestor of NODE: the container node, for a mounted node."
  (while (reactel-node-parent node) (setq node (reactel-node-parent node)))
  node)

;;;; Snapshots, for tests

(defun reactel-node-snapshot (node)
  "A printable description of the tree of NODE.
Text nodes are strings, other nodes are (TYPE PROPS CHILD...) where
PROPS lists the props whose values are not functions."
  (if (eq (reactel-node-type node) :string)
      (plist-get (reactel-node-props node) :value)
    (let (props)
      (cl-loop for (k v) on (reactel-node-props node) by #'cddr
               unless (functionp v) do (setq props (nconc props (list k v))))
      `(,(reactel-node-type node) ,props
        ,@(mapcar #'reactel-node-snapshot (reactel-node-children node))))))

(defvar reactel-node-test-backend (reactel-node-make-backend :name 'test)
  "A headless backend: a tree of nodes without any painting.")

(defun reactel-node-test-root ()
  "Create a root on `reactel-node-test-backend'."
  (reactel-create-root reactel-node-test-backend))

(defun reactel-node-root-snapshot (root)
  "Snapshot of the children of the container of ROOT (see `reactel-node-snapshot')."
  (mapcar #'reactel-node-snapshot (reactel-node-children (reactel-root-container root))))

(provide 'reactel-node)
;;; reactel-node.el ends here
