;;; reactel.el --- React for Emacs Lisp: function components, hooks, reconciler -*- lexical-binding: t -*-

;; Copyright (C) 2026 Valentin Herrmann
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: lisp, tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Reactel is a port of the core ideas of React.js to Emacs Lisp:
;;
;;   - Declarative *elements* (`reactel-el') describe the UI.
;;   - *Function components* turn props into elements.  Any function
;;     accepting a keyword plist is a component; `reactel-defcomponent'
;;     is sugar for `cl-defun' with `&key' props.
;;   - *Hooks* give function components state and effects:
;;     `reactel-use-state', `reactel-use-reducer', `reactel-use-effect',
;;     `reactel-use-layout-effect', `reactel-use-memo',
;;     `reactel-use-callback', `reactel-use-ref', `reactel-use-context',
;;     `reactel-use-id' and `reactel-use-sync-external-store'.
;;   - A *reconciler* keeps a persistent instance tree, matches children
;;     by key / position / type, preserves hook state across renders and
;;     runs effect cleanups on unmount.
;;   - Contexts (`reactel-create-context'), memoization (`reactel-memo',
;;     `:memo' in `reactel-defcomponent'), error boundaries
;;     (`:error-boundary') and automatic batching of state updates.
;;
;; This file is renderer-agnostic.  A *render backend* (`reactel-backend',
;; the equivalent of react-reconciler's host config) receives the changes
;; of every commit as mutations of its own nodes: `create-node',
;; `update-node', `set-children', `destroy-node', then `finish-commit'.
;; Shipped backends: `reactel-buffer.el' (Emacs buffers and strings),
;; `reactel-markdown.el' (Markdown text) and `reactel-node.el' (a plain
;; node tree, the base for simple backends and for testing).
;; `reactel-c.el' defines the backend-neutral component vocabulary.
;;
;; Minimal example:
;;
;;   (reactel-defcomponent counter (&key (initial 0))
;;     (cl-destructuring-bind (n set-n) (reactel-use-state initial)
;;       (reactel-c-hstack
;;         (reactel-c-text (format "Count: %d" n))
;;         (reactel-c-button :on-click (lambda () (funcall set-n reactel-inc))
;;           "+"))))
;;
;;   (reactel-show "*counter*" (reactel-el 'counter :initial 5))

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'reactel-lint)
(require 'reactel-compile)

(defgroup reactel nil
  "React-style components for Emacs Lisp."
  :group 'tools
  :prefix "reactel-")

(defcustom reactel-synchronous nil
  "When non-nil, state updates re-render immediately.
By default updates made outside `reactel-batch' (for example from a
timer or a process filter) are coalesced and applied from a zero
second timer, like React 18's automatic batching."
  :type 'boolean)

(defconst reactel--max-render-phase-updates 25
  "Maximum number of re-runs of one component caused by render-phase updates.")

(defconst reactel--max-update-depth 100
  "Maximum number of consecutive commits caused by effect-triggered updates.")

;;;; Strict mode (runtime checks of the Rules of React)

(defcustom reactel-strict nil
  "Runtime checks of the Rules of React, like React's Strict Mode.
nil disables them, t enables all, or a list of checks:
  purity         render twice and compare, double-invoke memo/reducer functions
  mutation       props, state, memo values and elements must not be mutated
  render-update  no state updates of other components while rendering
  effects        mount effects twice (mount, cleanup, mount) to expose
                 missing cleanups
  buffer         rendering must not modify the current buffer
  impure         with `reactel-strict-mode': timers, processes and hooks
                 must not be started while rendering
Components defined with `:lint nil' are exempt."
  :type '(choice (const :tag "Off" nil) (const :tag "All checks" t)
                 (set (const purity) (const mutation) (const render-update)
                      (const effects) (const buffer) (const impure)))
  :group 'reactel)

(defun reactel--strict-p (check)
  "Non-nil if the strict mode CHECK is enabled."
  (and reactel-strict (or (eq reactel-strict t) (memq check reactel-strict))))

(defvar reactel--rendering-fn nil
  "The component function being rendered by the reconciler, or nil.")

(defun reactel--assert-rendering (name)
  "Signal an error unless the component NAME is called by the reconciler."
  (unless (or (eq reactel--rendering-fn name)
              (and (functionp reactel--rendering-fn)
                   (eq reactel--rendering-fn (symbol-function name))))
    (error "Component `%s' was called directly; render it with (reactel-el '%s ...)"
           name name)))

(defun reactel--deep-copy (x &optional depth)
  "Copy conses, vectors and strings of X (to detect later mutation)."
  (let ((depth (or depth 0)))
    (cond
     ((> depth 40) x)
     ((consp x)
      (let (acc (tail x))
        (while (consp tail)
          (push (reactel--deep-copy (car tail) (1+ depth)) acc)
          (setq tail (cdr tail)))
        (let ((copy (nreverse acc)))
          (when tail (setcdr (last copy) (reactel--deep-copy tail (1+ depth))))
          copy)))
     ((stringp x) (copy-sequence x))
     ((vectorp x) (vconcat (mapcar (lambda (e) (reactel--deep-copy e (1+ depth))) x)))
     (t x))))

(defun reactel--render-equal (a b)
  "Structural equality of render outputs A and B, ignoring closure identity."
  (cond ((eq a b) t)
        ((and (reactel-element-p a) (reactel-element-p b))
         (and (eq (reactel-element-type a) (reactel-element-type b))
              (equal (reactel-element-key a) (reactel-element-key b))
              (reactel--render-equal (reactel-element-props a) (reactel-element-props b))))
        ((and (consp a) (consp b))
         (and (reactel--render-equal (car a) (car b))
              (reactel--render-equal (cdr a) (cdr b))))
        ((and (vectorp a) (vectorp b) (= (length a) (length b)))
         (cl-every #'reactel--render-equal a b))
        ((and (functionp a) (functionp b) (not (symbolp a)) (not (symbolp b))) t)
        (t (equal a b))))

;;;; Elements

(cl-defstruct (reactel-element (:constructor reactel--make-element)
                               (:copier nil))
  "An immutable description of a piece of UI."
  type props key ref snap)

(defun reactel--text-element (value)
  "Make a text element for string or number VALUE."
  (reactel--make-element
   :type :string
   :props (list :value (if (stringp value) value (number-to-string value)))))

(defun reactel--flatten-children (children)
  "Normalize CHILDREN into a flat list of elements.
nil and t are ignored, strings and numbers become text elements and
lists or vectors are flattened recursively."
  (let (out)
    (cl-labels ((walk (c)
                  (cond ((or (null c) (eq c t)))
                        ((reactel-element-p c) (push c out))
                        ((or (stringp c) (numberp c))
                         (push (reactel--text-element c) out))
                        ((vectorp c) (mapc #'walk c))
                        ((proper-list-p c) (mapc #'walk c))
                        (t (error "Invalid reactel child: %S" c)))))
      (walk children))
    (nreverse out)))

(defun reactel--valid-type-p (type)
  "Non-nil when TYPE can be the type of an element."
  (or (keywordp type)
      (reactel-context-p type)
      (reactel-memo-p type)
      (and (symbolp type) type (fboundp type))
      (functionp type)))

(defun reactel-el (type &rest args)
  "Create an element of TYPE.
TYPE is one of:
  - a keyword naming a host element (`:vstack', `:text', `:button', ...;
    see `reactel-c.el'), or `:fragment' / `:error-boundary';
  - a function or a symbol naming a function: a function component,
    called with the props as a keyword plist;
  - a context made by `reactel-create-context': a provider;
  - a value made by `reactel-memo'.

ARGS start with keyword/value props followed by children.  `:key'
and `:ref' are not passed to the component.  Children may be
elements, strings, numbers, nil (ignored) or lists of those; they
are passed to components as the `:children' prop."
  (declare (indent defun))
  (unless (reactel--valid-type-p type)
    (error "Invalid element type: %S" type))
  (let (props key ref explicit-children)
    (while (and args (keywordp (car args)))
      (unless (cdr args) (error "Prop %S has no value" (car args)))
      (let ((k (car args)) (v (cadr args)))
        (pcase k
          (:key (setq key v))
          (:ref (setq ref v))
          (:children (setq explicit-children v))
          (_ (setq props (nconc props (list k v))))))
      (setq args (cddr args)))
    (let ((all-props (nconc props (list :children
                                        (reactel--flatten-children
                                         (or args explicit-children))))))
      (reactel--make-element
       :type type :key key :ref ref :props all-props
       ;; Strict mode: remember the props to detect mutation after creation.
       :snap (and (reactel--strict-p 'mutation) (reactel--deep-copy all-props))))))

(defmacro reactel--define-host (name type doc)
  "Define NAME as a constructor for host elements of TYPE with DOC."
  `(progn
     (defun ,name (&rest args)
       ,(concat doc "\n\n(fn [:PROP VALUE]... &rest CHILDREN)")
       (declare (indent defun))
       (apply #'reactel-el ,type args))
     ;; Calls of constructors are what the compiler memoizes (`reactel-compile').
     (put ',name 'reactel-constructor t)))


;;;; Compiler support (see `reactel-compile.el')

(defcustom reactel-compile nil
  "Whether components are compiled to memoize automatically.
Applies to components defined afterwards; each component can override it
with the `:compile' option.  See `reactel-compile.el'."
  :type 'boolean
  :group 'reactel)

(defun reactel--use-cache (scopes)
  "Hook: the cache vector of a compiled component with SCOPES memoized scopes."
  (reactel-hook-value
   (reactel--next-hook
    'cache (lambda () (reactel--make-hook :kind 'cache
                                          :value (make-vector (* 2 scopes) :unset))))))

(defun reactel--same-deps-p (a b)
  "Non-nil if the dependency lists A and B hold the same values."
  (and (= (length a) (length b)) (cl-every #'reactel--same-value a b)))

(defmacro reactel--memo (cache index deps form)
  "Return FORM, cached in CACHE at INDEX until one of the values DEPS changes."
  (let ((d (make-symbol "deps")) (old (make-symbol "old")))
    `(let ((,d (list ,@deps))
           (,old (aref ,cache ,index)))
       (if (and (not (eq ,old :unset)) (reactel--same-deps-p ,old ,d))
           (aref ,cache ,(1+ index))
         (let ((value ,form))
           (aset ,cache ,index ,d)
           (aset ,cache ,(1+ index) value)
           value)))))

(defun reactel--compile-body (name forms bound options skip)
  "Return the body FORMS of NAME, compiled if enabled.
BOUND are the lambda list variables, OPTIONS the parsed options and SKIP
non-nil when the definition opted out (`:lint nil')."
  (let ((want (if (plist-member options :compile)
                  (car (plist-get options :compile))
                reactel-compile)))
    (if (not (and want (not skip)))
        forms
      (let ((result (reactel-compile-forms forms bound macroexpand-all-environment)))
        (when name (put name 'reactel-compile-info result))
        (when-let ((why (plist-get result :skipped)))
          (display-warning 'reactel (format "%s was not compiled: %s" (or name "component") why)))
        (plist-get result :forms)))))

;;;; Components

(defun reactel--component-arglist (args)
  "Turn component ARGS into a `cl-defun' lambda list accepting any prop."
  (cond ((null args) '(&rest _props))
        ((and (memq '&key args) (not (memq '&allow-other-keys args)))
         (append args '(&allow-other-keys)))
        (t args)))

(defun reactel--parse-component-options (body)
  "Split BODY into (DOC OPTIONS FORMS): docstring, plist of options, rest."
  (let (doc options)
    (when (and (stringp (car body)) (cdr body)) (setq doc (pop body)))
    (while (memq (car body) '(:memo :lint :compile))
      (let ((k (pop body))) (setq options (plist-put options k (list (pop body))))))
    (list doc options body)))

(defmacro reactel-defcomponent (name args &rest body)
  "Define NAME as a function component taking keyword props ARGS.
ARGS is a `cl-defun' lambda list, usually starting with `&key'.  Props
that are not listed are ignored, and children arrive as the `children'
prop (`&key children').  BODY may start with a docstring and then
options:
  :memo t   skip re-rendering when no prop changed (see
            `reactel-props-equal'), or a function of the old and new props
  :lint nil opt out of the static checks (`reactel-lint') and of the
            runtime checks of `reactel-strict' (and of compilation)
  :compile  t or nil: memoize automatically (see `reactel-compile.el'),
            overriding `reactel-compile'

Hooks may be called in BODY, always in the same order.  BODY returns
an element (or a list of elements, or nil).  Use it as
`(reactel-el \\='NAME :prop value ...)'; calling NAME directly is an error."
  (declare (indent 2) (doc-string 3))
  (pcase-let* ((`(,doc ,options ,forms) (reactel--parse-component-options body))
               (lint (if (plist-member options :lint) (car (plist-get options :lint)) t))
               (compiled (reactel--compile-body
                          name forms
                          (reactel-lint--arglist-vars (reactel--component-arglist args))
                          options (not lint)))
               (expansion
                `(progn
                   (cl-defun ,name ,(reactel--component-arglist args)
                     ,@(and doc (list doc))
                     (reactel--assert-rendering ',name)
                     ,@compiled)
                   (put ',name 'reactel-component t)
                   ,@(unless lint `((put ',name 'reactel-lint-exempt t)))
                   ,@(when (plist-member options :memo)
                       `((put ',name 'reactel-memo ,(car (plist-get options :memo)))))
                   ',name)))
    ;; Register now, so that later forms of the same file are checked against it.
    (put name 'reactel-component t)
    (if lint
        (reactel-lint-wrap (format "component `%s'" name) forms expansion nil
                           (reactel-lint--arglist-vars (reactel--component-arglist args)))
      expansion)))

(defmacro reactel-defhook (name args &rest body)
  "Define NAME as a custom hook: a function that may call other hooks.
It follows the same rules as a component (hooks only at the top level)
and may only be called from components and other hooks.  Name it
`...use-...' by convention.  Takes the `:lint nil' option."
  (declare (indent 2) (doc-string 3))
  (pcase-let* ((`(,doc ,options ,forms) (reactel--parse-component-options body))
               (lint (if (plist-member options :lint) (car (plist-get options :lint)) t))
               (compiled (reactel--compile-body name forms (reactel-lint--arglist-vars args)
                                                options (not lint)))
               (expansion `(progn
                             (cl-defun ,name ,args ,@(and doc (list doc)) ,@compiled)
                             (put ',name 'reactel-hook t)
                             ',name)))
    (put name 'reactel-hook t)
    (if lint
        (reactel-lint-wrap (format "hook `%s'" name) forms expansion nil
                           (reactel-lint--arglist-vars args))
      expansion)))

(defmacro reactel-lambda (args &rest body)
  "Anonymous function component; like `reactel-defcomponent' without a name.
Beware: a closure created during a render is a new type each time, so
the subtree below it is remounted on every render.  Define components
at top level."
  (declare (indent 1))
  (pcase-let* ((`(,_ ,options ,forms) (reactel--parse-component-options body))
               (lint (if (plist-member options :lint) (car (plist-get options :lint)) t))
               (compiled (reactel--compile-body
                          nil forms (reactel-lint--arglist-vars (reactel--component-arglist args))
                          options (not lint)))
               (expansion `(cl-function (lambda ,(reactel--component-arglist args) ,@compiled))))
    (if lint
        (reactel-lint-wrap "anonymous component" forms expansion nil
                           (reactel-lint--arglist-vars (reactel--component-arglist args)))
      expansion)))

(cl-defstruct (reactel-memo (:constructor reactel--make-memo) (:copier nil))
  "A component wrapped by `reactel-memo'."
  inner compare)

(defun reactel-memo (component &optional compare)
  "Wrap COMPONENT so it re-renders only when its props change.
COMPARE receives the old and the new props plists and returns non-nil
when they are equal; it defaults to `reactel-props-equal'.  Keep the
returned value (e.g. in a `defconst') so its identity is stable."
  (reactel--make-memo :inner component :compare compare))

(defun reactel--same-value (a b)
  "Non-nil if A and B are the same value: `eq', `eql' or equal strings.
Strings must also have the same text properties."
  (or (eql a b) (and (stringp a) (stringp b) (equal-including-properties a b))))

(defun reactel-props-equal (old new)
  "Shallow comparison of props plists OLD and NEW.
Values are compared with `eq', numbers with `eql' and strings by
content and text properties; lists, closures and other objects by
identity, like React."
  (let ((n 0) (ok t))
    (cl-loop for (k v) on old by #'cddr
             while ok
             do (cl-incf n)
             (unless (and (plist-member new k)
                          (if (eq k :children)
                              (reactel--same-children v (plist-get new k))
                            (reactel--same-value v (plist-get new k))))
               (setq ok nil)))
    (and ok (= n (/ (length new) 2)))))

(defun reactel--same-children (a b)
  "Non-nil when child element lists A and B hold the same element objects."
  (and (= (length a) (length b)) (cl-every #'eq a b)))

(defun reactel--component-function (type)
  "Return the function that renders component TYPE."
  (if (reactel-memo-p type)
      (reactel--component-function (reactel-memo-inner type))
    type))

(defun reactel--memo-compare (type)
  "Return the props comparison function if TYPE is memoized, else nil."
  (cond ((reactel-memo-p type)
         (or (reactel-memo-compare type) #'reactel-props-equal))
        ((and (symbolp type) (get type 'reactel-memo))
         (let ((m (get type 'reactel-memo)))
           (if (eq m t) #'reactel-props-equal m)))))

(defun reactel--type-name (type)
  "Printable name for TYPE."
  (cond ((reactel-memo-p type) (reactel--type-name (reactel-memo-inner type)))
        ((reactel-context-p type) (format "%s.Provider" (reactel-context-name type)))
        ((symbolp type) (symbol-name type))
        (t "anonymous")))

;;;; Context

(cl-defstruct (reactel-context (:constructor reactel--make-context)
                               (:copier nil))
  "A context created by `reactel-create-context'.  Use it as a provider type."
  default name)

(defun reactel-create-context (&optional default name)
  "Create a context with DEFAULT value and optional NAME.
Provide a value with `(reactel-el CONTEXT :value V CHILDREN...)' and
read it in components with `reactel-use-context'."
  (reactel--make-context :default default :name (or name "Context")))

;;;; Instances and roots

(defvar reactel--pending-roots)         ; see "Scheduling"

(cl-defstruct (reactel-instance (:constructor reactel--make-instance)
                                (:copier nil))
  "A mounted element: the persistent counterpart of a `reactel-element'."
  kind type key element parent root children
  hooks hooks-ready dirty child-dirty dead
  contexts render-update data attached-ref
  node node-props host-nodes)

(cl-defstruct (reactel-backend (:constructor reactel-make-backend) (:copier nil))
  "A render backend: how host elements become something visible.
All functions work on *nodes*, values the backend creates and owns.
Host elements have a keyword type (`:button', ...) and plain text has
the type `:string' with the prop `:value'.  Props never contain
`:children', `:key' or `:ref'.

NAME             symbol, for messages.
CREATE-NODE      (TYPE PROPS) -> a new node.  Required.
UPDATE-NODE      (NODE TYPE OLD-PROPS NEW-PROPS) after props changed.  Required.
SET-CHILDREN     (NODE CHILD-NODES) replace the ordered children of NODE with
                 CHILD-NODES, each created before.  Required.  Called with
                 the container node for the top level.
DESTROY-NODE     (NODE) the node will not be used again (children first).
FINISH-COMMIT    (CONTAINER) after all the mutations of one commit; paint here.
CREATE-CONTAINER ()  -> the container node of a root made without one.
VALID-TYPE-P     (TYPE) -> non-nil for supported host types; others are
                 reported as errors as soon as they are rendered.

Event handlers in props are plain functions.  A backend must call them
through `reactel-batch' so that the updates they cause are applied once.
Refs (`:ref') receive the node."
  name create-node update-node set-children destroy-node finish-commit
  create-container valid-type-p)

(cl-defstruct (reactel-root (:constructor reactel--make-root) (:copier nil))
  "The top of an instance tree, its backend and its container node."
  instance backend container nodes busy)

(cl-defstruct (reactel-hook (:constructor reactel--make-hook) (:copier nil))
  "State of one hook call."
  kind state reducer queue deps value fn cleanup pending dispatch
  (snapshot :none) mounted)

(defun reactel--kind (type)
  "Instance kind for element TYPE."
  (cond ((eq type :string) :text)
        ((eq type :fragment) :fragment)
        ((eq type :error-boundary) :boundary)
        ((keywordp type) :host)
        ((reactel-context-p type) :provider)
        (t :component)))

(defun reactel-create-root (backend &optional container)
  "Create a root rendering through BACKEND, a `reactel-backend'.
CONTAINER is the backend node the top-level elements are put in; it
defaults to what the backend's CREATE-CONTAINER makes."
  (unless (and (reactel-backend-p backend)
               (reactel-backend-create-node backend)
               (reactel-backend-update-node backend)
               (reactel-backend-set-children backend))
    (error "Invalid render backend (needs create-node, update-node, set-children): %S"
           backend))
  (let* ((container (or container
                        (and (reactel-backend-create-container backend)
                             (funcall (reactel-backend-create-container backend)))))
         (root (reactel--make-root :backend backend :container container))
         (inst (reactel--make-instance
                :kind :fragment :type :fragment :root root
                :element (reactel--make-element
                          :type :fragment :props (list :children nil)))))
    (setf (reactel-root-instance root) inst)
    root))

(defun reactel-root-render (root element)
  "Render ELEMENT into ROOT, replacing what was rendered before.
The tree is up to date when this returns, even when called from an event
handler, inside `reactel-batch' or while another root renders (so a
component may render a string with another backend, e.g.
`reactel-markdown-render').  Rendering into a root that is rendering is
an error."
  (when (reactel-root-busy root)
    (error "Cannot render into a root while it is rendering"))
  (let ((inst (reactel-root-instance root)))
    (setf (reactel-instance-element inst)
          (reactel--make-element
           :type :fragment
           :props (list :children (reactel--flatten-children element))))
    (reactel--mark-dirty inst)
    (setq reactel--pending-roots (delq root reactel--pending-roots))
    (reactel--flush-root root)
    root))

(defun reactel-unmount (root)
  "Unmount everything in ROOT: run effect cleanups and remove the nodes."
  (let* ((inst (reactel-root-instance root))
         (backend (reactel-root-backend root))
         (kids (reactel-instance-children inst))
         err)
    (setf (reactel-instance-children inst) nil
          (reactel-instance-element inst)
          (reactel--make-element :type :fragment :props (list :children nil))
          (reactel-instance-dirty inst) nil
          (reactel-instance-child-dirty inst) nil)
    (dolist (k kids)
      (reactel--unmount-tree k (lambda (f) (condition-case e (funcall f)
                                             (error (unless err (setq err e)))))))
    (dolist (k kids) (reactel--destroy-tree backend k))
    (when (reactel-root-nodes root)
      (setf (reactel-root-nodes root) nil)
      (funcall (reactel-backend-set-children backend) (reactel-root-container root) nil))
    (when (reactel-backend-finish-commit backend)
      (funcall (reactel-backend-finish-commit backend) (reactel-root-container root)))
    (when err (signal (car err) (cdr err)))))

(defun reactel-instances (root &optional predicate)
  "List the instances below ROOT (depth first) satisfying PREDICATE."
  (let (out)
    (cl-labels ((walk (i)
                  (when (or (null predicate) (funcall predicate i)) (push i out))
                  (mapc #'walk (reactel-instance-children i))))
      (mapc #'walk (reactel-instance-children (reactel-root-instance root))))
    (nreverse out)))

(defun reactel-instance-props (inst)
  "Props plist of INST."
  (reactel-element-props (reactel-instance-element inst)))

;;;; Hook machinery

(defvar reactel--current nil
  "The component instance being rendered, or nil.")
(defvar reactel--hook-cursor nil
  "Remaining hooks of the current instance during an update render.")
(defvar reactel--hook-acc nil
  "Hooks created so far (reversed) during the first render.")
(defvar reactel--effects nil
  "Hooks with pending effects, in reverse completion order.")
(defvar reactel--deletions nil
  "Instance subtrees removed during this render, in reverse order.")
(defvar reactel--refs nil
  "Host instances whose `:ref' must be (re)attached on commit.")
(defvar reactel--touched nil
  "Instances rendered in this pass, newest first; they are synced to the backend.")

(defun reactel--next-hook (kind make)
  "Return the next hook of KIND, creating it with MAKE on the first render."
  (unless reactel--current
    (error "Hooks can only be called while a function component renders"))
  (if (reactel-instance-hooks-ready reactel--current)
      (let ((h (pop reactel--hook-cursor)))
        (unless h
          (error "Rendered more hooks than during the previous render (in <%s>)"
                 (reactel--type-name (reactel-instance-type reactel--current))))
        (unless (eq (reactel-hook-kind h) kind)
          (error "Hooks called in a different order (in <%s>): expected %s, got %s"
                 (reactel--type-name (reactel-instance-type reactel--current))
                 (reactel-hook-kind h) kind))
        h)
    (let ((h (funcall make)))
      (push h reactel--hook-acc)
      h)))

(defun reactel--callable-p (value nargs)
  "Non-nil if VALUE is a function object that accepts NARGS arguments.
Symbols are never callable here, even when `fboundp': states such as
`all' or `loud' are plain data and must not be mistaken for updaters.
Use a lambda, `apply-partially' or the constants `reactel-inc' etc."
  (and value (functionp value) (not (symbolp value))
       (let ((a (func-arity value)))
         (and (<= (car a) nargs)
              (or (eq (cdr a) 'many) (>= (cdr a) nargs))))))

(defun reactel--exempt-p (inst)
  "Non-nil if the component of INST opted out of strict checks (`:lint nil')."
  (let ((fn (reactel--component-function (reactel-instance-type inst))))
    (and (symbolp fn) (get fn 'reactel-lint-exempt))))

(defun reactel--check-idempotent (inst fn props name first)
  "Render INST a second time and compare with FIRST (rule: pure rendering)."
  (let ((reactel--hook-acc nil)
        (reactel--hook-cursor (reactel-instance-hooks inst)))
    (setf (reactel-instance-render-update inst) nil)
    (let ((second (apply fn props)))
      (when reactel--hook-cursor
        (error "Rendered fewer hooks than during the previous render (in <%s>)" name))
      (when (reactel-instance-render-update inst)
        (error "Component <%s> updates its own state differently each time it renders" name))
      (unless (reactel--render-equal first second)
        (error "Component <%s> is not pure: rendering it twice with the same props and state gave different output (do not read or write refs, variables or the clock while rendering)"
               name)))))

(defun reactel--call-component (inst element)
  "Run the function component of INST for ELEMENT and return its output."
  (let* ((type (reactel-instance-type inst))
         (fn (reactel--component-function type))
         (props (reactel-element-props element))
         (name (reactel--type-name type))
         (exempt (reactel--exempt-p inst))
         (props-snapshot (and (not exempt) (reactel--strict-p 'mutation)
                              (reactel--deep-copy props)))
         (guard-buffer (and (not exempt) (reactel--strict-p 'buffer)))
         (reactel--current inst)
         (reactel--rendering-fn fn)
         (reactel--hook-acc nil)
         (reactel--hook-cursor nil)
         (reruns 0)
         result)
    (condition-case err
        (let ((buffer-read-only (or buffer-read-only guard-buffer)))
          (while
              (progn
                (setf (reactel-instance-render-update inst) nil)
                (let ((mounting (not (reactel-instance-hooks-ready inst))))
                  (setq reactel--hook-acc nil
                        reactel--hook-cursor (reactel-instance-hooks inst))
                  (setq result (apply fn props))
                  (if mounting
                      (setf (reactel-instance-hooks inst) (nreverse reactel--hook-acc)
                            (reactel-instance-hooks-ready inst) t)
                    (when reactel--hook-cursor
                      (error "Rendered fewer hooks than during the previous render (in <%s>)"
                             name))))
                (when (reactel-instance-render-update inst)
                  (when (> (cl-incf reruns) reactel--max-render-phase-updates)
                    (error "Too many re-renders: <%s> updates its state unconditionally while rendering"
                           name))
                  t)))
          (when (and (not exempt) (reactel--strict-p 'purity))
            (reactel--check-idempotent inst fn props name result)))
      (buffer-read-only
       (if guard-buffer
           (error "Component <%s> modified the buffer while rendering; side effects belong in effects or event handlers"
                  name)
         (signal (car err) (cdr err)))))
    (when (and props-snapshot (not (equal props props-snapshot)))
      (error "Component <%s> mutated its props; props are immutable" name))
    result))

;;;; Hooks

(defun reactel--basic-reducer (state action)
  "Reducer behind `reactel-use-state': ACTION is a value or an updater of STATE."
  (if (reactel--callable-p action 1) (funcall action state) action))

(defun reactel--check-state-unmutated (inst hook)
  "Signal an error if the state of HOOK changed since it was last rendered."
  (let ((snap (reactel-hook-snapshot hook)))
    (unless (or (eq snap :none) (reactel--exempt-p inst)
                (equal (reactel-hook-state hook) snap))
      (error "State of <%s> was mutated in place; state is immutable, replace it with a new value"
             (reactel--type-name (reactel-instance-type inst))))))

(defun reactel--make-dispatch (inst hook)
  "Create the dispatch function of reducer HOOK owned by INST."
  (lambda (action)
    (when (and (not (reactel-instance-dead inst)) (reactel--strict-p 'mutation))
      (reactel--check-state-unmutated inst hook))
    (cond
     ((reactel-instance-dead inst) nil)
     ((eq inst reactel--current)
      ;; Update during own render: re-run the component right away.
      (push action (reactel-hook-queue hook))
      (setf (reactel-instance-render-update inst) t))
     ((and reactel--current (reactel--strict-p 'render-update)
           (not (reactel--exempt-p reactel--current)))
      (error "Component <%s> updated the state of <%s> while rendering; update state in event handlers or effects"
             (reactel--type-name (reactel-instance-type reactel--current))
             (reactel--type-name (reactel-instance-type inst))))
     ((and (null (reactel-hook-queue hook))
           (equal-including-properties (funcall (reactel-hook-reducer hook)
                                                (reactel-hook-state hook) action)
                                       (reactel-hook-state hook)))
      nil)                              ; eager bail-out: nothing changes
     (t
      (push action (reactel-hook-queue hook))
      (reactel--mark-dirty inst)
      (reactel--schedule (reactel-instance-root inst))))
    nil))

(defun reactel-use-reducer (reducer initial-arg &optional init)
  "Hook: state managed by REDUCER, a function of (STATE ACTION).
The state starts as INITIAL-ARG, or as (funcall INIT INITIAL-ARG).
Returns a list (STATE DISPATCH); DISPATCH has a stable identity."
  (let* ((inst reactel--current)
         (hook (reactel--next-hook
                'reducer
                (lambda ()
                  (let ((h (reactel--make-hook
                            :kind 'reducer
                            :state (if init (funcall init initial-arg) initial-arg))))
                    (setf (reactel-hook-dispatch h) (reactel--make-dispatch inst h))
                    h)))))
    (setf (reactel-hook-reducer hook) reducer)
    (let ((strict (and (not (reactel--exempt-p inst))
                       (list (reactel--strict-p 'mutation) (reactel--strict-p 'purity)))))
      (when (car strict) (reactel--check-state-unmutated inst hook))
      (when (reactel-hook-queue hook)
        (let ((state (reactel-hook-state hook))
              (queue (nreverse (reactel-hook-queue hook))))
          (setf (reactel-hook-queue hook) nil)
          (dolist (action queue)
            (let ((next (funcall reducer state action)))
              (when (and (cadr strict)
                         (not (reactel--render-equal next (funcall reducer state action))))
                (error "Reducer of <%s> is not pure: the same state and action gave different results"
                       (reactel--type-name (reactel-instance-type inst))))
              (setq state next)))
          (setf (reactel-hook-state hook) state)))
      (when (car strict)
        (setf (reactel-hook-snapshot hook) (reactel--deep-copy (reactel-hook-state hook)))))
    (list (reactel-hook-state hook) (reactel-hook-dispatch hook))))

(defconst reactel-inc (lambda (n) (1+ n))
  "State updater adding one: `(funcall set-count reactel-inc)'.")
(defconst reactel-dec (lambda (n) (1- n))
  "State updater subtracting one.")
(defconst reactel-toggle (lambda (b) (not b))
  "State updater negating a boolean.")

(defun reactel-use-state (initial)
  "Hook: local state.  Returns a list (VALUE SETTER).
INITIAL is the first value, or a function object of no arguments
computing it lazily.  SETTER takes a new value, or a function object
of one argument mapping the previous value to the next (use it when the
new state depends on the old one; see `reactel-inc').  Symbols are
always plain values, even when they happen to name a function.  SETTER
is stable across renders and a no-op once the component is unmounted.
Setting a state `equal-including-properties' to the current one does
not re-render.

  (cl-destructuring-bind (count set-count) (reactel-use-state 0)
    ... (funcall set-count reactel-inc) ...)"
  (reactel-use-reducer #'reactel--basic-reducer initial
                       (lambda (x) (if (reactel--callable-p x 0) (funcall x) x))))

(defun reactel--use-effect-1 (kind effect deps)
  "Shared implementation of effect hooks of KIND."
  (unless (or (eq deps :always) (listp deps))
    (error "Effect DEPS must be a list or :always, got %S" deps))
  (let ((hook (reactel--next-hook
               kind (lambda () (reactel--make-hook :kind kind :deps :uninitialized)))))
    (when (or (eq deps :always)
              (eq (reactel-hook-deps hook) :uninitialized)
              (not (equal-including-properties deps (reactel-hook-deps hook))))
      (setf (reactel-hook-fn hook) effect
            (reactel-hook-deps hook) deps
            (reactel-hook-pending hook) t))
    nil))

(defun reactel-use-effect (effect deps)
  "Hook: run EFFECT after the component is committed.
EFFECT is a function of no arguments.  If it returns a function, that
is the cleanup, called before the next run and on unmount.  DEPS is a
list of values compared with `equal-including-properties' to the
previous render (strings differing only in text properties differ): the
effect runs only when they changed.  nil means run once on mount; the
symbol `:always' means run after every render.

Effects run after the tree is painted.  State updates made inside
are batched and re-render afterwards.

  (reactel-use-effect
   (lambda () (let ((timer (run-with-timer 1 1 #\\='tick)))
                (lambda () (cancel-timer timer))))
   nil)"
  (reactel--use-effect-1 'effect effect deps))

(defun reactel-use-layout-effect (effect deps)
  "Like `reactel-use-effect', but runs right after painting and before
passive effects, so it can read the painted result (see `:ref')."
  (reactel--use-effect-1 'layout-effect effect deps))

(defun reactel-use-memo (compute deps)
  "Hook: cache the result of calling COMPUTE until DEPS change.
DEPS as in `reactel-use-effect' (nil: compute once; `:always': never cache)."
  (let ((hook (reactel--next-hook
               'memo (lambda () (reactel--make-hook :kind 'memo :deps :uninitialized)))))
    (let ((strict-mutation (and (reactel--strict-p 'mutation)
                                (not (reactel--exempt-p reactel--current)))))
      (when (and strict-mutation (not (eq (reactel-hook-snapshot hook) :none))
                 (not (equal (reactel-hook-value hook) (reactel-hook-snapshot hook))))
        (error "A value returned by use-memo/use-callback was mutated in <%s>; hook values are immutable"
               (reactel--type-name (reactel-instance-type reactel--current))))
      (when (or (eq deps :always)
                (eq (reactel-hook-deps hook) :uninitialized)
                (not (equal-including-properties deps (reactel-hook-deps hook))))
        (let ((value (funcall compute)))
          (when (and (reactel--strict-p 'purity) (not (reactel--exempt-p reactel--current))
                     (not (reactel--render-equal value (funcall compute))))
            (error "The function passed to use-memo is not pure in <%s>: calling it twice gave different results"
                   (reactel--type-name (reactel-instance-type reactel--current))))
          (setf (reactel-hook-value hook) value
                (reactel-hook-deps hook) deps)))
      (when strict-mutation
        (setf (reactel-hook-snapshot hook) (reactel--deep-copy (reactel-hook-value hook)))))
    (reactel-hook-value hook)))

(defun reactel-use-callback (fn deps)
  "Hook: return FN, but keep returning the first function until DEPS change."
  (reactel-use-memo (lambda () fn) deps))

(cl-defstruct (reactel-ref (:constructor reactel-create-ref (&optional current))
                           (:copier nil))
  "A mutable box that survives re-renders; setting it does not re-render."
  current)

(defun reactel-use-ref (&optional initial)
  "Hook: return a stable `reactel-ref' initialised to INITIAL.
Read and write it with `reactel-ref-current'."
  (reactel-hook-value
   (reactel--next-hook
    'ref (lambda () (reactel--make-hook :kind 'ref :value (reactel-create-ref initial))))))

(defun reactel-use-context (context)
  "Hook: the value of the nearest provider of CONTEXT above, or its default.
The component re-renders when the provided value changes."
  (unless reactel--current
    (error "Hooks can only be called while a function component renders"))
  (cl-pushnew context (reactel-instance-contexts reactel--current))
  (let ((p (reactel-instance-parent reactel--current)))
    (while (and p (not (and (eq (reactel-instance-kind p) :provider)
                            (eq (reactel-instance-type p) context))))
      (setq p (reactel-instance-parent p)))
    (if p
        (plist-get (reactel-instance-props p) :value)
      (reactel-context-default context))))

(defvar reactel--id-counter 0)

(defun reactel-use-id ()
  "Hook: a unique string identifier, stable for the component's lifetime."
  (reactel-hook-value
   (reactel--next-hook
    'id (lambda () (reactel--make-hook
                    :kind 'id :value (format "reactel-%d" (cl-incf reactel--id-counter)))))))

(defun reactel-use-sync-external-store (subscribe get-snapshot)
  "Hook: read mutable state living outside of Reactel.
GET-SNAPSHOT returns the current value (compared with
`equal-including-properties').
SUBSCRIBE receives a function to call when the store changes and
returns an unsubscribe function.  Returns the current snapshot."
  (let* ((snapshot (funcall get-snapshot))
         (ref (reactel-use-ref snapshot))
         (force (cadr (reactel-use-reducer (lambda (n _) (1+ n)) 0))))
    (reactel-use-layout-effect
     (lambda () (setf (reactel-ref-current ref) snapshot) nil) :always)
    (reactel-use-effect
     (lambda ()
       (let ((check (lambda ()
                      (unless (equal-including-properties (funcall get-snapshot)
                                                          (reactel-ref-current ref))
                        (funcall force nil)))))
         (funcall check)
         (funcall subscribe check)))
     (list subscribe get-snapshot))
    snapshot))

;;;; Hook macros

;; Sugar over the hook functions.  They add no behaviour: each expands
;; to a call of the corresponding function with a lambda.

(eval-and-compile
  (defun reactel--deps-form (deps)
    "Turn the DEPS spec of a hook macro into a form building the deps value.
DEPS is `:always', nil (run once) or a list of forms to watch."
    (cond ((eq deps :always) :always)
          ((null deps) nil)
          ((proper-list-p deps) `(list ,@deps))
          (t (error "Invalid hook dependencies: %S" deps)))))

(defmacro reactel-use-effect* (deps &rest body)
  "Run BODY as an effect; see `reactel-use-effect'.
DEPS is a list of forms to watch (not a quoted list), () to run once, or
`:always'.  The value of BODY is the cleanup function, if it is a
function, so end with nil when there is none.

  (reactel-use-effect* (user-id)
    (let ((sub (subscribe user-id)))
      (lambda () (unsubscribe sub))))"
  (declare (indent 1))
  `(reactel-use-effect (lambda () ,@body) ,(reactel--deps-form deps)))

(defmacro reactel-use-layout-effect* (deps &rest body)
  "Macro form of `reactel-use-layout-effect'; DEPS as in `reactel-use-effect*'."
  (declare (indent 1))
  `(reactel-use-layout-effect (lambda () ,@body) ,(reactel--deps-form deps)))

(defmacro reactel-use-memo* (deps &rest body)
  "Cache the value of BODY until DEPS change; see `reactel-use-memo'.
DEPS as in `reactel-use-effect*'."
  (declare (indent 1))
  `(reactel-use-memo (lambda () ,@body) ,(reactel--deps-form deps)))

(defmacro reactel-use-callback* (deps arglist &rest body)
  "Return `(lambda ARGLIST BODY...)', stable until DEPS change.
DEPS as in `reactel-use-effect*'; see `reactel-use-callback'.

  (reactel-use-callback* (id) (value) (save id value))"
  (declare (indent 2))
  `(reactel-use-callback (lambda ,arglist ,@body) ,(reactel--deps-form deps)))

(defmacro reactel-let-hooks (bindings &rest body)
  "Like `let*' for hooks, destructuring list values.
Each binding is (PATTERN FORM).  A PATTERN that is a list is bound with
`cl-destructuring-bind', so the lists returned by `reactel-use-state'
and `reactel-use-reducer' unpack directly:

  (reactel-let-hooks (((count set-count) (reactel-use-state 0))
                      (ref (reactel-use-ref nil))
                      (inc (reactel-use-callback* () ()
                             (funcall set-count #\='1+))))
    ...)

Use `_' to ignore a value, e.g. for `reactel-use-effect*'.  (For richer
patterns, `-let*' from dash.el works equally well.)"
  (declare (indent 1))
  (if (null bindings)
      `(progn ,@body)
    (let ((pattern (car (car bindings)))
          (form (cadr (car bindings))))
      (if (consp pattern)
          `(cl-destructuring-bind ,pattern ,form
             (reactel-let-hooks ,(cdr bindings) ,@body))
        `(let ((,pattern ,form))
           (reactel-let-hooks ,(cdr bindings) ,@body))))))

;;;; Scheduling

(defvar reactel--batch-depth 0)
(defvar reactel--pending-roots nil)
(defvar reactel--timer nil)
(defvar reactel--flushing nil)

(defun reactel--mark-dirty (inst)
  "Flag INST as needing a render and its ancestors as having dirty children."
  (setf (reactel-instance-dirty inst) t)
  (let ((p (reactel-instance-parent inst)))
    (while p
      (setf (reactel-instance-child-dirty p) t)
      (setq p (reactel-instance-parent p)))))

(defun reactel--schedule (root)
  "Make sure ROOT is rendered soon."
  (cl-pushnew root reactel--pending-roots)
  (cond ((or (> reactel--batch-depth 0) reactel--flushing) nil)
        (reactel-synchronous (reactel-flush))
        ((null reactel--timer)
         (setq reactel--timer (run-with-timer 0 nil #'reactel--timer-flush)))))

(defun reactel--timer-flush ()
  "Timer entry point."
  (setq reactel--timer nil)
  (reactel-flush))

(defmacro reactel-batch (&rest body)
  "Run BODY, then apply all state updates it made in a single render.
Event handlers invoked by the UI run inside `reactel-batch'.  Also
usable as `reactel-act' in tests: the tree is up to date afterwards."
  (declare (indent 0))
  `(reactel--call-batched (lambda () ,@body)))

(defmacro reactel-act (&rest body)
  "Alias of `reactel-batch' (name taken from React's test utility)."
  (declare (indent 0))
  `(reactel-batch ,@body))

(defun reactel--call-batched (fn)
  "Call FN with updates batched; flush when the outermost batch ends."
  (let ((outermost (zerop reactel--batch-depth))
        (ok nil))
    (unwind-protect
        (prog1 (let ((reactel--batch-depth (1+ reactel--batch-depth)))
                 (funcall fn))
          (setq ok t))
      (when outermost
        (if ok
            (reactel-flush)
          (when (and reactel--pending-roots (null reactel--timer))
            (setq reactel--timer (run-with-timer 0 nil #'reactel--timer-flush))))))))

(defun reactel-flush ()
  "Render and commit all roots with pending updates, synchronously."
  (when (and reactel--timer (not reactel--flushing))
    (cancel-timer reactel--timer)
    (setq reactel--timer nil))
  (unless (or reactel--flushing (> reactel--batch-depth 0))
    (let ((reactel--flushing t))
      (while reactel--pending-roots
        (reactel--flush-root (pop reactel--pending-roots))))))

(defun reactel--flush-root (root)
  "Render and commit ROOT until it has no more pending work."
  (let ((inst (reactel-root-instance root))
        (commits 0)
        ;; A root may be flushed from inside the render of another one.
        (reactel--current nil) (reactel--rendering-fn nil)
        (reactel--hook-cursor nil) (reactel--hook-acc nil))
    (setf (reactel-root-busy root) t)
    (unwind-protect
        (while (or (reactel-instance-dirty inst) (reactel-instance-child-dirty inst))
          (when (> (cl-incf commits) reactel--max-update-depth)
            (setq reactel--pending-roots nil)
            (error "Maximum update depth exceeded: an effect keeps updating state"))
          (condition-case err
              (let ((reactel--effects nil) (reactel--deletions nil) (reactel--refs nil)
                    (reactel--touched nil))
                (reactel--visit inst (reactel-instance-element inst))
                (reactel--commit root (nreverse reactel--effects)
                                 (nreverse reactel--deletions) (nreverse reactel--refs)
                                 (nreverse reactel--touched)))
            (error
             ;; An uncaught render error unmounts the whole tree, as in React.
             (setq reactel--pending-roots nil)
             (ignore-errors (reactel-unmount root))
             (signal (car err) (cdr err)))))
      (setf (reactel-root-busy root) nil))))

;;;; Reconciler

(defun reactel--visit (inst element)
  "Bring INST up to date with ELEMENT, rendering only what is needed."
  (let ((old (reactel-instance-element inst)))
    (when-let ((snap (reactel-element-snap element)))
      (unless (equal (reactel-element-props element) snap)
        (error "The props of a <%s> element were mutated after it was created; values passed to elements are immutable"
               (reactel--type-name (reactel-element-type element)))))
    (setf (reactel-instance-element inst) element)
    (cond
     ((reactel--should-render-p inst old element)
      (reactel--render inst old element))
     ((reactel-instance-child-dirty inst)
      (dolist (c (reactel-instance-children inst))
        (reactel--visit c (reactel-instance-element c)))))
    (setf (reactel-instance-child-dirty inst) nil)
    inst))

(defun reactel--should-render-p (inst old new)
  "Non-nil if INST must re-render going from element OLD to NEW."
  (or (null old)
      (reactel-instance-dirty inst)
      (and (not (eq old new))
           (not (and (eq (reactel-instance-kind inst) :component)
                     (let ((cmp (reactel--memo-compare (reactel-instance-type inst))))
                       (and cmp (funcall cmp (reactel-element-props old)
                                         (reactel-element-props new)))))))))

(defun reactel--render (inst old element)
  "Render INST for ELEMENT (previously OLD, nil on mount) and reconcile children."
  (setf (reactel-instance-dirty inst) nil)
  (push inst reactel--touched)
  (pcase (reactel-instance-kind inst)
    (:text nil)
    (:boundary (reactel--render-boundary inst element))
    (kind
     (let ((child-els
            (pcase kind
              (:component
               (reactel--flatten-children (reactel--call-component inst element)))
              (:provider
               (when (and old (not (equal (plist-get (reactel-element-props old) :value)
                                          (plist-get (reactel-element-props element) :value))))
                 (reactel--propagate-context inst))
               (plist-get (reactel-element-props element) :children))
              (_ (when (eq kind :host) (reactel--note-ref inst element))
                 (plist-get (reactel-element-props element) :children)))))
       (setf (reactel-instance-children inst)
             (reactel--reconcile inst (reactel-instance-children inst) child-els))
       (when (eq kind :component) (reactel--collect-effects inst))))))

(defun reactel--note-ref (inst element)
  "Queue the ref of host INST for attachment if it changed."
  (unless (eq (reactel-element-ref element) (reactel-instance-attached-ref inst))
    (push inst reactel--refs)))

(defun reactel--collect-effects (inst)
  "Queue the pending effect hooks of INST (children were queued earlier)."
  (dolist (h (reactel-instance-hooks inst))
    (when (reactel-hook-pending h) (push h reactel--effects))))

(defun reactel--propagate-context (provider)
  "Mark consumers below PROVIDER dirty after its value changed."
  (let ((ctx (reactel-instance-type provider)))
    (cl-labels ((walk (i)
                  (when (memq ctx (reactel-instance-contexts i))
                    (reactel--mark-dirty i))
                  (unless (and (eq (reactel-instance-kind i) :provider)
                               (eq (reactel-instance-type i) ctx))
                    (mapc #'walk (reactel-instance-children i)))))
      (mapc #'walk (reactel-instance-children provider)))))

(defun reactel--check-host-type (root type)
  "Return the instance kind of TYPE, after checking ROOT's backend supports it."
  (let ((kind (reactel--kind type))
        (valid (reactel-backend-valid-type-p (reactel-root-backend root))))
    (when (and (eq kind :host) valid (not (funcall valid type)))
      (error "The %s backend does not support the element type %S"
             (reactel-backend-name (reactel-root-backend root)) type))
    kind))

(defun reactel--reconcile (parent old-children new-elements)
  "Match NEW-ELEMENTS against OLD-CHILDREN of PARENT; return the new children.
Elements with a key match the old child with an equal key, others the
old child at the same index; the types must also agree or the old
child is unmounted and a new instance created."
  (let* ((olds (vconcat old-children))
         (keyed (make-hash-table :test 'equal))
         (claimed (make-hash-table :test 'eq))
         (seen (make-hash-table :test 'equal))
         (index 0)
         result)
    (cl-loop for o across olds
             when (reactel-instance-key o) do (puthash (reactel-instance-key o) o keyed))
    (dolist (el new-elements)
      (let* ((key (reactel-element-key el))
             (cand (if key
                       (gethash key keyed)
                     (let ((o (and (< index (length olds)) (aref olds index))))
                       (and o (null (reactel-instance-key o)) o))))
             (inst (if (and cand
                            (not (gethash cand claimed))
                            (eq (reactel-instance-type cand) (reactel-element-type el)))
                       cand
                     (reactel--make-instance
                      :kind (reactel--check-host-type
                             (reactel-instance-root parent) (reactel-element-type el))
                      :type (reactel-element-type el)
                      :key key :parent parent
                      :root (reactel-instance-root parent)))))
        (when key
          (when (gethash key seen)
            (display-warning 'reactel (format "Duplicate key %S among siblings" key)))
          (puthash key t seen))
        (when (eq inst cand) (puthash cand t claimed))
        (setf (reactel-instance-key inst) key)
        (reactel--visit inst el)
        (push inst result)
        (cl-incf index)))
    (cl-loop for o across olds
             unless (gethash o claimed) do (push o reactel--deletions))
    (nreverse result)))

;;;; Error boundaries

(defun reactel--render-boundary (inst element)
  "Render error boundary INST for ELEMENT."
  (let* ((props (reactel-element-props element))
         (data (reactel-instance-data inst))
         (keys (plist-get props :reset-keys))
         (old-children (reactel-instance-children inst)))
    ;; A change of :reset-keys leaves the error state.
    (when (and data (not (equal keys (plist-get data :keys))))
      (setf data nil (reactel-instance-data inst) nil))
    (let ((saved-effects reactel--effects)
          (saved-deletions reactel--deletions)
          (saved-refs reactel--refs)
          (saved-touched reactel--touched))
      (unless data
        (condition-case err
            (setf (reactel-instance-children inst)
                  (reactel--reconcile inst old-children (plist-get props :children)))
          (error
           ;; Roll back the failed attempt and unmount what was there.
           (setq reactel--effects saved-effects
                 reactel--deletions saved-deletions
                 reactel--refs saved-refs
                 reactel--touched saved-touched)
           (dolist (c old-children) (push c reactel--deletions))
           (setf (reactel-instance-children inst) nil
                 data (list :error err :keys keys)
                 (reactel-instance-data inst) data)
           (when-let ((handler (plist-get props :on-error)))
             (push (reactel--make-hook
                    :kind 'layout-effect :pending t
                    :fn (lambda () (funcall handler err) nil))
                   reactel--effects)))))
      (when data
        (let* ((fallback (plist-get props :fallback))
               (reset (lambda ()
                        (unless (reactel-instance-dead inst)
                          (setf (reactel-instance-data inst) nil)
                          (reactel--mark-dirty inst)
                          (reactel--schedule (reactel-instance-root inst)))))
               (el (if (functionp fallback)
                       (funcall fallback (plist-get data :error) reset)
                     fallback)))
          (setf (reactel-instance-children inst)
                (reactel--reconcile inst (reactel-instance-children inst)
                                    (reactel--flatten-children el))))))))

;;;; Commit

(defun reactel--set-ref (ref value)
  "Point REF (a `reactel-ref' or function) at VALUE."
  (cond ((null ref))
        ((reactel-ref-p ref) (setf (reactel-ref-current ref) value))
        ((functionp ref) (funcall ref value))))

(defun reactel--unmount-tree (inst guard)
  "Mark INST's subtree dead and run effect cleanups, via GUARD."
  (setf (reactel-instance-dead inst) t)
  (dolist (h (reactel-instance-hooks inst))
    (when (memq (reactel-hook-kind h) '(effect layout-effect))
      (let ((cleanup (reactel-hook-cleanup h)))
        (setf (reactel-hook-cleanup h) nil (reactel-hook-pending h) nil)
        (when cleanup (funcall guard cleanup)))))
  (when (reactel-instance-attached-ref inst)
    (let ((ref (reactel-instance-attached-ref inst)))
      (setf (reactel-instance-attached-ref inst) nil)
      (funcall guard (lambda () (reactel--set-ref ref nil)))))
  (dolist (c (reactel-instance-children inst))
    (reactel--unmount-tree c guard)))

;;;; Host sync

(defun reactel--host-kind-p (inst)
  "Non-nil if INST is a host element or text, that is, has a backend node."
  (memq (reactel-instance-kind inst) '(:host :text)))

(defun reactel--host-ancestor (inst)
  "The nearest host instance above INST, or nil if it is at the top level."
  (let ((p (reactel-instance-parent inst)))
    (while (and p (not (eq (reactel-instance-kind p) :host)))
      (setq p (reactel-instance-parent p)))
    p))

(defun reactel--host-descendants (inst)
  "The host instances directly below INST once components are looked through."
  (cl-loop for c in (reactel-instance-children inst)
           append (if (reactel--host-kind-p c) (list c) (reactel--host-descendants c))))

(defun reactel--host-props (props)
  "PROPS without `:children', as the backend sees them."
  (let (out)
    (cl-loop for (k v) on props by #'cddr
             unless (eq k :children) do (setq out (nconc out (list k v))))
    out))

(defun reactel--sync-node (backend inst)
  "Create or update the backend node of host INST."
  (let ((props (reactel--host-props (reactel-instance-props inst)))
        (node (reactel-instance-node inst))
        (type (if (eq (reactel-instance-kind inst) :text) :string (reactel-instance-type inst))))
    (if (null node)
        (setf (reactel-instance-node inst) (funcall (reactel-backend-create-node backend) type props)
              (reactel-instance-node-props inst) props)
      (let ((old (reactel-instance-node-props inst)))
        (unless (reactel-props-equal old props)
          (funcall (reactel-backend-update-node backend) node type old props)
          (setf (reactel-instance-node-props inst) props))))))

(defun reactel--same-nodes-p (a b)
  "Non-nil if the node lists A and B hold the same nodes in the same order."
  (and (= (length a) (length b)) (cl-every #'eq a b)))

(defun reactel--sync-children (backend inst)
  "Give the node of host INST the children its subtree now has."
  (let ((nodes (mapcar #'reactel-instance-node (reactel--host-descendants inst))))
    (unless (reactel--same-nodes-p nodes (reactel-instance-host-nodes inst))
      (funcall (reactel-backend-set-children backend) (reactel-instance-node inst) nodes)
      (setf (reactel-instance-host-nodes inst) nodes))))

(defun reactel--destroy-tree (backend inst)
  "Destroy the backend nodes in INST's subtree, children first."
  (dolist (c (reactel-instance-children inst)) (reactel--destroy-tree backend c))
  (when-let ((node (reactel-instance-node inst)))
    (setf (reactel-instance-node inst) nil (reactel-instance-host-nodes inst) nil)
    (when-let ((f (reactel-backend-destroy-node backend))) (funcall f node))))

(defun reactel--sync-host (root touched deletions)
  "Apply the changes of one render pass of ROOT to its backend.
TOUCHED are the instances rendered, DELETIONS the removed subtrees."
  (let ((backend (reactel-root-backend root))
        (inst (reactel-root-instance root))
        dirty root-dirty)
    (cl-flet ((mark-parent (i)
                (if-let ((hp (reactel--host-ancestor i)))
                    (cl-pushnew hp dirty)
                  (setq root-dirty t))))
      (dolist (d deletions)
        (mark-parent d)
        (reactel--destroy-tree backend d))
      (dolist (i touched)
        (unless (reactel-instance-dead i)
          (when (reactel--host-kind-p i) (reactel--sync-node backend i))
          (when (eq (reactel-instance-kind i) :host) (cl-pushnew i dirty))
          (mark-parent i)))
      (dolist (h dirty)
        (unless (reactel-instance-dead h) (reactel--sync-children backend h)))
      (when root-dirty
        (let ((nodes (mapcar #'reactel-instance-node (reactel--host-descendants inst))))
          (unless (reactel--same-nodes-p nodes (reactel-root-nodes root))
            (funcall (reactel-backend-set-children backend) (reactel-root-container root) nodes)
            (setf (reactel-root-nodes root) nodes))))
      (when (reactel-backend-finish-commit backend)
        (funcall (reactel-backend-finish-commit backend) (reactel-root-container root))))))

(defun reactel--commit (root effects deletions refs touched)
  "Apply a finished render of ROOT.
EFFECTS, DELETIONS, REFS and TOUCHED were collected during rendering."
  (let (first-error)
    (cl-flet ((guard (f)
                (condition-case err (funcall f)
                  (error (unless first-error (setq first-error err))))))
      (dolist (d deletions) (reactel--unmount-tree d #'guard))
      (let ((layout (cl-remove-if-not (lambda (h) (eq (reactel-hook-kind h) 'layout-effect))
                                      effects))
            (passive (cl-remove-if-not (lambda (h) (eq (reactel-hook-kind h) 'effect))
                                       effects)))
        (dolist (h layout) (reactel--run-cleanup h #'guard))
        (guard (lambda () (reactel--sync-host root touched deletions)))
        ;; Detach every old ref before attaching new ones, so a ref that
        ;; moves between elements is not cleared after being set.
        (dolist (i refs)
          (unless (reactel-instance-dead i)
            (let ((old (reactel-instance-attached-ref i)))
              (setf (reactel-instance-attached-ref i) nil)
              (guard (lambda () (reactel--set-ref old nil))))))
        (dolist (i refs)
          (unless (reactel-instance-dead i)
            (let ((new (reactel-element-ref (reactel-instance-element i))))
              (setf (reactel-instance-attached-ref i) new)
              (guard (lambda () (reactel--set-ref new (reactel-instance-node i)))))))
        (dolist (h layout) (reactel--run-effect h #'guard))
        (dolist (h passive) (reactel--run-cleanup h #'guard))
        (dolist (h passive) (reactel--run-effect h #'guard))))
    (when first-error (signal (car first-error) (cdr first-error)))))

(defun reactel--run-cleanup (hook guard)
  "Run and clear the cleanup of effect HOOK via GUARD."
  (let ((cleanup (reactel-hook-cleanup hook)))
    (setf (reactel-hook-cleanup hook) nil)
    (when cleanup (funcall guard cleanup))))

(defun reactel--run-effect (hook guard)
  "Run the pending effect of HOOK via GUARD and keep its cleanup."
  (when (reactel-hook-pending hook)
    (setf (reactel-hook-pending hook) nil)
    (funcall guard
             (lambda ()
               (let ((result (funcall (reactel-hook-fn hook))))
                 (when (and (not (reactel-hook-mounted hook)) (reactel--strict-p 'effects))
                   ;; Strict mode: mount, clean up, mount again, to expose
                   ;; effects whose cleanup does not undo them.
                   (when (and result (functionp result)) (funcall result))
                   (setq result (funcall (reactel-hook-fn hook))))
                 (setf (reactel-hook-mounted hook) t)
                 (when (and result (functionp result))
                   (setf (reactel-hook-cleanup hook) result)))))))

;;;; Strict mode

(defconst reactel-strict--impure-functions
  '(run-with-timer run-at-time run-with-idle-timer make-process start-process
                   add-hook remove-hook)
  "Functions that must not be called while rendering when `impure' is checked.")

(defun reactel-strict--guard (&rest _)
  "Signal an error if called while a component renders."
  (when (and reactel--current (reactel--strict-p 'impure)
             (not (reactel--exempt-p reactel--current)))
    (error "<%s> has a side effect while rendering; start timers, processes and hooks in an effect"
           (reactel--type-name (reactel-instance-type reactel--current)))))

;;;###autoload
(define-minor-mode reactel-strict-mode
  "Enable all runtime checks of the Rules of React (see `reactel-strict')."
  :global t
  :group 'reactel
  (if reactel-strict-mode
      (progn (setq reactel-strict t)
             (dolist (f reactel-strict--impure-functions)
               (advice-add f :before #'reactel-strict--guard)))
    (setq reactel-strict nil)
    (dolist (f reactel-strict--impure-functions)
      (advice-remove f #'reactel-strict--guard))))

(provide 'reactel)
;;; reactel.el ends here
