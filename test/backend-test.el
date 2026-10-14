;;; backend-test.el --- Tests for the render backend protocol -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'reactel)
(require 'reactel-c)
(require 'reactel-node)
(require 'reactel-buffer)
(require 'reactel-markdown)

;;; A recording backend: nodes are plain conses (ID TYPE PROPS CHILDREN)

(defvar backend-log nil "Operations seen by the recording backend, oldest first.")
(defvar backend-ids 0)

(defun backend-recording (&optional valid-p)
  "A backend that logs every call it receives into `backend-log'."
  (reactel-make-backend
   :name 'recording
   :valid-type-p valid-p
   :create-container (lambda () (list 'container :root nil nil))
   :create-node (lambda (type props)
                  (let ((n (list (cl-incf backend-ids) type props nil)))
                    (setq backend-log (append backend-log (list (list 'create type props))))
                    n))
   :update-node (lambda (node type old new)
                  (setf (nth 2 node) new)
                  (setq backend-log (append backend-log (list (list 'update type old new)))))
   :set-children (lambda (node children)
                   (setf (nth 3 node) children)
                   (setq backend-log (append backend-log
                                             (list (list 'children (nth 1 node)
                                                         (mapcar (lambda (c) (nth 0 c)) children))))))
   :destroy-node (lambda (node)
                   (setq backend-log (append backend-log (list (list 'destroy (nth 1 node)))))) 
   :finish-commit (lambda (_container)
                    (setq backend-log (append backend-log (list '(finish)))))))

(defmacro with-recording (var element &rest body)
  "Mount ELEMENT on a recording backend as VAR, run BODY, then unmount."
  (declare (indent 2))
  `(let* ((backend-log nil) (backend-ids 0)
          (,var (reactel-create-root (backend-recording))))
     (unwind-protect (progn (reactel-root-render ,var ,element) ,@body)
       (ignore-errors (reactel-unmount ,var)))))

(defun backend-ops (kind)
  (cl-remove-if-not (lambda (op) (eq (car op) kind)) backend-log))

(defun backend-reset () (setq backend-log nil))

;;; Protocol

(ert-deftest backend-initial-render-creates-nodes-and-finishes-once ()
  (with-recording r (reactel-c-vstack (reactel-c-button :on-click #'ignore "go") "hi")
    (should (equal (mapcar #'cadr (backend-ops 'create)) '(:vstack :button :string :string)))
    (should (= (length (backend-ops 'finish)) 1))
    ;; props never contain children, key or ref
    (let ((props (nth 2 (cl-find-if (lambda (op) (eq (cadr op) :button)) (backend-ops 'create)))))
      (should (equal (plist-get props :on-click) #'ignore))
      (should-not (plist-member props :children)))
    ;; the container receives the top-level node
    (should (equal (car (last (backend-ops 'children))) '(children :root (1))))))

(ert-deftest backend-state-update-only-updates-the-changed-node ()
  (reactel-defcomponent backend-test-counter ()
    :compile nil                        ; compiled, fewer updates are needed
    (cl-destructuring-bind (n set-n) (reactel-use-state 0)
      (reactel-c-vstack (reactel-c-text "fixed")
        (reactel-c-button :on-click (lambda () (funcall set-n reactel-inc))
          (format "n=%d" n)))))
  (with-recording r (reactel-el 'backend-test-counter)
    (backend-reset)
    (reactel-click-via-props r "n=0")
    (should (equal (mapcar #'car backend-log) '(update update finish)))
    (should (equal (mapcar #'cadr (backend-ops 'update)) '(:button :string)))
    (should-not (backend-ops 'create))
    (should-not (backend-ops 'destroy))
    (should-not (backend-ops 'children))))

(defun reactel-click-via-props (root label)
  "Find the handler of the button with text LABEL in the recording ROOT and call it."
  (let (found)
    (cl-labels ((walk (n) (when (and (eq (nth 1 n) :button)
                                     (equal (nth 1 (car (nth 3 n))) :string)
                                     (equal (plist-get (nth 2 (car (nth 3 n))) :value) label))
                            (setq found (plist-get (nth 2 n) :on-click)))
                  (dolist (c (nth 3 n)) (walk c))))
      (walk (reactel-root-container root)))
    (unless found (error "no button %S" label))
    (reactel-batch (funcall found))))

(ert-deftest backend-keyed-list-changes-are-minimal ()
  (reactel-defcomponent backend-test-item (&key name) (reactel-c-text name))
  (cl-flet ((tree (names)
              (reactel-c-vstack (mapcar (lambda (n) (reactel-el 'backend-test-item :key n :name n))
                                        names))))
    (with-recording r (tree '("a" "b" "c"))
      ;; reorder: same nodes, only the parent's child list changes
      (backend-reset)
      (reactel-root-render r (tree '("c" "b" "a")))
      (should-not (backend-ops 'create))
      (should-not (backend-ops 'destroy))
      (should (= (length (backend-ops 'children)) 1))
      ;; remove one, add one
      (backend-reset)
      (reactel-root-render r (tree '("c" "a" "d")))
      (should (= (length (backend-ops 'create)) 2))      ; "d" text host + its string... see below
      (should (= (length (backend-ops 'destroy)) 2)))))  ; "b" and its string

(ert-deftest backend-unmount-destroys-everything-children-first ()
  (with-recording r (reactel-c-vstack "a" (reactel-c-text "b"))
    (backend-reset)
    (reactel-unmount r)
    (should (equal (mapcar #'cadr (backend-ops 'destroy)) '(:string :string :text :vstack)))
    (should (equal (car (backend-ops 'children)) '(children :root nil)))
    (should (equal (car (last backend-log)) '(finish)))))

(ert-deftest backend-unsupported-types-are-reported ()
  (let ((r (reactel-create-root (backend-recording (lambda (type) (memq type '(:text)))))))
    (reactel-root-render r (reactel-c-text "ok"))
    (let ((msg (cadr (should-error (reactel-root-render r (reactel-c-vstack "x"))))))
      (should (string-match-p "recording backend does not support the element type :vstack" msg)))))

(ert-deftest backend-refs-receive-the-nodes ()
  (let ((ref (reactel-create-ref)))
    (with-recording r (reactel-c-text :ref ref "x")
      (should (consp (reactel-ref-current ref)))
      (should (eq (nth 1 (reactel-ref-current ref)) :text))
      (reactel-root-render r nil)
      (should-not (reactel-ref-current ref)))))

(ert-deftest backend-invalid-backends-are-rejected ()
  (should-error (reactel-create-root (reactel-make-backend :name 'broken)))
  (should-error (reactel-create-root 'not-a-backend)))

;;; The generic node-tree backend

(ert-deftest backend-node-tree-snapshots ()
  (let ((r (reactel-node-test-root)))
    (reactel-root-render r (reactel-c-vstack "a" (reactel-c-button :on-click #'ignore :disabled t "go")))
    (should (equal (reactel-node-root-snapshot r)
                   '((:vstack nil "a" (:button (:disabled t) "go")))))
    (reactel-unmount r)
    (should-not (reactel-node-root-snapshot r))))

(ert-deftest backend-node-tree-keeps-parent-links ()
  (let ((r (reactel-node-test-root)))
    (reactel-root-render r (reactel-c-vstack (reactel-c-text "x")))
    (let* ((container (reactel-root-container r))
           (vstack (car (reactel-node-children container)))
           (text (car (reactel-node-children vstack))))
      (should (eq (reactel-node-parent text) vstack))
      (should (eq (reactel-node-root text) container))
      (should (equal (reactel-node-text container) "x")))
    (reactel-unmount r)))

(ert-deftest backend-click-and-type-work-on-any-node-backend ()
  (reactel-defcomponent backend-test-form ()
    (cl-destructuring-bind (n set-n) (reactel-use-state 0)
      (cl-destructuring-bind (s set-s) (reactel-use-state "")
        (reactel-c-vstack
          (reactel-c-button :on-click (lambda () (funcall set-n reactel-inc)) "inc")
          (reactel-c-field :label "Name" :value s :on-change set-s)
          (format "n=%d s=%s" n s)))))
  (let ((r (reactel-node-test-root)))
    (reactel-root-render r (reactel-el 'backend-test-form))
    (reactel-click r "inc")
    (reactel-type r "Name" "Ada")
    (should (string-match-p "n=1 s=Ada" (reactel-node-text (reactel-root-container r))))
    (reactel-unmount r)))

;;; Two backends, one component

(reactel-defcomponent backend-test-card (&key title items)
  (reactel-c-vstack
    (reactel-c-heading :level 2 title)
    (reactel-c-list (mapcar (lambda (i) (reactel-c-text :face (and (cdr i) 'bold) (car i))) items))
    (reactel-c-table :align '(left right)
      (reactel-c-row "Item" "Qty")
      (mapcar (lambda (i) (reactel-c-row (car i) (number-to-string (length (car i))))) items))
    (reactel-c-link :url "https://example.org" "docs")))

(ert-deftest backend-same-component-renders-on-buffer-and-markdown ()
  (let ((el (reactel-el 'backend-test-card :title "Stock" :items '(("apple" . t) ("fig")))))
    (should (equal (reactel-markdown-render el)
                   (concat "## Stock\n\n"
                           "- **apple**\n- fig\n\n"
                           "| Item | Qty |\n| --- | ---: |\n| apple | 5 |\n| fig | 3 |\n\n"
                           "[docs](https://example.org)\n")))
    (let ((text (substring-no-properties (reactel-render-to-string el))))
      (should (string-match-p "^Stock$" text))
      (should (string-match-p "^• apple$" text))
      (should (string-match-p "Item  │ Qty" text)))))

(ert-deftest backend-markdown-interactive-root-updates ()
  (reactel-defcomponent backend-test-md-counter ()
    (cl-destructuring-bind (n set-n) (reactel-use-state 0)
      (reactel-c-vstack (reactel-c-heading (format "Count %d" n))
        (reactel-c-button :on-click (lambda () (funcall set-n reactel-inc)) "inc"))))
  (let ((r (reactel-markdown-root)))
    (reactel-root-render r (reactel-el 'backend-test-md-counter))
    (should (equal (reactel-markdown-output r) "# Count 0\n\n[inc]\n"))
    (reactel-click r "inc")
    (should (equal (reactel-markdown-output r) "# Count 1\n\n[inc]\n"))
    (reactel-unmount r)
    (should (equal (reactel-markdown-output r) ""))))

(ert-deftest backend-markdown-blocks ()
  (should (equal (reactel-markdown-render
                  (reactel-c-box :title "Note" (reactel-c-vstack "one" "two")))
                 "> **Note**\n>\n> one\n>\n> two\n"))
  (should (equal (reactel-markdown-render
                  (reactel-c-list :ordered t "a" (reactel-c-vstack "b" "c")))
                 "1. a\n2. b\n\n   c\n"))
  (should (equal (reactel-markdown-render
                  (reactel-c-vstack (reactel-c-separator)
                    (reactel-c-hstack "x" (reactel-c-checkbox :checked t "ok")
                      (reactel-c-field :value "v"))))
                 "---\n\nx [x] ok `v`\n"))
  (should (equal (reactel-markdown-render
                  (reactel-c-table :header nil (reactel-c-row "a" "b")))
                 "|  |  |\n| --- | --- |\n| a | b |\n")))

;;; Rendering into another root

(ert-deftest backend-render-into-another-root-from-a-batch-and-from-a-render ()
  ;; A root renders synchronously even inside `reactel-batch' (an event
  ;; handler) and inside the render of another root.
  (should (equal (reactel-batch (reactel-markdown-render (reactel-c-text "hi"))) "hi\n"))
  (reactel-defcomponent backend-test-nested ()
    (reactel-c-text (string-trim (reactel-markdown-render (reactel-c-heading "inner")))))
  (let ((r (reactel-markdown-root)))
    (reactel-root-render r (reactel-el 'backend-test-nested))
    (should (equal (reactel-markdown-output r) "# inner\n"))
    (reactel-unmount r)))

(ert-deftest backend-nested-root-with-effects-and-state ()
  (reactel-defcomponent backend-test-inner ()
    (cl-destructuring-bind (n set-n) (reactel-use-state 0)
      (reactel-use-effect* () (funcall set-n 5) nil)
      (reactel-c-text (format "n=%d" n))))
  (reactel-defcomponent backend-test-outer ()
    (cl-destructuring-bind (s _) (reactel-use-state "")
      (reactel-c-text (concat s (string-trim
                                 (reactel-markdown-render (reactel-el 'backend-test-inner)))))))
  (let ((reactel-strict '(render-update mutation))
        (r (reactel-markdown-root)))
    (reactel-root-render r (reactel-el 'backend-test-outer))
    (should (equal (reactel-markdown-output r) "n=5\n"))
    (reactel-unmount r)))

(ert-deftest backend-rendering-into-a-busy-root-is-an-error ()
  (let (root)
    (reactel-defcomponent backend-test-reentrant ()
      (reactel-root-render root (reactel-c-text "again"))
      (reactel-c-text "x"))
    (setq root (reactel-markdown-root))
    (should (string-match-p "while it is rendering"
                            (cadr (should-error (reactel-root-render
                                                 root (reactel-el 'backend-test-reentrant))))))
    (should-not (reactel-root-busy root))))

(provide 'backend-test)
;;; backend-test.el ends here
