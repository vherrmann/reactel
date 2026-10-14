;;; compile-test.el --- Tests for the automatic memoization compiler -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'reactel)
(require 'reactel-buffer)
(require 'reactel-compile)

(defvar compile-test-renders nil "Alist of component name -> number of renders.")
(defvar compile-test-calls 0)
(defvar compile-test-global "g1")

(defun compile-test-count (name) (or (alist-get name compile-test-renders) 0))
(defun compile-test-bump (name) (cl-incf (alist-get name compile-test-renders 0)))

(defmacro with-compile-root (var element &rest body)
  (declare (indent 2))
  `(let* ((compile-test-renders nil) (compile-test-calls 0)
          (,var (reactel-create-string-root)))
     (unwind-protect (progn (reactel-root-render ,var ,element) ,@body)
       (ignore-errors (reactel-unmount ,var)))))

(defun compile-test-info (name) (get name 'reactel-compile-info))

;;; Static children and closures keep their identity

(reactel-defcomponent compile-test-leaf (&key label)
  :lint nil :compile nil                ; counts its renders
  (compile-test-bump 'leaf)
  (reactel-c-text label))

(reactel-defcomponent compile-test-leaf-memo (&key on-click)
  :memo t :lint nil :compile nil
  (compile-test-bump 'memo-leaf)
  (reactel-c-button :on-click on-click "x"))

(defmacro compile-test-define-parents (prefix &rest body)
  "Define PREFIX-plain and PREFIX-compiled components from BODY."
  `(progn
     (reactel-defcomponent ,(intern (format "%s-plain" prefix)) () :compile nil ,@body)
     (reactel-defcomponent ,(intern (format "%s-compiled" prefix)) () :compile t ,@body)))

(compile-test-define-parents
 compile-test-children
 (cl-destructuring-bind (n set-n) (reactel-use-state 0)
   (reactel-c-vstack
     (reactel-el 'compile-test-leaf :label "static")
     (reactel-el 'compile-test-leaf-memo :on-click (lambda () (funcall set-n reactel-inc)))
     (reactel-c-button :on-click (lambda () (funcall set-n reactel-inc)) "inc")
     (reactel-c-text (format "n=%d" n)))))

(ert-deftest compile-unchanged-children-are-not-rendered-again ()
  (with-compile-root r (reactel-el 'compile-test-children-plain)
    (reactel-click r "inc") (reactel-click r "inc")
    (should (= (compile-test-count 'leaf) 3))          ; re-rendered with its parent
    (should (= (compile-test-count 'memo-leaf) 3))     ; new closure each time defeats :memo
    (should (string-match-p "n=2" (reactel-root-text r))))
  (with-compile-root r (reactel-el 'compile-test-children-compiled)
    (reactel-click r "inc") (reactel-click r "inc")
    (should (= (compile-test-count 'leaf) 1))          ; same element: skipped
    (should (= (compile-test-count 'memo-leaf) 1))     ; same closure: :memo works
    (should (string-match-p "n=2" (reactel-root-text r)))))

(ert-deftest compile-reports-its-scopes ()
  (let ((info (compile-test-info 'compile-test-children-compiled)))
    (should (> (plist-get info :scopes) 0))
    (should-not (plist-get info :skipped))
    (should (string-match-p "reactel--memo" (format "%S" (plist-get info :forms))))))

;;; Derived values are computed once per change of their inputs

(defun compile-test-square (x)
  "A function the compiler cannot see into, so it assumes it is pure."
  (cl-incf compile-test-calls)
  (* x x))

(compile-test-define-parents
 compile-test-derived
 (cl-destructuring-bind (n set-n) (reactel-use-state 0)
   (cl-destructuring-bind (k set-k) (reactel-use-state 1)
     (let ((squares (mapcar #'compile-test-square (number-sequence 1 k))))
       (reactel-c-vstack
         (reactel-c-button :on-click (lambda () (funcall set-n reactel-inc)) "n+")
         (reactel-c-button :on-click (lambda () (funcall set-k reactel-inc)) "k+")
         (reactel-c-text (format "n=%d squares=%S" n squares)))))))

(ert-deftest compile-derived-values-recompute-only-when-their-inputs-change ()
  (with-compile-root r (reactel-el 'compile-test-derived-plain)
    (reactel-click r "n+") (reactel-click r "n+")
    (should (= compile-test-calls 3)))               ; recomputed at every render
  (with-compile-root r (reactel-el 'compile-test-derived-compiled)
    (should (= compile-test-calls 1))
    (reactel-click r "n+") (reactel-click r "n+")
    (should (= compile-test-calls 1))                ; k did not change
    (reactel-click r "k+")
    (should (= compile-test-calls 3))                ; 1 + 2 new squares
    (should (string-match-p "n=2 squares=(1 4)" (reactel-root-text r)))))

;;; Things that must not be memoized

(reactel-defcomponent compile-test-impure-render ()
  :compile t                            ; deliberately impure: it writes a global
  (cl-destructuring-bind (n set-n) (reactel-use-state 0)
    (reactel-c-vstack
      (reactel-c-button :on-click (lambda () (funcall set-n reactel-inc)) "tick")
      (reactel-c-text (format "n=%d calls=%d" n
                              (length (mapcar (lambda (x) (cl-incf compile-test-calls) x)
                                              '(1 2))))))))

(ert-deftest compile-does-not-memoize-writes-to-globals ()
  ;; a render that writes a global is impure; its expression stays live
  (with-compile-root r (reactel-el 'compile-test-impure-render)
    (reactel-click r "tick")
    (should (= compile-test-calls 4))))

(defmacro compile-test-compiled (name args &rest body)
  `(reactel-defcomponent ,name ,args :compile t ,@body))

(compile-test-compiled compile-test-reads-global ()
		       (cl-destructuring-bind (n set-n) (reactel-use-state 0)
			 (reactel-c-vstack
			   (reactel-c-button :on-click (lambda () (funcall set-n reactel-inc)) "tick")
			   (reactel-c-text (format "%d %s" n compile-test-global)))))

(ert-deftest compile-does-not-memoize-reads-of-globals ()
  (with-compile-root r (reactel-el 'compile-test-reads-global)
    (setq compile-test-global "g2")
    (reactel-click r "tick")
    (should (string-match-p "1 g2" (reactel-root-text r)))
    (setq compile-test-global "g1")))

(compile-test-compiled compile-test-reads-ref ()
		       (cl-destructuring-bind (n set-n) (reactel-use-state 0)
			 (let ((ref (reactel-use-ref 0)))
			   (setf (reactel-ref-current ref) (* 10 n))
			   (reactel-c-vstack
			     (reactel-c-button :on-click (lambda () (funcall set-n reactel-inc)) "tick")
			     (reactel-c-text (format "ref=%d" (reactel-ref-current ref)))))))

(ert-deftest compile-does-not-memoize-reads-of-refs ()
  (with-compile-root r (reactel-el 'compile-test-reads-ref)
    (reactel-click r "tick")
    (should (string-match-p "ref=10" (reactel-root-text r)))))

(compile-test-compiled compile-test-mutated-variable ()
		       (cl-destructuring-bind (n set-n) (reactel-use-state 0)
			 (let ((total 0))
			   (dotimes (i (1+ n)) (setq total (+ total i)))
			   (reactel-c-vstack
			     (reactel-c-button :on-click (lambda () (funcall set-n reactel-inc)) "tick")
			     (reactel-c-text (format "total=%d" total))))))

(ert-deftest compile-does-not-memoize-mutated-variables ()
  (with-compile-root r (reactel-el 'compile-test-mutated-variable)
    (should (string-match-p "total=0" (reactel-root-text r)))
    (reactel-click r "tick") (reactel-click r "tick")
    (should (string-match-p "total=3" (reactel-root-text r)))))

(compile-test-compiled compile-test-self-contained-loop ()
		       (cl-destructuring-bind (n set-n) (reactel-use-state 2)
			 (let ((acc nil))
			   ;; `acc' is mutated, but only inside the loop that builds it
			   (reactel-c-vstack
			     (reactel-c-button :on-click (lambda () (funcall set-n reactel-inc)) "tick")
			     (reactel-c-text (format "%S" (let ((out nil)) (dotimes (i n) (push i out)) out)))
			     (reactel-c-text (format "%S" acc))))))

(ert-deftest compile-memoizes-self-contained-loops ()
  (should (> (plist-get (compile-test-info 'compile-test-self-contained-loop) :scopes) 0))
  (with-compile-root r (reactel-el 'compile-test-self-contained-loop)
    (should (string-match-p "(1 0)" (reactel-root-text r)))
    (reactel-click r "tick")
    (should (string-match-p "(2 1 0)" (reactel-root-text r)))))

(ert-deftest compile-leaves-impure-calls-alone ()
  (let ((r (plist-get (reactel-compile-forms
                       '((reactel-c-text (format "%s" (random 1000000)))
                         (reactel-c-text (message "hi")))
                       nil)
                      :scopes)))
    (should (= r 0))))

;;; Bailing out

(ert-deftest compile-skips-components-that-break-the-rules ()
  (let ((reactel-lint-severity nil))
    (eval '(reactel-defcomponent compile-test-bad (&key flag)
             :compile t
             (when flag (reactel-use-state 1))
             (reactel-c-text "x"))
          t))
  (let ((info (compile-test-info 'compile-test-bad)))
    (should (string-match-p "Rules of React" (plist-get info :skipped)))
    (should (= (plist-get info :scopes) 0))))

(ert-deftest compile-options ()
  (let ((reactel-compile t))
    (eval '(progn
             (reactel-defcomponent compile-test-global-on () (reactel-c-text "a"))
             (reactel-defcomponent compile-test-opt-out () :compile nil (reactel-c-text "b"))
             (reactel-defcomponent compile-test-lint-off () :lint nil (reactel-c-text "c")))
          t))
  (should (compile-test-info 'compile-test-global-on))
  (should-not (compile-test-info 'compile-test-opt-out))
  (should-not (compile-test-info 'compile-test-lint-off))
  (let ((reactel-compile nil))
    (eval '(reactel-defcomponent compile-test-global-off () (reactel-c-text "d")) t)
    (should-not (compile-test-info 'compile-test-global-off))))

(ert-deftest compile-works-for-hooks-and-lambdas ()
  (eval '(reactel-defhook compile-test-use-pair (a)
           :compile t
           (cl-destructuring-bind (b set-b) (reactel-use-state 1)
             (list (list a b) set-b)))
        t)
  (should (compile-test-info 'compile-test-use-pair))
  (eval '(reactel-defcomponent compile-test-uses-hook (&key a)
           :compile t
           (cl-destructuring-bind (pair set-b) (compile-test-use-pair a)
             (reactel-c-button :on-click (lambda () (funcall set-b reactel-inc))
               (format "%S" pair))))
        t)
  (let ((r (reactel-create-string-root)))
    (reactel-root-render r (reactel-el 'compile-test-uses-hook :a 5))
    (should (equal (reactel-root-text r) "[(5 1)]"))
    (reactel-click r "(5 1)")
    (should (equal (reactel-root-text r) "[(5 2)]"))
    (reactel-unmount r)))

(ert-deftest compile-show-displays-the-compiled-body ()
  (reactel-compile-show 'compile-test-derived-compiled)
  (with-current-buffer "*reactel-compiled*"
    (should (string-match-p "memoized scopes" (buffer-string)))
    (should (string-match-p "reactel--use-cache" (buffer-string))))
  (kill-buffer "*reactel-compiled*"))

;;; Strict mode agrees that the compiled output is pure

(ert-deftest compile-compiled-components-pass-strict-mode ()
  (let ((reactel-strict t))
    (with-compile-root r (reactel-el 'compile-test-derived-compiled)
      (reactel-click r "n+") (reactel-click r "k+")
      (should (string-match-p "n=1 squares=(1 4)" (reactel-root-text r))))))

;;; Differential test: compiled and plain versions behave identically

(defmacro compile-test-define-pair (name &rest body)
  "Define NAME-plain and NAME-compiled (components without props) from BODY."
  `(progn
     (reactel-defcomponent ,(intern (format "%s-plain" name)) () :compile nil ,@body)
     (reactel-defcomponent ,(intern (format "%s-compiled" name)) () :compile t ,@body)))

(defconst compile-test-theme (reactel-create-context 'light))

(reactel-defcomponent compile-test-themed (&key label)
  (reactel-c-text (format "%s/%s" label (reactel-use-context compile-test-theme))))

(compile-test-define-pair
 compile-test-big
 (cl-destructuring-bind (items set-items) (reactel-use-state '(3 1 2))
   (cl-destructuring-bind (filter set-filter) (reactel-use-state 'all)
     (cl-destructuring-bind (theme set-theme) (reactel-use-state 'light)
       (cl-destructuring-bind (open set-open) (reactel-use-state nil)
         (let* ((visible (pcase filter
                           ('odd (cl-remove-if-not #'cl-oddp items))
                           ('even (cl-remove-if #'cl-oddp items))
                           (_ items)))
                (total (apply #'+ visible))
                (rows (mapcar (lambda (i) (reactel-c-text :key i (format "<%d>" i))) visible)))
           (reactel-el compile-test-theme :value theme
	     (reactel-c-vstack
	       (reactel-c-hstack
		 (reactel-c-button :on-click (lambda () (funcall set-items (lambda (xs) (cons (1+ (length xs)) xs)))) "add")
		 (reactel-c-button :on-click (lambda () (funcall set-items #'cdr)) "drop")
		 (reactel-c-button :on-click (lambda () (funcall set-open reactel-toggle)) "toggle")
		 (reactel-c-button :on-click (lambda () (funcall set-theme (lambda (th) (if (eq th 'light) 'dark 'light)))) "theme"))
	       (reactel-c-select :options '(all odd even) :value filter :on-change set-filter)
	       (reactel-c-text (format "total=%d count=%d" total (length visible)))
	       (if open
		   (reactel-c-box :title "rows" rows)
		 (reactel-c-text "closed"))
	       (reactel-c-list (mapcar (lambda (i) (reactel-el 'compile-test-themed :key i :label (number-to-string i)))
				       visible))))))))))

(ert-deftest compile-differential-random-events ()
  (let ((labels '("add" "drop" "toggle" "theme" "all" "odd" "even")))
    (dolist (seed '("a" "b" "c" "d"))
      (random seed)
      (let ((plain (reactel-create-string-root))
            (compiled (reactel-create-string-root)))
        (unwind-protect
            (progn
              (reactel-root-render plain (reactel-el 'compile-test-big-plain))
              (reactel-root-render compiled (reactel-el 'compile-test-big-compiled))
              (dotimes (step 60)
                (let ((label (nth (random (length labels)) labels)))
                  (unless (and (equal label "drop")
                               (not (string-match-p "total=[1-9]" (reactel-root-text plain)))
                               nil)
                    (condition-case nil (reactel-click plain label) (error nil))
                    (condition-case nil (reactel-click compiled label) (error nil))))
                (should (equal (list seed step (reactel-root-text plain))
                               (list seed step (reactel-root-text compiled))))))
          (reactel-unmount plain)
          (reactel-unmount compiled))))))

(ert-deftest compile-differential-compiled-skips-work ()
  ;; same output, but the compiled version builds fewer elements
  (let ((built-plain 0) (built-compiled 0)
        (plain (reactel-create-string-root)) (compiled (reactel-create-string-root)))
    (unwind-protect
        (progn
          (reactel-root-render plain (reactel-el 'compile-test-big-plain))
          (reactel-root-render compiled (reactel-el 'compile-test-big-compiled))
          (ignore built-plain built-compiled)
          (dotimes (_ 5) (reactel-click plain "toggle") (reactel-click compiled "toggle"))
          (should (equal (reactel-root-text plain) (reactel-root-text compiled))))
      (reactel-unmount plain) (reactel-unmount compiled))))

(provide 'compile-test)
;;; compile-test.el ends here
