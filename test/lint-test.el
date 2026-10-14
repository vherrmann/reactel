;;; lint-test.el --- Tests for the static Rules of React checks -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'reactel)
(require 'reactel-lint)

(defun lint-rules (&rest body)
  "Rules reported for component body forms BODY."
  (mapcar #'car (reactel-lint-forms body)))

(defun lint-rules-plain (&rest body)
  (mapcar #'car (reactel-lint-forms body 'plain)))

;;; Rules of Hooks: top level only

(ert-deftest lint-clean-component ()
  (should-not (lint-rules '(let ((a (reactel-use-state 0)) (b (reactel-use-ref nil)))
                             (reactel-use-effect (lambda () nil) nil)
                             (list a b)))))

(ert-deftest lint-hook-in-if-branches ()
  (should (equal (lint-rules '(if c (reactel-use-state 1) nil)) '(hooks-top-level)))
  (should (equal (lint-rules '(if c nil (reactel-use-state 1))) '(hooks-top-level)))
  (should (equal (lint-rules '(when c (reactel-use-state 1))) '(hooks-top-level)))
  (should (equal (lint-rules '(unless c (reactel-use-state 1))) '(hooks-top-level)))
  ;; the *test* of an `if' runs unconditionally
  (should-not (lint-rules '(if (reactel-use-state 1) 'a 'b))))

(ert-deftest lint-hook-in-and-or-cond ()
  (should (equal (lint-rules '(and c (reactel-use-ref 1))) '(hooks-top-level)))
  (should (equal (lint-rules '(or c (reactel-use-ref 1))) '(hooks-top-level)))
  (should-not (lint-rules '(and (reactel-use-ref 1) c)))
  (should (equal (lint-rules '(cond (a 1) ((reactel-use-ref 1) 2))) '(hooks-top-level)))
  (should (equal (lint-rules '(cond (a (reactel-use-ref 1)))) '(hooks-top-level)))
  (should-not (lint-rules '(cond ((reactel-use-ref 1) 2)))))

(ert-deftest lint-hook-in-loops ()
  (should (equal (lint-rules '(dolist (x xs) (reactel-use-ref x))) '(hooks-top-level)))
  (should (equal (lint-rules '(while c (reactel-use-ref 1))) '(hooks-top-level)))
  (should (equal (lint-rules '(cl-loop for x in xs collect (reactel-use-ref x))) '(hooks-top-level)))
  (should (equal (lint-rules '(dotimes (_ 3) (reactel-use-ref 1))) '(hooks-top-level))))

(ert-deftest lint-hook-in-nested-function ()
  (should (equal (lint-rules '(mapcar (lambda (x) (reactel-use-ref x)) xs)) '(hooks-top-level)))
  (should (equal (lint-rules '(reactel-use-effect (lambda () (reactel-use-state 1) nil) nil))
                 '(hooks-top-level)))
  (should (equal (lint-rules '(reactel-use-effect* (a) (reactel-use-state 1) nil))
                 '(hooks-top-level)))
  ;; the hook call itself and its dependency forms are fine
  (should-not (lint-rules '(reactel-use-effect* (a b) (message "x") nil)))
  (should-not (lint-rules '(reactel-use-callback* (a) (x) (+ a x)))))

(ert-deftest lint-hook-after-early-return ()
  (should (equal (lint-rules '(progn (when c (cl-return-from foo nil)) (reactel-use-state 1)))
                 '(hooks-top-level)))
  (should (equal (lint-rules '(progn (throw 'outer 1) (reactel-use-state 1))) '(hooks-top-level)))
  ;; a catch/throw pair inside the body is not an early return of the component
  (should-not (lint-rules '(progn (catch 'x (throw 'x 1)) (reactel-use-state 1))))
  (should-not (lint-rules '(progn (cl-loop for x in xs do (cl-return x)) (reactel-use-state 1))))
  ;; a throw inside a stored callback is not an early return of the component
  (should-not (lint-rules '(progn (setq f (lambda () (throw 'x 1))) (reactel-use-state 1))))
  ;; ...but one in an immediately called lambda is
  (should (equal (lint-rules '(progn (funcall (lambda () (throw 'x 1))) (reactel-use-state 1)))
                 '(hooks-top-level))))

(ert-deftest lint-hook-in-condition-case-handler ()
  (should (equal (lint-rules '(condition-case nil (foo) (error (reactel-use-state 1))))
                 '(hooks-top-level)))
  (should-not (lint-rules '(condition-case nil (reactel-use-state 1) (error nil)))))

(ert-deftest lint-destructuring-macros-do-not-false-positive ()
  (should-not (lint-rules '(cl-destructuring-bind (a b) (reactel-use-state 0) (list a b))))
  (should-not (lint-rules '(pcase-let ((`(,a ,b) (reactel-use-state 0))) (list a b))))
  (should-not (lint-rules '(reactel-let-hooks (((a b) (reactel-use-state 0))
                                               (r (reactel-use-ref a)))
                             (list a b r))))
  (should-not (lint-rules '(let* ((s (reactel-use-state 0)) (a (car s))) (reactel-use-ref a)))))

(ert-deftest lint-dash-let-does-not-false-positive ()
  (skip-unless (require 'dash nil t))
  (should-not (lint-rules '(-let* (((a b) (reactel-use-state 0))
                                   ((&plist :x x) (reactel-use-ref nil)))
                             (list a b x)))))

;;; Hooks only from components and hooks

(ert-deftest lint-plain-function-calling-hook ()
  (should (equal (lint-rules-plain '(reactel-use-state 0)) '(hooks-callers)))
  (should (equal (lint-rules-plain '(let ((x 1)) (list (reactel-use-ref x)))) '(hooks-callers)))
  (should-not (lint-rules-plain '(list 1 2))))

;;; Hooks as values, components called directly

(ert-deftest lint-hooks-as-values ()
  (should (equal (lint-rules '(mapcar #'reactel-use-state xs)) '(hooks-as-values)))
  ;; (funcall #'hook 0) is just a call of the hook, and is checked as one
  (should (equal (lint-rules '(when c (funcall #'reactel-use-state 0))) '(hooks-top-level)))
  (should (equal (lint-rules '(apply 'reactel-use-state '(0))) '(hooks-as-values))))

(reactel-defcomponent lint-test-target (&key x) (reactel-c-text x))

(ert-deftest lint-direct-component-call ()
  (should (equal (lint-rules '(lint-test-target :x 1)) '(component-call)))
  (should (equal (lint-rules '(funcall #'lint-test-target :x 1)) '(component-call)))
  (should-not (lint-rules '(reactel-el 'lint-test-target :x 1)))
  (should-not (lint-rules '(reactel-el #'lint-test-target :x 1))))

(ert-deftest lint-direct-call-fails-at-runtime-too ()
  (should-error (lint-test-target :x 1) :type 'error)
  (should (string-match-p "called directly"
                          (cadr (should-error (lint-test-target :x 1))))))

;;; Custom hooks

(reactel-defhook reactel-test-use-toggle (initial)
  (cl-destructuring-bind (on set-on) (reactel-use-state initial)
    (list on (lambda () (funcall set-on reactel-toggle)))))

(ert-deftest lint-custom-hooks-are-hooks ()
  (should (reactel-lint-hook-p 'reactel-test-use-toggle))
  (should (equal (lint-rules '(when c (reactel-test-use-toggle nil))) '(hooks-top-level)))
  (should (equal (lint-rules-plain '(reactel-test-use-toggle nil)) '(hooks-callers)))
  (should-not (lint-rules '(reactel-test-use-toggle nil))))

(ert-deftest lint-custom-hook-works-in-component ()
  (reactel-defcomponent lint-test-toggler ()
    (cl-destructuring-bind (on flip) (reactel-test-use-toggle nil)
      (reactel-c-button :on-click flip (if on "on" "off"))))
  (let ((r (reactel-create-string-root)))
    (reactel-root-render r (reactel-el 'lint-test-toggler))
    (should (equal (reactel-root-text r) "[off]"))
    (reactel-click r "off")
    (should (equal (reactel-root-text r) "[on]"))
    (reactel-unmount r)))

;;; Reporting through the defining macros

(defun lint-capture (form)
  "Macroexpand-all FORM; return the lint messages produced."
  (let (msgs)
    (let ((reactel-lint-reporter (lambda (msg f) (push msg msgs) f)))
      (macroexpand-all form))
    (nreverse msgs)))

(ert-deftest lint-defcomponent-reports-on-expansion ()
  (let ((msgs (lint-capture '(reactel-defcomponent lint-test-bad (&key flag)
                               (when flag (reactel-use-state 1))
                               (reactel-c-text "x")))))
    (should (= (length msgs) 1))
    (should (string-match-p "component `lint-test-bad'.*reactel-use-state.*conditional" (car msgs)))))

(ert-deftest lint-lambda-and-hook-macros-report ()
  (should (lint-capture '(reactel-lambda () (dolist (x xs) (reactel-use-ref x)))))
  (should (lint-capture '(reactel-defhook reactel-test-use-bad () (when c (reactel-use-ref 1))))))

(ert-deftest lint-opt-out-and-severity ()
  (should-not (lint-capture '(reactel-defcomponent lint-test-ok (&key flag)
                               :lint nil
                               (when flag (reactel-use-state 1)) (reactel-c-text "x"))))
  (let ((reactel-lint-severity nil))
    (should-not (lint-capture '(reactel-defcomponent lint-test-ok2 () (when c (reactel-use-state 1))))))
  (let ((reactel-lint-severity 'error))
    (should-error (macroexpand-all '(reactel-defcomponent lint-test-err ()
                                      (when c (reactel-use-state 1)))))))

(ert-deftest lint-registration-at-expansion-time ()
  ;; defcomponent registers the name while expanding, so it can be checked
  ;; against in forms that follow, even before the definition is evaluated.
  (macroexpand-all '(reactel-defcomponent lint-test-later () (reactel-c-text "x")))
  (should (reactel-lint-component-p 'lint-test-later)))

;;; Exhaustive dependencies

(defun lint-deps (bound &rest body)
  "Diagnostics of the exhaustive-deps rule for BODY with props BOUND."
  (mapcar #'cadr (cl-remove-if-not (lambda (d) (eq (car d) 'exhaustive-deps))
                                   (reactel-lint-forms body 'component nil bound))))

(ert-deftest lint-deps-missing-state-prop-and-derived ()
  (let ((msgs (lint-deps '(prop)
                         '(cl-destructuring-bind (count set-count) (reactel-use-state 0)
                            (let ((double (* 2 count)))
                              (reactel-use-effect* ()
                                (message "%s %s %s" count prop double)
                                (funcall set-count 1)
                                nil)))))
        (case-fold-search nil))
    (should (= (length msgs) 1))
    (should (string-match-p "`count', `prop', `double'.* not in its dependency list" (car msgs)))
    (should-not (string-match-p "set-count" (car msgs)))))

(ert-deftest lint-deps-complete-lists-are-clean ()
  (should-not (lint-deps '(prop)
                         '(cl-destructuring-bind (count set-count) (reactel-use-state 0)
                            (reactel-use-effect* (count prop) (funcall set-count (+ count prop)) nil))))
  (should-not (lint-deps nil '(let ((x 1)) (reactel-use-memo* (x) (* x 2)))))
  (should-not (lint-deps nil '(let ((x 1)) (reactel-use-callback* (x) (y) (+ x y))))))

(ert-deftest lint-deps-always-and-nonliteral-lists-are-skipped ()
  (should-not (lint-deps nil '(let ((x 1)) (reactel-use-effect* :always (message "%s" x) nil))))
  (should-not (lint-deps nil '(let ((x 1) (deps (list 1))) (reactel-use-effect (lambda () x) deps)))))

(ert-deftest lint-deps-stable-values-are-exempt ()
  ;; setters, dispatch functions and refs never change
  (should-not (lint-deps nil
                         '(cl-destructuring-bind (s dispatch) (reactel-use-reducer #'ignore 0)
                            (let ((ref (reactel-use-ref nil)))
                              (reactel-use-effect* () (funcall dispatch 1) (setf (reactel-ref-current ref) 2)
						   nil)))))
  ;; but the state itself is a dependency
  (should (lint-deps nil
                     '(cl-destructuring-bind (s dispatch) (reactel-use-reducer #'ignore 0)
                        (reactel-use-effect* () (funcall dispatch s) nil)))))

(ert-deftest lint-deps-destructuring-styles ()
  (should-not (lint-deps nil '(pcase-let ((`(,a ,set-a) (reactel-use-state 0)))
                                (reactel-use-effect* (a) (funcall set-a a) nil))))
  (should (lint-deps nil '(pcase-let ((`(,a ,set-a) (reactel-use-state 0)))
                            (reactel-use-effect* () (funcall set-a a) nil))))
  (should-not (lint-deps nil '(reactel-let-hooks (((a set-a) (reactel-use-state 0)))
                                (reactel-use-effect* (a) (funcall set-a a) nil))))
  (should-not (lint-deps nil '(let* ((s (reactel-use-state 0)) (a (car s)) (set-a (cadr s)))
                                (reactel-use-effect* (a) (funcall set-a a) nil)))))

(ert-deftest lint-deps-dash-patterns ()
  (skip-unless (require 'dash nil t))
  (should-not (lint-deps nil '(-let* (((a set-a) (reactel-use-state 0)))
                                (reactel-use-effect* (a) (funcall set-a a) nil))))
  (should (lint-deps nil '(-let* (((a set-a) (reactel-use-state 0)))
                            (reactel-use-effect* () (funcall set-a a) nil)))))

(ert-deftest lint-deps-ignores-locals-of-the-callback-and-globals ()
  (should-not (lint-deps nil '(reactel-use-effect* ()
                                (let ((local 1)) (message "%s" (+ local (length load-path))))
                                nil)))
  (should-not (lint-deps nil '(reactel-use-effect* ()
                                (mapc (lambda (x) (message "%s" x)) '(1 2))
                                (condition-case err (foo) (error (message "%s" err)))
                                nil))))

(ert-deftest lint-deps-reported-by-defcomponent-expansion ()
  (let ((msgs (lint-capture '(reactel-defcomponent lint-test-stale (&key id)
                               (reactel-use-effect* () (fetch id) nil)
                               (reactel-c-text "x")))))
    (should (= (length msgs) 1))
    (should (string-match-p "uses `id'" (car msgs))))
  (let ((reactel-lint-exhaustive-deps nil))
    (should-not (lint-capture '(reactel-defcomponent lint-test-stale2 (&key id)
                                 (reactel-use-effect* () (fetch id) nil)
                                 (reactel-c-text "x"))))))

;;; File linting

(ert-deftest lint-file ()
  (let ((file (make-temp-file "reactel-lint" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "(reactel-defcomponent good () (reactel-use-state 0) (reactel-c-text \"x\"))\n"
                    "(defun helper () (reactel-use-ref 1))\n"
                    "(defun my-use-thing () (reactel-use-ref 1))\n"
                    "(reactel-defcomponent bad (&key f) (if f (reactel-use-ref 1) nil))\n"
                    "(reactel-defcomponent skipped () :lint nil (when c (reactel-use-ref 1)))\n"))
          (let ((res (reactel-lint-file file)))
            (should (equal (mapcar (lambda (d) (list (nth 1 d) (nth 2 d) (nth 3 d))) res)
                           '((2 helper hooks-callers) (4 bad hooks-top-level))))))
      (delete-file file))))

(ert-deftest lint-the-project-itself-is-clean ()
  (dolist (file (append (directory-files "." t "\\`reactel[a-z-]*\\.el\\'")
                        (directory-files "examples" t "\\.el\\'")))
    (unless (string-match-p "reactel-lint\\.el\\'" file)
      (let ((res (reactel-lint-file file)))
        (should (equal (list file nil) (list file (mapcar #'cdr res))))))))

(provide 'lint-test)
;;; lint-test.el ends here
