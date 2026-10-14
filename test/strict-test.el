;;; strict-test.el --- Tests for the runtime Rules of React checks -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'reactel)
(require 'reactel-buffer)

(defvar strict-log nil)
(defvar strict-counter 0)

(defmacro with-strict (checks &rest body)
  "Run BODY with strict CHECKS enabled and a fresh string root bound to `r'."
  (declare (indent 1))
  `(let ((reactel-strict ,checks) (strict-log nil) (strict-counter 0)
         (r (reactel-create-string-root)))
     (unwind-protect (progn ,@body)
       (ignore-errors (reactel-unmount r)))))

(defun strict-error-message (thunk)
  (cadr (should-error (funcall thunk))))

;;; Rule: components are pure and idempotent

(reactel-defcomponent strict-test-impure-global ()
  (cl-incf strict-counter)
  (reactel-c-text (format "n=%d" strict-counter)))

(ert-deftest strict-impure-render-detected ()
  (with-strict '(purity)
    (let ((msg (strict-error-message
		(lambda () (reactel-root-render r (reactel-el 'strict-test-impure-global))))))
      (should (string-match-p "strict-test-impure-global.*not pure" msg)))))

(ert-deftest strict-impure-render-allowed-when-off ()
  (with-strict nil
    (reactel-root-render r (reactel-el 'strict-test-impure-global))
    (should (equal (reactel-root-text r) "n=1"))))

(reactel-defcomponent strict-test-ref-in-render ()
  (let ((ref (reactel-use-ref 0)))
    (cl-incf (reactel-ref-current ref))
    (reactel-c-text (number-to-string (reactel-ref-current ref)))))

(ert-deftest strict-writing-refs-during-render-detected ()
  (with-strict '(purity)
    (should-error (reactel-root-render r (reactel-el 'strict-test-ref-in-render)))))

(reactel-defcomponent strict-test-pure ()
  (cl-destructuring-bind (n set-n) (reactel-use-state 0)
    (let ((v (reactel-use-memo* (n) (* n 2))))
      (reactel-c-button :on-click (lambda () (funcall set-n reactel-inc))
        (format "n=%d v=%d" n v)))))

(ert-deftest strict-pure-component-passes-and-renders-once-visible ()
  (with-strict t
    (reactel-root-render r (reactel-el 'strict-test-pure))
    (reactel-click r "n=0 v=0")
    (should (equal (reactel-root-text r) "[n=1 v=2]"))))

(reactel-defcomponent strict-test-exempt ()
  :lint nil
  (cl-incf strict-counter)
  (reactel-c-text (format "n=%d" strict-counter)))

(ert-deftest strict-lint-nil-opts-out ()
  (with-strict t
    (reactel-root-render r (reactel-el 'strict-test-exempt))
    (should (string-match-p "n=" (reactel-root-text r)))))

(ert-deftest strict-impure-memo-and-reducer-detected ()
  (reactel-defcomponent strict-test-memo-impure ()
    (reactel-c-text (number-to-string
                     (reactel-use-memo (lambda () (cl-incf strict-counter)) nil))))
  (with-strict '(purity)
    (should (string-match-p "use-memo is not pure"
			    (strict-error-message
			     (lambda () (reactel-root-render r (reactel-el 'strict-test-memo-impure)))))))
  (let (dispatch)
    (reactel-defcomponent strict-test-reducer-impure ()
      (cl-destructuring-bind (s d) (reactel-use-reducer
                                    (lambda (s _) (+ s (cl-incf strict-counter))) 0)
        (setq dispatch d)
        (reactel-c-text (number-to-string s))))
    (with-strict '(purity)
      (reactel-root-render r (reactel-el 'strict-test-reducer-impure))
      (funcall dispatch 'go)
      (should-error (reactel-flush))
      (setq reactel--pending-roots nil))))

;;; Rule: props and state are immutable

(reactel-defcomponent strict-test-mutates-props (&key items)
  (push 'extra (car items))
  (reactel-c-text "x"))

(ert-deftest strict-props-mutation-detected ()
  (with-strict '(mutation)
    (should (string-match-p "mutated its props"
			    (strict-error-message
			     (lambda () (reactel-root-render
					 r (reactel-el 'strict-test-mutates-props
					     :items (list (list 1 2))))))))))

(ert-deftest strict-state-mutation-detected-even-when-setter-would-bail-out ()
  (let (setter items)
    (reactel-defcomponent strict-test-state-mutation ()
      (cl-destructuring-bind (xs set-xs) (reactel-use-state (list 1 2))
        (setq setter set-xs items xs)
        (reactel-c-text (format "%S" xs))))
    ;; Without strict mode the in-place mutation is silently lost: the setter
    ;; is called with the very same list, so React-style bail-out kicks in.
    (with-strict nil
      (reactel-root-render r (reactel-el 'strict-test-state-mutation))
      (setcdr (last items) (list 3))    ; mutate the state in place
      (funcall setter items)            ; "same value", so no re-render
      (reactel-flush)
      (should (equal (reactel-root-text r) "(1 2)")))
    (with-strict '(mutation)
      (reactel-root-render r (reactel-el 'strict-test-state-mutation))
      (setcar items 99)                 ; mutate in place
      (should (string-match-p "State of <strict-test-state-mutation> was mutated in place"
			      (strict-error-message (lambda () (funcall setter items))))))))

(ert-deftest strict-memo-value-mutation-detected ()
  (let (value)
    (reactel-defcomponent strict-test-memo-mutation (&key bump)
      (setq value (reactel-use-memo* () (list 1 2)))
      (when bump (setcar value 99))
      (reactel-c-text (format "%S" value)))
    (with-strict '(mutation)
      (reactel-root-render r (reactel-el 'strict-test-memo-mutation))
      (setcar value 5)
      (should (string-match-p "use-memo.*mutated"
			      (strict-error-message
			       (lambda () (reactel-root-render r (reactel-el 'strict-test-memo-mutation)))))))))

(ert-deftest strict-element-props-mutation-after-creation-detected ()
  (with-strict '(mutation)
    (let* ((style (list :color "red"))
	   (el (reactel-c-text :props style "x")))
      (setf (cadr style) "blue")        ; mutate a value already passed to JSX
      (should (string-match-p "mutated after it was created"
			      (strict-error-message (lambda () (reactel-root-render r el))))))))

;;; Rule: no side effects during render

(ert-deftest strict-updating-another-component-while-rendering-detected ()
  (let (child-setter)
    (reactel-defcomponent strict-test-child ()
      (cl-destructuring-bind (n set-n) (reactel-use-state 0)
        (setq child-setter set-n)
        (reactel-c-text (number-to-string n))))
    (reactel-defcomponent strict-test-parent-sets-child ()
      (when child-setter (funcall child-setter 5))
      (reactel-c-vstack (reactel-el 'strict-test-child)))
    (with-strict '(render-update)
      (reactel-root-render r (reactel-el 'strict-test-parent-sets-child))
      (should (string-match-p "updated the state of <strict-test-child> while rendering"
			      (strict-error-message
			       (lambda () (reactel-root-render
					   r (reactel-el 'strict-test-parent-sets-child)))))))))

(reactel-defcomponent strict-test-inserts ()
  (insert "side effect")
  (reactel-c-text "x"))

(ert-deftest strict-buffer-modification-during-render-detected ()
  (with-strict '(buffer)
    (with-temp-buffer
      (should (string-match-p "modified the buffer while rendering"
			      (strict-error-message
			       (lambda () (reactel-root-render r (reactel-el 'strict-test-inserts)))))))))

(ert-deftest strict-impure-calls-while-rendering-detected-by-strict-mode ()
  (reactel-defcomponent strict-test-timer ()
    (run-with-timer 100 nil #'ignore)
    (reactel-c-text "x"))
  (unwind-protect
      (progn
        (reactel-strict-mode 1)
        (should (string-match-p "side effect while rendering"
                                (strict-error-message
                                 (lambda ()
                                   (let ((r (reactel-create-string-root)))
                                     (reactel-root-render r (reactel-el 'strict-test-timer))))))))
    (reactel-strict-mode -1))
  (should-not reactel-strict)
  ;; and the advice is gone again
  (should-not (advice-member-p #'reactel-strict--guard 'run-with-timer)))

;;; Rule: effects must clean up after themselves

(ert-deftest strict-effects-mount-cleanup-mount ()
  (reactel-defcomponent strict-test-effects ()
    (reactel-use-effect* ()
      (push 'mount strict-log)
      (lambda () (push 'cleanup strict-log)))
    (reactel-c-text "e"))
  (with-strict '(effects)
    (reactel-root-render r (reactel-el 'strict-test-effects))
    (should (equal (reverse strict-log) '(mount cleanup mount)))
    (reactel-root-render r nil)
    (should (equal (reverse strict-log) '(mount cleanup mount cleanup))))
  ;; without strict mode: a plain mount / cleanup
  (with-strict nil
    (reactel-root-render r (reactel-el 'strict-test-effects))
    (reactel-root-render r nil)
    (should (equal (reverse strict-log) '(mount cleanup)))))

(ert-deftest strict-effects-expose-missing-cleanup ()
  (reactel-defcomponent strict-test-leaky ()
    (reactel-use-effect* () (cl-incf strict-counter) nil) ; subscribes, never unsubscribes
    (reactel-c-text "l"))
  (with-strict '(effects)
    (reactel-root-render r (reactel-el 'strict-test-leaky))
    (should (= strict-counter 2))))

;;; Rule: hooks are only called by React

(ert-deftest strict-guard-against-direct-calls-is-always-on ()
  (with-strict nil
    (should-error (strict-test-pure))))

;;; The examples follow every rule

(defmacro strict-test-with-example (element &rest body)
  "Mount ELEMENT on a string root bound to `r', run BODY, then unmount."
  (declare (indent 1))
  `(let ((r (reactel-create-string-root)))
     (unwind-protect (progn (reactel-root-render r ,element) ,@body)
       (ignore-errors (reactel-unmount r)))))

(ert-deftest strict-every-example-passes-all-checks ()
  ;; Each example is driven through real interactions with every runtime
  ;; check on, including the double render, the mount/cleanup/mount of
  ;; effects and the guard against timers and hooks started while rendering.
  ;; None of them needs `:lint nil'.  `make test-compiled' runs this compiled.
  (skip-unless (require 'dash nil t))
  (require 'reactel-examples)
  (let ((kill-ring nil) (kill-ring-yank-pointer nil))
    (unwind-protect
        (progn
          (reactel-strict-mode 1)
          (reactel-ex-render-counter-reset)
          (strict-test-with-example (reactel-el 'reactel-ex-counter)
	    (reactel-click r "+") (reactel-click r "+") (reactel-click r "-")
	    (reactel-click r "reset"))
          (strict-test-with-example (reactel-el 'reactel-ex-stopwatch :interval 0.01)
	    (reactel-click r "start") (sleep-for 0.05) (reactel-click r "lap")
	    (reactel-click r "stop") (reactel-click r "reset"))
          (strict-test-with-example (reactel-el 'reactel-ex-todo)
	    (reactel-type r "New todo" "x") (reactel-click r "add")
	    (reactel-click r "Port React to Emacs") (reactel-click r "done")
	    (reactel-click r "clear done") (reactel-click r "all"))
          (strict-test-with-example (reactel-el 'reactel-ex-form)
	    (reactel-type r "Name" "Ada Lovelace") (reactel-type r "Email" "a@b.org")
	    (reactel-type r "Age" "36") (reactel-click r "sign up"))
          (strict-test-with-example (reactel-el 'reactel-ex-cart)
	    (reactel-click r "add") (reactel-click r "+") (reactel-click r "-")
	    (reactel-click r "clear"))
          (strict-test-with-example (reactel-el 'reactel-ex-boundary)
	    (dotimes (_ 3) (reactel-click r "increment"))
	    (reactel-click r "retry") (reactel-click r "zero"))
          (strict-test-with-example (reactel-el 'reactel-ex-store)
	    (reactel-click r "+1") (reactel-act (reactel-ex-store-set 9))
	    (reactel-click r "reset"))
          (strict-test-with-example (reactel-el 'reactel-ex-markdown-app)
	    (reactel-click r "doing") (reactel-click r "Copy as Markdown")
	    (reactel-click r "all"))
          (strict-test-with-example (reactel-el 'reactel-ex-compiler)
	    (reactel-click r "tick") (reactel-click r "beta"))
          ;; the larger examples
          (strict-test-with-example (reactel-el 'reactel-ex-kanban)
            (reactel-click r "→")        ; refused: the doing column is full
            (reactel-click r "✕") (reactel-click r "undo") (reactel-click r "redo")
            (reactel-type r "New card" "x") (reactel-click r "add card")
            (reactel-click r "grace") (reactel-click r "everyone") (reactel-click r "✕"))
          (let ((dir (file-name-as-directory (make-temp-file "reactel-strict" t))))
            (unwind-protect
                (progn
                  (make-directory (expand-file-name "sub" dir))
                  (with-temp-file (expand-file-name "a.txt" dir) (insert "a\nb\n"))
                  (strict-test-with-example (reactel-el 'reactel-ex-tree :root dir)
                    (reactel-click r "▸ sub/") (reactel-click r "a.txt")
                    (reactel-type r "Filter" "a") (reactel-click r "hidden files")))
              (delete-directory dir t)))
          (let ((reactel-ex-search-requests 0))
            (strict-test-with-example (reactel-el 'reactel-ex-search :debounce 0.02 :latency 0.03)
              (reactel-type r "Search" "mode")
              (let ((deadline (+ (float-time) 3)))
                (while (and (not (string-match-p "mode line" (reactel-root-text r)))
                            (< (float-time) deadline))
                  (sleep-for 0.01)))
              (should (string-match-p "mode line" (reactel-root-text r)))
              (reactel-type r "Search" "x!") (sleep-for 0.1)))
          (strict-test-with-example (reactel-el 'reactel-ex-life :width 6 :height 6 :interval 0.01)
            (reactel-click r "step") (reactel-click r "run") (sleep-for 0.05)
            (reactel-click r "stop") (reactel-click r "clear"))
          (unwind-protect
              (strict-test-with-example (reactel-el 'reactel-ex-buffers)
                (reactel-type r "Filter" "scratch") (reactel-click r "size")
                (reactel-act (get-buffer-create "strict-test-buf")))
            (when (get-buffer "strict-test-buf") (kill-buffer "strict-test-buf")))
          (unwind-protect
              (progn
                (reactel-render-buffer "*strict-jump*" (reactel-el 'reactel-ex-jump :rows 20 :start 5))
                (reactel-click (buffer-local-value 'reactel-buffer--root (get-buffer "*strict-jump*"))
                               "next"))
            (kill-buffer "*strict-jump*")))
      (reactel-strict-mode -1)))
  (should-not reactel-strict))

(provide 'strict-test)
;;; strict-test.el ends here
