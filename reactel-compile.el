;;; reactel-compile.el --- Automatic memoization, the React Compiler for Reactel -*- lexical-binding: t -*-

;; Copyright (C) 2026 Valentin Herrmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Like the React Compiler (https://react.dev/learn/react-compiler), this
;; memoizes automatically so that components need no `reactel-use-memo',
;; `reactel-use-callback' or `:memo' for the common cases.  It relies on
;; the Rules of React: rendering is pure, and values are never mutated
;; after they are created.
;;
;; When a component is defined with compilation enabled (the `:compile t'
;; option of `reactel-defcomponent', `reactel-defhook' and `reactel-lambda',
;; or globally `reactel-compile'), its body is macroexpanded and rewritten:
;;
;;   - *Scopes* are expressions whose identity matters: element constructors
;;     (`reactel-el', `reactel-c-*'), closures, and allocating or iterating
;;     calls such as `list', `mapcar' and `--map'.
;;   - The *dependencies* of a scope are the reactive values it reads:
;;     props, state, and values derived from them.  Setters, dispatch
;;     functions and refs are stable and never a dependency.
;;   - Each scope is wrapped in a cache lookup (`reactel--memo') keyed by
;;     its dependencies, stored in one per-component cache hook.  When no
;;     dependency changed the previous object is returned, so an unchanged
;;     element is `eq' to the old one and the reconciler skips its whole
;;     subtree, and a closure keeps its identity for memoized children.
;;   - A scope with the same dependencies as the scope around it is merged
;;     into it, and a scope without dependencies is built only once.
;;
;; What is never memoized, so that behavior is preserved:
;;
;;   - expressions calling hooks, impure functions (`random', `current-time',
;;     buffer and timer functions, `message', ...), reading refs
;;     (`reactel-ref-current'), or exiting non-locally (`throw');
;;   - expressions reading variables that are neither lexical variables of
;;     the component nor `defconst' constants (so specials and globals), or
;;     variables that are mutated anywhere in the body (`setq', `setcar',
;;     `puthash', ...);
;;   - anything inside a loop or a lambda body, which runs several times or
;;     later, outside the render.
;;
;; Like the real compiler, a component that breaks the Rules of Hooks is
;; skipped (with the reason recorded), never miscompiled.  Inspect the result
;; with `reactel-compile-show'.  Opt out with `:compile nil' or `:lint nil'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'reactel-lint)

(defvar reactel-compile--cache-sym nil "The symbol bound to the cache vector.")
(defvar reactel-compile--site 0 "Number of scopes allocated so far.")
(defvar reactel-compile--mutated nil "Variables mutated somewhere in the body.")

;;;; Classification of forms

(defconst reactel-compile--impure-calls
  '(random current-time float-time current-time-string current-buffer point
	   point-min point-max buffer-string buffer-substring buffer-substring-no-properties
	   buffer-size insert delete-region erase-buffer set-buffer save-excursion
	   save-current-buffer save-restriction message display-warning run-with-timer
	   run-at-time run-with-idle-timer make-process start-process add-hook remove-hook
	   kill-new sleep-for read-string completing-read y-or-n-p getenv emacs-pid
	   symbol-value set eval load throw reactel-ref-current reactel-batch
	   reactel-flush reactel-root-render reactel-render-to-string reactel-markdown-render)
  "Functions whose calls must not be memoized (impure or context-dependent).")

(defconst reactel-compile--allocators
  '(list vector cons append mapcar seq-map seq-filter seq-remove cl-remove-if
	 cl-remove-if-not reverse vconcat number-sequence copy-sequence)
  "Calls that allocate or iterate, and so are scopes.")

(defconst reactel-compile--mutators
  '((setcar . 1) (setcdr . 1) (aset . 1) (puthash . 3) (remhash . 2) (clrhash . 1)
    (nconc . 1) (nreverse . 1) (sort . 1) (delq . 2) (delete . 2) (delete-dups . 1)
    (fillarray . 1) (add-to-list . 1))
  "Mutating functions and the position of the argument they mutate.")

(defconst reactel-compile--opaque-heads
  '(while catch condition-case unwind-protect save-excursion save-restriction
	  save-current-buffer interactive declare defvar defconst)
  "Special forms that are never transformed.
They loop, or are not plain evaluation.")

(defun reactel-compile--hook-call-p (form)
  "Non-nil if FORM is a call of a hook."
  (and (consp form) (symbolp (car form)) (reactel-lint-hook-p (car form))))

(defun reactel-compile--candidate-p (form)
  "Non-nil if FORM is a scope root: an expression whose identity matters."
  (pcase form
    (`(function (lambda . ,_)) t)
    (`(,(and head (pred symbolp)) . ,_)
     (or (eq head 'reactel-el)
         (get head 'reactel-constructor)
         (memq head reactel-compile--allocators)
         (and (memq head '(let let*))
              (reactel-compile--contains-p form (lambda (f) (eq (car-safe f) 'while))))))))

(defun reactel-compile--contains-p (form pred)
  "Non-nil if PRED holds for FORM or any form inside it (quotes are skipped)."
  (cond ((not (consp form)) nil)
        ((eq (car form) 'quote) nil)
        ((funcall pred form) t)
        (t (let ((tail form) found)
             (while (and (consp tail) (not found))
               (setq found (reactel-compile--contains-p (car tail) pred)
                     tail (cdr tail)))
             found))))

(defun reactel-compile--bad-p (form)
  "Non-nil if FORM contains a call that makes it unsafe to memoize."
  (reactel-compile--contains-p
   form
   (lambda (f)
     (or (and (symbolp (car f)) (or (memq (car f) reactel-compile--impure-calls)
                                    (reactel-lint-hook-p (car f))))
         (and (eq (car f) 'function) (symbolp (cadr f))
              (or (memq (cadr f) reactel-compile--impure-calls)
                  (reactel-lint-hook-p (cadr f))))))))

(defun reactel-compile--mutated-vars (form)
  "The variables FORM assigns or destructively modifies."
  (let (out)
    (reactel-compile--contains-p
     form
     (lambda (f)
       (cond ((eq (car f) 'setq)
              (cl-loop for (v _) on (cdr f) by #'cddr
                       do (when (symbolp v) (cl-pushnew v out))))
             ((and (symbolp (car f)) (assq (car f) reactel-compile--mutators))
              (let ((target (nth (cdr (assq (car f) reactel-compile--mutators)) f)))
                (dolist (v (reactel-lint--free-vars target)) (cl-pushnew v out)))))
       nil))
    out))

;;;; Reactivity

;; ENV is an alist of (VAR ABSTRACT . REACTIVE): ABSTRACT as in
;; `reactel-lint--abstract', REACTIVE non-nil if the value can change
;; between renders.

(defun reactel-compile--lint-env (env)
  "ENV as the (VAR . ABSTRACT) alist `reactel-lint--abstract' expects."
  (mapcar (lambda (e) (cons (car e) (cadr e))) env))

(defun reactel-compile--reactive-vars (form env)
  "The reactive variables of ENV that FORM reads."
  (cl-remove-if-not (lambda (v) (let ((e (assq v env))) (and e (cddr e))))
                    (reactel-lint--free-vars form)))

(defun reactel-compile--constant-p (var)
  "Non-nil if the global VAR is a constant (`defconst' marks its variable risky)."
  (and (boundp var) (get var 'risky-local-variable) t))

(defun reactel-compile--memoizable-p (form env)
  "Non-nil if FORM can be cached by the values of its reactive variables.
Every variable it reads must be a lexical variable of the component or a
constant, and must not be mutated anywhere."
  (and (not (reactel-compile--bad-p form))
       (cl-every (lambda (v) (and (or (assq v env) (reactel-compile--constant-p v))
                                  (not (memq v reactel-compile--mutated))))
                 (reactel-lint--free-vars form))))

(defun reactel-compile--classify (var init env)
  "The ENV entry for VAR bound to INIT."
  (let* ((abstract (reactel-lint--abstract init (reactel-compile--lint-env env)))
         (reactive
          (cond ((vectorp abstract)
                 (pcase (aref abstract 0)
                   ('elem (= (aref abstract 1) 0)) ; the value, not the setter
                   ('stable nil)
                   (_ t)))
                ((reactel-compile--hook-call-p init) t) ; unknown hook result
                (t (or (not (reactel-compile--memoizable-p init env))
                       (and (reactel-compile--reactive-vars init env) t))))))
    (cons var (cons (and (vectorp abstract) abstract) reactive))))

;;;; The transformation

(defun reactel-compile--same-set-p (a b)
  "Non-nil if the lists A and B hold the same elements."
  (and (cl-subsetp a b) (cl-subsetp b a)))

(defun reactel-compile--scope (form env parent)
  "Memoize FORM if it is memoizable, else only transform inside it.
PARENT is the dependency list of the enclosing scope, or `:none'."
  (if (not (reactel-compile--memoizable-p form env))
      (reactel-compile--inside form env parent)
    (let ((deps (reactel-compile--reactive-vars form env)))
      (if (and (listp parent) (reactel-compile--same-set-p deps parent))
          (reactel-compile--inside form env parent) ; merged into the enclosing scope
        (let* ((index (* 2 (prog1 reactel-compile--site (cl-incf reactel-compile--site))))
               (inner (reactel-compile--inside form env deps)))
          `(reactel--memo ,reactel-compile--cache-sym ,index ,deps ,inner))))))

(defun reactel-compile--inside (form env parent)
  "Transform the evaluated subforms of the scope FORM."
  (pcase form
    (`(function . ,_) form)             ; the closure body runs later: untouched
    (`(,(or 'let 'let*) . ,_) (reactel-compile--walk-let form env parent))
    (`(,head . ,args)
     (cons head (mapcar (lambda (a) (reactel-compile--walk a env parent)) args)))))

(defun reactel-compile--walk-let (form env parent)
  "Transform the `let' or `let*' FORM, tracking the variables it binds."
  (let ((star (eq (car form) 'let*))
        (new env)
        bindings)
    (dolist (b (cadr form))
      (let* ((var (if (consp b) (car b) b))
             (init (and (consp b) (cadr b)))
             (scope (if star new env)))
        (push (if (consp b) (list var (reactel-compile--walk init scope parent)) b) bindings)
        (push (reactel-compile--classify var init scope) new)))
    `(,(car form) ,(nreverse bindings)
      ,@(mapcar (lambda (f) (reactel-compile--walk f new parent)) (cddr form)))))

(defun reactel-compile--walk (form env parent)
  "Return FORM with its scopes memoized.  ENV and PARENT as above."
  (if (not (and (consp form) (proper-list-p form)))
      form
    (let ((head (car form)) (args (cdr form)))
      (cond
       ((memq head '(quote function))
        (if (and (eq head 'function) (reactel-compile--candidate-p form))
            (reactel-compile--scope form env parent)
          form))
       ((memq head '(progn prog1))
        (cons head (mapcar (lambda (f) (reactel-compile--walk f env parent)) args)))
       ((eq head 'if)
        `(if ,@(mapcar (lambda (f) (reactel-compile--walk f env parent)) args)))
       ((memq head '(and or))
        (cons head (mapcar (lambda (f) (reactel-compile--walk f env parent)) args)))
       ((eq head 'cond)
        `(cond ,@(mapcar (lambda (clause)
                           (if (consp clause)
                               (mapcar (lambda (f) (reactel-compile--walk f env parent)) clause)
                             clause))
                         args)))
       ((eq head 'setq)
        `(setq ,@(cl-loop for (v val) on args by #'cddr
                          append (list v (reactel-compile--walk val env parent)))))
       ((memq head reactel-compile--opaque-heads) form)
       ((or (not (symbolp head)) (reactel-compile--hook-call-p form)) form)
       ((memq head '(let let*))
        (if (reactel-compile--candidate-p form)
            (reactel-compile--scope form env parent)
          (reactel-compile--walk-let form env parent)))
       ((not (or (fboundp head) (special-form-p head))) form) ; unknown (macro?): leave alone
       ((special-form-p head) form)
       ((reactel-compile--candidate-p form) (reactel-compile--scope form env parent))
       (t (cons head (mapcar (lambda (a) (reactel-compile--walk a env parent)) args)))))))

;;;; Entry points

(defun reactel-compile-forms (forms &optional bound env)
  "Compile the component body FORMS; return a plist (:forms :scopes :skipped).
BOUND lists the variables of the lambda list (the props).  ENV is the macro
environment.  :FORMS are the rewritten forms (FORMS themselves when nothing
was memoized or the component was skipped, with :SKIPPED giving the reason)."
  (cl-block nil
    (unless (bound-and-true-p lexical-binding)
      (cl-return (list :forms forms :scopes 0 :skipped "lexical-binding is nil")))
    (let ((violations (cl-remove-if (lambda (d) (memq (car d) '(unanalyzable exhaustive-deps nested)))
                                    (let ((reactel-lint-exhaustive-deps nil))
                                      (reactel-lint-forms forms 'component env bound)))))
      (when violations
        (cl-return (list :forms forms :scopes 0
                         :skipped (format "breaks the Rules of React: %s" (cadr (car violations)))))))
    (condition-case err
        (let* ((expanded (macroexpand-all `(progn ,@forms) env))
               (reactel-compile--cache-sym (make-symbol "reactel-cache"))
               (reactel-compile--site 0)
               (reactel-compile--mutated (reactel-compile--mutated-vars expanded))
               (env (mapcar (lambda (v) (cons v (cons nil t))) bound))
               (walked (mapcar (lambda (f) (reactel-compile--walk f env :none)) (cdr expanded))))
          (if (zerop reactel-compile--site)
              (list :forms forms :scopes 0)
            (list :forms `((let ((,reactel-compile--cache-sym
                                  (reactel--use-cache ,reactel-compile--site)))
                             ,@walked))
                  :scopes reactel-compile--site)))
      (error (list :forms forms :scopes 0
                   :skipped (format "cannot analyze: %s" (error-message-string err)))))))

(defun reactel-compile-show (name)
  "Display the compiled body of the component NAME."
  (interactive (list (intern (completing-read
                              "Component: "
                              (let (names)
                                (mapatoms (lambda (s) (when (get s 'reactel-compile-info)
                                                        (push (symbol-name s) names))))
                                names)
                              nil t))))
  (let ((info (get name 'reactel-compile-info)))
    (unless info (user-error "%s was not compiled by reactel" name))
    (with-current-buffer (get-buffer-create "*reactel-compiled*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (emacs-lisp-mode)
        (insert (format ";; %s: %s\n" name
                        (if-let ((why (plist-get info :skipped)))
                            (format "skipped, %s" why)
                          (format "%d memoized scopes" (plist-get info :scopes)))))
        (dolist (f (plist-get info :forms))
          (insert (pp-to-string f) "\n"))
        (goto-char (point-min)))
      (pop-to-buffer (current-buffer)))))

(provide 'reactel-compile)
;;; reactel-compile.el ends here
