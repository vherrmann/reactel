;;; reactel-lint.el --- Static checks for the Rules of React -*- lexical-binding: t -*-

;; Copyright (C) 2026 Valentin Herrmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Static analysis of Reactel components, in the spirit of
;; eslint-plugin-react-hooks.  The analyzer macroexpands a component body
;; (`macroexpand-all' leaves a tiny core language) and walks it, tracking
;; whether each position is evaluated unconditionally.
;;
;; Rules checked (see https://react.dev/reference/rules):
;;
;;   hooks-top-level     Hooks must not be called in conditions, loops,
;;                       `condition-case' handlers, nested functions, or
;;                       after an early return (`cl-return-from'/`throw').
;;   hooks-callers       Hooks may only be called from components and custom
;;                       hooks (`reactel-defhook', or a function named
;;                       `...use-...').
;;   hooks-as-values     Hooks must not be passed around as values.
;;   component-call      Components must not be called directly.
;;   exhaustive-deps     The callback of `reactel-use-effect', `-layout-effect',
;;                       `-memo' and `-callback' must list in its dependencies
;;                       every prop, state value and derived value it uses.
;;                       Setters, dispatch functions and refs are stable and
;;                       exempt (like eslint-plugin-react-hooks).
;;
;; The analysis runs automatically when `reactel-defcomponent',
;; `reactel-lambda' or `reactel-defhook' is macroexpanded, so warnings show
;; up while byte-compiling or evaluating.  `reactel-lint-file' and
;; `reactel-lint-flymake' also check plain `defun's (hooks-callers), and
;; `make lint' runs `reactel-lint-batch'.
;;
;; A hook is a symbol starting with `reactel-use-', or one registered with
;; `reactel-defhook'.  Opt out per component with `:lint nil'.
;;
;; The check is conservative like React's: a balanced
;; (if c (reactel-use-state 1) (reactel-use-state 2)) is still a violation.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup reactel-lint nil "Static checks for the Rules of React." :group 'reactel)

(defcustom reactel-lint-severity 'warn
  "What to do about a Rules of React violation found while macroexpanding.
`warn' reports a warning (byte-compile warning or `display-warning'),
`error' signals an error so the definition fails, nil disables the checks."
  :type '(choice (const :tag "Warn" warn) (const :tag "Error" error) (const :tag "Off" nil))
  :group 'reactel-lint)

(defcustom reactel-lint-exhaustive-deps t
  "Whether to check that hook dependency lists are exhaustive."
  :type 'boolean
  :group 'reactel-lint)

(defvar reactel-lint-reporter #'reactel-lint--default-reporter
  "Function called with (MESSAGE FORM) for each diagnostic; returns FORM.
The default wraps FORM with a compile-time warning.  Tests and the
file linter rebind it to collect messages.")

;;;; Classification

(defun reactel-lint-hook-p (symbol)
  "Non-nil if SYMBOL names a hook."
  (and (symbolp symbol) symbol
       (or (get symbol 'reactel-hook)
           (string-prefix-p "reactel-use-" (symbol-name symbol)))))

(defun reactel-lint-hook-like-name-p (symbol)
  "Non-nil if SYMBOL is named like a custom hook (`use-' segment)."
  (string-match-p "\\(\\`\\|-\\)use-" (symbol-name symbol)))

(defun reactel-lint-component-p (symbol)
  "Non-nil if SYMBOL names a component defined by `reactel-defcomponent'."
  (and (symbolp symbol) symbol (get symbol 'reactel-component)))

;;;; The walker

(defvar reactel-lint--diags nil "Diagnostics found, newest first: (RULE MESSAGE).")
(defvar reactel-lint--exited nil "Non-nil once an early return may have happened.")
(defvar reactel-lint--tags nil "`catch' tags established inside the analyzed body.")
(defvar reactel-lint--in-fn nil "Non-nil while inside a nested function.")
(defvar reactel-lint--mode 'component
  "`component' when hooks are allowed at the top level, `plain' when never.")

(defun reactel-lint--note (rule fmt &rest args)
  "Record a diagnostic for RULE with a message made from FMT and ARGS."
  (push (list rule (apply #'format fmt args)) reactel-lint--diags))

(defun reactel-lint--walk-seq (forms ctx)
  "Walk FORMS, evaluated in order, in context CTX."
  (dolist (f forms) (reactel-lint--walk f ctx)))

(defun reactel-lint--walk-function (f)
  "Walk the operand F of a `function' form."
  (cond ((and (consp f) (eq (car f) 'lambda))
         (let ((reactel-lint--in-fn t))
           (reactel-lint--walk-seq (cddr f) "inside a nested function")))
        ((reactel-lint-hook-p f)
         (reactel-lint--note 'hooks-as-values
                             "Hook `%s' is used as a value; hooks can only be called, not passed around" f))))

(defun reactel-lint--check-call (head args ctx)
  "Check the call of HEAD with already-walked ARGS in context CTX."
  (cond
   ((reactel-lint-hook-p head)
    (cond
     ((eq reactel-lint--mode 'plain)
      (reactel-lint--note 'hooks-callers
                          "Hook `%s' is called from a function that is not a component or a custom hook (use `reactel-defcomponent' or `reactel-defhook')"
                          head))
     (ctx (reactel-lint--note 'hooks-top-level
                              "Hook `%s' is called %s; hooks must be called in the same order on every render"
                              head ctx))
     (reactel-lint--exited
      (reactel-lint--note 'hooks-top-level
                          "Hook `%s' is called after a possible early return; hooks must be called in the same order on every render"
                          head))))
   ((reactel-lint-component-p head)
    (reactel-lint--note 'component-call
                        "Component `%s' is called directly; render it with (reactel-el '%s ...)"
                        head head))
   ((and (memq head '(funcall apply)) (consp (car args))
         (memq (car (car args)) '(function quote))
         (symbolp (cadr (car args))))
    (let ((target (cadr (car args))))
      (cond ((reactel-lint-component-p target)
             (reactel-lint--note 'component-call
                                 "Component `%s' is called directly through `%s'" target head))
            ((reactel-lint-hook-p target)
             (reactel-lint--note 'hooks-as-values
                                 "Hook `%s' is passed to `%s'; hooks must be called directly" target head)))))))

(defun reactel-lint--walk (form ctx)
  "Walk the macroexpanded FORM.  CTX is nil in unconditional positions,
else a phrase saying why FORM is not always evaluated exactly once."
  (when (and (consp form) (proper-list-p form))
    (let ((head (car form)) (args (cdr form)))
      (pcase head
        ('quote nil)
        ('function (reactel-lint--walk-function (car args)))
        ((or 'progn 'prog1 'prog2 'save-excursion 'save-restriction
             'save-current-buffer 'unwind-protect)
         (reactel-lint--walk-seq args ctx))
        ('if (reactel-lint--walk (car args) ctx)
            (reactel-lint--walk-seq (cdr args) "inside a conditional branch"))
        ('cond (cl-loop for clause in args
                        for first = t then nil
                        do (when (consp clause)
                             (reactel-lint--walk (car clause) (if first ctx "inside a conditional branch"))
                             (reactel-lint--walk-seq (cdr clause) "inside a conditional branch"))))
        ((or 'and 'or) (when args
                         (reactel-lint--walk (car args) ctx)
                         (reactel-lint--walk-seq (cdr args) "inside a conditional branch")))
        ('while (reactel-lint--walk-seq args "inside a loop"))
        ((or 'let 'let*)
         (dolist (b (car args))
           (when (consp b) (reactel-lint--walk-seq (cdr b) ctx)))
         (reactel-lint--walk-seq (cdr args) ctx))
        ('setq (cl-loop for (_ v) on args by #'cddr do (reactel-lint--walk v ctx)))
        ('catch (reactel-lint--walk (car args) ctx)
          (let ((reactel-lint--tags
                 (if (and (consp (car args)) (eq (car (car args)) 'quote))
                     (cons (cadr (car args)) reactel-lint--tags)
                   reactel-lint--tags)))
            (reactel-lint--walk-seq (cdr args) ctx)))
        ('throw (reactel-lint--walk-seq args ctx)
                (unless (or reactel-lint--in-fn
                            (and (consp (car args)) (eq (car (car args)) 'quote)
                                 (memq (cadr (car args)) reactel-lint--tags)))
                  (setq reactel-lint--exited t)))
        ('condition-case
            (reactel-lint--walk (cadr args) ctx)
            (dolist (handler (cddr args))
              (when (consp handler)
		(reactel-lint--walk-seq (cdr handler) "inside a condition-case handler"))))
        ((or 'interactive 'declare 'defvar 'defconst) nil)
        ((pred consp)                   ; ((lambda (..) ..) args...)
         (reactel-lint--walk-seq args ctx)
         (when (eq (car head) 'lambda)
           (reactel-lint--walk-seq (cddr head) ctx)))
        (_ (reactel-lint--walk-seq args ctx)
           (reactel-lint--check-call head args ctx))))))

;;;; Exhaustive dependencies

(defconst reactel-lint--lambda-keywords
  '(&optional &rest &key &aux &body &allow-other-keys &environment &whole)
  "Markers in lambda lists that are not variables.")

(defun reactel-lint--arglist-vars (args)
  "The variables bound by the (cl) lambda list ARGS."
  (let (vars)
    (dolist (a args)
      (cond ((memq a reactel-lint--lambda-keywords))
            ((symbolp a) (push a vars))
            ((consp a)
             (cond ((symbolp (car a)) (push (car a) vars))
                   ((and (consp (car a)) (symbolp (cadr (car a)))) (push (cadr (car a)) vars)))
             (when (and (cddr a) (symbolp (nth 2 a)) (nth 2 a)) (push (nth 2 a) vars)))))
    vars))

(defun reactel-lint--free-vars (form &optional bound)
  "Variables referenced in the macroexpanded FORM that are not in BOUND."
  (let (out)
    (cl-labels
        ((walk-seq (forms bound) (dolist (f forms) (walk f bound)))
         (walk (f bound)
           (cond
            ((symbolp f)
             (when (and f (not (keywordp f)) (not (eq f t)) (not (memq f bound)))
               (cl-pushnew f out)))
            ((not (and (consp f) (proper-list-p f))) nil)
            (t (pcase (car f)
                 ('quote nil)
                 ('function
                  (let ((x (cadr f)))
                    (when (and (consp x) (eq (car x) 'lambda))
                      (walk-seq (cddr x)
                                (append (reactel-lint--arglist-vars (cadr x)) bound)))))
                 ('let
                     (dolist (b (cadr f)) (when (consp b) (walk (cadr b) bound)))
                   (walk-seq (cddr f)
                             (append (mapcar (lambda (b) (if (consp b) (car b) b)) (cadr f))
                                     bound)))
                 ('let*
                     (let ((bound bound))
                       (dolist (b (cadr f))
			 (when (consp b) (walk (cadr b) bound))
			 (push (if (consp b) (car b) b) bound))
                       (walk-seq (cddr f) bound)))
                 ('setq (cl-loop for (v val) on (cdr f) by #'cddr
                                 do (walk v bound) (walk val bound)))
                 ('cond (dolist (clause (cdr f)) (when (consp clause) (walk-seq clause bound))))
                 ('condition-case
                     (walk (nth 2 f) bound)
                     (dolist (h (nthcdr 3 f))
                       (when (consp h) (walk-seq (cdr h) (cons (cadr f) bound)))))
                 ((pred consp)
                  (when (eq (car (car f)) 'lambda)
                    (walk-seq (cddr (car f))
                              (append (reactel-lint--arglist-vars (cadr (car f))) bound)))
                  (walk-seq (cdr f) bound))
                 (_ (walk-seq (cdr f) bound)))))))
      (walk form bound))
    (nreverse out)))

(defun reactel-lint--abstract (form env)
  "Classify what FORM evaluates to, given the variable environment ENV.
Returns a vector: [tail K] (a hook result list from which K elements were
taken), [elem I] (element I of such a list), [stable], [reactive], or nil."
  (pcase form
    ((pred symbolp) (cdr (assq form env)))
    (`(,(or 'reactel-use-state 'reactel-use-reducer) . ,_) (vector 'tail 0))
    (`(reactel-use-ref . ,_) (vector 'stable))
    (`(,(or 'reactel-use-memo 'reactel-use-callback 'reactel-use-context
            'reactel-use-sync-external-store)
       . ,_)
     (vector 'reactive))
    (`(if ,_ ,inner (signal . ,_)) (reactel-lint--abstract inner env))
    (`(,(or 'car-safe 'car) (prog1 ,(and v (pred symbolp)) (setq ,v (cdr ,v))))
     (let ((a (cdr (assq v env))))
       (when (and (vectorp a) (eq (aref a 0) 'tail))
         (prog1 (vector 'elem (aref a 1)) (aset a 1 (1+ (aref a 1)))))))
    (`(,(or 'car-safe 'car) ,x)
     (let ((a (reactel-lint--abstract x env)))
       (when (and (vectorp a) (eq (aref a 0) 'tail)) (vector 'elem (aref a 1)))))
    (`(,(or 'cdr-safe 'cdr) ,x)
     (let ((a (reactel-lint--abstract x env)))
       (when (and (vectorp a) (eq (aref a 0) 'tail)) (vector 'tail (1+ (aref a 1))))))
    (`(cadr ,x)
     (let ((a (reactel-lint--abstract x env)))
       (when (and (vectorp a) (eq (aref a 0) 'tail)) (vector 'elem (1+ (aref a 1))))))
    (`(nth ,(and i (pred integerp)) ,x)
     (let ((a (reactel-lint--abstract x env)))
       (when (and (vectorp a) (eq (aref a 0) 'tail)) (vector 'elem (+ i (aref a 1))))))))

(defun reactel-lint--reactive-p (var env)
  "Non-nil if VAR (bound in ENV) can change between renders and so is a dependency."
  (let ((a (cdr (assq var env))))
    (cond ((null a) t)                  ; props, plain `let' variables
          ((not (vectorp a)) t)
          ((eq (aref a 0) 'elem) (= (aref a 1) 0)) ; the value, not the setter
          ((eq (aref a 0) 'reactive) t)
          (t nil))))                    ; refs, unclassified hook results

(defun reactel-lint--deps-vars (deps)
  "The symbols mentioned in the dependency form DEPS."
  (let (out)
    (cl-labels ((walk (f) (cond ((and f (symbolp f)) (push f out))
                                ((consp f) (walk (car f)) (walk (cdr f))))))
      (walk deps))
    out))

(defun reactel-lint--check-deps (head fn deps env)
  "Check that the callback FN of hook HEAD lists what it uses in DEPS."
  (when (and (consp fn) (eq (car fn) 'function) (consp (cadr fn))
             (eq (car (cadr fn)) 'lambda)
             (or (null deps) (eq (car-safe deps) 'list)))
    (let* ((used (reactel-lint--free-vars fn))
           (listed (reactel-lint--deps-vars deps))
           (missing (cl-remove-if-not
                     (lambda (v) (and (assq v env)
                                      (reactel-lint--reactive-p v env)
                                      (not (memq v listed))))
                     used)))
      (when missing
        (reactel-lint--note
         'exhaustive-deps
         "Hook `%s' uses %s but %s not in its dependency list; add %s or the callback may see stale values"
         head
         (mapconcat (lambda (v) (format "`%s'" v)) missing ", ")
         (if (cdr missing) "they are" "it is")
         (if (cdr missing) "them" "it"))))))

(defun reactel-lint--deps-walk (form env)
  "Check the dependency lists of hooks in the macroexpanded FORM.
ENV maps the variables in scope to their `reactel-lint--abstract' class."
  (when (and (consp form) (proper-list-p form))
    (let ((head (car form)) (args (cdr form)))
      (cl-flet ((walk-seq (forms env) (dolist (f forms) (reactel-lint--deps-walk f env))))
        (pcase head
          ('quote nil)
          ('function
           (let ((x (car args)))
             (when (and (consp x) (eq (car x) 'lambda))
               (walk-seq (cddr x)
                         (append (mapcar #'list (reactel-lint--arglist-vars (cadr x))) env)))))
          ((or 'let 'let*)
           (let ((new env))
             (dolist (b (car args))
               (let* ((var (if (consp b) (car b) b))
                      (init (and (consp b) (cadr b)))
                      (scope (if (eq head 'let*) new env)))
                 (reactel-lint--deps-walk init scope)
                 (push (cons var (reactel-lint--abstract init scope)) new)))
             (walk-seq (cdr args) new)))
          ('cond (dolist (clause args) (when (consp clause) (walk-seq clause env))))
          ('condition-case
              (reactel-lint--deps-walk (cadr args) env)
              (dolist (h (cddr args))
		(when (consp h) (walk-seq (cdr h) (cons (list (car args)) env)))))
          ((or 'reactel-use-effect 'reactel-use-layout-effect
               'reactel-use-memo 'reactel-use-callback)
           (when (= (length args) 2)
             (reactel-lint--check-deps head (car args) (cadr args) env))
           (walk-seq args env))
          ((pred consp)
           (when (eq (car head) 'lambda) (walk-seq (cddr head) env))
           (walk-seq args env))
          (_ (walk-seq args env)))))))

;;;; Entry points

(defun reactel-lint-forms (forms &optional mode env bound)
  "Analyze body FORMS and return a list of diagnostics (RULE MESSAGE).
MODE is `component' (the default: hooks allowed at the top level) or
`plain' (hooks never allowed).  ENV is the macro environment and BOUND
the variables of the function's lambda list (props)."
  (let ((reactel-lint--diags nil)
        (reactel-lint--exited nil)
        (reactel-lint--tags nil)
        (reactel-lint--in-fn nil)
        (reactel-lint--mode (or mode 'component))
        ;; Nested components/lambdas report through the same list.
        (reactel-lint-reporter (lambda (msg form)
                                 (push (list 'nested msg) reactel-lint--diags)
                                 form)))
    (condition-case err
        (let ((expanded (macroexpand-all `(progn ,@forms) env)))
          (reactel-lint--walk expanded nil)
          (when (and reactel-lint-exhaustive-deps (eq reactel-lint--mode 'component))
            (reactel-lint--deps-walk expanded (mapcar #'list bound))))
      (error (push (list 'unanalyzable
                         (format "cannot analyze: %s" (error-message-string err)))
                   reactel-lint--diags)))
    (nreverse reactel-lint--diags)))

(defun reactel-lint--default-reporter (message form)
  "Wrap FORM with a compile-time warning MESSAGE."
  (macroexp-warn-and-return message form))

(defun reactel-lint-wrap (what forms expansion &optional mode bound)
  "Lint body FORMS of WHAT (a description) and return EXPANSION, wrapped.
BOUND lists the variables of the lambda list.  Used by the defining
macros while they expand.  Honors `reactel-lint-severity'."
  (when reactel-lint-severity
    (dolist (d (reactel-lint-forms forms mode macroexpand-all-environment bound))
      (unless (eq (car d) 'unanalyzable)
        (let ((msg (format "reactel: %s: %s" what (cadr d))))
          (if (eq reactel-lint-severity 'error)
              (error "%s" msg)
            (setq expansion (funcall reactel-lint-reporter msg expansion)))))))
  expansion)

;;;; Linting files and buffers

(defun reactel-lint--toplevel (form)
  "Return the diagnostics for the top-level FORM, and its name."
  (pcase form
    (`(,(or 'reactel-defcomponent 'reactel-defhook) ,name ,args . ,body)
     (when (stringp (car body)) (setq body (cdr body)))
     (let ((lint t))
       (while (keywordp (car body))
         (when (and (eq (car body) :lint) (null (cadr body))) (setq lint nil))
         (setq body (cddr body)))
       (when lint
         (cons name (reactel-lint-forms body 'component nil
                                        (reactel-lint--arglist-vars args))))))
    (`(,(or 'defun 'cl-defun 'defsubst 'cl-defsubst) ,name ,_args . ,body)
     (unless (or (reactel-lint-hook-p name) (reactel-lint-hook-like-name-p name)
                 (reactel-lint-component-p name))
       (cons name (reactel-lint-forms body 'plain))))))

(defun reactel-lint-buffer-diagnostics ()
  "Lint the forms of the current buffer.
Return a list of (START END NAME RULE MESSAGE)."
  (let (out)
    (save-excursion
      (goto-char (point-min))
      (condition-case nil
          (while t
            (forward-comment (buffer-size))
            (let* ((start (point))
                   (form (read (current-buffer)))
                   (end (point)))
              (dolist (f (if (eq (car-safe form) 'progn) (cdr form) (list form)))
                (when-let ((r (and (consp f) (reactel-lint--toplevel f))))
                  (dolist (d (cdr r))
                    (unless (memq (car d) '(unanalyzable))
                      (push (list start end (car r) (car d) (cadr d)) out)))))))
        (end-of-file nil)))
    (nreverse out)))

(defun reactel-lint-file (file)
  "Lint FILE; return a list of (FILE LINE NAME RULE MESSAGE)."
  (with-temp-buffer
    (insert-file-contents file)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (mapcar (lambda (d)
              (list file (line-number-at-pos (nth 0 d)) (nth 2 d) (nth 3 d) (nth 4 d)))
            (reactel-lint-buffer-diagnostics))))

(defun reactel-lint-batch ()
  "Lint the files named on the command line and exit nonzero on findings."
  (let ((count 0))
    (dolist (file command-line-args-left)
      (dolist (d (reactel-lint-file file))
        (cl-incf count)
        (message "%s:%d: warning: %s (in `%s')" (nth 0 d) (nth 1 d) (nth 4 d) (nth 2 d))))
    (setq command-line-args-left nil)
    (kill-emacs (if (zerop count) 0 1))))

(defun reactel-lint-flymake (report-fn &rest _args)
  "Flymake backend for the Rules of React.  Add to `flymake-diagnostic-functions'."
  (require 'flymake)
  (funcall report-fn
           (mapcar (lambda (d)
                     (flymake-make-diagnostic (current-buffer) (nth 0 d) (nth 1 d) :warning
                                              (format "reactel (%s): %s" (nth 3 d) (nth 4 d))))
                   (reactel-lint-buffer-diagnostics))))

(provide 'reactel-lint)
;;; reactel-lint.el ends here
