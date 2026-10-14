;;; reactel-ex-compiler.el --- Reactel example: automatic memoization -*- lexical-binding: t -*-

;; M-x reactel-demo-compiler
;;
;; The same panel is defined twice from one body, once plain and once with
;; `:compile t' (the Reactel counterpart of the React Compiler).  Both show
;; a counter, a static header, and a row component whose renders are counted
;; (see `reactel-ex-render-counter.el').  Press "tick" in each panel: in the plain one the rows render
;; again every time because the parent builds new elements and closures; in
;; the compiled one they are built once and the reconciler skips them.
;; `M-x reactel-compile-show' displays the rewritten code.

(require 'dash)
(require 'reactel)
(require 'reactel-c)
(require 'reactel-buffer)
(require 'reactel-ex-render-counter)

(reactel-defcomponent reactel-ex-compiler-row (&key panel name on-pick)
  (reactel-ex-use-render-count (cons panel name))
  (reactel-c-hstack
    (reactel-c-button :on-click (lambda () (funcall on-pick name)) name)
    (reactel-el 'reactel-ex-render-count :counter-key (cons panel name))))

(defmacro reactel-ex-compiler--define-panel (name compile)
  "Define the component NAME, compiled when COMPILE is non-nil."
  `(reactel-defcomponent ,name (&key panel)
     :compile ,compile
     (-let* (((ticks set-ticks) (reactel-use-state 0))
             ((picked set-picked) (reactel-use-state "none"))
             (rows '("alpha" "beta" "gamma")))
       (reactel-c-box :title (format "%s (%d ticks)" panel ticks)
         (reactel-c-vstack
           (reactel-c-text :face 'bold "Rows (the parent re-renders on every tick)")
           (--map (reactel-el 'reactel-ex-compiler-row :key it :panel panel :name it
                    :on-pick (lambda (n) (funcall set-picked n)))
                  rows)
           (reactel-c-hstack
             (reactel-c-button :on-click (lambda () (funcall set-ticks reactel-inc)) "tick")
             (reactel-c-text (format "picked: %s" picked))))))))

(reactel-ex-compiler--define-panel reactel-ex-compiler-plain nil)
(reactel-ex-compiler--define-panel reactel-ex-compiler-compiled t)

(reactel-defcomponent reactel-ex-compiler ()
  (reactel-c-vstack :spacing 1
    (reactel-el 'reactel-ex-compiler-plain :panel "plain")
    (reactel-el 'reactel-ex-compiler-compiled :panel "compiled")))

(defun reactel-demo-compiler ()
  "Show the compiler example."
  (interactive)
  (reactel-ex-render-counter-reset)
  (reactel-show "*reactel-compiler*" (reactel-el 'reactel-ex-compiler)))

(provide 'reactel-ex-compiler)
;;; reactel-ex-compiler.el ends here
