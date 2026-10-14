;;; reactel-ex-render-counter.el --- Counting renders without breaking the rules -*- lexical-binding: t -*-

;; Helpers for the examples that show how often a component renders.
;;
;; Counting renders by writing a ref (or a variable) while rendering is what
;; the Rules of React forbid.  The accepted way is:
;;
;;   1. the counted component notes each render *in an effect*, where side
;;      effects belong: `reactel-ex-use-render-count';
;;   2. the count lives outside Reactel, in a table plus a hook;
;;   3. a *separate* component shows it, subscribing with
;;      `reactel-use-sync-external-store': `reactel-ex-render-count'.
;;
;; Showing the count re-renders only that small component, never the counted
;; one, so there is no feedback loop.  Both pass lint and strict mode.

(require 'reactel)
(require 'reactel-c)

(defvar reactel-ex-render-counter--counts (make-hash-table :test 'equal)
  "Number of renders by key.")

(defvar reactel-ex-render-counter-change-hook nil
  "Run after a count changed.")

(defun reactel-ex-render-counter-get (key)
  "The number of renders noted for KEY."
  (gethash key reactel-ex-render-counter--counts 0))

(defun reactel-ex-render-counter-reset ()
  "Forget all counts."
  (clrhash reactel-ex-render-counter--counts)
  (run-hooks 'reactel-ex-render-counter-change-hook))

(defun reactel-ex-render-counter--note (key)
  "Count one more render for KEY and tell the subscribers."
  (puthash key (1+ (reactel-ex-render-counter-get key)) reactel-ex-render-counter--counts)
  (run-hooks 'reactel-ex-render-counter-change-hook))

(defun reactel-ex-render-counter--subscribe (callback)
  "Call CALLBACK when a count changes; return a function that stops that."
  (add-hook 'reactel-ex-render-counter-change-hook callback)
  (lambda () (remove-hook 'reactel-ex-render-counter-change-hook callback)))

(reactel-defhook reactel-ex-use-render-count (key)
  "Count the renders of the calling component under KEY (in an effect)."
  (reactel-use-effect* :always
    (reactel-ex-render-counter--note key)
    nil))

(reactel-defcomponent reactel-ex-render-count (&key counter-key)
  "Show how often the component counting under COUNTER-KEY rendered."
  (let ((get-count (reactel-use-callback* (counter-key) ()
                     (reactel-ex-render-counter-get counter-key))))
    (reactel-c-text :face 'shadow
      (format "(rendered %dx)"
              (reactel-use-sync-external-store
               #'reactel-ex-render-counter--subscribe get-count)))))

(provide 'reactel-ex-render-counter)
;;; reactel-ex-render-counter.el ends here
