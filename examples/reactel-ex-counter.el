;;; reactel-ex-counter.el --- Reactel example: state, callbacks and effects -*- lexical-binding: t -*-

;; M-x reactel-demo-counter
;;
;; Shows `reactel-use-state', `reactel-use-callback*', `reactel-use-effect*'
;; and `-let*' from dash.el to unpack hooks.

(require 'dash)
(require 'reactel)
(require 'reactel-buffer)

(reactel-defcomponent reactel-ex-counter (&key (step 1))
  "A counter that remembers its last five values."
  (-let* (((count set-count) (reactel-use-state 0))
          ((history set-history) (reactel-use-state nil))
          ;; Stable between renders as long as STEP is unchanged.
          (bump (reactel-use-callback* (step) (sign)
                  (funcall set-count (lambda (n) (+ n (* sign step))))))
          (reset (reactel-use-callback* () () (funcall set-count 0))))
    ;; Runs after every change of COUNT, not after every render.
    (reactel-use-effect* (count)
      (funcall set-history (lambda (h) (-take 5 (cons count h))))
      nil)
    (reactel-c-box :title "Counter"
      (reactel-c-vstack
	(reactel-c-text :face 'bold (format "Count: %d" count))
	(reactel-c-hstack
          (reactel-c-button :on-click (lambda () (funcall bump -1)) "-")
          (reactel-c-button :on-click (lambda () (funcall bump 1)) "+")
          (reactel-c-button :on-click reset :disabled (zerop count) "reset"))
	(reactel-c-text :face 'shadow
          "history: "
          (->> history reverse (-map #'number-to-string) (-interpose " > ")
               (apply #'concat)))))))

(defun reactel-demo-counter ()
  "Show the counter example."
  (interactive)
  (reactel-show "*reactel-counter*" (reactel-el 'reactel-ex-counter :step 2)))

(provide 'reactel-ex-counter)
;;; reactel-ex-counter.el ends here
