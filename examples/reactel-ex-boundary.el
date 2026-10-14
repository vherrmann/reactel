;;; reactel-ex-boundary.el --- Reactel example: error boundaries -*- lexical-binding: t -*-

;; M-x reactel-demo-boundary
;;
;; A component that signals an error is replaced by the boundary's fallback
;; instead of taking the whole UI down.

(require 'dash)
(require 'reactel)
(require 'reactel-buffer)

(reactel-defcomponent reactel-ex-boundary-fragile (&key n)
  (when (>= n 3)
    (error "I cannot count past 2 (got %d)" n))
  (reactel-c-text (format "n = %d" n)))

(reactel-defcomponent reactel-ex-boundary ()
  (-let (((n set-n) (reactel-use-state 0))
         ((caught set-caught) (reactel-use-state 0)))
    (reactel-c-vstack
      (reactel-c-hstack
	(reactel-c-button :on-click (lambda () (funcall set-n reactel-inc)) "increment")
	(reactel-c-button :on-click (lambda () (funcall set-n 0)) "zero"))
      (reactel-c-error-boundary
	;; Changing :reset-keys (here: N) leaves the error state.
	:reset-keys (list n)
	:on-error (lambda (_err) (funcall set-caught reactel-inc))
	:fallback (lambda (err reset)
                    (reactel-c-box :border 'double :title "error"
                      (reactel-c-vstack
			(reactel-c-text :face 'error (error-message-string err))
			(reactel-c-button :on-click reset "retry"))))
	(reactel-c-box (reactel-el 'reactel-ex-boundary-fragile :n n)))
      (reactel-c-text :face 'shadow (format "errors caught: %d" caught)))))

(defun reactel-demo-boundary ()
  "Show the error boundary example."
  (interactive)
  (reactel-show "*reactel-boundary*" (reactel-el 'reactel-ex-boundary)))

(provide 'reactel-ex-boundary)
;;; reactel-ex-boundary.el ends here
