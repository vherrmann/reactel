;;; reactel-ex-store.el --- Reactel example: external state via use-sync-external-store -*- lexical-binding: t -*-

;; M-x reactel-demo-store, then evaluate (reactel-ex-store-set 42) anywhere.
;;
;; The state lives outside Reactel, in an ordinary variable plus a hook.
;; Components subscribe with `reactel-use-sync-external-store' and stay in
;; sync, even when the variable changes from outside the UI.

(require 'dash)
(require 'reactel)
(require 'reactel-buffer)

(defvar reactel-ex-store-value 0 "The external state.")
(defvar reactel-ex-store-change-hook nil "Run after `reactel-ex-store-value' changes.")

(defun reactel-ex-store-set (value)
  "Set the external VALUE and notify subscribers."
  (setq reactel-ex-store-value value)
  (run-hooks 'reactel-ex-store-change-hook))

(defun reactel-ex-store-subscribe (callback)
  "Call CALLBACK on changes; return a function that stops that."
  (add-hook 'reactel-ex-store-change-hook callback)
  (lambda () (remove-hook 'reactel-ex-store-change-hook callback)))

(defun reactel-ex-store-snapshot () "The current value." reactel-ex-store-value)

(reactel-defcomponent reactel-ex-store-view (&key label (fn #'identity))
  (reactel-c-row label (format "%s" (funcall fn (reactel-use-sync-external-store
						 #'reactel-ex-store-subscribe #'reactel-ex-store-snapshot)))))

(reactel-defcomponent reactel-ex-store ()
  (reactel-c-box :title "External store"
    (reactel-c-vstack
      (reactel-c-table :header nil :separator ": "
	(reactel-el 'reactel-ex-store-view :label "value")
	(reactel-el 'reactel-ex-store-view :label "squared" :fn (lambda (v) (* v v)))
	(reactel-el 'reactel-ex-store-view :label "octal" :fn (lambda (v) (format "%o" v))))
      (reactel-c-hstack
	(reactel-c-button :on-click (lambda () (reactel-ex-store-set (1+ reactel-ex-store-value))) "+1")
	(reactel-c-button :on-click (lambda () (reactel-ex-store-set 0)) "reset")))))

(defun reactel-demo-store ()
  "Show the external store example."
  (interactive)
  (reactel-show "*reactel-store*" (reactel-el 'reactel-ex-store)))

(provide 'reactel-ex-store)
;;; reactel-ex-store.el ends here
