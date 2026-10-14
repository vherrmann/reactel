;;; reactel-ex-jump.el --- Reactel example: refs and layout effects -*- lexical-binding: t -*-

;; M-x reactel-demo-jump
;;
;; A `reactel-ref' passed as `:ref' receives the painted host node.  A layout
;; effect runs right after painting, so it can move point to that node.

(require 'dash)
(require 'reactel)
(require 'reactel-buffer)

(reactel-defcomponent reactel-ex-jump (&key (rows 40) (start 25))
  (-let* (((selected set-selected) (reactel-use-state start))
          (row-ref (reactel-use-ref nil))
          (step (reactel-use-callback* (rows) (delta)
                  (funcall set-selected (lambda (s) (1+ (mod (+ s delta -1) rows)))))))
    (reactel-use-layout-effect* (selected)
      (reactel-buffer-goto-node (reactel-ref-current row-ref))
      nil)
    (reactel-c-vstack
      (reactel-c-hstack
	(reactel-c-button :on-click (lambda () (funcall step -1)) "prev")
	(reactel-c-button :on-click (lambda () (funcall step 1)) "next")
	(reactel-c-text (format "selected: %d" selected)))
      (--map (reactel-c-text :key it
               :ref (and (= it selected) row-ref)
               :face (and (= it selected) 'highlight)
               (format "row %02d" it))
             (number-sequence 1 rows)))))

(defun reactel-demo-jump ()
  "Show the refs example."
  (interactive)
  (reactel-show "*reactel-jump*" (reactel-el 'reactel-ex-jump)))

(provide 'reactel-ex-jump)
;;; reactel-ex-jump.el ends here
