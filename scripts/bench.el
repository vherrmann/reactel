;;; bench.el --- Repaint benchmark of the buffer backend -*- lexical-binding: t -*-

;; Run with `make bench'.  Renders a list of N rows into a buffer and times
;; repaints after a state change, reporting time, GCs and allocation.

(require 'benchmark)
(require 'reactel)
(require 'reactel-buffer)

(defvar reactel-bench--set nil "Setter of the benchmark's tick state.")

(reactel-defcomponent reactel-bench-row (&key i tick)
  (reactel-c-hstack
    (reactel-c-button :on-click #'ignore (format "%d" i))
    (reactel-c-text :face 'bold (format "row %d, tick %d\nsecond line" i tick))))

(reactel-defcomponent reactel-bench-app (&key n)
  (let* ((state (reactel-use-state 0))
         (tick (car state)))
    (reactel-use-effect (lambda () (setq reactel-bench--set (cadr state)) nil) nil)
    (reactel-c-vstack :indent 2
      (reactel-c-heading "Bench")
      (apply #'reactel-c-list
             (mapcar (lambda (i) (reactel-el 'reactel-bench-row :key i :i i :tick tick))
                     (number-sequence 1 n))))))

(defun reactel-bench-run ()
  "Time repaints of `reactel-bench-app' and print the results."
  (let ((buf "*reactel-bench*")
        (n 500) (reps 20))
    (reactel-render-buffer buf (reactel-el 'reactel-bench-app :n n))
    (reactel-flush)
    (with-current-buffer buf (buffer-enable-undo))
    (garbage-collect)
    (let* ((cons-before cons-cells-consed)
           (strings-before string-chars-consed)
           (res (benchmark-run reps
                  (reactel-batch (funcall reactel-bench--set reactel-inc)))))
      (princ (format "%d repaints of %d rows: %.3fs (%.1fms each), %d GCs (%.3fs)\n"
                     reps n (car res) (* 1000 (/ (car res) reps)) (nth 1 res) (nth 2 res)))
      (princ (format "consed per repaint: %d cons cells, %d string chars\n"
                     (/ (- cons-cells-consed cons-before) reps)
                     (/ (- string-chars-consed strings-before) reps)))
      (with-current-buffer buf
        (princ (format "undo list length: %s\n"
                       (if (listp buffer-undo-list) (length buffer-undo-list) buffer-undo-list)))))
    (kill-buffer buf)))

;;; bench.el ends here
