;;; reactel-ex-stopwatch.el --- Reactel example: reducer, timer effect with cleanup -*- lexical-binding: t -*-

;; M-x reactel-demo-stopwatch

(require 'dash)
(require 'reactel)
(require 'reactel-buffer)

(defun reactel-ex-stopwatch--reduce (state action)
  "Pure transition of the stopwatch STATE (time in tenths of a second) on ACTION."
  (-let (((&plist :elapsed elapsed :running running :laps laps) state))
    (pcase action
      ('tick  (if running (plist-put (copy-sequence state) :elapsed (1+ elapsed)) state))
      ('toggle (plist-put (copy-sequence state) :running (not running)))
      ('lap   (if running (plist-put (copy-sequence state) :laps (cons elapsed laps)) state))
      ('reset (list :elapsed 0 :running nil :laps nil))
      (_ (error "Unknown stopwatch action %S" action)))))

(defun reactel-ex-stopwatch--format (tenths)
  "Format TENTHS of a second as MM:SS.t."
  (format "%02d:%02d.%d" (/ tenths 600) (% (/ tenths 10) 60) (% tenths 10)))

(reactel-defcomponent reactel-ex-stopwatch (&key (interval 0.1))
  "Start/stop/lap/reset stopwatch ticking every INTERVAL seconds."
  (-let* (((state dispatch)
           (reactel-use-reducer #'reactel-ex-stopwatch--reduce (list :elapsed 0 :running nil :laps nil)))
          ((&plist :elapsed elapsed :running running :laps laps) state))
    ;; The timer only exists while running; the returned lambda is the
    ;; cleanup, run when RUNNING changes or the buffer is killed.
    (reactel-use-effect* (running interval)
      (when running
        (let ((timer (run-with-timer interval interval
                                     (lambda () (funcall dispatch 'tick)))))
          (lambda () (cancel-timer timer)))))
    (reactel-c-box :title "Stopwatch"
      (reactel-c-vstack
	(reactel-c-text :face 'bold (reactel-ex-stopwatch--format elapsed))
	(reactel-c-hstack
          (reactel-c-button :on-click (lambda () (funcall dispatch 'toggle))
            (if running "stop" "start"))
          (reactel-c-button :on-click (lambda () (funcall dispatch 'lap)) :disabled (not running) "lap")
          (reactel-c-button :on-click (lambda () (funcall dispatch 'reset)) "reset"))
	(when laps
          (reactel-c-table :align '(right left)
            (reactel-c-row "Lap" "Time")
            (--map-indexed
             (reactel-c-row :key it (- (length laps) it-index) (reactel-ex-stopwatch--format it))
             laps)))))))

(defun reactel-demo-stopwatch ()
  "Show the stopwatch example."
  (interactive)
  (reactel-show "*reactel-stopwatch*" (reactel-el 'reactel-ex-stopwatch)))

(provide 'reactel-ex-stopwatch)
;;; reactel-ex-stopwatch.el ends here
