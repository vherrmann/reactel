;;; reactel-ex-search.el --- Reactel example: debounced async search -*- lexical-binding: t -*-

;; M-x reactel-demo-search
;;
;; Search-as-you-type against a slow, sometimes failing "server".  It shows
;; how asynchronous work is written with hooks:
;;
;;   - `reactel-ex-use-debounced' returns a value that follows its input only
;;     after it stopped changing (a timer, cancelled by the effect's cleanup);
;;   - `reactel-ex-use-search' runs a request in an effect and keeps its
;;     status in a reducer; changing the query cancels the request in flight,
;;     so a slow old answer can never overwrite a newer one;
;;   - `reactel-ex-use-ticker' drives a spinner while loading;
;;   - failures are shown with a retry button, and a history of the searches
;;     that succeeded is kept in state.
;;
;; The server is `reactel-ex-search--request', a function with a delay; the
;; text "!" in a query makes it fail.

(require 'dash)
(require 'reactel)
(require 'reactel-c)
(require 'reactel-buffer)

(defconst reactel-ex-search-corpus
  '("emacs lisp" "emacs org-mode" "elisp macros" "reactel components"
    "react hooks" "reconciler" "regular expressions" "magit status"
    "markdown mode" "mode line" "macro expansion" "minibuffer completion")
  "What the fake server knows.")

(defvar reactel-ex-search-requests 0
  "Number of requests sent so far (for the demo and the tests).")

;;;; The "server"

(defun reactel-ex-search--request (query latency on-done)
  "Answer QUERY after LATENCY seconds by calling ON-DONE with (:ok RESULTS) or
\(:error MESSAGE).  Returns the timer, to cancel the request."
  (cl-incf reactel-ex-search-requests)
  (run-with-timer latency nil
                  (lambda ()
                    (funcall on-done
                             (if (string-match-p "!" query)
                                 (list :error "the server failed")
                               (list :ok (--filter (string-match-p (regexp-quote query) it)
                                                   reactel-ex-search-corpus)))))))

;;;; Hooks

(reactel-defhook reactel-ex-use-debounced (value delay)
  "VALUE, but only after it stayed unchanged for DELAY seconds."
  (-let (((settled set-settled) (reactel-use-state value)))
    (reactel-use-effect* (value delay)
      (let ((timer (run-with-timer delay nil (lambda () (funcall set-settled value)))))
        (lambda () (cancel-timer timer))))
    settled))

(defun reactel-ex-search--reduce (_state action)
  "The status after ACTION: start, (done RESULT)."
  (pcase action
    ('start (list :status 'loading))
    (`(done (:ok ,results)) (list :status 'done :results results))
    (`(done (:error ,message)) (list :status 'failed :message message))
    ('idle (list :status 'idle))
    (_ (error "Unknown search action %S" action))))

(reactel-defhook reactel-ex-use-search (query latency attempt)
  "The status of the search for QUERY: (:status idle|loading|done|failed ...).
Changing QUERY or ATTEMPT (a retry) cancels the request in flight."
  (-let (((state dispatch) (reactel-use-reducer #'reactel-ex-search--reduce '(:status idle))))
    (reactel-use-effect* (query latency attempt)
      (if (string-empty-p query)
          (progn (funcall dispatch 'idle) nil)
        (funcall dispatch 'start)
        (let ((timer (reactel-ex-search--request
                      query latency (lambda (result) (funcall dispatch `(done ,result))))))
          (lambda () (cancel-timer timer)))))
    state))

(reactel-defhook reactel-ex-use-ticker (active interval)
  "A counter that advances every INTERVAL seconds while ACTIVE."
  (-let (((ticks set-ticks) (reactel-use-state 0)))
    (reactel-use-effect* (active interval)
      (when active
        (let ((timer (run-with-timer interval interval
                                     (lambda () (funcall set-ticks reactel-inc)))))
          (lambda () (cancel-timer timer)))))
    ticks))

;;;; Components

(defconst reactel-ex-search-spinner-frames ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"])

(reactel-defcomponent reactel-ex-search-spinner (&key active)
  (let ((ticks (reactel-ex-use-ticker active 0.08)))
    (reactel-c-text (if active
                        (aref reactel-ex-search-spinner-frames
                              (mod ticks (length reactel-ex-search-spinner-frames)))
                      " "))))

(reactel-defcomponent reactel-ex-search-results (&key results query)
  (if results
      (reactel-c-list
        (--map (reactel-c-text :key it
                 (reactel-ex-search--highlight it query))
               results))
    (reactel-c-text :face 'shadow "no results")))

(defun reactel-ex-search--highlight (text query)
  "TEXT with the first match of QUERY in bold."
  (let ((start (and (not (string-empty-p query)) (string-match (regexp-quote query) text))))
    (if start
        (concat (substring text 0 start)
                (propertize (substring text start (+ start (length query))) 'face 'bold)
                (substring text (+ start (length query))))
      text)))

(reactel-defcomponent reactel-ex-search (&key (debounce 0.3) (latency 0.5))
  (-let* (((input set-input) (reactel-use-state ""))
          ((attempt set-attempt) (reactel-use-state 0))
          ((history set-history) (reactel-use-state nil))
          (query (reactel-ex-use-debounced input debounce))
          ((&plist :status status :results results :message message)
           (reactel-ex-use-search query latency attempt))
          (retry (reactel-use-callback* () () (funcall set-attempt reactel-inc))))
    ;; remember the searches that succeeded
    (reactel-use-effect* (status query)
      (when (eq status 'done)
        (funcall set-history (lambda (h) (-take 5 (-distinct (cons query h))))))
      nil)
    (reactel-c-vstack :spacing 1
      (reactel-c-hstack
        (reactel-c-field :label "Search" :placeholder "type to search" :width 20
          :value input :on-change set-input)
        (reactel-el 'reactel-ex-search-spinner :active (eq status 'loading)))
      (reactel-c-text :face 'shadow
        (if (equal input query)
            (format "query: %S" query)
          (format "typing… (settled on %S)" query)))
      (pcase status
        ('idle (reactel-c-text :face 'shadow "enter a query"))
        ('loading (reactel-c-text "searching…"))
        ('failed (reactel-c-hstack
                   (reactel-c-text :face 'error message)
                   (reactel-c-button :on-click retry "retry")))
        ('done (reactel-el 'reactel-ex-search-results :results results :query query)))
      (when history
        (reactel-c-text :face 'shadow (format "history: %s" (string-join history ", ")))))))

(defun reactel-demo-search ()
  "Show the search example."
  (interactive)
  (reactel-show "*reactel-search*" (reactel-el 'reactel-ex-search)))

(provide 'reactel-ex-search)
;;; reactel-ex-search.el ends here
