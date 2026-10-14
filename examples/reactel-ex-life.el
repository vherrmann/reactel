;;; reactel-ex-life.el --- Reactel example: Conway's Game of Life -*- lexical-binding: t -*-

;; M-x reactel-demo-life
;;
;; A grid you can draw on, and let evolve.  It shows:
;;
;;   - a *pure* simulation (`reactel-ex-life--step') kept apart from the UI,
;;     and a reducer around it;
;;   - an effect owning a timer, started and stopped by the `running' state;
;;   - rows memoized with a custom comparison (`:memo' with a function): a
;;     generation that leaves a row unchanged does not re-render it, even
;;     though the new grid is a different object;
;;   - values derived from state (the population) with `reactel-use-memo*';
;;   - a loop that stops by itself when the world reaches a still life.
;;
;; The world is a vector of rows, each a vector of 0 and 1, treated as
;; immutable: every change builds new rows, and only the rows that changed.
;; The edges wrap around.

(require 'dash)
(require 'reactel)
(require 'reactel-c)
(require 'reactel-buffer)

(defconst reactel-ex-life-patterns
  '((glider . ((1 . 0) (2 . 1) (0 . 2) (1 . 2) (2 . 2)))
    (blinker . ((0 . 1) (1 . 1) (2 . 1)))
    (block . ((0 . 0) (1 . 0) (0 . 1) (1 . 1)))
    (r-pentomino . ((1 . 0) (2 . 0) (0 . 1) (1 . 1) (1 . 2))))
  "Patterns as lists of (X . Y) cells.")

;;;; The world, pure

(defun reactel-ex-life-empty (width height)
  "A world of WIDTH x HEIGHT dead cells."
  (apply #'vector (--map (make-vector width 0) (number-sequence 1 height))))

(defun reactel-ex-life-from-cells (width height cells)
  "A WIDTH x HEIGHT world with the live CELLS, a list of (X . Y)."
  (let ((rows (--map (make-vector width 0) (number-sequence 1 height))))
    (dolist (cell cells)
      (aset (nth (mod (cdr cell) height) rows) (mod (car cell) width) 1))
    (apply #'vector rows)))

(defun reactel-ex-life--cell (world x y)
  "1 if the cell at X, Y of WORLD is alive, else 0 (the edges wrap around)."
  (aref (aref world (mod y (length world))) (mod x (length (aref world 0)))))

(defun reactel-ex-life--neighbours (world x y)
  "The number of live neighbours of the cell at X, Y in WORLD."
  (-sum (--map (reactel-ex-life--cell world (+ x (car it)) (+ y (cdr it)))
               '((-1 . -1) (0 . -1) (1 . -1) (-1 . 0) (1 . 0) (-1 . 1) (0 . 1) (1 . 1)))))

(defun reactel-ex-life--next-cell (world x y)
  "The state of the cell at X, Y in the generation after WORLD."
  (let ((n (reactel-ex-life--neighbours world x y)))
    (if (or (= n 3) (and (= 1 (reactel-ex-life--cell world x y)) (= n 2))) 1 0)))

(defun reactel-ex-life--step (world)
  "The generation after WORLD.  A row that did not change is shared (`eq')."
  (let ((width (length (aref world 0))))
    (apply #'vector
           (--map-indexed
            (let* ((y it-index)
                   (row it)
                   (next (apply #'vector
                                (--map (reactel-ex-life--next-cell world it y)
                                       (number-sequence 0 (1- width))))))
              (if (equal next row) row next))
            (append world nil)))))

(defun reactel-ex-life--toggle (world x y)
  "WORLD with the cell at X, Y flipped; the other rows are shared."
  (let ((new (vconcat (aref world y))))
    (aset new x (- 1 (aref new x)))
    (apply #'vector (--map-indexed (if (= it-index y) new it) (append world nil)))))

(defun reactel-ex-life--population (world)
  "The number of live cells in WORLD."
  (-sum (--map (-sum (append it nil)) (append world nil))))

;;;; The state

(defun reactel-ex-life--reduce (state action)
  "STATE (:world :generation :running) after ACTION."
  (-let (((&plist :world world :generation generation :running running) state))
    (pcase action
      (`(toggle ,x ,y) (list :world (reactel-ex-life--toggle world x y)
                             :generation generation :running running))
      ('step (let ((next (reactel-ex-life--step world)))
               (if (--every? (eq (aref next it) (aref world it))
                             (number-sequence 0 (1- (length world))))
                   ;; nothing changed: a still life, stop running
                   (list :world world :generation generation :running nil)
                 (list :world next :generation (1+ generation) :running running))))
      ('toggle-run (list :world world :generation generation :running (not running)))
      ('clear (list :world (reactel-ex-life-empty (length (aref world 0)) (length world))
                    :generation 0 :running nil))
      (`(load ,pattern)
       (list :world (reactel-ex-life-from-cells
                     (length (aref world 0)) (length world)
                     (--map (cons (+ 2 (car it)) (+ 2 (cdr it)))
                            (alist-get pattern reactel-ex-life-patterns)))
             :generation 0 :running nil))
      (_ (error "Unknown life action %S" action)))))

;;;; Components

(defun reactel-ex-life--same-row-p (old new)
  "Whether the row props OLD and NEW draw the same thing."
  (and (eq (plist-get old :on-toggle) (plist-get new :on-toggle))
       (eql (plist-get old :y) (plist-get new :y))
       (equal (plist-get old :row) (plist-get new :row))))

(reactel-defcomponent reactel-ex-life-row (&key y row on-toggle)
  :memo #'reactel-ex-life--same-row-p
  (reactel-c-hstack :spacing 0
    (--map-indexed (reactel-c-button :key it-index
                     :on-click (lambda () (funcall on-toggle it-index y))
                     (if (= it 1) "■" "·"))
                   (append row nil))))

(reactel-defcomponent reactel-ex-life (&key (width 10) (height 8) (interval 0.2))
  (-let* (((state dispatch) (reactel-use-reducer
                             #'reactel-ex-life--reduce
                             (list :world (reactel-ex-life-from-cells
                                           width height
                                           (--map (cons (+ 1 (car it)) (+ 1 (cdr it)))
                                                  (alist-get 'glider reactel-ex-life-patterns)))
                                   :generation 0 :running nil)))
          ((&plist :world world :generation generation :running running) state)
          ((pattern set-pattern) (reactel-use-state 'glider))
          (population (reactel-use-memo* (world) (reactel-ex-life--population world)))
          (toggle (reactel-use-callback* (dispatch) (x y) (funcall dispatch `(toggle ,x ,y))))
          (step (reactel-use-callback* (dispatch) () (funcall dispatch 'step)))
          (run (reactel-use-callback* (dispatch) () (funcall dispatch 'toggle-run)))
          (clear (reactel-use-callback* (dispatch) () (funcall dispatch 'clear)))
          (load (reactel-use-callback* (dispatch pattern) () (funcall dispatch `(load ,pattern)))))
    ;; The timer exists only while running, and is cancelled by the cleanup.
    (reactel-use-effect* (running interval)
      (when running
        (let ((timer (run-with-timer interval interval (lambda () (funcall dispatch 'step)))))
          (lambda () (cancel-timer timer)))))
    (reactel-c-box :title "Life"
      (reactel-c-vstack
        (--map-indexed (reactel-el 'reactel-ex-life-row :key it-index :y it-index
                         :row it :on-toggle toggle)
                       (append world nil))
        (reactel-c-text :face 'shadow
          (format "generation %d, population %d%s" generation population
                  (if running ", running" "")))
        (reactel-c-hstack
          (reactel-c-button :on-click run (if running "stop" "run"))
          (reactel-c-button :on-click step :disabled running "step")
          (reactel-c-button :on-click clear "clear"))
        (reactel-c-hstack
          (reactel-c-select :options (mapcar #'car reactel-ex-life-patterns)
            :value pattern :on-change set-pattern)
          (reactel-c-button :on-click load "load"))))))

(defun reactel-demo-life ()
  "Show the Game of Life example."
  (interactive)
  (reactel-show "*reactel-life*" (reactel-el 'reactel-ex-life)))

(provide 'reactel-ex-life)
;;; reactel-ex-life.el ends here
