;;; reactel-ex-kanban.el --- Reactel example: a kanban board with undo -*- lexical-binding: t -*-

;; M-x reactel-demo-kanban
;;
;; A board with three columns and rules (a work-in-progress limit), built from:
;;
;;   - a pure reducer for the board (`reactel-ex-kanban--reduce');
;;   - a custom hook, `reactel-ex-use-history', that wraps any reducer with
;;     undo and redo;
;;   - a context that gives deeply nested cards the (stable) action function;
;;   - column components memoized with a custom comparison, so a card moving
;;     in one column does not re-render the others;
;;   - derived data (owners, statistics) computed with `reactel-use-memo*'.

(require 'dash)
(require 'reactel)
(require 'reactel-c)
(require 'reactel-buffer)

(defconst reactel-ex-kanban-columns '(todo doing done) "The columns, left to right.")
(defconst reactel-ex-kanban-wip-limit 2 "Most cards allowed in the `doing' column.")

(defconst reactel-ex-kanban-cards
  '((:id 1 :title "Write the protocol" :owner "ada" :column done)
    (:id 2 :title "Buffer backend" :owner "ada" :column doing)
    (:id 3 :title "Markdown backend" :owner "grace" :column doing)
    (:id 4 :title "Compiler" :owner "grace" :column todo)
    (:id 5 :title "Write the docs" :owner "ada" :column todo))
  "The cards of the sample board.")

(defconst reactel-ex-kanban-actions (reactel-create-context #'ignore "KanbanActions")
  "Provides the function that applies a board action (an undoable change).")

;;;; Board logic, pure

(defun reactel-ex-kanban--count (cards column)
  "The number of CARDS in COLUMN."
  (--count (eq (plist-get it :column) column) cards))

(defun reactel-ex-kanban--move (cards id direction)
  "Move the card ID by DIRECTION (-1 or 1) columns.
Returns CARDS itself when the move is not allowed: at the edge of the board,
or into `doing' when it is full."
  (let* ((card (--first (= (plist-get it :id) id) cards))
         (index (and card (-elem-index (plist-get card :column) reactel-ex-kanban-columns)))
         (target-index (and index (+ index direction)))
         (target (and target-index
                      (<= 0 target-index (1- (length reactel-ex-kanban-columns)))
                      (nth target-index reactel-ex-kanban-columns))))
    (if (or (null target)
            (and (eq target 'doing)
                 (>= (reactel-ex-kanban--count cards 'doing) reactel-ex-kanban-wip-limit)))
        cards
      (-map-when (lambda (c) (= (plist-get c :id) id))
                 (lambda (c) (plist-put (copy-sequence c) :column target))
                 cards))))

(defun reactel-ex-kanban--reduce (cards action)
  "The board after ACTION: (add TITLE OWNER), (move ID DIRECTION) or (remove ID)."
  (pcase action
    (`(add ,title ,owner)
     (if (string-empty-p (string-trim title))
         cards
       (append cards (list (list :id (1+ (apply #'max 0 (--map (plist-get it :id) cards)))
                                 :title (string-trim title) :owner owner :column 'todo)))))
    (`(move ,id ,direction) (reactel-ex-kanban--move cards id direction))
    (`(remove ,id) (--remove (= (plist-get it :id) id) cards))
    (_ (error "Unknown board action %S" action))))

;;;; Undo and redo for any reducer

(defun reactel-ex-kanban--history-step (reducer state action)
  "STATE (:past :present :future) after ACTION: undo, redo or (do INNER)."
  (-let (((&plist :past past :present present :future future) state))
    (pcase action
      ('undo (if past
                 (list :past (cdr past) :present (car past) :future (cons present future))
               state))
      ('redo (if future
                 (list :past (cons present past) :present (car future) :future (cdr future))
               state))
      (`(do ,inner)
       (let ((next (funcall reducer present inner)))
         ;; a change that does nothing is not a step of the history
         (if (eq next present)
             state
           (list :past (cons present past) :present next :future nil))))
      (_ (error "Unknown history action %S" action)))))

(reactel-defhook reactel-ex-use-history (reducer initial)
  "State managed by REDUCER with undo and redo.
Returns (STATE ACT UNDO REDO CAN-UNDO CAN-REDO); ACT, UNDO and REDO are stable."
  (-let* (((history dispatch)
           (reactel-use-reducer
            (lambda (state action) (reactel-ex-kanban--history-step reducer state action))
            (list :past nil :present initial :future nil)))
          ((&plist :past past :present present :future future) history)
          (act (reactel-use-callback* (dispatch) (action) (funcall dispatch (list 'do action))))
          (undo (reactel-use-callback* (dispatch) () (funcall dispatch 'undo)))
          (redo (reactel-use-callback* (dispatch) () (funcall dispatch 'redo))))
    (list present act undo redo (and past t) (and future t))))

;;;; Components

(reactel-defcomponent reactel-ex-kanban-card (&key card)
  :memo t
  (-let (((&plist :id id :title title :owner owner :column column) card)
         (act (reactel-use-context reactel-ex-kanban-actions)))
    (reactel-c-hstack
      (reactel-c-text title)
      (reactel-c-text :face 'shadow (concat "@" owner))
      (reactel-c-button :on-click (lambda () (funcall act `(move ,id -1)))
        :disabled (eq column 'todo) "←")
      (reactel-c-button :on-click (lambda () (funcall act `(move ,id 1)))
        :disabled (eq column 'done) "→")
      (reactel-c-button :on-click (lambda () (funcall act `(remove ,id))) "✕"))))

(defun reactel-ex-kanban--same-column-p (old new)
  "Whether the column props OLD and NEW show the same cards."
  (and (eq (plist-get old :column) (plist-get new :column))
       (equal (plist-get old :cards) (plist-get new :cards))))

(reactel-defcomponent reactel-ex-kanban-column (&key column cards)
  :memo #'reactel-ex-kanban--same-column-p
  (let ((limit (and (eq column 'doing) reactel-ex-kanban-wip-limit)))
    (reactel-c-box :title (format "%s (%d%s)" (upcase (symbol-name column)) (length cards)
                                  (if limit (format "/%d" limit) ""))
      (if cards
          (reactel-c-vstack
            (--map (reactel-el 'reactel-ex-kanban-card :key (plist-get it :id) :card it) cards))
        (reactel-c-text :face 'shadow "empty")))))

(reactel-defcomponent reactel-ex-kanban (&key (initial reactel-ex-kanban-cards))
  (-let* (((cards act undo redo can-undo can-redo) (reactel-ex-use-history
                                                    #'reactel-ex-kanban--reduce initial))
          ((owner set-owner) (reactel-use-state "everyone"))
          ((draft set-draft) (reactel-use-state ""))
          ((assignee set-assignee) (reactel-use-state "ada"))
          (owners (reactel-use-memo* (cards) (-sort #'string< (-distinct (--map (plist-get it :owner) cards)))))
          (visible (reactel-use-memo* (cards owner)
                     (if (equal owner "everyone")
                         cards
                       (--filter (equal (plist-get it :owner) owner) cards))))
          (add (reactel-use-callback* (act draft assignee) ()
                 (funcall act `(add ,draft ,assignee))
                 (funcall set-draft ""))))
    (reactel-el reactel-ex-kanban-actions :value act
      (reactel-c-vstack :spacing 1
        (reactel-c-hstack
          (reactel-c-button :on-click undo :disabled (not can-undo) "undo")
          (reactel-c-button :on-click redo :disabled (not can-redo) "redo")
          (reactel-c-text "Show:")
          (reactel-c-select :options (cons "everyone" owners) :value owner :on-change set-owner))
        (reactel-c-vstack
          (-map (lambda (column)
                  (reactel-el 'reactel-ex-kanban-column :key column :column column
                    :cards (--filter (eq (plist-get it :column) column) visible)))
                reactel-ex-kanban-columns))
        (reactel-c-hstack
          (reactel-c-field :label "New card" :placeholder "title" :width 18
            :value draft :on-change set-draft)
          (reactel-c-select :options owners :value assignee :on-change set-assignee)
          (reactel-c-button :on-click add :disabled (string-empty-p (string-trim draft)) "add card"))
        (reactel-c-text :face 'shadow
          (format "%d cards, %d done" (length cards)
                  (reactel-ex-kanban--count cards 'done)))))))

(defun reactel-demo-kanban ()
  "Show the kanban example."
  (interactive)
  (reactel-show "*reactel-kanban*" (reactel-el 'reactel-ex-kanban)))

(provide 'reactel-ex-kanban)
;;; reactel-ex-kanban.el ends here
