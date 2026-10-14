;;; reactel-ex-todo.el --- Reactel example: lists, keys, memo, callbacks -*- lexical-binding: t -*-

;; M-x reactel-demo-todo
;;
;; Each todo item is a memoized component.  Its props are the item plist
;; (unchanged items stay `eq' because `-map-when' reuses them) and two
;; callbacks that are stable thanks to `reactel-use-callback*', so toggling
;; one item re-renders only that item.  The "rendered Nx" counters prove it;
;; they are kept by an effect and shown by a separate component, see
;; `reactel-ex-render-counter.el', so nothing here breaks the Rules of React.

(require 'dash)
(require 'reactel)
(require 'reactel-buffer)
(require 'reactel-ex-render-counter)

(reactel-defcomponent reactel-ex-todo-item (&key item on-toggle on-remove)
  :memo t
  ;; Counts this component's renders (in an effect) for the label below.
  (reactel-ex-use-render-count (plist-get item :id))
  (-let (((&plist :id id :text text :done done) item))
    (reactel-c-hstack
      (reactel-c-checkbox :checked done :on-change (lambda (_) (funcall on-toggle id))
        (reactel-c-text :face (and done 'shadow) text))
      (reactel-c-button :on-click (lambda () (funcall on-remove id)) "delete")
      (reactel-el 'reactel-ex-render-count :counter-key id))))

(defun reactel-ex-todo--with-id (id fn items)
  "Return ITEMS with FN applied to the item whose :id is ID."
  (-map-when (lambda (i) (= (plist-get i :id) id)) fn items))

(reactel-defcomponent reactel-ex-todo (&key (initial '("Port React to Emacs" "Add dash examples")))
  (-let* (((items set-items)
           (reactel-use-state (lambda () (--map-indexed (list :id it-index :text it :done nil)
                                                        initial))))
          ((filter set-filter) (reactel-use-state 'all))
          ((draft set-draft) (reactel-use-state ""))
          (next-id (reactel-use-ref (length initial)))
          (toggle (reactel-use-callback* () (id)
                    (funcall set-items
                             (lambda (xs)
                               (reactel-ex-todo--with-id id (lambda (i) (plist-put (copy-sequence i) :done
                                                                                   (not (plist-get i :done))))
                                                         xs)))))
          (remove (reactel-use-callback* () (id)
                    (funcall set-items (lambda (xs) (--remove (= (plist-get it :id) id) xs)))))
          (add (reactel-use-callback* (draft) ()
                 (unless (string-empty-p (string-trim draft))
                   (let ((id (reactel-ref-current next-id)))
                     (setf (reactel-ref-current next-id) (1+ id))
                     (funcall set-items (lambda (xs) (append xs (list (list :id id :text draft :done nil)))))
                     (funcall set-draft "")))))
          (clear (reactel-use-callback* () ()
                   (funcall set-items (lambda (xs) (--remove (plist-get it :done) xs)))))
          (visible (reactel-use-memo* (items filter)
                     (pcase filter
                       ('active (--remove (plist-get it :done) items))
                       ('done (--filter (plist-get it :done) items))
                       (_ items))))
          (left (--count (not (plist-get it :done)) items)))
    (reactel-c-vstack
      (reactel-c-heading "Todos")
      (--map (reactel-el 'reactel-ex-todo-item :key (plist-get it :id) :item it
               :on-toggle toggle :on-remove remove)
             visible)
      (reactel-c-separator :width 40)
      (reactel-c-hstack
	(reactel-c-field :label "New todo" :placeholder "what needs doing?" :width 20 :value draft
          :on-change set-draft)
	(reactel-c-button :on-click add :disabled (string-empty-p (string-trim draft)) "add"))
      (reactel-c-hstack
	(reactel-c-text (format "%d left" left))
	(reactel-c-select :options '(all active done) :value filter :on-change set-filter)
	(reactel-c-button :on-click clear :disabled (not (--any? (plist-get it :done) items))
          "clear done")))))

(defun reactel-demo-todo ()
  "Show the todo example."
  (interactive)
  (reactel-ex-render-counter-reset)
  (reactel-show "*reactel-todo*" (reactel-el 'reactel-ex-todo)))

(provide 'reactel-ex-todo)
;;; reactel-ex-todo.el ends here
