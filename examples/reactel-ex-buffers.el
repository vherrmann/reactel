;;; reactel-ex-buffers.el --- Reactel example: a live buffer manager -*- lexical-binding: t -*-

;; M-x reactel-demo-buffers
;;
;; A view of Emacs's own buffers that stays in sync while you work.  It shows:
;;
;;   - `reactel-use-sync-external-store' bridging Emacs state into the UI:
;;     the list of buffers is read from Emacs and the view subscribes to
;;     `buffer-list-update-hook', so creating or killing a buffer anywhere
;;     updates it;
;;   - derived data (filtering, sorting, totals) computed with
;;     `reactel-use-memo*' from the snapshot and the user's choices;
;;   - per-row state: each row has a two-step kill (the button first asks
;;     "sure?") that lives in the row, not in the list;
;;   - tables of components (`reactel-c-table' with components rendering
;;     rows), and event handlers that act on Emacs (switch, kill).
;;
;; Reading `buffer-list' during render would be impure; the hook does it
;; through the store instead, and handlers do the side effects.

(require 'dash)
(require 'reactel)
(require 'reactel-c)
(require 'reactel-buffer)

;;;; The store: Emacs's buffers

(defun reactel-ex-buffers-snapshot ()
  "A list describing the live buffers, one (NAME SIZE MODE MODIFIED) each.
It is compared with `equal', so the view only updates when it really changed."
  (--map (with-current-buffer it
           (list (buffer-name) (buffer-size) (symbol-name major-mode) (buffer-modified-p)))
         (buffer-list)))

(defun reactel-ex-buffers-subscribe (callback)
  "Call CALLBACK when the buffer list changes; return a function that stops that."
  (add-hook 'buffer-list-update-hook callback)
  (lambda () (remove-hook 'buffer-list-update-hook callback)))

(reactel-defhook reactel-ex-use-buffers ()
  "The current snapshot of the buffers, kept up to date."
  (reactel-use-sync-external-store
   #'reactel-ex-buffers-subscribe #'reactel-ex-buffers-snapshot))

;;;; Components

(defun reactel-ex-buffers--internal-p (name)
  "Whether the buffer NAME is an internal one (it starts with a space)."
  (string-prefix-p " " name))

(reactel-defcomponent reactel-ex-buffers-row (&key buffer)
  :memo t
  (-let (((name size mode modified) buffer)
         ((confirming set-confirming) (reactel-use-state nil)))
    (reactel-c-row
      (concat name (if modified " *" ""))
      (number-to-string size)
      mode
      (reactel-c-hstack
        (reactel-c-button :on-click (lambda () (switch-to-buffer name)) "switch")
        (if confirming
            (reactel-c-hstack
              (reactel-c-button :on-click (lambda () (kill-buffer name)) "sure?")
              (reactel-c-button :on-click (lambda () (funcall set-confirming nil)) "no"))
          (reactel-c-button :on-click (lambda () (funcall set-confirming t)) "kill"))))))

(reactel-defcomponent reactel-ex-buffers ()
  (-let* ((buffers (reactel-ex-use-buffers))
          ((filter set-filter) (reactel-use-state ""))
          ((sort-key set-sort-key) (reactel-use-state 'name))
          ((show-internal set-show-internal) (reactel-use-state nil))
          (visible (reactel-use-memo* (buffers filter sort-key show-internal)
                     (let ((matching (--filter (and (or show-internal
                                                        (not (reactel-ex-buffers--internal-p (car it))))
                                                    (string-match-p (regexp-quote filter) (car it)))
                                               buffers)))
                       (pcase sort-key
                         ('size (-sort (lambda (a b) (> (nth 1 a) (nth 1 b))) matching))
                         ('mode (-sort (lambda (a b) (string< (nth 2 a) (nth 2 b))) matching))
                         (_ (-sort (lambda (a b) (string< (car a) (car b))) matching))))))
          (total (reactel-use-memo* (visible) (-sum (--map (nth 1 it) visible)))))
    (reactel-c-vstack :spacing 1
      (reactel-c-hstack
        (reactel-c-field :label "Filter" :placeholder "buffer name" :width 16
          :value filter :on-change set-filter)
        (reactel-c-text "sort:")
        (reactel-c-select :options '(name size mode) :value sort-key :on-change set-sort-key)
        (reactel-c-checkbox :checked show-internal :on-change set-show-internal "internal"))
      (reactel-c-table :align '(left right left left)
        (reactel-c-row "Buffer" "Size" "Mode" "")
        (--map (reactel-el 'reactel-ex-buffers-row :key (car it) :buffer it) visible))
      (reactel-c-text :face 'shadow
        (format "%d %s, %d characters" (length visible)
                (if (= (length visible) 1) "buffer" "buffers") total)))))

(defun reactel-demo-buffers ()
  "Show the buffer manager example."
  (interactive)
  (reactel-show "*reactel-buffers*" (reactel-el 'reactel-ex-buffers)))

(provide 'reactel-ex-buffers)
;;; reactel-ex-buffers.el ends here
