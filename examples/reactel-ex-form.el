;;; reactel-ex-form.el --- Reactel example: controlled fields, derived state -*- lexical-binding: t -*-

;; M-x reactel-demo-form
;;
;; Validation errors are derived with `reactel-use-memo*' instead of being
;; stored as extra state.

(require 'dash)
(require 'reactel)
(require 'reactel-buffer)

(defun reactel-ex-form--validate (name email age)
  "Return the list of problems with NAME, EMAIL and AGE."
  (-non-nil
   (list (when (< (length name) 3) "name needs at least 3 characters")
         (unless (string-match-p "\\`[^@ ]+@[^@ ]+\\.[a-z]+\\'" email) "email looks invalid")
         (unless (and (string-match-p "\\`[0-9]+\\'" age) (>= (string-to-number age) 18))
           "age must be a number, 18 or more"))))

(reactel-defcomponent reactel-ex-form-signup (&key on-submit)
  (-let* (((name set-name) (reactel-use-state ""))
          ((email set-email) (reactel-use-state ""))
          ((age set-age) (reactel-use-state ""))
          ((touched set-touched) (reactel-use-state nil))
          (errors (reactel-use-memo* (name email age) (reactel-ex-form--validate name email age)))
          (editing (lambda (setter) (lambda (v) (funcall set-touched t) (funcall setter v))))
          (submit (reactel-use-callback* (on-submit name email age) ()
                    (funcall on-submit (list :name name :email email :age (string-to-number age)))
                    (-each (list set-name set-email set-age) (-rpartial #'funcall ""))
                    (funcall set-touched nil))))
    (reactel-c-box :title "Sign up"
      (reactel-c-vstack
	(reactel-c-table :header nil :separator " "
          (reactel-c-row "Name" (reactel-c-field :label "Name" :placeholder "Ada" :width 20 :value name
                                  :on-change (funcall editing set-name)))
          (reactel-c-row "Email" (reactel-c-field :label "Email" :placeholder "ada@example.org" :width 20
                                   :value email :on-change (funcall editing set-email)))
          (reactel-c-row "Age" (reactel-c-field :label "Age" :placeholder "18+" :width 20 :value age
                                 :on-change (funcall editing set-age))))
	(when (and touched errors)
          (reactel-c-list :bullet "!"
            (--map (reactel-c-text :key it :face 'error it) errors)))
	(reactel-c-button :on-click submit :disabled (and errors t) "sign up")))))

(reactel-defcomponent reactel-ex-form ()
  (-let* (((people set-people) (reactel-use-state nil))
          (add (reactel-use-callback* () (person)
                 (funcall set-people (lambda (ps) (append ps (list person)))))))
    (reactel-c-vstack :spacing 1
      (reactel-el 'reactel-ex-form-signup :on-submit add)
      (reactel-c-vstack
	(reactel-c-heading :level 2 (format "Members (%d)" (length people)))
	(when people
          (reactel-c-table :align '(left left right)
            (reactel-c-row "Name" "Email" "Age")
            (--map-indexed
             (-let (((&plist :name n :email e :age a) it))
               (reactel-c-row :key it-index n e a))
             people)))))))

(defun reactel-demo-form ()
  "Show the form example."
  (interactive)
  (reactel-show "*reactel-form*" (reactel-el 'reactel-ex-form)))

(provide 'reactel-ex-form)
;;; reactel-ex-form.el ends here
