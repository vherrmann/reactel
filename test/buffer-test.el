;;; buffer-test.el --- Tests for the Reactel UI primitives -*- lexical-binding: t -*-

(require 'ert)
(require 'reactel)
(require 'reactel-buffer)

(defun ui-render (element)
  "Render ELEMENT headlessly; return (ROOT . TEXT)."
  (let ((r (reactel-create-string-root)))
    (reactel-root-render r element)
    (cons r (reactel-root-text r))))

(defmacro ui-should-render (element expected)
  `(should (equal (cdr (ui-render ,element)) ,expected)))

(ert-deftest ui-text-and-layout ()
  (ui-should-render (reactel-c-text "a" (reactel-c-text "b") 1) "ab1")
  (ui-should-render (reactel-c-vstack "a" "b") "a\nb\n")
  (ui-should-render (reactel-c-vstack :spacing 1 "a" "b") "a\n\nb\n")
  (ui-should-render (reactel-c-vstack :indent 2 "a" (reactel-c-vstack "b" "c")) "  a\n  b\n  c\n")
  (ui-should-render (reactel-c-hstack "a" nil "b" "c") "a b c")
  (ui-should-render (reactel-c-hstack :spacing 3 "a" "b") "a   b")
  (ui-should-render (reactel-c-text "x" (reactel-c-newline 2) (reactel-c-space 3) "y") "x\n\n   y"))

(ert-deftest ui-empty-children-do-not-leave-gaps ()
  (ui-should-render (reactel-c-vstack :spacing 1 "a" (reactel-c-fragment) "b") "a\n\nb\n")
  (ui-should-render (reactel-c-hstack "a" (reactel-c-fragment) "b") "a b"))

(ert-deftest ui-heading-and-separator ()
  (ui-should-render (reactel-c-vstack (reactel-c-heading :level 2 "Hi") (reactel-c-separator :width 3 :char ?-))
                    "Hi\n---\n")
  (let ((s (reactel-render-to-string (reactel-c-heading :level 2 "Hi"))))
    (should (eq (get-text-property 0 'face s) 'outline-2))))

(ert-deftest ui-text-properties ()
  (let ((s (reactel-render-to-string
            (reactel-c-text :face 'bold :help "tip" :props '(foo 1) "x"))))
    (should (eq (get-text-property 0 'face s) 'bold))
    (should (equal (get-text-property 0 'help-echo s) "tip"))
    (should (eql (get-text-property 0 'foo s) 1))))

(ert-deftest ui-list ()
  (let ((reactel-buffer-bullet "*"))
    (ui-should-render (reactel-c-list "a" (reactel-c-vstack "b" "c")) "* a\n* b\n  c\n"))
  (ui-should-render (reactel-c-list "a" "b") "• a\n• b\n"))

(ert-deftest ui-ordered-list ()
  (ui-should-render (reactel-c-list :ordered t "a" "b") "1. a\n2. b\n"))

(ert-deftest ui-box ()
  (ui-should-render (reactel-c-box (reactel-c-vstack "ab" "abcd"))
                    "┌──────┐\n│ ab   │\n│ abcd │\n└──────┘\n")
  (ui-should-render (reactel-c-box :border 'ascii :padding 0 "x")
                    "+-+\n|x|\n+-+\n")
  (ui-should-render (reactel-c-box :title "T" :padding 0 "x")
                    "┌─ T ─┐\n│x    │\n└─────┘\n")
  (ui-should-render (reactel-c-vstack :indent 2 (reactel-c-box :padding 0 "x"))
                    "  ┌─┐\n  │x│\n  └─┘\n"))

(ert-deftest ui-box-followed-by-content ()
  (ui-should-render (reactel-c-vstack (reactel-c-box :padding 0 "x") "after")
                    "┌─┐\n│x│\n└─┘\nafter\n"))

(ert-deftest ui-table-alignment ()
  (ui-should-render
   (reactel-c-table :align '(left right)
     (reactel-c-row "Name" "Qty") (reactel-c-row "apple" 3) (reactel-c-row "kiwi" 12))
   "Name  │ Qty\n───────────\napple │   3\nkiwi  │  12\n")
  (ui-should-render
   (reactel-c-table :header nil :separator " | " (reactel-c-row "a" "bb") (reactel-c-row "ccc" "d"))
   "a   | bb\nccc | d\n"))

(ert-deftest ui-table-cells-can-be-components-and-buttons ()
  (let (clicked)
    (reactel-defcomponent reactel-test-ui-cell (&key n) (reactel-c-text (format "<%d>" n)))
    (let* ((res (ui-render (reactel-c-table :header nil
                             (reactel-c-row (reactel-el 'reactel-test-ui-cell :n 1)
                               (reactel-c-button :on-click (lambda () (setq clicked t)) "go"))
                             (reactel-c-row "wide cell" "x"))))
           (root (car res)))
      (should (equal (cdr res) "<1>       │ [go]\nwide cell │ x\n"))
      (reactel-click root "go")
      (should clicked))))

(ert-deftest ui-row-outside-table-errors ()
  (should-error (ui-render (reactel-c-row "a"))))

(ert-deftest ui-regions-survive-box-and-table-edits ()
  (let (r1 r2)
    (setq r1 (reactel-create-ref) r2 (reactel-create-ref))
    (let* ((res (ui-render (reactel-c-vstack
                             (reactel-c-box (reactel-c-text :ref r1 "inner"))
                             (reactel-c-table :header nil
                               (reactel-c-row "a" (reactel-c-text :ref r2 "z"))
                               (reactel-c-row "bbb" "y")))))
           (text (cdr res)))
      (cl-flet ((region-text (ref) (let ((reg (reactel-buffer-node-region (reactel-ref-current ref))))
                                     (substring text (1- (car reg)) (1- (cdr reg))))))
        (should (equal (region-text r1) "inner"))
        (should (equal (region-text r2) "z"))))))

;;; Controls

(ert-deftest ui-buttons-are-real-buttons ()
  (let* ((clicked 0)
         (s (reactel-render-to-string
             (reactel-c-button :on-click (lambda () (cl-incf clicked)) "go"))))
    (should (equal (substring-no-properties s) "[go]"))
    (should (get-text-property 1 'action s))
    (with-temp-buffer
      (insert s)
      (goto-char 2)
      (should (button-at (point)))
      (push-button)
      (should (= clicked 1)))))

(ert-deftest ui-disabled-button ()
  (let ((res (ui-render (reactel-c-button :on-click #'ignore :disabled t "no"))))
    (should (equal (cdr res) "[no]"))
    (should-error (reactel-click (car res) "no"))
    (should-not (get-text-property 1 'action (reactel-root-output (car res))))))

(ert-deftest ui-checkbox ()
  (reactel-defcomponent reactel-test-ui-cb ()
    (cl-destructuring-bind (on set-on) (reactel-use-state nil)
      (reactel-c-checkbox :checked on :on-change set-on "agree")))
  (let ((res (ui-render (reactel-el 'reactel-test-ui-cb))))
    (should (equal (cdr res) "[ ] agree"))
    (reactel-click (car res) "agree")
    (should (equal (reactel-root-text (car res)) "[x] agree"))
    (reactel-click (car res) "agree")
    (should (equal (reactel-root-text (car res)) "[ ] agree"))))

(ert-deftest ui-select ()
  (reactel-defcomponent reactel-test-ui-sel (&key layout)
    (cl-destructuring-bind (v set-v) (reactel-use-state 'b)
      (reactel-c-select :options '(a (b . "Bee") "c") :value v :on-change set-v :layout layout)))
  (let ((res (ui-render (reactel-el 'reactel-test-ui-sel))))
    (should (equal (cdr res) "( ) a  (*) Bee  ( ) c"))
    (reactel-click (car res) "a")
    (should (equal (reactel-root-text (car res)) "(*) a  ( ) Bee  ( ) c"))
    (reactel-click (car res) "c")
    (should (equal (reactel-root-text (car res)) "( ) a  ( ) Bee  (*) c")))
  (should (equal (cdr (ui-render (reactel-el 'reactel-test-ui-sel :layout 'vertical)))
                 "( ) a\n(*) Bee\n( ) c\n")))

(ert-deftest ui-field ()
  (reactel-defcomponent reactel-test-ui-fld ()
    (cl-destructuring-bind (v set-v) (reactel-use-state "")
      (reactel-c-field :label "Name" :placeholder "who" :width 6 :value v :on-change set-v)))
  (let ((res (ui-render (reactel-el 'reactel-test-ui-fld))))
    (should (equal (cdr res) "[who___]"))
    (reactel-type (car res) "Name" "Ada")
    (should (equal (reactel-root-text (car res)) "[Ada___]"))
    ;; the minibuffer path
    (cl-letf (((symbol-function 'read-string) (lambda (prompt initial)
                                                (should (equal prompt "Name: "))
                                                (concat initial "!"))))
      (let ((action (get-text-property 1 'action (reactel-root-output (car res)))))
        (funcall action nil)))
    (should (equal (reactel-root-text (car res)) "[Ada!__]"))))

(ert-deftest ui-link ()
  (let (opened)
    (cl-letf (((symbol-function 'browse-url) (lambda (u &rest _) (setq opened u))))
      (let ((res (ui-render (reactel-c-link :url "https://example.org" "site"))))
        (should (equal (cdr res) "site"))
        (reactel-click (car res) "site")
        (should (equal opened "https://example.org"))))))

(ert-deftest ui-buffer-navigation-uses-button-el ()
  (let ((buf "*reactel-buffer-test*"))
    (unwind-protect
        (progn
          (reactel-render-buffer buf (reactel-c-vstack (reactel-c-button :on-click #'ignore "one") (reactel-c-button :on-click #'ignore "two")))
          (with-current-buffer buf
            (goto-char (point-min))
            (should (button-at (point)))
            (forward-button 1) (should (looking-at "\\[two\\]"))
            (backward-button 1) (should (looking-at "\\[one\\]"))
            (should (eq (key-binding (kbd "TAB")) 'forward-button))))
      (kill-buffer buf))))

;; Painting each line of STRING directly (no substring) keeps the indentation
;; rules: empty lines are not indented, text after a newline is.
(ert-deftest ui-indent-multiline-strings ()
  (ui-should-render (reactel-c-vstack :indent 2 "a\n\nb\nc") "  a\n\n  b\n  c\n")
  (ui-should-render (reactel-c-vstack :indent 1 (reactel-c-text "x" "\ny\n" "z")) " x\n y\n z\n")
  (let ((s (reactel-render-to-string
            (reactel-c-vstack :indent 2 (reactel-c-text :face 'bold "p\nq")))))
    (should (equal s "  p\n  q\n"))
    (should (eq (get-text-property 2 'face s) 'bold))
    (should (eq (get-text-property 6 'face s) 'bold))))

;; Repaints replace the whole text; recording that as undo made the undo list
;; the main allocation of every commit.
(ert-deftest ui-repaint-records-no-undo ()
  (let ((buf "*reactel-buffer-test*"))
    (unwind-protect
        (progn
          (reactel-render-buffer buf (reactel-c-vstack "one"))
          (with-current-buffer buf
            (buffer-enable-undo)
            (setq buffer-undo-list nil))
          (reactel-render-buffer buf (reactel-c-vstack "two" "three"))
          (with-current-buffer buf
            (should (equal (buffer-string) "two\nthree\n"))
            (should-not buffer-undo-list)))
      (kill-buffer buf))))

(provide 'buffer-test)
;;; buffer-test.el ends here
