;;; indent-test.el --- Tests for the indentation rules of the components -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'reactel-buffer)
(require 'reactel-markdown)

(defun indent-test-reindent (text)
  "TEXT reindented by `emacs-lisp-mode' (its first line stays as is)."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert text)
    (let ((inhibit-message t)) (indent-region (point-min) (point-max)))
    (buffer-string)))

(ert-deftest indent-every-component-constructor-has-a-rule ()
  (let (constructors)
    ;; newline and space take no children, so there is nothing to indent
    (mapatoms (lambda (s) (when (and (get s 'reactel-constructor) (fboundp s)
                                     (not (memq s '(reactel-c-newline reactel-c-space))))
                            (push s constructors))))
    (should (> (length constructors) 10))
    (dolist (c constructors)
      (should (equal (list c (function-get c 'lisp-indent-function))
                     (list c 'defun)))))
  (should (eq (function-get 'reactel-el 'lisp-indent-function) 'defun)))

(ert-deftest indent-children-are-indented-by-two ()
  (should (equal (indent-test-reindent
                  "(reactel-c-vstack :spacing 1
(reactel-c-heading \"Title\")
(reactel-c-hstack
(reactel-c-text \"a\")
(reactel-c-button :on-click #'ignore
:disabled t \"go\")))")
                 "(reactel-c-vstack :spacing 1
  (reactel-c-heading \"Title\")
  (reactel-c-hstack
    (reactel-c-text \"a\")
    (reactel-c-button :on-click #'ignore
      :disabled t \"go\")))")))

(ert-deftest indent-reactel-el-and-fragments ()
  (should (equal (indent-test-reindent
                  "(reactel-el 'row :key id :item item
:on-toggle toggle
(reactel-c-fragment
(reactel-c-text \"x\")))")
                 "(reactel-el 'row :key id :item item
  :on-toggle toggle
  (reactel-c-fragment
    (reactel-c-text \"x\")))")))

(ert-deftest indent-component-definitions-and-hooks ()
  (should (equal (indent-test-reindent
                  "(reactel-defcomponent counter (&key initial)
:memo t
(cl-destructuring-bind (n set-n) (reactel-use-state initial)
(reactel-use-effect* (n)
(message \"%d\" n)
nil)
(reactel-c-text (format \"%d\" n))))")
                 "(reactel-defcomponent counter (&key initial)
  :memo t
  (cl-destructuring-bind (n set-n) (reactel-use-state initial)
    (reactel-use-effect* (n)
      (message \"%d\" n)
      nil)
    (reactel-c-text (format \"%d\" n))))")))

(ert-deftest indent-the-sources-are-indented-as-emacs-would-do ()
  ;; `make indent' rewrites the files this way; this keeps them from drifting.
  (skip-unless (require 'dash nil t))
  (dolist (file (append (directory-files "." t "\\`reactel[a-z-]*\\.el\\'")
                        (directory-files "examples" t "\\.el\\'")))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((text (buffer-string)))
        (should (equal (list file (indent-test-reindent text))
                       (list file text)))))))

(provide 'indent-test)
;;; indent-test.el ends here
