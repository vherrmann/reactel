;;; reindent.el --- Reindent the project's Elisp files with Emacs's own rules -*- lexical-binding: t -*-

;; Usage: emacs -Q --batch -L . -L DASH-DIR -l scripts/reindent.el
;; (`make indent').  Loads reactel so that the indentation rules of the
;; components (`reactel-el', `reactel-c-*', ...) are known.

(require 'cl-lib)
(require 'reactel-buffer)
(require 'reactel-markdown)
(require 'dash)

;; The test files define macros with their own indentation rules; load them so
;; that those rules are known when their forms are indented.
(let ((reactel-lint-severity nil))      ; the tests define components that break rules
  (dolist (file (directory-files "test" t "\\.el\\'"))
    (load file nil t)))

(let ((changed 0))
  (dolist (file (append (directory-files "." t "\\`reactel[a-z-]*\\.el\\'")
                        (directory-files "examples" t "\\.el\\'")
                        (directory-files "test" t "\\.el\\'")))
    (with-current-buffer (find-file-noselect file t)
      (emacs-lisp-mode)
      (let ((before (buffer-string)) (inhibit-message t))
        (indent-region (point-min) (point-max))
        (unless (equal before (buffer-string))
          (cl-incf changed)
          (write-region nil nil file nil 'silent)
          (message "reindented %s" file)))
      (kill-buffer)))
  (message "%d file(s) reindented" changed))
