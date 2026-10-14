;;; reactel-examples.el --- Menu of the Reactel examples -*- lexical-binding: t -*-

;; M-x reactel-examples  picks one of the demos in this directory.
;; Needs dash.el.

(require 'reactel-ex-counter)
(require 'reactel-ex-stopwatch)
(require 'reactel-ex-todo)
(require 'reactel-ex-form)
(require 'reactel-ex-cart)
(require 'reactel-ex-boundary)
(require 'reactel-ex-store)
(require 'reactel-ex-jump)
(require 'reactel-ex-markdown)
(require 'reactel-ex-compiler)
(require 'reactel-ex-kanban)
(require 'reactel-ex-tree)
(require 'reactel-ex-search)
(require 'reactel-ex-life)
(require 'reactel-ex-buffers)

(defconst reactel-examples-alist
  '(("counter: state, callbacks, effects" . reactel-demo-counter)
    ("stopwatch: reducer, timer cleanup" . reactel-demo-stopwatch)
    ("todo: keys, memo, stable callbacks" . reactel-demo-todo)
    ("form: controlled inputs, derived state" . reactel-demo-form)
    ("cart: reducer and context" . reactel-demo-cart)
    ("boundary: error boundaries" . reactel-demo-boundary)
    ("store: external store" . reactel-demo-store)
    ("jump: refs and layout effects" . reactel-demo-jump)
    ("markdown: one component, two backends" . reactel-demo-markdown)
    ("compiler: automatic memoization" . reactel-demo-compiler)
    ("kanban: reducer, undo hook, context, rules" . reactel-demo-kanban)
    ("tree: recursive components, file I/O in effects" . reactel-demo-tree)
    ("search: debounce, cancellation, async status" . reactel-demo-search)
    ("life: simulation, timer effect, custom memo" . reactel-demo-life)
    ("buffers: live Emacs state via an external store" . reactel-demo-buffers)))

;;;###autoload
(defun reactel-examples ()
  "Run one of the Reactel example apps."
  (interactive)
  (call-interactively
   (cdr (assoc (completing-read "Example: " reactel-examples-alist nil t)
               reactel-examples-alist))))

(provide 'reactel-examples)
;;; reactel-examples.el ends here
