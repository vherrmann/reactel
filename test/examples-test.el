;;; examples-test.el --- Run the example apps headlessly -*- lexical-binding: t -*-

(require 'ert)
(require 'reactel)
(require 'reactel-buffer)

(defmacro with-example (var element &rest body)
  "Render ELEMENT into a string root VAR and run BODY; skipped without dash."
  (declare (indent 2))
  `(progn
     (skip-unless (require 'dash nil t))
     (let ((,var (reactel-create-string-root)))
       (unwind-protect
           (progn (reactel-root-render ,var ,element) ,@body)
         (reactel-unmount ,var)))))

(defun ex-has (root regexp) (string-match-p regexp (reactel-root-text root)))

(ert-deftest example-counter ()
  (skip-unless (require 'dash nil t))
  (require 'reactel-ex-counter)
  (with-example r (reactel-el 'reactel-ex-counter :step 2)
    (should (ex-has r "Count: 0"))
    (reactel-click r "+") (reactel-click r "+") (reactel-click r "-")
    (should (ex-has r "Count: 2"))
    (should (ex-has r "history: 0 > 2 > 4 > 2"))
    (reactel-click r "reset")
    (should (ex-has r "Count: 0"))))

(ert-deftest example-stopwatch-reducer-and-timer ()
  (skip-unless (require 'dash nil t))
  (require 'reactel-ex-stopwatch)
  (should (equal (reactel-ex-stopwatch--format 754) "01:15.4"))
  (should (equal (plist-get (reactel-ex-stopwatch--reduce '(:elapsed 3 :running nil :laps nil) 'tick) :elapsed) 3))
  (with-example r (reactel-el 'reactel-ex-stopwatch :interval 0.01)
    (reactel-click r "start")
    (sleep-for 0.12)
    (should-not (ex-has r "00:00\\.0"))
    (reactel-click r "lap")
    (should (ex-has r "Lap +│ Time"))
    (reactel-click r "stop")
    (let ((frozen (reactel-root-text r)))
      (sleep-for 0.06)
      (should (equal frozen (reactel-root-text r))))   ; timer cleaned up
    (reactel-click r "reset")
    (should (ex-has r "00:00\\.0"))
    (should-not (ex-has r "Lap +│"))))

(ert-deftest example-todo-memo-and-filters ()
  (skip-unless (require 'dash nil t))
  (require 'reactel-ex-todo)
  (with-example r (reactel-el 'reactel-ex-todo)
    (should (ex-has r "\\[ \\] Port React to Emacs"))
    (should (ex-has r "2 left"))
    (reactel-type r "New todo" "Write docs")
    (reactel-click r "add")
    (should (ex-has r "Write docs"))
    (should (ex-has r "3 left"))
    ;; Toggle the first item: only it re-renders, the others stay memoized.
    (reactel-click r "Port React to Emacs")
    (should (ex-has r "2 left"))
    (should (ex-has r "\\[x\\] Port React to Emacs \\[delete\\] (rendered 2x)"))
    (should (ex-has r "\\[ \\] Add dash examples \\[delete\\] (rendered 1x)"))
    (reactel-click r "done")                      ; the radio option
    (should-not (ex-has r "Add dash examples"))
    (should (ex-has r "(\\*) done"))
    (reactel-click r "clear done")
    (should-not (ex-has r "Port React to Emacs"))
    (reactel-click r "all")
    (should (ex-has r "Write docs"))
    (should (ex-has r "Add dash examples"))))

(ert-deftest example-form-validation ()
  (skip-unless (require 'dash nil t))
  (require 'reactel-ex-form)
  (should (equal (reactel-ex-form--validate "Al" "x" "5") '("name needs at least 3 characters"
							    "email looks invalid"
							    "age must be a number, 18 or more")))
  (with-example r (reactel-el 'reactel-ex-form)
    (should-not (ex-has r "!"))             ; no errors before the user touches it
    (reactel-type r "Name" "Ada Lovelace")
    (should (ex-has r "! email looks invalid"))
    (should-error (reactel-click r "sign up"))   ; disabled
    (reactel-type r "Email" "ada@example.org")
    (reactel-type r "Age" "36")
    (should-not (ex-has r "!"))
    (reactel-click r "sign up")
    (should (ex-has r "Members (1)"))
    (should (ex-has r "Ada Lovelace +│ ada@example.org +│ +36"))
    (should (equal (plist-get (reactel-node-props (reactel-find-field r "Name")) :value) ""))))

(ert-deftest example-cart-reducer-and-context ()
  (skip-unless (require 'dash nil t))
  (require 'reactel-ex-cart)
  (should (equal (reactel-ex-cart--reduce '((:sku a :qty 1)) '(dec a)) nil))
  (should (equal (reactel-ex-cart--reduce nil '(add a)) '((:sku a :qty 1))))
  (with-example r (reactel-el 'reactel-ex-cart)
    (should (ex-has r "empty"))
    (cl-flet ((press (n) (funcall (plist-get (reactel-node-props
					      (nth n (reactel-node-find-all (reactel-root-container r)
									    (lambda (i) (eq (reactel-node-type i) :button)))))
					     :on-click))
		(reactel-flush)))
      (press 0) (press 0) (press 1)         ; beans x2, grinder x1
      (should (ex-has r "Cart (3 items)"))
      (should (ex-has r "2 +│ Espresso beans"))
      (should (ex-has r "Total: \\$78.00"))
      (reactel-click r "clear")
      (should (ex-has r "empty"))
      (should (ex-has r "Total: \\$0.00")))))

(ert-deftest example-boundary ()
  (skip-unless (require 'dash nil t))
  (require 'reactel-ex-boundary)
  (with-example r (reactel-el 'reactel-ex-boundary)
    (should (ex-has r "n = 0"))
    (reactel-click r "increment") (reactel-click r "increment")
    (should (ex-has r "n = 2"))
    (reactel-click r "increment")
    (should (ex-has r "I cannot count past 2 (got 3)"))
    (should (ex-has r "errors caught: 1"))
    (reactel-click r "retry")                    ; still broken
    (should (ex-has r "I cannot count"))
    (reactel-click r "zero")                     ; :reset-keys changed
    (should (ex-has r "n = 0"))
    (should-not (ex-has r "I cannot count"))))

(ert-deftest example-store-syncs-with-outside-changes ()
  (skip-unless (require 'dash nil t))
  (require 'reactel-ex-store)
  (reactel-ex-store-set 3)
  (with-example r (reactel-el 'reactel-ex-store)
    (should (ex-has r "value +: 3"))
    (should (ex-has r "squared: 9"))
    (reactel-click r "+1")
    (should (ex-has r "value +: 4"))
    (reactel-act (reactel-ex-store-set 8))
    (should (ex-has r "squared: 64"))
    (should (ex-has r "octal +: 10")))
  (should (null reactel-ex-store-change-hook)))           ; unsubscribed on unmount

(ert-deftest example-jump-ref-and-layout-effect ()
  (skip-unless (require 'dash nil t))
  (require 'reactel-ex-jump)
  (let ((buf "*reactel-jump-test*"))
    (unwind-protect
        (progn
          (reactel-render-buffer buf (reactel-el 'reactel-ex-jump :rows 30 :start 12))
          (with-current-buffer buf
            (should (equal (buffer-substring-no-properties (point) (line-end-position)) "row 12"))
            (goto-char (point-min))
            (let ((root reactel-buffer--root))
              (reactel-click root "next")
              (should (equal (buffer-substring-no-properties (point) (line-end-position)) "row 13"))
              (dotimes (_ 18) (reactel-click root "next"))   ; wraps 30 -> 1
              (should (equal (buffer-substring-no-properties (point) (line-end-position)) "row 01")))))
      (kill-buffer buf))))

;;; Markdown: one component, two backends

(ert-deftest example-markdown-report-as-markdown ()
  (skip-unless (require 'dash nil t))
  (require 'reactel-ex-markdown)
  (should (equal (reactel-ex-markdown-text)
                 (concat "# Sprint report\n\n"
                         "**2 of 4 done** (18 points)\n\n"
                         "## Tasks\n\n"
                         "| Task | Owner | Status | Points |\n"
                         "| --- | --- | --- | ---: |\n"
                         "| Design the backend protocol | ada | done | 5 |\n"
                         "| Buffer backend | ada | done | 8 |\n"
                         "| Markdown backend | grace | doing | 3 |\n"
                         "| Document the protocol | grace | todo | 2 |\n\n"
                         "## Checklist\n\n"
                         "- [x] Design the backend protocol\n"
                         "- [x] Buffer backend\n"
                         "- [ ] Markdown backend\n"
                         "- [ ] Document the protocol\n\n"
                         "> **Note**\n>\n"
                         "> Generated by reactel; the same component renders in a buffer and as Markdown.\n\n"
                         "[Related project: vui.el](https://github.com/d12frosted/vui.el)\n")))
  (should (string-match-p "\\*No tasks match\\.\\*"
                          (reactel-markdown-render
                           (reactel-el 'reactel-ex-markdown-report :title "Empty" :tasks nil)))))

(ert-deftest example-markdown-app-filters-and-exports ()
  (skip-unless (require 'dash nil t))
  (require 'reactel-ex-markdown)
  (let ((kill-ring nil) (kill-ring-yank-pointer nil)
        (r (reactel-create-string-root)))
    (unwind-protect
        (progn
          (reactel-root-render r (reactel-el 'reactel-ex-markdown-app))
          (should (ex-has r "(\\*) all"))
          (should (ex-has r "Markdown backend +│ grace"))
          (reactel-click r "done")
          (should (ex-has r "(\\*) done"))
          (should (ex-has r "2 of 2 done"))
          (should-not (ex-has r "Markdown backend"))
          ;; the live view is the buffer backend, the export is the Markdown backend
          (reactel-click r "Copy as Markdown")
          (should (string-prefix-p "# Sprint report\n\n**2 of 2 done** (13 points)\n"
                                   (car kill-ring)))
          (should-not (string-match-p "Markdown backend" (car kill-ring)))
          (reactel-click r "all")
          (reactel-click r "Copy as Markdown")
          (should (string-match-p "Markdown backend" (car kill-ring))))
      (reactel-unmount r))))

(ert-deftest example-markdown-app-show-buffer ()
  (skip-unless (require 'dash nil t))
  (require 'reactel-ex-markdown)
  (let ((r (reactel-create-string-root)))
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
          (reactel-root-render r (reactel-el 'reactel-ex-markdown-app))
          (reactel-click r "doing")
          (reactel-click r "Show Markdown")
          (with-current-buffer "*reactel-markdown*"
            (should (string-match-p "\\*\\*0 of 1 done\\*\\* (3 points)" (buffer-string)))
            (should (string-match-p "^- \\[ \\] Markdown backend$" (buffer-string)))))
      (reactel-unmount r)
      (when (get-buffer "*reactel-markdown*") (kill-buffer "*reactel-markdown*")))))

;;; Compiler

(ert-deftest example-compiler-compiled-panel-renders-rows-less ()
  (skip-unless (require 'dash nil t))
  (require 'reactel-ex-compiler)
  (reactel-ex-render-counter-reset)
  (let ((r (reactel-create-string-root)))
    (unwind-protect
        (progn
          (reactel-root-render r (reactel-el 'reactel-ex-compiler))
          (dotimes (_ 3) (reactel-click r "tick"))     ; clicks the plain panel first
          (should (ex-has r "plain (3 ticks)"))
          ;; the plain panel rebuilds its rows on every tick: initial render + 3
          (should (= (reactel-ex-render-counter-get '("plain" . "alpha")) 4))
          (should (= (reactel-ex-render-counter-get '("compiled" . "alpha")) 1))
          (let ((compiled (reactel-node-find-all
                           (reactel-root-container r)
                           (lambda (n) (and (eq (reactel-node-type n) :button)
                                            (equal (reactel-node-text n) "tick"))))))
            (should (= (length compiled) 2))
            (reactel-batch (funcall (plist-get (reactel-node-props (cadr compiled)) :on-click)))
            (reactel-batch (funcall (plist-get (reactel-node-props (cadr compiled)) :on-click))))
          (should (ex-has r "compiled (2 ticks)"))
          ;; ...while the compiled one rendered its rows once, however often it ticked
          (should (= (reactel-ex-render-counter-get '("compiled" . "alpha")) 1))
          (should (= (reactel-ex-render-counter-get '("plain" . "alpha")) 4))
          (reactel-click r "beta")
          (should (ex-has r "picked: beta")))
      (reactel-unmount r))))

;;; Larger examples

(defun ex-click-nth (root label n)
  "Activate the Nth (from 0) enabled control labelled LABEL in ROOT."
  (let* ((nodes (reactel-node-find-all
                 (reactel-root-container root)
                 (lambda (x) (memq (reactel-node-type x) '(:button :link :checkbox :select)))))
         (matches (cl-loop for node in nodes
                           for a = (assoc label (reactel-c-actions node))
                           when a collect a))
         (thunk (cadr (nth n matches))))
    (unless thunk (error "No enabled control %S number %d" label n))
    (reactel-batch (funcall thunk))))

(defun ex-wait-until (predicate &optional timeout)
  "Let timers run until PREDICATE holds; error after TIMEOUT (default 3) seconds."
  (let ((deadline (+ (float-time) (or timeout 3))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (sleep-for 0.01))
    (unless (funcall predicate) (error "Timed out waiting for the condition"))))

(defmacro ex-with-root (var element &rest body)
  (declare (indent 2))
  `(let ((,var (reactel-create-string-root)))
     (unwind-protect (progn (reactel-root-render ,var ,element) ,@body)
       (ignore-errors (reactel-unmount ,var)))))

;;; Kanban

(ert-deftest example-kanban-logic ()
  (skip-unless (require 'dash nil t))
  (require 'reactel-ex-kanban)
  (let ((cards reactel-ex-kanban-cards))
    ;; into a full `doing' column: refused, and the very same list is returned
    (should (eq (reactel-ex-kanban--move cards 4 1) cards))
    ;; off the edge of the board
    (should (eq (reactel-ex-kanban--move cards 4 -1) cards))
    (should (eq (reactel-ex-kanban--move cards 1 1) cards))
    ;; making room first
    (let ((moved (reactel-ex-kanban--move cards 2 1)))
      (should (eq (plist-get (--first (= (plist-get it :id) 2) moved) :column) 'done))
      (should (eq (plist-get (--first (= (plist-get it :id) 4) (reactel-ex-kanban--move moved 4 1)) :column)
                  'doing)))
    (should (eq (reactel-ex-kanban--reduce cards '(add "  " "ada")) cards))
    (should (= (length (reactel-ex-kanban--reduce cards '(add "x" "ada"))) 6))
    (should (= (length (reactel-ex-kanban--reduce cards '(remove 1))) 4))))

(ert-deftest example-kanban-history-step ()
  (skip-unless (require 'dash nil t))
  (require 'reactel-ex-kanban)
  (let* ((reducer (lambda (s a) (if (eq a 'noop) s (+ s a))))
         (s0 (list :past nil :present 0 :future nil))
         (s1 (reactel-ex-kanban--history-step reducer s0 '(do 5)))
         (s2 (reactel-ex-kanban--history-step reducer s1 '(do 2))))
    (should (equal (plist-get s2 :present) 7))
    (should (eq (reactel-ex-kanban--history-step reducer s2 '(do noop)) s2)) ; no step recorded
    (should (eq (reactel-ex-kanban--history-step reducer s0 'undo) s0))
    (let ((undone (reactel-ex-kanban--history-step reducer s2 'undo)))
      (should (equal (plist-get undone :present) 5))
      (should (equal (plist-get (reactel-ex-kanban--history-step reducer undone 'redo) :present) 7))
      ;; a new change forgets the redo branch
      (should-not (plist-get (reactel-ex-kanban--history-step reducer undone '(do 1)) :future)))))

(ert-deftest example-kanban-board ()
  (skip-unless (require 'dash nil t))
  (require 'reactel-ex-kanban)
  (ex-with-root r (reactel-el 'reactel-ex-kanban)
    (should (ex-has r "TODO (2)"))
    (should (ex-has r "DOING (2/2)"))
    (should (ex-has r "5 cards, 1 done"))
    ;; moving into the full column is refused: nothing changes, nothing to undo
    (ex-click-nth r "→" 0)
    (should (ex-has r "DOING (2/2)"))
    (should-error (reactel-click r "undo"))
    ;; the first enabled → of the doing column finishes a card
    (ex-click-nth r "→" 2)
    (should (ex-has r "DOING (1/2)"))
    (should (ex-has r "DONE (2)"))
    (ex-click-nth r "→" 0)                                ; now there is room
    (should (ex-has r "DOING (2/2)"))
    (should (ex-has r "TODO (1)"))
    ;; undo twice, redo once
    (reactel-click r "undo") (reactel-click r "undo")
    (should (ex-has r "TODO (2)"))
    (should (ex-has r "DOING (2/2)"))
    (should-error (reactel-click r "undo"))
    (reactel-click r "redo")
    (should (ex-has r "DOING (1/2)"))
    ;; a new card, an owner filter, a removal
    (reactel-type r "New card" "Ship it")
    (reactel-click r "add card")
    (should (ex-has r "Ship it @ada"))
    (should (ex-has r "6 cards"))
    (reactel-click r "grace")
    (should (ex-has r "Compiler @grace"))
    (should-not (ex-has r "Ship it"))
    (reactel-click r "everyone")
    (ex-click-nth r "✕" 0)
    (should (ex-has r "5 cards"))))

;;; File tree

(defmacro ex-with-tree-dir (var &rest body)
  "Run BODY with VAR bound to a temporary directory holding a small tree."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "reactel-tree" t))))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "src/sub" ,var) t)
           (make-directory (expand-file-name "gone" ,var))
           (with-temp-file (expand-file-name "readme.txt" ,var) (insert "one\ntwo\nthree\n"))
           (with-temp-file (expand-file-name "notes.org" ,var) (insert "notes\n"))
           (with-temp-file (expand-file-name ".secret" ,var) (insert "s\n"))
           (with-temp-file (expand-file-name "src/main.el" ,var) (insert "(code)\n"))
           (with-temp-file (expand-file-name "src/sub/deep.el" ,var) (insert "deep\n"))
           ,@body)
       (delete-directory ,var t))))

(ert-deftest example-tree-lists-and-sorts-directories ()
  (skip-unless (require 'dash nil t))
  (require 'reactel-ex-tree)
  (ex-with-tree-dir dir
    (should (equal (reactel-ex-tree--list dir)
		   '(("gone" . t) ("src" . t) (".secret") ("notes.org") ("readme.txt"))))
    (should (equal (reactel-ex-tree--visible (reactel-ex-tree--list dir) "" nil)
		   '(("gone" . t) ("src" . t) ("notes.org") ("readme.txt"))))
    (should (equal (reactel-ex-tree--visible (reactel-ex-tree--list dir) "read" nil)
		   '(("gone" . t) ("src" . t) ("readme.txt"))))
    (should (eq (car (reactel-ex-tree--list (expand-file-name "nope" dir))) :error))
    (should (equal (plist-get (reactel-ex-tree--preview (expand-file-name "readme.txt" dir)) :lines)
		   '("one" "two" "three" "")))))

(ert-deftest example-tree-browse ()
  (skip-unless (require 'dash nil t))
  (require 'reactel-ex-tree)
  (ex-with-tree-dir dir
    (ex-with-root r (reactel-el 'reactel-ex-tree :root dir)
      ;; the root is open, subdirectories are not
      (should (ex-has r "▸ src/"))
      (should (ex-has r "\\[ \\] readme.txt"))
      (should-not (ex-has r "main.el"))
      (should-not (ex-has r "secret"))
      ;; expanding loads the directory (an effect) and nests the children
      (reactel-click r "▸ src/")
      (should (ex-has r "▾ src/\\]\n    \\[▸ sub/\\]\n    \\[ \\] main.el"))
      (reactel-click r "▸ sub/")
      (should (ex-has r "deep.el"))
      (reactel-click r "▾ sub/")
      (should-not (ex-has r "deep.el"))
      ;; selecting a file shows a preview, read in an effect
      (should (ex-has r "select a file"))
      (reactel-click r "readme.txt")
      (should (ex-has r "\\[x\\] readme.txt"))
      (should (ex-has r "readme.txt ─"))
      (should (ex-has r "14 bytes"))
      (should (ex-has r "│ two "))
      ;; filter and hidden files
      (reactel-type r "Filter" "notes")
      (should (ex-has r "notes.org"))
      (should-not (ex-has r "\\] readme.txt"))
      (reactel-type r "Filter" "")
      (should-not (ex-has r "secret"))
      (reactel-click r "hidden files")
      (should (ex-has r "\\.secret"))
      ;; open
      (let (opened)
	(cl-letf (((symbol-function 'find-file) (lambda (f &rest _) (setq opened f))))
	  (reactel-click r "open"))
	(should (equal opened (expand-file-name "readme.txt" dir)))))))

(ert-deftest example-tree-shows-read-errors-in-place ()
  (skip-unless (require 'dash nil t))
  (require 'reactel-ex-tree)
  (ex-with-tree-dir dir
    (ex-with-root r (reactel-el 'reactel-ex-tree :root dir)
      (delete-directory (expand-file-name "gone" dir))   ; vanishes after it was listed
      (reactel-click r "▸ gone/")
      (should (ex-has r "▾ gone/\\]\n    [^\n]*No such file"))
      (should (ex-has r "readme.txt")))))               ; the rest keeps working

;;; Search

(defmacro ex-with-search (var &rest body)
  (declare (indent 1))
  `(let ((reactel-ex-search-requests 0))
     (ex-with-root ,var (reactel-el 'reactel-ex-search :debounce 0.04 :latency 0.12)
       ,@body)))

(ert-deftest example-search-debounces-and-loads ()
  (skip-unless (require 'dash nil t))
  (require 'reactel-ex-search)
  (ex-with-search r
    (should (ex-has r "enter a query"))
    (reactel-type r "Search" "m")
    (reactel-type r "Search" "ma")                      ; typing on: no request yet
    (should (ex-has r "typing"))
    (should (= reactel-ex-search-requests 0))
    (ex-wait-until (lambda () (ex-has r "searching")))
    (should (= reactel-ex-search-requests 1))           ; one request, for the settled query
    (ex-wait-until (lambda () (ex-has r "• ")))
    (should (ex-has r "query: \"ma\""))
    (should (ex-has r "markdown mode"))
    (should-not (ex-has r "react hooks"))   ; it does not contain "ma"
    (should (ex-has r "history: ma"))))

(ert-deftest example-search-cancels-the-request-in-flight ()
  (skip-unless (require 'dash nil t))
  (require 'reactel-ex-search)
  (ex-with-search r
    (reactel-type r "Search" "mode")
    (ex-wait-until (lambda () (ex-has r "searching")))
    (reactel-type r "Search" "macro")                   ; while "mode" is loading
    (ex-wait-until (lambda () (ex-has r "query: \"macro\"")))
    (ex-wait-until (lambda () (ex-has r "macro expansion")))
    ;; the answer for "mode" was cancelled: it never shows, nor enters the history
    (should-not (ex-has r "mode line"))
    (should (ex-has r "history: macro$"))
    (should (= reactel-ex-search-requests 2))))

(ert-deftest example-search-failure-and-retry ()
  (skip-unless (require 'dash nil t))
  (require 'reactel-ex-search)
  (ex-with-search r
    (reactel-type r "Search" "x!")
    (ex-wait-until (lambda () (ex-has r "the server failed")))
    (should (= reactel-ex-search-requests 1))
    (reactel-click r "retry")
    (ex-wait-until (lambda () (= reactel-ex-search-requests 2)))
    (ex-wait-until (lambda () (ex-has r "the server failed")))
    ;; a good query recovers; clearing it goes back to idle
    (reactel-type r "Search" "hooks")
    (ex-wait-until (lambda () (ex-has r "react hooks")))
    (reactel-type r "Search" "")
    (ex-wait-until (lambda () (ex-has r "enter a query")))))

(ert-deftest example-search-stops-its-timers-on-unmount ()
  (skip-unless (require 'dash nil t))
  (require 'reactel-ex-search)
  (let ((before (length timer-list)) (reactel-ex-search-requests 0))
    (ex-with-root r (reactel-el 'reactel-ex-search :debounce 0.5 :latency 0.5)
      (reactel-type r "Search" "emacs")
      (should (> (length timer-list) before)))        ; debounce timer pending
    (should (= (length timer-list) before))))           ; cleaned up by the effect

;;; Life

(ert-deftest example-life-simulation ()
  (skip-unless (require 'dash nil t))
  (require 'reactel-ex-life)
  (let* ((blinker (reactel-ex-life-from-cells 6 6 '((1 . 2) (2 . 2) (3 . 2))))
         (flipped (reactel-ex-life--step blinker))
         (block (reactel-ex-life-from-cells 6 6 '((1 . 1) (2 . 1) (1 . 2) (2 . 2))))
         (glider (reactel-ex-life-from-cells 8 8 '((1 . 0) (2 . 1) (0 . 2) (1 . 2) (2 . 2)))))
    (should-not (equal blinker flipped))
    (should (equal (reactel-ex-life--step flipped) blinker))
    (should (equal (reactel-ex-life--step block) block))
    ;; unchanged rows are shared, which is what lets memoized rows skip
    (should (eq (aref (reactel-ex-life--step blinker) 5) (aref blinker 5)))
    (should (eq (aref (reactel-ex-life--toggle block 1 1) 4) (aref block 4)))
    ;; a glider moves one cell diagonally every four generations
    (let ((w glider)) (dotimes (_ 4) (setq w (reactel-ex-life--step w)))
	 (should (equal w (reactel-ex-life-from-cells 8 8 '((2 . 1) (3 . 2) (1 . 3) (2 . 3) (3 . 3))))))
    (should (= (reactel-ex-life--population glider) 5))))

(ert-deftest example-life-ui ()
  (skip-unless (require 'dash nil t))
  (require 'reactel-ex-life)
  (ex-with-root r (reactel-el 'reactel-ex-life :width 8 :height 6 :interval 0.02)
    (should (ex-has r "generation 0, population 5"))
    (dotimes (_ 4) (reactel-click r "step"))
    (should (ex-has r "generation 4, population 5"))
    ;; draw a cell
    (ex-click-nth r "·" 0)
    (should (ex-has r "population 6"))
    (reactel-click r "clear")
    (should (ex-has r "generation 0, population 0"))
    ;; run: the timer effect advances the generations, stop cancels it
    (reactel-click r "glider")
    (reactel-click r "load")
    (reactel-click r "run")
    (should (ex-has r "running"))
    (ex-wait-until (lambda () (ex-has r "generation [3-9]")))
    (reactel-click r "stop")
    (should-not (ex-has r "running"))
    ;; a still life stops by itself
    (reactel-click r "block")
    (reactel-click r "load")
    (reactel-click r "run")
    (ex-wait-until (lambda () (not (ex-has r "running"))))
    (should (ex-has r "generation 0, population 4"))))

;;; Buffers

(ert-deftest example-buffers-live-view ()
  (skip-unless (require 'dash nil t))
  (require 'reactel-ex-buffers)
  (let ((hook-before (length buffer-list-update-hook)))
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create "rxb-big") (insert (make-string 30 ?x)))
          (with-current-buffer (get-buffer-create "rxb-small") (insert "ab"))
          (ex-with-root r (reactel-el 'reactel-ex-buffers)
	    (reactel-type r "Filter" "rxb-")
	    (should (ex-has r "rxb-big \\*.*\n.*rxb-small \\*"))          ; by name
	    (should (ex-has r "2 buffers, 32 characters"))
	    (reactel-click r "size")
	    (should (ex-has r "rxb-big \\*.*\n.*rxb-small \\*"))          ; biggest first
	    (reactel-click r "mode")
	    ;; Emacs state changing outside of the UI updates the view
	    (reactel-act (with-current-buffer (get-buffer-create "rxb-new") (insert "n")))
	    (should (ex-has r "3 buffers, 33 characters"))
	    (should (ex-has r "rxb-new"))
	    ;; a two-step kill, whose state lives in the row
	    (should-not (ex-has r "sure"))
	    (ex-click-nth r "kill" 0)
	    (should (ex-has r "sure\\?"))
	    (reactel-click r "no")
	    (should-not (ex-has r "sure"))
	    (ex-click-nth r "kill" 0)
	    (reactel-click r "sure?")
	    (should (= 2 (length (--filter (string-prefix-p "rxb-" (buffer-name it)) (buffer-list)))))
	    (should (ex-has r "2 buffers"))
	    ;; internal buffers are hidden unless asked for
	    (with-current-buffer (get-buffer-create " rxb-internal") (insert "i"))
	    (reactel-flush)
	    (should-not (ex-has r "rxb-internal"))
	    (reactel-click r "internal")
	    (should (ex-has r "rxb-internal")))
          (should (= (length buffer-list-update-hook) hook-before)))   ; unsubscribed
      (dolist (b '("rxb-big" "rxb-small" "rxb-new" " rxb-internal"))
        (when (get-buffer b) (kill-buffer b))))))

(provide 'examples-test)
;;; examples-test.el ends here
