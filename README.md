# reactel

React.js for Emacs Lisp: **function components**, **hooks**, a keyed
**reconciler**, context, memoization, error boundaries and batched updates.
Design reference for the Elisp idioms: [vui.el](https://github.com/d12frosted/vui.el)
(which uses class-like components with `:state`; reactel instead follows React's
function-component/hook model).

## Pluggable render backends

Components describe *what* to show; a **render backend** decides *how*. The core
(`reactel.el`) never draws anything. After each commit it hands the backend only the
changes, as mutations of the backend's own nodes, like react-reconciler's host config:

```
 components + hooks ──► reconciler ──► reactel-backend ──► your output
 (reactel-defcomponent)  (reactel.el)    create-node          Emacs buffer / string
                                         update-node          Markdown text
                                         set-children         node tree (tests)
                                         destroy-node         ...anything else
                                         finish-commit
```

| Backend | File | Output |
|---|---|---|
| Emacs buffer / string | `reactel-buffer.el` | interactive text UI with `button.el` buttons, faces, boxes, tables |
| Markdown | `reactel-markdown.el` | Markdown text (static); the same components render here unchanged |
| Node tree | `reactel-node.el` | a plain tree of `reactel-node`s, for tests, inspection and as the base of simple backends |

The same component renders on every backend that supports the components it uses:

```elisp
(reactel-markdown-render (reactel-el 'card :title "Stock"))   ; => "## Stock\n\n- apple\n..."
(reactel-show "*card*" (reactel-el 'card :title "Stock"))     ; interactive buffer
```

**Writing a backend.** The simplest route is a retained node tree plus a painter, which
`reactel-node-make-backend` provides: the core keeps the tree up to date and your `:finish`
function walks it after every commit.

```elisp
(require 'reactel-node)

(defvar my-backend
  (reactel-node-make-backend
   :name 'my
   :valid-p (lambda (type) (memq type '(:text :vstack)))      ; others are reported as errors
   :finish (lambda (container)                                 ; paint the tree under CONTAINER
             (message "%s" (reactel-node-text container)))))

(let ((root (reactel-create-root my-backend)))
  (reactel-root-render root (reactel-c-vstack "hello" (reactel-c-text "world"))))
```

For a retained-mode target (widgets, SVG, a remote UI) implement the protocol directly with
`reactel-make-backend`:

| Function | Called |
|---|---|
| `create-node (type props)` | for each new host element; `type` is a keyword, or `:string` for text (`:value`) |
| `update-node (node type old-props new-props)` | when props changed |
| `set-children (node child-nodes)` | when the ordered children changed; also for the container |
| `destroy-node (node)` | when a node is gone for good (children first) |
| `finish-commit (container)` | once after all the mutations of a commit |
| `valid-type-p`, `create-container` | optional |

Props never contain `:children`, `:key` or `:ref`; refs receive your nodes. Event handlers are
plain functions in the props: call them through `reactel-batch` so the updates they cause are
applied once. Only nodes that changed are touched (see `test/backend-test.el`, which records the
calls of a backend to check this).

| File | Contents |
|---|---|
| `reactel.el` | Backend-agnostic core: elements, components, hooks, reconciler, scheduler, the `reactel-backend` protocol |
| `reactel-c.el` | The standard component vocabulary `reactel-c-*` and the interaction helpers (`reactel-click`, `reactel-type`) |
| `reactel-node.el` | Generic node-tree backend and the headless test backend |
| `reactel-buffer.el` | Emacs buffer / string backend |
| `reactel-markdown.el` | Markdown backend |
| `reactel-lint.el` | Static checks of the Rules of React (`make lint`, flymake, byte-compile warnings) |
| `reactel-compile.el` | The compiler: automatic memoization of components, like the React Compiler |
| `test/*.el` | ERT suites: core, backend protocol, lint, strict mode, buffer backend, examples (`make test`) |
| `examples/*.el` | Fifteen demo apps using dash.el (see below) |

Design reference for the Elisp idioms: [vui.el](https://github.com/d12frosted/vui.el)
(which uses class-like components with `:state`; reactel instead follows React's
function-component/hook model).

## Function components

A component is any function taking props as a keyword plist.
`reactel-defcomponent` is `cl-defun` with `&allow-other-keys` added:

```elisp
(reactel-defcomponent counter (&key (initial 0) label)
  (cl-destructuring-bind (n set-n) (reactel-use-state initial)
    (reactel-c-hstack
      (reactel-c-text (format "%s%d" (or label "") n))
      (reactel-c-button :on-click (lambda () (funcall set-n reactel-inc)) "+"))))

(reactel-show "*counter*" (reactel-el 'counter :initial 5 :label "n="))
```

`(reactel-el TYPE :prop value ... CHILDREN...)` builds an element. Children can be
elements, strings, numbers, `nil` (ignored) or lists (flattened) and reach the component as `children`.
`:key` and `:ref` are reserved. Plain strings are text.

**Naming:** a component is an ordinary global function, so give yours a package prefix
(`myapp-todo-item`), as for any Elisp function. The examples use `reactel-ex-`
(`reactel-ex-todo`, `reactel-ex-todo-item`, ...) and the tests use `reactel-test-`.

## Components (`reactel-c.el`)

Instead of web-style `div`/`span`, the standard components are what Emacs UIs are made of. They are
backend-neutral (`reactel-el` with a keyword type); the buffer backend paints all of them, other
backends may support a subset:

| Primitive | Meaning |
|---|---|
| `reactel-c-text` | inline text; `:face`, `:help` (tooltip), `:props` (any text properties) |
| `reactel-c-newline`, `reactel-c-space` | `(reactel-c-newline 2)`, `(reactel-c-space 4)` |
| `reactel-c-vstack` | one child per line; `:spacing` blank lines, `:indent`, `:face` |
| `reactel-c-hstack` | children side by side; `:spacing` |
| `reactel-c-box` | border around children; `:border` single/double/rounded/ascii, `:padding`, `:title` |
| `reactel-c-table` + `reactel-c-row` | aligned columns; `:header`, `:align '(left right)`, `:separator`. Components may render rows |
| `reactel-c-list` | bullets with hanging indent; `:ordered`, `:bullet` |
| `reactel-c-heading`, `reactel-c-separator` | `outline-N` faced line, horizontal rule |
| `reactel-c-button`, `reactel-c-link` | real `button.el` buttons: TAB/S-TAB, RET, mouse, `push-button` |
| `reactel-c-checkbox`, `reactel-c-select` | `[x] label`; radio group `(*) a  ( ) b` (`:layout 'vertical`) |
| `reactel-c-field` | `[value____]` edited in the minibuffer (`completing-read` with `:completions`) |

```
┌─ Cart (3 items) ─────────────────────────────────┐
│ Qty │ Item           │  Price │                  │
│ ──────────────────────────────────────────────── │
│   3 │ Espresso beans │ $43.50 │ [-] [+] [remove] │
│ Total: $43.50 [clear]                            │
└──────────────────────────────────────────────────┘
```

Notes (buffer backend): table cells are single-line; a box or table that is the first thing in a list
item starts on the line after the bullet. Buttons without a handler are drawn disabled.

## Indentation

Components nest deeply, so they declare Emacs indentation rules and **children are indented by two
columns** instead of being aligned under the first argument. `reactel-el`, all `reactel-c-*` constructors
and `reactel-c-fragment` use `(declare (indent defun))`; `reactel-defcomponent`, `reactel-defhook`,
`reactel-lambda`, the `reactel-use-*` macros, `reactel-let-hooks` and `reactel-batch` already declared theirs.
Nothing to configure: `indent-region` or `TAB` in `emacs-lisp-mode` do the right thing once `reactel` is loaded.

```elisp
(reactel-c-vstack :spacing 1
  (reactel-c-heading "Title")
  (reactel-el 'row :key id :item item
    :on-toggle toggle)
  (reactel-c-hstack
    (reactel-c-button :on-click save "save")
    (reactel-c-button :on-click cancel :disabled busy "cancel")))
```

Give your own element-building functions the same rule, either in the definition with
`(declare (indent defun))` or with `(put 'my-fn 'lisp-indent-function 'defun)`. `make indent` reindents
all sources of this project the same way, and a test keeps the library and the examples from drifting.

## Hooks

Hooks return lists, so destructure them with `cl-destructuring-bind` / `pcase-let`.

| Hook | Notes |
|---|---|
| `(reactel-use-state INITIAL)` → `(value setter)` | `INITIAL` may be a 0-arg function object (lazy). Setter takes a value or a 1-arg updater function object (`reactel-inc`, `reactel-dec`, `reactel-toggle`, a lambda), has stable identity, bails out when `equal` to current. **Symbols are always plain values** (`'all` is fine even if `all` is a function) |
| `(reactel-use-reducer REDUCER INIT &optional INIT-FN)` → `(state dispatch)` | |
| `(reactel-use-effect FN DEPS)` | `FN` may return a cleanup. `DEPS` is a list (compared with `equal-including-properties`), `nil` = once, **`:always`** = every render |
| `(reactel-use-layout-effect FN DEPS)` | runs after paint, before passive effects |
| `(reactel-use-memo FN DEPS)`, `(reactel-use-callback FN DEPS)` | |
| `(reactel-use-ref &optional INITIAL)` | `reactel-ref-current` (setf-able); pass a ref as `:ref` to a host element to get its backend node (e.g. `reactel-buffer-node-region`, `reactel-buffer-goto-node`) |
| `(reactel-use-context CTX)` | with `reactel-create-context`; provider is `(reactel-el CTX :value v ...)` |
| `(reactel-use-id)`, `(reactel-use-sync-external-store SUBSCRIBE GET)` | |

The Rules of Hooks hold and are *checked*: calling hooks in a different order, or outside a
render, signals an error. Since Elisp has no `undefined`, `DEPS` is a required argument.

## Hook macros

Each effect/memo/callback hook has a macro twin ending in `*` that wraps the body in a
lambda. `DEPS` is written as a list of *forms to watch* (not quoted), `()` means once,
`:always` means every render:

```elisp
(reactel-use-effect* (user-id)            ; = (reactel-use-effect (lambda () ...) (list user-id))
  (let ((sub (subscribe user-id)))
    (lambda () (unsubscribe sub))))       ; the value of the body is the cleanup

(reactel-use-layout-effect* (selected) ... nil)
(reactel-use-memo* (items filter) (--filter (matches it filter) items))
(reactel-use-callback* (id) (value) (save id value))   ; (lambda (value) ...)
```

`reactel-let-hooks` is `let*` with destructuring, to keep a component flat:

```elisp
(reactel-let-hooks (((count set-count) (reactel-use-state 0))
                    (inc (reactel-use-callback* () () (funcall set-count reactel-inc))))
  ...)
```

The examples use `-let*` from [dash.el](https://github.com/magnars/dash.el) for the same
purpose, which additionally destructures plists (`(&plist :id id)`). The core does not
depend on dash.

## Examples

Run `M-x reactel-examples` after `(add-to-list 'load-path "examples")` and
`(require 'reactel-examples)`, or one of the commands below. They need dash.el.

| Command | Shows |
|---|---|
| `reactel-demo-counter` | `box`, state, `use-callback*`, effect on a changing value |
| `reactel-demo-stopwatch` | `use-reducer`, a timer effect with cleanup, a `table` of laps |
| `reactel-demo-todo` | keyed lists, `checkbox`/`select`/`field`, `:memo t` with stable callbacks (render counters prove it) |
| `reactel-demo-form` | controlled `field`s in a `table`, validation via `use-memo*`, `list` of errors |
| `reactel-demo-cart` | reducer + context, memoized components rendering table rows |
| `reactel-demo-boundary` | `:error-boundary` with a `double` box fallback, retry and `:reset-keys` |
| `reactel-demo-store` | `use-sync-external-store`: sync with state living outside Reactel |
| `reactel-demo-jump` | `:ref` on host elements + layout effect to move point |
| `reactel-demo-compiler` | the same panel plain and with `:compile t`: render counters show the compiled one skipping its static rows |
| `reactel-demo-kanban` | a board with rules: pure reducer, a custom `reactel-ex-use-history` hook adding undo/redo to any reducer, context, columns memoized with a custom comparison, WIP limit |
| `reactel-demo-tree` | a file browser: a recursive component with per-node state, directory listing and file preview done in effects (stale loads dropped by cleanups), errors shown in place, filter and hidden-file toggle |
| `reactel-demo-search` | search-as-you-type against a slow, failing "server": `use-debounced` and `use-search` hooks, requests cancelled when the query changes, animated spinner, retry, history |
| `reactel-demo-life` | Conway's Game of Life: pure simulation, timer effect that starts/stops with state, rows memoized with `:memo` and a comparison function, stops by itself on a still life |
| `reactel-demo-buffers` | a live manager for Emacs's own buffers via `use-sync-external-store` on `buffer-list-update-hook`; sorting, filtering, a two-step kill with state in each row |
| `reactel-demo-markdown` | one component, two backends: a live report in a buffer, exported as Markdown by the Markdown backend (copy / show buttons) |

`test/examples-test.el` drives every example headlessly with `reactel-click` (buttons, links, checkboxes, radio options) and
`reactel-type` (fields).

## Enforcing the Rules of React

reactel checks the [Rules of React](https://react.dev/reference/rules) in two layers.

**Static (`reactel-lint.el`).** When `reactel-defcomponent`, `reactel-lambda` or
`reactel-defhook` is macroexpanded (so while byte-compiling or evaluating), the body is
`macroexpand-all`ed and analyzed. Findings are warnings with the component name, via
`macroexp-warn-and-return`:

| Rule | Reported when |
|---|---|
| `hooks-top-level` | a hook is called in an `if`/`when`/`cond`/`and`/`or` branch, a loop (`dolist`, `cl-loop`, `while`), a `condition-case` handler, a nested lambda (e.g. inside an effect callback) or after an early return (`cl-return-from`/`throw`) |
| `hooks-callers` | a plain `defun` calls a hook (use `reactel-defcomponent`, or `reactel-defhook` for custom hooks; names containing `use-` are accepted by the file linter) |
| `hooks-as-values` | a hook is passed around, e.g. `(mapcar #'reactel-use-state ...)` |
| `component-call` | a component is called directly instead of `(reactel-el 'name ...)` (also an error at runtime, always) |
| `exhaustive-deps` | an effect/memo/callback body uses a prop, state value or derived value that is not in its dependency list. Setters, dispatch functions and refs are exempt |

`M-x reactel-lint-flymake` is a flymake backend (`(add-hook 'flymake-diagnostic-functions
#'reactel-lint-flymake nil t)`), `make lint` checks the project and exits nonzero on findings.
`reactel-lint-severity` is `warn` (default), `error` or `nil`; `reactel-lint-exhaustive-deps`
toggles the dependency check. Like React's own checks, the analysis is conservative: a balanced
`(if c (reactel-use-state 1) (reactel-use-state 2))` is still a violation.

**Runtime strict mode.** `(setq reactel-strict t)` or `M-x reactel-strict-mode` (also guards
timers/processes/hooks started while rendering) turns on, individually selectable via a list:

| Check | Catches |
|---|---|
| `purity` | rendering twice gives different output (writing refs or variables, reading the clock); impure `use-memo` / reducer functions |
| `mutation` | props, state, `use-memo` values, or element props mutated in place. Also reveals in-place state mutation that the equal-state bail-out would otherwise hide |
| `render-update` | updating another component's state while rendering |
| `effects` | effects are mounted twice (mount, cleanup, mount) so a missing cleanup shows up |
| `buffer` | a component modifies the current buffer while rendering |
| `impure` | (with `reactel-strict-mode`) `run-with-timer`, `make-process`, `add-hook` ... while rendering |

Opt a component out of both layers with `:lint nil`; none of the examples needs it (they all pass every check,
see `test/strict-test.el`). The todo and compiler examples count renders the legitimate way, in an effect, with the
count shown by a separate component through `reactel-use-sync-external-store`
(`examples/reactel-ex-render-counter.el`). `make test-strict` runs the library tests with the checks on.

## The compiler: automatic memoization

Like the [React Compiler](https://react.dev/learn/react-compiler/introduction), `reactel-compile.el`
makes `reactel-use-memo`, `reactel-use-callback` and `:memo` unnecessary in the common cases. It relies
on the Rules of React (pure rendering, no mutation after creation), which the linter and strict mode check.

```elisp
(reactel-defcomponent counter (&key label)
  :compile t                       ; or (setq reactel-compile t) for all components
  (cl-destructuring-bind (n set-n) (reactel-use-state 0)
    (reactel-c-vstack
      (reactel-c-text "Static header")
      (reactel-c-text (format "%s: %d" label n))
      (reactel-c-button :on-click (lambda () (funcall set-n reactel-inc)) "+"))))
```

compiles to (see `M-x reactel-compile-show`):

```elisp
(let ((cache (reactel--use-cache 3)))
  ... (reactel-use-state 0) ...
  (reactel-c-vstack
    (reactel--memo cache 2 nil (reactel-c-text "Static header"))     ; built once
    (reactel-c-text (format "%s: %d" label n))                       ; merged into the scope below
    (reactel--memo cache 4 nil (reactel-c-button :on-click #'(lambda () ...) "+"))))
;; and the whole vstack is cached by (label n) in slot 0
```

How it works:

- **Scopes** are expressions whose identity matters: element constructors (`reactel-el`, `reactel-c-*`),
  closures, and allocating or iterating calls (`list`, `mapcar`, `--map`, ...).
- **Dependencies** are the reactive values a scope reads: props, state, and values derived from them.
  Setters, dispatch functions, refs and `defconst` constants are stable and never a dependency.
- Each scope is wrapped in a cache lookup keyed by its dependencies (compared like React's `Object.is`:
  `eq`/`eql`, strings by content and text properties), stored in one per-component cache hook. When nothing changed the old
  object is returned, so an unchanged element is `eq` to the previous one and the reconciler skips its whole
  subtree, and a closure keeps its identity so `:memo` children really skip. A scope with the same
  dependencies as the one around it is merged into it; a scope without dependencies is built once.

What it will **not** memoize (so behaviour is preserved, verified by a differential test that runs compiled
and plain components side by side on random events, and by running the whole test suite compiled with
`make test-compiled`):

- expressions calling hooks, impure functions (`random`, `current-time`, buffer and timer functions,
  `message`, ...), reading refs, or exiting non-locally (`throw`);
- expressions reading variables that are neither lexical variables of the component nor `defconst`
  constants (globals can change behind its back), or variables mutated anywhere in the body
  (`setq`, `setcar`, `puthash`, ...). Mutation that stays inside a self-contained loop is fine;
- anything inside a loop or lambda body: it runs several times or later, outside the render.

Like the real compiler it is conservative about the code it is given: a component that breaks the Rules
of Hooks is **skipped** with the reason recorded (a warning is shown), never miscompiled. Calls to
functions it cannot see into are assumed pure, as in React; register your own element-building helpers with
`(put 'my-fn 'reactel-constructor t)` to make their calls scopes. Opt out with `:compile nil` or `:lint nil`.
Compilation needs `lexical-binding`. It is off by default so existing code never changes behaviour unnoticed.

## Semantics worth knowing

* **Reconciliation**: children match by `:key`, else by position; a different type remounts
  (state lost, cleanups run). Don't create components with `lambda` during render.
* **Memoization**: `:memo t` in `reactel-defcomponent`, or `(reactel-memo 'fn [COMPARE])`.
  Props are compared shallowly (`eq`; strings by content and text properties). Returning the same element object
  (e.g. via `use-memo`) also skips a subtree. Context changes still reach memoized children.
* **Batching**: updates inside event handlers or `reactel-batch` are applied in one render
  at the end; updates from timers/processes are coalesced via a 0s timer
  (`reactel-synchronous` to apply immediately). Tests use `reactel-act`/`reactel-flush`.
* **Render-phase updates** (calling a setter during render) re-run that component, as in React;
  runaway loops (render or effect) are detected.
* **Effects**: children before parents, layout before passive; cleanups run before the next
  run and on unmount (also when the buffer is killed). Errors in effects don't abort a commit;
  the first is re-signalled at the end.
* **Errors**: `:error-boundary` (`:fallback` element or `(lambda (err reset))`, `:on-error`,
  `:reset-keys`) catches errors from render. An uncaught render error unmounts the root.

## Differences from React.js

No concurrent rendering, Suspense, portals, or `forwardRef`. The core mutates backend nodes
minimally, but the buffer backend repaints the whole buffer on each commit (point and window
positions are preserved) rather than patching text.
`reactel-c-field` edits through the minibuffer, not in place.

## Testing

`make test`, `make test-strict`, `make test-compiled`, `make lint`, `make indent` (set `EMACS=...`; `DASH_DIR=...` if dash.el is not found automatically), `make compile`.
Use `reactel-create-string-root`, `reactel-root-render`, `reactel-root-text`, `reactel-click` (buttons, links,
checkboxes, radio options), `reactel-type` (fields) and `reactel-act`. The last two work on any backend built on
`reactel-node.el`; for tests that do not care about text, `reactel-node-test-root` and `reactel-node-root-snapshot`
give a printable tree, and `test/backend-test.el` shows a recording backend that checks which mutations a commit makes.
