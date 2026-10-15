# reactel

React.js for Emacs Lisp: function components, hooks, a keyed reconciler, context, memoization,
error boundaries, batched updates, and enforcement of the Rules of React. See `README.md` for the
user-facing documentation.

## Key feature: pluggable render backends

The core never draws anything. After each commit the reconciler gives a **render backend**
(`reactel-backend`, defined in `reactel.el`; the equivalent of react-reconciler's host config) the
changes as mutations of the backend's own nodes: `create-node`, `update-node`, `set-children`,
`destroy-node`, then `finish-commit`. Only nodes that changed are touched.

- Components are backend-neutral: they return `reactel-el` elements. The standard vocabulary is
  `reactel-c-*` in `reactel-c.el`; each backend states which host types it supports
  (`valid-type-p`) and unsupported ones are an error.
- `reactel-node.el` is a generic retained node tree plus `reactel-node-make-backend`; most backends
  only add a painter as `:finish`. Include `reactel-node` in your own node struct for extra state.
- Shipped backends: `reactel-buffer.el` (Emacs buffer / string, interactive, `button.el` buttons),
  `reactel-markdown.el` (static Markdown), and `reactel-node-test-backend` (headless, for tests).
- Keep the core free of backend knowledge: no buffer, text, face or painting code in `reactel.el`.
  Nodes belong to the backend; instances (core) only hold a `node`. Refs (`:ref`) receive nodes.
- Event handlers are plain functions in props; a backend must call them through `reactel-batch`.
- A new backend needs a test in `test/backend-test.el` style: render the same components, and use a
  recording backend to check which mutations a commit makes.

## Layout

| File | Role |
|---|---|
| `reactel.el` | Core: elements, `reactel-defcomponent`/`reactel-defhook`, hooks, reconciler, scheduler, `reactel-backend`, strict mode |
| `reactel-c.el` | `reactel-c-*` components, interaction helpers (`reactel-c-actions`, `reactel-click`, `reactel-type`) |
| `reactel-node.el` | Node-tree backend base, test backend, snapshots |
| `reactel-buffer.el` | Buffer/string backend (painter, `reactel-show`, `reactel-render-to-string`) |
| `reactel-markdown.el` | Markdown backend |
| `reactel-lint.el` | Static Rules-of-React analyzer (runs at macroexpansion; `make lint`; flymake) |
| `reactel-compile.el` | The compiler: automatic memoization (React Compiler analogue), built on the lint analysis |
| `examples/` | Demo apps using dash.el, one `reactel-demo-*` command each, menu `reactel-examples` |
| `test/` | ERT: `reactel-test` (core), `backend-test`, `buffer-test`, `lint-test`, `strict-test`, `compile-test`, `examples-test` |

## Commands

```
make test         # all ERT tests            (EMACS=emacs-nox on this machine)
make test-strict  # library tests with strict runtime checks on
make test-compiled # whole suite with every component compiled (also with strict checks)
make lint         # static Rules of React check over the sources
make compile      # byte-compile, warnings are errors; cleans up .elc files
make indent       # reindent all sources with Emacs's rules (scripts/reindent.el)
make bench        # repaint benchmark of the buffer backend, byte-compiled (scripts/bench.el)
```

Emacs is installed as `emacs-nox`; dash.el is found automatically (`DASH_DIR=` to override) and is
needed only for `examples/` and their tests. The core has no dependency besides Emacs 27.1+.

## Conventions

- Prefixes: `reactel-` public core API, `reactel--` private, `reactel-c-` components,
  `reactel-node-`, `reactel-buffer-`, `reactel-markdown-`, `reactel-lint-`. Components in examples
  are `reactel-ex-*`, in tests `reactel-test-*`; a component is a global function, so user
  components need their own prefix.
- Indentation: components indent their children by two columns. `reactel-el` and every host
  constructor (`reactel--define-host`, `reactel-c-fragment`) declare `(indent defun)`; a new
  element-building function must too. Files are kept `indent-region`-clean (`make indent`;
  `test/indent-test.el` checks the library and the examples).
- `lexical-binding: t` everywhere. Docstrings stay within 80 columns: the byte-compiler treats longer
  ones as errors here (`make compile` uses `byte-compile-error-on-warn`).
- State values may be symbols: only function *objects* (lambdas) are updaters or lazy initializers.
  Use `reactel-inc`, `reactel-dec`, `reactel-toggle` or a lambda; never rely on `#'fn` symbols.
- Hook deps: a list (compared with `equal-including-properties`), `nil` = run once, `:always` = every
  render. Text properties count everywhere values are compared to skip work: deps, state and store
  snapshots, `:memo` props and compiler deps (strings by content *and* properties, never `string=`).
  Macro forms `reactel-use-effect*` etc. take the deps as forms to watch, not a quoted list.
- Hooks follow the Rules of Hooks; the linter and `:lint nil` (opt-out, also for strict mode) are
  documented in the README. Examples must pass lint and every strict check without `:lint nil`
  (`strict-every-example-passes-all-checks`); to show render counts use `reactel-ex-render-counter.el`
  (count in an effect, display via `reactel-use-sync-external-store`), never a ref written in render.
- Tests: ERT, no mocks of the reconciler. Use `reactel-create-string-root` / `reactel-node-test-root`,
  `reactel-click`, `reactel-type`, `reactel-act`. Timer-based updates are coalesced by a 0s timer; use
  `reactel-flush` or `reactel-batch` in tests.

## Gotchas

- A stale `.elc` next to a changed `.el` is loaded first and can fail confusingly; `make compile`
  removes them, delete leftovers if a compile was interrupted.
- Components created with `lambda` during render are a new type each render (remounted every time).
- Table cells in the buffer backend are single-line; a box or table that starts a list item begins
  on the line after the bullet.
- Compiler (`reactel-compile.el`, option `:compile`, default off): it rewrites the macroexpanded body,
  wrapping scopes (element constructor calls, closures, allocators) in `reactel--memo` keyed by their
  reactive dependencies, in one `reactel--use-cache` hook. Keep these invariants when changing it:
  never memoize inside loops or lambda bodies (a site runs many times or later), never an expression with
  hook calls, impure calls (`reactel-compile--impure-calls`), ref reads, `throw`, non-constant globals, or
  variables mutated anywhere in the body; skip the whole component (with a reason) when the linter finds a
  Rules-of-Hooks violation. Soundness is checked by `test/compile-test.el` (incl. a randomized
  compiled-vs-plain differential test) and by `make test-compiled`; run both after any change. Tests that
  assert exact render or mutation counts pin `:compile nil`. Calls to unknown functions are assumed pure.
