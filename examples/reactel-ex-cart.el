;;; reactel-ex-cart.el --- Reactel example: reducer + context, tables -*- lexical-binding: t -*-

;; M-x reactel-demo-cart
;;
;; The cart lives in one `reactel-use-reducer'.  Its `dispatch' is stable,
;; so it is shared through a context: deeply nested components call
;; `reactel-use-context' instead of receiving callbacks as props, and the
;; memoized rows never re-render needlessly.  Components may render the
;; rows of a `reactel-c-table'.

(require 'dash)
(require 'reactel)
(require 'reactel-buffer)

(defconst reactel-ex-cart-catalog
  '((:sku beans :name "Espresso beans" :price 1450)
    (:sku grinder :name "Hand grinder" :price 4900)
    (:sku filters :name "Paper filters" :price 600))
  "Products on sale; prices in cents.")

(defconst reactel-ex-cart-dispatch-context (reactel-create-context #'ignore "CartDispatch"))

(defun reactel-ex-cart--money (cents)
  "Format CENTS as dollars."
  (format "$%d.%02d" (/ cents 100) (% cents 100)))

(defun reactel-ex-cart--product (sku)
  "The catalog entry for SKU."
  (-find (lambda (p) (eq (plist-get p :sku) sku)) reactel-ex-cart-catalog))

(defun reactel-ex-cart--map-line (sku fn cart)
  "Apply FN to the line of SKU in CART."
  (-map-when (lambda (l) (eq (plist-get l :sku) sku)) fn cart))

(defun reactel-ex-cart--with-qty (delta)
  "Function adding DELTA to the :qty of a line."
  (lambda (l) (plist-put (copy-sequence l) :qty (+ delta (plist-get l :qty)))))

(defun reactel-ex-cart--reduce (cart action)
  "Pure transition of CART (a list of (:sku :qty) lines) on ACTION."
  (pcase action
    (`(add ,sku) (if (--any? (eq (plist-get it :sku) sku) cart)
                     (reactel-ex-cart--map-line sku (reactel-ex-cart--with-qty 1) cart)
                   (append cart (list (list :sku sku :qty 1)))))
    (`(dec ,sku) (->> (reactel-ex-cart--map-line sku (reactel-ex-cart--with-qty -1) cart)
                      (--remove (<= (plist-get it :qty) 0))))
    (`(remove ,sku) (--remove (eq (plist-get it :sku) sku) cart))
    ('clear nil)
    (_ (error "Unknown cart action %S" action))))

(reactel-defcomponent reactel-ex-cart-product-row (&key product)
  :memo t
  (-let (((&plist :sku sku :name name :price price) product)
         (dispatch (reactel-use-context reactel-ex-cart-dispatch-context)))
    (reactel-c-row name (reactel-ex-cart--money price)
      (reactel-c-button :on-click (lambda () (funcall dispatch `(add ,sku))) "add"))))

(reactel-defcomponent reactel-ex-cart-line-row (&key line)
  :memo t
  (-let* (((&plist :sku sku :qty qty) line)
          ((&plist :name name :price price) (reactel-ex-cart--product sku))
          (dispatch (reactel-use-context reactel-ex-cart-dispatch-context)))
    (reactel-c-row qty name (reactel-ex-cart--money (* qty price))
      (reactel-c-hstack
        (reactel-c-button :on-click (lambda () (funcall dispatch `(dec ,sku))) "-")
        (reactel-c-button :on-click (lambda () (funcall dispatch `(add ,sku))) "+")
        (reactel-c-button :on-click (lambda () (funcall dispatch `(remove ,sku))) "remove")))))

(reactel-defcomponent reactel-ex-cart ()
  (-let* (((cart dispatch) (reactel-use-reducer #'reactel-ex-cart--reduce nil))
          (total (reactel-use-memo* (cart)
                   (-sum (--map (* (plist-get it :qty) (plist-get (reactel-ex-cart--product (plist-get it :sku)) :price))
                                cart)))))
    (reactel-el reactel-ex-cart-dispatch-context :value dispatch
      (reactel-c-hstack
        (reactel-c-vstack :spacing 1
          (reactel-c-box :title "Shop"
            (reactel-c-table :header nil :align '(left right left)
              (--map (reactel-el 'reactel-ex-cart-product-row :key (plist-get it :sku) :product it)
                     reactel-ex-cart-catalog)))
          (reactel-c-box :title (format "Cart (%d items)" (-sum (--map (plist-get it :qty) cart)))
            (reactel-c-vstack
              (if cart
                  (reactel-c-table :align '(right left right left)
                    (reactel-c-row "Qty" "Item" "Price" "")
                    (--map (reactel-el 'reactel-ex-cart-line-row :key (plist-get it :sku) :line it) cart))
                (reactel-c-text :face 'shadow "empty"))
              (reactel-c-hstack
                (reactel-c-text (format "Total: %s" (reactel-ex-cart--money total)))
                (reactel-c-button :on-click (lambda () (funcall dispatch 'clear))
                  :disabled (null cart) "clear")))))))))

(defun reactel-demo-cart ()
  "Show the cart example."
  (interactive)
  (reactel-show "*reactel-cart*" (reactel-el 'reactel-ex-cart)))

(provide 'reactel-ex-cart)
;;; reactel-ex-cart.el ends here
