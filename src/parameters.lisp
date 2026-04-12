(in-package :vega-lite)

(define-spec variable-param 1 (expr)
  "References:

1. https://vega.github.io/vega-lite/docs/parameter.html#variable-parameters")

(define-spec selection-param 1 (select))

(define-spec param (variable-param selection-param)
    (name value
     (bind :initarg :bind
           :type (or nil scale-binding input-element-binding legend-binding)))
  "https://vega.github.io/vega-lite/docs/parameter.html")

(deftype params () `(cons param list))

(define-spec input-element-binding 2
    ((input :allocation :class)
     element name debounce)
  "References:

1. https://vega.github.io/vega-lite/docs/bind.html#input-element-binding
2. https://vega.github.io/vega/docs/signals/#bind")

(define-spec radio-input-binding (input-element-binding)
    ((input :initform :radio) labels))
(define-spec select-input-binding (input-element-binding)
    ((input :initform :select) labels))
(define-spec range-input-binding (input-element-binding)
    ((input :initform :range) max min step))

(deftype scale-binding () `(eql :scales))

(define-spec legend-binding 2 (legend))

(define-spec select (param)
    ((type :type (member :point :interval) :initarg :type)
     encodings fields on clear resolve
     mark translate zoom)
  "References:

1. https://vega.github.io/vega-lite/docs/parameter.html#select
2. https://vega.github.io/vega-lite/docs/selection.html")
