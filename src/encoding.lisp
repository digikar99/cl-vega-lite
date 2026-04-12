(in-package :vega-lite)

(define-spec encoding 1
    (x y x-error y-error
     x2 y2 x-error2 y-error2

     color opacity size angle shape

     text tooltip
     href description
     detail key order

     facet row column)
  "An integral part of the data visualization process is encoding data with visual properties of graphical marks. The encoding property of a single view specification represents the mapping between encoding channels (such as x, y, or color) and data fields, constant visual values, or constant data values (datum).

For a full list of encodings, see https://vega.github.io/vega-lite/docs/encoding.html")

(define-spec encoding-channel 2 ()
  "https://vega.github.io/vega-lite/docs/encoding.html")

(define-subclasses (encoding-channel)
  position-channel
  position-offset-channel
  polar-position-channel
  geographic-position-channel
  mark-property-channel
  text-and-tooltip-channel
  hyperlink-channel
  description-channel
  level-of-detail-channel
  key-channel
  order-channel
  facet-channel)

(setf (documentation 'geographic-position-channel 'type)
      "https://vega.github.io/vega-lite/examples/geo_circle.html")

(define-spec field-definition 2
    (field type aggregate bin sort stack time-unit band title)
  "https://vega.github.io/vega-lite/docs/encoding.html#field-def")

(define-spec value-definition 2 (value)
  "References:

1. https://vega.github.io/vega-lite/docs/encoding.html#value-def
2. https://vega.github.io/vega-lite/docs/value.html")

(define-spec datum-definition 2
    (datum)
  "https://vega.github.io/vega-lite/docs/encoding.html#datum-def")

;; Ideally, channel definition must be a disjunction of these.
;; But that complicates the class labels and hierarchy.
(define-subclasses (field-definition value-definition datum-definition) channel-definition)

(define-spec x (encoding-channel channel-definition) (scale axis sort impute stack))
(define-spec y (encoding-channel channel-definition) (scale axis sort impute stack))
(define-spec x2 (encoding-channel channel-definition) (impute)
  "x2 and y2 do not have their own definitions for scale, axis, sort, and stack since they share the same scales and axes with x and y respectively.")
(define-spec y2 (encoding-channel channel-definition) (impute)
  "x2 and y2 do not have their own definitions for scale, axis, sort, and stack since they share the same scales and axes with x and y respectively.")

(define-spec x-offset (position-offset-channel channel-definition) (scale sort))
(define-spec y-offset (position-offset-channel channel-definition) (scale sort))

(define-spec theta (polar-position-channel channel-definition) (scale stack sort)
  "theta and radius position channels determine the position or interval on polar coordinates for arc and text marks.")
(define-spec radius (polar-position-channel channel-definition) (scale stack sort)
  "theta and radius position channels determine the position or interval on polar coordinates for arc and text marks.")
(define-spec theta2 (polar-position-channel channel-definition) ())
(define-spec radius2 (polar-position-channel channel-definition) ())

(define-spec longitude (geographic-position-channel channel-definition) ())
(define-spec latitude (geographic-position-channel channel-definition) ())
(define-spec longitude2 (geographic-position-channel channel-definition) ())
(define-spec latitude2 (geographic-position-channel channel-definition) ())

(macrolet ((def (&rest names)
             `(progn
                ,@(loop :for name :in names
                        :collect `(define-spec ,name
                                      (mark-property-channel channel-definition)
                                      (field type bin time-unit aggregate condition))))))
  ;; FIXME: Datum definitions
  (def angle color fill stroke opacity fill-opacity stroke-opacity shape size stroke-dash stroke-width))

(define-spec text (text-and-tooltip-channel channel-definition)
    (format format-type condition))
(define-spec tooltip (text-and-tooltip-channel channel-definition)
    (format format-type condition))

(define-spec href (hyperlink-channel channel-definition) (condition))

(define-spec description (description-channel channel-definition)
    (format format-type condition)
  "By setting the description channel, you can add a text description to the mark for ARIA accessibility (SVG output only). The \"aria-label\" attribute in the generated SVG will be set to this description.

By default, Vega-Lite generates a description based on the encoding similar to default tooltips. To disable automatic descriptions, set config.aria to false. No description will be generated if mark.aria is set to false.")

(define-spec detail (level-of-detail-channel channel-definition) ())
(define-spec key (key-channel channel-definition) ())
(define-spec order (order-channel channel-definition) (sort condition))

(define-spec facet (facet-channel channel-definition) ())
(define-spec row (facet-channel channel-definition) ())
(define-spec column (facet-channel channel-definition) ())
