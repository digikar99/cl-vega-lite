(cl:defpackage :vega-lite
  (:use :cl)
  (:shadow #:fill)
  (:export #:*plot-server*
           #:+default-port+
           #:+default-address+
           #:ensure-plot-server

           #:vega-plot
           #:save-plot

           #:compose
           #:vega-compose
           #:vega-dataset

           #:spec
           #:plot
           #:make-plot
           #:concat
           #:make-concat

           #:data
           #:make-data
           #:datasets
           #:meke-datasets
           #:mark
           #:make-mark

           #:param
           #:params
           #:select

           #:make-param
           #:make-select

           #:encoding
           #:x
           #:y
           #:x2
           #:y2
           #:x-offset
           #:y-offset
           #:theta
           #:radius
           #:theta2
           #:radius2
           #:longitude
           #:latitude
           #:longitude2
           #:latitude2
           #:angle
           #:color
           #:fill
           #:stroke
           #:opacity
           #:fill-opacity
           #:stroke-opacity
           #:shape
           #:size
           #:stroke-dash
           #:stroke-width
           #:text
           #:tooltip
           #:href
           #:description
           #:detail
           #:key
           #:order
           #:facet
           #:row
           #:column

           ;; Copied above block with make- prefix
           #:make-encoding
           #:make-x
           #:make-y
           #:make-x2
           #:make-y2
           #:make-x-offset
           #:make-y-offset
           #:make-theta
           #:make-radius
           #:make-theta2
           #:make-radius2
           #:make-longitude
           #:make-latitude
           #:make-longitude2
           #:make-latitude2
           #:make-angle
           #:make-color
           #:make-fill
           #:make-stroke
           #:make-opacity
           #:make-fill-opacity
           #:make-stroke-opacity
           #:make-shape
           #:make-size
           #:make-stroke-dash
           #:make-stroke-width
           #:make-text
           #:make-tooltip
           #:make-href
           #:make-description
           #:make-detail
           #:make-key
           #:make-order
           #:make-facet
           #:make-row
           #:make-column

           #:axis
           #:make-axis
           #:scale
           #:make-scale
           #:bin
           #:make-bin

           #:+data-types+
           #:+mark-types+
           #:+channel-types+
           #:+scale-types+

           #:1d-data-as-list

           #:bar-chart
           #:histogram
           #:scatter))

(cl:in-package :vega-lite)

(defvar *vega-lite-string* "")
