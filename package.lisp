(cl:defpackage :vega-lite
  (:use :cl)
  (:export #:*plot-server*
           #:+default-port+
           #:+default-address+
           #:ensure-plot-server

           #:vega-plot

           #:make-data
           #:make-data*
           #:meke-datasets
           #:make-mark
           #:make-mark*
           #:make-encoding
           #:make-encoding*
           #:make-positional-channel
           #:make-axis*
           #:make-scale*
           #:make-bin-params
           #:make-bin-params*

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
