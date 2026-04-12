(cl:defpackage :vega-lite
  (:use :cl)
  (:export #:*plot-server*
           #:+default-port+
           #:+default-address+
           #:ensure-plot-server

           #:vega-plot
           #:save-plot

           #:compose
           #:vega-compose


           #:data
           #:make-data
           #:datasets
           #:meke-datasets
           #:mark
           #:make-mark
           #:encoding
           #:make-encoding
           #:x
           #:make-x
           #:y
           #:make-y
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
