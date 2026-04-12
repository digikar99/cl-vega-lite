(in-package :vega-lite)

(defvar *default-height* 360)

(defvar *default-width* 480)

(alexandria:define-constant +default-schema+
  "https://vega.github.io/schema/vega-lite/v6.json"
  :test #'string=)

(deftype spec-like (&optional (class 'cl:*))
  (if (eq 'cl:* class)
      `(or null list hash-table)
      `(or null string ,class)))

(defun vega-plot (specification)
  "Low level plot function.

Lisp names will be converted to snake case names."
  (ensure-plot-server :verbose nil)
  (with-shasht-config
    (setf *vega-lite-string* (shasht:write-json specification nil))
    (loop :for client :in (hunchensocket:clients *vega-lite-string-socket*)
          :do (hunchensocket:send-text-message client *vega-lite-string*))
    specification))

(defun save-plot (filename &optional type)
  (ensure-plot-server :verbose nil)
  (with-shasht-config
    (loop :for client :in (hunchensocket:clients *save-plot-socket*)
          :do (hunchensocket:send-text-message
               client
               (shasht:write-json (list :format (or (pathname-type filename)
                                                    type
                                                    "svg")
                                        :filename filename)
                                  nil)))))

(defun save-plot-from-client (json-string)
  (with-shasht-config
    (let* ((json (shasht:read-json json-string))
           (json (let ((plist nil))
                   (alexandria:doplist (k v json)
                     ;; FIXME: More general lisp-case-name
                     (push (intern (string-upcase k) :keyword) plist)
                     (push v plist))
                   (nreverse plist))))
      (destructuring-bind (&key filename format data &allow-other-keys)
          json
        (with-open-file (f filename :direction :output
                                    :if-does-not-exist :create
                                    :if-exists :supersede
                                    :element-type
                                    (cond ((string= format "svg")
                                           'character)
                                          ((string= format "png")
                                           '(unsigned-byte 8))
                                          (t
                                           (error "Expeced format to be one of svg or png, but it is ~A" format))))
          (when (string= format "png")
            (setf data (base64:base64-string-to-usb8-array data)))
          (write-sequence data f))))))

(define-spec plot 0 (name description title data transform params
                     resolve
                     align bounds center spacing
                     ($schema :initform +default-schema+)
                     background padding autosize config usermeta
                     mark encoding
                     (height :initform *default-height* :initarg :height)
                     (width :initform *default-width* :initarg :width)
                     view projection
                     style corner-radius cursor fill fill-opacity opacity
                     stroke stroke-cap stroke-dash stroke-dash-offset
                     stroke-join stroke-miter-limit stroke-opacity stroke-width)
    "Top-level specification (SPEC) for vega-lite. All other specifications
occur as some slot or sub-slot of the PLOT class.

https://vega.github.io/vega-lite/docs/spec.html")

;;; Data

(define-spec data 1 (values url name format parse)
    "https://vega.github.io/vega-lite/docs/data.html")

(define-spec datasets 1 (datasets)
    "https://vega.github.io/vega-lite/docs/data.html#datasets")

(alexandria:define-constant +data-types+
    (list :json :csv :tsv :dsv)
  :test #'equal)

;;; Marks

(alexandria:define-constant +primitive-mark-types+
    '(:area :bar :circle :line :point :rect :rule :square :text :tick :geoshape)
  :test #'equal
  :documentation "https://vega.github.io/vega-lite/docs/mark.html#types")

(alexandria:define-constant +complex-mark-types+
    '(:boxplot :errorband :errorbar)
  :test #'equal
  :documentation "https://vega.github.io/vega-lite/docs/mark.html#types")

(alexandria:define-constant +mark-types+
    (union +primitive-mark-types+ +complex-mark-types+)
  :test #'equal
  :documentation "https://vega.github.io/vega-lite/docs/mark.html#types")

(define-spec mark 1
    (type aria cursor description style tooltip clip invalid order
     x y x2 y2 x-offset y-offset x2-offset y2-offset width height
     color fill filled)
    "MARK-TYPE must be one of +MARK-TYPES+

https://vega.github.io/vega-lite/docs/mark.html#mark-def")

;;; Encoding

(alexandria:define-constant +channel-types+ '(:quantitative :nominal :temporal)
  :test #'equal)

(alexandria:define-constant +aggregation-operations+
  '(:count :valid :values :missing :distinct
    :sum :product :mean :average
    :variance :variancep
    :stdev :stdevp
    :stderr
    :median
    :q1 :q3
    :ci0 :ci1
    :min :max)
  ;; argmin and argmax must be specified as objects
  :test #'equal
  :documentation "https://vega.github.io/vega-lite/docs/aggregate.html")

(defun make-position-channel (name &rest args
                              &key field type axis aggregate bin sort stack time-unit
                                title
                              &allow-other-keys)
  "https://vega.github.io/vega-lite/docs/encoding.html#field-def"
  (declare (ignorable axis sort stack time-unit title))
  (assert (member name
                  '(:x :y :x2 :y2 :x-error :y-error :x-error2 :y-error2)
                  :test #'string-equal))
  (assert (or (stringp field)
              (symbolp field)))
  (when type
    (assert (member type +channel-types+ :test #'string-equal)))
  (when aggregate
    (assert (member aggregate +aggregation-operations+ :test #'string-equal)))
  (assert (or (typep bin 'boolean)
              (listp bin)
              (and (stringp bin) (string-equal bin "binned"))))
  (list name args))

(define-spec axis 3
    (aria band-position description
     max-extent min-extent
     orient offset position style
     translate zindex

     ticks tick-band tick-cap tick-color tick-dash tick-extra tick-min-step tick-offset
     tick-opacity tick-round tick-size tick-width values

     format format-type
     labels label-align label-angle label-baseline label-bound label-color label-expr label-flush label-flush-offset
     label-font label-font-size label-font-style label-font-weight label-limit
     label-line-height label-offset label-opacity label-overlap label-padding label-separation)
    "https://vega.github.io/vega-lite/docs/axis.html")

(alexandria:define-constant +continuous-scale-types+
  '(:linear :pow :sqrt :symlog :log :time :utc)
  :test #'equal)

(alexandria:define-constant +discrete-scale-types+
  '(:ordinal :band :point)
  :test #'equal)

(alexandria:define-constant +discretizing-scale-types+
  '("bin-ordinal" :quantile :quantize :threshold)
  :test #'equalp)

(alexandria:define-constant +scale-types+
    (append +continuous-scale-types+ +discrete-scale-types+ +discretizing-scale-types+)
  :test #'equalp)

(define-spec scale 3 (type
                      domain
                      domain-max domain-min domain-mid domain-raw)
    "https://vega.github.io/vega-lite/docs/scale.html")

(defun make-scale* (&rest args
                    &key type
                      domain
                      domain-max domain-min domain-mid domain-raw
                    &allow-other-keys)
  "https://vega.github.io/vega-lite/docs/scale.html"
  (declare (ignorable domain domain-max domain-min domain-mid domain-raw))
  (when type (assert (member type +scale-types+ :test #'string-equal)))
  args)

(define-spec bin 3
    ((binned :initform t :type t) anchor base divide extent maxbins minstep nice step steps)
  "https://vega.github.io/vega-lite/docs/bin.html#bin-parameters")
