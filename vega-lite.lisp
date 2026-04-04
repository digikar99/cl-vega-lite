(in-package :vega-lite)

(alexandria:define-constant +default-schema+
  "https://vega.github.io/schema/vega-lite/v6.json"
  :test #'string=)

(defun vega-plot (specification)
  "Low level plot function.

Lisp names will be converted to snake case names."
  (ensure-plot-server)
  (let ((shasht:*write-plist-as-object* t)
        (shasht:*write-alist-as-object* t)
        (shasht:*symbol-name-function* #'snake-case-name)
        (shasht:*write-null-values* '(:null nil))
        (shasht:*write-false-values* '(:false)))
    (setf *vega-lite-string*
          (shasht:write-json (list* :$schema +default-schema+
                                    specification)
                             nil))
    specification))

(defun vega-compose (&rest specs)
  "Each element of SPECS must be a plist"
  (apply #'nconc specs))

(defmacro define-spec-function (function-name spec-name (&rest parameters) &optional doc)
  (let ((args (gensym "ARGS")))
    `(defun ,function-name (&rest ,args &key ,@parameters &allow-other-keys)
       ,(or doc "")
       (declare (ignorable ,@(mapcar (lambda (p)
                                       (if (symbolp p)
                                           p
                                           (first p)))
                                     parameters)))
       ,(if spec-name
            `(list ,spec-name ,args)
            args))))

(define-spec-function make-plot nil
    (data mark encoding height width))

;;; Data

(define-spec-function make-data :data
    (values url name format parse)
    "https://vega.github.io/vega-lite/docs/data.html")

(define-spec-function make-data* nil
    (values url name format parse)
    "https://vega.github.io/vega-lite/docs/data.html")

(defun make-datasets (&rest datasets &key &allow-other-keys)
  "https://vega.github.io/vega-lite/docs/data.html#datasets"
  (list :datasets datasets))

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

(define-spec-function make-mark :mark
    (type aria cursor description style tooltip clip invalid order
     x y x2 y2 x-offset y-offset x2-offset y2-offset width height
     color fill filled)
    "MARK-TYPE must be one of +MARK-TYPES+

https://vega.github.io/vega-lite/docs/mark.html#mark-def")

(define-spec-function make-mark* nil
    (type aria cursor description style tooltip clip invalid order
     x y x2 y2 x-offset y-offset x2-offset y2-offset width height
     color fill filled)
    "MARK-TYPE must be one of +MARK-TYPES+

https://vega.github.io/vega-lite/docs/mark.html#mark-def")

;;; Encoding

(define-spec-function make-encoding :encoding
    (x y x-error y-error
     x2 y2 x-error2 y-error2

     color opacity size angle shape

     text tooltip
     href description
     detail key order

     facet row column)
    "For a full list of encodings, see https://vega.github.io/vega-lite/docs/encoding.html")

(define-spec-function make-encoding* nil
    (x y x-error y-error
     x2 y2 x-error2 y-error2

     color opacity size angle shape

     text tooltip
     href description
     detail key order

     facet row column)
    "For a full list of encodings, see https://vega.github.io/vega-lite/docs/encoding.html")

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

(define-spec-function make-axis* nil
    (aria band-position description
     max-extent min-extent
     orient position style
     translate zindex)
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

(defun make-scale* (&rest args
                    &key type
                      domain
                      domain-max domain-min domain-mid domain-raw
                    &allow-other-keys)
  "https://vega.github.io/vega-lite/docs/scale.html"
  (declare (ignorable domain domain-max domain-min domain-mid domain-raw))
  (when type (assert (member type +scale-types+ :test #'string-equal)))
  args)

(define-spec-function make-bin-params :bin
    ((binned t) anchor base divide extent maxbins minstep nice step steps)
  "https://vega.github.io/vega-lite/docs/bin.html#bin-parameters")

(define-spec-function make-bin-params* nil
    ((binned t) anchor base divide extent maxbins minstep nice step steps)
  "https://vega.github.io/vega-lite/docs/bin.html#bin-parameters")
