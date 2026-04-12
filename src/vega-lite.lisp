(in-package :vega-lite)

(alexandria:define-constant +default-schema+
  "https://vega.github.io/schema/vega-lite/v6.json"
  :test #'string=)

(defun vega-plot (specification)
  "Low level plot function.

Lisp names will be converted to snake case names."
  (ensure-plot-server :verbose nil)
  (let ((shasht:*write-plist-as-object* t)
        (shasht:*write-alist-as-object* t)
        (shasht:*symbol-name-function* #'snake-case-name)
        (shasht:*write-null-values* '(:null nil))
        (shasht:*write-false-values* '(:false)))
    (setf *vega-lite-string* (shasht:write-json specification nil))
    (loop :for client :in (hunchensocket:clients *vega-lite-string-socket*)
          :do (hunchensocket:send-text-message client *vega-lite-string*))
    specification))

(defclass spec-class (standard-class)
  ((depth :allocation :class :initform -1)))

(defclass spec ()
  ((depth :allocation :class :initform -1))
  (:documentation "The base class for vega-lite specification objects"))

(defmacro define-spec (spec-name depth (&rest parameters) &optional (doc ""))
  (let* ((args (gensym "ARGS"))
         (parameters (loop :for p :in parameters
                           :if (listp p)
                             :collect p
                           :else
                             :collect (list p :initarg (intern (string p) :keyword))))
         (fn-params (mapcar (lambda (p)
                              (if (symbolp p)
                                  p
                                  (list (first p) (getf (rest p) :initform))))
                            parameters)))
    (loop :for p :in parameters
          :do (setf (getf (rest p) :type)
                    (or (getf (rest p) :type)
                        `(or null spec string))))
    `(progn
       (defclass ,spec-name (spec)
         ((depth :initform ,depth)
          ,@parameters)
         (:documentation ,doc))
       (closer-mop:ensure-finalized (find-class ',spec-name))
       (defun ,(alexandria:symbolicate 'make '- spec-name) (&rest ,args &key ,@fn-params)
         ,doc
         (declare (ignorable ,@(mapcar (lambda (p)
                                         (if (symbolp p)
                                             p
                                             (first p)))
                                       parameters)))
         (apply #'make-instance ',spec-name ,args)))))

(define-spec plot 0 (name description title data transform params
                     resolve
                     align bounds center spacing
                     ($schema :initform +default-schema+)
                     background padding autosize config usermeta
                     mark encoding height width view projection
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

(define-spec encoding 1
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

(define-spec x 2 (field scale type axis aggregate bin sort stack time-unit title)
    "https://vega.github.io/vega-lite/docs/encoding.html#field-def")

(define-spec y 2 (field scale type axis aggregate bin sort stack time-unit title)
    "https://vega.github.io/vega-lite/docs/encoding.html#field-def")

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
