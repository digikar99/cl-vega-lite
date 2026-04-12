(in-package :vega-lite)

(defclass spec-class (standard-class)
  ((depth :allocation :class :initform -1)))

(defclass spec ()
  ((depth :allocation :class :initform -1))
  (:documentation "The base class for vega-lite specification objects"))

(closer-mop:ensure-finalized (find-class 'spec))

(defmacro define-spec (spec-name depth-or-superclasses
                       (&rest direct-slots) &body (&optional (doc "")))
  (let* ((args (gensym "ARGS"))
         (depth (when (integerp depth-or-superclasses)
                  depth-or-superclasses))
         (superclasses (if (integerp depth-or-superclasses)
                           `(spec)
                           depth-or-superclasses))
         (direct-slots
           (loop :for slot :in direct-slots
                 :if (listp slot)
                   :collect slot
                 :else
                   :collect (list slot
                                  :initarg (intern (string slot) :keyword))))
         (fn-params
           (mapcar (lambda (slot)
                     (typecase slot
                       (symbol
                        slot)
                       (cons
                        (let ((initform (getf (rest slot) :initform)))
                          (if initform
                              (list (first slot) initform)
                              (first slot))))
                       (closer-mop:slot-definition
                        (let ((initform (closer-mop:slot-definition-initform slot))
                              (name (closer-mop:slot-definition-name slot)))
                          (if initform
                              (list name initform)
                              name)))))
                   (nconc
                    (alexandria:mappend #'closer-mop:class-slots
                                        (mapcar #'find-class superclasses))
                    direct-slots)))
         (fn-params (remove 'depth
                            (remove-duplicates fn-params
                                               :key #'car-or-self)
                            :key #'car-or-self)))
    (loop :for (name . spec) :in direct-slots
          :do (setf (getf spec :type)
                    (or (getf spec :type)
                        (if (find-class name nil)
                            `(spec-like ,name)
                            'spec-like))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,spec-name ,(or superclasses `(spec))
         (,@(when depth
              `((depth :initform ,depth)))
          ,@direct-slots)
         (:documentation ,doc))
       (closer-mop:ensure-finalized (find-class ',spec-name))
       (defun ,(alexandria:symbolicate 'make '- spec-name) (&rest ,args &key ,@fn-params)
         ,doc
         (declare (ignorable ,@(mapcar (lambda (p)
                                         (if (symbolp p)
                                             p
                                             (first p)))
                                       fn-params)))
         (apply #'make-instance ',spec-name ,args)))))
