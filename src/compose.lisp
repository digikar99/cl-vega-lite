(in-package :vega-lite)

(defun compose (spec1 spec2)
  "Return a new spec that has spec2 properties added to spec1 or overriding spec1"
  (declare (type spec spec1 spec2)
           (optimize debug))
  (let ((class1 (class-of spec1))
        (class2 (class-of spec2))
        (depth2 (slot-value spec2 'depth))
        (top-class (find-class 'plot)))
    (cond ((eq class1 class2)
           (let* ((class (class-of spec2))
                  (new-spec (make-instance class))
                  (slots (closer-mop:class-slots class)))
             (loop :for slot :in slots
                   :for name := (closer-mop:slot-definition-name slot)
                   :do (cond ((slot-boundp spec2 name)
                              (setf (slot-value new-spec name) (slot-value spec2 name)))
                             ((slot-boundp spec1 name)
                              (setf (slot-value new-spec name) (slot-value spec1 name)))))
             new-spec))
          ((eq class1 top-class)
           (labels ((%compose (spec)
                      (let* ((class (class-of spec))
                             (slots (closer-mop:class-slots class)))
                        (loop :for slot :in slots
                              :for name := (closer-mop:slot-definition-name slot)
                              :do (when (slot-boundp spec2 name)
                                    (setf (slot-value spec name)
                                          (slot-value spec2 name))))
                        spec))
                    (traverse-slots (spec1)
                      (let ((class1 (class-of spec1)))
                        (cond ((not (typep spec1 'spec))
                               spec1)
                              ((eq class1 class2)
                               (%compose spec1))
                              ((>= (slot-value spec1 'depth) depth2)
                               nil)
                              (t
                               (loop :for slot :in (closer-mop:class-slots class1)
                                     :for slot-name := (closer-mop:slot-definition-name slot)
                                     :for slot-class := (find-class slot-name nil)
                                     :do (let* ((subspec
                                                  (cond ((slot-boundp spec1 slot-name)
                                                         (slot-value spec1 slot-name))
                                                        (slot-class
                                                         (make-instance slot-class))))
                                                (traversed-subspec
                                                  (when subspec
                                                    (traverse-slots subspec))))
                                           (when traversed-subspec
                                             (setf (slot-value spec1 slot-name)
                                                   traversed-subspec))))
                               spec1)))))
             (traverse-slots spec1)))
          ((eq class2 top-class)
           (compose spec2 spec1))
          (t
           (compose (compose (make-instance 'plot)
                             spec1)
                    spec2)))))

(defun vega-compose (initial-spec &rest more-specs)
  "Each element of SPECS must be an instance of a subclass of SPEC"
  (reduce #'compose more-specs :initial-value initial-spec))
