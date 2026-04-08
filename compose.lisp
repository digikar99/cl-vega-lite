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
           (labels ((%compose (class)
                      (let* ((new-spec (make-instance class))
                             (slots (closer-mop:class-slots class)))
                        (loop :for slot :in slots
                              :for name := (closer-mop:slot-definition-name slot)
                              :do (when (slot-boundp spec2 name)
                                    (setf (slot-value new-spec name)
                                          (slot-value spec2 name))))
                        new-spec))
                    (%class-depth (class)
                      (let* ((slots (closer-mop:class-slots class))
                             (depth-slot
                               (loop :for slot :in slots
                                     :for sname := (closer-mop:slot-definition-name slot)
                                     :if (eq 'depth sname)
                                       :do (return slot))))
                        (closer-mop:slot-definition-initform depth-slot)))
                    (traverse-slots (class)
                      (cond ((eq class class2)
                             (%compose class))
                            ((>= (%class-depth class) depth2)
                             nil)
                            (t
                             (let ((slots (closer-mop:class-slots class)))
                               (loop :for slot :in slots
                                     :for sname := (closer-mop:slot-definition-name slot)
                                     :for sclass := (find-class sname nil)
                                     :do (when sclass
                                           (let ((subspec (traverse-slots sclass)))
                                             (when subspec
                                               (let ((new-spec (make-instance class)))
                                                 (setf (slot-value new-spec sname)
                                                       subspec)
                                                 (return new-spec)))))))))))
             (traverse-slots class1)))
          ((eq class2 top-class)
           (compose spec2 spec1))
          (t
           (compose (compose (make-instance 'plot)
                             spec1)
                    spec2)))))

(defun vega-compose (initial-spec &rest more-specs)
  "Each element of SPECS must be an instance of a subclass of SPEC"
  (reduce #'compose more-specs :initial-value initial-spec))
