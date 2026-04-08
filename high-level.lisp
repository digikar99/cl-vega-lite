(in-package :vega-lite)

(defgeneric 1d-data-as-list (data))

(defmethod 1d-data-as-list ((data list))
  data)

(defmethod 1d-data-as-list ((data vector))
  (coerce data 'list))

(defvar *default-height* 360)

(defvar *default-width* 480)

(defun bar-chart (data
                  &key (labels (alexandria:iota (length data)))
                    min max
                    (height *default-height*)
                    (width *default-width*)
                    (xlabel "x")
                    (ylabel "y")
                    aggregate
                    label-sort
                    (tooltip t))
  "
  DATA: 1 dimensional data
  LABELS: A list-like object of length equal to data. Each element should be a
    string labeling the corresponding data element
  MIN: Minimum for y-axis
  MAX: Maximum for y-axis
  LABEL-SORT: See https://vega.github.io/vega-lite/docs/sort.html
   To sort labels in ascending order of data, use \"y\". For descending, use \"-y\"."
  (assert (stringp xlabel))
  (assert (stringp ylabel))
  (vega-plot
   (make-plot :data (make-data :values (map 'vector
                                            (lambda (point label)
                                              (list (cons ylabel point)
                                                    (cons xlabel label)))
                                            (1d-data-as-list data)
                                            (1d-data-as-list labels)))
              :mark (make-mark :type :bar :clip t :tooltip tooltip)
              :height height
              :width width
              :encoding (make-encoding
                         :x
                         (make-x
                          :field xlabel
                          :type "nominal"
                          :aggregate aggregate
                          :axis (make-axis :label-angle 0)
                          :sort label-sort)
                         :y
                         (make-y
                          :field ylabel :type "quantitative"
                          :scale (make-scale :domain-min min
                                             :domain-max max))))))

;; TODO: Grouped bar charts
;; TODO: Stacked bar charts
;; See https://vega.github.io/vega-lite/examples/#bar-charts

(defun histogram (data
                  &key
                    min max
                    (height *default-height*)
                    (width *default-width*)
                    (xlabel "x")
                    (ylabel "count")
                    step
                    (tooltip t))
  "
  DATA: 1 dimensional data
  MIN: Minimum for y-axis
  MAX: Maximum for y-axis
  STEP: Step size to use for bin creation
"
  (assert (stringp xlabel))
  (assert (stringp ylabel))
  (vega-plot
   (make-plot :data (make-data :values (map 'vector
                                            (lambda (point)
                                              (list (cons xlabel point)))
                                            (1d-data-as-list data)))
              :mark (make-mark :type :bar :clip t :tooltip tooltip)
              :height height
              :width width
              :encoding (make-encoding
                         :x
                         (make-x
                          :bin (if step
                                   (make-bin :binned t :step step)
                                   t)
                          :field xlabel
                          :title xlabel
                          :axis (make-axis :label-angle 0))
                         :y
                         (make-y
                          :aggregate "count"
                          :title ylabel
                          :scale (make-scale :domain-min min
                                             :domain-max max))))))

(defun scatter (x y &key xmin xmax
                      ymin ymax
                      (height *default-height*)
                      (width *default-width*)
                      (xlabel "x")
                      (ylabel "y")
                      xticks yticks
                      (tooltip t)
                      color)
  "
  DATA: 1 dimensional data
  MIN: Minimum for y-axis
  MAX: Maximum for y-axis
  STEP: Step size to use for bin creation
"
  (assert (stringp xlabel))
  (assert (stringp ylabel))
  (vega-plot
   (make-plot :data (make-data :values (map 'vector
                                            (lambda (x y)
                                              (list (cons xlabel x)
                                                    (cons ylabel y)))
                                            (1d-data-as-list x)
                                            (1d-data-as-list y)))
              :mark (make-mark :type :point :clip t
                               :tooltip tooltip
                               :color color)
              :height height
              :width width
              :encoding (make-encoding
                         :x (make-x
                             :field xlabel
                             :type "quantitative"
                             :title xlabel
                             :axis (make-axis :label-angle 0 :values xticks)
                             :scale (make-scale :domain-min xmin
                                                :domain-max xmax))
                         :y (make-y
                             :field ylabel
                             :type "quantitative"
                             :title ylabel
                             :axis (make-axis :label-angle 0 :values yticks)
                             :scale (make-scale :domain-min ymin
                                                :domain-max ymax))))))
