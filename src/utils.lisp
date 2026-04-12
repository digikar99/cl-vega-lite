(in-package :vega-lite)

(defun jsonify-symbol (symbol)
  (let ((string (string-downcase symbol)))
    (loop :for i :below (length string)
          :do (when (char= #\- (char string i))
                (setf (char string i) #\_)))
    string))

(defun snake-case-name (symbol)
  (let ((word-break nil))
    (with-output-to-string (*standard-output*)
      (loop :for ch :across (string-downcase symbol)
            :do (cond (word-break
                       (write-char (char-upcase ch))
                       (setf word-break nil))
                      ((char= #\- ch)
                       (setf word-break t))
                      (t
                       (write-char ch)))))))

(defun vega-dataset (name)
  (concatenate 'string "https://cdn.jsdelivr.net/npm/vega-datasets@latest/data/" name))
