(in-package :vega-lite)

(defvar *plot-server*)

(defconstant +default-port+ 6731)
(alexandria:define-constant +default-address+ "localhost"
  :test #'string=)

(defun ensure-plot-server
    (&key (port +default-port+)
       (address +default-address+))
  (when (boundp '*plot-server*)
    (let ((old-port (hunchentoot:acceptor-port *plot-server*))
          (old-address (hunchentoot:acceptor-address *plot-server*)))
      (when (not (and (= port old-port)
                      (string= address old-address)))
        (format t "~%Stopping old *plot-server* at ~A:~A..." old-address port)
        (hunchentoot:stop *plot-server*)
        (format t "Done~%")
        (force-output))))
  (when (not (boundp '*plot-server*))
    (setf *plot-server*
          (make-instance 'hunchentoot:easy-acceptor
                         :document-root (namestring
                                         (asdf:component-pathname
                                          (asdf:find-system "vega-lite")))
                         :name "vega-lite"
                         :access-log-destination nil
                         :address address
                         :port port))
    (setf (hunchentoot:acceptor-access-log-destination *plot-server*) nil)
    (hunchentoot:start *plot-server*)
    (format t "Started *plot-server*. It is available at~%  http://~a:~a"
            address port)))

(hunchentoot:define-easy-handler (vega-lite-string-handler :uri "/vega-lite-string/") ()
  (setf (hunchentoot:content-type*) "text/json")
  *vega-lite-string*)

