(in-package :vega-lite)

(defvar *plot-server*)

(defconstant +default-port+ 6731)
(alexandria:define-constant +default-address+ "localhost"
  :test #'string=)

(defclass plot-server-socket (hunchensocket:websocket-resource)
  ((task :initarg :task :initform (error ":TASK is unsupplied") :reader task)
   (hunchensocket::write-lock :initarg :write-lock))
  (:default-initargs :client-class 'plot-client))
(defclass plot-client (hunchensocket:websocket-client) ())

(defmethod hunchensocket:client-connected ((pss plot-server-socket) client)
  (format t "~A connected to ~A~%" client pss))

(defvar *vega-lite-string-socket* (make-instance 'plot-server-socket :task "/vega-lite-string/"))

(defvar *plot-server-sockets* (list *vega-lite-string-socket*))

(defun dispatch-plot-task (request)
  (find (hunchentoot:script-name request) *plot-server-sockets* :test #'string= :key #'task))

(pushnew 'dispatch-plot-task hunchensocket:*websocket-dispatch-table*)

(defun ensure-plot-server
    (&key (port +default-port+)
       (address +default-address+)
       (verbose t))
  (when (boundp '*plot-server*)
    (let ((old-port (hunchentoot:acceptor-port *plot-server*))
          (old-address (hunchentoot:acceptor-address *plot-server*)))
      (when verbose
        (format t "~%*plot-server* is already running at ~A:~A.~%" old-address port))
      (when (not (and (= port old-port)
                      (string= address old-address)))
        (when verbose (format t "Stopping it... "))
        (hunchentoot:stop *plot-server*)
        (when verbose (format t "Done~%"))
        (force-output))))
  (when (not (boundp '*plot-server*))
    (setf *plot-server*
          (make-instance 'hunchensocket:websocket-acceptor
                         :document-root (namestring
                                         (asdf:component-pathname
                                          (asdf:find-system "vega-lite-wip")))
                         :name "vega-lite"
                         :access-log-destination nil
                         :address address
                         :port port))
    (setf (hunchentoot:acceptor-access-log-destination *plot-server*) nil)
    (hunchentoot:start *plot-server*)
    (when verbose
      (format t "Started *plot-server*. It is available at~%  http://~a:~a"
              address port))))
