(in-package :vega-lite)

(defvar *plot-server*)

(defconstant +default-port+ 6731)
(alexandria:define-constant +default-address+ "localhost"
  :test #'string=)

(defclass plot-server-socket (hunchensocket:websocket-resource)
  ((task :initarg :task :initform (error ":TASK is unsupplied") :reader task)
   (task-handler :initarg :task-handler :initform nil :reader task-handler)
   (hunchensocket::write-lock :initarg :write-lock))
  (:default-initargs :client-class 'plot-client))

(defclass plot-client (hunchensocket:websocket-client) ())

(defmethod hunchensocket:client-connected ((pss plot-server-socket) client)
  (when (boundp '*vega-lite-string*)
    (loop :for client :in (hunchensocket:clients *vega-lite-string-socket*)
          :do (hunchensocket:send-text-message client *vega-lite-string*))))

(defmethod hunchensocket:text-message-received ((pss plot-server-socket) client message)
  (when (task-handler pss) (funcall (task-handler pss) message)))

(defvar *vega-lite-string-socket*
  (make-instance 'plot-server-socket
                 :task "/vega-lite-string/"))
(defvar *save-plot-socket*
  (make-instance 'plot-server-socket
                 :task "/save-plot/"
                 :task-handler 'save-plot-from-client))

(defvar *plot-server-sockets* (list *vega-lite-string-socket*
                                    *save-plot-socket*))

(defun dispatch-plot-task (request)
  (find (hunchentoot:script-name request) *plot-server-sockets*
        :test #'string= :key #'task))

(pushnew 'dispatch-plot-task hunchensocket:*websocket-dispatch-table*)

(defun ensure-plot-server
    (&key (port +default-port+)
       (address +default-address+)
       (verbose t))
  (when (boundp '*plot-server*)
    (let ((old-port (hunchentoot:acceptor-port *plot-server*))
          (old-address (hunchentoot:acceptor-address *plot-server*)))
      (when verbose
        (cl:format t "~%*plot-server* is already running at ~A:~A.~%" old-address port))
      (when (not (and (= port old-port)
                      (string= address old-address)))
        (when verbose (cl:format t "Stopping it... "))
        (hunchentoot:stop *plot-server*)
        (when verbose (cl:format t "Done~%"))
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
      (cl:format t "Started *plot-server*. It is available at~%  http://~a:~a"
                 address port))))
