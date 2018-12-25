(ql:quickload "cl-json")
(ql:quickload "websocket-driver-client")
(ql:quickload "bordeaux-threads")
(ql:quickload "alexandria")
(ql:quickload "dexador")
(load "./utilities.lisp")

(defvar *last-s* nil)

(defconstant +gateway+
  (key-from-json :url
                 (dex:get "http://discordapp.com/api/gateway"))
  "On loading the library, gets the current gateway URL.")

(defconstant +client+
  (wsd:make-client +gateway+))

(defun heartbeat (interval)
  "Sends a heartbeat packet every _interval_ milliseconds."
  (progn
    (sleep (/ interval 1000))
    (wsd:send +client+
              (json:encode-json-to-string
               '((:op . 1)
                 (:d . *last-s*))))
    (print "Sent heartbeat packet")
    (heartbeat interval)))

(defun handle-messages (message)
  (let ((op (key-from-json :op message))
        (s (key-from-json :s message)))
    (progn
      (print "message received")
      (setf *last-s* s)
      (case op
       (10 (progn
             (bt:make-thread
              (heartbeat (alexandria:assoc-value
                          (key-from-json :d message)
                          :heartbeat--interval)))))
       (11 (print "got heartbeat ACK"))
       (otherwise (print message))))))

(defun connect-gateway ()
  "Opens a gateway connection."
  (progn
    (wsd:on :message +client+
            #'handle-messages)
    (wsd:on :close +client+
            (lambda (&key code reason)
              (format t "Closed because '~A' (Code=~A)~%" reason code)))
    (wsd:on :error +client+
        (lambda (error)
          (format t "Got an error: ~S~%" error)))
    (wsd:start-connection +client+)))
