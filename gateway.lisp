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

(defun default-cb (message) (print message))

(defun build-payload (op data)
  (json:encode-json-to-string
   `((:op . ,op)
     (:d . ,data))))

(defun heartbeat (interval)
  "Sends a heartbeat packet every _interval_ milliseconds."
  (progn
    (sleep (/ interval 1000))
    (wsd:send +client+
              (build-payload 1 *last-s*))
    (print "Sent heartbeat packet")
    (heartbeat interval)))

(defun identify (token)
  (wsd:send +client+
            (build-payload 2
                           `((:token . ,token)
                             (:properties . ((:$os . "linux")
                                             (:$browser . "cl-discord")
                                             (:$device . "cl-discord")))))))

(defun generate-callbacks (&key
                                (channel-create #'default-cb)
                                (channel-update #'default-cb)
                                (channel-delete #'default-cb)
                                (channel-pins-update #'default-cb)

                                (guild-create #'default-cb)
                                (guild-update #'default-cb)
                                (guild-delete #'default-cb)
                                (guild-ban-add #'default-cb)
                                (guild-ban-remove #'default-cb)
                                (guild-emojis-update #'default-cb)

                                (guild-member-add #'default-cb)
                                (guild-member-remove #'default-cb)
                                (guild-member-update #'default-cb)

                                (guild-role-create #'default-cb)
                                (guild-role-update #'default-cb)
                                (guild-role-delete #'default-cb)

                                (message-create #'default-cb)
                                (message-update #'default-cb)
                                (message-delete #'default-cb)
                                (message-delete-bulk #'default-cb)

                                (message-reaction-add #'default-cb)
                                (message-reaction-remove #'default-cb)
                                (message-reaction-remove-all #'default-cb)

                                (presence-update #'default-cb)
                                (typing-start #'default-cb)
                                (user-update #'default-cb)

                                (voice-state-update #'default-cb)
                                (voice-server-update #'default-cb)
                                (webhooks-update #'default-cb))

  (progn
    (setf (fdefinition 'channel-create-cb) channel-create)
    (setf (fdefinition 'channel-update-cb) channel-update)
    (setf (fdefinition 'channel-delete-cb) channel-delete)
    (setf (fdefinition 'channel-pins-update-cb) channel-pins-update)

    (setf (fdefinition 'guild-create-cb) guild-create)
    (setf (fdefinition 'guild-update-cb) guild-update)
    (setf (fdefinition 'guild-delete-cb) guild-delete)

    (setf (fdefinition 'guild-ban-add-cb) guild-ban-add)
    (setf (fdefinition 'guild-ban-remove-cb) guild-ban-add)

    (setf (fdefinition 'message-create-cb) message-create)
    (setf (fdefinition 'message-update-cb) message-update)
    (setf (fdefinition 'message-delete-cb) message-delete)
    (setf (fdefinition 'message-delete-bulk-cb) message-delete-bulk)

    (setf (fdefinition 'message-reaction-add-cb) message-reaction-add)
    (setf (fdefinition 'message-reaction-remove-cb) message-reaction-remove)
    (setf (fdefinition 'message-reaction-remove-all-cb) message-reaction-remove-all)

    (setf (fdefinition 'presence-update-cb) presence-update)
    (setf (fdefinition 'typing-start-cb) typing-start)
    (setf (fdefinition 'user-update-cb) user-update)

    (setf (fdefinition 'voice-state-update-cb) voice-state-update)
    (setf (fdefinition 'voice-server-update-cb) voice-server-update)
    (setf (fdefinition 'webhooks-update-cb) webhooks-update)))

(defun handle-messages (message)
  (let ((op (key-from-json :op message))
        (s (key-from-json :s message))
        (ty (key-from-json :t message)))
    (progn
      (if (/= op 11) (setf *last-s* s))
      (case op
        (0 (cond
             ((equal ty "CHANNEL_CREATE") (channel-create-cb message))
             ((equal ty "CHANNEL_UPDATE") (channel-update-cb message))
             ((equal ty "CHANNEL_DELETE") (channel-delete-cb message))
             ((equal ty "CHANNEL_PINS_UPDATE") (channel-pins-update-cb message))

             ((equal ty "GUILD_CREATE") (guild-create-cb message))
             ((equal ty "GUILD_UPDATE") (guild-update-cb message))
             ((equal ty "GUILD_DELETE") (guild-delete-cb message))

             ((equal ty "GUILD_BAN_ADD") (guild-ban-add-cb message))
             ((equal ty "GUILD_BAN_REMOVE") (guild-ban-remove-cb message))

             ((equal ty "MESSAGE_CREATE") (message-create-cb message))
             ((equal ty "MESSAGE_UPDATE") (message-update-cb message))
             ((equal ty "MESSAGE_DELETE") (message-delete-cb message))
             ((equal ty "MESSAGE_DELETE_BULK") (message-delete-bulk-cb message))

             ((equal ty "MESSAGE_REACTION_ADD") (message-reaction-add-cb message))
             ((equal ty "MESSAGE_REACTION_REMOVE") (message-reaction-remove-cb message))
             ((equal ty "MESSAGE_REACTION_REMOVE_ALL") (message-reaction-remove-all-cb message))

             ((equal ty "PRESENCE_UPDATE") (presence-update-cb message))
             ((equal ty "TYPING_START") (typing-start-cb message))
             ((equal ty "USER_UPDATE") (user-update-cb message))

             ((equal ty "VOICE_STATE_UPDATE") (voice-state-update-cb message))
             ((equal ty "VOICE_SERVER_UPDATE") (voice-server-update-cb message))
             ((equal ty "WEBHOOKS_UPDATE") (webhooks-update-cb message))))
        (1 (wsd:send +client+
                     (build-payload 1 *last-s*)))
        (10 (progn
              (print "got hello packet")
              (bt:make-thread
               (lambda ()
                 (heartbeat (alexandria:assoc-value
                             (key-from-json :d message)
                             :heartbeat--interval))))))
        (11 (print "got heartbeat ACK"))
        (otherwise (print message))))))

(defun connect-gateway (token)
  "Opens a gateway connection."
  (progn
                                        ; Open connection
    (wsd:on :message +client+
            #'handle-messages)
    (wsd:on :close +client+
            (lambda (&key code reason)
              (format t "Closed because '~A' (Code=~A)~%" reason code)))
    (wsd:on :error +client+
        (lambda (error)
          (format t "Got an error: ~S~%" error)))
    (wsd:start-connection +client+)
    ; Identify
    (identify token)))
