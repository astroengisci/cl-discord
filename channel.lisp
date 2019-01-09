(defun auth-request (path key &key (method 'get) (type "application/json") content)
  (dex:request (build-url path)
               :method method
               :headers `(("Authorization" . ,(concatenate 'string "Bot " key))
                          ("Content-Type" . ,type))
               :content content))

(defun get-channel (channel key)
  (auth-request (concatenate 'string "channels/" channel)
                key))

(defun modify-channel (channel key 
                               &key name position topic (nsfw nil nsfw-p) rate-limit-per-user 
                               bitrate user-limit permission-overwrites parent-id)
  "Accepts any of the parameters accepted by the API call. Note that :nsfw must be set to 'false rather than nil, or else it will break."
  (let (content)
    (progn
      (if name 
          (setf content (acons :name name content))) 
      (if position 
          (setf content (acons :position position content)))
      (if topic 
          (setf content (acons :topic topic content)))
      (if nsfw-p 
          (setf content (acons :nsfw nsfw content)))
      (if rate-limit-per-user 
          (setf content (acons :rate_limit_per_user rate-limit-per-user content)))
      (if bitrate 
          (setf content (acons :bitrate bitrate content)))
      (if user-limit 
          (setf content (acons :user_limit user-limit content)))
      (if permission-overwrites 
          (setf content (acons :permission_overwrites permission-overwrites content)))
      (if parent-id 
          (setf content (acons :parent_id parent-id content)))
      (setf content (json:encode-json-to-string content))
      (auth-request (concatenate 'string "channels/" channel)
                key
                :method 'patch
                :content content
                ))))

(defun delete-channel (channel key)
  (auth-request (concatenate 'string "channels/" channel)
                key
                :method 'delete))

(defun get-channel-messages (channel key &key around before after limit)
  (let ((url (concatenate 'string "channels/" channel "/messages"))
        (query ()))
    (progn
      (if around (setf query (append query `(("around" . ,around)))))
      (if before (setf query (append query `(("before" . ,before)))))
      (if after (setf query (append query `(("after" . ,after)))))
      (if limit (setf query (append query `(("limit" . ,limit)))))
      (setf url (concatenate 'string url (build-query query)))
      (auth-request url key))))

(defun get-channel-message (channel key message)
  (auth-request (concatenate 'string "channels/" channel "/messages/" message)
                key))

(defun send-message (channel key content)
  (auth-request (concatenate 'string "channels/" channel "/messages")
                key
                :method 'post
                :content (json:encode-json-to-string `((:content . ,content)))))
