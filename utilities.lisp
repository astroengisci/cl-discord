(defun key-from-json (key jstring)
  (alexandria:assoc-value
   (json:decode-json-from-string jstring)
   key))

(defun build-url (path)
  (concatenate 'string "https://discordapp.com/api/" path))
