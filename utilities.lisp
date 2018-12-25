(defun key-from-json (key jstring)
  (alexandria:assoc-value
   (json:decode-json-from-string jstring)
   key))
