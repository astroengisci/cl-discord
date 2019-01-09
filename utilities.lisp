(defun key-from-json (key jstring)
  (alexandria:assoc-value
   (json:decode-json-from-string jstring)
   key))

(defun build-url (path)
  (concatenate 'string "https://discordapp.com/api/" path))

(defun build-query (params)
  "Accepts a list of cons-pairs of keys and values"
  (let ((output "?")) 
    (loop for param in params
          do (setf output 
                   (concatenate 'string output (car param) "=" (cdr param) "&")))
      output))
