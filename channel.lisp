(ql:quickload "cl-json")
(ql:quickload "dexador")

(defun send-message (content channel key)
  (dex:post
   (concatenate 'string "https://discordapp.com/api/channels/" channel "/messages") 
   :headers `(("Authorization" . ,(concatenate 'string "Bot " key))
              ("Content-Type" . "application/json"))
   :content (json:encode-json-to-string `((:content . ,content)))))
