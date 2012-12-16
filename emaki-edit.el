(require 'url)
(require 'json)


(defun get-post-content () 
  (let ((json-object-type 'hash-table))
    (goto-char (point-max))
    (setq cnt (thing-at-point `line))
    (json-read-from-string cnt)
    )
  )
  
(defun show-post (status)
  (progn
    (setq postc (get-post-content))
    (message "Title: %sContent: %s" 
	     (gethash "title" postc)
	     (gethash "content" postc))
    ) 
  )

(defun blog-get-post (id)
  "Fetch the JSON post with the specific id from 
   the blog"
  (let ((post-url "http://127.0.0.1:8080/post"))
    (url-retrieve 
     (concat post-url "/" 
	     (number-to-string id)
	     ".json")
     'show-post))
  )

(blog-get-post 10)


