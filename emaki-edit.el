(require 'url)
(require 'json)

(defun get-post-content () 
  (let ((json-object-type 'hash-table))
    (goto-char (point-max))
    (setq cnt (thing-at-point `line))
    (json-read-from-string cnt)
    )
  )
  
(defun add-rst-title (title)
  (progn
    (dotimes (_ (length title)) 
      (insert "="))
    (insert "\n")
    )
  )

(defun draw-post-title (title)
  (progn
    (insert (format "%s\n" title))
    (add-rst-title title)
    )
  )

(defun draw-post-simple-section (name content)
  (progn
    (insert (format ".. %s\n%s\n" name content))
    )
  )

(defun draw-post-abstract (abstract)
  (draw-post-simple-section "Abstract" abstract)
  )

(defun draw-post-category (category)
  (draw-post-simple-section "Category" category)
)

(defun draw-post-content (content)
  (draw-post-simple-section "Content" content)
  )

(defun draw-post-tags (tags)
  (progn
    (insert ".. Tags\n")
    (dotimes (pos (length tags))
      (let* ((tag (aref tags pos)))
	(insert (format " - %s\n" tag))
	)
      )
    )
  )

(defun draw-post (postc)
  (let* ((title (gethash "title" postc))
	 (abstract (gethash "abstract" postc))
	 (content (gethash "content" postc))
	 (tags (gethash "tags" postc))
	 (category (gethash "category" postc))
	 (pformat (gethash "format" postc))
	 )
    (draw-post-title title)
    (draw-post-abstract abstract)
    (draw-post-category category)
    (draw-post-tags tags)
    (draw-post-content content)
    (cond ((equal pformat "rst") (rst-mode))
	  ((equal pformat "textile") (textile-mode))
      )
    )
  )

(defun show-post (status)
  (if (null status)
      (let* ((postc (get-post-content)))
	(setq buffname
	      (format "blog_post_%s.%s" 
		      (gethash "slug" postc)
		      (gethash "format" postc)))
	(generate-new-buffer  buffname)
	(switch-to-buffer buffname)
	(draw-post postc)
	)
    (message "Unable to fetch post %s " (cdr status)))
  )



(defun maki-get-post (identifier)
  "Fetch the JSON post with the id or slug (identifier) from the maki blog"
  (interactive "sPost identifier: ")
  (let ((post-url "http://127.0.0.1:8080/post")
	(url-request-extra-headers 
	 '(("Content-Type" . "application/json")
	   ("Accept" . "application/json"))
	 )
	)
    (url-retrieve 
     (concat post-url "/"  identifier )
     'show-post))
  )



