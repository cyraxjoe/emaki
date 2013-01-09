;; Maki mode
;;
(require 'url)
(require 'json)

(defconst maki-host "http://127.0.0.1:8080" )
(defconst maki-post-uri "/post")

(defvar maki-mode-map nil
  "Local key map to the maki mode.")

(defvar maki-post-mode-map nil
  "Local keymap to the maki-post minor mode."
  )

(setq maki-mode-map (make-sparse-keymap))
(define-key maki-mode-map "\C-xo" 'maki-get-post)

(setq maki-post-mode-map (make-sparse-keymap))




(defun maki-mode ()
  "Major mode to edit the maki blog."
  (interactive)
  (progn 
    (pop-to-buffer "*Maki*")
    (kill-all-local-variables)
    (make-local-variable 'maki-curr-post)
    (setq maki-curr-post nil)
    (setq major-mode 'maki-mode)
    (setq mode-name "Maki")
    (use-local-map maki-mode-map)
    )
  )

(define-minor-mode maki-post-mode
  "Set the minor mode for the post edition.
     With no argument, this command toggles the mode.
     Non-null prefix argument turns on the mode.
     Null prefix argument turns off the mode."
  :init-value nil
  :lighter " maki-post"
  :keymap 'maki-post-mode-map
  )



(defun maki-get-post (identifier)
  "Fetch the JSON post with the id or slug (identifier) from the maki blog"
  (interactive "sPost identifier: ")
  (let ((post-url (maki-post-url))
	(url-request-extra-headers 
	 '(("Content-Type" . "application/json")
	   ("Accept" . "application/json"))
	 )
	)
    (url-retrieve  (concat post-url "/"  identifier ) 'maki-show-post))
  )


(defun maki-save-post () 
  "Save the changes to the blog post. Do not ask for confirmation."
  (interactive)
  (let* ((post (makehash)))
    (save-excursion
      (puthash "title" (maki-get-current-title) post)
      (puthash "abstract" (maki-get-current-abstract) post)
      (puthash "tags" (maki-get-current-tags) post)
      (puthash "content" (maki-get-current-content) post)
      (puthash "id" maki-post-id post)
      (message (json-encode post))
      )
    (set-buffer-modified-p nil)
    )
  )

(defun maki-ask-save-post () 
  "Save the changes to the blog post, but confirm first."
  (interactive)
  (if (buffer-modified-p) 
      (if (equal "y" (read-string "Do you really wanna save the changes? (y/n)"))
	  (maki-save-post)
	nil)
      (message "The blog post has not been modified.")
      )
  )


(defun maki-show-post (status)
  (if (null status)
      (let* ((postc (maki-get-post-content)))
	(setq buffname
	      (format "blogpost (%s) [%s]"
		      (gethash "slug" postc)
		      (gethash "id" postc)))
	(if  (get-buffer  buffname)
	    (if (equal (read-string "The post is already loaded. Reload? (y/n)" )
		       "y")
		(kill-buffer buffname)
	      )
	  (generate-new-buffer buffname))
	(setq maki-curr-post (gethash "id" postc)) ;; before switching buffer
	(switch-to-buffer buffname)
	(maki-draw-post postc)
	(set-visited-file-name buffname)
	(set-buffer-modified-p nil) ;; to remove the **
	(progn  ;; maki-post-mode minor mode changes
	  (maki-post-mode)
	  (maki-set-post-mode)
	  (setq maki-post-id (gethash "id" postc)) ;; this is in the minor mode
	  )
	)
    (message "Unable to fetch post %s " (cdr status)))
  )


(defun maki-post-url ()
  (concat maki-host maki-post-uri)
  )


(defun maki-get-post-content () 
  (let ((json-object-type 'hash-table))
    (goto-char (point-max))
    (setq cnt (thing-at-point `line))
    (json-read-from-string cnt)
    )
  )
  
(defun maki-add-rst-title (title)
  (progn
    (dotimes (_ (length title)) 
      (insert "="))
    (insert "\n\n")
    )
  )

(defun maki-draw-post-title (title)
  (progn
    (insert (format "%s\n" title))
    (maki-add-rst-title title)
    )
  )

(defun maki-draw-post-simple-section (name content)
  (progn
    (insert (format ".. %s\n%s\n\n" name content))
    )
  )

(defun maki-draw-post-abstract (abstract)
  (maki-draw-post-simple-section "Abstract" abstract)
  )

(defun maki-draw-post-category (category)
  (maki-draw-post-simple-section "Category" category)
)

(defun maki-draw-post-content (content)
  (maki-draw-post-simple-section "Content" content)
  )

(defun maki-draw-post-tags (tags)
  (progn
    (insert ".. Tags\n")
    (dotimes (pos (length tags))
      (let* ((tag (aref tags pos)))
	(insert (format " - %s\n" tag))
	)
      )
    (insert "\n")
    )
  )

(defun maki-draw-post (postc)
  (let* ((title (gethash "title" postc))
	 (abstract (gethash "abstract" postc))
	 (content (gethash "content" postc))
	 (tags (gethash "tags" postc))
	 (category (gethash "category" postc))
	 (pformat (gethash "format" postc))
	 )
    (maki-draw-post-title title)
    (maki-draw-post-abstract abstract)
    (maki-draw-post-category category)
    (maki-draw-post-tags tags)
    (maki-draw-post-content content)
    (cond ((equal pformat "rst") (rst-mode))
	  ((equal pformat "textile") (textile-mode))
      )
    
    )
  )


(defun maki-get-current-title () 
    (progn
      (goto-char 1)
      (thing-at-point `line)
      )
    )

(defun maki-get-range-from (mytag nexttag) 
  (let (start stop)
    (goto-char 1)
    (setq start (1+ (re-search-forward (format "^%s$" mytag) nil t)))
    (setq stop (re-search-forward (format "^%s$" nexttag) nil t))
    (buffer-substring  start  (- stop (length nexttag)))
    )
)

(defun maki-get-current-abstract () 
  (let ((abstract ".. Abstract")
	(category ".. Category"))
    (maki-get-range-from abstract category)
    )
)

(defun maki-get-current-tags () 
  (let ((tags ".. Tags")
	(content ".. Content")
	(tag-cnt nil))
    (setq tag-cnt (maki-get-range-from tags content))
    (split-string (replace-regexp-in-string ".*- " "" tag-cnt))
    )
  )


(defun maki-get-current-content () 
  (let (start end)
    (goto-char 1)
    (setq start (1+ (re-search-forward "^.. Content$" nil t)))
    (setq end (1- (point-max)))
    (buffer-substring start end)
    )
  )


(defun maki-set-post-mode () 
  "Setup the environment in the maki-post minour mode."
  (progn
    (message "setting post mode...")
    (make-local-variable 'maki-post-id)
    ;; Overwrite the default save process, with a hardoced "t"
    ;; to always stop the chain of hooks.
    (make-local-variable 'write-file-functions)
    (setq write-file-functions 
	  (list
	      '(lambda () "Catch the save execution to save the changes"
		 (progn (maki-ask-save-post) t) 
		 )))
    )
  )

