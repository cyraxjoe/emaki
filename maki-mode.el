;; Maki mode
;;
(require 'url)
(require 'json)

(defconst maki-mode-version "0.1"
  "Version of `maki-mode'.")

(defgroup maki nil 
  "Maki bloggin major mode")

(defcustom maki-host "http://127.0.0.1:8080" 
  "Base host url"
  :type 'string
  :group 'maki)

(defcustom maki-debug nil
  "Set to not nil to show debugging messages"
  :type 'boolean
  :group 'maki)

(defcustom maki-post-uri "/post/"
  "URI to fetch the post content"
  :type 'string
  :group 'maki)

(defcustom maki-login-uri "/login/"
  "URI to authenticate in the server."
  :type 'string
  :group 'maki)


(defcustom maki-banner 
  (mapconcat 'identity 
	     '(
	       "    ,;'O@';,    ,;'O@';,    ,;'O@';,  "
	       "   |',_@H_,'|  |',_@H_,'|  |',_@H_,'| "
	       "   |        |  |        |  |        | "
	       "    '.____.'    '.____.'    '.____.'  "
	       "        ___  ___        _     _       "
	       "        |  \\/  |       | |   (_)      "
	       "        | .  . |  __ _ | | __ _       "
	       "        | |\\/| | / _` || |/ /| |      "
	       "        | |  | || (_| ||   < | |      "
	       "        \\_|  |_/ \\__,_||_|\\_\\|_|      "
	       "                                      "
	       "               Welcome                ") "\n")
  "Text to be displayed at the begining of the maki-mode."
  :type 'string
  :group 'maki)

(defcustom maki-user nil
  "User to be used in the maki server. If `nil' ask the user."
  :type 'string
  :group 'maki)

(defcustom maki-passwd nil
  "Password to be used in the maki server. If `nil' ask user."
  :type 'string
  :group 'maki)

(defcustom maki-autologin t
  "Try to login when the mode is invoked."
  :type 'boolean
  :group 'maki)

(defcustom maki-mode-hook nil 
  "Hook caled by `maki-mode'. Use this to customize."
)


;; keybinding
(defvar maki-mode-map nil
  "Local key map to the maki mode.")

(defvar maki-post-mode-map nil
  "Local keymap to the maki-post minor mode."
  )

(setq maki-mode-map (make-sparse-keymap))
(define-key maki-mode-map "\C-c\C-f" 'maki-get-post)
(setq maki-post-mode-map (make-sparse-keymap))
;;

;; utilities
(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
		       str)
    (setq str (replace-match "" t t str)))
  str)

(defun maki-json-headers ()
  '(("Content-Type" . "application/json")
    ("Accept" . "application/json"))
  )
;;

;; HTTP request related functions
(defun maki-get-post (pid)
  "Fetch the JSON post with the id or slug (identifier) from the maki blog"
  (interactive "nPost id: ")
  (let ((url-request-extra-headers (maki-json-headers)))
    (if maki-debug
	(message "Fetching '%s' " (maki-post-url pid))
      )
    (url-retrieve  (maki-post-url pid) 'maki-show-post))
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
      )
    (if maki-debug
	(message "Posting post hash = %s\njson = %s " 
		 post (json-encode post))
	)
    (let ((cbuffer (current-buffer))
	  (url-request-method "POST")
	  (url-request-extra-headers (maki-json-headers))
	  (url-request-data (json-encode post)))
      (url-retrieve (maki-post-update-url maki-post-id) 
		    'maki-confirm-save 
		    (list cbuffer))
      )
    )
  )
;;

(defun maki-confirm-save (status postbuff) 
  "Check the response of the server and inform the results."
  (if (null status)
      (with-current-buffer postbuff
	(message "Post successfully saved") 
	(set-buffer-modified-p nil)
	)
    (message "Unable to save %s" status)
    (message (buffer-substring (point-min) (point-max)))
    )
  )

(defun maki-ask-save-post () 
  "Save the changes to the blog post, but confirm first."
  (interactive)
  (if (buffer-modified-p) 
      (if (equal "y" (read-string "Do you really wanna save the changes? (y/n): "))
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
	    (if (equal (read-string "The post is already loaded. Reload? (y/n): " )
		       "y")
		(kill-buffer buffname))
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

(defun maki-get-post-content () 
  "Parse the body of the GET response, return a hash with the received JSON."
  (progn
    (if maki-debug
	(message "Parsing json post content")
	)
    (maki-get-json-response)
    ))

(defun maki-get-json-response () 
  "This is meant to be used in the url-retrieve callbacks."
  (let ((json-object-type 'hash-table))
    (goto-char (point-max))
    (setq cnt (thing-at-point `line))
    (if maki-debug
	(message "JSON Response content \n %s" cnt))
    (json-read-from-string cnt)
    )
  )
  


(defun maki-post-url (pid)
  (concat maki-host maki-post-uri (number-to-string pid))
  )

(defun maki-login-url ()
  (concat maki-host maki-login-uri)
  )

(defun maki-post-update-url (pid)
  (concat maki-host maki-post-uri "/update/" (number-to-string pid))
  )


;; Function to format the fetched post from the server.
(defun maki-draw-rst-title (title)
  (progn
    (dotimes (_ (length title)) 
      (insert "="))
    (insert "\n")
    )
  )

(defun maki-draw-post-title (title)
  (progn
    (insert (format "%s\n" title))
    (maki-draw-rst-title title)
    )
  )

(defun maki-draw-post-simple-section (name content)
  (insert (format ".. %s\n%s\n" name content))
  
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
      (chomp (thing-at-point `line))
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
    (chomp (maki-get-range-from abstract category))
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

;; Authentication related function

(defun maki-login (&optional refresh)
  "Login to the server"
  (interactive "sRefresh auth data?: ")
  (let ((user (maki-auth-get-user refresh))
	(passwd (maki-auth-get-passwd refresh)))
	
    (let* ((url-request-method "POST")
	   (url-request-extra-headers (maki-json-headers))
	   (url-request-data 
	    (json-encode (list (cons "user" user) 
			       (cons "passwd" passwd))
			 )))
      (url-retrieve (maki-login-url) 'maki-login-check)
      (if maki-debug
	  (progn
	    (message "Loggin in with %s %s" user passwd)
	    (message "Request data %s" url-request-data )
	    )
	)
      )
    ))

(defun maki-login-check (status)
  "Validate the response of the server."
  (if (null status)
      (let ((json-false nil))
	(let ((response (maki-get-json-response)))
	  (if maki-debug
	      (message "Login server response %s" response)
	    )
	  (if (null (gethash "authenticated" response))
	      (message "Unable to login, Invalid authentication.")
	    (message "Successfully logged in.")
	    )
	  )
	)
    (message "Unable to login")
    (message (buffer-substring (point-min) (point-max)))
    )
  )

(defun maki-auth-get-user (&optional refresh)
  (progn 
    (if refresh
	(setq maki-user nil)
      )
    (if (null maki-user)
	(setq maki-user (read-string "User: "))
      )
    maki-user)
  )

(defun maki-auth-get-passwd (&optional refresh)
  "Get the user password, if is not defined use password-cache ask and return."
  (progn 
    (if refresh
	(progn
	  (setq maki-passwd nil)
	  (password-cache-remove "maki")
	  )
      )
    )
  (if (null maki-passwd)
      (let* ((passwd (password-read "Password: " "maki")))
	(password-cache-add "maki" passwd)
	passwd)
    maki-passwd)
  )
;;



(defun maki-set-post-mode () 
  "Setup the environment in the maki-post minor mode."
  (progn
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

(defun maki-mode ()
  "Major mode to edit the maki blog."
  (interactive)
  (progn 
    (switch-to-buffer "*Maki*")
    (insert maki-banner)
    (kill-all-local-variables)
    (make-local-variable 'maki-curr-post)
    (setq maki-curr-post nil)
    (setq major-mode 'maki-mode)
    (setq mode-name "Maki")
    (setq buffer-read-only t)
    (use-local-map maki-mode-map)
    (if maki-autologin
	(maki-login)
	)
    )
  )

(define-minor-mode maki-post-mode
  "Set the minor mode for the post edition.
     With no argument, this command toggles the mode.
     Non-null prefix argument turns on the mode.
     Null prefix argument turns off the mode."
  :init-value nil
  :lighter " maki-post"
  :keymap 'maki-post-mode-map)

(provide 'maki-mode)
