(in-package :cl-user)
(defpackage riley
  (:use :cl :hunchentoot :cl-who :cl-pass :trivial-shell :mito :let-over-lambda :opticl :parenscript))
(in-package :riley)

;;;Updating item in db
;;;let binding, setf (slot-value 'obj 'slot) 'new-value
;;;mito:save-dao 'obj
;;;All predicted data objects neededers


(defun string-web-safe (stg)
  (let* ((poz (position #\' stg))
	 (sanitized-first (subseq stg 0 poz))
	 (sanitized-last (subseq stg (+ poz 1))))
    (web-safep (concatenate 'string sanitized-first "&#39;" sanitized-last))))

;;;Recursively remove ' from HTML to prevent escaping
(defun web-safep (stg)
  (if (position #\' stg)
      (string-web-safe stg)
      stg))

;;;Remove harmful characters to directory searching
(defun directory-safe (stg)
  (remove-if #'(lambda (x)
		 (or (char= x #\ )
		     (char= x #\')))
	     stg))
;;;If an item already has a return date remove it from the check-in list
;;;before passing the data to the check-in page
(defun remove-returned (inv-itemlist)
  (remove-if-not (lambda (x)
		   (string= "" (item-returned-on x)))
		 inv-itemlist))

(defun remove-returned-db (inv-id)
  (let ((itemlist (find-invoice-item-list inv-id)))
    (remove-if-not (lambda (x)
		     (string= "" (item-returned-on x)))
		   itemlist)))

;;;Helper function rarely used.  Finds an invoice from a make instanced invoice
(defun find-invoice-from-invoice (inv)
  (let ((setname (invoice-set-name inv))
	(showname (show-name inv)))
    (first (remove-if-not (lambda (x)
			    (and (string= setname (invoice-set-name x))
				 (string= showname (show-name x))))
			  *global-invoice-list*))))

;;;If an picture is already in the itemlist then remove it
;;;before sending it to the write-order page
(defun filter-already-in-itemlist (images invoice)
  (let ((itemlist (itemlist-image-location invoice)))
    (set-difference images itemlist
		    :test #'string=)))

;;;Helper function for grabbing image location
(defun itemlist-image-location (invoice)
  (mapcar #'(lambda (x)
	      (item-picture x))
	  (invoice-item-list invoice)))

;;DB Version
(defun list-image-loc (inv-id)
  (let ((itemlist (find-invoice-item-list inv-id)))
    (mapcar #'(lambda (x)
		(item-picture x))
	    itemlist)))

;;;Search the *GLOBAL-INVOICE-LIST* for INVOICENAME
(defun find-invoice (invoicename)
  (find invoicename *global-invoice-list* :test #'string-equal
	:key #'show-name))


;;;Helper function for moving uploaded pictures to the correct directory
;;;Needs to delete the pictures after and be expanded.
;;;This version (past revision 66) now has side effects
;;;deletes the temporary image!
(defun move-image-to-invoice-dir (img-data)
  (let* ((current-invoice (find-invoice-from-cookie (cookie-in "current-invoice")))
	 (invoice-location (invoice-root-dir current-invoice))
	 (temp-image-directory "/tmp/")
	 (image-name (third img-data)))
    (cl-fad:copy-file (make-pathname :directory temp-image-directory
				     :name image-name)
		      (make-pathname :directory invoice-location
				     :name image-name) :overwrite t)
    (ensure-directories-exist (concatenate 'string invoice-location "webimg/"))
    (let* ((new-img-name (concatenate 'string (invoice-set-name current-invoice)
				      "-"
				      (show-name current-invoice)
				      "-"
				      (write-to-string (invoice-id-num current-invoice))
				      ".jpg")))      
      (cl-fad:copy-file (make-pathname :directory temp-image-directory
				       :name image-name)
			(make-pathname :directory (concatenate 'string invoice-location "webimg/")
				       :name (directory-safe new-img-name)) :overwrite t)
      (let* ((new-img-path (make-pathname :directory (concatenate 'string invoice-location "webimg/")
					  :name (directory-safe new-img-name)))
	     (img (read-jpeg-file new-img-path)))
	(write-jpeg-file new-img-path (resize-image img 1200 1600))))
    (setf (invoice-id-num current-invoice) (+ (invoice-id-num current-invoice) 1))
    (delete-file (make-pathname :directory temp-image-directory
				:name image-name))))

;;; Webserver functions and scaffolding
;;;Set HTML5 preamble


(defun start-server (port)
  (start (make-instance 'easy-acceptor :port port))
  (mito:connect-toplevel :sqlite3 :database-name "riley"))



;;;Define page handler functions

;;;This handles adding a newly created invoice to the global invoice list
;;;This should be expanded to add logging of which user added which order/invoice
;;;also invoice id numbers need to be worked out

(hunchentoot:define-easy-handler (createInvoice :uri "/createInvoice") ()
  (let* ((showname (hunchentoot:post-parameter "inputShowname"))
	 (setname (hunchentoot:post-parameter "inputSetname"))
	 (contact (hunchentoot:post-parameter "inputContact"))
	 (root-dir (concatenate 'string (directory-namestring (acceptor-document-root *acceptor*))
				"show-bank"
				"/"
				(directory-safe showname)
				"/" (directory-safe setname) "/")))
    (ensure-directories-exist root-dir)
    (register-invoice 
		      :set-name setname
		      :show-name showname
		      :contact-name contact
		      :root-dir root-dir
		      :pdf-location '())
    (set-cookie "current-invoice" :value  *global-invoice-id*)
    (register-message :sender "Global Messages" :recipient "global"
		      :content (concatenate 'string "An order has been started for " showname
					    " for set " setname
					    " ordered by " contact )
		      :invoice-name (let ((shn showname)
					  (stn setname)
					  (cont contact))
				      (lambda ()
					(find-invoice-from-invoice
					 (make-instance 'invoice
							:set-name stn
							:show-name shn
							:contact-name cont))))))
  
  (redirect "/dashboard"))

;;;Basic function to create a new show		      
(define-easy-handler (write-order :uri "/write-order") ()
  (standard-page (:title "Write Order")
		 (:navbar (test-navbar))
		 (standard-order-intro)))

;;;Adds a message to the global message list
;;;This can be a notification for a new order/show/invoice
;;;or a message posted by a user
(define-easy-handler (addmessage :uri "/addmessage") ()
  (let ((username (cookie-in "current-user"))
	(message (hunchentoot:post-parameter "message")))
    (register-message :sender username
		      :recipient "global"
		      :content message))
  (redirect "/dashboard"))

;;;Standard dashboard
(define-easy-handler (dashboard :uri "/dashboard") ()
  (set-cookie "current-invoice" :value "none")
  (let ((username (cookie-in "current-user")))
    (if (not (or (string= username "login") (string= username "")))
	(standard-page (:title "Dashboard")
		       (:navbar  (test-navbar))
		       (standard-dashboard :messages (standard-global-messages)))
	(redirect "/login"))))

;;;This function needs to be redone.  Currently it takes the post parameters of the item
;;;associated with an invoice and finds which item it is in the itemlist;  current date needs to be fixed
(define-easy-handler (check-in-item :uri "/check-in-item") ()
  (let* ((invoice (find-invoice-from-cookie (cookie-in "current-invoice")))
	 (item-desc (hunchentoot:post-parameter "item-desc"))
	 (item-price (hunchentoot:post-parameter "item-price"))
	 (item-qty (hunchentoot:post-parameter "item-qty"))
	 (item (first (remove-if-not #'(lambda (x)
					 (and (string= (item-description x) item-desc)
					      (string= (item-price x) item-price)
					      (string= (item-quantity x) item-qty)))
				     (invoice-item-list invoice)))))

    (setf (item-returned-on item)
	  (current-date-string))
    (createpdf2)
    (redirect "/check-in-set")))

(defun current-date-string ()
  (multiple-value-bind
	(second minute hour date month year)
      (get-decoded-time)
    (declare (ignore second minute hour year))
    (format nil "~d/~d" month date)))
;;;This function has some leftover cruft that needs to be refactored.
(define-easy-handler (displayimagegot :uri "/displayimagegot") ()
  (let ((whatever (loop for post-parameter in (hunchentoot:post-parameters*)
		     if (equal (car post-parameter) "picture-batch")
		     collect post-parameter)))
    (standard-page (:title "Picture Batch")
		   (mapc #'(lambda (x)
			     (rename-file (second x)
					  (concatenate 'string "/tmp/"
						       (third x)))
			     (move-image-to-invoice-dir x))
			 
			 whatever)))
  (redirect "/setthemcookies"))

;;;Current solution to being able to refresh pages without putting data in the URI
;;;May need to start putting data in the URI but the function works pretty decently already.
(define-easy-handler (pre-set-cookies :uri "/pre-set-cookies") (showname setname)
  (set-cookie "current-invoice" :value (mito:object-id
					(mito:find-dao 'invoice-db
						       :set-name setname
						       :show (mito:find-dao 'show-db :name showname))))
  (redirect "/setthemcookies"))

;;;Adds an item from the additem panel on the write order page
;;;Absolutely needs javascript to test for the following conditions
;;;Empty input boxes;  Invalid input; and removing the pdf glitch
(define-easy-handler (add-item :uri "/additem") ()
  (let* ((invoice (find-invoice-from-cookie (cookie-in "current-invoice")))
	 (description (hunchentoot:post-parameter "input-item-description"))
	 (price (hunchentoot:post-parameter "input-item-price"))
	 (qty (hunchentoot:post-parameter "input-item-qty"))
	 (image (hunchentoot:post-parameter "image-data"))
	 (set-name (invoice-set-name invoice))
	 (show-name (show-name invoice)))
    
    (mito:create-dao 'item-db :description description
		     :quantity qty
		     :price price
		     :invoice-db (mito:find-dao 'invoice-db
						:show (mito:find-dao 'show-db :name show-name)
						:set-name set-name))
    

    (push (make-instance 'item :description description
			 :quantity qty
			 :price price
			 :pictures image
			 :returned-on ""
			 :returned-qty "0")
	  (invoice-item-list invoice)))
  (redirect "/setthemcookies"))

;;;Removes an item from the invoice.  Searches very basically.  Should probably have an item id
(define-easy-handler (remove-item :uri "/removeitem") ()
  (let* ((invoice (find-invoice-from-cookie (cookie-in "current-invoice")))
	 (item (hunchentoot:post-parameter "item"))
	 (item-price (hunchentoot:post-parameter "item-price"))
	 (item-quantity (hunchentoot:post-parameter "item-quantity")))
    
    (if (not (invoice-finalized invoice))
	(setf (invoice-item-list invoice) (remove-if #'(lambda (x)
							 (and (string= (item-description x) item)
							      (string= (item-price x) item-price)
							      (string= (item-quantity x) item-quantity)))	        
						     (invoice-item-list invoice))))
    (redirect "/setthemcookies")))

(defmacro web-math (&key func a b)
  `(write-to-string (,func (parse-integer ,a) (parse-integer ,b))))

(defun web-math-add (a b)
  (web-math :func +
	    :a a
	    :b b))

(defun web-math-subtract (a b)
  (web-math :func -
	    :a a
	    :b b))

(define-easy-handler (partial-check-in :uri "/partial-check-in") (qty desc price)
  (let* ((rtn (escape-string (hunchentoot:post-parameter "ranged")))
	 (invoice (find-invoice-from-cookie (cookie-in "current-invoice")))
	 (item-r (first (remove-if-not #'(lambda (x)
					   (and (string= desc (item-description x))
						(string= price (item-price x))
						(string= qty (item-quantity x))))
				       (invoice-item-list invoice)))))
    (cond
      ((string= (item-quantity item-r) (web-math-add (item-returned-qty item-r) rtn))
       (setf (item-returned-on item-r)
	     (current-date-string)))
      (t
       (setf (item-returned-qty item-r) (web-math-add (item-returned-qty item-r) rtn))
       (add-note-item item-r
		      (current-date-string)
		      rtn))))
  (redirect "/check-in-set"))

(defun add-note-item (item date amt)
  (if (already-note-p item date)
      (already-note item date amt)
      (new-note item date amt)))

(defun already-note-p (item date)
  (let ((check (remove-if-not #'(lambda (x)
				  (string= (funcall x :tag) date))
			      (item-notes item))))
    (if (null check)
	'()
	t)))

(defun already-note (item date amt)
  (let ((note (first (remove-if-not #'(lambda (x)
					(string= (funcall x :tag) date))
				    (item-notes item)))))
    (funcall note :add amt)))

(defun new-note (item date amt)
  (push                                              
   (let ((count amt)                                                           
	 (rtnd (concatenate 'string " RTND " date)))                                                 
     (let-over-lambda:dlambda                                                                
      (:display () (concatenate 'string count             
				rtnd))                                   
      (:inc () (incf count))                                                 
      (:tag () date)                                                 
      (:add (x) (setf count (web-math-add count x)))))
   (item-notes item)))
;;;Standard check in page that displays a table of shows with invoices

(define-easy-handler (checkinlist :uri "/checkinlist") ()
  (standard-page (:title "Check in list")
		 (:navbar (test-navbar))
		 (standard-check-in-showlist)))

;;;Required to get around the refresh problem.  Will need to expand solution
(define-easy-handler (check-in-set-pre :uri "/check-in-set-pre") (showname setname)
  (set-cookie "current-invoice"
	      :value (mito:object-id
		      (mito:find-dao 'invoice-db
				     :set-name setname
				     :show (mito:find-dao 'show-db :name showname))))
  (redirect "/check-in-set"))

;;;Presents a table of items that have not been checked in yet
(define-easy-handler (checkinset :uri "/check-in-set") ()
  (let* ((invoice (find-invoice-from-cookie (cookie-in "current-invoice")))
	 (showname (show-name invoice))
	 (setname (invoice-set-name invoice)))
    (standard-page (:title (concatenate 'string "Check in: " showname "-" setname))
		   (:navbar (test-navbar))
		   (standard-invoice-writing  :show  (fmt "~A" (escape-string showname))
					      :set (fmt "~A" (escape-string setname))
					      :contact (fmt "~A" (escape-string (invoice-contact-name invoice)))
					      :pic-num (fmt "~A" (escape-string (count-pics-from-invoice
										 (concatenate 'string showname
											      "-" setname)))))
		   (standard-check-in :invoice invoice))))


;;;Provides all the abstraction for generating a pdf from an invoice
;;;as long as a valid invoice is sent
;;;Might want a defun version instead so it doesn't route to a uri
(define-easy-handler (createpdf :uri "/createpdf") ()
  (let* ((invoice (find-invoice-from-cookie (cookie-in "current-invoice")))
	 (root-dir (invoice-root-dir invoice)))
    (generate-latex invoice root-dir))
  (redirect "/setthemcookies"))

;;;Cruft
(define-easy-handler (createpdf2 :uri "/createpdf2") ()
  (let* ((invoice (find-invoice-from-cookie (cookie-in "current-invoice")))
	 (root-dir (invoice-root-dir invoice)))
    (generate-latex invoice root-dir))
  (redirect "/check-in-set"))

;;;Need to rewrite so defintions are in the URL for reloading to work
(define-easy-handler (setthemcookies :uri "/setthemcookies") ()
  (let* ((invoice (find-invoice-from-cookie (cookie-in "current-invoice")))
	 (showname (db-show-name (invoice-show  invoice)))
	 (setname (db-set-name invoice))
	 (contact (db-contact (invoice-show  invoice)))
	 (change-pic-cookie (cookie-in "current-picture"))
	 (images (prepare-for-table (cl-fad:list-directory
				     (concatenate 'string (invoice-root-dir invoice) "webimg/"))))
	 (images-filtered (sort-item-list (filter-already-in-itemlist images invoice)
					  change-pic-cookie)))
    (set-cookie "current-picture" :value  "")
    (standard-page (:title "Order Writeup")
		   (:navbar (test-navbar))
		   (standard-three-nine-hook
		    (:bodythree
		     ((standard-invoice-writing :show  (fmt "~A" (escape-string showname))
						:set (fmt "~A" (escape-string setname))
						:contact (fmt "~A" (escape-string contact))
						:pic-num (fmt "~A" (escape-string (count-pics-from-invoice (concatenate 'string showname
															"-" setname)))))
		      (standard-picture-upload)))
		    
		    (standard-item-writeup :image (first images-filtered)
					   :full-images (rest images-filtered))
		    (standard-item-list-table :invoice invoice)
		    (standard-pdf-iframe :pdf "")))))


(defun sort-item-list (itemlist pic-need)
  (if (string= pic-need "")
      itemlist
      (cons (first (remove-if-not #'(lambda (x)
				      (equal x pic-need))
				  itemlist))
	    (remove-if #'(lambda (x)
			   (equal x pic-need))
		       itemlist))))

;;;Remove the absolute pathname and limit it to the show-bank directory
;;;Probably needs to be reworked entirely
(defun prepare-for-table (fad-list)
  (mapcar #'(lambda (x)
	      (let ((string-path-image (namestring x)))
		(subseq (namestring string-path-image) 53)))
	  (remove-if #'(lambda (x)
			 (cl-fad:directory-exists-p x))
		     fad-list)))

;;;Expects an invoice with the string showname-setname
(defun count-pics-from-invoice (inv)
  (if (string= "none" inv)
      "0"
      (let* ((invoice (find-invoice-from-cookie inv))
	     (invoice-pathname (invoice-root-dir invoice)))
	(concatenate 'string (write-to-string (filtered-length (cl-fad:list-directory invoice-pathname))) " pictures on order"))))

(defun filtered-length (directory-list)
  (length (remove-if #'(lambda (x)
			 (cl-fad:directory-exists-p x))
		     directory-list)))

(define-easy-handler (swapitemposition :uri "/swapitemposition") ()
  (set-cookie "current-picture" :value (hunchentoot:post-parameter "image-name"))
  (redirect "/setthemcookies"))

(define-easy-handler (signout :uri "/signout") ()
  (set-cookie "current-user" :value "login")
  (redirect "/login"))

(define-easy-handler (profilelogin :uri "/profilelogin") ()
  (redirect "/login"))

(define-easy-handler (home :uri "/") ()
  (standard-page (:title "RILEY Inventory System")
		 (redirect "/login")))

(define-easy-handler (check-login :uri "/check-login") ()
  (let
      ((username (hunchentoot:post-parameter "username"))
       (password (hunchentoot:post-parameter "password")))
    (cond ((cl-pass:check-password password (user-password (find-user username)))
	   (set-cookie "current-user" :value username)
	   (redirect "/dashboard"))
	  (t
	   (redirect "/login")))))

(define-easy-handler (adduser :uri "/adduser") ()
  (let ((username (hunchentoot:post-parameter "username"))
	(password (hunchentoot:post-parameter "password"))
	(password-repeat (hunchentoot:post-parameter "password-repeat")))
    (if (and (string= password password-repeat)
	     (not (find-user username)))
	(and (register-user :username username
			    :password (cl-pass:hash password))
	     (redirect "/login"))
	(redirect "/badpassword"))))

(define-easy-handler (login :uri "/login") ()
  (standard-page (:title "Login")
		 (standard-login)))

(define-easy-handler (rotate-image :uri "/rotate-image") (image)
  (let* ((invoice (find-invoice-from-cookie (cookie-in "current-invoice")))
	 (rotation (/ PI 2))
	 (root-dir (invoice-root-dir invoice))
	 (image-name (subseq-img-path image)))
    (opticl:write-image-file
     (format nil (concatenate 'string root-dir "webimg/" image-name) rotation)
     (transform-image (read-jpeg-file (make-pathname :directory (concatenate 'string root-dir "webimg/")
						     :name image-name))
		      (reduce #'opticl:matrix-multiply
			      (list (make-affine-transformation :theta rotation)))
		      :transform-bounds t)))
  (redirect "/setthemcookies"))


