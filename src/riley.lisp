(in-package :cl-user)
(defpackage riley
  (:use :cl :hunchentoot :cl-who :cl-pass :trivial-shell :mito :opticl :parenscript))
(in-package :riley)

;;;Updating item in db
;;;let binding, setf (slot-value 'obj 'slot) 'new-value
;;;mito:save-dao 'obj
;;;All predicted data objects neededers

(defclass show-db ()
  ((contact :initarg :contact
	    :accessor db-contact
	    :col-type :text)
   (phone-number :initarg :phone-number
		 :accessor db-phone-number
		 :col-type :text)
   (name :initarg :name
	 :accessor db-show-name
	 :col-type :text))
  (:metaclass mito:dao-table-class))

(defclass invoice-db ()
  ((set-name :initarg :set-name
	     :accessor db-set-name
	     :col-type :text)
   (date-out :initarg :date-out
	     :accessor db-date-out
	     :col-type :text)
   (show :col-type show-db
	 :initarg :show-db
	 :accessor invoice-show)
   (root-dir :col-type :text
	     :accessor invoice-root-dir
	     :initarg :root-dir)
   (pdf-location :initarg :pdf-location
		 :accessor invoice-pdf-location
		 :col-type :text))
  (:metaclass mito:dao-table-class))

(defclass item-db ()
  ((price :initarg :price
	  :accessor db-item-price
	  :col-type :text)
   (quantity :initarg :quantity
	     :accessor db-item-qty
	     :col-type :text)
   (description :initarg :description
		:accessor db-item-desc
		:col-type :text)
   (invoice :col-type invoice-db
	    :initarg :invoice-db
	    :accessor db-item-invoice)
   (picture :initarg :pictures
	    :accessor item-picture
	    :col-type :text)
   (returned-qty :initarg :returned-qty
		 :accessor item-returned-qty
		 :col-type :text)
   (returned-on :initarg :returned-on
		:accessor item-returned-on
		:col-type :text))
  (:metaclass mito:dao-table-class))

(defclass multi-pic ()
  ((item :col-type item-db
	 :initarg :item-db
	 :accessor db-multi-item)
   (picture :initarg :picture
	    :accessor multi-picture
	    :col-type :text))
  (:metaclass mito:dao-table-class))

(defclass user-db ()
  ((name :initarg :name
	 :accessor db-user-name
	 :col-type :text)
   (rank :initarg :rank
	 :accessor db-user-rank
	 :col-type :text)
   (password :initarg :password
	     :accessor db-user-pass
	     :col-type :text))
  (:metaclass mito:dao-table-class))

(defclass partial-db ()
  ((return-qty :initarg :return-qty
	       :accessor partial-return-qty
	       :col-type :text)
   (return-date :initarg :return-date
		:accessor partial-return-date
		:col-type :text)
   (item :col-type item-db
	 :initarg :item-db
	 :accessor db-partial-item))
  (:metaclass mito:dao-table-class))

(defun invoice-item-list (invoice)
  (mito:retrieve-dao 'item-db :invoice invoice))

(defun find-show-db (&rest args)
  (apply #'mito:find-dao 'show-db args))

(defun find-invoice-db (&rest args)
  (apply #'mito:find-dao 'invoice-db args))

(defun find-item-db (&rest args)
  (apply #'mito:find-dao 'item-db args))

(defun find-user-db (&rest args)
  (apply #'mito:find-dao 'user-db args))

(defun register-user (&key username password)
  (mito:create-dao 'user-db 
		   :name username
		   :password password
		   :rank "user"))

(defun more-than-one (item)
  (mito:retrieve-dao 'multi-pic :item item))

(defun add-show-to-db (&key name contact)
  (mito:create-dao 'show-db :name name :contact contact :phone-number "000"))

(defun register-invoice (&key 
			   set-name
			   show-name
			   contact-name
			   root-dir
			   pdf-location)
  ;;;Check if show exists in db table
  ;;;if not then create it and link it to the new set
  (if (not (mito:find-dao 'show-db :name show-name))
      (add-show-to-db :name show-name
		      :contact contact-name))
  (mito:create-dao 'invoice-db :set-name set-name :date-out "000" :show-db (mito:find-dao 'show-db :name show-name) :root-dir root-dir :pdf-location (subseq pdf-location 71)))

(defun find-invoice-from-cookie (id)
  (find-invoice-db :id (parse-integer id)))

(defun find-invoice-item-list (id)
  (mito:retrieve-dao 'item-db :invoice-id (parse-integer id)))

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
		     (char= x #\')
		     
		     (char= x #\")
		     (char= x #\!)))
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
    (let* ((new-img-name (concatenate 'string (db-set-name current-invoice)
				      "-"
				      (db-show-name (invoice-show  current-invoice))
				      "-"
				      image-name
				      ".jpg")))      
      (cl-fad:copy-file (make-pathname :directory temp-image-directory
				       :name image-name)
			(make-pathname :directory (concatenate 'string invoice-location "webimg/")
				       :name (directory-safe new-img-name)) :overwrite t)
      (let* ((new-img-path (make-pathname :directory (concatenate 'string invoice-location "webimg/")
					  :name (directory-safe new-img-name)))
	     (img (read-jpeg-file new-img-path)))
	(write-jpeg-file new-img-path (resize-image img 1200 1600))))
    
    (delete-file (make-pathname :directory temp-image-directory
				:name image-name))))

;;; Webserver functions and scaffolding
;;;Set HTML5 preamble


(defun start-server (port)
  (start (make-instance 'easy-acceptor :port port))
  (mito:connect-toplevel :sqlite3 :database-name "riley.db")
  (mapcar #'mito:ensure-table-exists '(item-db invoice-db show-db user-db partial-db multi-pic)))



;;;Define page handler functions

;;;This handles adding a newly created invoice to the global invoice list
;;;This should be expanded to add logging of which user added which order/invoice
;;;also invoice id numbers need to be worked out

(define-easy-handler (createInvoice :uri "/createInvoice") ()
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
     :pdf-location (directory-safe (concatenate 'string root-dir "pdf/" showname "-" setname ".pdf")))
    (redirect "/dashboard")))

;;;Basic function to create a new show		      
(define-easy-handler (write-order :uri "/write-order") ()
  (standard-page (:title "Write Order")
    (:navbar (test-navbar))
    (standard-order-intro)))

;;;Adds a message to the global message list
;;;This can be a notification for a new order/show/invoice
;;;or a message posted by a user

;;;Standard dashboard
(define-easy-handler (dashboard :uri "/dashboard") ()
  (set-cookie "current-invoice" :value "none")
  (let ((username (cookie-in "current-user")))
    (if (not (or (string= username "login") (string= username "")))
        (redirect "/checkinlist")
	(redirect "/login"))))

;;;This function needs to be redone.  Currently it takes the post parameters of the item
;;;associated with an invoice and finds which item it is in the itemlist;  current date needs to be fixed
(define-easy-handler (check-in-item :uri "/check-in-item") ()
  (let* ((invoice (find-invoice-from-cookie (cookie-in "current-invoice")))
	 (item-desc (hunchentoot:post-parameter "item-desc"))
	 (item-price (hunchentoot:post-parameter "item-price"))
	 (item-qty (hunchentoot:post-parameter "item-qty"))
	 (item (find-item-db :invoice invoice :description item-desc
			     :price item-price :quantity item-qty)))

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
	 (set-name (db-set-name invoice))
	 (show-name (db-show-name (invoice-show invoice))))
    
    (mito:create-dao 'item-db :description description
		     :quantity qty
		     :price price
		     :picture image
		     :pictures ""
		     :returned-on ""
		     :returned-qty "0"
		     :invoice-db (mito:find-dao 'invoice-db
						:show (mito:find-dao 'show-db :name show-name)
						:set-name set-name)))
  (redirect "/setthemcookies"))

;;;Removes an item from the invoice.  Searches very basically.  Should probably have an item id
(define-easy-handler (remove-item :uri "/removeitem") ()
  (let* ((invoice (find-invoice-from-cookie (cookie-in "current-invoice")))
	 (item (hunchentoot:post-parameter "item"))
	 (item-price (hunchentoot:post-parameter "item-price"))
	 (item-quantity (hunchentoot:post-parameter "item-quantity")))
    
    
    (mito:delete-dao  (find-item-db
		       :description item
		       :quantity item-quantity
		       :price item-price
		       :invoice (mito:find-dao 'invoice-db
					       :show (mito:find-dao 'show-db :name (db-show-name (invoice-show invoice)))
					       :set-name (db-set-name invoice))))
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

(defun concat-rtn-qty (item)
  (let* ((item-id (mito:object-id item))
	 (partial-list (mito:retrieve-dao 'partial-db :item-id item-id)))
    (if (not (null partial-list))
	(reduce #'web-math-add (mapcar #'partial-return-qty partial-list))
	"0")))

(define-easy-handler (multi-pic-add :uri "/multi-pic-add") (qty desc price pic)
  (let* ((invoice (find-invoice-from-cookie (cookie-in "current-invoice")))
	 (item-r (find-item-db :price price :quantity qty :description desc :invoice (mito:find-dao 'invoice-db
												    :show (mito:find-dao 'show-db :name (db-show-name (invoice-show invoice)))
												    :set-name (db-set-name invoice)))))
    (mito:create-dao 'multi-pic
		     :item-db item-r
		     :picture pic)
    (redirect "/setthemcookies")))

(define-easy-handler (partial-check-in :uri "/partial-check-in") (qty desc price)
  (let* ((rtn (escape-string (hunchentoot:post-parameter "ranged")))
	 (invoice (find-invoice-from-cookie (cookie-in "current-invoice")))
	 (item-r (find-item-db :price price :quantity qty :description desc  :invoice (mito:find-dao 'invoice-db
												     :show (mito:find-dao 'show-db :name (db-show-name (invoice-show invoice)))
												     :set-name (db-set-name invoice))))
	 (item-cur-part (mito:retrieve-dao 'partial-db :item item-r)))
    
    (if (not (search-cur item-cur-part))
	(mito:create-dao 'partial-db :return-qty rtn
			 :return-date (current-date-string)
			 :item-db item-r)
	(let* ((partial-item (mito:find-dao 'partial-db :return-date (current-date-string)
					    :item item-r))
	       (partial-item-qty (partial-return-qty partial-item)))
	  (if (string>= (web-math-add rtn partial-item-qty) (db-item-qty item-r))
	      (and (setf (slot-value item-r 'returned-on) (current-date-string))
		   (mito:save-dao item-r))
	      (and (setf (slot-value partial-item 'return-qty) (web-math-add rtn partial-item-qty))									   (mito:save-dao partial-item))))))
  (redirect "/check-in-set"))

(defun search-cur (item-cur-part)
  (remove-if-not #'(lambda (x)
		     (and (string= (current-date-string) (partial-return-date x))))
		 item-cur-part))
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
	 (showname (db-show-name (invoice-show invoice)))
	 (setname (db-set-name invoice)))
    (standard-page (:title (concatenate 'string "Check in: " showname "-" setname))
      (:navbar (test-navbar))
      (standard-invoice-writing  :show  (fmt "~A" (escape-string showname))
				 :set (fmt "~A" (escape-string setname))
				 :contact (fmt "~A" (escape-string (db-contact (invoice-show invoice))))
				 :pic-num (fmt "~A" (escape-string (count-pics-from-invoice))))
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
	 (showname (db-show-name (invoice-show invoice)))
	 (setname (db-set-name invoice))
	 (contact (db-contact (invoice-show invoice)))
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
				      :pic-num (fmt "~A" (escape-string (count-pics-from-invoice))))
	    (standard-picture-upload)))
	
	(standard-item-writeup :image (first images-filtered)
			       :full-images (rest images-filtered))
	(standard-item-list-table :invoice invoice)
	(standard-pdf-iframe :pdf (invoice-pdf-location invoice))))))


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
(defun count-pics-from-invoice ()
  (let* ((invoice (find-invoice-from-cookie (cookie-in "current-invoice")))
	 (invoice-pathname (invoice-root-dir invoice)))
    (concatenate 'string (write-to-string (filtered-length (cl-fad:list-directory invoice-pathname))) " pictures on order")))

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
    (cond ((cl-pass:check-password password (db-user-pass (find-user-db :name username)))
	   (set-cookie "current-user" :value username)
	   (redirect "/dashboard"))
	  (t
	   (redirect "/login")))))

(define-easy-handler (adduser :uri "/adduser") ()
  (let ((username (hunchentoot:post-parameter "username"))
	(password (hunchentoot:post-parameter "password"))
	(password-repeat (hunchentoot:post-parameter "password-repeat")))
    (if (and (string= password password-repeat)
	     (not (find-user-db :name username)))
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


;;;Macro's for generating HTML
;;;Macros should be designed to work together so pages
;;;can be as modular as possible.

(defmacro standard-page ((&key title navbar) &body body)
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html :lang "en"
	    (:head
	     (:meta :name "viewport" :content "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no")
	     (:link :type "text/css"
		    :rel "stylesheet"
		    :href "css/materialize.min.css")
	     (:script :src "js/jquery.js")
	     (:script :src "js/ajax-item.js")
	     (:script :src "js/materialize.min.js")
	     (:script :src "js/jQueryRotate.js")
	     (:script :src "js/handlebars.js")
	     (:script :src "js/removeitem.js")
	     (:script :src "js/modal-add.js")
	     (:link :href "https://fonts.googleapis.com/icon?family=Material+Icons" :rel "stylesheet")
	     (:script :src "plugins/jq-input.js")
	     (:title ,title))
	    
	    (:body 
	     ,@navbar		   	       
	     (:div :class "container"
		   (:div :class "section"
			 ,@body ))))))


(defmacro standard-three-nine-hook ((&key bodythree) &body bodynine)
  `(with-html-output (*standard-output* nil :indent t)
     
     (:div :class "row"
	   (:div :class "col s12 md3"
		 
		 ,@bodythree)
	   
	   (:div :class "col s12 md9"
		 ,@bodynine))))

(defmacro standard-login ()
  `(with-html-output (*standard-output* nil :indent t)
     (:heading (:h2 :class "center" "Welcome to RILEY"))
     (:div :class "row red darken-4"
	   (:div :class "col s12 m6"
		 
		 (:div :class "card blue-grey darken-1"
		       (:div :class "card-content white-text"      
			     (:form :action "/adduser"
				    :method "POST"
				    :id "commentform"
				    (:div :class "input-field col s12"
					  (:input :type "text"
						  :class "form-control"
						  :name "username"
						  :id "username"
						  :required "required")
					  (:label :class "white-text" :for "username" "Username"))

				    (:div :class "input-field col s6"
					  (:input :type "password"
						  :name "password"
						  :class "form-control"
						  :id "password"
						  :required "required")
					  (:label :class "white-text" :for "password" "Password"))

				    (:div :class "input-field col s6"
					  (:input :type "password"
						  :class "form-control"
						  :name "password-repeat"
						  :id "password-repeat"
						  :required "required")
					  (:label :class "white-text" :for "password-repeat" "Repeat"))
				    
				    (:div :class "center"
					  (:button :type "submit"
						   :class "btn waves-effect waves-light"
						   "Create Account"
						   (:i :class "material-icons right" "send")))))))
	   
	   (:div :class "col m6 s12"
		 (:div :class "card blue-grey darken-1"
		       (:div :class "card-content white-text"
			     
			     
			     (:form :action "/check-login"
				    :method "POST"
				    :id "commentform"
				    (:div :class "input-field col s12"
					  (:input :type "text"
						  :class "form-control"
						  :name "username"
						  :id "username"
						  :required "required")
					  (:label :class "white-text" :for "username" "Username"))
				    
				    (:div :class "input-field col s12"
					  
					  (:input :type "password"
						  :class "form-control"
						  :name "password"
						  :id "password"
						  :required "required")
					  (:label :class "white-text" :for "password" "Password"))
				    (:div :class "center"
					  (:button :type "submit"
						   :class "btn waves-effect waves-light"
						   "Login"
						   (:i :class "material-icons right" "send"))))))))))


(defmacro test-navbar ()
  `(with-html-output (*standard-output* nil :indent t)
     (:header 
      (:nav :class "red darken-4"
	    
	    (:div :class "nav-wrapper"
		  (:a :class "center brand-logo" "CAPS")
		  
		  (:a :href "#" :data-activates "mobile-demo" :class "button-collapse"
		      (:i :class "material-icons" "menu"))

		  (:ul :class "right hide-on-med-and-down"
		       (:li (:a :href "/dashboard" "Dashboard"))
		       (:li (:a :href "/write-order" "Write Order"))
		       (:li (:a :href "/checkinlist" "Show List"))
		       (:li (:a :href "/signout" "Sign Out")))
		  
		  (:ul :class "side-nav" :id "mobile-demo"
		       (:li (:a :href "/dashboard" "Dashboard"))
		       (:li (:a :href "/write-order" "Write Order"))
		       (:li (:a :href "/checkinlist" "Show List"))
		       (:li (:a :href "/signout" "Sign Out")))
		  (:script "$('.button-collapse').sideNav();"))))))

(defmacro standard-dashboard (&key messages)
  `(with-html-output (*standard-output* nil :indent t)	     
     (:div :class "row"
	   (:div :class "col s12 m12"
		 (:h1 :class "center" "Dashboard")
		 (:div :class "row"
		       (:form :role "form"
			      :action "/addmessage"
			      :method "post"		      
			      (:div :class "input-field col s12"
				    (:label :for "say-something" "Post a message!")
				    (:input :type "text" :class "validate"
					    :id "message" :name "message")
				    (:button :type "Submit Message" :class "btn btn-default btn-info" "Message")))))	   
	   ,messages)))


;;;Ugly as hell
;;;Tree's lists so they work properly on the website
(defun pair-off (lst)
  (cond
    ((null lst) '())
    ((> (length lst) 1) (list (list (car lst) (cadr lst)) (pair-off (cddr lst))))
    ((<= (length lst) 1) (list (car lst) '()))))


;;;gensym portion probably needs to be changed
(defmacro standard-item-writeup (&key image full-images)
  `(with-html-output (*standard-output* nil :indent t)
     (:div :class "section"
	   (:div :class "row blue-grey"
		 (:div :class "col s12 m6 l6"
		       (:div :class "card"
			     
			     (if (null ,image)
				 (htm (:div :class "card-title" (:p :class "center" "No pictures available")))
				 (htm  (:div :class "card-image"
					     (:img :id "input-picture" :src (escape-string (concatenate 'string ,image)) :class  "materialboxed responsive-img" :name ,image))))))
		 (:div :class "col s12 m6 l6"
		       (:script :id "new-pic-template" :type "text/x-handlebars-template"
				"<li> <div class='col l3 m4 s6'>
<div class='card-image'>
<div class='carousel'>
<a class='carousel-item' href='#one!'>
<img src={{imgname}} class='responsive-img materialboxed' data-caption={{caption}} />
</a>
</div>
</div>
<div class='card-content'>
<span class='card-title truncate'> {{caption}}
</span>
<span
<i class='material-icons'> attach_money
</i> {{price}} &emsp;
<a href='#' class='right black-text'>
            <i class='material-icons'>all_inclusive
            </i> {{quantity}}
          </a>
        </span>
      </div>
      <div class='card-action'>
        <form class='form-inline' action='/removeitem' method='POST' id='removebutton'>
          <input type='hidden' value={{caption}} name='item' id='item' />
          <input type='hidden' value={{price}} name='item-price' id='item-price' />
          <input type='hidden' value={{quantity}} name='item-quantity' id='item-quantity' />
          <input type='hidden' value='' name='invoice' id='invoice' />
<button type='submit' class='red darken-4 btn btn-default btn-sm btn-danger'>Remove
</button>
          <a href='#' class='btn-floating waves-effect weaves-light' onclick='modal-add.js' data-target='myModal'>
            <i class='material-icons'>content_copy
            </i>
          </a>
        </form>
      </div>
    </div>
  </div>
</li>")
		       
		       (:div :class "card" :id "box-picture"
			     
			     
			     (:div :class "card-content"
				   (:form 
				    :action "/additem"
				    :method "POST"
				    :id "new-item"
				    (:div :class "input-field col s12 l12 m12"
					  (:label :for "inputDesc" :class "black-text"  "Description")
					  
					  (:input :type "text" 
						  :id "input-item-description"
						  :name "input-item-description"
						  :required "required"))
				    (:div :class "input-field col s12 l6 m6"
					  
					  (:label :for "inputPrice" :class "black-text"  "Price")
					  (:input :type "number" 
						  :id "input-item-price"
						  :step "0.01"
						  :name "input-item-price"
						  :required "required"))
				    (:div :class "input-field col s12 m6 l6"
					  
					  (:label :for "inputQty" :class "black-text" "Quantity")
					  (:input :type "number" 
						  :id "input-item-qty"
						  :name "input-item-qty"
						  :required "required"))
				    (:input :type "hidden" :id "image-data" :name "image-data" :value ,image)
				    
				    (:button :onclick "additem()" :type "submit" :class "red darken-4 btn waves-effect waves-light" "Add")
				    (:a  :class "red  dropdown-button darken-4 btn waves-effect waves-light"
					 
					 :href "#"
					 :data-activates "dropdown1"
					 :data-beloworigin "true"
					 "Rotate")
				    (:ul :id "dropdown1" :class "dropdown-content"
					 (:li (:a :href "#"
						  :class "red-text" (:i :class "material-icons" "rotate_left") "Rotate Left"))
					 (:li (:a :href (format nil "rotate-image?image=~a"
								,image)
						  :class "red-text" (:i :class "material-icons" "rotate_right") "Rotate Right")))
				    (:button :type "button" :class "red darken-4 btn waves-effect waves-light"
					     :data-target "myModal" "Switch")))))))
     
     (:div :id "myModal" :class "modal" 
	   (:div :class "modal-content"
		 (:h4 "Switch Pictures")
		 (dolist (img ,full-images)
		   (htm
		    (:div :class "col s12 m6 l6"
			  (:div :class "card"
				(:div :class "card-content"
			  (:form :action "/swapitemposition"
				 :method "POST"
				 (:input :type "hidden" :id "image-name"
					 :name "image-name" :value img)
				 (:input :type "image" :id "saveform" :class "img-responsive"
					 :width "100%" :height "50%" :src img
					 :alt "Submit Form")))
				(:div :class "card-action"
				      (:a :id "modalButton2" :class "dropdown-button btn" :href "#" :data-activates "dropdown2" "Multi-Pic")))))))
	   (:script "$(document).ready(function(){ $('.modal').modal(); });"))))

(defmacro standard-order-intro ()
  `(with-html-output (*standard-output* nil :indent t)
     
     (:div :class "card blue-grey"
	   (:div :class "card-content white-text"
		 (:form 
		  :action "/createInvoice"
		  :method "POST"
		  :id "New-invoice-form"
		  (:div :class "input-field col s12"
			(:label :class "white-text" :for "inputShow" "Show")
			(:input :type "text" :class "form-control" :id "inputShowname"
				:name "inputShowname"))
		  (:div :class "input-field col s6 m6 l6"
			(:label :class "white-text" :for "inputSet" "Set")
			(:input :type "text" :class "white-text" :id "inputSetname"
				:name "inputSetname"))
		  (:div :class "input-field col s6 m6 l6"
			(:label :class "white-text" :for "inputContact" "Contact Name")
			(:input :type "text" :class "form-control" :id "inputContact"
				:name "inputContact"))
		  (:div :class "center"
			(:button :type "submit" :class "red darken-4 btn waves-effect waves-light" "Write Show Order" (:i :class "material-icons right" "send"))))))))

(defmacro standard-check-in-showlist ()
  `(with-html-output (*standard-output* nil :indent t)
     (:div :class "row"
	   (dolist (invoice (mito:retrieve-dao 'invoice-db))
	     (htm
	      (:div :class "col s12 m4 l4"
		    (:div :class "card"
			  (:div :class "card-content"
				(:span :class "card-title" (:h5
							    (fmt "~A" (escape-string (db-show-name (invoice-show  invoice))))))
				
				
				(:div :class "chip black-text"  (fmt "~A" (escape-string (db-set-name invoice))))
					;(:div :class "chip black-text" (fmt "Contact: ~A" (escape-string ( invoice))))
				(:div :class "card-action"
				      (:a :href (format nil "check-in-set-pre?showname=~a&setname=~a" (web-safep (db-show-name (invoice-show  invoice))) (web-safep (db-set-name invoice)))
					  :class "btn-flat waves-effect red-text  text-darken-4" "Check")
				      (:a :href (format nil "pre-set-cookies?showname=~a&setname=~a" (web-safep (db-show-name (invoice-show invoice))) (web-safep (db-set-name invoice)))
					  :class "btn-flat waves-effect red-text text-darken-4" "Invoice"))))))))))

(defmacro standard-item-list-table (&key invoice)
  `(with-html-output (*standard-output* nil :indent t)
     
     (:div :class "row"
	   (:script :src "plugins/scrollfire.js")
	   (:div :class "input-field"
		 (:label :for "myInput" :class "black-text" "Search")
		 (:input :type "text" :id "myInput" :onkeyup "myFunction()" :class "black-text"))
	   (:script :src "plugins/search.js")
	   (:script :src "js/modal-add.js")
	   (:ul :id "dropdown2" :class "dropdown-content"
		(dolist (item (invoice-item-list ,invoice))
		  (htm
		   (:li (:a :id (db-item-desc item) :onclick (format nil "javascript:modalSwitch(\"~A\");" (db-item-desc item)) (fmt "~A" (db-item-desc item)))))))
	   (:div :id "itemlist"
		 (:ul :id "myUL"     
		      (dolist (item (invoice-item-list ,invoice))
			(htm
			 (:li
			  (:div :class "col l3 m4 s6"
				(:div :class "card" :data-indicators "true"
				      (if (more-than-one item)
					  (htm (:div :class "card-image"
					    (:div :class "carousel"
						  (:a :class "carousel-item" :href "#one!"
						      (:img :src (item-picture item) :class "responsive-img materialboxed" :data-caption (db-item-desc item))))))
					  (htm (:div :class "card-image"
						     (:img :src (item-picture item) :class "responsive-img materialboxed" :data-caption (db-item-desc item)))))
				      
				      (:div :class "card-content"
					    (:span :class "card-title truncate"
						   (fmt "~A" (escape-string (db-item-desc item))))
					    (:span
					     (:i :class "material-icons" "attach_money") (fmt " ~A &emsp;" (escape-string (db-item-price item)))
					     (:a :href "#" :class "right black-text" (:i :class "material-icons" "all_inclusive") (fmt " ~A" (escape-string (db-item-qty item))))))
				      (:div :class "card-action"
					    (:form :class "form-inline"
						   :action "/removeitem"
						   :method "POST"
						   :onclick (concatenate 'string "removeitem(\"" (db-item-desc item) "\")")
						   
						   :id (db-item-desc item)
						   (:input :type "hidden" :value (db-item-desc item)
							   :name "item" :id "item")
						   (:input :type "hidden" :value (db-item-price item)
							   :name "item-price" :id "item-price")
						   (:input :type "hidden" :value (db-item-qty item)
							   :name "item-quantity" :id "item-quantity")
						   (:input :type "hidden" :value ,invoice
							   :name "invoice" :id "invoice")
						   (if (string= (item-returned-on item) "")
						       (htm (:button :type "submit" :class "red darken-4 btn btn-default btn-sm btn-danger" "Remove"))
						       (htm (:button :type "submit" :class "btn black-text disabled" :disabled "true"
								     (fmt "RTN'D ~A" (escape-string (item-returned-on item))))))
						   (:button :id "modalButton":class "btn-floating waves-effect weaves-light" :data-target "myModal" (:i :class "material-icons" "content_copy")))))))))))
	   
	   (:script "$(document).ready(function(){ $('.carousel').carousel();});")
	   (:script "$(document).ready(function(){ $('.materialboxed').materialbox();});")
	   (:script "$('.carousel.carousel-slider').carousel({fullWidth: false});"))))

(defmacro standard-check-in (&key invoice)
  `(with-html-output (*standard-output* nil :indent t)
     (:div :class "row"
	   (:div :class "input-field col s12 l4 m4"
		 (:label :for "myInput" :class "black-text" "Search")
		 (:input :type "text" :id "myInput" :class "black-text" :onkeyup "myFunction()"))
	   (:ul :id "myUL"
		(dolist (item (remove-returned (invoice-item-list ,invoice)))
		  (htm
		   (:li
		    (:div :class "col s12 m4 l4"
			  (:div :class "card medium blue-grey"
				(:div :class "card-image"
				      (:img :src (item-picture item) :class "materialboxed responsive-img" :data-caption (db-item-desc item))
				      (:span :class "card-title truncate"
					     :style "font-size:20px; text-shadow:-1px 0 black, 0 1px black, 1px 0 black, 0 -1px black;" ;;;Set correct fontsize and give a blackoutline to help readability
					     (fmt "~A" (escape-string (db-item-desc item)))))
				
				(:div :class "card-content"
				      (:span 
				       (:i :class "material-icons" "attach_money") (fmt "~A" (escape-string (db-item-price item)))					    
				       (:a :href "#" :class "right black-text" (:i :class "material-icons" "all_inclusive") (fmt " ~A/~A &emsp;"
																 (escape-string (concat-rtn-qty item))
																 (escape-string (db-item-qty item))))))
				
				(:div :class "card-action"
				      
				      (:form 
				       :action "/check-in-item"
				       :method "POST"
				       :id "check-in-table"
				       
				       (:input :type "hidden" :name "item-price" :id "item-price"
					       :value (db-item-price item))
				       (:input :type "hidden" :name "item-desc" :id "item-desc"
					       :value (db-item-desc item)) 
				       (:input :type "hidden" :name "item-qty" :id "item-qty"
					       :value (db-item-qty item))
				       (:button :type "submit" :class "red darken-4 btn-floating waves-effect waves-light" (:i :class "material-icons" "check_circle"))			        
				       (:a :class "right red darken-4 btn-floating activator" (:i :class "material-icons" "arrow_upward"))))
				
				(:div :class "card-reveal"
				      (:span :class "card-title grey-text text-darken-4"
					     (fmt "~A" (escape-string (db-item-desc item))) (:i :class "material-icons right" "close"))
				      (:form :action (concatenate 'string "/partial-check-in?qty=" (db-item-qty item) "&desc=" (db-item-desc item) "&price=" (db-item-price item))
					     :method "POST"
					     (:p :class "range-field"
						 (:input :id "ranged" :name "ranged" :type "range" :min "0" :max (web-math-subtract (db-item-qty item) (concat-rtn-qty item))
							 :value (web-math-subtract (db-item-qty item) (concat-rtn-qty item)) :oninput "this.form.rangedName.value=this.value"))
					     (:input :class "left" :type "number" :name "rangedName" :min "0" :max (web-math-subtract (db-item-qty item) (concat-rtn-qty item))
						     :value (web-math-subtract (db-item-qty item) (concat-rtn-qty item)) :oninput "this.form.ranged.value=this.value")
					     
					     (:button :type "submit"  :class "right red darken-4 btn-floating" (:i :class "material-icons" "arrow_downward")))))))))))
     
     (:script :src "plugins/search.js")
     (:script :src "plugins/scrollfire.js"))) 



(defmacro standard-picture-upload ()
  `(with-html-output (*standard-output* nil :indent t)
     (:div :class "row"
	   (:div :class "col s12 m12 l12"
		 (:form :action "/displayimagegot"	        
			:method "POST"
			:enctype "multipart/form-data"
			:id "new-picture-upload"
			(:div
			 (:input
			  :class "btn"
			  :multiple "multiple"
			  :id "picture-batch"
			  :name "picture-batch"
			  :type "file"
			  :name "img")
			 (:button :type "submit" :height "10%" :width "10%"  :class "blue-grey btn waves-effect waves-light" "Upload")))))
     (:script :src "plugins/input-upload.js")))

(defmacro standard-pdf-iframe (&key pdf)
  `(with-html-output (*standard-output* nil :indent t)
     (:iframe :id "iframepdf"
	      :height "600"
	      :width "100%"
	      :src (concatenate 'string (web-safep ,pdf) "?name=" (write-to-string (random 100))))))

(defmacro standard-invoice-writing (&key show set contact pic-num)
  `(with-html-output (*standard-output* nil :indent t)
     (:br)
     (:div :class "row"
	   (:div :class "col s12 m6 l6"
		 (:div :class " black-text chip s3 m3 l3"
		       ,show)
		 (:div :class "black-text chip s3 m3 l3"
		       ,set)
		 (:div :class "black-text chip s3 m3 l3"
		       ,contact)
		 (:div :class "black-text chip s3 m3 l3"
		       ,pic-num))
	   (:div :class "col s12 m6 l6"
		 
		 
		 (:a :href "/createpdf" (:button :type "submit" :class "red darken-4 btn waves-effect waves-light"
						 "PDF"))
		 (:a :href "/check-in-set" (:button :type "submit" :class "red darken-4 btn waves-effect waves-light" "Check"))
		 (:a :href "/setthemcookies" (:button :type "submit" :class "red darken-4 btn waves-effect waves-light" "Invoice"))))))

;;;Latex constants---------------------------------------------------

(defparameter header-name "\\dotfill \\textsc{Ordered by:} ")
(defparameter header-set "\\dotfill \\textsc{Set:} ")
(defparameter header-show "\\dotfill \\textsc{Show:} ")
(defparameter date-header "\\\\ {\\bf Pickup Date:} \\today \\dotfill")
(defparameter rental-period "{\\bf {Rental Period:}")
(defparameter begin-table "\\begin{invoiceTable}")
(defparameter end-table "\\end{invoiceTable}")
(defparameter unitrow "\\unitrow{")
(defparameter begin-img-table "\\figureSeriesFloat{}{")
(defparameter end-img-table "}")
(defparameter end-document " \\end{document}")
(defparameter begin-document "\\begin{document}")
;;;Takes an invoice and root-dir
(defparameter tail-conf "
\\par
{\\scriptsize \\begin{singlespace} It is agreed that \\textsc{Lessee} assumes all liability and responsibility for the item(s) listed below.  When item(s) have been accepted by \\textsc{Lessee} (as evidenced by \\textsc{Lessee’s} signature below), and while in the custody and possession of \\textsc{Lessee}, its employees, agents, owners, directors and officers and any other person or entity to whom the item(s) is entrusted by \\textsc{Lessee}, or assigns.  Liability extends to the full replacement cost or repair, in Lessor’s sole discretion.  Further, Lessee assumes full responsibility for any liability arising because of the use of the item(s) during the term of the lease and until item(s) is returned to the custody and control of Lessor (as evidenced by the written acknowledgement by Lessor). Further, to the extent permitted by law, the \\textsc{Lessee} agrees to protect, indemnify, defend and hold harmless Lessor, its directors, officers, agents, shareholders, and employees, against all claims or damages to people or property and costs (including reasonable attorney’s fees), up to Lessor’s pro-rata share of the item(s), arising out of or connected with the operation of \\textsc{Lessee’s} activities with respect to the item(s), including damage caused by inadequate maintenance of the item(s), use of the item(s), or a part thereof, by any customer, any guest, invitee, or by any agent of the \\textsc{Lessee}. Replacement value of all items (unless otherwise noted) equals (10X) first week rate. \\end{singlespace}}

{\\color{red} \\textsc{Signature}}\\hspace{0.5cm} \\makebox[3in]{\\hrulefill} \\hspace{0.5cm} \\textsc{Date}\\hspace{0.5cm} \\makebox[1in]{\\hrulefill} \\\\
\\textsc{Print}\\hspace{1.25cm} \\makebox[3in]{\\hrulefill}
  ")

(defparameter document-conf "\\documentclass{invoice} % Use the custom invoice class (invoice.cls)

\\def \\tab {\\hspace*{3ex}} % Define \\tab to create some horizontal white space

\\usepackage{color}
\\usepackage{courier}
\\usepackage{setspace}
\\usepackage{graphicx}
\\usepackage{pgffor}
\\usepackage{caption}
\\usepackage{expl3}
")

(defparameter heading-conf "\\includegraphics[scale=.33]{caps}
\\hfil{\\huge\\color{red}{\\textsc{Checkout Sheet}}}\\hfil
% \\bigskip\\break % Whitespace
\\break
\\hrule % Horizontal line

675 Metropolitan Parkway SW \\hfill \\emph{Mobile:} (661) 000-0000 \\\\
Suite 5121 \\hfill{ \\emph{Office:} (470) 000-0000} \\\\
% Your address and contact information
Atlanta, Georgia 30310 \\hfill caps@capsga.com
\\\\ \\\\
{\\bf Invoice To:} \\\\ ")

;;;Latex Functions----------------------------------------------
(defun generate-invoice-single-header (invoice func)
  (concatenate 'string  func invoice " "))

(defun generate-latex (invoice root-dir)
  (ensure-directories-exist (concatenate 'string root-dir
					 "pdf/"))
  (let* ((pdfname (concatenate 'string "pdf/" (directory-safe (db-show-name (invoice-show invoice)))
			       "-" (directory-safe (db-set-name invoice))
			       ".tex"))
	 (ironic-tex-name (concatenate 'string "pdf/" (directory-safe (db-show-name (invoice-show invoice)))
				       "-" (directory-safe (db-set-name invoice))
				       ".pdf"))
	 (complete-stream (concatenate 'string root-dir
				       pdfname))
	 (completer-stream (concatenate 'string root-dir
					ironic-tex-name)))
    (with-open-file (s complete-stream :direction :output
		       :if-exists :supersede)
      
      (princ document-conf s)
      (princ (concatenate 'string
			  "\\graphicspath{ {"
			  (invoice-root-dir invoice)
			  "webimg/}{/home/silver/texmf/tex/latex/invoice/}}") s)
      (princ begin-document s)
      (fresh-line s)
      (princ heading-conf s)
      (princ (generate-invoice-single-header
	      (db-show-name (invoice-show invoice)) header-show) s)
      (princ (generate-invoice-single-header
	      (db-set-name invoice) header-set) s)
      (princ (generate-invoice-single-header
	      (db-contact (invoice-show invoice)) header-name) s)
      
      (fresh-line s)
      (fresh-line s)
      
      (princ date-header s)
      (princ rental-period s)
      (fresh-line s)
      (princ tail-conf s)
      (princ begin-table s)
      (fresh-line s)
      (mapc (lambda (b) (princ (format-description b) s)
		    (fresh-line s)) (invoice-item-list invoice))
      (princ end-table s)
      (fresh-line s)
      
      (princ begin-img-table s)
      (mapc (lambda (b) (princ (format-picture b) s)
		    (fresh-line s)
		    (fresh-line s))
	    (picture-list (invoice-item-list invoice)))
      (princ end-img-table s)
      (princ end-document s))

    (trivial-shell:shell-command (concatenate 'string "pdflatex "
					      "-interaction=nonstopmode -output-directory="
					      root-dir "pdf/"
					      " " complete-stream))
    (setf (invoice-pdf-location invoice)
	  (subseq completer-stream 70))))

(defun format-description (b)
  (concatenate 'string
	       unitrow
	       (db-item-desc b)
	       "}{"
	       (db-item-qty b) "}{"
	       (db-item-price  b) "}{"	       
	       (if (and (mito:retrieve-dao 'partial-db :item b)
			(string= "" (item-returned-on b)))
		   (let ((c (first (mito:retrieve-dao 'partial-db :item b))))
		     (concatenate 'string (partial-return-qty c) " RTN'D " (partial-return-date c)))
		   "")
	       "}{"
	       (item-returned-on b)
	       "}"))

;;;This kills the latex
(defun format-notes (notes)
  (concat-notes (mapcar #'(lambda (x) (funcall x :display))
			notes)))

(defun concat-notes (notes)
  (if (null notes)
      '()
      (concatenate 'string (car notes) "\\\\"
		   (concat-notes (cdr notes)))))

(defun subseq-img (b)
  (if (null b)
      ""
      (subseq (item-picture b)
	      (+ (search "webimg" (item-picture b)) 7))))

(defun subseq-img-path (b)
  (if (null b)
      ""
      (subseq b
	      (+ (search "webimg" b) 7))))
;;;I know this is ugly as sin.  I'm tired
;;;I'll fix it in a little bit.

(defun format-picture (b)
  (concatenate 'string
	       "\\figureSeriesRow{ "
	       (picrow-format (first b))
	       (picrow-format (second b))
	       (picrow-format (third b))
	       "}"))

(defun picrow-format (a)
  (if (null a)
      ""
      (concatenate 'string "\\figureSeriesElement{"
		   (db-item-desc a)
		   "}{\\resizebox{0.3\\linewidth}{5cm}{\\includegraphics{"
		   (subseq-img a)
		   "}}}")))

(defun picture-list (b)
  (cond
    ((null b) nil)
    (t
     (cons (list (car b) (cadr b) (third b)) (picture-list (cdddr b))))))

