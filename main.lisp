(defpackage :riley
  (:use :cl
	:cl-who
	:cl-pass
	:hunchentoot
	:parenscript))
;;;All predicted data objects needed

(defclass show ()
  ((contact-name :initarg :contact-name
		 :accessor show-contact-name)
   (phone-number :initarg :phone-number
		 :accessor show-phone-number)
   (order-amount :initarg :order-amount
		 :initform 0
		 :accessor show-order-amount)
   (list-of-invoices :initarg :list-of-invoices
		     :initform '()
		     :accessor list-of-invoices)
   (name :initarg :name
	 :accessor show-name)))

(defun make-show (&key contact-name-t phone-number-t
		    order-amount-t list-of-invoices-t
		    name-t )
  (make-instance 'show
		 :name name-t
		 :contact-name contact-name-t
		 :phone-number phone-number-t
		 :order-amount order-amount-t
		 :list-of-invoices list-of-invoices-t))
(defclass invoice ()
  ((id-num :initarg :id-num
	      :accessor invoice-id-num)
   (set-name :initarg :set-name
	     :accessor invoice-set-name)
   (date-out :initarg :date
	     :initform 0
	     :accessor invoice-date-out)
   (show-name :initarg :show-name
	      :accessor show-name)
   (contact-name :initarg :contact-name
		 :accessor invoice-contact-name)
   (check-in-accounting :initarg :check-in-accounting
			:accessor invoice-check-in-accounting)
   (item-list :initarg :itemlist
	      :accessor invoice-item-list)
   (pdf-location :initarg :pdf-location
		 :accessor invoice-pdf-location)))

(defclass check-in ()
  ((date :initarg :date
	 :accessor date)
   (invoice-refer :initarg :invoice-refer
		  :accessor check-in-invoice-refer)
   (checked-by :initarg :checked-by
	       :accessor check-in-checked-by)
   (show-name :initarg :show-name
	      :accessor check-in-show-name)
   (checks :initarg :checks
	   :accessor check-in-checks)))

(defclass item ()
  ((price :initarg :price
	  :accessor item-price)
   (quantity :initarg :quantity
	     :accessor item-quantity)
   (description :initarg :description
		:accessor item-description)
   (invoices-on :initarg :invoices-on
		:accessor item-invoices-on)
   (shows-on :initarg :shows-on
	     :accessor item-shows-on)))

(defclass user ()
  ((name :initarg :name
	 :accessor user-name)
   (rank :initarg :rank
	 :accessor user-rank)
   (password :initarg :password
	     :accessor user-password)
   (shows-checked :initarg :shows-checked
		  :accessor user-shows-checked)
   (invoice-history :initarg :invoice-history
		    :accessor user-invoice-history)
   (points :initarg :points
	   :accessor user-points)
   (messages :initarg :messages
	     :accessor user-messages)
   (status :initarg :status
	   :accessor user-status)))

(defclass message ()
  ((sender :initarg :sender
	   :accessor message-sender)
   (recipient :initarg :recipient
	      :accessor message-recipient)
   (date :initarg :date
	 :accessor message-date)
   (content :initarg :content
	    :accessor message-content)
   (reply :initarg :content
	  :accessor message-reply)
   (read-date :initarg :read-date
	 :accessor message-read-date)
   (read-on :initarg :read-on
	    :accessor message-read-on)
   (private :initarg :private
	    :accessor message-private)
   (show-name :initarg :show-name
	      :accessor message-show-name)
   (invoice-name :initarg :invoice-name
		 :accessor message-invoice-name)
   (item-name :initarg :item-name
	      :accessor message-item-name)))

(defclass dashboard ()
    ((posts :initarg :posts
	    :accessor dashboard-posts)
     (current-date :initarg :current-date
		   :accessor dashboard-current-date)))
;;; Data
(defvar *current-show-list* '())

(defun find-show-name (name data-lst)
  (find name data-lst :test #'string-equal
	:key #'name))

(defun add-show (show-obj)
  (push (make-instance 'show
		       :name (show-name show-obj)
		       :contact-name (show-contact-name show-obj)
		       :phone-number (show-phone-number show-obj)
		       :order-amount (show-order-amount show-obj)
		       :list-of-invoices (list-of-invoices show-obj))  *current-show-list* ))

(defvar *users* '())

(defun find-user (username)
  (find username *users* :test #'string-equal
	:key #'user-name))

(defun register-user (&key username password)
  (push (make-instance 'user
		       :name username
		       :password password
		       :rank 'user)
	*users*))

;;; Webserver functions and scaffolding
(setf (html-mode) :html5)

(defun start-server (port)
  (start (make-instance 'easy-acceptor :port port)))

(defmacro standard-page ((&key title) &body body)
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html :lang "en"
	    (:head
	     (:link :rel "stylesheet"
		    :type "text/css"
		    :href "css/bootstrap.css")
	     (:title ,title)
	     (:link :type "text/css"
		    :rel "stylesheet"
		    :href "css/bootstrap-theme.css")
	     (:link :type "text/css"
		    :rel "stylesheet"
		    :href "css/corrections.css"))
	    (:body
	     
		  ,@body))))
(define-easy-handler (home :uri "/") ()
  (standard-page (:title "RILEY Inventory System")
    (:a :href "/createshow" "Create Show")))
(define-easy-handler (orc :uri "/orc") ()
  (standard-page (:title "WHEW LAD")
    (:h1 "If you got here you can stop for tonight.")))
(define-easy-handler (check-login :uri "/check-login") ()
  (let
      ((username (hunchentoot:post-parameter "username"))
       (password (hunchentoot:post-parameter "password")))
    (if (cl-pass:check-password password (user-password (find-user username)))
	(redirect "/orc")
	(redirect "/epicfail"))))
(define-easy-handler (createshow :uri "/createshow") ()
  (standard-page (:title "CREATE SHOW")
    (:div :id "form"
	  (:form :action "/addshow"
		 :method "POST"
		 :id "commentform"
		 (:input :type "text"
			 :placeholder "Show Name"
			 :name "name"
			 :id "name")
		 (:br)
		 (:input :type "text"
			 :placeholder "Contact Name"
			 :name "contact-name"
			 :id "contact-name")
		 (:br)
		 (:input :type "text"
			 :placeholder "Phone Number"
			 :name "phone-number"
			 :id "phone-number")
		 (:br)
		 (:button :type "submit"
			  :class "btn btn-primary"
			  "Create Show")))))
(define-easy-handler (addshow :uri "/addshow") ()
  (add-show (make-instance 'show
			   :name (hunchentoot:post-parameter "name")
			   :contact-name (hunchentoot:post-parameter "contact-name")
			   :phone-number (hunchentoot:post-parameter "phone-number")))
  (redirect "/"))
(define-easy-handler (adduser :uri "/adduser") ()
  (let ((username (hunchentoot:post-parameter "username"))
	(password (hunchentoot:post-parameter "password"))
	(password-repeat (hunchentoot:post-parameter "password-repeat")))
    (if (and (string= password password-repeat)
	     (not (find-user username)))
	(and (register-user :username username
			    :password (cl-pass:hash password))
	     (redirect "/"))
	(redirect "/fail"))))
(define-easy-handler (login :uri "/login") ()
  (standard-page (:title "Login")
    (:div :id "landing"
	  (:div :class "panel panel-default"
	  :id "welcome-panel"
	  (:div :class "panel-heading"
		(:h1 "Welcome to the RILEY Inventory System"))
	  (:div :class "panel-body"
		(:div :class "row"
		(:div :class "col-md-6"
		     
		(:div :id "form"
	  (:form :action "/adduser"
		 :method "POST"
		 :id "commentform"
		 (:input :type "text"
			 :placeholder "Username"
			 :name "username"
			 :id "username")
		 (:br)
		 (:input :type "text"
			 :placeholder "Password"
			 :name "password"
			 :id "password")
		 (:br)
		 (:input :type "text"
			 :placeholder "Repeat Password"
			 :name "password-repeat"
			 :id "password-repeat")
		 (:br)
		 (:button :type "submit"
			  :class "btn btn-primary"
			  "Create Account"))))
    (:br)
    (:div :class "col-md-6"
	  (:div :id "form"
	  (:form :action "/check-login"
		 :method "POST"
		 :id "commentform"
		 (:input :type "text"
			 :placeholder "Username"
			 :name "username"
			 :id "username")
		 (:br)
		 (:input :type "text"
			 :placeholder "Password"
			 :name "password"
			 :id "password")
		 (:br)
		 (:button :type "submit"
			  :class "btn btn-primary"
			  "Login"))))))))))
