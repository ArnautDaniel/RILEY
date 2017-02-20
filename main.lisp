;;;Initial Declarations
(defpackage :riley
  (:use :cl
	:cl-who
	:cl-pass
	:hunchentoot
	:parenscript
	:trivial-shell))

(defvar *current-message-list* '())
(defvar *global-invoice-list* '())
(defvar *users* '())
(defvar *global-invoice-id* 0)
(defvar *current-show-list* '())

;;;All predicted data objects neededers

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
   (root-dir :initarg :root-dir
	     :accessor invoice-root-dir)
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
   (picture    :initarg :pictures
		:accessor item-picture)
   (shows-on :initarg :shows-on
	     :accessor item-shows-on)
   (returned-on :initarg :returned-on
		:accessor item-returned-on)))

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
   (reply :initarg :reply
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

;;; Helper functions for classes

;;;Create a show, doesn't appear to be used at the moment.
(defun make-show (&key contact-name-t phone-number-t
		    order-amount-t list-of-invoices-t
		    name-t )
  (make-instance 'show
		 :name name-t
		 :contact-name contact-name-t
		 :phone-number phone-number-t
		 :order-amount order-amount-t
		 :list-of-invoices list-of-invoices-t))

;;;Using a string NAME search for SHOW-NAME in DATA-LST
(defun find-show-name (name data-lst)
  (find name data-lst :test #'string-equal
	:key #'show-name))

;;;Provide the function a show-obj and push a new show onto the *CURRENT-SHOW-LIST*
(defun add-show (show-obj)
  (push (make-instance 'show
		       :name (show-name show-obj)
		       :contact-name (show-contact-name show-obj)
		       :phone-number (show-phone-number show-obj)
		       :order-amount (show-order-amount show-obj)
		       :list-of-invoices (list-of-invoices show-obj))  *current-show-list* ))

;;;Search *USERS* for USERNAME
(defun find-user (username)
  (find username *users* :test #'string-equal
	:key #'user-name))

;;;Create a user from a USERNAME and PASSWORD and push onto *USERS*
(defun register-user (&key username password)
  (push (make-instance 'user
		       :name username
		       :password password
		       :rank "user")
	*users*))

;;;Find all messages recieved by GLOBAL
(defun find-global-messages ()
  (remove-if-not (lambda (x)
		   (string= "global" (message-recipient x)))
		 *current-message-list*))

;;;Create a message from any of the valid keys and push it onto the *CURRENT-MESSAGE-LIST*
(defun register-message (&key sender recipient
			   date content
			   reply read-date
			   read-on private
			   show-name
			   invoice-name
			   item-name)
  (push (make-instance 'message
		       :sender sender
		       :recipient recipient
		       :reply reply
		       :date date
		       :content content
		       :read-date read-date
		       :read-on read-on
		       :private private
		       :show-name show-name
		       :invoice-name invoice-name
		       :item-name item-name)
	*current-message-list*))

;;;Find an invoice-object connected to a message
;;;Used for generating buttons on the dashboard
(defun find-invoice-from-message (mess)
  (let ((setname (first (message-invoice-name mess)))
	(showname (second (message-invoice-name mess))))
    (first (remove-if-not (lambda (x)
		     (and (string= setname (invoice-set-name x))
			  (string= showname (show-name x))))
			  *global-invoice-list*))))

;;;If an item already has a return date remove it from the check-in list
;;;before passing the data to the check-in page
(defun remove-returned (inv-itemlist)
  (remove-if-not (lambda (x)
	       (string= "" (item-returned-on x)))
	     inv-itemlist))

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

;;;Search the *GLOBAL-INVOICE-LIST* for INVOICENAME
(defun find-invoice (invoicename)
  (find invoicename *global-invoice-list* :test #'string-equal
	:key #'show-name))

;;;Create an invoice and and push it onto the *GLOBAL-INVOICE-LIST*
(defun register-invoice (&key id-num
			   set-name
			   show-name
			   contact-name
			   root-dir
			   pdf-location)
  (push (make-instance 'invoice
		       :id-num id-num
		       :set-name set-name
		       :show-name show-name
		       :contact-name contact-name
		       :itemlist '()
		       :root-dir root-dir
		       :pdf-location pdf-location) *global-invoice-list*))

;;;USED ALOT.  Lets you find an invoice on the global invoice list
;;;from the *showname*-*setname* form of a cookie string.
(defun find-invoice-from-cookie (invoice-string)
  (if (not (string= "none" invoice-string))
  (let* ((magic-number (search "-" invoice-string))
	 (showname (subseq invoice-string 0 magic-number))
	 (setname (subseq invoice-string (+ magic-number 1)))
	 (temp-invoice (make-instance 'invoice :show-name showname
				      :set-name setname)))
    (find-invoice-from-invoice temp-invoice))))

;;;Helper function for moving uploaded pictures to the correct directory
;;;Needs to delete the pictures after and be expanded.
(defun move-image-to-invoice-dir (img-data)
  (let* ((current-invoice (find-invoice-from-cookie (cookie-in "current-invoice")))
	 (invoice-location (invoice-root-dir current-invoice))
	 (temp-image-directory "/tmp/")
	 (image-name (third img-data)))
    (cl-fad:copy-file (make-pathname :directory temp-image-directory
				     :name image-name)
		      (make-pathname :directory invoice-location
				     :name image-name))))

;;; Webserver functions and scaffolding
;;;Set HTML5 preamble
(setf (html-mode) :html5)

(defun start-server (port)
  (start (make-instance 'easy-acceptor :port port)))

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
		    :href "bootstrap/css/bootstrap.min.css")
	     (:link :type "text/css"
		    :rel "stylesheet"
		    :href "plugins/font-awesome/css/font-awesome.min.css")
	     (:link :type "text/css"
		    :rel "stylesheet"
		    :href "plugins/ionicons/css/ionicons.min.css")
	     (:link :type "text/css"
		    :rel "stylesheet"
		    :href "dist/css/AdminLTE.min.css")
	     (:link :type "text/css"
		    :rel "stylesheet"
		    :href "dist/css/skins/_all-skins.min.css")
	     (:title ,title)
	    (:script :src "plugins/jQuery/jquery-3.1.1.min.js")
	    (:script :src "bootstrap/js/bootstrap.min.js")
	    (:script :src "dist/js/app.min.js")
	    (:script :src "plugins/slimScroll/jquery.slimscroll.min.js")
	    (:script :src "plugins/fastclick/fastclick.js")
	    (:script :src "plugins/datatables/jquery.dataTables.min.js")
	    (:script :src "plugins/datatables/dataTables.bootstrap.min.js")
	    (:link :rel "stylesheet" :type "text/css"
		   :href "plugins/custom/fancybox/source/jquery.fancybox.css")
	    (:script :src "plugins/custom/fancybox/source/jquery.fancybox.pack.js")
	    (:script :src "plugins/custom/fancybox/lib/jquery.mousewheel.pack.js")
	    (:script :src "plugins/custom/fancybox/source/helpers/jquery.fancybox-media.js")
	    (:link :rel "stylesheet" :type "text/css"
		   :href "plugins/custom/fancybox/source/helpers/jquery.fancybox-thumbs.css")
	    (:script :src "plugins/custom/fancybox/source/helpers/jquery.fancybox-thumbs.js")
	    (:script "$(document).ready(function() {
	$(\".fancybox\").fancybox({
		openEffect	: 'none',
		closeEffect	: 'none'
	});
});"))
	    
	    (:body :class "hold-transition skin-blue fixed"
	        
		   (:div :class "wrapper"
			 ,@navbar)
		   (:div :class "content-wrapper"
			 (:section :class "content"
	     ,@body))))))

(defmacro standard-login ()
  `(with-html-output (*standard-output* nil :indent t)
      
 (:div :class "register-page"
	  (:div :class "register-box"
		
	  (:div :class "register-logo"
		(:b "RILEY Inventory System"))
	  (:div :class "register-box-body"
	  (:p :class "login-box-msg" "Register a new account")
	  (:form :action "/adduser"
		 :method "POST"
		 :id "commentform"
		 (:div :class "form-group has-feedback"
		 (:span :class "form-control-feedback"
			:id "basic-addon1" "@")
		 (:input :type "text"
			 :class "form-control"
			 :placeholder "Username"
			 :name "username"
			 :aria-describedby "basic-addon1"
			 :id "username"))
		 (:div :class "form-group has-feedback"
		       (:span :class "form-control-feedback"
			      :id "basic-addon1" "$")      
		 (:input :type "password"
			 :placeholder "Password"
			 :name "password"
			 :class "form-control"
			 :aria-describedby "basic-addon1"
			 :id "password"))
		 
		 (:div :class "form-group has-feedback"
		       (:span :class "form-control-feedback"
			      :id "basic-addon1" "$")
		       (:input :type "password"
			       :class "form-control"
			       :aria-describedby "basic-addon1"
			 :placeholder "Repeat Password"
			 :name "password-repeat"
			 :id "password-repeat"))
	        
	        
		  (:button :type "submit"
			  :class "btn btn-primary btn-block btn-flat"
			  "Create Account")))
    
	  (:div :class "register-box-body"
        
	  (:form :action "/check-login"
		 :method "POST"
		 :id "commentform"
		 (:div :class "form-group has-feedback"
		       (:span :class "form-control-feedback"
			      :id "basic-addon1" "@")
		       (:input :type "text"
			       :class "form-control"
			       :aria-describedby "basic-addon1"
			 :placeholder "Username"
			 :name "username"
			 :id "username"))
		 (:div :class "form-group has-feedback"
		      
		       (:input :type "password"
			       :class "form-control"
			       :aria-describedby "basic-addon1"
			 :placeholder "Password"
			 :name "password"
			 :id "password")
		        (:span :class "form-control-feedback"
			      :id "basic-addon1" "$"))
	        
	         (:button :type "submit"
			  :class "btn btn-primary btn-flat btn-block"
			  "Login")))))))

(defmacro standard-navbar ()
  `(with-html-output (*standard-output* nil :indent t)
     (:nav :class "navbar navbar-default" :role "navigation"
	   (:div :class "container-fluid"
		 (:div :class "navbar-header"
		       (:button :type "button"
				:class "navbar-toggle collapsed"
				:data-toggle "collapsed"
				:data-target "#collapsible"
				(:span :class "sr-only" "Toggle navigation")
				(:span :class "icon-bar")
				(:span :class "icon-bar")))
		 (:div :class "collapse navbar-collapse"
		       :id "collapsible"
		       (:ul :class "nav navbar-nav"
			    (:li (:a :href "/dashboard" "Timeline"))
			    (:li (:a :href (concatenate 'string "/profile/" (cookie-in "current-user"))  "Profile"))
			    (:li (:a :href "/write-order" "Write Order"))
			    (:li (:a :href "/check-in-order" "Check-in Order")))
		       (:ul :class "nav navbar-nav navbar-right"
			    (:li (:a :href "/signout" "Sign out"))
			    (:li (:a :href "/checkinlist" "Check In"))))))))

(defmacro test-navbar ()
  `(with-html-output (*standard-output* nil :indent t)
     (:aside :class "main-sidebar"
	     (:section :class "sidebar"
		       (:div :class "user-panel"
			     (:div :class "pull-left image"
				   (:img :class "img-circle" :src "img/littlec.jpg"))
			     (:div :class "pull-left info" (:p "CAPS USER")
			     (:a :href "#" (:i :class "fa fa-circle text-success") "Online")))
		       
		       (:ul :class "sidebar-menu"
			    (:li :class "header" "Essential")
			    (:li :class "treeview active"
				 (:a :href "/dashboard" (:i :class "fa fa-link")
							    (:span "Dashboard")))
			    (:li :class "treeview active"
				 (:a :href "/write-order" (:i :class "fa fa-circle")
				     (:span "Write Order")))
			    (:li :class "treeview active"
				 (:a :href "/checkinlist" (:i :class "fa fa-check-square")
				     (:span "Check-in Order")))
			    (:li :class "treeview active"
				 (:a :href "/signout" (:i :class "fa fa-sign-out")
				     (:span "Sign Out"))))))))

(defmacro standard-dashboard (&key messages)
  `(with-html-output (*standard-output* nil :indent t)
     (:div :class "panel panel-default"
	   (:div :class "panel-heading user-brief"
		 (:h1 "Dashboard")
		 (:form :role "form"
			:action "/addmessage"
			:method "post"
			:class "form-inline"
			(:div :class "form-group"
			      (:label :class "sr-only" :for "tweet" "Say Something")
			      (:input :type "text" :class "form-control"
				      :id "message" :name "message"
				      :placeholder "Post a message")
			      (:button :type "Submit Message" :class "btn btn-default btn-info" "Message")))))
	   
		 ,messages))

(defmacro standard-global-messages ()
  `(with-html-output (*standard-output* nil :indent t)
   
     (:ul :class "timeline"
	  (:li :class "time-label"
	       (:span :class "bg-red"
     (dolist (messages (find-global-messages)) 
       (htm
	(:li (:i :class "fa fa-envelope bg-blue")
	(:div :class "timeline-item"
	   (:div :class "timeline-header"
		 (:a :class "pull-left" :href (concatenate 'string "/profile/" (message-sender messages))))
	   (:div :class "timeline-body"
		 (:h4 :class "media-heading"
		      (:a :href (concatenate 'string "/profile/" (message-sender messages))
			  (fmt "~A" (escape-string (message-sender messages)))))
		 
		 (fmt "~A" (escape-string (message-content messages))))
	   (if (find-invoice-from-message messages)
	       (htm
		(:div :class "timeline-footer"
		      (:form :action "/pre-set-cookies"
			     :method "POST"
			     (:input :type "hidden" :name "showname" :value (show-name (find-invoice-from-message messages)))
			     (:input :type "hidden" :name "setname" :value (invoice-set-name (find-invoice-from-message messages)))
			     (:input :type "hidden" :name "contact" :value (invoice-contact-name (find-invoice-from-message messages)))
			     (:button :type "submit" :class "btn btn-default btn-sm btn-flat btn-info" "Write Order"))))))))))))))

(defmacro standard-item-writeup (&key image full-images invoice-data)
  `(with-html-output (*standard-output* nil :indent t)
     (:div :class "panel panel-default login-panel"
	   (:div :class "box"
		 
		 (:div :class "box-header"
		       (:center (:img :src ,image :class "img-responsive"
				      :width "40%" :height "40%")))
		 (:div :class  "box-body"
		 (:form 
			:action "/additem"
			:method "POST"
			:id "New-item"
			(:div :class "form-group has-feedback "
			      (:label :for "inputDesc" "Description")
			      (:span :class "form-control-feedback")
			      (:input :type "text" :class "form-control"
				      :id "input-item-description" :placeholder "Item Description"
				      :name "input-item-description"))
			(:div :class "form-group has-feedback"
			      (:span :class "form-control-feedback")
			      (:label :for "inputPrice" "Price")
			      (:input :type "text" :class "form-control"
				      :id "input-item-price" :placeholder "Item Price"
				      :name "input-item-price"))
			(:div :class "form-group has-feedback"
			      (:span :class "form-control-feedback")
			      (:label :for "inputQty" "Quantity")
			      (:input :type "text" :class "form-control"
				      :id "input-item-qty" :placeholder "Item Quantity"
				      :name "input-item-qty"))
			(:input :type "hidden" :id "image-data" :name "image-data" :value ,image)
	        
			(:button :type "submit" :class "btn btn-info btn-default btn-flat btn-block" "Add item")
			(:button :type "button" :class "btn btn-warning btn-default btn-flat btn-block"
				 :data-toggle "modal" :data-target "#myModal" "Switch Item or Add Multiple Pictures"))
		 (:div :id "myModal" :class "modal fade" :role "dialog"
		       (:div :class "modal-dialog"
			     (:div :class "modal-content"
				   (:div :class "modal-header"
					 (:button :type "button" :class "close" :data-dismiss "modal" "Close")
					 (:h4 :class "modal-title" "Switch or Add multiple pictures")
					 (:div :class "modal-body"
					        (dolist (img ,full-images)
						  (htm 
						   (:form :action "/swapitemposition"
							  :method "POST"
							  (:input :type "hidden" :id "image-name"
								  :name "image-name" :value img)
							  (:input :type "image" :id "saveform" :class "img-responsive"
								  :width "40%" :height "40%" :src img
								  :alt "Submit Form"))))))))))))))
		      				      
(defmacro standard-order-intro ()
  `(with-html-output (*standard-output* nil :indent t)
     (:div :class "panel panel-default"
	   (:div :class "panel-body" (:form :class "form-inline"
	    :action "/createInvoice"
	    :method "POST"
	    :id "New-invoice-form"
	    (:div :class "form-group"
		  (:label :for "inputShow" "Show")
		  (:input :type "text" :class "form-control" :id "inputShowname"
			  :placeholder "Showname" :name "inputShowname"))
	    (:div :class "form-group"
		  (:label :for "inputSet" "Set")
		  (:input :type "text" :class "form-control" :id "inputSetname"
			  :placeholder "Set Name" :name "inputSetname"))
	    (:div :class "form-group"
		  (:label :for "inputContact" "Contact Name")
		  (:input :type "text" :class "form-control" :id "inputContact"
			  :placeholder "Contact Name" :name "inputContact"))
	    (:button :type "submit" :class "btn btn-default" "Write Show Order"))))))

(defmacro standard-picture-table (&key image-list)
  `(with-html-output (*standard-output* nil :indent t)
     (:div :class "box box-default collapsed-box"
	   (:div :class "box-header with-border"
		 "List of pictures to be written up"
		 (:div :class "box-tools pull-right"
		       (:button :class "btn btn-box-tool" :data-widget "collapse"
				(:i :class "fa fa-plus"))))
	   (:div :class "box-body"
		    (dolist (image ,image-list)
		      (htm (:div :class "col-md-3 col-sm-4 col-xs-6"
				 (:img :src image :class "img-responsive"
				       :width "40%" :height "40%"))))))))

(defmacro standard-check-in-showlist ()
  `(with-html-output (*standard-output* nil :indent t)
     (:div :class "container panel panel-default"
	   (:table :class "table table-bordered table-striped"
		   (:thead
		    (:tr
		     (:th "Show Name")
		     (:th "Set Name")
		     (:th "Contact")
		     (:th "Check In")))
		   (:tbody
		    (dolist (invoice *global-invoice-list*)
		      (htm
		       (:tr
			(:td (fmt "~A" (escape-string (show-name invoice))))
			(:td (fmt "~A" (escape-string (invoice-set-name invoice))))
			(:td (fmt "~A" (escape-string (invoice-contact-name invoice))))
			(:td (:form :class "form-inline"
				    :action "/check-in-set-pre"
				    :method "POST"
				    :id "item-table"
				    (:input :type "hidden" :value (invoice-set-name invoice)
					    :name "setname" :id "setname")
				    (:input :type "hidden" :value (show-name invoice)
					    :name "showname" :id "showname")
				    (:button :type "submit" :class "btn btn-default btn-sm btn-info"
					     "Check In")))))))))))

(defmacro standard-item-list-table (&key invoice)
  `(with-html-output (*standard-output* nil :indent t)
     (:div :class "box"
	   (:div :class "box-header")
		 
     (:div :class "box-body"
	  (:table :id "itemlist" :class "table table-bordered table-striped"
		 (:thead
		 
		  (:tr
		   (:th "Description")
		   (:th "Price")
		   (:th "Quantity")
		   (:th "Remove?")))
		 (:tbody
		  (dolist (item (invoice-item-list ,invoice))
		    (htm
		     (:tr
		      
		      (:td (fmt "~A" (escape-string (item-description item))))
		      (:td (fmt "~A" (escape-string (item-price item))))
		      (:td (fmt "~A" (escape-string (item-quantity item))))
		      (:td (:form :class "form-inline"
			     :action "/removeitem"
			     :method "POST"
			     :id "item-table"
			     (:input :type "hidden" :value (item-description item)
				     :name "item" :id "item")
			     (:input :type "hidden" :value (item-price item)
				     :name "item-price" :id "item-price")
			     (:input :type "hidden" :value (item-quantity item)
				     :name "item-quantity" :id "item-quantity")
			     (:input :type "hidden" :value ,invoice
				     :name "invoice" :id "invoice")
			     (:button :type "submit" :class "btn btn-default btn-sm btn-danger" "Remove"))))))))
	  (:script "$(\"#itemlist\").DataTable();")))))
	  
     

(defmacro standard-check-in (&key invoice)
  `(with-html-output (*standard-output* nil :indent t)
     (:div :class "box"
	   (:div :class "box-body"
		 (:table :id "checkinlist" :class "table table-bordered table-striped"
			 (:thead
			  (:tr
			   (:th "Picture")
			   (:th "Description")
			   (:th "QTY")
			   (:th "Price")
			   (:th "Check?")))
			 (:tbody
			  (dolist (item (remove-returned (invoice-item-list ,invoice)))
			    (htm
		
			     (:tr
			      (:td (:a :class "fancybox"
				       :id "fancybox"
				       :rel "gallery1"
				       :href (item-picture item)
				       (:img :src (item-picture item) :class "img-responsive"
					     :width "40%" :height "40%")))
					       
			      (:td (fmt "~A" (escape-string (item-description item))))
			      (:td (fmt "~A" (escape-string (item-quantity item))))
			      (:td (fmt "~A" (escape-string (item-price item))))
			      (:td
			       (:form :class "form-inline"
					  :action "/check-in-item"
					  :method "POST"
					  :id "check-in-table"
					 
					  (:input :type "hidden" :name "item-price" :id "item-price"
						  :value (item-price item))
					  (:input :type "hidden" :name "item-desc" :id "item-desc"
						  :value (item-description item))
					  (:input :type "hidden" :name "item-qty" :id "item-qty"
						  :value (item-quantity item))
					  (:button :type "submit" :class "btn btn-default btn-sm btn-danger" "Check in"))))))))
		 (:script "$(\"#checkinlist\").DataTable();")))))
		 
		 
(defmacro standard-picture-upload ()
  `(with-html-output (*standard-output* nil :indent t)
     (:div :class "box box-success"
	   (:div :class "box-header"
		 (:h3 "Upload pictures to this order:"))
	   (:div :class "box-body"
		 (:form :action "/displayimagegot"
			:class "form-inline"
			:method "POST"
			:enctype "multipart/form-data"
			:id "new-picture-upload"
			(:div :class "form-group"
			      (:input
			       :multiple "multiple"
			       :id "picture-batch"
			       :name "picture-batch"
			       :type "file"
			       :name "img")
			      (:input :type "submit")))))))

(defmacro standard-pdf-iframe (&key pdf)
  `(with-html-output (*standard-output* nil :indent t)
     (:div :class "box box-default collapsed-box"
	   (:div :class "box-header with-border"
		 "PDF"
	   (:div :class "box-tools pull-right"
		 (:button :class "btn btn-box-tool" :data-widget "collapse" (:i :class "fa fa-plus"))))
	   (:div :class "box-body"
	   (:iframe :id "iframepdf"
		    :height "600"
		    :width "100%"
		    :src ,pdf)))))

(defmacro standard-invoice-writing (&key show set contact pic-num)
  `(with-html-output (*standard-output* nil :indent t)
     (:div :class "box box-info"
	   (:div :class "box-body"
		 (:ul :class "list-group"
		      (:li :class "list-group-item list-group-item-success" ,show)
		      (:li :class "list-group-item list-group-item-danger" ,set)
		      (:li :class "list-group-item list-group-item-success" ,contact)
		      (:li :class "list-group-item list-group-item-danger" ,pic-num))
		 (:a :href "/createpdf" (:button :type "submit" :class "btn btn-default btn-sm btn-info"
						 "Create PDF"))
		 (:a :href "/check-in-set" (:button :type "submit" :class "btn btn-default btn-sm btn-info" "Check-in Items"))
		 (:a :href "/setthemcookies" (:button :type "submit" :class "btn btn-default btn-sm btn-info" "Invoice homepage"))))))

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
				 showname
				"/" setname "/")))
    (ensure-directories-exist root-dir)
    (register-invoice :id-num (+ *global-invoice-id* 1)
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
		    :invoice-name (list setname showname contact *global-invoice-id*)))
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
							      (string= (item-quantity x) item-qty))) (invoice-item-list invoice)))))

    (setf  (item-returned-on item)
	   "1/17")
    (createpdf2)
  (redirect "/check-in-set")))

;;;This function has some leftover cruft that needs to be refactored.
(define-easy-handler (displayimagegot :uri "/displayimagegot") ()
  (let ((whatever (loop for post-parameter in (hunchentoot:post-parameters*)
		     if (equal (car post-parameter) "picture-batch")
				    collect post-parameter)))
    (standard-page (:title "Picture Batch")
      (standard-navbar)
      (mapc #'(lambda (x)
		(format t "~A ~A ~A ~A <br>"  (first x)
			(second x)
			(third x)
			(fourth x))
		(rename-file (second x)
			     (concatenate 'string "/tmp/"
					  (third x)))
		(move-image-to-invoice-dir x))
		     
	    whatever)))
  (redirect "/setthemcookies"))

;;;Current solution to being able to refresh pages without putting data in the URI
;;;May need to start putting data in the URI but the function works pretty decently already.
(define-easy-handler (pre-set-cookies :uri "/pre-set-cookies") ()
  (set-cookie "current-invoice" :value (concatenate 'string (hunchentoot:post-parameter "showname")
						    "-"
						    (hunchentoot:post-parameter "setname")))
  (redirect "/setthemcookies"))

;;;Adds an item from the additem panel on the write order page
;;;Absolutely needs javascript to test for the following conditions
;;;Empty input boxes;  Invalid input; and removing the pdf glitch
(define-easy-handler (add-item :uri "/additem") ()
  (let* ((invoice (find-invoice-from-cookie (cookie-in "current-invoice")))
	 (description (hunchentoot:post-parameter "input-item-description"))
	 (price (hunchentoot:post-parameter "input-item-price"))
	 (qty (hunchentoot:post-parameter "input-item-qty"))
	 (image (hunchentoot:post-parameter "image-data")))
    (push (make-instance 'item :description description
			 :quantity qty
			 :price price
			 :pictures image
			 :returned-on "")
	  (invoice-item-list invoice)))
  (redirect "/setthemcookies"))

;;;Removes an item from the invoice.  Searches very basically.  Should probably have an item id
(define-easy-handler (remove-item :uri "/removeitem") ()
  (let* ((invoice (find-invoice-from-cookie (cookie-in "current-invoice")))
	 (item (hunchentoot:post-parameter "item"))
	 (item-price (hunchentoot:post-parameter "item-price"))
	 (item-quantity (hunchentoot:post-parameter "item-quantity")))
        (setf (invoice-item-list invoice) (remove-if #'(lambda (x)
							 (and (string= (item-description x) item)
							      (string= (item-price x) item-price)
							      (string= (item-quantity x) item-quantity)))
						        
						     (invoice-item-list invoice))))
  (redirect "/setthemcookies"))

;;;Standard check in page that displays a table of shows with invoices
(define-easy-handler (checkinlist :uri "/checkinlist") ()
  (standard-page (:title "Check in list")
    (:navbar (test-navbar))
    (standard-check-in-showlist)))

;;;Required to get around the refresh problem.  Will need to expand solution
(define-easy-handler (check-in-set-pre :uri "/check-in-set-pre") ()
    (let* ((showname (hunchentoot:post-parameter "showname"))
	   (setname (hunchentoot:post-parameter "setname")))
      (set-cookie "current-invoice" :value (concatenate 'string showname "-" setname))
      (redirect "/check-in-set")))

;;;Presents a table of items that have not been checked in yet
(define-easy-handler (checkinset :uri "/check-in-set") ()
  (let* ((invoice (find-invoice-from-cookie (cookie-in "current-invoice")))
	(showname (show-name invoice))
	(setname (invoice-set-name invoice)))
      (standard-page (:title (concatenate 'string "Check in: " showname "-" setname))
        (:navbar (test-navbar))
	(standard-invoice-writing)
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
	 (showname (show-name invoice))
	 (setname (invoice-set-name invoice))
	 (contact (invoice-contact-name invoice))
	 (change-pic-cookie (cookie-in "current-picture"))
	 (images (prepare-for-table (cl-fad:list-directory (invoice-root-dir invoice))))
	 (images-filtered (sort-item-list (filter-already-in-itemlist images invoice) change-pic-cookie)))
    
  (standard-page (:title "Order Writeup")
  
    (:navbar (test-navbar))
    (standard-picture-upload)
    (standard-invoice-writing :show  (fmt "Showname: ~A" (escape-string showname))
			      :set (fmt "Setname: ~A" (escape-string setname))
			      :contact (fmt "Contact: ~A" (escape-string contact))
			      :pic-num (fmt "~A" (escape-string (count-pics-from-invoice (concatenate 'string showname
    									      "-" setname)))))
    (standard-item-writeup :image (first images-filtered)
			   :full-images (rest images-filtered)
			   :invoice-data invoice)
    (standard-item-list-table :invoice invoice)
    (standard-pdf-iframe :pdf (invoice-pdf-location invoice))
    (standard-picture-table :image-list (rest images-filtered)))))


(defun sort-item-list (itemlist pic-need)
  (if (null pic-need)
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
		    (subseq (namestring string-path-image) 70)))
		(remove-if #'(lambda (x)
			       (cl-fad:directory-exists-p x))
			   fad-list)))

;;;Expects an invoice with the string showname-setname
(defun count-pics-from-invoice (inv)
  (if (string= "none" inv)
	"0"
	(let* ((invoice (find-invoice-from-cookie inv))
	       (invoice-pathname (invoice-root-dir invoice)))
	  (concatenate 'string "There are " (write-to-string (filtered-length (cl-fad:list-directory invoice-pathname)))
		       " pictures on this order"))))

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

;;;For the love of McCarthy please turn this into a macro already
(define-easy-handler (login :uri "/login") ()
  (standard-page (:title "Login")
   (standard-login)))


;;;Latex constants---------------------------------------------------

(defparameter header-name "\\dotfill \\textsc{Ordered by:} ")
(defparameter header-set "\\dotfill \\textsc{Set:} ")
(defparameter header-show "\\dotfill \\textsc{Show:} ")
(defparameter date-header "\\tab {\\bf Pickup Date:} \\today \\dotfill")
(defparameter rental-period "{\\bf {Rental Period:}")
(defparameter begin-table "\\begin{invoiceTable}")
(defparameter end-table "\\end{invoiceTable}")
(defparameter unitrow "\\unitrow{")

;;;Takes an invoice and root-dir

(defparameter tail-conf "
\\par
{\\scriptsize \\begin{singlespace} It is agreed that \\textsc{Lessee} assumes all liability and responsibility for the item(s) listed above.  When item(s) have been accepted by \\textsc{Lessee} (as evidenced by \\textsc{Lessee’s} signature below), and while in the custody and possession of \\textsc{Lessee}, its employees, agents, owners, directors and officers and any other person or entity to whom the item(s) is entrusted by \\textsc{Lessee}, or assigns.  Liability extends to the full replacement cost or repair, in Lessor’s sole discretion.  Further, Lessee assumes full responsibility for any liability arising because of the use of the item(s) during the term of the lease and until item(s) is returned to the custody and control of Lessor (as evidenced by the written acknowledgement by Lessor). Further, to the extent permitted by law, the \\textsc{Lessee} agrees to protect, indemnify, defend and hold harmless Lessor, its directors, officers, agents, shareholders, and employees, against all claims or damages to people or property and costs (including reasonable attorney’s fees), up to Lessor’s pro-rata share of the item(s), arising out of or connected with the operation of \\textsc{Lessee’s} activities with respect to the item(s), including damage caused by inadequate maintenance of the item(s), use of the item(s), or a part thereof, by any customer, any guest, invitee, or by any agent of the \\textsc{Lessee}. Replacement value of all items (unless otherwise noted) equals (10X) first week rate. \\end{singlespace}}

{\\color{red} \\textsc{Signature}}\\hspace{0.5cm} \\makebox[3in]{\\hrulefill} \\hspace{0.5cm} \\textsc{Date}\\hspace{0.5cm} \\makebox[1in]{\\hrulefill} \\\\
\\textsc{Print}\\hspace{1.25cm} \\makebox[3in]{\\hrulefill}
\\newpage
\\end{document}")


(defparameter document-conf "\\documentclass{invoice} % Use the custom invoice class (invoice.cls)

\\def \\tab {\\hspace*{3ex}} % Define \\tab to create some horizontal white space

\\usepackage{color}
\\usepackage{courier}
\\usepackage{setspace}
\\usepackage{graphicx}
\\usepackage{subfig}
\\usepackage{pgffor}
\\usepackage{caption}
\\usepackage{expl3}


\\begin{document}")

(defparameter heading-conf "
\\hfil{\\huge\\color{red}{\\textsc{Checkout Sheet}}}\\hfil
% \\bigskip\\break % Whitespace
\\break
\\hrule % Horizontal line

000 E Atlanta Drive \\hfill \\emph{Mobile:} (000) 000-0000 \\\\
Ste. 000 \\hfill{ \\emph{Office:} (000) 000-0000} \\\\
% Your address and contact information
Norfolk, Georgia 00000 \\hfill anon@anon.com
\\\\ \\\\
{\\bf Invoice To:} \\\\ % From here --->")

;;;Latex Functions----------------------------------------------
(defun generate-invoice-single-header (invoice func)
  (concatenate 'string  func invoice))

(defun generate-latex (invoice root-dir)
  (ensure-directories-exist (concatenate 'string root-dir
					 "pdf/"))
  (let* ((pdfname (concatenate 'string "pdf/" (show-name invoice)
			       "-" (invoice-set-name invoice)
			       ".tex"))
	 (ironic-tex-name (concatenate 'string "pdf/" (show-name invoice)
				       "-" (invoice-set-name invoice)
				       ".pdf"))
	 (complete-stream (concatenate 'string root-dir
				       pdfname))
	 (completer-stream (concatenate 'string root-dir
					ironic-tex-name)))
    (with-open-file (s complete-stream :direction :output
		       :if-exists :supersede)
      (princ document-conf s)
      (fresh-line s)
      (princ heading-conf s)
      (princ (generate-invoice-single-header
	      (show-name invoice) header-show) s)
      (princ (generate-invoice-single-header
	      (invoice-set-name invoice) header-set) s)
      (princ (generate-invoice-single-header
	      (invoice-contact-name invoice) header-name) s)
      (fresh-line s)
      (fresh-line s)
      (princ date-header s)
      (princ rental-period s)
      (fresh-line s)
      (princ begin-table s)
      (fresh-line s)
      (mapc (lambda (b) (princ (format-description b) s)
		    (fresh-line s)) (invoice-item-list invoice))
      (princ end-table s)
      (fresh-line s)
      (princ tail-conf s))

    (trivial-shell:shell-command (concatenate 'string "pdflatex "
					      "-output-directory="
					      root-dir "pdf/"
					      " " complete-stream))
    (setf (invoice-pdf-location invoice)
	  (subseq completer-stream 70))))

(defun format-description (b)
  (concatenate 'string
	       unitrow
	       (item-description b)
	       "}{"
	       (item-quantity b) "}{"
	       (item-price b) "}{"
	       (item-returned-on b) "}"))
    
;;;Alot of the latex code was ported from the racket version and has already been tested alot
;;;Shouldn't need to mess with this at all unless I'm adding note taking abilities
;;;--------------------------------------------------------------------

;;;Fix the pdf shows up as item bug  (FIXED)
;;;Display a no more items message when all pictures have been used
;;;Add a show list
;;;Get items to add into a global list for searching when adding a new item
;;;Make an item page (Display item checkout history)
;;;Make graphs for how much has been checked in
;;;Allow an order to be finalized so no new items can be added
;;;Allow an order to be checked in completely so a summary is displayed instead
;;;User profiles with timelines
;;;Better dashboard (Filter by date, type)
;;;Move invoice list to a hashtable or alist with the showname as a key (Will be much faster)
 ;;;Get picture size ratios better on checkin and invoice page
;;;Try to figure out adding an item to the invoice without refreshing (JS/Parenscript)
;;;Change current-user cookie to use a hash so you can't change users by screwing with the cookies
;;;Allow for multiple pictures to reference one item
;;;Allow for changing which item is being written by clicking on its picture (FIXED)
;;;Get the image-table code working (well) in the latex class file
;;;Move html invoice table to a sidebar?
;;;Display pictures on the invoice table
;;;Change the setthemcookies name to something reasonable
;;;Remove the createpdf button and have it generate upon each action

;;;Add a message getting sent to the *global-message-list* when a checkin is done
;;;^ it should display how many items were checked in and maybe a percentage of what is left

;;;Do not allow an item to be removed if it's checked in
;;;Setup special privileges for root account and an admin page

;;;Super hard but possibly figure out how to keep pages in sync so more than one person
;;;^ can be viewing them at the same time

;;;Write order page needs to be something like add show
;;;^ Should have the ability to select a show that already exists
;;;^ and create a new invoice for it

;;;Fix invoice numbering and come up with a scheme for it
;;;Move system over to referencing orders by number

;;;Fix sidebar on mobile view or create a mobile page that is limited
;;;^ so you can only add pictures or check in items

;;;Work on serializing the structures, perhaps something as simple as cl-conspack
;;;Create a show info page that displays all the information
;;;Create a button to download a csv copy of the invoice for importing to quicken
;;;LOW PRIORITY: Setup https

;;;Dynamically generate the sidebar depending on what show you are on
;;;^ Should perhaps have a list of links to other sets/invoices in that show

;;;Display the top 10 shows?
;;;Make the pdf iframe hidable by default (FIXED)

;;;LOW PRIORITY: Clean up the imported javascript libraries so it's using specific versions
;;;^instead of CVN's (FIXED)

;;;LOW PRIORITY: Messaging system between users
;;;LOW PRIORITY: Printing
;;;LOW PRIORITY: Split the code into several files that are grouped accordingly
;;;LOW PRIORITY: Clean up the hacky workaround for pathnames (removing 70 characters)

;;;Change the latex code to have the correct address, and fix the header

;;;LOW PRIORITY: If a user is logged in the /login page should automatically go to the dashboard
;;;LOW PRIORITY: Uploaded images should have a reformated version for quicker loading
;;;Fold the two createpdf functions into one


