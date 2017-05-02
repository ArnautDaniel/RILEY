
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
					;(:script :src "js/ajax-item.js")
	     (:script :src "js/materialize.min.js")
	     (:link :href "https://fonts.googleapis.com/icon?family=Material+Icons" :rel "stylesheet")
	     (:script :src "plugins/jq-input.js")
	     (:title ,title))
	    
	    (:body 
	     ,@navbar		   	       
	     (:div :class "container"
		   (:div :class "section"
			 ,@body
			 (:footer :class "page-footer"
				  (:div :class "container"
					(:div :class "row"
					      
					      (:h5 :class "center white-text" "Made with Powerful Parenthesis"))))))))))


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
		 (:div :class "card teal"
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

(defmacro standard-global-messages ()
  `(with-html-output (*standard-output* nil :indent t)
     (:div :class "row"
	   
	   (dolist (messages (find-global-messages))
	     (htm
	      (:div :class "col s12 m6 l6"
		    (:div :class "card"
			  (:div :class "card-content"
				(:span :class "card-title" (:h4 
							    (:a :href (concatenate 'string "/profile/" (message-sender messages))
								(fmt "~A" (escape-string (message-sender messages))))))
				
				(fmt "~A" (escape-string (message-content  messages))))
			  (if (message-invoice-name messages)
			      (htm
			       (:div :class "card-action"
				     (:a :href "#" "Order")
				     (:a :href "#" "Show")
				     (:a :href "#" "Check In")))))))))))

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
					     (:img :id "input-picture" :src (escape-string (concatenate 'string ,image "?gensym=" (string (gensym)))) :class  "materialboxed responsive-img" :name ,image))))))
		 (:div :class "col s12 m6 l6"
		       
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
						  :name "input-item-price"
						  :required "required"))
				    (:div :class "input-field col s12 m6 l6"
					  
					  (:label :for "inputQty" :class "black-text" "Quantity")
					  (:input :type "number" 
						  :id "input-item-qty"
						  :name "input-item-qty"
						  :required "required"))
				    (:input :type "hidden" :id "image-data" :name "image-data" :value ,image)
				    
				    (:button :type "submit" :class "red darken-4 btn waves-effect waves-light" "Add")
				    (:a  :class "red  dropdown-button darken-4 btn waves-effect waves-light"
					 
					 :href "#"
					 :data-activates "dropdown1"
					 :data-beloworigin "true"
					 "Rotate")
				    (:ul :id "dropdown1" :class "dropdown-content"
					 (:li (:a :href (format nil "rotate-image?image=~a"
								,image)
						  :class "red-text" (:i :class "material-icons" "rotate_left") "Rotate Left"))
					 (:li (:a :href (format nil "rotate-image?image=~a"
								,image)
						  :class "red-text" (:i :class "material-icons" "rotate_right") "Rotate Right")))
				    (:button :type "button" :class "red darken-4 btn waves-effect waves-light"
					     :data-target "myModal" "Switch")))))))
     
     
					;(:script :src "plugins/custom/ajax-item.js")

     (:div :id "myModal" :class "modal bottom-sheet" 
	   
	   (:div :class "modal-content"
		 
		 (:h4 "Switch Pictures")
		 
		 (dolist (img ,full-images)
		   (htm
		    (:div :class "col s12 m3 l3"
			  (:form :action "/swapitemposition"
				 :method "POST"
				 (:input :type "hidden" :id "image-name"
					 :name "image-name" :value img)
				 (:input :type "image" :id "saveform" :class "img-responsive"
					 :width "100%" :height "50%" :src img
					 :alt "Submit Form"))))))
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
	   (dolist (invoice *global-invoice-list*)
	     (htm
	      (:div :class "col s12 m4 l4"
		    (:div :class "card"
			  (:div :class "card-content"
				(:span :class "card-title" (:h5
							    (fmt "~A" (escape-string (show-name invoice)))))
				
				
				(:div :class "chip black-text"  (fmt "Set: ~A" (escape-string (invoice-set-name invoice))))
				(:div :class "chip black-text" (fmt "Contact: ~A" (escape-string (invoice-contact-name invoice))))
				(:div :class "card-action"
				      (:a :href (format nil "check-in-set-pre?showname=~a&setname=~a" (web-safep (show-name invoice)) (web-safep (invoice-set-name invoice)))
					  :class "btn-flat waves-effect red-text  text-darken-4" "Check-in")
				      (:a :href (format nil "pre-set-cookies?showname=~a&setname=~a" (web-safep (show-name invoice)) (web-safep (invoice-set-name invoice)))
					  :class "btn-flat waves-effect red-text text-darken-4" "Invoice"))))))))))

(defmacro standard-item-list-table (&key invoice)
  `(with-html-output (*standard-output* nil :indent t)
     (:div :class "row"
	   (:script :src "plugins/scrollfire.js")
	   (:div :class "input-field col s12 l4 m4"
		 (:label :for "myInput" :class "black-text" "Search")
		 (:input :type "text" :id "myInput" :onkeyup "myFunction()" :class "black-text"))
	   (:script :src "plugins/search.js")
	   (:div :id "itemlist"
		 (:ul :id "myUL"
		      (dolist (item (invoice-item-list ,invoice))
			(htm
			 (:li
			  (:div :class "col s12 m4 l4"
				(:div :class "card medium blue-grey"
				      (:div :class "card-image"
					    (:img :src (item-picture item) :width "25%" :height "25%" :class "materialboxed responsive-img" :data-caption (item-description item))
					    (:span :class "card-title truncate"
						   :style "font-size:20px; text-shadow:-1px 0 black, 0 1px black, 1px 0 black, 0 -1px black;" ;;;Set correct fontsize and give a blackoutline to help readability
						   (fmt "~A" (escape-string (item-description item)))))
				      (:div :class "card-content"
					    
					    (:span
					     
					     (:i :class "material-icons" "attach_money") (fmt " ~A &emsp;" (escape-string (item-price item)))
					     (:a :href "#" :class "right black-text" (:i :class "material-icons" "all_inclusive") (fmt " ~A" (escape-string (item-quantity item))))))
				      (:div :class "card-action"
					    (:form :class "form-inline"
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
						   (if (string= (item-returned-on item) "")
						       (htm (:button :type "submit" :class "red darken-4 btn btn-default btn-sm btn-danger" "Remove"))
						       (htm (:button :type "submit" :class "btn black-text disabled" :disabled "true"
								     (fmt "RTN'D ~A" (escape-string (item-returned-on item))))))
						   (:a :href "#" :class "btn-floating waves-effect weaves-light" :data-target "myModal" (:i :class "material-icons" "content_copy"))))))))))))))
;;;Force remove option may be necessary due to this




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
				      (:img :src (item-picture item) :class "materialboxed responsive-img" :data-caption (item-description item))
				      (:span :class "card-title truncate"
					     :style "font-size:20px; text-shadow:-1px 0 black, 0 1px black, 1px 0 black, 0 -1px black;" ;;;Set correct fontsize and give a blackoutline to help readability
					     (fmt "~A" (escape-string (item-description item)))))
				
				(:div :class "card-content"
				      (:span 
				       (:i :class "material-icons" "attach_money") (fmt "~A" (escape-string (item-price item)))					    
				       (:a :href "#" :class "right black-text" (:i :class "material-icons" "all_inclusive") (fmt " ~A/~A &emsp;"
																 (escape-string (item-returned-qty item))
																 (escape-string (item-quantity item))))))
				
				(:div :class "card-action"
				      
				      (:form 
				       :action "/check-in-item"
				       :method "POST"
				       :id "check-in-table"
				       
				       (:input :type "hidden" :name "item-price" :id "item-price"
					       :value (item-price item))
				       (:input :type "hidden" :name "item-desc" :id "item-desc"
					       :value (item-description item))
				       (:input :type "hidden" :name "item-qty" :id "item-qty"
					       :value (item-quantity item))
				       (:button :type "submit" :class "red darken-4 btn-floating waves-effect waves-light" (:i :class "material-icons" "check_circle"))			        
				       (:a :class "right red darken-4 btn-floating activator" (:i :class "material-icons" "arrow_upward"))))
				
				(:div :class "card-reveal"
				      (:span :class "card-title grey-text text-darken-4"
					     (fmt "~A" (escape-string (item-description item))) (:i :class "material-icons right" "close"))
				      (:form :action (concatenate 'string "/partial-check-in?qty=" (item-quantity item) "&desc=" (item-description item) "&price=" (item-price item))
					     :method "POST"
					     (:p :class "range-field"
						 (:input :id "ranged" :name "ranged" :type "range" :min "0" :max (web-math-subtract (item-quantity item) (item-returned-qty item)) :oninput "this.form.rangedName.value=this.value"))
					     (:input :class "left" :type "number" :name "rangedName" :min "0" :max (web-math-subtract (item-quantity item) (item-returned-qty item)):value (web-math-subtract (item-quantity item) (item-returned-qty item)) :oninput "this.form.ranged.value=this.value")
					     
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
	      :src (concatenate 'string (web-safep ,pdf) "?name="))))

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
