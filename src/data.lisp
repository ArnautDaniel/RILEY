
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
      (mito:create-dao 'invoice-db :set-name set-name :date-out "000" :show-db (mito:find-dao 'show-db :name show-name) :root-dir root-dir :pdf-location pdf-location))

(defun find-invoice-from-cookie (id)
  (find-invoice-db :id (parse-integer id)))

(defun find-invoice-item-list (id)
  (mito:retrieve-dao 'item-db :invoice-id (parse-integer id)))

