#|
  This file is a part of riley project.
  Copyright (c) 2017 Jack Lucas (silver@simonides.xyz)
|#

#|
  Author: Jack Lucas (silver@simonides.xyz)
|#

(in-package :cl-user)
(defpackage riley-asd
  (:use :cl :asdf))
(in-package :riley-asd)

(defsystem riley
  :version "0.7"
  :author "Jack Lucas"
  :license "MIT"
  :depends-on (:cl-who :cl-pass :hunchentoot
:parenscript :trivial-shell :opticl :mito 
:let-over-lambda)
  :components ((:module "src"
			:components
			((:file "riley")
			 (:file "data")
			 (:file "latex")
			 (:file "pages"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op riley-test))))
