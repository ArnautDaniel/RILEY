#|
  This file is a part of riley project.
  Copyright (c) 2017 Jack Lucas (silver@simonides.xyz)
|#

(in-package :cl-user)
(defpackage riley-test-asd
  (:use :cl :asdf))
(in-package :riley-test-asd)

(defsystem riley-test
  :author "Jack Lucas"
  :license "MIT"
  :depends-on (:riley
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "riley"))))
  :description "Test system for riley"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
