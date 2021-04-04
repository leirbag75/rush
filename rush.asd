
(defsystem "rush"
  :author "Gabriel Johnson <leirbag75@gmail.com>"
  :serial t
  :components ((:file "package")
               (:module "src"
                        :components ((:file "battle-interface")))
               (:module "test"
                        :components ((:file "framework")))))
