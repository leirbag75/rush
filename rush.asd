
(defsystem "rush"
  :author "Gabriel Johnson <leirbag75@gmail.com>"
  :serial t
  :components ((:file "package")
               (:module "src"
                :components ((:file "battle-interface")
                             (:file "events")
                             (:file "battle")))
               (:module "test"
                :components ((:file "framework")
                             (:file "test-battle")))))
