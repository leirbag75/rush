
(defsystem "rush"
  :author "Gabriel Johnson <leirbag75@gmail.com>"
  :serial t
  :components ((:file "package")
               (:module "src"
                :components ((:file "battle-interface")
                             (:file "event-accumulator-interface")
                             (:file "events")
                             (:file "event-accumulator")
                             (:file "battle")))
               (:module "test"
                :components ((:file "framework")
                             (:file "test-event-accumulator")
                             (:file "test-battle")))))
