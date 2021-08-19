
(defsystem "rush"
  :author "Gabriel Johnson <leirbag75@gmail.com>"
  :serial t
  :depends-on ("closer-mop")
  :components ((:file "package")
               (:module "src"
                :components ((:file "battle-interface")
                             (:file "event-accumulator-interface")
                             (:file "turn-manager-interface")
                             (:file "move-interface")
                             (:file "deck-manager-interface")
                             (:file "events")
                             (:file "event-accumulator")
                             (:file "turn-manager")
                             (:file "battle")))
               (:module "test"
                :components ((:file "framework")
                             (:file "test-event-accumulator")
                             (:file "test-battle")))))
