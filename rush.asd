
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
                             (:file "combatant-interface")
                             (:file "subscriber-interface")
                             (:file "selector-interface")
                             (:file "move-selector-interface")
                             (:file "target-selector-interface")
                             (:file "event-interface")
                             (:file "events")
                             (:file "event-accumulator")
                             (:file "selector")
                             (:file "move-selector")
                             (:file "target-selector")
                             (:file "turn-manager")
                             (:file "deck-manager")
                             (:file "battle")))
               (:module "test"
                :components ((:file "framework")
                             (:file "test-event-accumulator")
                             (:file "test-deck-manager")
                             (:file "test-selector")
                             (:file "test-battle")
                             (:file "test-turn-manager")))))
