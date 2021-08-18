
(in-package :rush)

(defgeneric initialize-turn-manager (turn-manager battle))

(defgeneric next-player-to-move (turn-manager))

(defgeneric end-turn (turn-manager))
