;; PATHS
(defconstant +maze-path+ "../data/mazes/")
(defconstant +stats-path+ "../data/stats/")

;; CELL TYPES
(defconstant +cell-type-entrance+ "entrance")
(defconstant +cell-type-exit+ "exit")
(defconstant +cell-type-path+ "path")
(defconstant +cell-type-wall+ "wall")
(defconstant +empty-type+ "")

;; CHAR REPRESENTATION TYPES
(defconstant +char-entrance+  #\e)
(defconstant +char-exit+  #\s)
(defconstant +char-path+  #\.)
(defconstant +char-wall+  #\#)
(defconstant +char-space+  #\Space)
(defconstant +char-newline+  #\Newline)

;; DIMENSION LIMITS
(defconstant +min-dimension+ 10) ; This will be used as viewport dimension, so less than 10 makes no sense
(defconstant +max-dimension+ 45) ; Increase this causes stack overflow on squared mazes (45x45)

;; GRAPHICS CONSTANTS
(defconstant +screen-height+ 375)
(defconstant +screen-width+ 640)
(defconstant +screen-height-chars+ 25)
(defconstant +screen-width-chars+ 80)
(defconstant +char-width+ 8)
(defconstant +char-height+ 15)
(defconstant +viewport-size+ 10)

(defconstant +empty-color+ '(-1 -1 -1))
(defconstant +wall-color+ '(0 0 0))
(defconstant +path-color+ '(255 255 255))
(defconstant +entrance-color+ '(255 95 60))
(defconstant +exit-color+ '(60 255 70))
(defconstant +player-color+ '(60 100 255))
(defconstant +visited-color+ '(255 255 60))

;; GAME CONSTANTS
(defconstant up-keys    '(119 87))  ; w / W
(defconstant down-keys  '(115 83))  ; s / S
(defconstant left-keys  '(97 65))   ; a / A
(defconstant right-keys '(100 68))  ; d / D
(defconstant map-keys   '(109 77))  ; m / M
(defconstant esc-key    27)         ; ESC

;; UI CONSTANTS
(defconstant +instructions+ '("Instructions:" "" "W: Move up." "A: Move left." "S: Move down." "D: Move right." "M: Switch minimap." "Esc: Exit game."))
(defconstant +top-limit+ 3)

;; Cell "object": 
;;(
;;  type: string, 
;;  visible: boolean,
;;  current: boolean, (used for player and generation)
;;  visited: boolean
;;)
(defconstant +default-cell+ (list +cell-type-wall+ nil nil nil))
(defconstant +empty-cell+ (list +empty-type+ nil nil nil))