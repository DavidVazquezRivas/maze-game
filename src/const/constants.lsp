;; PATHS
(defconstant +maze-path+ "../data/mazes/")

;; CELL TYPES
(defconstant +cell-type-entrance+ "entrance")
(defconstant +cell-type-exit+ "exit")
(defconstant +cell-type-path+ "path")
(defconstant +cell-type-wall+ "wall")

;; CHAR REPRESENTATION TYPES
(defconstant +char-entrance+  #\e)
(defconstant +char-exit+  #\s)
(defconstant +char-path+  #\.)
(defconstant +char-wall+  #\#)
(defconstant +char-space+  #\Space)
(defconstant +char-newline+  #\Newline)

;; DIMENSION LIMITS
(defconstant +min-dimension+ 10) ; This will be used as viewport dimension, so less than 10 makes no sense
(defconstant +max-dimension+ 50) ; Increase this causes stack overflow on squared mazes (50x50)

;; GRAPHICS CONSTANTS
(defconstant +screen-height+ 375)
(defconstant +screen-width+ 640)
(defconstant +viewport-size+ 10)
(defconstant +wall-color+ '(0 0 0))
(defconstant +path-color+ '(255 255 255))
(defconstant +entrance-color+ '(255 95 60))
(defconstant +exit-color+ '(60 255 70))
(defconstant +player-color+ '(60 100 255))

;; Cell "object": 
;;(
;;  type: string, 
;;  visible: boolean,
;;  current: boolean, (used for player and generation)
;;  visited: boolean
;;)
(defconstant +default-cell+ (list +cell-type-wall+ nil nil nil))