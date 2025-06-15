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
(defconstant +min-dimension+ 10) ; This will be used as viewport dimension, so less makes no sense
(defconstant +max-dimension+ 50) ; Increase this causes stack overflow on squared mazes (50x50)

;; Cell "object": 
;;(
;;  type: string, 
;;  visible: boolean,
;;  current: boolean, (used for player and generation)
;;  visited: boolean
;;)
(defconstant +default-cell+ (list +cell-type-wall+ nil nil nil))