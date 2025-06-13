;; CELL TYPES
(defconstant +cell-type-entrance+ "entrance")
(defconstant +cell-type-exit+ "exit")
(defconstant +cell-type-path+ "path")
(defconstant +cell-type-wall+ "wall")

;; Cell "object": 
;;(
;;  type: string, 
;;  visible: boolean,
;;  current: boolean, (used for player and generation)
;;  visited: boolean
;;)
(defconstant +default-cell+ (list +cell-type-wall+ nil nil nil))