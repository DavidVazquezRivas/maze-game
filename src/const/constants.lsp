;; CELL TYPES
(defconstant +cell-type-entrance+ "entrance")
(defconstant +cell-type-exit+ "exit")
(defconstant +cell-type-path+ "path")
(defconstant +cell-type-wall+ "wall")

;; Cell "object": 
;;[
;;  type: string, 
;;  visible: boolean,
;;  actual: boolean, (used for player and generation)
;;]
(defconstant +default-cell+ (list +cell-type-wall+ nil nil))