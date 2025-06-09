;; Definition: Returns a new cell changing the type of the given
;; In:
;;    - cell= The base cell to copy
;;    - new-type= The new type to set on the cell
;; Out: New cell "object"
(defun change-cell-type(cell new-type)
  (cons new-type (cdr cell))
)

;; Definition: Returns a new cell changing the visibility of the given
;; In:
;;    - cell= The base cell to copy
;;    - new-visibility= The new visibility to set on the cell (t or nil)
;; Out: New cell "object"
(defun change-cell-visibility (cell new-visibility)
  (cons (car cell)           ; Type
        (cons new-visibility ; Visibility
          (cddr cell)        ; Other fields
        )
  )
)