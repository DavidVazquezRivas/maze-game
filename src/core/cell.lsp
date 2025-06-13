;; Definition: Returns a new cell changing the type of the given
;; In:
;;    - cell = The base cell to copy
;;    - new-type = The new type to set on the cell
;; Out: New cell "object"
(defun change-cell-type(cell new-type)
  (cons new-type (cdr cell))
)

;; Definition: Returns a new cell changing the visibility of the given
;; In:
;;    - cell = The base cell to copy
;;    - new-visibility = The new visibility to set on the cell (t or nil)
;; Out: New cell "object"
(defun change-cell-visibility (cell new-visibility)
  (cons (car cell)           ; Type
        (cons new-visibility ; Visibility
          (cddr cell)        ; Other fields
        )
  )
)

;; Definition: Returns a new cell changing the "current" flag of the given cell.
;; In:
;;    - cell = The base cell to copy
;;    - is-current = Whether this cell is currently selected (t or nil)
;; Out: A new cell with updated current flag
(defun change-cell-current (cell is-current)
  (cons 
    (car cell)               ; Type
    (cons (cadr cell)        ; Visibility
    (cons is-current         ; Current flag
    (cdddr cell)))           ; Remaining fields
  )
)

;; Definition: Checks is a cell has been visited
;; In:
;;    - cell = The cell to check status
;; Out: t if cell has been visited, nil otherwise
(defun is-visited (cell)
  (cadddr cell)
)

;; Definition: Marks a cell as visited
;; In:
;;    - cell = The cell to visit
;; Out: The original cell, changing visited state
(defun visit (cell)
(print cell)
  (list
    +cell-type-path+  ; type
    (cadr cell)         ; visible
    (caddr cell)        ; current
    t                   ; visited
    (cddddr cell)       ; Remaining fields
  )
)