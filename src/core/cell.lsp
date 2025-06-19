;; Definition: Given a cell, returns the encrypted character
;; In:
;;    - cell: Cell "object"
;; Out: Char encoding the cell
(defun char-from-cell (cell)
  (cond
    ((eq (car cell) +cell-type-wall+) +char-wall+)
    ((eq (car cell) +cell-type-path+) +char-path+)
    ((eq (car cell) +cell-type-entrance+) +char-entrance+)
    ((eq (car cell) +cell-type-exit+) +char-exit+)
    (t +char-space+)))

;; Definition: Given a cell encrypted char representing a cell
;; In:
;;    - c: Encrypted char
;; Out: New cell "object"
(defun cell-from-char (c)
  (list
    (cond 
      ((char= c +char-wall+) +cell-type-wall+)
      ((char= c +char-path+) +cell-type-path+)
      ((char= c +char-entrance+) +cell-type-entrance+)
      ((char= c +char-exit+) +cell-type-exit+)
      (t +cell-type-path+)); type
    nil                           ; visible
    (char= c +char-entrance+)     ; current
    nil))

;; Definition: Given a cell cell, returns the color to paint it
;; In:
;;    - cell: The cell to check correspondant color
;; Out: The color to be painted format rgb
(defun get-cell-color (cell)
  (cond 
    ((string= (caddr cell) t) +player-color+)
    ((string= (car cell) +cell-type-entrance+) +entrance-color+)
    ((string= (car cell) +cell-type-exit+) +exit-color+)
    ((string= (cadddr cell) t) +visited-color+)
    ((string= (car cell) +cell-type-path+) +path-color+)
    ((string= (car cell) +cell-type-wall+) +wall-color+)
    (t +path-color+)))

;; Definition: Returns a new cell changing the type of the given
;; In:
;;    - cell = The base cell to copy
;;    - new-type = The new type to set on the cell
;; Out: New cell "object"
(defun change-cell-type(cell new-type)
  (cons new-type (cdr cell)))

;; Definition: Returns a new cell changing the visibility of the given
;; In:
;;    - cell = The base cell to copy
;;    - new-visibility = The new visibility to set on the cell (t or nil)
;; Out: New cell "object"
(defun change-cell-visibility (cell new-visibility)
  (cons (car cell)           ; Type
        (cons new-visibility ; Visibility
          (cddr cell))))     ; Other fields

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
    (cdddr cell)))))         ; Remaining fields

;; Definition: Checks is a cell has been visited
;; In:
;;    - cell = The cell to check status
;; Out: t if cell has been visited, nil otherwise
(defun is-visited (cell)
  (cadddr cell))

;; Definition: Marks a cell as visited
;; In:
;;    - cell = The cell to visit
;; Out: The original cell, changing visited state
(defun visit (cell)
  (list
    +cell-type-path+  ; type
    (cadr cell)         ; visible
    (caddr cell)        ; current
    t                   ; visited
    (cddddr cell)))       ; Remaining fields