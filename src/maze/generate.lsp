;; Definition: Creates a row of the maze with the default values.
;; In:
;;    - m = Number of elements in the row
;; Out: List of m elements with the default cell value
(defun create-row (m)
  (cond
    ((= m 0) nil)
    (t (cons +default-cell+ (create-row (- m 1))))
  )
)

;; Definition: Creates a table for the maze with the default values.
;; In:
;;    - n = Number of rows
;;    - m = Number of columns
;; Out: List of n rows with m cells
(defun create-table (n m)
  (cond
    ((= n 0) nil)
    (t (cons (create-row m) (create-table (- n 1) m)))  
  )
)

;; Definition: Given a maze, a position and a type, returns a new maze copying the original but
;;             replacing the type of the cell in the position with the given
;; In:
;;    - maze = The table of cells
;;    - row = The row of the target cell position
;;    - col = The column of the target cell position
;;    - new-type = The new type to be applied to the target cell
;; Out: The maze updating the cell in the given position
(defun change-position-type (maze row col new-type)
  (replace-in-grid
    maze
    row
    col
    (change-cell-type (nth col (nth row maze)) new-type)
  )
)

;; Definition: Adds an entrance to the maze on the side given
;; In:
;;    - maze = The maze grid
;;    - n = Number of rows
;;    - m = Number of columns
;;    - side = The side where the entrance should be added
;; Out: The maze with the entrance added
(defun add-entrance (grid n m side)
  (apply-on-side grid n m
    (lambda (cell) (change-cell-type cell +cell-type-entrance+))
    side
  )
)

;; Definition: Adds an exit to the maze on the side given
;;    - maze = The maze grid
;;    - n = Number of rows
;;    - m = Number of columns
;;    - side = The side where the exit should be added
;; Out: The maze with the exit added
(defun add-exit (grid n m side)
  (apply-on-side grid n m
    (lambda (cell) (change-cell-type cell +cell-type-exit+))
    side
  )
)

;; Definition: Adds the entrance and exit to the borders of the maze on opposite sides.
;; In:
;;    - maze = The grid of cells
;;    - n = Number of rows
;;    - m = Number of columns
;; Out: A new maze with the entrance and exit placed on opposite sides
(defun add-borders (maze n m)
  (let ((side (random 4)))
    (add-exit 
      (add-entrance maze n m side)
      n
      m
      (opposite-side side)
    )
  )
)

;; Definition: Generates a maze of size n x m, defining the entrance and exit
;;             and the walls and paths.
;; In:
;;    - n = Number of rows
;;    - m = Number of columns
;; Out: A maze (grid of cells) with all the functionalities
(defun generate (n m)
  (add-borders (create-table n m) n m)
)