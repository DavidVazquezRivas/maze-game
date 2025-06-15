;; Maze "object":
;; A list representing the current state of the maze:
;; (
;;   grid: A 2D list of cell objects (the maze grid),
;;   current-row: Row of the current/active cell
;;   current-col: Column of the current/active cell
;; )

;; Definition: A default maze with no grid and no current cell set.
(defconstant +default-maze+ (list nil nil))

;; Definition: Returns the grid (2D list of cells) from the maze.
;; In:
;;   - maze: The maze object
;; Out: The grid part of the maze
(defun get-grid (maze)
  (car maze)
)

;; Definition: Sets the grid (2D list of cells) in the maze.
;; In:
;;   - maze: The maze object
;;   - new-grid: The new 2D list of cells to set as the grid
;; Out: A new maze object with the grid updated
(defun set-grid (maze new-grid)
  (list new-grid (get-current maze))
)

;; Definition: Returns the position of the current active cell in the maze.
;; In:
;;    - maze = The maze object
;; Out: A (row . col) pair representing the current cell's position
(defun get-current (maze)
  (cadr maze)
)

;; Definition: Sets the current cell position in the maze.
;; In:
;;   - maze: The maze object
;;   - current-row: Row of the current/active cell
;;   - current-col: Column of the current/active cell
;; Out: A new maze object with the current cell updated
(defun set-current (maze row col)
  (list (get-grid maze) row col)
)

;; Definition: Returns the row index of the current active cell in the maze.
;; In:
;;    - maze = The maze object
;; Out: An integer representing the current cell's row
(defun get-current-row (maze)
  (cadr maze)
)

;; Definition: Returns the column index of the current active cell in the maze.
;; In:
;;    - maze = The maze object
;; Out: An integer representing the current cell's column
(defun get-current-col (maze)
  (caddr maze)
)

;; Definition: Finds the position of the first cell of a given type in the grid.
;; In:
;;   - grid: A list of rows, each row is a list of cells
;;   - target-type: The type to look for (e.g., +cell-type-entrance+)
;; Out: A dotted pair (row . col) representing the position of the first matching cell, or nil if not found
(defun find-cell-of-type (grid target-type)
  (find-cell-of-type-helper grid target-type 0)
)

;; Helper function with row index tracking
(defun find-cell-of-type-helper (grid target-type row-index)
  (cond
    ((null grid) nil) ; No more rows to search
    (t
     (let ((col-index (find-in-row (car grid) target-type 0)))
       (cond
         (col-index (cons row-index col-index)) ; Found: return (row . col)
         (t (find-cell-of-type-helper (cdr grid) target-type (+ row-index 1)))
       )
     )
    )
  )
)


;; Searches a single row for the target type
(defun find-in-row (row target-type col-index)
  (cond
    ((null row) nil) ; End of row
    ((equal (car (car row)) target-type) col-index) ; Found
    (t (find-in-row (cdr row) target-type (+ col-index 1)))
  )
)


;; Definition: Initializes the current cell position of the maze by locating the entrance cell.
;; In:
;;   - maze: The maze object (with a grid but possibly no current cell)
;; Out: A new maze object with the current cell set to the entrance position
(defun init-current (maze)
  (let ((pos (find-cell-of-type (get-grid maze) +cell-type-entrance+)))
    (cond
      (pos (set-current maze (car pos) (cdr pos)))
      (t maze))
  )
)

;; Definition: Returns the character representation of a cell type.
;; In:
;;   - cell: A cell object (a list whose car is the cell type)
;; Out: A string character representing the cell
(defun cell-char (cell)
  (cond
    ((eq (car cell) +cell-type-wall+) #\#)
    ((eq (car cell) +cell-type-path+) #\.)
    ((eq (car cell) +cell-type-entrance+) #\e)
    ((eq (car cell) +cell-type-exit+) #\s)
    (t #\Space)
  )
)


;; Definition: Recursively processes one row of the grid, concatenating cell chars.
;; In:
;;   - grid: The maze grid (list of rows)
;;   - row: The index of the current row
;;   - col: The current column index being processed
;;   - cols: Total number of columns
;; Out: A string representing the processed row from col to end
(defun process-col (grid row col cols)
  (cond
    ((>= col cols) nil)
    (t (cons (cell-char (get-cell grid row col))
             (process-col grid row (+ col 1) cols)))
  )
)

;; Definition: Recursively processes all rows of the grid, concatenating lines.
;; In:
;;   - grid: The maze grid (list of rows)
;;   - row: The current row index being processed
;;   - rows: Total number of rows
;;   - cols: Total number of columns
;; Out: A string representing the entire maze formatted
(defun process-row (grid row rows cols)
  (cond
    ((>= row rows) nil)
    (t (append (process-col grid row 0 cols)
               (list #\Newline)
               (process-row grid (+ row 1) rows cols)))
  )
)

;; Definition: Encrypts the given maze into format:
;;   - # for walls
;;   - . for paths
;;   - e for entrance
;;   - s for exit
;;   - \n at the end of each row
;; In:
;;   - maze: The maze object
;; Out: Formatted string encrypting maze
(defun encrypt-maze (maze)
  (let* ((grid (get-grid maze))
         (rows (length grid))
         (cols (length (car grid))))
        (process-row grid 0 rows cols)
  )
)