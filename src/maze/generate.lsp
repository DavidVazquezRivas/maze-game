;; Definition: Creates a row of the maze with the default values.
;; In:
;;    - m: Number of elements in the row
;; Out: List of m elements with the default cell value
(defun create-row (m)
  (cond
    ((= m 0) nil)
    (t (cons +default-cell+ (create-row (- m 1))))
  )
)

;; Definition: Creates a maze grid with default values and an initial state.
;; In:
;;   - n: Number of rows
;;   - m: Number of columns
;; Out: A list with two elements:
;;        1. The generated grid (a list of n rows with m cells each)
;;        2. nil (representing no current cell set yet)
(defun create-grid (n m)
  (list
    (labels ((generate-rows (rows)
               (cond
                 ((= rows 0) nil)
                 (t (cons (create-row m) (generate-rows (- rows 1)))))))
      (generate-rows n))
    0
    0
  )
)

;; Definition: Adds an entrance to the maze on the side given.
;; In:
;;    - maze: The maze object (list: grid row col)
;;    - n: Number of rows
;;    - m: Number of columns
;;    - side: The side where the entrance should be added (0=Top, 1=Right, 2=Bottom, 3=Left)
;; Out: A new maze with the entrance added on the given side
(defun add-entrance (maze n m side)
  (let ((updated-grid (apply-on-side 
                        (get-grid maze) 
                        n 
                        m
                        (lambda (cell) (change-cell-type cell +cell-type-entrance+))
                         
                         side
                      )))
    (list updated-grid (cadr maze) (caddr maze)) ; Keep the current position unchanged
  )
)

;; Definition: Adds an exit to the maze on the side given.
;; In:
;;    - maze: The maze object (list: grid row col)
;;    - n: Number of rows
;;    - m: Number of columns
;;    - side: The side where the exit should be added
;; Out: A new maze with the exit added on the given side
(defun add-exit (maze n m side)
  (let ((updated-grid (apply-on-side (get-grid maze) n m
                         (lambda (cell) (change-cell-type cell +cell-type-exit+))
                         side)))
    (list updated-grid (cadr maze) (caddr maze))
  )
)

;; Definition: Adds entrance and exit on opposite sides of the maze.
;; In:
;;    - maze: The maze object
;;    - n: Number of rows
;;    - m: Number of columns
;; Out: A maze with entrance and exit placed on opposite sides
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

;; Definition: Returns the list of orthogonal neighbours (up, down, left, right) of the given position.
;; In:
;;    - row: Row index of the current cell
;;    - col: Column index of the current cell
;; Out: A list of (row . col) positions representing neighbours
(defun get-neighbours (row col)
  (list
    (cons (- row 1) col) ; Up
    (cons (+ row 1) col) ; Down
    (cons row (- col 1)) ; Left
    (cons row (+ col 1)) ; Right
  )
)

;; Definition: Given the maze and a list of the neighbours from the cell, counts the amount of path-type neighbours
;; In:
;;    - maze: The maze object
;;    - neighbours: The neighbours of the cell (row col) 
;; Out: The amount of path-type elements in the neighbours list
(defun count-paths (maze neighbours)
  (cond
    ((null neighbours) 0)
    (t (let* ((pos (car neighbours))
              (r (car pos))
              (c (cdr pos))
              (cell (get-cell (get-grid maze) r c)))
         (+ (cond
              ((eq (car cell) +cell-type-path+) 1)
              (t 0))
            (count-paths maze (cdr neighbours)))))
  )
)

;; Definition: Checks is a position given is valid to be visited
;; In:
;;    - maze: The maze object
;;    - row: Row index of the evaluated cell
;;    - col: Column index of the evaluated cell
;;    - n: Number of rows in the maze
;;    - m: Number of columns in the maze
;; Out: t if the cell is valid, nil otherwise
(defun is-valid (maze row col m n)
  (cond
    ;; Is on border or outside
    ((or (<= row 0) (<= col 0) (>= row (- m 1)) (>= col (- n 1))) nil)
    
    (t (let* ((cell (get-cell (get-grid maze) row col)))
      (cond 
        ;; Is visited
        ((is-visited cell) nil)
        
        
        ;; Is not a wall
        ((not (eq (car cell) +cell-type-wall+)) nil)
        
        
        ;; Has more than one path neighbour
        ((> (count-paths maze (get-neighbours row col)) 1) nil)
        
        ;; Is valid
        (t t)
      )
    ))
  )
)

;; Definition: Processes and explores a neighbour list
;; In:
;;    - maze: The maze object
;;    - neighbours: The neightbours of the cell (row col) 
;;    - n: Number of rows in the maze
;;    - m: Number of columns in the maze
;; Out: The maze object updated
(defun process-neighbours (maze neighbours m n)
  (cond
    ((null neighbours) maze)
    (t (let* ((pos (car neighbours))
              (r (car pos))
              (c (cdr pos)))
         ;; Explore this neighbour and update maze
         (setq maze (explore maze r c m n))
         ;; Process other neighbours
         (process-neighbours maze (cdr neighbours) m n)))
  )
)


;; Definition: Explores a cell, and processes it's neighbours
;; In:
;;    - maze: The maze object
;;    - row: Row index of the cell to explore
;;    - col: Column index of the cell to explore
;;    - n: Number of rows in the maze
;;    - m: Number of columns in the maze
;; Out: The maze object updated
(defun explore (maze row col m n)
  (cond
    ;; Base case
    ((not (is-valid maze row col m n)) maze)
    
    ;; Recursive case
    (t (progn
          ;; Mark cell as visited
          (let* ((grid (get-grid maze))
                (cell (get-cell grid row col))
                (new-cell (visit cell))
                (new-grid (replace-in-grid grid row col new-cell)))
            (setf maze (set-grid maze new-grid)))

          ;; Process neighbours
          (process-neighbours maze (shuffle (get-neighbours row col)) m n)))
  )
)

(defun generate-path (maze m n)
  (let ((row (get-current-row maze))
        (col (get-current-col maze)))
    (process-neighbours maze (get-neighbours row col) m n)
  )
)

;; Definition: Generates a maze of size n x m, defining the entrance and exit
;;             and generating a path from the entrance.
;; In:
;;    - n: Number of rows
;;    - m: Number of columns
;; Out: A maze (list: grid current-row current-col) with path carved from entrance
(defun generate (n m)
  (let ((maze (init-current (add-borders (create-grid n m) n m))))
    (generate-path maze n m)
  )
)