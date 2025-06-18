;; Maze "object":
;; A list representing the current state of the maze:
;; (
;;   grid: A 2D list of cell objects (the maze grid),
;;   current-row: Row of the current/active cell
;;   current-col: Column of the current/active cell
;;   minimap: Boolean flag that indicates wether minimap is on/off
;; )

;; Definition: A default maze with no grid and no current cell set.
(defconstant +default-maze+ (list 0 0 nil))

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
  (list new-grid (get-current maze) (get-minimap maze))
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
;;   - new-row: Row of the current/active cell
;;   - new-col: Column of the current/active cell
;; Out: A new maze object with the current cell updated
(defun set-current (maze new-row new-col)
  (let* ((grid (get-grid maze))
         (old-row (get-current-row maze))
         (old-col (get-current-col maze))
         
         ;; Get and update the old current cell (set current to NIL)
         (old-cell (get-cell grid old-row old-col))
         (updated-old-cell (change-cell-current old-cell nil))
         (grid-with-old-cleared (replace-in-grid grid old-row old-col updated-old-cell))
         
         ;; Get and update the new current cell (set current to T)
         (new-cell (get-cell grid-with-old-cleared new-row new-col))
         (updated-new-cell (change-cell-current new-cell t))
         (final-grid (replace-in-grid grid-with-old-cleared new-row new-col updated-new-cell)))
    
    ;; Return the updated maze as a list (or a struct if you use one)
    (list final-grid new-row new-col (get-minimap maze))))

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

;; Definition: Returns wether the minimap is active or not
;; In:
;;    - maze = The maze object
;; Out: A boolean for minimap state
(defun get-minimap (maze)
  (cadddr maze)
)

;; Definition: Sets wether the minimap is open or not
;; In:
;;    - maze: The maze object
;;    - minimap: The new minimap state 
;; Out: The maze "object" updating minimap state
(defun set-minimap (maze minimap)
  (list
    (get-grid maze)
    (get-current-row maze)
    (get-current-col maze)
    minimap))

;; Definition: Switches the minimap state
;; In:
;;    - maze = The maze object
;; Out: The updated maze
(defun switch-minimap (maze)
  (cond
    ((get-minimap maze) (set-minimap maze nil))
    (t (set-minimap maze t))))

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

;; Definition: Recursively encrypts one row of the grid, concatenating cell chars.
;; In:
;;   - grid: The maze grid (list of rows)
;;   - row: The index of the current row
;;   - col: The current column index being encrypted
;;   - cols: Total number of columns
;; Out: A string representing the encrypted row from col to end
(defun encrypt-col (grid row col cols)
  (cond
    ((>= col cols) nil)
    (t (cons (char-from-cell (get-cell grid row col))
             (encrypt-col grid row (+ col 1) cols)))
  )
)

;; Definition: Recursively encrypts all rows of the grid, concatenating lines.
;; In:
;;   - grid: The maze grid (list of rows)
;;   - row: The current row index being encrypted
;;   - rows: Total number of rows
;;   - cols: Total number of columns
;; Out: A string representing the entire maze formatted
(defun encrypt-row (grid row rows cols)
  (cond
    ((>= row rows) nil)
    (t (append (encrypt-col grid row 0 cols)
               (list #\Newline)
               (encrypt-row grid (+ row 1) rows cols)))
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
        (encrypt-row grid 0 rows cols)
  )
)

;; Definition: Given a row of encrypted cells, returns a row with the cell "objects"
;; In:
;;   - row: A list of chars encrypted.
;; Out: A list of cell "objects"
(defun decrypt-row (row)
  (cond 
    ((null row) nil)
    (t (cons (cell-from-char (car row)) (decrypt-row (cdr row))))
  )
)

;; Definition: Given a list, splits it by rows separated by #\Newline
;; In:
;;   - chars: The initial list of characters
;; Out: List of lists of chars, separated by the splitter #\Newline
(defun split-by-newline (chars)
  (reduce (lambda (acc ch)
            (cond
              ((char= ch #\Newline) (append acc (list '())))
              (t (let ((last-row (car (last acc))))
                 (append (butlast acc)
                         (list (append last-row (list ch))))))
            )
          )
          chars
          :initial-value (list '()))
)

;; Definition: Given a list of encrypted cells, returns a grid for a maze decrypted.
;; In:
;;   - grid: The list of characters
;; Out: A grid of cell "objects"
(defun decrypt-grid (grid)
  (mapcar #'decrypt-row (split-by-newline grid))
)

;; Definition: Given a list of encrypted cells, returns a the maze decrypted.
;; In:
;;   - chars: The list of characters
;; Out: The maze "object"
(defun decrypt-maze (chars)
  (init-current (list (decrypt-grid chars) 0 0))
)

;; Definition: Loads a stored maze from a file on given name
;; In:
;;   - name: The name of the file to be loaded
;; Out: The maze "object"
(defun load-maze (name)
  (decrypt-maze (read-maze name)))

;; Definition: Helper to get a cell from the original maze grid safely, returning a wall if it's out of range.
;; In:
;;   - grid: The original maze grid.
;;   - row: The row index to retrieve.
;;   - col: The column index to retrieve.
;; Out: The cell at (row, col) or the +default-cell+ if out of bounds.
(defun safe-get-cell (grid row col)
  (let* ((num-rows (length grid))
         (row-content (cond
                        ((and (>= row 0) (< row num-rows)) (nth row grid))
                        (t nil)))
         (num-cols (cond
                      (row-content (length row-content))
                      (t 0))))
    (let ((cell (cond
                  ((and row-content (>= col 0) (< col num-cols)) (nth col row-content))
                  (t +default-cell+))))
      cell)))

;; Definition: Get's the viewport nth row of the grid given
;; In:
;;   - grid: The grid to get the viewport
;;   - row: Current generating row 
;;   - col: Current generating col (for recursive)
;;   - max-col: The column number where the viewport ends
;; Out: The grid of cells, represening the current viewport of the maze
(defun get-viewport-row (grid row col max-col)
  (cond 
    ((= max-col col) (cons (safe-get-cell grid row col) nil))
    (t (cons (safe-get-cell grid row col) (get-viewport-row grid row (+ col 1) max-col)))))

;; Definition: Get's the viewport grid of a grid given
;; In:
;;   - grid: The grid to get the viewport
;;   - row: Current generating row (for recursive)
;;   - min-col: The column number where the viewport starts
;;   - max-col: The column number where the viewport ends
;;   - max-row: The row number where the viewport ends
;; Out: The grid of cells, represening the current viewport of the maze
(defun get-viewport-grid (grid row min-col max-col max-row)
  ;; (format t "get-viewport-grid called with row=~a, min-col=~a, max-col=~a and max-row=~a" row min-col max-col max-row)
  (cond 
    ((= max-row row) (cons (get-viewport-row grid row min-col max-col) nil))
    (t (cons (get-viewport-row grid row min-col max-col) (get-viewport-grid grid (+ row 1) min-col max-col max-row)))))

;; Definition: Returns the viewport grid of a maze given, based on the viewport-size constant, works as a facade for the previous function
;; In:
;;   - maze: The maze to get the viewport
;; Out: The grid of cells, represening the current viewport of the maze
(defun get-viewport (maze)
  (let* ((half (floor (/ +viewport-size+ 2)))
         (current-col (get-current-col maze))
         (current-row (get-current-row maze))
         (min-col (- current-col half))
         (max-col (+ current-col half))
         (min-row (- current-row half))
         (max-row (+ current-row half)))
    (get-viewport-grid (get-grid maze) min-row min-col max-col max-row)))

;; Definition: Moves the player to a given position if it's possible
;; In:
;;   - maze: The maze of the game
;;   - new-row: New player's row
;;   - new-col: New player's column
;; Out: The maze updated
(defun move-player (maze new-row new-col)
  (setq steps (+ steps 1))  ; Even if you can't move, step increases. Don't crash with walls!
  (cond
    ((string= (car (safe-get-cell (get-grid maze) new-row new-col)) +cell-type-wall+) maze)
    (t (set-current maze new-row new-col))))

;; Definition: Moves the player up if it's posible
;; In:
;;   - maze: The maze of the game
;; Out: The maze updated
(defun move-up (maze)
  (move-player 
    maze 
    (- (get-current-row maze) 1)
    (get-current-col maze)))

;; Definition: Moves the player down if it's posible
;; In:
;;   - maze: The maze of the game
;; Out: The maze updated
(defun move-down (maze)
  (move-player 
    maze 
    (+ (get-current-row maze) 1)
    (get-current-col maze)))

;; Definition: Moves the player left if it's posible
;; In:
;;   - maze: The maze of the game
;; Out: The maze updated
(defun move-left (maze)
  (move-player 
    maze 
    (get-current-row maze)
    (- (get-current-col maze) 1)))

;; Definition: Moves the player right if it's posible
;; In:
;;   - maze: The maze of the game
;; Out: The maze updated
(defun move-right (maze)
  (move-player 
    maze 
    (get-current-row maze)
    (+ (get-current-col maze) 1)))