;; Definition: Sets paint color to the given in rgb list format
;; In:
;;   - rgb: The color to set
;; Out: None
(defun set-color (rgb)
  (apply #'color rgb))

;; Definition: Paints a square of the color given
;; In:
;;   - size: The size of the square in px
;;   - color: The color of the square in rgb list format
;; Out: None
(defun square (size color)
  (cond
      ((= size 0) nil)
      (t  (set-color color)
          (drawrel 0 (- size))        ; move down
          (drawrel size 0)            ; move right
          (drawrel 0 size)            ; move up
          (drawrel (- size) 0)        ; move left
          (square (- size 1) color))))

;; Definition: Paints a cell given the size to paint it and the cell
;; In:
;;   - size: Size in px
;;   - cell: The cell to be painted
;;   - row: Current painting row
;;   - col: Current painting col
;; Out: None
(defun draw-cell (size cell row col)
  (cond 
    ((not (equal 
            (get-cell-color cell)
            (get-cell-color (get-cell-or-empty painted row col))))
        (square size (get-cell-color cell)))))

;; Definition: Paints a row of cells given, with the specified cell-size
;; In:
;;   - size: Cell size in px
;;   - row: Row of cells
;;   - r: Current painting row
;;   - c: Current painting col
;; Out: None
(defun draw-row (size row r c)
  (cond
    ((null row) nil)
    (t
      (draw-cell size (car row) r c)
      (moverel size 0)
      (draw-row size (cdr row) r (+ 1 c))
      (moverel (- size) 0))))

;; Definition: Paints a grid of cells given, with the specified cell-size
;; In:
;;   - size: Cell size in px
;;   - grid: Grid of cells
;;   - row: Current painting row
;;   - col: Current painting col
;; Out: None
(defun draw-grid-rec (size grid row col)
  (cond
    ((null grid) nil)
    (t
      (draw-row size (car grid) row col)
      (moverel 0 (- size))
      (draw-grid-rec size (cdr grid) (+ 1 row) col))))

;; Definition: Paints the grid, and notes the grid painted
;; In:
;;   - size: Cell size in px
;;   - grid: Grid of cells
;; Out: None (modifies painted global)
(defun draw-grid (size grid)
  (draw-grid-rec size grid 0 0)
  (setq painted grid))

;; Definition: Paints a maze given, viewport or minimap
;; In:
;;   - maze: The maze to paint
;; Out: None
(defun draw-maze (maze)
  (cond
    ((get-minimap maze)
      (let* ((grid (get-grid maze))
              (rows (length grid))
              (cols (length (car grid)))
              (longest (max rows cols))
              (margin (+ 1 (floor (/ (mod +screen-height+ longest)) 2)))
              (cell-size (floor (/ (- +screen-height+ (* 2 margin)) longest)))
              (v-margin (floor ( / (- +screen-height+ (* rows cell-size)) 2)))
              (h-margin (floor ( / (- +screen-height+ (* cols cell-size)) 2))))
        (move h-margin (- +screen-height+ v-margin))
        (draw-grid 
          (floor (/ 
                    (- +screen-height+ (* 2 margin))
                    (max rows cols)))
          grid)))
    (t
      (let* ((margin (+ 1 (floor (/ (mod +screen-height+ (+ 1 +viewport-size+)) 2)))))
        (move margin (- +screen-height+ margin))
        (draw-grid
              (floor (/ (min +screen-height+ +screen-width+) (+ 1 +viewport-size+))) ; +1 is to countwith player position
              (get-viewport maze))))))

;; Definition: Paints the first instruction in the given
;; In:
;;   - Instructions: The instruction list
;;   - x: The character column
;;   - y: The character row
;; Out: None
(defun draw-instruction (instructions x y)
  (cond 
    ((null instructions) nil)
    (t
      (goto-xy x y)
      (color 0 0 0)
      (princ (car instructions))
      (draw-instruction
        (cdr instructions)
        x
        (+ y 1)))))

;; Definition: Paints the instructions
;; In: None
;; Out: None
(defun draw-instructions ()
  (draw-instruction 
    +instructions+
    (+ 2 (ceiling (/ +screen-height+ +char-width+)))
    2))

;; Definition: Clears the screen and paints the basic UI
;; In: None
;; Out: None
(defun clear-screen ()
  (cls)
  (setq painted '())
  (color 0 0 0)
  (draw-instructions))