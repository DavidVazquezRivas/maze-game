;; Definition: Returns a new list where the element at the given index is replaced
;; In:
;;    - lst = The original list
;;    - index = The position of the element to replace
;;    - new-elem = The element that will replace the original at that position
;; Out: A new list with the specified element replaced
(defun replace-in-list (lst index new-elem)
  (cond
    ((null lst) nil)
    ((= index 0) (cons new-elem (cdr lst)))
    (t (cons 
      (car lst)
      (replace-in-list (cdr lst) (- index 1) new-elem)
    ))
  )
)

;; Definition: Returns a new grid replacing the element at the given row and column
;; In:
;;    - grid = The 2D list (grid) to copy and modify
;;    - row = The row index of the target element
;;    - col = The column index of the target element
;;    - new-elem = The element to place at the specified position
;; Out: A new grid with the element replaced at (row, col)
(defun replace-in-grid (grid row col new-elem)
  (replace-in-list 
    grid 
    row 
    (replace-in-list (nth row grid) col new-elem)
  )
)

;; Definition: Applies a function to the cell located on the given side of the grid,
;;             at a random non-corner position, and returns the updated grid.
;; In:
;;    - side = The side of the grid (0 = top, 1 = bottom, 2 = left, 3 = right)
;;    - grid = The 2D list (grid) to modify
;;    - n = Number of rows
;;    - m = Number of columns
;;    - f = A function to apply to the target cell
;; Out: A new grid with the cell on the given side modified by the function
(defun apply-on-side (grid n m f side)
  (cond
    ((= side 0) ; top row
     (let ((col (+ 1 (random (- m 2)))))
       (replace-in-grid grid 0 col (funcall f (nth col (nth 0 grid))))))
    ((= side 1) ; bottom row
     (let ((col (+ 1 (random (- m 2)))))
       (replace-in-grid grid (- n 1) col (funcall f (nth col (nth (- n 1) grid))))))
    ((= side 2) ; left column
     (let ((row (+ 1 (random (- n 2)))))
       (replace-in-grid grid row 0 (funcall f (nth 0 (nth row grid))))))
    ((= side 3) ; right column
     (let ((row (+ 1 (random (- n 2)))))
       (replace-in-grid grid row (- m 1) (funcall f (nth (- m 1) (nth row grid))))))
    (t grid)
  )
)

;; Definition: Returns the opposite side of a given side.
;; In:
;;    - side = An integer representing a side (0 = top, 1 = bottom, 2 = left, 3 = right)
;; Out: The integer representing the opposite side
(defun opposite-side (side)
  (cond
    ((= side 0) 1)
    ((= side 1) 0)
    ((= side 2) 3)
    ((= side 3) 2)
    (t side) ; fallback if invalid input
  )
) 