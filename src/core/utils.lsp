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

;; Definition: Returns a list of elements from the input list for which the predicate returns true.
;; In:
;; - pred = A function that takes an element and returns true or false
;; - lst = The input list
;; Out: A list containing only the elements of lst for which pred returns true
(defun remove-if-not (pred lst)
  (cond ((null lst) nil)
        ((funcall pred (car lst)) (cons (car lst) (remove-if-not pred (cdr lst))))
        (t (remove-if-not pred (cdr lst)))
  )
)

;; Definition: Returns a random element from a non-empty list
;; In:
;;   - lst: a non-empty list
;; Out:
;;   - an element randomly selected from lst
(defun random-element (lst)
  (nth (random (length lst)) lst)
)

;; Definition: Returns the cell at the given position in the grid
;; In:
;;   - grid: The 2D list (list of lists) representing the maze
;;   - row: Row of the position
;;   - col: Column of the position
;; Out:
;;   - The cell at the specified position
(defun get-cell (grid row col)
  (nth col (nth row grid))
)

;; Definition: Swaps the given positions in a list.
;; In:
;;   - lst: The list in which to swap positions.
;;   - i: Index of the first element.
;;   - j: Index of the second element.
;; Out:
;;   - A new list with the elements at positions i and j swapped.
(defun swap (lst i j)
  (let ((val-i (nth i lst))
        (val-j (nth j lst)))
    (swap-helper lst i j 0 val-i val-j)
  )
)

;; Definition: Helper function for `swap` that traverses the list recursively,
;;             replacing the elements at positions i and j with the swapped values.
;; In:
;;   - lst: The list being traversed.
;;   - i: The first position to swap.
;;   - j: The second position to swap.
;;   - index: The current index during traversal.
;;   - val-i: The element originally at position i.
;;   - val-j: The element originally at position j.
;; Out:
;;   - A new list where elements at positions i and j are swapped,
;;     and all other elements remain the same.
(defun swap-helper (lst i j index val-i val-j)
  (cond
    ((null lst) nil)
    (t
     (let ((x (car lst)))
       (cons
         (cond
           ((= index i) val-j)
           ((= index j) val-i)
           (t x))
         (swap-helper (cdr lst) i j (+ index 1) val-i val-j))))
  )
)

;; Definition: Recursively applies Fisher-Yates shuffle logic.
;; In:
;;   - lst: List to shuffle.
;;   - n: Current length (starts as (length lst)).
;; Out:
;;   - A new list with elements randomly shuffled.
(defun shuffle-rec (lst n)
  (cond
    ((<= n 1) lst)
    (t (let* ((j (random n))
              (swapped (swap lst j (- n 1))))
         (shuffle-rec swapped (- n 1))))
  )
)

;; Definition: Shuffles a list using Fisher-Yates algorithm.
;; In:
;;   - lst: A list of elements.
;; Out:
;;   - A new list with the same elements in random order.
(defun shuffle (lst)
  (shuffle-rec lst (length lst))
)