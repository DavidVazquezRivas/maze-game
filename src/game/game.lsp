;; Definition: Starts the game on the given maze file
;; In: 
;;    - f: The file where the maze is stored
;; Out: None
(defun start-game (f)
  (clear-screen)
  (play (load-maze f) 0 f (get-internal-real-time)))

;; Definition: Play recursive function
;; In: 
;;    - maze: The maze of the game
;;    - steps: Steps made on the maze
;;    - file: The file of the maze
;;    - start-time: Moment where the game started
;; Out: None
(defun play (maze steps file start-time)
  (cond
    ((check-win maze) (end-game maze steps file start-time))
    (t 
      (draw-maze maze)
      (setq key (get-key))
      (cond
        ((member key up-keys)
         (play (move-up maze) (+ 1 steps) file start-time))
        ((member key left-keys)
         (play (move-left maze) (+ 1 steps) file start-time))
        ((member key down-keys)
         (play (move-down maze) (+ 1 steps) file start-time))
        ((member key right-keys)
         (play (move-right maze) (+ 1 steps) file start-time))
        ((member key map-keys)
         (clear-screen)
         (play (switch-minimap maze) steps file start-time))
        ((= key esc-key) t)
        (t (play maze steps file start-time))))))

;; Definition: Checks if the game is won
;; In: 
;;    - maze: The maze of the game
;; Out: None
(defun check-win (maze)
  (string= 
    +cell-type-exit+
    (car  (get-cell 
            (get-grid maze)
            (get-current-row maze)
            (get-current-col maze)))))

;; Definition: Game ending actions (save stats and display)
;; In: 
;;    - maze: The maze of the game
;;    - steps: Steps made on the maze
;;    - file: The file of the maze
;;    - start-time: Moment where the game started
;; Out: None
(defun end-game (maze steps file start-time)
  (save-stats steps file start-time)
  (cls)
  (color 0 0 0)
  (format t "You have completed the maze in ~a steps\n" steps)
  (format t "~a~%" (display-stats file)))