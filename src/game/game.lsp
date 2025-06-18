;; Definition: Starts the game on the given maze file
;; In: 
;;    - f: The file where the maze is stored
;; Out: None
(defun start-game (f)
  (clear-screen)
  (setq steps 0)
  (setq file f)
  (setq start-time (get-internal-real-time))
  (play (load-maze f)))

;; Definition: Play recursive function
;; In: 
;;    - maze: The maze of the game
;; Out: None
(defun play (maze)
  (cond
    ((check-win maze) (end-game maze))
    (t 
      (draw-maze maze)
      (setq key (get-key))
      (cond
        ((member key up-keys)
         (play (move-up maze)))
        ((member key left-keys)
         (play (move-left maze)))
        ((member key down-keys)
         (play (move-down maze)))
        ((member key right-keys)
         (play (move-right maze)))
        ((member key map-keys)
         (clear-screen)
         (play (switch-minimap maze)))
        ((= key esc-key) t)
        (t (play maze))))))

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

(defun end-game (maze)
  (save-stats)
  (cls)
  (color 0 0 0)
  (format t "You have completed the maze in ~a steps" steps)
  (display-stats file))