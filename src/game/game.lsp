;; Definition: Starts the game on the given maze file
;; In: 
;;    - file: The file where the maze is stored
;; Out: None
(defun start-game (file)
  (clear-screen)
  (play (load-maze file)))

;; Definition: Play recursive function
;; In: 
;;    - maze: The maze of the game
;; Out: None
(defun play (maze)
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
    ((= key esc-key)
     t)
    (t (play maze))))