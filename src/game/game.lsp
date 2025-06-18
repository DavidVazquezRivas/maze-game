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
    ((= key 119)      ; 'w'
     (play (move-up maze)))
    ((= key 97)       ; 'a'
     (play (move-left maze)))
    ((= key 115)      ; 's'
     (play (move-down maze)))
    ((= key 100)      ; 'd'
     (play (move-right maze)))
    ((= key 109)      ; 'm'
      (clear-screen)
      (play (switch-minimap maze)))
    ((= key 27)
      t)              ; Esc
    (t (play maze)))) ; Repeat