
(defun load-game (file)
  (cls)
  (play (load-maze file)))

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
      (cls)
      (play (switch-minimap maze)))
    ((= key 27)
      t)              ; Esc
    (t (play maze)))) ; Repeat