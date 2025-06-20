;; Stat "object":
;; A list representing the current state of the maze:
;; (
;;   steps: Number of steps to solve
;;   time: Number of seconds took to solve
;; )

;; Definition: Saves the stats of the game just played
;; In: 
;;    - steps: Steps made on the maze
;;    - file: The file of the maze
;;    - start-time: Moment where the game started
;; Out: None
(defun save-stats (steps file start-time)
    (store-stats 
      file 
      (sort-stats-by-steps 
        (cons 
          (list 
            steps 
            (- (get-internal-real-time) start-time)) 
          (load-stats file)))))

;; Definition: Loads stats of the given file
;; In:
;;   - f: The file to read
;; Out:
;;   - The stats of the file, in stat list format
(defun load-stats (f)
  (read-stats f))

;; Definition: Write a list of stats to a file
;; In:
;;   - f: The file to write
;; Out: None
(defun store-stats (f stats)
  (write-stats f stats))

;; Definition: Sorts a stat list by steps count
;; In:
;;   - stats: Stat list
;; Out: Stat list ordered
(defun sort-stats-by-steps (stats)
  (sort stats (lambda (a b) (< (car a) (car b)))))

;; Definition: Sorts a stat list by tim
;; In:
;;   - stats: Stat list
;; Out: Stat list ordered
(defun sort-stats-by-time (stats)
  (sort stats (lambda (a b) (< (cadr a) (cadr b)))))

;; Definition: Formats a stat to be printed
;; In:
;;   - stat: The stat
;; Out: String representing the stat
(defun format-stat (stat)
  (format nil "Steps: ~A, Time: ~A milliseconds~%" (car stat) (cadr stat)))

;; Definition: Display top stats from a file
;; In:
;;   - file: The maze stat file
;; Out: String to be printed
(defun display-stats (file)
  (let* ((stats (load-stats file)))
    (concatenate 'string
                 "=== Top " (format nil "~a" +top-limit+) " by Steps ===\n"
                 (mapconcat 
                  #'format-stat 
                  (subseq 
                    (sort-stats-by-steps stats)
                    0 
                    (min +top-limit+ (length stats)))
                  "")
                 "\n=== Top " (format nil "~a" +top-limit+) " by Time ===\n"
                 (mapconcat 
                  #'format-stat 
                  (subseq 
                    (sort-stats-by-time stats) 
                    0 
                    (min +top-limit+ (length stats)))
                  ""))))