;; Function: write-maze
;; Description: Writes content to the specified file in maze path. If the file does not exist,
;;              it is created. Content is written character by character.
;; In:
;;   - name: String. Name (or path) of the file to write to.
;;   - content: List of characters to write into the file.
;; Out: None
(defun write-maze (name content)
  (let ((fp (open (concatenate 'string +maze-path+ name) :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)))
       (write-aux fp content)
       (close fp)))

;; Function: write-stats
;; Description: Writes content to the specified file in stats path. If the file does not exist,
;;              it is created.
;; In:
;;   - name: String. Name (or path) of the file to write to.
;;   - content: Expression to write into the file.
;; Out: None
(defun write-stats (name content)
  (let ((fp (open (concatenate 'string +stats-path+ name) :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)))
       (print content fp)
       (close fp)))

;; Function: write-aux
;; Description: Helper function for 'write'. Writes a list of characters to an open file stream.
;; In:
;;   - fp: Output file stream.
;;   - content: List of characters to write.
;; Out: None
(defun write-aux (fp content)
  (cond
    ((null content) nil)
    (t (write-char (car content) fp)
       (write-aux fp (cdr content)))))

;; Description: Reads content from the specified file in maze path, character 
;;              by character and returns it as a list.
;; In:
;;   - name: String. Name (or path) of the file to read.
;; Out: List of characters read from the file.
(defun read-maze (name)
  (let* ((fp (open (concatenate 'string +maze-path+ name) :direction :input))
         (content (read-aux fp)))
        (close fp)
        content))

;; Description: Reads content from the specified file in stats path.
;; In:
;;   - name: String. Name (or path) of the file to read.
;; Out: Expression read from the file.
(defun read-stats (name)
  (let* ((fp (open (concatenate 'string +stats-path+ name)))
         (content (read fp nil nil)))
        (close fp)
        content))

;; Function: read-aux
;; Description: Helper function for 'read'. Reads all characters from an open file stream and 
;;              returns them as a list.
;; In:
;;   - fp: Input file stream.
;; Out: List of characters read.
(defun read-aux (fp)
  (let ((c (read-char fp nil nil)))
       (cond ((null c) '())
             (t (cons c (read-aux fp))))))