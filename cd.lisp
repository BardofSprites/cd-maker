(require 'asdf)

(asdf:load-system :uiop)

(defun count-lines (path)
  ;; count the number of lines in a file at `path`
  (let ((lines (uiop:read-file-lines (uiop:native-namestring path)))
        (count 0))
    (dolist (line lines count)
      (setf count (1+ count)))))

(defun read-playlist (path)
  (if (probe-file (uiop:native-namestring path))
      (progn
        (format t "Playlist found: ~A songs in playlist" (count-lines path)))
      (progn
        (format t "Playlist file doesn't exist: ~a~%" (uiop:native-namestring))
        (return-from read-playlist nil))))

(defun read-playlist-debug (path)
  (if (probe-file (uiop:native-namestring path))
      (progn
        (format t "File exists, do you want to print? (y/n): ")
        (let ((response (read-line)))
          (if (string= response "y")
              (progn (format t "Printing the file...~%")
                     (uiop:read-file-lines path))
              (format t "File not printed.~%"))))
      (progn
        (format t "Playlist file doesn't exist: ~a~%" (uiop:native-namestring))
        (return-from read-playlist nil))))

;; (defun create-fs (destination file-num))
