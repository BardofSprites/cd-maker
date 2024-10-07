(require 'asdf)

(asdf:load-system :uiop)
(ql:quickload :id3v2)

(defun count-lines (path)
  ;; count the number of lines in a file at `path`
  (let ((lines (uiop:read-file-lines (uiop:native-namestring path)))
        (count 0))
    (dolist (line lines count)
      (setf count (1+ count)))))

(defun count-playlist (path)
  (if (probe-file (uiop:native-namestring path))
      (progn
        (format t "Playlist found: ~A songs in playlist" (count-lines path)))
      (progn
        (format t "Playlist file doesn't exist: ~a~%" (uiop:native-namestring))
        (return-from read-playlist nil))))

(defun read-playlist (path)
  (if (probe-file (uiop:native-namestring path))
      (uiop:read-file-lines path)
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

(defun group-by-album (playlist-file)
  ;; returns a hash table of songs grouped by album
  ;; example: '((album-1 (song-1-from-album-1 song-2-from-album-1))
  ;;            (album-2 (song-2-from-album-2 song-2-from-album-2)))
  (let* ((mp3-files (read-playlist playlist-file)))
    (loop with album-grouped = (make-hash-table :test #'equal)
          for file in mp3-files
          for album = (id3v2:mp3-album (id3v2:read-mp3-file file))
          do (push file (gethash album album-grouped))
          finally (return
                    (loop for album being the hash-keys of album-grouped
                          collect (list album (reverse (gethash album album-grouped))))))))

;; (defun create-fs (playlist-file)
;;   (let* ((playlist-hash (group-by-album playlist-file))
;;          (album-count (length playlist-hash))
;;          ))
;;   ;; create album directories
;;   )


;; get all songs from hash (mapcan (lambda (album) (cadr album)) (group-by-album "~/Music/Playlists/playlist.m3u"))

;; get all albums from hash (mapcar 'cdr (group-by-album "~/Music/Playlists/playlist.m3u"))
