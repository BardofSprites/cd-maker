(require 'asdf)

(asdf:load-system :uiop)
(ql:quickload :id3v2)
(ql:quickload :osicat)

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

(defun check-if-space (playlist-file)
  (let ((songs (read-playlist playlist-file))
        (total-size 0))
    (dolist (file songs total-size)
      (when (probe-file file)
        (let ((file-size (osicat-posix:stat-size (osicat-posix:stat file))))
          (incf total-size file-size))))))

(defun copy-songs (songs idx dest-dir)
  ;; This function copies all songs in the same album to the album directory
  (let* ((album (id3v2:mp3-album (id3v2:read-mp3-file (first songs))))
         (album-dir (make-pathname :directory (list :relative (format nil "~2,'0d - ~a" idx album))))
         (copy-dir (merge-pathnames album-dir dest-dir)))
    ;; Ensure the album directory exists
    (ensure-directories-exist copy-dir)
    ;; Copy each song in the album to the album directory
    (loop for song in songs
          do (let* ((file-name (pathname-name (parse-namestring song)))
                    (destination-file (merge-pathnames (make-pathname :name file-name :type "mp3") copy-dir)))
               (uiop:copy-file song destination-file)))))

(defun create-fs (playlist-file dest-path)
  ;; Creates the directory structure as described in README.org
  (let* ((playlist-list (group-by-album playlist-file))) ;; Assume list of album-song pairs
    ;; Create album directories and copy songs
    (loop for (album songs) in playlist-list
          for idx from 1
          do (copy-songs songs idx dest-path))))


;; get all songs from hash (mapcan (lambda (album) (cadr album)) playlist-hash)

;; get all albums from hash (mapcar #'first playlist-hash)
