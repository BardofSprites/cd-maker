(require 'asdf)

(asdf:load-system :uiop)
(ql:quickload :id3v2)
(ql:quickload :osicat)
(ql:quickload :cl-ppcre)

(defun read-playlist (playlist)
  "Reads playlist into list"
  (if (probe-file (uiop:native-namestring playlist))
      (uiop:read-file-lines playlist)
      (progn
        (format t "Playlist file doesn't exist: ~a~%" (uiop:native-namestring))
        (return-from read-playlist nil))))

(defun count-playlist (playlist)
  "Count songs in playlist"
  (let ((songs (read-playlist playlist))
        (count 0))
    (loop for song in songs
          do (incf count))
    count))

(defun check-playlist-formation (playlist)
  "Checks to make sure all files in playlist exist"
  (let ((files (read-playlist playlist)))
    (file-or-dir files)))

(defun group-by-album (playlist-file)
  "Returns a hash table of songs grouped by album
  example: '((album-1 (song-1-from-album-1 song-2-from-album-1))
             (album-2 (song-2-from-album-2 song-2-from-album-2)))"
  (let* ((mp3-files (read-playlist playlist-file)))
    (loop with album-grouped = (make-hash-table :test #'equal)
          for file in mp3-files
          for album = (id3v2:mp3-album (id3v2:read-mp3-file file))
          do (push file (gethash album album-grouped))
          finally (return
                    (loop for album being the hash-keys of album-grouped
                          collect (list album (reverse (gethash album album-grouped))))))))

(defun check-if-space (playlist-file)
  "Returns size of all songs in `playlist` in megabytes"
  (let ((songs (read-playlist playlist-file))
        (total-size 0))
    (dolist (file songs total-size)
      (when (probe-file file)
        (let ((file-size (osicat-posix:stat-size (osicat-posix:stat file))))
          (incf total-size (/ file-size 1024.0 1024.0)))))
    total-size))

(defun copy-songs (songs idx dest-dir)
  "Copies all songs in the same album to the album directory"
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

(defun create-playlist-dir (playlist dest-path)
  "Create playlist directory in directory structure as described in README.org"
  (file-or-dir (list dest-path playlist))
  (let* ((playlist-dest-path (format nil "~A/00 - Playlists/" dest-path))
         (full-path (format nil "~A~A" playlist-dest-path (file-namestring playlist))))
    (ensure-directories-exist playlist-dest-path)
    (uiop:copy-file playlist full-path)))

(defun create-fs (playlist dest-path)
  "Creates the directory structure as described in README.org"
  (file-or-dir (list dest-path playlist))
  (let* ((playlist-list (group-by-album playlist))
         (dest-path (uiop:ensure-directory-pathname dest-path)))
    (loop for (album songs) in playlist-list
          for idx from 1
          do (copy-songs songs idx dest-path))
    (create-playlist-dir playlist dest-path)))

(defun change-path (original-path destination-directory album-index)
  "Change the path from original playlist path in .m3u to destination-directory/Album-number-album-name/songname.mp3"
  (let* ((split-path (cl-ppcre:split "/" original-path))
         ;; Extract the song name from the original path
         (song-with-extension (nth (1- (length split-path)) split-path))
         (song-name (first (cl-ppcre:split "\\.mp3" song-with-extension)))
         ;; Extract the album name using ID3v2 tags
         (album-name (id3v2:mp3-album (id3v2:read-mp3-file original-path)))
         ;; Format the album directory with the album index
         (album-dir (format nil "~2,'0d - ~a" album-index album-name))
         ;; Create the new path using the destination directory, album directory, and song name
         (new-path (concatenate 'string destination-directory
                                "/" album-dir
                                "/" song-name ".mp3")))
    ;; Return the new path
    new-path))

(defun reform-playlist (playlist destination-directory)
  "Reform the playlist paths according to the new directory structure."
  (let* ((playlist-list (group-by-album playlist))) ; Group songs by album
    (loop for (album songs) in playlist-list
          for idx from 1
          append (loop for song in songs
                       ;; Update each song's path using change-path
                       collect (change-path song destination-directory idx)))))

(defun write-reformed-playlist (song-paths output-file)
  "Writes the list of song paths to an output .m3u playlist file."
  (with-open-file (stream output-file :direction :output :if-does-not-exist :create :element-type 'character)
    (dolist (song-path song-paths)
      (format stream "~a~%" song-path))))

(defun file-or-dir (files)
  "Errors if any files in list are nonexistent"
  (let ((results (mapcar (lambda (path)
                           (or (probe-file path) (directory path)))
                         files)))
    (unless (every #'identity results)
      (error "One or more paths are invalid: ~a"
             (remove-if #'identity (mapcar #'(lambda (path result)
                                               (when (null result) path))
                                           files results))))
    results))



;; main function
;; ask for playlist, count songs, create file system
