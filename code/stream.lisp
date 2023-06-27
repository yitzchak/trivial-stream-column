(in-package #:trivial-stream-column)

(defun fundamental-character-output-stream-p (stream)
  (or #+(or abcl allegro ccl clasp clisp cmucl ecl genera lispworks mezzano mkcl mocl sbcl sicl)
      (typep stream #+sbcl 'sb-gray:fundamental-character-output-stream
                    #+allegro 'excl:fundamental-character-output-stream
                    #+cmucl 'ext:fundamental-character-output-stream
                    #+(or clisp ecl mocl clasp mkcl) 'gray:fundamental-character-output-stream
                    #+ccl 'ccl:fundamental-character-output-stream
                    #+lispworks 'stream:fundamental-character-output-stream
                    #+(or abcl genera) 'gray-streams:fundamental-character-output-stream
                    #+mezzano 'mezzano.gray:fundamental-character-output-stream
                    #+sicl 'cyclosis:fundamental-character-output-stream)
      #+(or ecl mkcl)
      (find-method #'trivial-gray-streams:stream-line-column nil
                   (list (class-of stream)) nil)))

(defun frob-stream (stream)
  (cond ((null stream)
         *standard-output*)
        ((eql stream t)
         *terminal-io*)
        (t
         stream)))

#-(or clasp cmucl mezzano sbcl sicl)
(defgeneric stream-line-length (stream)
  (:method (stream)
    (declare (ignore stream))
    #+allegro (excl:stream-output-width stream)))

#-(or mezzano sicl)
(defun line-column (&optional stream &aux (str (frob-stream stream)))
  #+abcl (ext:charpos str)
  #+allegro (excl:charpos str)
  #+clasp (trivial-gray-streams:stream-line-column str)
  #+ccl (ccl::column str)
  ;; This is a hack. Both ECL uses C based dispatch inside
  ;; sys:file-column that truncates STREAM-LINE-COLUMN to an int.
  ;; In order To allow stream-line-column to return a real number
  ;; we try to detect Gray streams and bypass the dispatch.
  #+(or ecl mkcl)
    (if (fundamental-character-output-stream-p str)
        (trivial-gray-streams:stream-line-column str)
        (sys:file-column str))
  #+cmucl (lisp::charpos str)
  #+sbcl (sb-kernel:charpos str))

#-(or cmucl mezzano sbcl sicl)
(defun line-length (&optional stream)
  (stream-line-length (frob-stream stream)))

(defun advance-to-column/fallback (column stream)
  (loop for line-column = (line-column stream)
        while (and line-column (< line-column column))
        do (write-char #\Space stream)
        finally (return (and line-column t))))

#-(or mezzano sicl)
(defun advance-to-column (column &optional stream &aux (str (frob-stream stream)))
  #+cmucl (lisp::stream-dispatch str
            (advance-to-column/fallback column str)
            (advance-to-column/fallback column str)
            (ext:stream-advance-to-column str column))
  #+sbcl (sb-impl::stream-api-dispatch (str)
           :native (advance-to-column/fallback column str)
           :simple (advance-to-column/fallback column str)
           :gray (sb-gray:stream-advance-to-column str column))
  #-(or cmucl sbcl) (advance-to-column/fallback column str))

(defun start-line-p/fallback (stream)
  (eql 0 (line-column stream)))

#-(or mezzano sicl)
(defun start-line-p (&optional stream &aux (str (frob-stream stream)))
  #+cmucl (lisp::stream-dispatch str
            (start-line-p/fallback str)
            (start-line-p/fallback str)
            (ext:stream-start-line-p str))
  #+sbcl (sb-impl::stream-api-dispatch (str)
           :native (start-line-p/fallback str)
           :simple (start-line-p/fallback str)
           :gray (sb-gray:stream-start-line-p str))
  #-(or cmucl sbcl) (start-line-p/fallback str))

(defgeneric stream-style (stream))

(defun style (&optional stream)
  (stream-style (frob-stream stream)))

(defgeneric (setf stream-style) (new-style stream))

(defun set-style (style &optional stream)
  (setf (stream-style (frob-stream stream)) style))

(defgeneric stream-copy-style (stream style &rest overrides &key &allow-other-keys))

(defun copy-style (style &optional stream &rest overrides &key &allow-other-keys)
  (apply #'stream-copy-style (frob-stream stream) style overrides))

(defgeneric stream-scale-column (stream column old-style new-style))

(defun scale-column (column &optional stream &key old-style new-style)
  (stream-scale-column (frob-stream stream)
                       column old-style new-style))

(defmethod stream-style (stream)
  (declare (ignore stream))
  nil)

(defmethod (setf stream-style) (new-style stream)
  (declare (ignore stream))
  new-style)

(defmethod stream-copy-style (stream style &rest overrides &key &allow-other-keys)
  (declare (ignore stream style overrides))
  nil)

(defmethod stream-scale-column (stream column old-style new-style)
  (declare (ignore stream old-style new-style))
  column)

(defgeneric stream-measure-char (stream char &optional style))

(defmethod stream-measure-char (stream char &optional style)
  (declare (ignore stream char style))
  1)

(defun measure-char (char &optional stream &key style)
  (check-type char character)
  (stream-measure-char (frob-stream stream) char style))

(defgeneric stream-measure-string (stream string &optional start end style))

(defmethod stream-measure-string (stream string &optional start end style)
  (declare (ignore stream style))
  (- (or end (length string))
     (or start 0)))

(defun measure-string (string &optional stream &key start end style)
  (stream-measure-string (frob-stream stream) string
                         (or start 0) (or end (length string))
                         style))
  
