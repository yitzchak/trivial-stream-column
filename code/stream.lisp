(in-package #:trivial-stream-column)

(defun frob-stream (stream)
  (cond ((null stream)
         *standard-output*)
        ((eql stream t)
         *terminal-io*)
        (t
         stream)))

#-(or cmucl mezzano sbcl sicl)
(defgeneric stream-line-length (stream)
  (:method (stream)
    (declare (ignore stream))
    #+acl (excl:stream-output-width stream)
    #-acl nil))

#-(or mezzano sicl)
(defun line-column (&optional stream)
  #+abcl (ext:charpos (frob-stream stream))
  #+acl (excl:charpos (frob-stream stream))
  #+(or clasp ecl) (sys:file-column (frob-stream stream))
  #+cmucl (lisp::charpos (frob-stream stream))
  #+sbcl (sb-kernel:charpos (frob-stream stream))
  #-(or abcl acl clasp cmucl ecl sbcl) nil)

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
  #+cmucl (lisp::stream-api-dispatch (str)
            :native (advance-to-column/fallback column str)
            :simple (advance-to-column/fallback column str)
            :gray (ext:stream-advance-to-column str column))
  #+sbcl (sb-impl::stream-api-dispatch (str)
           :native (advance-to-column/fallback column str)
           :simple (advance-to-column/fallback column str)
           :gray (sb-gray:stream-advance-to-column str column))
  #-(or cmucl sbcl) (advance-to-column/fallback column str))

(defun start-line-p/fallback (stream)
  (eql 0 (line-column stream)))

#-(or mezzano sicl)
(defun start-line-p (&optional stream &aux (str (frob-stream stream)))
  #+cmucl (lisp::stream-api-dispatch (str)
            :native (start-line-p/fallback str)
            :simple (start-line-p/fallback str)
            :gray (ext:stream-start-line-p str))
  #+sbcl (sb-impl::stream-api-dispatch (str)
           :native (start-line-p/fallback str)
           :simple (start-line-p/fallback str)
           :gray (sb-gray:stream-start-line-p str))
  #-(or cmucl sbcl) (start-line-p/fallback str))

(defgeneric stream-measure-char (stream char))

(defmethod stream-measure-char (stream char)
  (declare (ignore stream char))
  1)

(defun measure=char (char &optional stream)
  (check-type char character)
  (stream-measure-char (frob-stream stream) char))

(defgeneric stream-measure-string (stream string &optional start end))

(defmethod stream-measure-string (stream string &optional start end)
  (declare (ignore stream))
  (- (or end (length string))
     (or start 0)))

(defun measure-string (string &optional stream &key start end)
  (stream-measure-string (frob-stream stream) string (or start 0) (or end (length string))))
  
