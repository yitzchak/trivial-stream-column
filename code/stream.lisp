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
  #+sbcl (sb-impl::stream-api-dispatch (str)
           :native (advance-to-column/fallback column str)
           :simple (advance-to-column/fallback column str)
           :gray (sb-gray:stream-advance-to-column str column))
  #-sbcl (advance-to-column/fallback column str))

(defun start-line-p/fallback (stream)
  (eql 0 (line-column stream)))

#-(or mezzano sicl)
(defun start-line-p (&optional stream &aux (str (frob-stream stream)))
  #+sbcl (sb-impl::stream-api-dispatch (str)
           :native (start-line-p/fallback str)
           :simple (start-line-p/fallback str)
           :gray (sb-gray:stream-start-line-p str))
  #-sbcl (start-line-p/fallback str))

(defgeneric stream-text-length (stream text)
  (:method (stream text)
    (declare (ignore stream))
    nil))

(defmethod stream-text-length (stream (text charactor))
  (declare (ignore stream text))
  1)

(defmethod stream-text-length (stream (text string))
  (declare (ignore stream text))
  (length string))

(defun text-length (&optional stream)
  (stream-text-length (frob-stream stream)))
  
