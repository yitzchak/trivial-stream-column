(defpackage #:trivial-stream-column
  (:use #:common-lisp)
  (:documentation "A standard interface to the various stream column functions.")
  #+cmucl
  (:import-from #:ext
                #:stream-line-length)
  #+cmucl
  (:import-from #:lisp
                #:line-length)
  #+mezzano
  (:import-from #:mezzano.internals
                #:advance-to-column
                #:line-column
                #:line-length
                #:start-line-p)
  #+mezzano
  (:import-from #:mezzano.gray
                #:stream-line-length)
  #+sbcl
  (:import-from #:sb-kernel
                #:line-length)
  #+sbcl
  (:import-from #:sb-gray
                #:stream-line-length)
  #+sicl
  (:import-from #:cyclosis
                #:advance-to-column
                #:line-column
                #:line-length
                #:start-line-p
                #:stream-line-length)
  (:export #:advance-to-column
           #:line-column
           #:line-length
           #:start-line-p
           #:stream-line-length
           #:stream-text-length
           #:text-length))

