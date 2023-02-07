(defpackage #:trivial-stream-column
  (:use #:common-lisp)
  (:documentation "A standard interface to the various stream column functions.")
  #+clasp
  (:inport-from #:gray
                #:stream-line-length)
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
           #:copy-style
           #:line-column
           #:line-length
           #:measure-char
           #:measure-string
           #:scale-column
           #:set-style
           #:start-line-p
           #:stream-copy-style
           #:stream-line-length
           #:stream-measure-char
           #:stream-measure-string
           #:stream-scale-column
           #:stream-style
           #:style))

