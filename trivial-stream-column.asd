(asdf:defsystem #:trivial-stream-column
  :description "A standard interface to the various stream column functions."
  :author "Tarn W. Burton"
  :license "MIT"
  :version "0.1"
  :homepage "https://yitzchak.github.io/trivial-stream-column/"
  :bug-tracker "https://github.com/yitzchak/trivial-stream-column/issues"
  :depends-on ((:feature (:not :sicl) #:trivial-gray-streams))
  :components
    ((:module code
      :serial t
      :components
        ((:file "packages")
         (:file "stream")))))
