(asdf:defsystem "vega-lite-wip"
  :depends-on ("alexandria"
               "closer-mop"
               "hunchentoot"
               "hunchensocket"
               "shasht")
  :license "MIT"
  :author "Shubhamkar Ayare (digikar@proton.me)"
  :version "0.0.0"
  :pathname #p"src/"
  :components ((:file "package")
               (:file "utils")
               (:file "server")
               (:file "spec")
               (:file "compose")
               (:file "vega-lite")
               (:file "encoding")
               (:file "high-level")))
