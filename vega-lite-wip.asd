(asdf:defsystem "vega-lite-wip"
  :depends-on ("alexandria"
               "hunchentoot"
               "shasht")
  :license "MIT"
  :author "Shubhamkar Ayare (digikar@proton.me)"
  :version "0.0.0"
  :components ((:file "package")
               (:file "utils")
               (:file "server")
               (:file "vega-lite")
               (:file "high-level")))
