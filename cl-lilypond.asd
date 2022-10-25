;;;; cl-lilypond.asd

(asdf:defsystem #:cl-lilypond
  :description "A library to output Lilypond code."
  :author "Nuno Trocado"
  :license  "Apache 2.0"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "cl-lilypond")))
