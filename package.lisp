;;;; package.lisp

(defpackage #:cl-lilypond
  (:use #:cl)
  (:export
   #:*lilypond-path*
   #:note->ly-pitch
   #:*template-melody*
   #:*template-harmony*
   #:ly-melody
   #:ly-harmony
   #:run-lilypond))
