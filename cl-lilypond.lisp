;;;; cl-lilypond.lisp

(in-package #:cl-lilypond)

(defparameter *lilypond-path*
  #+(or windows os-windows win32) "C:/Program Files (x86)/LilyPond/usr/bin/lilypond.exe"
  #+linux "/app/.apt/usr/bin/lilypond.real")

(defgeneric note->ly-pitch (note)
    (:documentation "Generates a lilypond string for a given pitch."))

(defmethod note->ly-pitch ((note pitch-spelling:note))
  (format nil "~a~a~a"
	  (pitch-spelling:letter note)
	  (case (pitch-spelling:accidental note)
	    (:double-flat "eses")
	    (:flat "es")
	    (:natural "")
	    (:sharp "is")
	    (:double-sharp "isis"))
	  (let ((oct (pitch-spelling:octave note)))
	    (make-string (abs (- 3 oct))
			 :initial-element (if (> oct 3)
					      #\'
					      #\,)))))

(defmethod note->ly-pitch ((note integer))
  (note->ly-pitch (car (pitch-spelling:pitch-spell (list note)))))

(defmethod note->ly-pitch ((note (eql :rest)))
  "r")

(defmethod note->ly-pitch ((note string))
  note)
		
(defun final-rest (pitches durations &optional (beats-per-bar 4))
  "Fills up the last bar with rests."
  (let ((total (reduce #'+ durations)))
    (if (zerop (mod total beats-per-bar))
	(values pitches durations)
	(let ((missing-dur (loop :for x :by beats-per-bar
				 :when (> x total)
				   :return (- x total))))
	  (if (pitch-spelling:rest-p (alexandria:last-elt pitches))
	      (values pitches (append (butlast durations) (list (+ (alexandria:last-elt durations)
								   missing-dur))))
	      (values (append pitches (list :rest)) (append durations (list missing-dur))))))))

(defparameter *template-melody*
  "\\score {
  \\new Staff {
    \\clef treble
    ~a \\bar \"|.\"~%
  }
}")

(defparameter *template-harmony*
  "\\score { 
  \\new Staff { 
    \\omit Score.TimeSignature 
    \\omit Score.BarNumber 
    \\omit Score.BarLine
    \\time 1/4 
    \\omit Stem
    \\set Score.markFormatter = #format-mark-numbers
    ~{\\mark \\default ~{~a ~}~^\\break ~} }~% 
    \\layout {
      indent = #0
      ragged-right = ##t
    }
}")

;;;TODO version of this function taking note objects as input (no spelling needed)
(defun ly-melody (pitches durations &key (template "~a") (fill-last-bar-with-rests nil))
  "Takes a list of PITCHES in midi note values and a list of DURATIONS and returns a text string in Lilypond format."
  (format nil template
	  (string-trim '(#\Space)
		       (with-output-to-string
			   (out)
			 (mapc (lambda (item)
				 (princ 
				  (cond
				    ((pitch-spelling:rest-p item) " r")
				    ((eq (type-of item) 'pitch-spelling:note)
				     (concatenate 'string " " (note->ly-pitch item)))
				    ((eq item :dot) ".")
				    ((eq item :tie) "~")
				    (t item))
				  out))
			       (let ((fill-fun (if fill-last-bar-with-rests #'final-rest #'identity)))
				 (funcall (alexandria:multiple-value-compose #'rhythm-spelling:rhythm-spell
									     fill-fun)
					  (pitch-spelling:pitch-spell pitches)
					  durations)))))))


(defun grand-staff-split (chord-seq)
  "Returns ((bass clef part of chords) (treble clef part of chords)).
E.g. (grand-staff-split (grand-staff-split '((48 64 67) (47 55 62) (48 52 55)))
-> (((48) (47 55) (48 52 55)) ((64 67) (62) NIL))"
  (loop :for chord :in chord-seq
	:with treble
	:with bass
	:do (loop :for note :in chord
		  :if (>= note 60) :collect note :into tr
		    :else :collect note :into ba
		  :finally (push tr treble) (push ba bass))
	:finally (return (list (reverse bass) (reverse treble)))))


;;;TODO Use template
(defun ly-harmony (chord-seq)
  (with-output-to-string (out)
    (format out "\\new GrandStaff <<~%")
    (loop :for staff :in (reverse (copy-seq (grand-staff-split chord-seq)))
	  :for i :from 0
	  :do (loop :initially (format out "  \\new Staff {~%")
			       (case i
				 (0 (format out "    \\clef treble ~%"))
				 (1 (format out "    \\clef bass ~%")))
		    :for chord :in staff
		    :do (if chord
			    (progn (format out "    <")
				   (format out "~{~a~^ ~}"
					   (mapcar #'note->ly-pitch
						   (pitch-spelling:pitch-spell chord)))
				   (format out ">1 ~%"))
			    (format out "    r1 ~%"))
		    :finally (format out "  }~%")))
    (format out ">>~%")))

(defun exec-lilypond (input &key (output "") args (lilypond-path *lilypond-path*))
  (declare (ignore args)) ;TODO allow argument passing
  (uiop:run-program (list lilypond-path
			  "-o" (uiop:native-namestring (namestring (make-pathname :type nil
										  :defaults output)))
			  (uiop:native-namestring input))
		    :output :interactive :error-output :interactive))

(defun open-pdf (file)
  ;;TODO linux
  (sb-ext:run-program "c:/Windows/explorer.exe"
		      (list (format nil "file:///~a" file))))

(defun run-lilypond (ly-code &key (ly-file nil ly-file-supplied-p)
			       (output-file nil output-file-supplied-p)
			       args (open-output t) (lilypond-path *lilypond-path*))
  (flet ((tmp (pathname)
	   (uiop:tmpize-pathname (uiop:merge-pathnames* (uiop:temporary-directory)
							pathname))))
    (let* ((ly-file (or ly-file (tmp "cl-lilypond.ly")))
	   (output-file (or output-file (tmp "lilypond.pdf"))))
      (unwind-protect
	   (progn
	     (alexandria:write-string-into-file ly-code ly-file :if-exists :overwrite)
	     (exec-lilypond ly-file
			    :output output-file :args args :lilypond-path lilypond-path)
	     (when open-output (open-pdf output-file)))
	(unless ly-file-supplied-p (uiop:delete-file-if-exists ly-file))
	(unless (or output-file-supplied-p open-output) (uiop:delete-file-if-exists output-file))))))
