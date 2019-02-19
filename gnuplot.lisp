(defpackage gnuplot
  (:nicknames gp)
  (:use common-lisp)
  (:export *gnuplot-program*
	   *numerical-print-format*
	   *default-print-format*
	   gnuplot-error
	   with-gnuplot
	   raw
	   dataset))

(in-package :gnuplot)

(defvar *gnuplot-program* "gnuplot")
(defvar *numerical-print-format* "~,10f")
(defvar *default-print-format* "~a")

(defvar *gnuplot-stream* nil)

(define-condition gnuplot-error (error)
  ((text :initarg :text :reader gnuplot-error/text))
  (:report (lambda (condition stream) (format stream "Gnuplot error:~%~a" (gnuplot-error/text condition)))))

(defconstant +interactive-terminals+
	     '("qt" "wxt" "tkcanvas" "aqua" "dumb" "x11"))

(defun with-gnuplot (fn &key
			(gnuplot-args nil)
			(in nil)
			(out *standard-output*)
			(external-format :default)
			(terminal "qt"))
  "Executes a function that takes an input stream to `gnuplot`.
  :gnuplot-args are concatenated when launching the gnuplot executable.
  :in is the stream to which we write to (by default, the gnuplot executable's
					     input stream).
  :out is the output stream to which gnuplot writes
  :external-format is the external format for program lauch.
  :terminal is the gnuplot terminal to use. Similar to
  `(raw \"set terminal ~a\")` but fixes some options to end the session
  properly at the end."

  (let ((gnuplot-err
	  (with-output-to-string (err)
	    (let ((gnuplot
		    (uiop:launch-program (if (null gnuplot-args)
					   *gnuplot-program*
					   (list *gnuplot-program* gnuplot-args))
					 :input :stream
					 :output out
					 :error-output err
					 :external-format external-format)))
	      (let ((*gnuplot-stream* (if in in (uiop:process-info-input gnuplot))))
		(unwind-protect
		  (progn
		    (format *gnuplot-stream* "set terminal ~a~%" terminal)
		    (funcall fn)
		    (when (not (find (car (str:words terminal)) +interactive-terminals+ :test #'equal))
		      (format *gnuplot-stream* "set output~%"))
		    (format *gnuplot-stream* "exit~%")
		    (finish-output *gnuplot-stream*)
		    (uiop:close-streams gnuplot)
		    (uiop:wait-process gnuplot)
		    t)
		  (progn
		    (when (uiop:process-alive-p gnuplot)
		      (uiop:close-streams gnuplot)
		      (uiop:terminate-process gnuplot :urgent t)
		      nil))))))))
    (if (not (uiop:emptyp gnuplot-err))
      (error 'gnuplot-error :text gnuplot-err)
      nil)))

(defun raw (&rest strings)
  "Dump gnuplot commands as strings directly to the stream (with newlines added)"
  (loop for str in strings do
	(format *gnuplot-stream* str)
	(terpri *gnuplot-stream*)))

(defun gnuplot-repr (x)
  "Helper returning the gnuplot representation of `x`."
  (typecase x
    (complex (format nil (str:concat "{" *numerical-print-format*
				     "," *numerical-print-format*
				     "}")
		     (realpart x) (imagpart x)))
    (number (format nil *numerical-print-format* x))
    (t (format nil *default-print-format* x))))

(defun typed-data-format (x y)
  "Helper to choose the print format.
  Gnuplot doesn't like characters in its numbers (1.23d4, for example),
  so we need to print numbers in non-lisp style and optionally
  higher precision. Other values (e.g. strings) should be accepted here
  to bypass our own printing rules or let the user do other customization."
  (format *gnuplot-stream*
	  "~a ~a~%"
	  (gnuplot-repr x)
	  (gnuplot-repr y)))

(defun dataset (dset &optional (keys '(:x :y)))
  "Dump a dataset, with \"end\" marker at the end.
  `keys` is a list of keys for the `x` and `y` coordinates in the data,
  respectively.
  `dset` is the dataset, either a plist of lists of all values along a coordinate,
  or a list of plists of `x` : `y` coordinates."
  (if (listp dset)
    (loop for dpoint in dset do
	  (let ((x (getf dpoint (car keys)))
		(y (getf dpoint (cadr keys))))
	      (typed-data-format x y)))
    (loop for x in (getf dset (car keys))
	  and y in (getf dset (cadr keys)) do
	  (typed-data-format x y)))
  (format *gnuplot-stream* "end~%~%"))
