# Gnuplot

## Trivial gnuplot wrapper for common lisp

This trivial library provides the most basic forms and error capture to generate gnuplot plots from within common lisp.
The following example covers this package's current basic fetaures.

```
(ql:quickload :gnuplot)

(let ((gp:*gnuplot-program* "custom/path/to/gnuplot"))
	(gp:with-gnuplot
		#'(lambda ()
			(gp:raw "set output \"test.svg\""
				"set border 0")
			(gp:raw "plot '-' title \"Test Plot\" lw 2 with lines")
			(gp:dataset (loop for x from 0 below 1000 by 10
					and y from 0 below 10 by 0.1 collect
						(list :x x :y (* y y)))))
	:in *standard-output*
	:terminal "svg standalone mouse size 2000,2000"))
```

This should output the commands that would be sent to the gnuplot process to standard output. Passing `:in nil` (the default) should _actually_ send the comamnds to gnuplot. Format strings for numeric types and any other type can be customized. `gp:dataset` currently only supports 2D datasets (see `gnuplot.lisp` for more details). If gnuplot errors out, a condition is signaled with the exact text of the error.
