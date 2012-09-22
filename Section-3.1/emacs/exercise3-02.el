(require 'cl)

(defun make-monitored (f)
  (lexical-let ((count 0))
    (defun mf (m)
      (cond ((equal m 'how-many-calls?) count)
	    ((equal m 'reset-count) (setq count 0) 0)
	    (t
	     (setq count (+ count 1))
	     (funcall f m))))
    mf))
