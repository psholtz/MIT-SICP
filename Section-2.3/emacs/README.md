Section 2.3
=========== 

Contains worked solutions expressed in the Emacs Lisp dialect.

To load a sample file into a running Emacs interpreter:

> (load-file "filename.el")

To load files into a running Emacs interpreter, you may have to update the load-path variable in your Emacs settings to include the current working directory:

> (setq load-path (append (list ".") load-path))