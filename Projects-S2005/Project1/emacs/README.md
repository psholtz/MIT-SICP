Basebot implementation in Emacs Lisp.

To load the file into a running Emacs interpreter:

> (load "basebot.el")

To load files into a running Emacs interpreter, you may have to update the load-path variable in your Emacs settings to include the current working directory:

> (setq load-path (append (list nil ".") load-path))