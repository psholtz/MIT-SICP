Section 1.3
=========== 

Contains worked solutions expressed in the Emacs Lisp dialect.

To load a sample file into a running Emacs interpreter:

> (load-file "filename.el")

To load the files into a running Emacs interpreter, you may have to update the load-path variable in your Emacs settings to include the current working directory:

> (setq load-path (append (list ".") load-path))

Lexical Scoping versus Dynamic Scoping
-------------------------------------- 

[explain this for the various problems]

[The Art of the Interpreter](http://dspace.mit.edu/bitstream/handle/1721.1/6094/AIM-453.pdf)

[The Original Lambda Papers](http://library.readscheme.org/page1.html)