Section 1.3
=========== 

Contains worked solutions expressed in the Emacs Lisp dialect.

To load a sample file into a running Emacs interpreter:

> (load-file "filename.el")

To load the files into a running Emacs interpreter, you may have to update the load-path variable in your Emacs settings to include the current working directory:

> (setq load-path (append (list ".") load-path))

Lexical Scoping versus Dynamic Scoping
-------------------------------------- 

The difference between lexical and dynamic scoping can be stated looosely as:

### Dynamic Binding 
All variable names and their values live in one global table.

### Lexical Binding
Every binding scope (i.e,. function, let syntax, etc) creates a new table of variable names and values, organized in a hierarchy called "the environment".

Emacs only supports dynamic binding, while the version of Scheme used in SICP, and indeed most Lisp implementations today, support both lexical and dynamic binding. Some of the problem sets, beginning in this problem set onwards, require dynamic binding. Accordingly, in the emacs solutions, we "hack" our through these cases uses a combination of the "CL" Emacs Lisp package (i.e,. ```(require 'cl)```) and -- when necessary -- macros.

**References**

[1] [The Art of the Interpreter](http://dspace.mit.edu/bitstream/handle/1721.1/6094/AIM-453.pdf)

[2] [The Original Lambda Papers](http://library.readscheme.org/page1.html)
