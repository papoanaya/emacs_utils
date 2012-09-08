emacs_utils
===========

Emacs Lisp Ulitities that I have developed during the course of using
the editor. These are for my own use but feel free to use them as
required.

In the contrib directory

- org-e-groff.el : Org Export for groff using groff's Memorandum
  Macros. This one uses the new org-export.el framework that generalizes
  how org items are passed. This one leverages of those tags and
  translates those to Groff Memorandum Macros.
- org-e-man.el : Org Export for groff using groff's Man Page
  Macros. This one uses the new org-export.el framework that generalizes
  how org items are passed. 
  


The source-highlight directory contains files are used to perform syntax 
colorization during export for org-e-groff.el and org-e-man.el. These are:

- groff_mm.outlang : GNU Source Highlight definition file for black and
  white output for mm macros.
- groff_mm_color.outlang : GNU Source Highlight definition file for
  colored output for mm macros
- outlang.map : GNU Source Highlight definition file to add the groff_mm
  ones. 
- groff_man.outlang : GNU Source Highlight definition file for black and
  white output for man pages.

The org-old directory contains deprecated items in org mode

- org-groff.el : Org Export for groff using groff's Memorandum
  Macros. Another work in progress but it exports the basic outlines,
  todo, rudimentary table export and date markers (SCHEDULED: DEADLINE:). 
  No graph support as of yet, but the objective is to be
  able to use TBL and PIC to support those.  It is based on
  the LaTeX export code.  There is still a lot of the latex code
  there that needs to be cleaned up.  Development of this code is halted
  and all efforts are being spent on org-e-groff.el .

The doc directory contains documentation for org mode in org mode

- org-e-groff-documentation.org : Documentation for groff export mode.
- org-e-man-documentation.org : Documenetaiton for man pages export
  mode.
  
 
 The babel directory contains org-babel languages support:
 
 - ob-eukleides.el : Support for Eukleides, a geometry visualization
   program. 
 - ob-mathomatic.el : Support for the Mathomatic CAS system. 
 - ob-tcl.el : Support for the execution of tcl scripts. 
 
