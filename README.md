emacs_utils
===========

Emacs Lisp Ulitities that I have developed during the course of using
the editor. These are for my own use but feel free to use them as
required.

- regina.el : REXX mode, which is a mash up of netrexx.el and
  rexx.el. In this way I have an electric mode with syntax highlight for
  developing REXX scripts. LOOP is not working, use DO. 
- muse-mm.el : MUSE driver for groff Memorandum Macros (-mm).  Even though
  muse already has a groff mode, that mode  uses the MOM and WWW macros
  which seems to be geared to literary works (i. e. books).
  The MM macros are catered for professional communications
  (Memorandum, White Papers, Business Letters, etc.)
  This MUSE driver may be rough around the edges but it works good enough for me.
- mantislib.el : Copy of jira2.el library for the Mantis bug tracking
  system.  It is still in development, but getting there. These can be 
  used in combination of other tools for access data from Mantis tickets. 
  It is still a work in progress, a lot of functions are not
  done. 
- org-groff.el : Org Export for groff using groff's Memorandum
  Macros. Another work in progress but it exports the basic outlines,
  todo, rudimentary table export and date markers (SCHEDULED: DEADLINE:). 
  No graph support as of yet, but the objective is to be
  able to use TBL and PIC to support those.  It is based on
  the LaTeX export code.  There is still a lot of the latex code
  there that needs to be cleaned up.  Development of this code is halted
  and all efforts are being spent on org-e-groff.el .
- org-e-groff.el : Org Export for groff using groff's Memorandum
  Macros. This one uses the new org-export.el framework that generalizes
  how org items are passed. This one leverages of those tags and
  translates those to Groff Memorandum Macros.
  
Auxiliary files:

The following files that are used by the org-e-groff.el are included in
this repository. These files are used to perform syntax colorization
during export.  

- groff_mm.outlang : GNU Source Highlight definition file for black and
  white output.
- groff_mm_color.outlang : GNU Source Highlight definition file for
  colored output.
- outlang.map : GNU Source Highlight definition file to add the groff_mm
  ones. 
 
