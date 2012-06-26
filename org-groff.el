;;; org-groff.el --- Groff exporter for org-mode
;;
;; Copyright (C) 2007-2012 Free Software Foundation, Inc.
;;
;; Emacs Lisp Archive Entry
;; Filename: org-groff.el
;; Author: Bastien Guerry <bzg AT gnu DOT org>
;; Maintainer: Carsten Dominik <carsten.dominik AT gmail DOT com>
;; Keywords: org, wp, tex
;; Description: Converts an org-mode buffer into Groff

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This library implements a Groff exporter for org-mode.
;;
;; It is part of Org and will be autoloaded
;;
;; The interactive functions are similar to those of the HTML exporter:
;;
;; M-x `org-export-as-groff'
;; M-x `org-export-as-pdf'
;; M-x `org-export-as-pdf-and-open'
;; M-x `org-export-as-groff-batch'
;; M-x `org-export-as-groff-to-buffer'
;; M-x `org-export-region-as-groff'
;; M-x `org-replace-region-by-groff'
;;
;;; Code:

(eval-when-compile
  (require 'cl))

(require 'footnote)
(require 'org)
(require 'org-exp)
(require 'org-macs)
(require 'org-beamer)

;;; Variables:
(defvar org-export-groff-class nil)
(defvar org-export-groff-class-options nil)
(defvar org-export-groff-header nil)
(defvar org-export-groff-append-header nil)
(defvar org-export-groff-options-plist nil)
(defvar org-export-groff-todo-keywords-1 nil)
(defvar org-export-groff-complex-heading-re nil)
(defvar org-export-groff-not-done-keywords nil)
(defvar org-export-groff-done-keywords nil)
(defvar org-export-groff-display-custom-times nil)
(defvar org-export-groff-all-targets-re nil)
(defvar org-export-groff-add-level 0)
(defvar org-export-groff-footmark-seen nil
  "List of footnotes markers seen so far by exporter.")
(defvar org-export-groff-sectioning "")
(defvar org-export-groff-sectioning-depth 0)
(defvar org-export-groff-special-keyword-regexp
  (concat "\\<\\(" org-scheduled-string "\\|"
		  org-deadline-string "\\|"
		  org-closed-string"\\)")
  "Regexp matching special time planning keywords plus the time after it.")
(defvar org-re-quote)				  ; dynamically scoped from org.el
(defvar org-commentsp)				  ; dynamically scoped from org.el

;;; User variables:

(defgroup org-export-groff nil
  "Options for exporting Org-mode files to Groff."
  :tag "Org Export Groff"
  :group 'org-export)

(defcustom org-export-groff-default-class "internal"
  "The default Groff class."
  :group 'org-export-groff
  :type '(string :tag "Groff class"))

(defcustom org-export-groff-classes
  '(("internal"
     ".MT 1"
	 (".H 1 \"%s\"" . ".HU \"%s\"")
	 (".H 2 \"%s\"" . ".HU \"%s\"")
	 (".H 3 \"%s\"" . ".HU \"%s\"")
	 (".H 4 \"%s\"" . ".HU \"%s\"")
	 (".H 5 \"%s\"" . ".HU \"%s\"") )
    ("external"
     ".MT 4" 
	 (".H 1 \"%s\"" . ".HU \"%s\"")
	 (".H 2 \"%s\"" . ".HU \"%s\"")
	 (".H 3 \"%s\"" . ".HU \"%s\"")
	 (".H 4 \"%s\"" . ".HU \"%s\"")
	 (".H 5 \"%s\"" . ".HU \"%s\"") )
    ("letter"
     ".MT 5"
	 (".H 1 \"%s\"" . ".HU \"%s\"")
	 (".H 2 \"%s\"" . ".HU \"%s\"")
	 (".H 3 \"%s\"" . ".HU \"%s\"")
	 (".H 4 \"%s\"" . ".HU \"%s\"")
	 (".H 5 \"%s\"" . ".HU \"%s\"") ) )
  "Alist of Groff classes and associated header and structure.
If #+Groff_CLASS is set in the buffer, use its value and the
associated information.  Here is the structure of each cell:

  \(class-name
    header-string
    (numbered-section . unnumbered-section\)
    ...\)

The header string
-----------------

The HEADER-STRING is the header that will be inserted into the Groff file.
To this header, the following commands will be added:

- Assigns for all packages mentioned in the variables
  `org-export-groff-default-packages-alist' and
  `org-export-groff-packages-alist'.  Thus, your header definitions should
  avoid to also request these packages.

- Lines specified via \"#+Groff_HEADER:\"

If you need more control about the sequence in which the header is built
up, or if you want to exclude one of these building blocks for a particular
class, you can use the following macro-like placeholders.

 [DEFAULT-PACKAGES]      \\usepackage statements for default packages
 [NO-DEFAULT-PACKAGES]   do not include any of the default packages
 [PACKAGES]              \\usepackage statements for packages
 [NO-PACKAGES]           do not include the packages
 [EXTRA]                 the stuff from #+Groff_HEADER
 [NO-EXTRA]              do not include #+Groff_HEADER stuff
 [BEAMER-HEADER-EXTRA]   the beamer extra headers

So a header like

  \\documentclass{article}
  [NO-DEFAULT-PACKAGES]
  [EXTRA]
  \\providecommand{\\alert}[1]{\\textbf{#1}}
  [PACKAGES]

will omit the default packages, and will include the #+Groff_HEADER lines,
then have a call to \\providecommand, and then place \\usepackage commands
based on the content of `org-export-groff-packages-alist'.

If your header or `org-export-groff-default-packages-alist' inserts
\"\\usepackage[AUTO]{inputenc}\", AUTO will automatically be replaced with
a coding system derived from `buffer-file-coding-system'.  See also the
variable `org-export-groff-inputenc-alist' for a way to influence this
mechanism.

The sectioning structure
------------------------

The sectioning structure of the class is given by the elements following
the header string.  For each sectioning level, a number of strings is
specified.  A %s formatter is mandatory in each section string and will
be replaced by the title of the section.

Instead of a cons cell (numbered . unnumbered), you can also provide a list
of 2 or 4 elements,

  (numbered-open numbered-close)

or

  (numbered-open numbered-close unnumbered-open unnumbered-close)

providing opening and closing strings for a Groff environment that should
represent the document section.  The opening clause should have a %s
to represent the section title.

Instead of a list of sectioning commands, you can also specify a
function name.  That function will be called with two parameters,
the (reduced) level of the headline, and the headline text.  The function
must return a cons cell with the (possibly modified) headline text, and the
sectioning list in the cdr."
  :group 'org-export-groff
  :type '(repeat
		  (list (string :tag "Groff class")
				(string :tag "Groff header")
				(repeat :tag "Levels" :inline t
						(choice
						 (cons :tag "Heading"
							   (string :tag "  numbered")
							   (string :tag "unnumbered"))
						 (list :tag "Environment"
							   (string :tag "Opening   (numbered)")
							   (string :tag "Closing   (numbered)")
							   (string :tag "Opening (unnumbered)")
							   (string :tag "Closing (unnumbered)"))
						 (function :tag "Hook computing sectioning"))))))

(defcustom org-export-groff-inputenc-alist nil
  "Alist of inputenc coding system names, and what should really be used.
For example, adding an entry

      (\"utf8\" . \"utf8x\")

will cause \\usepackage[utf8x]{inputenc} to be used for buffers that
are written as utf8 files."
  :group 'org-export-groff
  :version "24.1"
  :type '(repeat
		  (cons
		   (string :tag "Derived from buffer")
		   (string :tag "Use this instead"))))


(defcustom org-export-groff-emphasis-alist
  '(("*" "\\fB%s\\fP" nil)
    ("/" "\\fI%s\\fP" nil)
    ("_" "\\fI%s\\fP" nil)
    ("+" "\\fC%s\\fP" nil) )
  "Alist of Groff expressions to convert emphasis fontifiers.
Each element of the list is a list of three elements.
The first element is the character used as a marker for fontification.
The second element is a formatting string to wrap fontified text with.
If it is \"\\verb\", Org will automatically select a delimiter
character that is not in the string.  \"\\protectedtexttt\" will use \\texttt
to typeset and try to protect special characters.
The third element decides whether to protect converted text from other
conversions."
  :group 'org-export-groff
  :type 'alist)

(defcustom org-export-groff-title-command ".PH '''%s'"
  "The command used to insert the title.
If this string contains the formatting specification \"%s\" then
it will be used as a formatting string, passing the title as an
argument."
  :group 'org-export-groff
  :type 'string)

(defcustom org-export-groff-import-inbuffer-stuff nil
  "Non-nil means define TeX macros for Org's inbuffer definitions.
For example \orgTITLE for #+TITLE."
  :group 'org-export-groff
  :type 'boolean)

(defcustom org-export-groff-date-format
  "%Y-%m-%d"
  "Format string for .ND"
  :group 'org-export-groff
  :type 'string)

(defcustom org-export-groff-todo-keyword-markup "\\fB%s\\fP"
  "Markup for TODO keywords, as a printf format.
This can be a single format for all keywords, a cons cell with separate
formats for not-done and done states, or an association list with setup
for individual keywords.  If a keyword shows up for which there is no
markup defined, the first one in the association list will be used."
  :group 'org-export-groff
  :type '(choice
		  (string :tag "Default")
		  (cons :tag "Distinguish undone and done"
				(string :tag "Not-DONE states")
				(string :tag "DONE states"))
		  (repeat :tag "Per keyword markup"
				  (cons
				   (string :tag "Keyword")
				   (string :tag "Markup")))))

(defcustom org-export-groff-tag-markup "\\fB%s\\fP"
  "Markup for tags, as a printf format."
  :group 'org-export-groff
  :version "24.1"
  :type 'string)

(defcustom org-export-groff-timestamp-markup "\\fI%s\\fP"
  "A printf format string to be applied to time stamps."
  :group 'org-export-groff
  :type 'string)

(defcustom org-export-groff-timestamp-inactive-markup "\\fI%s\\fP"
  "A printf format string to be applied to inactive time stamps."
  :group 'org-export-groff
  :version "24.1"
  :type 'string)

(defcustom org-export-groff-timestamp-keyword-markup "\\fC%s\\fP"
  "A printf format string to be applied to time stamps."
  :group 'org-export-groff
  :type 'string)

;; From "%s"
(defcustom org-export-groff-href-format nil
  "A printf format string to be applied to href links.
The format must contain either two %s instances or just one.
If it contains two %s instances, the first will be filled with
the link, the second with the link description.  If it contains
only one, the %s will be filled with the link."
  :group 'org-export-groff
  :version "24.1"
  :type 'string)
;; From "%s"
(defcustom org-export-groff-hyperref-format nil
  "A printf format string to be applied to hyperref links.
The format must contain one or two %s instances.  The first one
will be filled with the link, the second with its description."
  :group 'org-export-groff
  :version "24.1"
  :type 'string)

(defcustom org-export-groff-footnote-separator nil
  "Text used to separate footnotes."
  :group 'org-export-groff
  :version "24.1"
  :type 'string)

(defcustom org-export-groff-quotes
  '(("fr" ("\\(\\s-\\|[[(]\\)\"" . "«~") ("\\(\\S-\\)\"" . "~»") ("\\(\\s-\\|(\\)'" . "'"))
    ("en" ("\\(\\s-\\|[[(]\\)\"" . "``") ("\\(\\S-\\)\"" . "''") ("\\(\\s-\\|(\\)'" . "`")))
  "Alist for quotes to use when converting english double-quotes.

The CAR of each item in this alist is the language code.
The CDR of each item in this alist is a list of three CONS:
- the first CONS defines the opening quote;
- the second CONS defines the closing quote;
- the last CONS defines single quotes.

For each item in a CONS, the first string is a regexp
for allowed characters before/after the quote, the second
string defines the replacement string for this quote."
  :group 'org-export-groff
  :version "24.1"
  :type '(list
		  (cons :tag "Opening quote"
				(string :tag "Regexp for char before")
				(string :tag "Replacement quote     "))
		  (cons :tag "Closing quote"
				(string :tag "Regexp for char after ")
				(string :tag "Replacement quote     "))
		  (cons :tag "Single quote"
				(string :tag "Regexp for char before")
				(string :tag "Replacement quote     "))))

(defcustom org-export-groff-tables-verbatim nil
  "When non-nil, tables are exported verbatim."
  :group 'org-export-groff
  :type 'boolean)

(defcustom org-export-groff-tables-centered t
  "When non-nil, tables are exported in a center environment."
  :group 'org-export-groff
  :type 'boolean)

(defcustom org-export-groff-table-caption-above t
  "When non-nil, the caption is set above the table.  When nil,
the caption is set below the table."
  :group 'org-export-groff
  :version "24.1"
  :type 'boolean)

(defcustom org-export-groff-tables-column-borders nil
  "When non-nil, grouping columns can cause outer vertical lines in tables.
When nil, grouping causes only separation lines between groups."
  :group 'org-export-groff
  :type 'boolean)

(defcustom org-export-groff-low-levels 'itemize
  "How to convert sections below the current level of sectioning.
This is specified by the `org-export-headline-levels' option or the
value of \"H:\" in Org's #+OPTION line.

This can be either nil (skip the sections), `description', `itemize',
or `enumerate' (convert the sections as the corresponding list type), or
a string to be used instead of \\section{%s}.  In this latter case,
the %s stands here for the inserted headline and is mandatory.

It may also be a list of three string to define a user-defined environment
that should be used.  The first string should be the like
\".BL\", the second should be like \".LI%s\" with up
to two occurrences of %s for the title and a label, respectively.  The third
string should be like \".LE"
  :group 'org-export-groff
  :type '(choice (const :tag "Ignore" nil)
				 (const :tag "Convert as descriptive list" description)
				 (const :tag "Convert as itemized list" itemize)
				 (const :tag "Convert as enumerated list" enumerate)
				 (list  :tag "User-defined environment"
						:value (".BL" ".LE" ".LI\n%s")
						(string :tag "Start")
						(string :tag "End")
						(string :tag "item"))
				 (string :tag "Use a section string" :value ".H 2 \"%s\"")))

(defcustom org-export-groff-list-parameters
  '(:cbon "$\(sq$" :cboff "$\(dg$" :cbtrans "$\(sq$")
  "Parameters for the Groff list exporter.
These parameters will be passed on to `org-list-to-groff', which in turn
will pass them (combined with the Groff default list parameters) to
`org-list-to-generic'."
  :group 'org-export-groff
  :type 'plist)

(defcustom org-export-groff-verbatim-wrap
  '(".DC L\n" . ".DE\n")
  "Environment to be wrapped around a fixed-width section in Groff export.
This is a cons with two strings, to be added before and after the
fixed-with text.

Defaults to .DC and .DE"
  :group 'org-export-translation
  :group 'org-export-groff
  :type '(cons (string :tag "Open")
			   (string :tag "Close")))

(defcustom org-export-groff-listings nil
  "Non-nil means export source code using the listings package.
This package will fontify source code, possibly even with color.
If you want to use this, you also need to make Groff use the
listings package, and if you want to have color, the color
package.  Just add these to `org-export-groff-packages-alist',
for example using customize, or with something like

  (require 'org-groff)
  (add-to-list 'org-export-groff-packages-alist '(\"\" \"listings\"))
  (add-to-list 'org-export-groff-packages-alist '(\"\" \"color\"))

Alternatively,

  (setq org-export-groff-listings 'minted)

causes source code to be exported using the minted package as
opposed to listings.  If you want to use minted, you need to add
the minted package to `org-export-groff-packages-alist', for
example using customize, or with

  (require 'org-groff)
  (add-to-list 'org-export-groff-packages-alist '(\"\" \"minted\"))

In addition, it is necessary to install
pygments (http://pygments.org), and to configure the variable
`org-groff-to-pdf-process' so that the -shell-escape option is
passed to pdfgroff.
"
  :group 'org-export-groff
  :type 'boolean)

(defcustom org-export-groff-listings-langs
  '((emacs-lisp "Lisp") (lisp "Lisp") (clojure "Lisp")
    (c "C") (cc "C++")
    (fortran "fortran")
    (perl "Perl") (cperl "Perl") (python "Python") (ruby "Ruby")
    (html "HTML") (xml "XML")
    (tex "TeX") (groff "TeX")
    (shell-script "bash")
    (gnuplot "Gnuplot")
    (ocaml "Caml") (caml "Caml")
    (sql "SQL") (sqlite "sql"))
  "Alist mapping languages to their listing language counterpart.
The key is a symbol, the major mode symbol without the \"-mode\".
The value is the string that should be inserted as the language parameter
for the listings package.  If the mode name and the listings name are
the same, the language does not need an entry in this list - but it does not
hurt if it is present."
  :group 'org-export-groff
  :type '(repeat
		  (list
		   (symbol :tag "Major mode       ")
		   (string :tag "Listings language"))))

(defcustom org-export-groff-listings-w-names t
  "Non-nil means export names of named code blocks.
Code blocks exported with the listings package (controlled by the
`org-export-groff-listings' variable) can be named in the style
of noweb."
  :group 'org-export-groff
  :version "24.1"
  :type 'boolean)

(defcustom org-export-groff-minted-langs
  '((emacs-lisp "common-lisp")
    (cc "c++")
    (cperl "perl")
    (shell-script "bash")
    (caml "ocaml"))
  "Alist mapping languages to their minted language counterpart.
The key is a symbol, the major mode symbol without the \"-mode\".
The value is the string that should be inserted as the language parameter
for the minted package.  If the mode name and the listings name are
the same, the language does not need an entry in this list - but it does not
hurt if it is present.

Note that minted uses all lower case for language identifiers,
and that the full list of language identifiers can be obtained
with:
pygmentize -L lexers
"
  :group 'org-export-groff
  :version "24.1"
  :type '(repeat
		  (list
		   (symbol :tag "Major mode       ")
		   (string :tag "Listings language"))))

(defcustom org-export-groff-listings-options nil
  "Association list of options for the groff listings package.

These options are supplied as a comma-separated list to the
\\lstset command. Each element of the association list should be
a list containing two strings: the name of the option, and the
value. For example,

  (setq org-export-groff-listings-options
    '((\"basicstyle\" \"\\small\")
      (\"keywordstyle\" \"\\color{black}\\bfseries\\underbar\")))

will typeset the code in a small size font with underlined, bold
black keywords.

Note that the same options will be applied to blocks of all
languages."
  :group 'org-export-groff
  :version "24.1"
  :type '(repeat
		  (list
		   (string :tag "Listings option name ")
		   (string :tag "Listings option value"))))

(defcustom org-export-groff-minted-options nil
  "Association list of options for the groff minted package.

These options are supplied within square brackets in
\\begin{minted} environments. Each element of the alist should be
a list containing two strings: the name of the option, and the
value. For example,

  (setq org-export-groff-minted-options
    '((\"bgcolor\" \"bg\") (\"frame\" \"lines\")))

will result in src blocks being exported with

\\begin{minted}[bgcolor=bg,frame=lines]{<LANG>}

as the start of the minted environment. Note that the same
options will be applied to blocks of all languages."
  :group 'org-export-groff
  :version "24.1"
  :type '(repeat
		  (list
		   (string :tag "Minted option name ")
		   (string :tag "Minted option value"))))

(defvar org-export-groff-custom-lang-environments nil
  "Association list mapping languages to language-specific groff
  environments used during export of src blocks by the listings
  and minted groff packages. For example,

  (setq org-export-groff-custom-lang-environments
     '((python \"pythoncode\")))

  would have the effect that if org encounters begin_src python
  during groff export it will output

  \\begin{pythoncode}
  <src block body>
  \\end{pythoncode}")

(defcustom org-export-groff-remove-from-headlines
  '(:todo nil :priority nil :tags nil)
  "A plist of keywords to remove from headlines.  OBSOLETE.
Non-nil means remove this keyword type from the headline.

Don't remove the keys, just change their values.

Obsolete, this variable is no longer used.  Use the separate
variables `org-export-with-todo-keywords', `org-export-with-priority',
and `org-export-with-tags' instead."
  :type 'plist
  :group 'org-export-groff)

(defcustom org-export-groff-image-default-option nil
  "Default option for images."
  :group 'org-export-groff
  :type 'string)

(defcustom org-groff-default-figure-position nil
  "Default position for groff figures."
  :group 'org-export-groff
  :version "24.1"
  :type 'string)

(defcustom org-export-groff-tabular-environment nil
  "Default environment used to build tables."
  :group 'org-export-groff
  :version "24.1"
  :type 'string)

(defcustom org-export-groff-inline-image-extensions
  '("pdf" "ps" "eps")
  "Extensions of image files that can be inlined into Groff.
Note that the image extension *actually* allowed depend on the way the
Groff file is processed.  When used with pdfgroff, pdf, jpg and png images
are OK.  When processing through dvi to Postscript, only ps and eps are
allowed.  The default we use here encompasses both."
  :group 'org-export-groff
  :type '(repeat (string :tag "Extension")))

(defcustom org-export-groff-coding-system nil
  "Coding system for the exported Groff file."
  :group 'org-export-groff
  :type 'coding-system)

(defgroup org-export-pdf nil
  "Options for exporting Org-mode files to PDF, via Groff."
  :tag "Org Export PDF"
  :group 'org-export-groff
  :group 'org-export)

(defcustom org-groff-to-pdf-process
  '("groff -mm %f | ps2pdf - > %o/%f.pdf "
	"groff -mm %f | ps2pdf - > %o/%f.pdf "
	"groff -mm %f | ps2pdf - > %o/%f.pdf ")
  "Commands to process a Groff file to a PDF file.
This is a list of strings, each of them will be given to the shell
as a command.  %f in the command will be replaced by the full file name, %b
by the file base name (i.e. without extension) and %o by the base directory
of the file.

The reason why this is a list is that it usually takes several runs of
`pdfgroff', maybe mixed with a call to `bibtex'.  Org does not have a clever
mechanism to detect which of these commands have to be run to get to a stable
result, and it also does not do any error checking.

By default, Org uses 3 runs of `pdfgroff' to do the processing.  If you
have texi2dvi on your system and if that does not cause the infamous
egrep/locale bug:

     http://lists.gnu.org/archive/html/bug-texinfo/2010-03/msg00031.html

then `texi2dvi' is the superior choice.  Org does offer it as one
of the customize options.

Alternatively, this may be a Lisp function that does the processing, so you
could use this to apply the machinery of AUCTeX or the Emacs Groff mode.
This function should accept the file name as its single argument."
  :group 'org-export-pdf
  :type '(choice
		  (repeat :tag "Shell command sequence"
				  (string :tag "Shell command"))
		  (const :tag "2 runs of pdfgroff"
				 (
				  "groff -mm %f | ps2pdf - > %o/%f.pdf "
				  "groff -mm %f | ps2pdf - > %o/%f.pdf "))
		  (const :tag "3 runs of pdfgroff"
				 (
				  "groff -mm %f | ps2pdf - > %o/%f.pdf "
				  "groff -mm %f | ps2pdf - > %o/%f.pdf "
				  "groff -mm %f | ps2pdf - > %o/%f.pdf " ))
		  (const :tag "groff,tbl"
				 ("tbl %f | groff -mm %f | ps2pdf - > %o/%f.pdf "
				  "tbl %f | groff -mm %f | ps2pdf - > %o/%f.pdf "))
		  (function)))

(defcustom org-export-pdf-logfiles
  '("aux" "idx" "log" "out" "toc" "nav" "snm" "vrb")
  "The list of file extensions to consider as Groff logfiles."
  :group 'org-export-pdf
  :version "24.1"
  :type '(repeat (string :tag "Extension")))

(defcustom org-export-pdf-remove-logfiles t
  "Non-nil means remove the logfiles produced by PDF production.
These are the .aux, .log, .out, and .toc files."
  :group 'org-export-pdf
  :type 'boolean)

;;; Hooks

(defvar org-export-groff-after-initial-vars-hook nil
  "Hook run before Groff export.
The exact moment is after the initial variables like org-export-groff-class
have been determined from the environment.")

(defvar org-export-groff-after-blockquotes-hook nil
  "Hook run during Groff export, after blockquote, verse, center are done.")

(defvar org-export-groff-final-hook nil
  "Hook run in the finalized Groff buffer.")

(defvar org-export-groff-after-save-hook nil
  "Hook run in the finalized Groff buffer, after it has been saved.")

;;; Autoload functions:

;;;###autoload
(defun org-export-as-groff-batch ()
  "Call `org-export-as-groff', may be used in batch processing.
For example:

emacs   --batch
        --load=$HOME/lib/emacs/org.el
        --eval \"(setq org-export-headline-levels 2)\"
        --visit=MyFile --funcall org-export-as-groff-batch"
  (org-export-as-groff org-export-headline-levels 'hidden))

;;;###autoload
(defun org-export-as-groff-to-buffer (arg)
  "Call `org-export-as-groff` with output to a temporary buffer.
No file is created.  The prefix ARG is passed through to `org-export-as-groff'."
  (interactive "P")
  (org-export-as-groff arg nil nil "*Org Groff Export*")
  (when org-export-show-temporary-export-buffer
    (switch-to-buffer-other-window "*Org Groff Export*")))

;;;###autoload
(defun org-replace-region-by-groff (beg end)
  "Replace the region from BEG to END with its Groff export.
It assumes the region has `org-mode' syntax, and then convert it to
Groff.  This can be used in any buffer.  For example, you could
write an itemized list in `org-mode' syntax in an Groff buffer and
then use this command to convert it."
  (interactive "r")
  (let (reg groff buf)
    (save-window-excursion
      (if (eq major-mode 'org-mode)
		  (setq groff (org-export-region-as-groff
					   beg end t 'string))
		(setq reg (buffer-substring beg end)
			  buf (get-buffer-create "*Org tmp*"))
		(with-current-buffer buf
		  (erase-buffer)
		  (insert reg)
		  (org-mode)
		  (setq groff (org-export-region-as-groff
					   (point-min) (point-max) t 'string)))
		(kill-buffer buf)))
    (delete-region beg end)
    (insert groff)))

;;;###autoload
(defun org-export-region-as-groff (beg end &optional body-only buffer)
  "Convert region from BEG to END in `org-mode' buffer to Groff.
If prefix arg BODY-ONLY is set, omit file header, footer, and table of
contents, and only produce the region of converted text, useful for
cut-and-paste operations.
If BUFFER is a buffer or a string, use/create that buffer as a target
of the converted Groff.  If BUFFER is the symbol `string', return the
produced Groff as a string and leave no buffer behind.  For example,
a Lisp program could call this function in the following way:

  (setq groff (org-export-region-as-groff beg end t 'string))

When called interactively, the output buffer is selected, and shown
in a window.  A non-interactive call will only return the buffer."
  (interactive "r\nP")
  (when (org-called-interactively-p 'any)
    (setq buffer "*Org Groff Export*"))
  (let ((transient-mark-mode t) (zmacs-regions t)
		ext-plist rtn)
    (setq ext-plist (plist-put ext-plist :ignore-subtree-p t))
    (goto-char end)
    (set-mark (point)) ;; to activate the region
    (goto-char beg)
    (setq rtn (org-export-as-groff
			   nil nil ext-plist
			   buffer body-only))
    (if (fboundp 'deactivate-mark) (deactivate-mark))
    (if (and (org-called-interactively-p 'any) (bufferp rtn))
		(switch-to-buffer-other-window rtn)
      rtn)))

;;;###autoload
(defun org-export-as-groff (arg &optional hidden ext-plist
								to-buffer body-only pub-dir)
  "Export current buffer to a Groff file.
If there is an active region, export only the region.  The prefix
ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will be exported
depending on `org-export-groff-low-levels'.  The default is to
convert them as description lists.
HIDDEN is obsolete and does nothing.
EXT-PLIST is a property list with
external parameters overriding org-mode's default settings, but
still inferior to file-local settings.  When TO-BUFFER is
non-nil, create a buffer with that name and export to that
buffer.  If TO-BUFFER is the symbol `string', don't leave any
buffer behind but just return the resulting Groff as a string.
When BODY-ONLY is set, don't produce the file header and footer,
simply return the content of \\begin{document}...\\end{document},
without even the \\begin{document} and \\end{document} commands.
when PUB-DIR is set, use this as the publishing directory."
  (interactive "P")
  (when (and (not body-only) arg (listp arg)) (setq body-only t))
  (run-hooks 'org-export-first-hook)

  ;; Make sure we have a file name when we need it.
  (when (and (not (or to-buffer body-only))
			 (not buffer-file-name))
    (if (buffer-base-buffer)
		(org-set-local 'buffer-file-name
					   (with-current-buffer (buffer-base-buffer)
						 buffer-file-name))
      (error "Need a file name to be able to export")))

  (message "Exporting to Groff...")
  (org-unmodified
   (let ((inhibit-read-only t))
     (remove-text-properties (point-min) (point-max)
							 '(:org-license-to-kill nil))))
  (org-update-radio-target-regexp)
  (org-export-groff-set-initial-vars ext-plist arg)
  (setq org-export-opt-plist org-export-groff-options-plist
		org-export-footnotes-data (org-footnote-all-labels 'with-defs)
		org-export-footnotes-seen nil
		org-export-groff-footmark-seen nil)
  (org-install-letbind)
  (run-hooks 'org-export-groff-after-initial-vars-hook)
  (let* ((wcf (current-window-configuration))
		 (opt-plist
		  (org-export-process-option-filters org-export-groff-options-plist))
		 (region-p (org-region-active-p))
		 (rbeg (and region-p (region-beginning)))
		 (rend (and region-p (region-end)))
		 (subtree-p
		  (if (plist-get opt-plist :ignore-subtree-p)
			  nil
			(when region-p
			  (save-excursion
				(goto-char rbeg)
				(and (org-at-heading-p)
					 (>= (org-end-of-subtree t t) rend))))))
		 (opt-plist (setq org-export-opt-plist
						  (if subtree-p
							  (org-export-add-subtree-options opt-plist rbeg)
							opt-plist)))
		 ;; Make sure the variable contains the updated values.
		 (org-export-groff-options-plist (setq org-export-opt-plist opt-plist))
		 ;; The following two are dynamically scoped into other
		 ;; routines below.
		 (org-current-export-dir
		  (or pub-dir (org-export-directory :html opt-plist)))
		 (org-current-export-file buffer-file-name)
		 (title (or (and subtree-p (org-export-get-title-from-subtree))
					(plist-get opt-plist :title)
					(and (not
						  (plist-get opt-plist :skip-before-1st-heading))
						 (org-export-grab-title-from-buffer))
					(and buffer-file-name
						 (file-name-sans-extension
						  (file-name-nondirectory buffer-file-name)))
					"No Title"))
		 (filename
		  (and (not to-buffer)
			   (concat
				(file-name-as-directory
				 (or pub-dir
					 (org-export-directory :Groff org-export-groff-options-plist)))
				(file-name-sans-extension
				 (or (and subtree-p
						  (org-entry-get rbeg "EXPORT_FILE_NAME" t))
					 (file-name-nondirectory ;sans-extension
					  (or buffer-file-name
						  (error "Don't know which export file to use")))))
				".groff")))
		 (filename
		  (and filename
			   (if (equal (file-truename filename)
						  (file-truename (or buffer-file-name "dummy.org")))
				   (concat filename ".groff")
				 filename)))
		 (auto-insert nil) ; Avoid any auto-insert stuff for the new file
		 (TeX-master (boundp 'TeX-master))
		 (buffer (if to-buffer
					 (cond
					  ((eq to-buffer 'string) (get-buffer-create
											   "*Org Groff Export*"))
					  (t (get-buffer-create to-buffer)))
				   (find-file-noselect filename)))
		 (odd org-odd-levels-only)
		 (header (org-export-groff-make-header title opt-plist))
		 (skip (cond (subtree-p nil)
					 (region-p nil)
					 (t (plist-get opt-plist :skip-before-1st-heading))))
		 (text (plist-get opt-plist :text))
		 (org-export-preprocess-hook
		  (cons
		   `(lambda () (org-set-local 'org-complex-heading-regexp
									  ,org-export-groff-complex-heading-re))
		   org-export-preprocess-hook))
		 (first-lines (if skip "" (org-export-groff-first-lines
								   opt-plist
								   (if subtree-p
									   (save-excursion
										 (goto-char rbeg)
										 (point-at-bol 2))
									 rbeg)
								   (if region-p rend))))
		 (coding-system (and (boundp 'buffer-file-coding-system)
							 buffer-file-coding-system))
		 (coding-system-for-write (or org-export-groff-coding-system
									  coding-system))
		 (save-buffer-coding-system (or org-export-groff-coding-system
										coding-system))
		 (region (buffer-substring
				  (if region-p (region-beginning) (point-min))
				  (if region-p (region-end) (point-max))))
		 (text
		  (and text (string-match "\\S-" text)
			   (org-export-preprocess-string
				text
				:emph-multiline t
				:for-backend 'groff
				:comments nil
				:tags (plist-get opt-plist :tags)
				:priority (plist-get opt-plist :priority)
				:footnotes (plist-get opt-plist :footnotes)
				:drawers (plist-get opt-plist :drawers)
				:timestamps (plist-get opt-plist :timestamps)
				:todo-keywords (plist-get opt-plist :todo-keywords)
				:tasks (plist-get opt-plist :tasks)
				:add-text nil
				:skip-before-1st-heading skip
				:select-tags nil
				:exclude-tags nil
				:Groff-fragments nil)))
		 (string-for-export
		  (org-export-preprocess-string
		   region
		   :emph-multiline t
		   :for-backend 'groff
		   :comments nil
		   :tags (plist-get opt-plist :tags)
		   :priority (plist-get opt-plist :priority)
		   :footnotes (plist-get opt-plist :footnotes)
		   :drawers (plist-get opt-plist :drawers)
		   :timestamps (plist-get opt-plist :timestamps)
		   :todo-keywords (plist-get opt-plist :todo-keywords)
		   :tasks (plist-get opt-plist :tasks)
		   :add-text (if (eq to-buffer 'string) nil text)
		   :skip-before-1st-heading skip
		   :select-tags (plist-get opt-plist :select-tags)
		   :exclude-tags (plist-get opt-plist :exclude-tags)
		   :Groff-fragments nil)))

    (set-buffer buffer)
    (erase-buffer)
    (org-install-letbind)

    (and (fboundp 'set-buffer-file-coding-system)
		 (set-buffer-file-coding-system coding-system-for-write))

    ;; insert the header and initial document commands
    (unless (or (eq to-buffer 'string) body-only)
      (insert header))

    ;; insert text found in #+TEXT
    (when (and text (not (eq to-buffer 'string)))
      (insert (org-export-groff-content
			   text '(lists tables fixed-width keywords))
			  "\n\n"))

    ;; insert lines before the first headline
    (unless (or skip (string-match "^\\*" first-lines))
      (insert first-lines))

    ;; export the content of headlines
    (org-export-groff-global
     (with-temp-buffer
       (insert string-for-export)
       (goto-char (point-min))
       (when (re-search-forward "^\\(\\*+\\) " nil t)
		 (let* ((asters (length (match-string 1)))
				(level (if odd (- asters 2) (- asters 1))))
		   (setq org-export-groff-add-level
				 (if odd (1- (/ (1+ asters) 2)) (1- asters)))
		   (org-export-groff-parse-global level odd)))))

    ;; finalization
    (unless body-only (insert "\n"))

    ;; Attach description terms to the \item macro
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*\\.LI\\([ \t]+\\)\\[" nil t)
      (delete-region (match-beginning 1) (match-end 1)))

    ;; Relocate the table of contents
    (goto-char (point-min))
    (when (re-search-forward "\\[TABLE-OF-CONTENTS\\]" nil t)
      (goto-char (point-min))
      (while (re-search-forward "\\.TC\\>[ \t]*\n?" nil t)
		(replace-match ""))
      (goto-char (point-min))
      (and (re-search-forward "\\[TABLE-OF-CONTENTS\\]" nil t)
		   (replace-match ".TC" t t)))

    ;; Cleanup forced line ends in items where they are not needed
    (goto-char (point-min))
    (while (re-search-forward
			"^[ \t]*\\.LI.*\\(\\\\\\\\\\)[ \t]*\\(\n\\\\label.*\\)*\n\\\\begin"
			nil t)
      (delete-region (match-beginning 1) (match-end 1)))
    (goto-char (point-min))
    (while (re-search-forward
			"^[ \t]*\\.LI.*\\(\\\\\\\\\\)[ \t]*\\(\n\\\\label.*\\)*"
			nil t)
      (if (looking-at "[\n \t]+")
		  (replace-match "\n")))

    (run-hooks 'org-export-groff-final-hook)
    (if to-buffer
		(unless (eq major-mode 'nroff-mode) (nroff-mode))
      (save-buffer))
    (org-export-groff-fix-inputenc)
    (run-hooks 'org-export-groff-after-save-hook)
    (goto-char (point-min))
    (or (org-export-push-to-kill-ring "Groff")
		(message "Exporting to Groff...done"))
    (prog1
		(if (eq to-buffer 'string)
			(prog1 (buffer-substring (point-min) (point-max))
			  (kill-buffer (current-buffer)))
		  (current-buffer))
      (set-window-configuration wcf))))

;;;###autoload
(defun org-export-as-pdf (arg &optional hidden ext-plist
							  to-buffer body-only pub-dir)
  "Export as Groff, then process through to PDF."
  (interactive "P")
  (message "Exporting to PDF...")
  (let* ((wconfig (current-window-configuration))
		 (lbuf (org-export-as-groff arg hidden ext-plist
									to-buffer body-only pub-dir))
		 (file (buffer-file-name lbuf))
		 (base (file-name-sans-extension (buffer-file-name lbuf)))
		 (pdffile (concat base ".pdf"))
		 (cmds (if (eq org-export-groff-listings 'minted)
				   ;; automatically add -shell-escape when needed
				   (mapcar (lambda (cmd)
							 (replace-regexp-in-string
							  "groff " "groff -shell-escape " cmd))
						   org-groff-to-pdf-process)
				 org-groff-to-pdf-process))
		 (outbuf (get-buffer-create "*Org PDF Groff Output*"))
		 (bibtex-p nil)
		 cmd output-dir errors)
    (with-current-buffer outbuf (erase-buffer))
    (message (concat "Processing Groff file " file "..."))
    (setq output-dir (file-name-directory file))
    (with-current-buffer lbuf
      (save-excursion
		(if (and cmds (symbolp cmds))
			(funcall cmds (shell-quote-argument file))
		  (while cmds
			(setq cmd (pop cmds))
			(while (string-match "%b" cmd)
			  (setq cmd (replace-match
						 (save-match-data
						   (shell-quote-argument base))
						 t t cmd)))
			(while (string-match "%f" cmd)
			  (setq cmd (replace-match
						 (save-match-data
						   (shell-quote-argument file))
						 t t cmd)))
			(while (string-match "%o" cmd)
			  (setq cmd (replace-match
						 (save-match-data
						   (shell-quote-argument output-dir))
						 t t cmd)))
			(shell-command cmd outbuf)))))
    (message (concat "Processing Groff file " file "...done"))
    (setq errors (org-export-groff-get-error outbuf))
    (if (not (file-exists-p pdffile))
		(error (concat "PDF file " pdffile " was not produced"
					   (if errors (concat ":" errors "") "")))
      (set-window-configuration wconfig)
      (when org-export-pdf-remove-logfiles
		(dolist (ext org-export-pdf-logfiles)
		  (setq file (concat base "." ext))
		  (and (file-exists-p file) (delete-file file))))
      (message (concat
				"Exporting to PDF...done"
				(if errors
					(concat ", with some errors:" errors)
				  "")))
      pdffile)))

(defun org-export-groff-get-error (buf)
  "Collect the kinds of errors that remain in groff processing."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward "" nil t)
		;; OK, we are at the location of the final run
		(let ((pos (point)) (errors "") (case-fold-search t))
		  (if (re-search-forward "Reference.*?undefined" nil t)
			  (setq errors (concat errors " [undefined reference]")))
		  (goto-char pos)
		  (and (org-string-nw-p errors) errors)) ))) )

;;;###autoload
(defun org-export-as-pdf-and-open (arg)
  "Export as Groff, then process through to PDF, and open."
  (interactive "P")
  (let ((pdffile (org-export-as-pdf arg)))
    (if pdffile
		(progn
		  (org-open-file pdffile)
		  (when org-export-kill-product-buffer-when-displayed
			(kill-buffer (find-buffer-visiting
						  (concat (file-name-sans-extension (buffer-file-name))
								  ".tex")))))
      (error "PDF file was not produced"))))

;;; Parsing functions:

(defun org-export-groff-parse-global (level odd)
  "Parse the current buffer recursively, starting at LEVEL.
If ODD is non-nil, assume the buffer only contains odd sections.
Return a list reflecting the document structure."
  (save-excursion
    (goto-char (point-min))
    (let* ((cnt 0) output
		   (depth org-export-groff-sectioning-depth))
      (while (org-re-search-forward-unprotected
			  (concat "^\\(\\(?:\\*\\)\\{"
					  (number-to-string (+ (if odd 2 1) level))
					  "\\}\\) \\(.*\\)$")
			  ;; make sure that there is no upper heading
			  (when (> level 0)
				(save-excursion
				  (save-match-data
					(org-re-search-forward-unprotected
					 (concat "^\\(\\(?:\\*\\)\\{"
							 (number-to-string level)
							 "\\}\\) \\(.*\\)$") nil t)))) t)
		(setq cnt (1+ cnt))
		(let* ((pos (match-beginning 0))
			   (heading (match-string 2))
			   (nlevel (if odd (/ (+ 3 level) 2) (1+ level))))
		  (save-excursion
			(narrow-to-region
			 (point)
			 (save-match-data
			   (if (org-re-search-forward-unprotected
					(concat "^\\(\\(?:\\*\\)\\{"
							(number-to-string (+ (if odd 2 1) level))
							"\\}\\) \\(.*\\)$") nil t)
				   (match-beginning 0)
				 (point-max))))
			(goto-char (point-min))
			(setq output
				  (append output
						  (list
						   (list
							`(pos . ,pos)
							`(level . ,nlevel)
							`(occur . ,cnt)
							`(heading . ,heading)
							`(content . ,(org-export-groff-parse-content))
							`(subcontent . ,(org-export-groff-parse-subcontent
											 level odd)))))))
		  (widen)))
      (list output))))

(defun org-export-groff-parse-content ()
  "Extract the content of a section."
  (let ((beg (point))
		(end (if (org-re-search-forward-unprotected "^\\(\\*\\)+ .*$" nil t)
				 (progn (beginning-of-line) (point))
			   (point-max))))
    (buffer-substring beg end)))

(defun org-export-groff-parse-subcontent (level odd)
  "Extract the subcontent of a section at LEVEL.
If ODD Is non-nil, assume subcontent only contains odd sections."
  (if (not (org-re-search-forward-unprotected
			(concat "^\\(\\(?:\\*\\)\\{"
					(number-to-string (+ (if odd 4 2) level))
					"\\}\\) \\(.*\\)$")
			nil t))
      nil								; subcontent is nil
    (org-export-groff-parse-global (+ (if odd 2 1) level) odd)))

;;; Rendering functions:
(defun org-export-groff-global (content)
  "Export CONTENT to Groff.
CONTENT is an element of the list produced by
`org-export-groff-parse-global'."
  (if (eq (car content) 'subcontent)
      (mapc 'org-export-groff-sub (cdr content))
    (org-export-groff-sub (car content))))

(defun org-export-groff-sub (subcontent)
  "Export the list SUBCONTENT to Groff.
SUBCONTENT is an alist containing information about the headline
and its content."
  (let ((num (plist-get org-export-groff-options-plist :section-numbers)))
    (mapc (lambda(x) (org-export-groff-subcontent x num)) subcontent)))

(defun org-export-groff-subcontent (subcontent num)
  "Export each cell of SUBCONTENT to Groff.
If NUM is non-nil export numbered sections, otherwise use unnumbered
sections.  If NUM is an integer, export the highest NUM levels as
numbered sections and lower levels as unnumbered sections."
  (let* ((heading (cdr (assoc 'heading subcontent)))
		 (level (- (cdr (assoc 'level subcontent))
				   org-export-groff-add-level))
		 (occur (number-to-string (cdr (assoc 'occur subcontent))))
		 (content (cdr (assoc 'content subcontent)))
		 (subcontent (cadr (assoc 'subcontent subcontent)))
		 (label (org-get-text-property-any 0 'target heading))
		 (label-list (cons label (cdr (assoc label
											 org-export-target-aliases))))
		 (sectioning org-export-groff-sectioning)
		 (depth org-export-groff-sectioning-depth)
		 main-heading sub-heading ctnt)
    (when (symbolp (car sectioning))
      (setq sectioning (funcall (car sectioning) level heading))
      (when sectioning
		(setq heading (car sectioning)
			  sectioning (cdr sectioning)
			  ;; target property migh have changed...
			  label (org-get-text-property-any 0 'target heading)
			  label-list (cons label (cdr (assoc label
												 org-export-target-aliases)))))
      (if sectioning (setq sectioning (make-list 10 sectioning)))
      (setq depth (if sectioning 10000 0)))
    (if (string-match "[ \t]*\\\\\\\\[ \t]*" heading)
		(setq main-heading (substring heading 0 (match-beginning 0))
			  sub-heading (substring heading (match-end 0))))
    (setq heading (org-export-groff-fontify-headline heading)
		  sub-heading (and sub-heading
						   (org-export-groff-fontify-headline sub-heading))
		  main-heading (and main-heading
							(org-export-groff-fontify-headline main-heading)))
    (cond
     ;; Normal conversion
     ((<= level depth)
      (let* ((sec (nth (1- level) sectioning))
			 (num (if (integerp num)
					  (>= num level)
					num))
			 start end)
		(if (consp (cdr sec))
			(setq start (nth (if num 0 2) sec)
				  end (nth (if num 1 3) sec))
		  (setq start (if num (car sec) (cdr sec))))
		(insert (format start (if main-heading main-heading heading)
						(or sub-heading "")))
		(insert	"\n")
		;; from when label
		(when nil
		  (insert (mapconcat (lambda (l) (format "%s" l))
							 label-list "\n") "\n"))
		(insert (org-export-groff-content content))
		(cond ((stringp subcontent) (insert subcontent))
			  ((listp subcontent)
			   (while (org-looking-back "\n\n") (backward-delete-char 1))
			   (org-export-groff-sub subcontent)))
		(when (and end (string-match "[^ \t]" end))
		  (let ((hook (org-get-text-property-any 0 'org-insert-hook end)))
			(and (functionp hook) (funcall hook)))
		  (insert end "\n"))))
     ;; At a level under the hl option: we can drop this subsection
	 ;;
	 ;; Needs to be debugged
	 ;;
     ((> level depth)
      (cond ((eq org-export-groff-low-levels 'description)
			 (insert "%s") )
			((stringp org-export-groff-low-levels)
			 (insert (format org-export-groff-low-levels heading) "\n")
			 (insert (org-export-groff-content content))
			 (cond ((stringp subcontent) (insert subcontent))
				   ((listp subcontent) (org-export-groff-sub subcontent)))))))))

;;; Exporting internals:
(defun org-export-groff-set-initial-vars (ext-plist level)
  "Store org local variables required for Groff export.
EXT-PLIST is an optional additional plist.
LEVEL indicates the default depth for export."
  (setq org-export-groff-todo-keywords-1 org-todo-keywords-1
		org-export-groff-done-keywords org-done-keywords
		org-export-groff-not-done-keywords org-not-done-keywords
		org-export-groff-complex-heading-re org-complex-heading-regexp
		org-export-groff-display-custom-times org-display-custom-times
		org-export-groff-all-targets-re
		(org-make-target-link-regexp (org-all-targets))
		org-export-groff-options-plist
		(org-combine-plists (org-default-export-plist) ext-plist
							(org-infile-export-plist))
		org-export-groff-class
		(or (and (org-region-active-p)
				 (save-excursion
				   (goto-char (region-beginning))
				   (and (looking-at org-complex-heading-regexp)
						(org-entry-get nil "Groff_CLASS" 'selective))))
			(save-excursion
			  (save-restriction
				(widen)
				(goto-char (point-min))
				(and (re-search-forward "^#\\+Groff_CLASS:[ \t]*\\([-/a-zA-Z]+\\)" nil t)
					 (match-string 1))))
			(plist-get org-export-groff-options-plist :groff-class)
			org-export-groff-default-class)
		org-export-groff-class-options
		(or (and (org-region-active-p)
				 (save-excursion
				   (goto-char (region-beginning))
				   (and (looking-at org-complex-heading-regexp)
						(org-entry-get nil "Groff_CLASS_OPTIONS" 'selective))))
			(save-excursion
			  (save-restriction
				(widen)
				(goto-char (point-min))
				(and (re-search-forward "^#\\+Groff_CLASS_OPTIONS:[ \t]*\\(.*?\\)[ \t]*$" nil t)
					 (match-string 1))))
			(plist-get org-export-groff-options-plist :groff-class-options))
		org-export-groff-class
		(or (car (assoc org-export-groff-class org-export-groff-classes))
			(error "No definition for class `%s' in `org-export-groff-classes'"
				   org-export-groff-class))
		org-export-groff-header
		(cadr (assoc org-export-groff-class org-export-groff-classes))
		org-export-groff-sectioning
		(cddr (assoc org-export-groff-class org-export-groff-classes))
		org-export-groff-sectioning-depth
		(or level
			(let ((hl-levels
				   (plist-get org-export-groff-options-plist :headline-levels))
				  (sec-depth (length org-export-groff-sectioning)))
			  (if (> hl-levels sec-depth) sec-depth hl-levels))))
  (when (and org-export-groff-class-options
			 (string-match "\\S-" org-export-groff-class-options)
			 (string-match "^[ \t]*\\(\\.MT\\)\\(\\[.*?\\]\\)?"
						   org-export-groff-header))
    (setq org-export-groff-header
		  (concat (substring org-export-groff-header 0 (match-end 1))
				  org-export-groff-class-options
				  (substring org-export-groff-header (match-end 0))))))

(defvar org-export-groff-format-toc-function
  'org-export-groff-format-toc-default
  "The function formatting returning the string to create the table of contents.
The function mus take one parameter, the depth of the table of contents.")

(defun org-export-groff-make-header (title opt-plist)
  "Make the Groff header and return it as a string.
TITLE is the current title from the buffer or region.
OPT-PLIST is the options plist for current buffer."
  (let ((toc (plist-get opt-plist :table-of-contents))
		(author (org-export-apply-macros-in-string
				 (plist-get opt-plist :author)))
		(email (replace-regexp-in-string
				"_" "\\\\_"
				(org-export-apply-macros-in-string
				 (plist-get opt-plist :email))))
		(description (org-export-apply-macros-in-string
					  (plist-get opt-plist :description)))
		(keywords (org-export-apply-macros-in-string
				   (plist-get opt-plist :keywords))))
    (concat
     
     ;; insert Groff custom header and packages from the list

     (org-export-apply-macros-in-string org-export-groff-append-header)
     ;; insert the title
     (format
      ".TL\n%s\n"
      (org-export-groff-fontify-headline title))
     ;; insert author info
     (if (plist-get opt-plist :author-info)
		 (format "\n.AU \"%s\"\n"
				 (org-export-groff-fontify-headline (or author user-full-name)) )
       (format ".AU \"%s\" \n"
			   (org-export-groff-fontify-headline (or author user-full-name))))
     ;; insert the date
     (format ".ND \"%s\"\n"
			 (format-time-string org-export-groff-date-format ))
     ;; add some hyperref options
     ;; FIXME: let's have a defcustom for this?

     (format ".MT \"%s\"\n"
			 (concat "Emacs Org-mode version " org-version))
     ;; beginning of the document
     "\n.P\n\n"
     ;; insert the title command
     (when (string-match "\\S-" title)
       (if (string-match "%s" org-export-groff-title-command)
		   (format org-export-groff-title-command title)
		 org-export-groff-title-command))
     "\n\n"
     ;; table of contents
     (when (and org-export-with-toc
				(plist-get opt-plist :section-numbers))
       (funcall org-export-groff-format-toc-function
				(cond ((numberp toc)
					   (min toc (plist-get opt-plist :headline-levels)))
					  (toc  (plist-get opt-plist :headline-levels))))))))

(defun org-export-groff-format-toc-default (depth)
  (when depth
    (format "" )))

(defun org-export-groff-first-lines (opt-plist &optional beg end)
  "Export the first lines before first headline.
If BEG is non-nil, it is the beginning of the region.
If END is non-nil, it is the end of the region."
  (save-excursion
    (goto-char (or beg (point-min)))
    (let* ((pt (point))
		   (end (if (re-search-forward
					 (concat "^" (org-get-limited-outline-regexp)) end t)
					(goto-char (match-beginning 0))
				  (goto-char (or end (point-max))))))
      (prog1
		  (org-export-groff-content
		   (org-export-preprocess-string
			(buffer-substring pt end)
			:for-backend 'groff
			:emph-multiline t
			:add-text nil
			:comments nil
			:skip-before-1st-heading nil
			:Groff-fragments nil
			:timestamps (plist-get opt-plist :timestamps)
			:footnotes (plist-get opt-plist :footnotes)))
		(org-unmodified
		 (let ((inhibit-read-only t)
			   (limit (max pt (1- end))))
		   (add-text-properties pt limit
								'(:org-license-to-kill t))
		   (save-excursion
			 (goto-char pt)
			 (while (re-search-forward "^[ \t]*#\\+.*\n?" limit t)
			   (let ((case-fold-search t))
				 (unless (org-string-match-p
						  "^[ \t]*#\\+\\(attr_\\|caption\\>\\|label\\>\\)"
						  (match-string 0))
				   (remove-text-properties (match-beginning 0) (match-end 0)
										   '(:org-license-to-kill t))))))))))))


(defvar org-export-groff-header-defs nil
  "The header definitions that might be used in the Groff body.")

(defun org-export-groff-content (content &optional exclude-list)
  "Convert CONTENT string to Groff.
Don't perform conversions that are in EXCLUDE-LIST.  Recognized
conversion types are: quotation-marks, emphasis, sub-superscript,
links, keywords, lists, tables, fixed-width"
  (with-temp-buffer
    (org-install-letbind)
    (insert content)
    (unless (memq 'timestamps exclude-list)
      (org-export-groff-time-stamps))
    (unless (memq 'quotation-marks exclude-list)
      (org-export-groff-quotation-marks))
    (unless (memq 'emphasis exclude-list)
      (when (plist-get org-export-groff-options-plist :emphasize)
		(org-export-groff-fontify)))
    (unless (memq 'sub-superscript exclude-list)
      (org-export-groff-special-chars
       (plist-get org-export-groff-options-plist :sub-superscript)))
	(unless (memq 'links exclude-list)
	  (org-export-groff-links))
    (unless (memq 'keywords exclude-list)
      (org-export-groff-keywords))
    (unless (memq 'lists exclude-list)
      (org-export-groff-lists))
	(unless (memq 'tables exclude-list)
	  (org-export-groff-tables
       (plist-get org-export-groff-options-plist :tables)))
    (unless (memq 'fixed-width exclude-list)
      (org-export-groff-fixed-width
       (plist-get org-export-groff-options-plist :fixed-width)))
	;; return string
    (buffer-substring (point-min) (point-max))))

(defun org-export-groff-protect-string (s)
  "Add the org-protected property to string S."
  (add-text-properties 0 (length s) '(org-protected t) s) s)

(defun org-export-groff-protect-char-in-string (char-list string)
  "Add org-protected text-property to char from CHAR-LIST in STRING."
  (with-temp-buffer
    (save-match-data
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward (regexp-opt char-list) nil t)
		(add-text-properties (match-beginning 0)
							 (match-end 0) '(org-protected t)))
      (buffer-string))))

(defun org-export-groff-keywords-maybe (&optional remove-list)
  "Maybe remove keywords depending on rules in REMOVE-LIST."
  (goto-char (point-min))
  (let ((re-todo (mapconcat 'identity org-export-groff-todo-keywords-1 "\\|"))
		(case-fold-search nil)
		(todo-markup org-export-groff-todo-keyword-markup)
		fmt)
    ;; convert TODO keywords
    (when (re-search-forward (concat "^\\(" re-todo "\\)") nil t)
      (if (plist-get remove-list :todo)
		  (replace-match "")
		(setq fmt (cond
				   ((stringp todo-markup) todo-markup)
				   ((and (consp todo-markup) (stringp (car todo-markup)))
					(if (member (match-string 1) org-export-groff-done-keywords)
						(cdr todo-markup) (car todo-markup)))
				   (t (cdr (or (assoc (match-string 1) todo-markup)
							   (car todo-markup))))))
		(replace-match (org-export-groff-protect-string
						(format fmt (match-string 1))) t t)))
    ;; convert priority string
    (when (re-search-forward "\\[\\\\#.\\]" nil t)
      (if (plist-get remove-list :priority)
		  (replace-match "")
		(replace-match (format "\\fB%s\\fP" (match-string 0)) t t)))
    ;; convert tags
    (when (re-search-forward "\\(:[a-zA-Z0-9_@#%]+\\)+:" nil t)
      (if (or (not org-export-with-tags)
			  (plist-get remove-list :tags))
		  (replace-match "")
		(replace-match
		 (org-export-groff-protect-string
		  (format org-export-groff-tag-markup
				  (save-match-data
					(replace-regexp-in-string
					 "\\([_#]\\)" "\\\\\\1" (match-string 0)))))
		 t t)))))

(defun org-export-groff-fontify-headline (string)
  "Fontify special words in STRING."
  (with-temp-buffer
    ;; FIXME: org-inside-Groff-fragment-p doesn't work when the $...$ is at
    ;; the beginning of the buffer - inserting "\n" is safe here though.
    (insert "\n" string)

    ;; Preserve math snippets

    (let* ((matchers '())
		   (re-list nil)
		   beg end re e m n block off)
      ;; Check the different regular expressions
      (while (setq e (pop re-list))
		(setq m (car e) re (nth 1 e) n (nth 2 e)
			  block (if (nth 3 e) "\n\n" ""))
		(setq off (if (member m '("$" "$1")) 1 0))
		(when (and (member m matchers) (not (equal m "begin")))
		  (goto-char (point-min))
		  (while (re-search-forward re nil t)
			(setq beg (+ (match-beginning 0) off) end (- (match-end 0) 0))
			(add-text-properties beg end
								 '(org-protected t org-groff-math t))))))


    (goto-char (point-min))
    (let ((re (concat "\\\\\\([a-zA-Z]+\\)"
					  "\\(?:<[^<>\n]*>\\)*"
					  "\\(?:\\[[^][\n]*?\\]\\)*"
					  "\\(?:<[^<>\n]*>\\)*"
					  "\\("
					  (org-create-multibrace-regexp "{" "}" 3)
					  "\\)\\{1,3\\}")))
      (while (re-search-forward re nil t)
		(unless (or
				 ;; check for comment line
				 (save-excursion (goto-char (match-beginning 0))
								 (org-in-indented-comment-line))
				 ;; Check if this is a defined entity, so that is may need conversion
				 (org-entity-get (match-string 1)))
		  (add-text-properties (match-beginning 0) (match-end 0)
							   '(org-protected t)))))
    (when (plist-get org-export-groff-options-plist :emphasize)
      (org-export-groff-fontify))
    (org-export-groff-time-stamps)
    (org-export-groff-quotation-marks)
    (org-export-groff-keywords-maybe)
    (org-export-groff-special-chars
     (plist-get org-export-groff-options-plist :sub-superscript))
    (org-export-groff-links)
    (org-trim (buffer-string))))

(defun org-export-groff-time-stamps ()
  "Format time stamps."
  (goto-char (point-min))
  (let ((org-display-custom-times org-export-groff-display-custom-times))
    (while (re-search-forward org-ts-regexp-both nil t)
      (org-if-unprotected-at (1- (point))
		(replace-match
		 (org-export-groff-protect-string
		  (format (if (string= "<" (substring (match-string 0) 0 1))
					  org-export-groff-timestamp-markup
					org-export-groff-timestamp-inactive-markup)
				  (substring (org-translate-time (match-string 0)) 1 -1)))
		 t t)))))

(defun org-export-groff-quotation-marks ()
  "Export quotation marks depending on language conventions."
  (mapc (lambda(l)
		  (goto-char (point-min))
		  (while (re-search-forward (car l) nil t)
			(let ((rpl (concat (match-string 1)
							   (org-export-groff-protect-string
								(copy-sequence (cdr l))))))
			  (org-if-unprotected-1
			   (replace-match rpl t t)))))
		(cdr (or (assoc (plist-get org-export-groff-options-plist :language)
						org-export-groff-quotes)
				 ;; falls back on english
				 (assoc "en" org-export-groff-quotes)))))

(defun org-export-groff-special-chars (sub-superscript)
  "Export special characters to Groff.
If SUB-SUPERSCRIPT is non-nil, convert \\ and ^.
See the `org-export-groff.el' code for a complete conversion table."
  (goto-char (point-min))
  (mapc (lambda(c)
		  (goto-char (point-min))
		  (while (re-search-forward c nil t)
			;; Put the point where to check for org-protected
			(unless (or (get-text-property (match-beginning 2) 'org-protected)
						(save-match-data (org-at-table.el-p)))
			  (cond ((member (match-string 2) '("\\$" "$"))
					 (if (equal (match-string 2) "\\$")
						 nil
					   (replace-match "\\$" t t)))
					((member (match-string 2) '("&" "%" "#"))
					 (if (equal (match-string 1) "\\")
						 (replace-match (match-string 2) t t)
					   (replace-match (concat (match-string 1) "\\"
											  (match-string 2)) t t)
					   (backward-char 1)))
					((equal (match-string 2) "...")
					 (replace-match
					  (concat (match-string 1)
							  (org-export-groff-protect-string "\\ldots{}")) t t))
					((equal (match-string 2) "~")
					 (cond ((equal (match-string 1) "\\") nil)
						   ((eq 'org-link (get-text-property 0 'face (match-string 2)))
							(replace-match (concat (match-string 1) "\\~") t t))
						   (t (replace-match
							   (org-export-groff-protect-string
								(concat (match-string 1) "\\~{}")) t t))))
					((member (match-string 2) '("{" "}"))
					 (unless (save-match-data (org-inside-groff-math-p))
					   (if (equal (match-string 1) "\\")
						   (replace-match (match-string 2) t t)
						 (replace-match (concat (match-string 1) "\\"
												(match-string 2)) t t)))))
			  (unless (save-match-data (org-inside-groff-math-p))
				(cond ((equal (match-string 2) "\\")
					   (replace-match (or (save-match-data
											(org-export-groff-treat-backslash-char
											 (match-string 1)
											 (or (match-string 3) "")))
										  "") t t)
					   (when (and (get-text-property (1- (point)) 'org-entity)
								  (looking-at "{}"))
						 ;; OK, this was an entity replacement, and the user
						 ;; had terminated the entity with {}.  Make sure
						 ;; {} is protected as well, and remove the extra {}
						 ;; inserted by the conversion.
						 (put-text-property (point) (+ 2 (point)) 'org-protected t)
						 (if (save-excursion (goto-char (max (- (point) 2) (point-min)))
											 (looking-at "{}"))
							 (replace-match ""))
						 (forward-char 2))
					   (backward-char 1))
					  ((member (match-string 2) '("_" "^"))
					   (replace-match (or (save-match-data
											(org-export-groff-treat-sub-super-char
											 sub-superscript
											 (match-string 2)
											 (match-string 1)
											 (match-string 3))) "") t t)
					   (backward-char 1)))))))
		'(			;"^\\([^\n$]*?\\|^\\)\\(\\\\?\\$\\)\\([^\n$]*\\)$"
		  "\\(\\(\\\\?\\$\\)\\)"
		  "\\([a-zA-Z0-9()]+\\|[ \t\n]\\|\\b\\|\\\\\\)\\(_\\|\\^\\)\\({[^{}]+}\\|[a-zA-Z0-9]+\\|[ \t\n]\\|[:punct:]\\|)\\|{[a-zA-Z0-9]+}\\|([a-zA-Z0-9]+)\\)"
		  "\\(.\\|^\\)\\(\\\\\\)\\([ \t\n]\\|\\([&#%{}\"]\\|[a-zA-Z][a-zA-Z0-9]*\\)\\)"
		  "\\(^\\|.\\)\\([&#%{}~]\\|\\.\\.\\.\\)"
		  ;; (?\< . "\\textless{}")
		  ;; (?\> . "\\textgreater{}")
		  )))

(defun org-inside-groff-math-p ()
  (get-text-property (point) 'org-groff-math))

(defun org-export-groff-treat-sub-super-char
  (subsup char string-before string-after)
  "Convert the \"_\" and \"^\" characters to Groff.
SUBSUP corresponds to the ^: option in the #+OPTIONS line.
Convert CHAR depending on STRING-BEFORE and STRING-AFTER."
  (cond ((equal string-before "\\")
		 (concat string-before char string-after))
		((and (string-match "\\S-+" string-after))
		 ;; this is part of a math formula
		 (cond ((eq 'org-link (get-text-property 0 'face char))
				(concat string-before "\\" char string-after))
			   ((save-match-data (org-inside-groff-math-p))
				(if subsup
					(cond ((eq 1 (length string-after))
						   (concat string-before char string-after))
						  ((string-match "[({]?\\([^)}]+\\)[)}]?" string-after)
						   (format "%s%s{%s}" string-before char
								   (match-string 1 string-after))))))
			   ((and (> (length string-after) 1)
					 (or (eq subsup t)
						 (and (equal subsup '{}) (eq (string-to-char string-after) ?\{)))
					 (or (string-match "[{]?\\([^}]+\\)[}]?" string-after)
						 (string-match "[(]?\\([^)]+\\)[)]?" string-after)))

				(org-export-groff-protect-string
				 (format "%s$%s{%s}$" string-before char
						 (if (and (> (match-end 1) (1+ (match-beginning 1)))
								  (not (equal (substring string-after 0 2) "{\\")))
							 (concat "\\mathrm{" (match-string 1 string-after) "}")
						   (match-string 1 string-after)))))
			   ((eq subsup t) (concat string-before "$" char string-after "$"))
			   (t (org-export-groff-protect-string
				   (concat string-before "\\" char "{}" string-after)))))
		(t (org-export-groff-protect-string
			(concat string-before "\\" char "{}" string-after)))))

(defun org-export-groff-treat-backslash-char (string-before string-after)
  "Convert the \"$\" special character to Groff.
The conversion is made depending of STRING-BEFORE and STRING-AFTER."
  (let  ((ass (org-entity-get string-after)))
    (cond
     (ass (org-add-props
			  (if (nth 2 ass)
				  (concat string-before
						  (org-export-groff-protect-string
						   (concat "$" (nth 1 ass) "$")))
				(concat string-before (org-export-groff-protect-string
									   (nth 1 ass))))
			  nil 'org-entity t))
     ((and (not (string-match "^[ \n\t]" string-after))
		   (not (string-match "[ \t]\\'\\|^" string-before)))
      ;; backslash is inside a word
      (concat string-before
			  (org-export-groff-protect-string
			   (concat "\\textbackslash{}" string-after))))
     ((not (or (equal string-after "")
			   (string-match "^[ \t\n]" string-after)))
      ;; backslash might escape a character (like \#) or a user TeX
      ;; macro (like \setcounter)
      (concat string-before
			  (org-export-groff-protect-string (concat "\\" string-after))))
     ((and (string-match "^[ \t\n]" string-after)
		   (string-match "[ \t\n]\\'" string-before))
      ;; backslash is alone, convert it to $\backslash$
      (org-export-groff-protect-string
       (concat string-before "\\textbackslash{}" string-after)))
     (t (org-export-groff-protect-string
		 (concat string-before "\\textbackslash{}" string-after))))))

(defun org-export-groff-keywords ()
  "Convert special keywords to Groff."
  (goto-char (point-min))
  (while (re-search-forward org-export-groff-special-keyword-regexp nil t)
    (replace-match (format org-export-groff-timestamp-keyword-markup
						   (match-string 0)) t t)
    (save-excursion
      (beginning-of-line 1)
      (unless (looking-at ".*\n[ \t]*\n")
		(end-of-line 1)
		(insert "\n")))))

(defun org-export-groff-fixed-width (opt)
  "When OPT is non-nil convert fixed-width sections to Groff."
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]*:\\([ \t]\\|$\\)" nil t)
    (unless (get-text-property (point) 'org-example)
	  (if opt
		  (progn (goto-char (match-beginning 0))
				 (insert ".DS L\n")
				 (while (looking-at "^\\([ \t]*\\):\\(\\([ \t]\\|$\\).*\\)$")
				   (replace-match (concat (match-string 1)
										  (match-string 2)) t t)
				   (forward-line))
				 (insert ".DE\n"))
		(progn (goto-char (match-beginning 0))
			   (while (looking-at "^\\([ \t]*\\):\\(\\([ \t]\\|$\\).*\\)$")
				 (replace-match (concat "%" (match-string 1)
										(match-string 2)) t t)
				 (forward-line)))))))

(defvar org-table-last-alignment)		; defined in org-table.el
(defvar org-table-last-column-widths)	; defined in org-table.el

(declare-function orgtbl-to-groff "org-table" (table params) t)

(defun org-export-groff-tables (insert)
  "Convert tables to Groff and INSERT it."
  ;; First, get the table.el tables
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]*\\(\\+-[-+]*\\+\\)[ \t]*\n[ \t]*|" nil t)
    (org-if-unprotected
     (require 'table)
     (org-export-groff-convert-table.el-table)))

  ;; And now the Org-mode tables
  (goto-char (point-min))
  (while (re-search-forward "^\\([ \t]*\\)|" nil t)
    (org-if-unprotected-at (1- (point))
      (org-table-align)
      (let* ((beg (org-table-begin))
             (end (org-table-end))
             (raw-table (buffer-substring beg end))
             (org-table-last-alignment (copy-sequence org-table-last-alignment))
             (org-table-last-column-widths (copy-sequence
                                            org-table-last-column-widths))
             fnum fields line lines olines gr colgropen line-fmt align
             caption width shortn label attr floatp placement
			 longtblp tblenv tabular-env)

        (if org-export-groff-tables-verbatim
            (let* ((tbl (concat ".DS C\n" raw-table
                                ".DE\n")))
              (apply 'delete-region (list beg end))
              (insert (org-export-groff-protect-string tbl)))
		  ;; else
          (progn
            (setq caption (org-find-text-property-in-string
                           'org-caption raw-table)
				  shortn (org-find-text-property-in-string
						  'org-caption-shortn raw-table)
                  attr (org-find-text-property-in-string
                        'org-attributes raw-table)
                  label (org-find-text-property-in-string
                         'org-label raw-table)
                  longtblp (and attr (stringp attr)
                                (string-match "\\<longtable\\>" attr))
				  tblenv (if (and attr (stringp attr))
							 (cond ((string-match "\\<sidewaystable\\>" attr)
									"sidewaystable")
								   ((or (string-match (regexp-quote "table*") attr)
										(string-match "\\<multicolumn\\>" attr))
									"table*")
								   (t "table"))
						   "table")
				  tabular-env
				  (if (and attr (stringp attr)
						   (string-match "\\(tabular.\\)" attr))
					  (match-string 1 attr)
					org-export-groff-tabular-environment)
				  width (and attr (stringp attr)
                             (string-match "\\<width=\\([^ \t\n\r]+\\)" attr)
                             (match-string 1 attr))
                  align (and attr (stringp attr)
                             (string-match "\\<align=\\([^ \t\n\r]+\\)" attr)
                             (match-string 1 attr))
                  floatp (or caption label (string= "table*" tblenv))
				  placement     (if (and attr
										 (stringp attr)
										 (string-match "[ \t]*\\<placement=\\(\\S-+\\)" attr))
									(match-string 1 attr)
								  (concat
								   "[" org-groff-default-figure-position "]")))
			(setq caption (and caption (org-export-groff-fontify-headline caption)))
            (setq lines (org-split-string raw-table "\n"))
            (apply 'delete-region (list beg end))
            (when org-export-table-remove-special-lines
              (setq lines (org-table-clean-before-export lines 'maybe-quoted)))
            (when org-table-clean-did-remove-column
			  (pop org-table-last-alignment)
			  (pop org-table-last-column-widths))
            ;; make a formatting string to reflect alignment
            (setq olines lines)
            (while (and (not line-fmt) (setq line (pop olines)))
              (unless (string-match "^[ \t]*|-" line)
                (setq fields (org-split-string line "[ \t]*|[ \t]*"))
                (setq fnum (make-vector (length fields) 0))
                (setq line-fmt
                      (mapconcat
                       (lambda (x)
                         (setq gr (pop org-table-colgroup-info))
                         (format "%s%%s%s"
                                 (cond ((eq gr :start)
                                        (prog1 (if colgropen "|" "|")
                                          (setq colgropen t)))
                                       ((eq gr :startend)
                                        (prog1 (if colgropen "|" "|")
                                          (setq colgropen nil)))
                                       (t ""))
                                 (if (memq gr '(:end :startend))
                                     (progn (setq colgropen nil) "|")
                                   "")))
                       fnum ""))))
            ;; fix double || in line-fmt
            (setq line-fmt (replace-regexp-in-string "||" "|" line-fmt))
            ;; maybe remove the first and last "|"
            (when (and (not org-export-groff-tables-column-borders)
                       (string-match "^\\(|\\)?\\(.+\\)|$" line-fmt))
              (setq line-fmt (match-string 2 line-fmt)))
            ;; format alignment
            (unless align
              (setq align (apply 'format
                                 (cons line-fmt
                                       (mapcar (lambda (x) (if x "r" "l"))
                                               org-table-last-alignment)))))
            ;; prepare the table to send to orgtbl-to-groff
            (setq lines
                  (mapcar
                   (lambda(elem)
                     (or (and (string-match "[ \t]*|-+" elem) 'hline)
                         (org-split-string
						  (progn (set-text-properties 0 (length elem) nil elem)
								 (org-trim elem)) "|")))
                   lines))

            (when insert
			  (setq first-line (car lines))
              (insert (org-export-groff-protect-string
                       (concat
                        (if  org-export-groff-tables-centered 
                            ".DS C\n")
						(concat ".TS\n"
								"box,center;\n" )
						(format "%s.\n"
								(let ((linea ""))
								  (dotimes (i (length first-line))
									(setq linea (concat linea "cb" " "))
									)
								  (setq linea (concat linea "\n"))
								  (dotimes (i (length first-line))
									(setq linea (concat linea "c" " ")))  linea ))
						(format "%s"
								(let ((linea ""))
								  (dolist (line-item lines)
									(cond ((listp line-item)
										   (dolist (line-object line-item)
											 (setq linea (concat linea (format "%s\t" line-object)))
											 )
										   (setq linea (concat linea "\n"))
										   )
										  ((eq line-item "hline") (setq linea (concat linea "_\n")))
										  )
									)  linea))

						(format "\n.TE" tabular-env)
						(if org-export-groff-tables-centered
							"\n.DE\n" "\n")))
                      "\n\n"))))))))

(defun org-export-groff-convert-table.el-table ()
  "Replace table.el table at point with Groff code."
  (let (tbl caption shortn label line floatp attr align rmlines)
    (setq line (buffer-substring (point-at-bol) (point-at-eol))
		  label (org-get-text-property-any 0 'org-label line)
		  caption (org-get-text-property-any 0 'org-caption line)
		  shortn (org-get-text-property-any 0 'org-caption-shortn line)
		  attr (org-get-text-property-any 0 'org-attributes line)
		  align (and attr (stringp attr)
					 (string-match "\\<align=\\([^ \t\n\r,]+\\)" attr)
					 (match-string 1 attr))
		  rmlines (and attr (stringp attr)
					   (string-match "\\<rmlines\\>" attr))
		  floatp (or label caption))
    (and (get-buffer "*org-export-table*")
		 (kill-buffer (get-buffer "*org-export-table*")))
    (table-generate-source 'groff "*org-export-table*" "caption")
    (setq tbl (with-current-buffer "*org-export-table*"
				(buffer-string)))
    (while (string-match "^%.*\n" tbl)
      (setq tbl (replace-match "" t t tbl)))
    ;; fix the hlines
    (when rmlines
      (let ((n 0) lines)
		(setq lines (mapcar (lambda (x)
							  (if (string-match "^\\\\hline$" x)
								  (progn
									(setq n (1+ n))
									(if (= n 2) x nil))
								x))
							(org-split-string tbl "\n")))
		(setq tbl (mapconcat 'identity (delq nil lines) "\n"))))
    (when (and align (string-match ".TS\n" tbl))
      (setq tbl (replace-match (concat ".TS\n")
							   t t tbl)))
    (and (get-buffer "*org-export-table*")
		 (kill-buffer (get-buffer "*org-export-table*")))
    (beginning-of-line 0)
    (while (looking-at "[ \t]*\\(|\\|\\+-\\)")
      (delete-region (point) (1+ (point-at-eol))))
    (when org-export-groff-tables-centered
      (setq tbl (concat ".DS C" tbl ".DE")))
    (when floatp
      (setq tbl (concat ".TS\n"
						(if (not org-export-groff-table-caption-above) tbl)
						"\n\\.TE\n")))
    (insert (org-export-groff-protect-string tbl))))

(defun org-export-groff-fontify ()
  "Convert fontification to Groff."
  (goto-char (point-min))
  (while (re-search-forward org-emph-re nil t)
    ;; The match goes one char after the *string*, except at the end of a line
    (let ((emph (assoc (match-string 3)
					   org-export-groff-emphasis-alist))
		  (beg (match-beginning 0))
		  (end (match-end 0))
		  rpl s)
      (unless emph
		(message "`org-export-groff-emphasis-alist' has no entry for formatting triggered by \"%s\""
				 (match-string 3)))
      (unless (or (and (get-text-property (- (point) 2) 'org-protected)
					   (not (get-text-property
							 (- (point) 2) 'org-verbatim-emph)))
				  (equal (char-after (match-beginning 3))
						 (char-after (1+ (match-beginning 3))))
				  (save-excursion
					(goto-char (match-beginning 1))
					(save-match-data
					  (and (org-at-table-p)
						   (string-match
							"[|\n]" (buffer-substring beg end)))))
				  (and (equal (match-string 3) "+")
					   (save-match-data
						 (string-match "\\`-+\\'" (match-string 4)))))
		(setq s (match-string 4))
		(setq rpl (concat (match-string 1)
						  (org-export-groff-emph-format (cadr emph)
														(match-string 4))
						  (match-string 5)))
		(if (caddr emph)
			(setq rpl (org-export-groff-protect-string rpl))
		  (save-match-data
			(if (string-match "\\`.?\\(\\\\[a-z]+{\\)\\(.*\\)\\(}\\).?\\'" rpl)
				(progn
				  (add-text-properties (match-beginning 1) (match-end 1)
									   '(org-protected t) rpl)
				  (add-text-properties (match-beginning 3) (match-end 3)
									   '(org-protected t) rpl)))))
		(replace-match rpl t t)))
    (backward-char)))

(defun org-export-groff-emph-format (format string)
  "Format an emphasis string and handle the \\verb special case."
  (when (member format '("\\verb" "\\protectedtexttt"))
    (save-match-data
      (if (equal format "\\verb")
		  (let ((ll "~,./?;':\"|!@#%^&-_=+abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ<>()[]{}"))
			(catch 'exit
			  (loop for i from 0 to (1- (length ll)) do
					(if (not (string-match (regexp-quote (substring ll i (1+ i)))
										   string))
						(progn
						  (setq format (concat "\\verb" (substring ll i (1+ i))
											   "%s" (substring ll i (1+ i))))
						  (throw 'exit nil))))))
		(let ((start 0)
			  (trans '(("\\" . "\\textbackslash{}")
					   ("~" . "\\textasciitilde{}")
					   ("^" . "\\textasciicircum{}")))
			  (rtn "") char)
		  (while (string-match "[\\{}$%&_#~^]" string)
			(setq char (match-string 0 string))
			(if (> (match-beginning 0) 0)
				(setq rtn (concat rtn (substring string
												 0 (match-beginning 0)))))
			(setq string (substring string (1+ (match-beginning 0))))
			(setq char (or (cdr (assoc char trans)) (concat "\\" char))
				  rtn (concat rtn char)))
		  (setq string (concat rtn string) format "\\texttt{%s}")
		  (while (string-match "--" string)
			(setq string (replace-match "-{}-" t t string)))))))
  (format format string))

(defun org-export-groff-links ()
  ;; Make sure to use the Groff hyperref and graphicx package
  ;; or send some warnings.
  "Convert links to Groff."
  (goto-char (point-min))
  (while (re-search-forward org-bracket-link-analytic-regexp++ nil t)
    (org-if-unprotected-1
     (goto-char (match-beginning 0))
     (let* ((re-radio org-export-groff-all-targets-re)
			(remove (list (match-beginning 0) (match-end 0)))
			(raw-path (org-extract-attributes (match-string 3)))
			(full-raw-path (concat (match-string 1) raw-path))
			(desc (match-string 5))
			(type (or (match-string 2)
					  (if (or (file-name-absolute-p raw-path)
							  (string-match "^\\.\\.?/" raw-path))
						  "file")))
			(coderefp (equal type "coderef"))
			(caption (org-find-text-property-in-string 'org-caption raw-path))
			(shortn (org-find-text-property-in-string 'org-caption-shortn raw-path))
			(attr (or (org-find-text-property-in-string 'org-attributes raw-path)
					  (plist-get org-export-groff-options-plist :groff-image-options)))
			(label (org-find-text-property-in-string 'org-label raw-path))
			imgp radiop fnc
			;; define the path of the link
			(path (cond
				   ((member type '("coderef"))
					raw-path)
				   ((member type '("http" "https" "ftp"))
					(concat type ":" raw-path))
				   ((and re-radio (string-match re-radio raw-path))
					(setq radiop t))
				   ((equal type "mailto")
					(concat type ":" raw-path))
				   ((equal type "file")
					(if (and (org-file-image-p
							  (expand-file-name
							   raw-path)
							  org-export-groff-inline-image-extensions)
							 (or (get-text-property 0 'org-no-description
													raw-path)
								 (equal desc full-raw-path)))
						(setq imgp t)
					  (progn (when (string-match "\\(.+\\)::.+" raw-path)
							   (setq raw-path (match-string 1 raw-path)))
							 (if (file-exists-p raw-path)
								 (concat type "://" (expand-file-name raw-path))
							   (concat type "://" (org-export-directory
												   :Groff org-export-groff-options-plist)
									   raw-path))))))))
       ;; process with link inserting
       (apply 'delete-region remove)
       (setq caption (and caption (org-export-groff-fontify-headline caption)))
       (cond ((and imgp
				   (plist-get org-export-groff-options-plist :inline-images))
			  ;; OK, we need to inline an image
			  (insert
			   (org-export-groff-format-image raw-path caption label attr shortn)))
			 (coderefp
			  (insert (format
					   (org-export-get-coderef-format path desc)
					   (cdr (assoc path org-export-code-refs)))))
			 (radiop (insert (format org-export-groff-hyperref-format
									 (org-solidify-link-text raw-path) desc)))
			 ((not type)
			  (insert (format org-export-groff-hyperref-format
							  (org-remove-initial-hash
							   (org-solidify-link-text raw-path))
							  desc)))
			 (path
			  (when (org-at-table-p)
				;; There is a strange problem when we have a link in a table,
				;; ampersands then cause a problem.  I think this must be
				;; a Groff issue, but we here implement a work-around anyway.
				(setq path (org-export-groff-protect-amp path)
					  desc (org-export-groff-protect-amp desc)))
			  (insert
			   (if (string-match "%s.*%s" org-export-groff-href-format)
				   (format org-export-groff-href-format path desc)
				 (format org-export-groff-href-format path))))

			 ((functionp (setq fnc (nth 2 (assoc type org-link-protocols))))
			  ;; The link protocol has a function for formatting the link
			  (insert
			   (save-match-data
				 (funcall fnc (org-link-unescape raw-path) desc 'groff))))

			 (t (insert "\\texttt{" desc "}")))))))


(defun org-export-groff-format-image (path caption label attr &optional shortn)
  "Format the image element, depending on user settings."
  (let (ind floatp wrapp multicolumnp placement figenv)
    (setq floatp (or caption label))
    (setq ind (org-get-text-property-any 0 'original-indentation path))
    (when (and attr (stringp attr))
      (if (string-match "[ \t]*\\<wrap\\>" attr)
		  (setq wrapp t floatp nil attr (replace-match "" t t attr)))
      (if (string-match "[ \t]*\\<float\\>" attr)
		  (setq wrapp nil floatp t attr (replace-match "" t t attr)))
      (if (string-match "[ \t]*\\<multicolumn\\>" attr)
		  (setq multicolumnp t attr (replace-match "" t t attr))))

    (setq placement
		  (cond
		   (wrapp "{l}{0.5\\textwidth}")
		   (floatp (concat "[" org-groff-default-figure-position "]"))
		   (t "")))

    (when (and attr (stringp attr)
			   (string-match "[ \t]*\\<placement=\\(\\S-+\\)" attr))
      (setq placement (match-string 1 attr)
			attr (replace-match "" t t attr)))
    (setq attr (and attr (org-trim attr)))
    (when (or (not attr) (= (length attr) 0))
      (setq attr (cond (floatp "width=0.7\\textwidth")
					   (wrapp "width=0.48\\textwidth")
					   (t attr))))
    (setq figenv
		  (cond
		   (wrapp "\\begin{wrapfigure}%placement
\\centering
\\includegraphics[%attr]{%path}
\\caption%shortn{%labelcmd%caption}
\\end{wrapfigure}")
		   (multicolumnp "\\begin{figure*}%placement
\\centering
\\includegraphics[%attr]{%path}
\\caption%shortn{%labelcmd%caption}
\\end{figure*}")
		   (floatp "\\begin{figure}%placement
\\centering
\\includegraphics[%attr]{%path}
\\caption%shortn{%labelcmd%caption}
\\end{figure}")
		   (t "\\includegraphics[%attr]{%path}")))


    (setq figenv (mapconcat 'identity (split-string figenv "\n")
							(save-excursion (beginning-of-line 1)
											(looking-at "[ \t]*")
											(concat "\n" (match-string 0)))))

    (if (and (not label) (not caption)
			 (string-match "^\\\\caption{.*\n" figenv))
		(setq figenv (replace-match "" t t figenv)))
    (org-add-props
		(org-fill-template
		 figenv
		 (list (cons "path"
					 (if (file-name-absolute-p path)
						 (expand-file-name path)
					   path))
			   (cons "attr" attr)
			   (cons "shortn" (if shortn (format "[%s]" shortn) ""))
			   (cons "labelcmd" (if label (format "\\label{%s}"
												  label)""))
			   (cons "caption" (or caption ""))
			   (cons "placement" (or placement ""))))
		nil 'original-indentation ind)))

(defun org-export-groff-protect-amp (s)
  (while (string-match "\\([^\\\\]\\)\\(&\\)" s)
    (setq s (replace-match (concat (match-string 1 s) "\\" (match-string 2 s))
						   t t s)))
  s)

(defun org-remove-initial-hash (s)
  (if (string-match "\\`#" s)
      (substring s 1)
    s))
(defvar org-groff-entities)				; defined below
(defvar org-groff-entities-regexp)		; defined below

(defun org-export-groff-preprocess (parameters)
  "Clean stuff in the Groff export."
  ;; Replace footnotes.
  (when (plist-get parameters :footnotes)
    (goto-char (point-min))
    (let (ref)
      (while (setq ref (org-footnote-get-next-reference))
		(let* ((beg (nth 1 ref))
			   (lbl (car ref))
			   (def (nth 1 (assoc (string-to-number lbl)
								  (mapcar (lambda (e) (cdr e))
										  org-export-footnotes-seen)))))
		  ;; Fix body for footnotes ending on a link or a list and
		  ;; remove definition from buffer.
		  (setq def
				(concat def
						(if (string-match "ORG-LIST-END-MARKER\\'" def)
							"\n" " ")))
		  (org-footnote-delete-definitions lbl)
		  ;; Compute string to insert (FNOTE), and protect the outside
		  ;; macro from further transformation.  When footnote at
		  ;; point is referring to a previously defined footnote, use
		  ;; \footnotemark. Otherwise, use \footnote.
		  (let ((fnote (if (member lbl org-export-groff-footmark-seen)
						   (org-export-groff-protect-string
							(format "\\footnotemark[%s]" lbl))
						 (push lbl org-export-groff-footmark-seen)
						 (concat (org-export-groff-protect-string "\\footnote{")
								 def
								 (org-export-groff-protect-string "}"))))
				;; Check if another footnote is immediately following.
				;; If so, add a separator in-between.
				(sep (org-export-groff-protect-string
					  (if (save-excursion (goto-char (1- (nth 2 ref)))
										  (let ((next (org-footnote-get-next-reference)))
											(and next (= (nth 1 next) (nth 2 ref)))))
						  org-export-groff-footnote-separator ""))))
			(when (org-at-heading-p)
			  (setq fnote (concat (org-export-groff-protect-string "\\protect")
								  fnote)))
			;; Ensure a footnote at column 0 cannot end a list
			;; containing it.
			(put-text-property 0 (length fnote) 'original-indentation 1000 fnote)
			;; Replace footnote reference with FNOTE and, maybe, SEP.
			;; `save-excursion' is required if there are two footnotes
			;; in a row.  In that case, point would be left at the
			;; beginning of the second one, and
			;; `org-footnote-get-next-reference' would then skip it.
			(goto-char beg)
			(delete-region beg (nth 2 ref))
			(save-excursion (insert fnote sep)))))))

  ;; Remove footnote section tag for Groff
  (goto-char (point-min))
  (while (re-search-forward
		  (concat "^" footnote-section-tag-regexp) nil t)
    (org-if-unprotected
     (replace-match "")))
  ;; Remove any left-over footnote definition.
  (mapc (lambda (fn) (org-footnote-delete-definitions (car fn)))
		org-export-footnotes-data)
  (mapc (lambda (fn) (org-footnote-delete-definitions fn))
		org-export-groff-footmark-seen)

  ;; Preserve line breaks
  (goto-char (point-min))
  (while (re-search-forward "\\\\\\\\" nil t)
    (add-text-properties (match-beginning 0) (match-end 0)
						 '(org-protected t)))

  ;; Preserve groff environments
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]*\\\\begin{\\([a-zA-Z]+\\*?\\)}" nil t)
    (org-if-unprotected
     (let* ((start (progn (beginning-of-line) (point)))
			(end (and (re-search-forward
					   (concat "^[ \t]*\\\\end{"
							   (regexp-quote (match-string 1))
							   "}") nil t)
					  (point-at-eol))))
       (if end
		   (add-text-properties start end '(org-protected t))
		 (goto-char (point-at-eol))))))

  ;; Preserve math snippets
  (let* ((matchers '())
		 (re-list nil)
		 beg end re e m n block off)
    ;; Check the different regular expressions
    (while (setq e (pop re-list))
      (setq m (car e) re (nth 1 e) n (nth 2 e)
			block (if (nth 3 e) "\n\n" ""))
      (setq off (if (member m '("$" "$1")) 1 0))
      (when (and (member m matchers) (not (equal m "begin")))
		(goto-char (point-min))
		(while (re-search-forward re nil t)
		  (setq beg (+ (match-beginning 0) off) end (- (match-end 0) 0))
		  (add-text-properties beg end '(org-protected t org-groff-math t))))))

  ;; Convert Groff to \Groff{} and TeX to \TeX{}
  (goto-char (point-min))
  (let ((case-fold-search nil))
    (while (re-search-forward "\\<\\(\\(La\\)?TeX\\)\\>" nil t)
      (unless (eq (char-before (match-beginning 1)) ?\\)
		(org-if-unprotected-1
		 (replace-match (org-export-groff-protect-string
						 (concat "\\" (match-string 1)
								 "{}")) t t)))))

  ;; Convert blockquotes
  (goto-char (point-min))
  (while (search-forward "ORG-BLOCKQUOTE-START" nil t)
    (org-replace-match-keep-properties "\\begin{quote}" t t))
  (goto-char (point-min))
  (while (search-forward "ORG-BLOCKQUOTE-END" nil t)
    (org-replace-match-keep-properties "\\end{quote}" t t))

  ;; Convert verse
  (goto-char (point-min))
  (while (search-forward "ORG-VERSE-START" nil t)
    (org-replace-match-keep-properties "\\begin{verse}" t t)
    (beginning-of-line 2)
    (while (and (not (looking-at "[ \t]*ORG-VERSE-END.*")) (not (eobp)))
      (when (looking-at "\\([ \t]+\\)\\([^ \t\n]\\)")
		(goto-char (match-end 1))
		(org-replace-match-keep-properties
		 (org-export-groff-protect-string
		  (concat "\\hspace*{1cm}" (match-string 2))) t t)
		(beginning-of-line 1))
      (if (looking-at "[ \t]*$")
		  (insert (org-export-groff-protect-string "\\vspace*{1em}"))
		(unless (looking-at ".*?[^ \t\n].*?\\\\\\\\[ \t]*$")
		  (end-of-line 1)
		  (insert "\\\\")))
      (beginning-of-line 2))
    (and (looking-at "[ \t]*ORG-VERSE-END.*")
		 (org-replace-match-keep-properties "\\end{verse}" t t)))

  ;; Convert #+INDEX to Groff \\index.
  (goto-char (point-min))
  (let ((case-fold-search t) entry)
    (while (re-search-forward
			"^[ \t]*#\\+index:[ \t]*\\([^ \t\r\n].*?\\)[ \t]*$"
			nil t)
      (setq entry
			(save-match-data
			  (org-export-groff-protect-string
			   (org-export-groff-fontify-headline (match-string 1)))))
      (replace-match (format "\\index{%s}" entry) t t)))

  ;; Convert center
  (goto-char (point-min))
  (while (search-forward "ORG-CENTER-START" nil t)
    (org-replace-match-keep-properties "\\begin{center}" t t))
  (goto-char (point-min))
  (while (search-forward "ORG-CENTER-END" nil t)
    (org-replace-match-keep-properties "\\end{center}" t t))

  (run-hooks 'org-export-groff-after-blockquotes-hook)

  ;; Convert horizontal rules
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]*-\\{5,\\}[ \t]*$" nil t)
    (org-if-unprotected
     (replace-match (org-export-groff-protect-string "\\hrule") t t)))

  ;; Protect Groff commands like \command[...]{...} or \command{...}
  (goto-char (point-min))
  (let ((re (concat
			 "\\\\\\([a-zA-Z]+\\*?\\)"
			 "\\(?:<[^<>\n]*>\\)*"
			 "\\(?:\\[[^][\n]*?\\]\\)*"
			 "\\(?:<[^<>\n]*>\\)*"
			 "\\(" (org-create-multibrace-regexp "{" "}" 3) "\\)\\{1,3\\}")))
    (while (re-search-forward re nil t)
      (unless (or
			   ;; Check for comment line.
			   (save-excursion (goto-char (match-beginning 0))
							   (org-in-indented-comment-line))
			   ;; Check if this is a defined entity, so that is may
			   ;; need conversion.
			   (org-entity-get (match-string 1))
			   ;; Do not protect interior of footnotes.  Those have
			   ;; already been taken care of earlier in the function.
			   ;; Yet, keep looking inside them for more commands.
			   (and (equal (match-string 1) "footnote")
					(goto-char (match-end 1))))
		(add-text-properties (match-beginning 0) (match-end 0)
							 '(org-protected t)))))

  ;; Special case for \nbsp
  (goto-char (point-min))
  (while (re-search-forward "\\\\nbsp\\({}\\|\\>\\)" nil t)
    (org-if-unprotected
     (replace-match (org-export-groff-protect-string "~"))))

  ;; Protect Groff entities
  (goto-char (point-min))
  (while (re-search-forward org-groff-entities-regexp nil t)
    (org-if-unprotected
     (add-text-properties (match-beginning 0) (match-end 0)
						  '(org-protected t))))

  ;; Replace radio links
  (goto-char (point-min))
  (while (re-search-forward
		  (concat "<<<?" org-export-groff-all-targets-re
				  ">>>?\\((INVISIBLE)\\)?") nil t)
    (org-if-unprotected-at (+ (match-beginning 0) 2)
      (replace-match
       (concat
		(org-export-groff-protect-string
		 (format "\\label{%s}" (save-match-data (org-solidify-link-text
												 (match-string 1)))))
		(if (match-string 2) "" (match-string 1)))
       t t)))

  ;; Delete @<...> constructs
  ;; Thanks to Daniel Clemente for this regexp
  (goto-char (point-min))
  (while (re-search-forward "@<\\(?:[^\"\n]\\|\".*\"\\)*?>" nil t)
    (org-if-unprotected
     (replace-match ""))))

(defun org-export-groff-fix-inputenc ()
  "Set the coding system in inputenc to what the buffer is."
  (let* ((cs buffer-file-coding-system)
		 (opt (or (ignore-errors (groffenc-coding-system-to-inputenc cs))
				  "utf8")))
    (when opt
      ;; Translate if that is requested
      (setq opt (or (cdr (assoc opt org-export-groff-inputenc-alist)) opt))
      ;; find the \usepackage statement and replace the option
      (goto-char (point-min))
      (while (re-search-forward "\\\\usepackage\\[\\(AUTO\\)\\]{inputenc}"
								nil t)
		(goto-char (match-beginning 1))
		(delete-region (match-beginning 1) (match-end 1))
		(insert opt))
      (and buffer-file-name
		   (save-buffer)))))

;;; List handling:

(defun org-export-groff-lists ()
  "Convert plain text lists in current buffer into Groff lists."
  ;; `org-list-end-re' output has changed since preprocess from
  ;; org-exp.el. Make sure it is taken into account.
  (let ((org-list-end-re "^ORG-LIST-END-MARKER\n"))
    (mapc
     (lambda (e)
       ;; For each type of context allowed for list export (E), find
       ;; every list, parse it, delete it and insert resulting
       ;; conversion to groff (RES), while keeping the same
       ;; `original-indentation' property.
       (let (res)
		 (goto-char (point-min))
		 (while (re-search-forward (org-item-beginning-re) nil t)
		   (when (and (eq (get-text-property (point) 'list-context) e)
					  (not (get-text-property (point) 'org-example)))
			 (beginning-of-line)
			 (setq res
				   (org-list-to-groff
					;; Narrowing is needed because we're converting
					;; from inner functions to outer ones.
					(save-restriction
					  (narrow-to-region (point) (point-max))
					  (org-list-parse-list t))
					org-export-groff-list-parameters))
			 ;; Extend previous value of original-indentation to the
			 ;; whole string
			 (insert (org-add-props res nil 'original-indentation
									(org-find-text-property-in-string
									 'original-indentation res)))))))
     ;; List of allowed contexts for export, and the default one.
     (append org-list-export-context '(nil)))))

(defconst org-groff-entities
  '("\\!"
	"\\'"
	"\\+"
	"\\,"
	"\\-"
	"\\:"
	"\\;"
	"\\<"
	"\\="
	"\\>"
	"\\Huge"
	"\\LARGE"
	"\\Large"
	"\\Styles"
	"\\\\"
	"\\`"
	"\\\""
	"\\addcontentsline"
	"\\address"
	"\\addtocontents"
	"\\addtocounter"
	"\\addtolength"
	"\\addvspace"
	"\\alph"
	"\\appendix"
	"\\arabic"
	"\\author"
	"\\begin{array}"
	"\\begin{center}"
	"\\begin{description}"
	"\\begin{enumerate}"
	"\\begin{eqnarray}"
	"\\begin{equation}"
	"\\begin{figure}"
	"\\begin{flushleft}"
	"\\begin{flushright}"
	"\\begin{itemize}"
	"\\begin{list}"
	"\\begin{minipage}"
	"\\begin{picture}"
	"\\begin{quotation}"
	"\\begin{quote}"
	"\\begin{tabbing}"
	"\\begin{table}"
	"\\begin{tabular}"
	"\\begin{thebibliography}"
	"\\begin{theorem}"
	"\\begin{titlepage}"
	"\\begin{verbatim}"
	"\\begin{verse}"
	"\\bf"
	"\\bf"
	"\\bibitem"
	"\\bigskip"
	"\\cdots"
	"\\centering"
	"\\circle"
	"\\cite"
	"\\cleardoublepage"
	"\\clearpage"
	"\\cline"
	"\\closing"
	"\\dashbox"
	"\\date"
	"\\ddots"
	"\\dotfill"
	"\\em"
	"\\fbox"
	"\\flushbottom"
	"\\fnsymbol"
	"\\footnote"
	"\\footnotemark"
	"\\footnotesize"
	"\\footnotetext"
	"\\frac"
	"\\frame"
	"\\framebox"
	"\\hfill"
	"\\hline"
	"\\hrulespace"
	"\\hspace"
	"\\huge"
	"\\hyphenation"
	"\\include"
	"\\includeonly"
	"\\indent"
	"\\input"
	"\\it"
	"\\kill"
	"\\label"
	"\\large"
	"\\ldots"
	"\\line"
	"\\linebreak"
	"\\linethickness"
	"\\listoffigures"
	"\\listoftables"
	"\\location"
	"\\makebox"
	"\\maketitle"
	"\\mark"
	"\\mbox"
	"\\medskip"
	"\\multicolumn"
	"\\multiput"
	"\\newcommand"
	"\\newcounter"
	"\\newenvironment"
	"\\newfont"
	"\\newlength"
	"\\newline"
	"\\newpage"
	"\\newsavebox"
	"\\newtheorem"
	"\\nocite"
	"\\nofiles"
	"\\noindent"
	"\\nolinebreak"
	"\\nopagebreak"
	"\\normalsize"
	"\\onecolumn"
	"\\opening"
	"\\oval"
	"\\overbrace"
	"\\overline"
	"\\pagebreak"
	"\\pagenumbering"
	"\\pageref"
	"\\pagestyle"
	"\\par"
	"\\parbox"
	"\\put"
	"\\raggedbottom"
	"\\raggedleft"
	"\\raggedright"
	"\\raisebox"
	"\\ref"
	"\\rm"
	"\\roman"
	"\\rule"
	"\\savebox"
	"\\sc"
	"\\scriptsize"
	"\\setcounter"
	"\\setlength"
	"\\settowidth"
	"\\sf"
	"\\shortstack"
	"\\signature"
	"\\sl"
	"\\small"
	"\\smallskip"
	"\\sqrt"
	"\\tableofcontents"
	"\\telephone"
	"\\thanks"
	"\\thispagestyle"
	"\\tiny"
	"\\title"
	"\\tt"
	"\\twocolumn"
	"\\typein"
	"\\typeout"
	"\\underbrace"
	"\\underline"
	"\\usebox"
	"\\usecounter"
	"\\value"
	"\\vdots"
	"\\vector"
	"\\verb"
	"\\vfill"
	"\\vline"
	"\\vspace")
  "A list of Groff commands to be protected when performing conversion.")

(defconst org-groff-entities-regexp
  (let (names rest)
    (dolist (x org-groff-entities)
      (if (string-match "[a-zA-Z]$" x)
		  (push x names)
		(push x rest)))
    (concat "\\(" (regexp-opt (nreverse names)) "\\>\\)"
			"\\|\\(" (regexp-opt (nreverse rest)) "\\)")))

(provide 'org-export-groff)
(provide 'org-groff)

;;; org-groff.el ends here
