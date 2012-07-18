;; org-e-man.el --- Man Back-End For Org Export Engine

;; Copyright (C) 2011-2012  Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <n.goaziou at gmail dot com>
;; Author: Luis R Anaya <papoanaya aroba hot mail punto com>
;; Keywords: outlines, hypermedia, calendar, wp
;; 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This library implements a Man back-end for Org generic exporter.
;;
;; To test it, run
;;
;;   M-: (org-export-to-buffer 'e-man "*Test e-Man*") RET
;;
;; in an org-mode buffer then switch to the buffer to see the Man
;; export.  See contrib/lisp/org-export.el for more details on how
;; this exporter works.
;;
;; It introduces three new buffer keywords: "MAN_CLASS",
;; "MAN_CLASS_OPTIONS" and "MAN_HEADER".

;;; Code:

(eval-when-compile (require 'cl))

(defvar org-export-man-default-packages-alist)
(defvar org-export-man-packages-alist)


(declare-function org-element-property "org-element" (property element))
(declare-function org-element-normalize-string "org-element" (s))

(declare-function org-export-data "org-export" (data info))
(declare-function org-export-directory "org-export" (type plist))
(declare-function org-export-expand-macro "org-export" (macro info))
(declare-function org-export-first-sibling-p "org-export" (headline))
(declare-function org-export-footnote-first-reference-p "org-export"
		  (footnote-reference info))
(declare-function org-export-format-code "org-export"
		  (code fun &optional num-lines ref-alist))
(declare-function org-export-format-code-default "org-export" (element info))
(declare-function org-export-get-coderef-format "org-export" (path desc))
(declare-function org-export-get-footnote-definition "org-export"
		  (footnote-reference info))
(declare-function org-export-get-footnote-number "org-export" (footnote info))
(declare-function org-export-get-previous-element "org-export" (blob))
(declare-function org-export-get-relative-level "org-export" (headline info))
(declare-function org-export-unravel-code "org-export" (element))
(declare-function org-export-inline-image-p "org-export"
		  (link &optional extensions))
(declare-function org-export-last-sibling-p "org-export" (headline))
(declare-function org-export-low-level-p "org-export" (headline info))
(declare-function org-export-output-file-name
		  "org-export" (extension &optional subtreep pub-dir))
(declare-function org-export-resolve-coderef "org-export" (ref info))
(declare-function org-export-resolve-fuzzy-link "org-export" (link info))
(declare-function org-export-resolve-radio-link "org-export" (link info))
(declare-function org-export-solidify-link-text "org-export" (s))
(declare-function
 org-export-to-buffer "org-export"
 (backend buffer &optional subtreep visible-only body-only ext-plist))
(declare-function
 org-export-to-file "org-export"
 (backend file &optional subtreep visible-only body-only ext-plist))



;;; Define Back-End

(defvar org-e-man-translate-alist
  '((babel-call . org-e-man-babel-call)
    (bold . org-e-man-bold)
    (center-block . org-e-man-center-block)
    (clock . org-e-man-clock)
    (code . org-e-man-code)
    (comment . org-e-man-comment)
    (comment-block . org-e-man-comment-block)
    (drawer . org-e-man-drawer)
    (dynamic-block . org-e-man-dynamic-block)
    (entity . org-e-man-entity)
    (example-block . org-e-man-example-block)
    (export-block . org-e-man-export-block)
    (export-snippet . org-e-man-export-snippet)
    (fixed-width . org-e-man-fixed-width)
    (footnote-definition . org-e-man-footnote-definition)
    (footnote-reference . org-e-man-footnote-reference)
    (headline . org-e-man-headline)
    (horizontal-rule . org-e-man-horizontal-rule)
    (inline-babel-call . org-e-man-inline-babel-call)
    (inline-src-block . org-e-man-inline-src-block)
    (inlinetask . org-e-man-inlinetask)
    (italic . org-e-man-italic)
    (item . org-e-man-item)
    (keyword . org-e-man-keyword)
    (man-environment . org-e-man-man-environment)
    (man-fragment . org-e-man-man-fragment)
    (line-break . org-e-man-line-break)
    (link . org-e-man-link)
    (macro . org-e-man-macro)
    (paragraph . org-e-man-paragraph)
    (plain-list . org-e-man-plain-list)
    (plain-text . org-e-man-plain-text)
    (planning . org-e-man-planning)
    (property-drawer . org-e-man-property-drawer)
    (quote-block . org-e-man-quote-block)
    (quote-section . org-e-man-quote-section)
    (radio-target . org-e-man-radio-target)
    (section . org-e-man-section)
    (special-block . org-e-man-special-block)
    (src-block . org-e-man-src-block)
    (statistics-cookie . org-e-man-statistics-cookie)
    (strike-through . org-e-man-strike-through)
    (subscript . org-e-man-subscript)
    (superscript . org-e-man-superscript)
    (table . org-e-man-table)
    (table-cell . org-e-man-table-cell)
    (table-row . org-e-man-table-row)
    (target . org-e-man-target)
    (template . org-e-man-template)
    (timestamp . org-e-man-timestamp)
    (underline . org-e-man-underline)
    (verbatim . org-e-man-verbatim)
    (verse-block . org-e-man-verse-block))
  "Alist between element or object types and translators.")

(defconst org-e-man-options-alist
  '((:date "DATE" nil org-e-man-date-format t)
    (:man-class "MAN_CLASS" nil org-e-man-default-class t)
    (:man-class-options "MAN_CLASS_OPTIONS" nil nil t)
    (:man-header-extra "MAN_HEADER" nil nil newline))
  "Alist between Man export properties and ways to set them.
See `org-export-options-alist' for more information on the
structure of the values.")



;;; Internal Variables

(defconst org-e-man-babel-language-alist
  '(("af" . "afrikaans")
    ("bg" . "bulgarian")
    ("bt-br" . "brazilian")
    ("ca" . "catalan")
    ("cs" . "czech")
    ("cy" . "welsh")
    ("da" . "danish")
    ("de" . "germanb")
    ("de-at" . "naustrian")
    ("de-de" . "ngerman")
    ("el" . "greek")
    ("en" . "english")
    ("en-au" . "australian")
    ("en-ca" . "canadian")
    ("en-gb" . "british")
    ("en-ie" . "irish")
    ("en-nz" . "newzealand")
    ("en-us" . "american")
    ("es" . "spanish")
    ("et" . "estonian")
    ("eu" . "basque")
    ("fi" . "finnish")
    ("fr" . "frenchb")
    ("fr-ca" . "canadien")
    ("gl" . "galician")
    ("hr" . "croatian")
    ("hu" . "hungarian")
    ("id" . "indonesian")
    ("is" . "icelandic")
    ("it" . "italian")
    ("la" . "latin")
    ("ms" . "malay")
    ("nl" . "dutch")
    ("no-no" . "nynorsk")
    ("pl" . "polish")
    ("pt" . "portuguese")
    ("ro" . "romanian")
    ("ru" . "russian")
    ("sa" . "sanskrit")
    ("sb" . "uppersorbian")
    ("sk" . "slovak")
    ("sl" . "slovene")
    ("sq" . "albanian")
    ("sr" . "serbian")
    ("sv" . "swedish")
    ("ta" . "tamil")
    ("tr" . "turkish")
    ("uk" . "ukrainian"))
  "Alist between language code and corresponding Babel option.")



;;; User Configurable Variables

(defgroup org-export-e-man nil
  "Options for exporting Org mode files to Man."
  :tag "Org Export Man"
  :group 'org-export)


;;;; Preamble

(defcustom org-e-man-default-class "1"
  "The default Man class."
  :group 'org-export-e-man
  :type '(string :tag "Man class"))

(defcustom org-e-man-classes
  '(("1"
     "1"
     (".SH \"%s\"" . ".SH \"%s\" ")
     (".SS \"%s\"" . ".SS \"%s\" "))
    )

  "Alist of Man classes and associated header and structure.
If #+Man_CLASS is set in the buffer, use its value and the
associated information.  Here is the structure of each cell:

  \(class-name
    header-string
    \(numbered-section . unnumbered-section\)
    ...\)

The header string
-----------------

The HEADER-STRING is the header that will be inserted into the
Man file.  It should contain the MT (Memorandum Type) macro, and
anything else that is needed for this setup.  

- Lines specified via \"#+MAN_HEADER:\"

If you need more control about the sequence in which the header
is built up, or if you want to exclude one of these building
blocks for a particular class, you can use the following
macro-like placeholders.

 [EXTRA]                 the stuff from #+MAN_HEADER
 [NO-EXTRA]              do not include #+MAN_HEADER stuff

So a header like

  [EXTRA]
  \\fB#1\\fP


will omit the default packages, and will include the
#+Man_HEADER lines. 


The sectioning structure
------------------------

The sectioning structure of the class is given by the elements
following the header string.  For each sectioning level, a number
of strings is specified.  A %s formatter is mandatory in each
section string and will be replaced by the title of the section.

Instead of a cons cell \(numbered . unnumbered\), you can also
provide a list of 2 or 4 elements,

  \(numbered-open numbered-close\)

or

  \(numbered-open numbered-close unnumbered-open unnumbered-close\)

providing opening and closing strings for a Man environment
that should represent the document section.  The opening clause
should have a %s to represent the section title.

Instead of a list of sectioning commands, you can also specify
a function name.  That function will be called with two
parameters, the \(reduced) level of the headline, and a predicate
non-nil when the headline should be numbered.  It must return
a format string in which the section title will be added."
  :group 'org-export-e-man
  :type '(repeat
	  (list (string :tag "Man class")
		(string :tag "Man header")
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

(defcustom org-e-man-inputenc-alist nil
  "Alist of inputenc coding system names, and what should really be used.
For example, adding an entry

      (\"utf8\" . \"utf8x\")

will cause \\usepackage[utf8x]{inputenc} to be used for buffers that
are written as utf8 files."
  :group 'org-export-e-man
  :type '(repeat
	  (cons
	   (string :tag "Derived from buffer")
	   (string :tag "Use this instead"))))

(defcustom org-e-man-date-format
  (format-time-string "%Y-%m-%d")
  "Format string for .ND "
  :group 'org-export-e-man
  :type 'boolean)


;;;; Headline


(defcustom org-e-man-format-headline-function nil
  "Function to format headline text.

This function will be called with 5 arguments:
TODO      the todo keyword (string or nil).
TODO-TYPE the type of todo (symbol: `todo', `done', nil)
PRIORITY  the priority of the headline (integer or nil)
TEXT      the main headline text (string).
TAGS      the tags as a list of strings (list of strings or nil).

The function result will be used in the section format string.

As an example, one could set the variable to the following, in
order to reproduce the default set-up:

\(defun org-e-man-format-headline (todo todo-type priority text tags)
  \"Default format function for an headline.\"
  \(concat (when todo
            \(format \"\\fB%s\\fP \" todo))
	  \(when priority
            \(format \"[\\#%c] \" priority))
	  text
	  \(when tags
            \(format \" %s \"
              \(mapconcat 'identity tags \":\"))))"
  :group 'org-export-e-man
  :type 'function)


;;;; Footnotes

;;;; Timestamps

(defcustom org-e-man-active-timestamp-format "\\fI%s\\fP"
  "A printf format string to be applied to active timestamps."
  :group 'org-export-e-man
  :type 'string)

(defcustom org-e-man-inactive-timestamp-format "\\fI%s\\fP"
  "A printf format string to be applied to inactive timestamps."
  :group 'org-export-e-man
  :type 'string)

(defcustom org-e-man-diary-timestamp-format "\\fI%s\\fP"
  "A printf format string to be applied to diary timestamps."
  :group 'org-export-e-man
  :type 'string)


;;;; Links

(defcustom org-e-man-image-default-option nil
  "Default option for images."
  :group 'org-export-e-man
  :type 'string)

(defcustom org-e-man-default-figure-position nil
  "Default position for man figures."
  :group 'org-export-e-man
  :type 'string)

(defcustom org-e-man-inline-image-rules nil
  "Rules characterizing image files that can be inlined into Man.

A rule consists in an association whose key is the type of link
to consider, and value is a regexp that will be matched against
link's path.

Note that, by default, the image extension *actually* allowed
depend on the way the Man file is processed.  When used with
pdfman, pdf, jpg and png images are OK.  When processing
through dvi to Postscript, only ps and eps are allowed.  The
default we use here encompasses both."
  :group 'org-export-e-man
  :type '(alist :key-type (string :tag "Type")
		:value-type (regexp :tag "Path")))

(defcustom org-e-man-link-with-unknown-path-format "\\fI%s\\fP"
  "Format string for links with unknown path type."
  :group 'org-export-man
  :type 'string)


;;;; Tables


(defcustom org-e-man-tables-centered t
  "When non-nil, tables are exported in a center environment."
  :group 'org-export-e-man
  :type 'boolean)

(defcustom org-e-man-tables-verbatim nil
  "When non-nil, tables are exported verbatim."
  :group 'org-export-e-man
  :type 'boolean)


(defcustom org-e-man-table-caption-above nil
  "When non-nil, place caption string at the beginning of the table.
Otherwise, place it near the end."
  :group 'org-export-e-man
  :type 'boolean)

(defcustom org-e-man-table-scientific-notation "%sE%s"
  "Format string to display numbers in scientific notation.
The format should have \"%s\" twice, for mantissa and exponent
\(i.e. \"%s\\\\times10^{%s}\").

When nil, no transformation is made."
  :group 'org-export-e-man
  :type '(choice
	  (string :tag "Format string")
	  (const :tag "No formatting")))


;;;; Text markup

(defcustom org-e-man-text-markup-alist '((bold . "\\fB%s\\fP")
					   ;; from "verb"
					   (code . "\\fC%s\\fP") 
					   (italic . "\\fI%s\\fP")

					   ;;
					   ;; Strike through
					   ;; and underline need to be revised.

					   (strike-through . "\\fC%s\\fP")
					   (underline . "\\fI%s\\fP")
					   (verbatim .   "protectedtexttt"))
  "Alist of Man expressions to convert text markup.

The key must be a symbol among `bold', `code', `italic',
`strike-through', `underline' and `verbatim'.  The value is
a formatting string to wrap fontified text with it. 

If no association can be found for a given markup, text will be
returned as-is."
  :group 'org-export-e-man
  :type 'alist
  :options '(bold code italic strike-through underline verbatim))


;;;; Drawers

(defcustom org-e-man-format-drawer-function nil
  "Function called to format a drawer in Man code.

The function must accept two parameters:
  NAME      the drawer name, like \"LOGBOOK\"
  CONTENTS  the contents of the drawer.

The function should return the string to be exported.

For example, the variable could be set to the following function
in order to mimic default behaviour:

\(defun org-e-man-format-drawer-default \(name contents\)
  \"Format a drawer element for Man export.\"
  contents\)"
  :group 'org-export-e-man
  :type 'function)


;;;; Inlinetasks
;; Src blocks

(defcustom org-e-man-source-highlight nil
  "Use GNU source highlight to embellish source blocks " 
  :group 'org-export-e-man
  :type 'boolean)


(defcustom org-e-man-source-highlight-langs
  '((emacs-lisp "lisp") (lisp "lisp") (clojure "lisp")
    (c "c") (cc "cpp")
    (fortran "fortran") (cobol "cobol") (pascal "pascal")
    (perl "perl") (cperl "perl") (python "python") (ruby "ruby") (tcl "tcl")
    (html "html") (xml "xml")
    (tex "latex") 
    (shell-script "sh") (awk "awk")
    (ocaml "caml") (caml "caml")
    (sql "sql") (sqlite "sql")
    )
  "Alist mapping languages to their listing language counterpart.
The key is a symbol, the major mode symbol without the \"-mode\".
The value is the string that should be inserted as the language
parameter for the listings package.  If the mode name and the
listings name are the same, the language does not need an entry
in this list - but it does not hurt if it is present."
  :group 'org-export-e-man
  :type '(repeat
	  (list
	   (symbol :tag "Major mode       ")
	   (string :tag "Listings language"))))

(defcustom org-e-man-source-highlight-options nil
  "Association list of options for the man listings package.

These options are supplied as a comma-separated list to the
\\lstset command.  Each element of the association list should be
a list containing two strings: the name of the option, and the
value.  For example,

  (setq org-e-man-source-highlight-options
    '((\"basicstyle\" \"\\small\")
      (\"keywordstyle\" \"\\color{black}\\bfseries\\underbar\")))

will typeset the code in a small size font with underlined, bold
black keywords.

Note that the same options will be applied to blocks of all
languages."
  :group 'org-export-e-man
  :type '(repeat
	  (list
	   (string :tag "Listings option name ")
	   (string :tag "Listings option value"))))



(defvar org-e-man-custom-lang-environments nil
  "Alist mapping languages to language-specific Man environments.

It is used during export of src blocks by the listings and 
man packages.  For example,

  \(setq org-e-man-custom-lang-environments
     '\(\(python \"pythoncode\"\)\)\)

would have the effect that if org encounters begin_src python
during man export it will output"
)




;;;; Plain text

(defcustom org-e-man-quotes
  '(("fr"
     ("\\(\\s-\\|[[(]\\|^\\)\"" . "«~")
     ("\\(\\S-\\)\"" . "~»")
     ("\\(\\s-\\|(\\|^\\)'" . "'"))
    ("en"
     ("\\(\\s-\\|[[(]\\|^\\)\"" . "``")
     ("\\(\\S-\\)\"" . "''")
     ("\\(\\s-\\|(\\|^\\)'" . "`")))

  "Alist for quotes to use when converting english double-quotes.

The CAR of each item in this alist is the language code.
The CDR of each item in this alist is a list of three CONS:
- the first CONS defines the opening quote;
- the second CONS defines the closing quote;
- the last CONS defines single quotes.

For each item in a CONS, the first string is a regexp
for allowed characters before/after the quote, the second
string defines the replacement string for this quote."
  :group 'org-export-e-man
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


;;;; Compilation

(defcustom org-e-man-pdf-process
  '("tbl %f | eqn | groff -man | ps2pdf - > %b.pdf"
    "tbl %f | eqn | groff -man | ps2pdf - > %b.pdf"
    "tbl %f | eqn | groff -man | ps2pdf - > %b.pdf"
    )

  "Commands to process a Man file to a PDF file.
This is a list of strings, each of them will be given to the
shell as a command.  %f in the command will be replaced by the
full file name, %b by the file base name \(i.e. without
extension) and %o by the base directory of the file.

The reason why this is a list is that it usually takes several
runs of `pdfgroff', maybe mixed with a call to `bibtex'.  Org
does not have a clever mechanism to detect which of these
commands have to be run to get to a stable result, and it also
does not do any error checking.

By default, Org uses 3 runs of `pdfgroff' to do the processing.
If you have texi2dvi on your system and if that does not cause
the infamous egrep/locale bug:

     http://lists.gnu.org/archive/html/bug-texinfo/2010-03/msg00031.html

then `texi2dvi' is the superior choice.  Org does offer it as one
of the customize options.

Alternatively, this may be a Lisp function that does the
processing, so you could use this to apply the machinery of
AUCTeX or the Emacs Man mode.  This function should accept the
file name as its single argument."
  :group 'org-export-pdf
  :type '(choice
	  (repeat :tag "Shell command sequence"
		  (string :tag "Shell command"))
	  (const :tag "2 runs of pdfgroff"
		 ("tbl %f | eqn | groff -mm | ps2pdf - > %b.pdf"
		  "tbl %f | eqn | groff -mm | ps2pdf - > %b.pdf" ))
	  (const :tag "3 runs of pdfgroff"
		 ("tbl %f | eqn | groff -mm | ps2pdf - > %b.pdf"
		  "tbl %f | eqn | groff -mm | ps2pdf - > %b.pdf"
		  "tbl %f | eqn | groff -mm | ps2pdf - > %b.pdf"))
	  (function)))

(defcustom org-e-man-logfiles-extensions
  '("aux" "idx" "log" "out" "toc" "nav" "snm" "vrb")
  "The list of file extensions to consider as Man logfiles."
  :group 'org-export-e-man
  :type '(repeat (string :tag "Extension")))

(defcustom org-e-man-remove-logfiles t
  "Non-nil means remove the logfiles produced by PDF production.
These are the .aux, .log, .out, and .toc files."
  :group 'org-export-e-man
  :type 'boolean)


;; Preamble

;; Adding MAN as a block parser to make sure that its contents
;; does not execute

(add-to-list 'org-element-block-name-alist
	     '("MAN" . org-element-export-block-parser))



;;; Internal Functions


(defun org-e-man--caption/label-string (caption label info)
  "Return caption and label Man string for floats.

CAPTION is a cons cell of secondary strings, the car being the
standard caption and the cdr its short form.  LABEL is a string
representing the label.  INFO is a plist holding contextual
information.

If there's no caption nor label, return the empty string.

For non-floats, see `org-e-man--wrap-label'."
  (let ((label-str "" ))
    (cond
     ((and (not caption) (not label)) "")
     ((not caption) (format "\\fI%s\\fP" label))
     ;; Option caption format with short name.
     ((cdr caption)
      (format "\\fR%s\\fP - \\fI%s\\P - %s\n"
	      (org-export-data (cdr caption) info)
	      label-str
	      (org-export-data (car caption) info)))
     ;; Standard caption format.
     (t (format "\\fR%s\\fP"
		(org-export-data (car caption) info)))))

  )



(defun org-e-man--guess-babel-language (header info)
  "Set Babel's language according to LANGUAGE keyword.

HEADER is the Man header string.  INFO is the plist used as
a communication channel.

Insertion of guessed language only happens when Babel package has
explicitly been loaded.  Then it is added to the rest of
package's options.

Return the new header."
  header )


(defun org-e-man--guess-inputenc (header)
  "Set the coding system in inputenc to what the buffer is.
HEADER is the Man header string.  Return the new header."
  header )


(defun org-e-man--make-option-string (options)
  "Return a comma separated string of keywords and values.
OPTIONS is an alist where the key is the options keyword as
a string, and the value a list containing the keyword value, or
nil."
  (mapconcat (lambda (pair)
	       (concat ":" (first pair) " "
		       (when (> (length (second pair)) 0)
			 (concat (second pair)))))
	     options
	     " "))

(defun org-e-man--quotation-marks (text info)
  "Export quotation marks depending on language conventions.
TEXT is a string containing quotation marks to be replaced.  INFO
is a plist used as a communication channel."
  (mapc (lambda(l)
	  (let ((start 0))
	    (while (setq start (string-match (car l) text start))
	      (let ((new-quote (concat (match-string 1 text) (cdr l))))
		(setq text (replace-match new-quote  t t text))))))
	(cdr (or (assoc (plist-get info :language) org-e-man-quotes)
		 ;; Falls back on English.
		 (assoc "en" org-e-man-quotes))))
  text)

(defun org-e-man--wrap-label (element output)
  "Wrap label associated to ELEMENT around OUTPUT, if appropriate.
This function shouldn't be used for floats.  See
`org-e-man--caption/label-string'."
  (let ((label (org-element-property :name element)))
    (if (or (not output) (not label) (string= output "") (string= label ""))
	output
      (concat (format "%s\n.br\n" label) output))))

(defun org-e-man--text-markup (text markup)
  "Format TEXT depending on MARKUP text markup.
See `org-e-man-text-markup-alist' for details."
  (let ((fmt (cdr (assq markup org-e-man-text-markup-alist))))
    (cond
     ;; No format string: Return raw text.
     ((not fmt) text)
     ((string= "protectedtexttt" fmt)
      (let ((start 0)
	    (trans '(("\\" . "\\")))
	    (rtn "")
	    char)
	(while (string-match "[\\{}$%&_#~^]" text)
	  (setq char (match-string 0 text))
	  (if (> (match-beginning 0) 0)
	      (setq rtn (concat rtn (substring text 0 (match-beginning 0)))))
	  (setq text (substring text (1+ (match-beginning 0))))
	  (setq char (or (cdr (assoc char trans)) (concat "\\" char))
		rtn (concat rtn char)))
	(setq text (concat rtn text) )
	(format "\\fC%s\\fP" text)))
     ;; Else use format string.
     (t (format fmt text)))))


;;; Template

(defun org-e-man-template (contents info)
  "Return complete document string after Man conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((title (org-export-data (plist-get info :title) info))
	(attr
	 (read
	  (format
	   "(%s)"
	   (mapconcat
	    #'identity
	    (list (plist-get info :man-class-options))
	    " "))))
	(class (plist-get info :man-class)))
    (concat
     ;; 2. Title
     (cond 
      ((string= "" title)
       (format ".TH \"%s\" \"%s\" \n" " " class )
       )
      (t
       (format ".TH \"%s\" \"%s\" \n" title class )))

     ;; 7. Document's body.
     
     contents

     ;; 8. Table of Content must be placed at the end being
     ;; that it gets collected from all the headers. 
     ;; In the case of letters, signature will be placed instead.

     )))


;;; Transcode Functions

;;;; Babel Call
;;
;; Babel Calls are ignored.


;;;; Bold

(defun org-e-man-bold (bold contents info)
  "Transcode BOLD from Org to Man.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (org-e-man--text-markup contents 'bold))


;;;; Center Block

(defun org-e-man-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to Man.
CONTENTS holds the contents of the center block.  INFO is a plist
holding contextual information."
  (org-e-man--wrap-label
   center-block
   (format ".ce %d\n.nf\n%s\n.fi" 
	   (- (length (split-string contents "\n")) 1 ) 
	   contents)))


;;;; Clock

(defun org-e-man-clock (clock contents info)
  "Transcode a CLOCK element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (concat
   (format "\\fB%s\\fP " org-clock-string)
   (format org-e-man-inactive-timestamp-format
	   (concat (org-translate-time (org-element-property :value clock))
		   (let ((time (org-element-property :time clock)))
		     (and time (format " (%s)" time)))))
   "\\\\"))


;;;; Code

(defun org-e-man-code (code contents info)
  "Transcode a CODE object from Org to Man.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-e-man--text-markup (org-element-property :value code) 'code))


;;;; Comment
;;
;; Comments are ignored.


;;;; Comment Block
;;
;; Comment Blocks are ignored.


;;;; Drawer

;; Drawers are ignored


;;;; Dynamic Block

(defun org-e-man-dynamic-block (dynamic-block contents info)
  "Transcode a DYNAMIC-BLOCK element from Org to Man.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See `org-export-data'."
  (org-e-man--wrap-label dynamic-block contents))


;;;; Entity

(defun org-e-man-entity (entity contents info)
  "Transcode an ENTITY object from Org to Man.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (let ((ent (org-element-property :man entity)))
    (if (org-element-property :man-math-p entity) (format "$%s$" ent) ent)))


;;;; Example Block

(defun org-e-man-example-block (example-block contents info)
  "Transcode an EXAMPLE-BLOCK element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (org-e-man--wrap-label
   example-block
   (format ".RS\n.nf\n%s\n.fi\n.RE"
	   (org-export-format-code-default example-block info))))


;;;; Export Block

(defun org-e-man-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "MAN")
    (org-remove-indentation (org-element-property :value export-block))))


;;;; Export Snippet

(defun org-e-man-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'e-man)
    (org-element-property :value export-snippet)))


;;;; Fixed Width

(defun org-e-man-fixed-width (fixed-width contents info)
  "Transcode a FIXED-WIDTH element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-e-man--wrap-label
   fixed-width
   (format "\\fC\n%s\\fP"
	   (org-remove-indentation
	    (org-element-property :value fixed-width)))))


;;;; Footnote Definition
;;
;; Footnote Definitions are ignored.

;; 
;; Footnotes are handled automatically in MAN. Although
;; manual references can be added, not really required.
;; 


;;;; Headline

(defun org-e-man-headline (headline contents info)
  "Transcode an HEADLINE element from Org to Man.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((class (plist-get info :man-class))
	 (level (org-export-get-relative-level headline info))
	 (numberedp (org-export-numbered-headline-p headline info))
	 (class-sectionning (assoc class org-e-man-classes))
	 ;; Section formatting will set two placeholders: one for the
	 ;; title and the other for the contents.
	 (section-fmt
	  (let ((sec (if (and (symbolp (nth 2 class-sectionning))
			      (fboundp (nth 2 class-sectionning)))
			 (funcall (nth 2 class-sectionning) level numberedp)
		       (nth (1+ level) class-sectionning))))
	    (cond
	     ;; No section available for that LEVEL.
	     ((not sec) nil)
	     ;; Section format directly returned by a function.
	     ((stringp sec) sec)
	     ;; (numbered-section . unnumbered-section)
	     ((not (consp (cdr sec)))
	      (concat (funcall (if numberedp #'car #'cdr) sec) "\n%s"))
	     ;; (numbered-open numbered-close)
	     ((= (length sec) 2)
	      (when numberedp (concat (car sec) "\n%s" (nth 1 sec))))
	     ;; (num-in num-out no-num-in no-num-out)
	     ((= (length sec) 4)
	      (if numberedp (concat (car sec) "\n%s" (nth 1 sec))
		(concat (nth 2 sec) "\n%s" (nth 3 sec)))))))
	 (text (org-export-data (org-element-property :title headline) info))
	 (todo
	  (and (plist-get info :with-todo-keywords)
	       (let ((todo (org-element-property :todo-keyword headline)))
		 (and todo (org-export-data todo info)))))
	 (todo-type (and todo (org-element-property :todo-type headline)))
	 (tags (and (plist-get info :with-tags)
		    (org-export-get-tags headline info)))
	 (priority (and (plist-get info :with-priority)
			(org-element-property :priority headline)))
	 ;; Create the headline text along with a no-tag version.  The
	 ;; latter is required to remove tags from table of contents.
	 (full-text (if (functionp org-e-man-format-headline-function)
			;; User-defined formatting function.
			(funcall org-e-man-format-headline-function
				 todo todo-type priority text tags)
		      ;; Default formatting.
		      (concat
		       (when todo
			 (format "\\fB%s\\fP " todo))
		       (when priority (format " [\\#%c] " priority))
		       text
		       (when tags
			 (format "\\fC:%s:\\fP "
				 (mapconcat 'identity tags ":"))))))
	 (full-text-no-tag
	  (if (functionp org-e-man-format-headline-function)
	      ;; User-defined formatting function.
	      (funcall org-e-man-format-headline-function
		       todo todo-type priority text nil)
	    ;; Default formatting.
	    (concat
	     (when todo (format "\\fB%s\\fP " todo))
	     (when priority (format " [\\#%c] " priority))
	     text)))
	 ;; Associate some \label to the headline for internal links.
	 ;; 	 (headline-label 
	 ;; 	  (format "\\label{sec-%s}\n"
	 ;; 		  (mapconcat 'number-to-string
	 ;; 			     (org-export-get-headline-number headline info)
	 ;; 			     "-")))
	 (headline-label "")
	 (pre-blanks
	  (make-string (org-element-property :pre-blank headline) 10)))
    (cond
     ;; Case 1: This is a footnote section: ignore it.
     ((org-element-property :footnote-section-p headline) nil)
     ;; Case 2. This is a deep sub-tree: export it as a list item.
     ;;         Also export as items headlines for which no section
     ;;         format has been found.
     ((or (not section-fmt) (org-export-low-level-p headline info))
      ;; Build the real contents of the sub-tree.
      (let ((low-level-body
	     (concat
	      ;; If the headline is the first sibling, start a list.
	      (when (org-export-first-sibling-p headline)
			(format "%s\n" ".RS"))
	      ;; Itemize headline
	      ".TP\n.ft I\n" full-text "\n.ft\n" 
		  headline-label pre-blanks contents ".RE")))
	;; If headline is not the last sibling simply return
	;; LOW-LEVEL-BODY.  Otherwise, also close the list, before any
	;; blank line.
	(if (not (org-export-last-sibling-p headline)) low-level-body
	  (replace-regexp-in-string
	   "[ \t\n]*\\'" ""
	   low-level-body))))
     ;; Case 3. Standard headline.  Export it as a section.
     (t
      (cond
       ((not (and tags (eq (plist-get info :with-tags) 'not-in-toc)))
	;; Regular section.  Use specified format string.
	(format section-fmt full-text
		(concat headline-label pre-blanks contents)))
       ((string-match "\\`\\\\\\(.*?\\){" section-fmt)
	;; If tags should be removed from table of contents, insert
	;; title without tags as an alternative heading in sectioning
	;; command.
	(format (replace-match (concat (match-string 1 section-fmt) "[%s]")
			       nil nil section-fmt 1)
		;; Replace square brackets with parenthesis since
		;; square brackets are not supported in optional
		;; arguments.
		(replace-regexp-in-string
		 "\\[" "("
		 (replace-regexp-in-string
		  "\\]" ")"
		  full-text-no-tag))
		full-text
		(concat headline-label pre-blanks contents)))
       (t
	;; Impossible to add an alternative heading.  Fallback to
	;; regular sectioning format string.
	(format section-fmt full-text
		(concat headline-label pre-blanks contents))))))))


;;;; Horizontal Rule
;; Not supported


;;;; Inline Babel Call
;;
;; Inline Babel Calls are ignored.

;;;; Inline Src Block

(defun org-e-man-inline-src-block (inline-src-block contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to Man.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((code (org-element-property :value inline-src-block))
	 (separator (org-e-man--find-verb-separator code)))
    (cond
     (org-e-man-source-highlight
      (let* ((tmpdir (if (featurep 'xemacs)
			 temp-directory 
		       temporary-file-directory ))
	     (in-file  (make-temp-name 
			(expand-file-name "srchilite" tmpdir))  )
	     (out-file (make-temp-name 
			(expand-file-name "reshilite" tmpdir)) )
	     (org-lang (org-element-property :language inline-src-block))
	     (lst-lang (cadr (assq (intern org-lang)
				   org-e-man-source-highlight-langs)))
	     
	     (cmd (concat (expand-file-name "source-highlight")
			  " -s " lst-lang
			  " -f groff_man"
			  " -i " in-file
			  " -o " out-file
			  )
		  ))

	(if lst-lang
	    (let ((code-block "" ))
	      (with-temp-file in-file (insert code))
	      (shell-command cmd)
	      (setq code-block  (org-file-contents out-file) )
	      (delete-file in-file)
	      (delete-file out-file)
	      code-block)
	  (format ".RS\n.nf\n\\fC\\m[black]%s\\m[]\\fP\n.fi\n.RE\n"
		  code))

	))

     ;; Do not use a special package: transcode it verbatim.
     (t
      (concat ".RS\n.nf\n" "\\fC" separator "\n" code "\n" separator 
	      "\\fP\n.fi\n.RE\n"))
     )))


;;;; Inlinetask
;;;; Italic

(defun org-e-man-italic (italic contents info)
  "Transcode ITALIC from Org to Man.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (org-e-man--text-markup contents 'italic))


;;;; Item


(defun org-e-man-item (item contents info)
  "Transcode an ITEM element from Org to Man.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((counter
	  (let ((count (org-element-property :counter item))
		(level
		 (loop for parent in (org-export-get-genealogy item)
		       count (eq (org-element-type parent) 'plain-list)
		       until (eq (org-element-type parent) 'headline))))
	    (and count
		 (< level 5)
		 (concat ""))))
	 (bullet (org-element-property :bullet item))
	 (checkbox (case (org-element-property :checkbox item)
		     (on "\\o'\\(sq\\(mu'")			;; 
		     (off "\\(sq ")					;;
		     (trans "\\o'\\(sq\\(mi'"   ))) ;;

	 (tag (let ((tag (org-element-property :tag item)))
		;; Check-boxes must belong to the tag.
		(and tag (format "\\fB%s\\fP"
				 (concat checkbox
					 (org-export-data tag info)))))))
;; removed counter

    (if (or (not (null tag))
	    (not (null checkbox))) 
	(concat "\n.TP\n" (or tag (concat " " checkbox)) "\n"
		(org-trim (or contents " " ) )
		;; If there are footnotes references in tag, be sure to
		;; add their definition at the end of the item.  This
		)
      (let ((marker (cond  ((string= "-" (org-trim bullet)) "\\(em")
			   ((string= "*" (org-trim bullet)) "\\(bu")
			   (t "\\(dg")
		      ) ))
	(concat "\n.IP " marker " 4\n"
		(org-trim (or contents " " ) )
		;; If there are footnotes references in tag, be sure to
		;; add their definition at the end of the item.  This
		) ))
))



;;;; Keyword


(defun org-e-man-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
	(value (org-element-property :value keyword)))
    (cond
     ((string= key "MAN") value)
     ((string= key "INDEX") nil)
     ;; Invisible targets.
     ((string= key "TARGET") nil)
     ((string= key "TOC"   ) nil  ))))


;;;; Man Environment

(defun org-e-man-man-environment (man-environment contents info)
  "Transcode a MAN-ENVIRONMENT element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((label (org-element-property :name man-environment))
	(value (org-remove-indentation
		(org-element-property :value man-environment))))
    (if (not (org-string-nw-p label)) value
      ;; Environment is labelled: label must be within the environment
      ;; (otherwise, a reference pointing to that element will count
      ;; the section instead).
      (with-temp-buffer
	(insert value)
	(goto-char (point-min))
	(forward-line)
	(insert (format "%s\n" label))
	(buffer-string)))))


;;;; Man Fragment

(defun org-e-man-man-fragment (man-fragment contents info)
  "Transcode a MAN-FRAGMENT object from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-property :value man-fragment))


;;;; Line Break

(defun org-e-man-line-break (line-break contents info)
  "Transcode a LINE-BREAK object from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ".br\n")


;;;; Link
;;;;
;;;; Inline images just place a call to .PSPIC or .PS/.PE
;;;  and load the graph.
;;;;


(defun org-e-man-link (link desc info)
  "Transcode a LINK object from Org to Man.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  
  (let* ((type (org-element-property :type link))
	 (raw-path (org-element-property :path link))
	 ;; Ensure DESC really exists, or set it to nil.
	 (desc (and (not (string= desc "")) desc))
	 (imagep (org-export-inline-image-p
		  link org-e-man-inline-image-rules))
	 (path (cond
		((member type '("http" "https" "ftp" "mailto"))
		 (concat type ":" raw-path))
		((string= type "file")
		 (when (string-match "\\(.+\\)::.+" raw-path)
		   (setq raw-path (match-string 1 raw-path)))
		 (if (file-name-absolute-p raw-path)
		     (concat "file://" (expand-file-name raw-path))
		   (concat "file://" raw-path)))
		(t raw-path)))
	 protocol)
    (cond
     ;; Radio link: Transcode target's contents and use them as link's
     ;; description.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
	(when destination
	  (format "\\fI [%s] \\fP"
		  (org-export-solidify-link-text path) ))))
     ;; Links pointing to an headline: Find destination and build
     ;; appropriate referencing command.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
			     (org-export-resolve-fuzzy-link link info)
			   (org-export-resolve-id-link link info))))
	(case (org-element-type destination)
	  ;; Id link points to an external file.
	  (plain-text
	   (if desc (format "%s \\fBat\\fP \\fIfile://%s\\fP" desc destination)
	     (format "\\fI file://%s \\fP" destination)))
	  ;; Fuzzy link points nowhere.
	  ('nil
	   (format org-e-man-link-with-unknown-path-format
		   (or desc
		       (org-export-data
			(org-element-property :raw-link link) info))))
	  ;; Fuzzy link points to an invisible target.
	  (keyword nil)
	  ;; LINK points to an headline.  If headlines are numbered
	  ;; and the link has no description, display headline's
	  ;; number.  Otherwise, display description or headline's
	  ;; title.
	  (headline
	   (let ((label ""))
	     (if (and (plist-get info :section-numbers) (not desc))
		 (format "\\fI%s\\fP" label)
	       (format "\\fI%s\\fP"
		       (or desc
			   (org-export-data
			    (org-element-property :title destination) info))))))
          ;; Fuzzy link points to a target.  Do as above.
	  (otherwise
	   (let ((path (org-export-solidify-link-text path)))
	     (if (not desc) (format "\\fI%s\\fP" path)
	       (format "%s \\fBat\\fP \\fI%s\\fP" desc path)))))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (format (org-export-get-coderef-format path desc)
	      (org-export-resolve-coderef path info)))
     ;; Link type is handled by a special function.
     ((functionp (setq protocol (nth 2 (assoc type org-link-protocols))))
      (funcall protocol (org-link-unescape path) desc 'man))
     ;; External link with a description part.
     ((and path desc) (format "%s \\fBat\\fP \\fI%s\\fP" path desc))
     ;; External link without a description part.
     (path (format "\\fI%s\\fP" path))
     ;; No path, only description.  Try to do something useful.
     (t (format org-e-man-link-with-unknown-path-format desc)))))


;;;; Macro

(defun org-e-man-macro (macro contents info)
  "Transcode a MACRO element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;; Use available tools.
  (org-export-expand-macro macro info))


;;;; Paragraph

(defun org-e-man-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to Man.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (setq parent (plist-get (nth 1 paragraph) :parent))
  (when parent
    (let ((parent-type (car parent)) 
	  (fixed-paragraph ""))
      (cond ((and (eq parent-type 'item)
		  (plist-get (nth 1 parent) :bullet ) )
	     (setq fixed-paragraph (concat "" contents)) )
	    ((eq parent-type 'section)
	     (setq fixed-paragraph (concat ".PP\n" contents) ) )
	    ((eq parent-type 'footnote-definition)
	     (setq fixed-paragraph (concat "" contents) ))
	    (t (setq fixed-paragraph (concat "" contents)) ) 
	    )
      fixed-paragraph)
    )
  )


;;;; Plain List

(defun org-e-man-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to Man.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* ((type (org-element-property :type plain-list))
	 (paralist-types '("inparaenum" "asparaenum" "inparaitem" "asparaitem"
			   "inparadesc" "asparadesc"))
	 (paralist-regexp (concat
			   "\\("
			   (mapconcat 'identity paralist-types "\\|")
			   "\\)"))
	 (attr (mapconcat #'identity
			  (org-element-property :attr_man plain-list)
			  " "))
	 (list-type (cond
		      ((eq type 'ordered) "\\(bu")
		      ((eq type 'unordered) "\\(em")
		      ((eq type 'descriptive) ""))))

    (org-e-man--wrap-label
     plain-list
     (format "%s %s "
	     
	     ;; Once special environment, if any, has been removed, the
	     ;; rest of the attributes will be optional arguments.
	     ;; They will be put inside square brackets if necessary.
	     (let ((opt (replace-regexp-in-string
			 (format " *%s *" paralist-regexp) "" attr)))
	       (cond ((string= opt "") "")
		     ((string-match "\\`\\[[^][]+\\]\\'" opt) opt)
		     (t (format "\\fB%s\\fP" opt))))
	     contents ))))


;;;; Plain Text

(defun org-e-man-plain-text (text info)
  "Transcode a TEXT string from Org to Man.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  ;; Protect 
    (setq text (replace-regexp-in-string
  	      "\\(?:[^\\]\\|^\\)\\(\\\\\\)\\(?:[^%$#&{}~^_\\]\\|$\\)"
  	      "$\\" text nil t 1))

    ;; Protect leading dots and quotes

    (setq text (replace-regexp-in-string  "^[.']" 
					  "\\\\&\\&" text nil t 1))
  ;; Handle quotation marks
   (setq text (org-e-man--quotation-marks text info))
  ;; Handle break preservation if required.
    (when (plist-get info :preserve-breaks)
      (setq text (replace-regexp-in-string "\\(\\\\\\\\\\)?[ \t]*\n" " \\\\\\\\\n"
  					 text)))
  ;; Return value.
  text)



;;;; Planning


;;;; Property Drawer


;;;; Quote Block

(defun org-e-man-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to Man.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (org-e-man--wrap-label
   quote-block
   (format ".RS\n%s\n.RE" contents)))

;;;; Quote Section

(defun org-e-man-quote-section (quote-section contents info)
  "Transcode a QUOTE-SECTION element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((value (org-remove-indentation
		(org-element-property :value quote-section))))
    (when value (format ".RS\\fI%s\\fP\n.RE\n" value))))


;;;; Radio Target

(defun org-e-man-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to Man.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (format "%s - %s"
	  (org-export-solidify-link-text
	   (org-element-property :value radio-target))
	  text))


;;;; Section

(defun org-e-man-section (section contents info)
  "Transcode a SECTION element from Org to Man.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  contents)


;;;; Special Block

(defun org-e-man-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to Man.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((type (downcase (org-element-property :type special-block))))
    (org-e-man--wrap-label
     special-block
     (format "%s\n" contents))))


;;;; Src Block

(defun org-e-man-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to Man.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."

  (let* ((lang (org-element-property :language src-block))
	 (caption (org-element-property :caption src-block))
	 (label (org-element-property :name src-block))
	 (code (org-element-property :value src-block))
	 (custom-env (and lang
			  (cadr (assq (intern lang)
				      org-e-man-custom-lang-environments))))
	 (num-start (case (org-element-property :number-lines src-block)
		      (continued (org-export-get-loc src-block info))
		      (new 0)))
	 (retain-labels (org-element-property :retain-labels src-block)))
    (cond
     ;; Case 1.  No source fontification.
     ((not org-e-man-source-highlight)
      (let ((caption-str (org-e-man--caption/label-string caption label info))
	    (float-env (when caption ".RS\n.nf\\fC%s\\fP\n.fi.RE\n")))
	(format
	 (or float-env "%s")
	 (concat 
	  (format ".RS\n.nf\n\\fC%s\\fP\n.fi\nRE\n\n"
		  (org-export-format-code-default src-block info) 
		  )))))
     ( (and org-e-man-source-highlight) 
       (let* ((tmpdir (if (featurep 'xemacs)
			  temp-directory 
			temporary-file-directory ))
	      
	      (in-file  (make-temp-name 
			 (expand-file-name "srchilite" tmpdir))  )
	      (out-file (make-temp-name 
			 (expand-file-name "reshilite" tmpdir)) )

	      (org-lang (org-element-property :language src-block))
	      (lst-lang (cadr (assq (intern org-lang)
				    org-e-man-source-highlight-langs)) )
	      
	      (cmd (concat "source-highlight"
			   " -s " lst-lang
			   " -f groff_man "
			   " -i " in-file
			   " -o " out-file
			   )
		   ))
	 
	 (if lst-lang
	     (let ((code-block "" ))
	       (with-temp-file in-file (insert code))
	       (shell-command cmd)
	       (setq code-block  (org-file-contents out-file) )
	       (delete-file in-file)
	       (delete-file out-file)
	       code-block)
	   (format ".RS\n.nf\n\\fC\\m[black]%s\\m[]\\fP\n.fi\n.RE"
		   code))

	 )
       )
     )))


;;;; Statistics Cookie

(defun org-e-man-statistics-cookie (statistics-cookie contents info)
  "Transcode a STATISTICS-COOKIE object from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-property :value statistics-cookie))


;;;; Strike-Through

(defun org-e-man-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to Man.
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (org-e-man--text-markup contents 'strike-through))

;;;; Subscript

(defun org-e-man-subscript (subscript contents info)
  "Transcode a SUBSCRIPT object from Org to Man.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format  "\\d\\s-2%s\\s+2\\u" contents))

;;;; Superscript "^_%s$

(defun org-e-man-superscript (superscript contents info)
  "Transcode a SUPERSCRIPT object from Org to Man.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format  "\\u\\s-2%s\\s+2\\d" contents))


;;;; Table
;;
;; `org-e-man-table' is the entry point for table transcoding.  It
;; takes care of tables with a "verbatim" attribute.  Otherwise, it
;; delegates the job to either `org-e-man-table--table.el-table' or
;; `org-e-man-table--org-table' functions, depending of the type of
;; the table.
;;
;; `org-e-man-table--align-string' is a subroutine used to build
;; alignment string for Org tables.

(defun org-e-man-table (table contents info)
  "Transcode a TABLE element from Org to Man.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (cond
   ;; Case 1: verbatim table.
   ((or org-e-man-tables-verbatim
	(let ((attr
	       (read
		(format
		 "(%s)"
		 (mapconcat
		  #'identity
		  (org-element-property :attr_man table)
		  " ")))) )

	  (and attr (plist-get attr :verbatim))))

    (format ".nf\n\\fC%s\\fP\n.fi"
	    ;; Re-create table, without affiliated keywords.
	    (org-trim
	     (org-element-interpret-data
	      `(table nil ,@(org-element-contents table))))))
   ;; Case 2: Standard table.
   (t (org-e-man-table--org-table table contents info))))

(defun org-e-man-table--align-string (divider table info)
  "Return an appropriate Man alignment string.
TABLE is the considered table.  INFO is a plist used as
a communication channel."
  (let ((attr
	 (read
	  (format
	   "(%s)"
	   (mapconcat
	    #'identity
	    (org-element-property :attr_man table)
	    " ")))))

    (setq align 	
	  (case (plist-get  attr :align)
	    ('center "c")
	    ('left "l")
	    ('right "r")))

    (let (alignment)
      ;; Extract column groups and alignment from first (non-rule)
      ;; row.
      (org-element-map
       (org-element-map
	table 'table-row
	(lambda (row)
	  (and (eq (org-element-property :type row) 'standard) row))
	info 'first-match)
       'table-cell
       (lambda (cell)
	 (let* ((borders (org-export-table-cell-borders cell info))
		(raw-width (org-export-table-cell-width cell info))
		(width-cm (when raw-width (/ raw-width 5)))
		(width (if raw-width (format "w(%dc)" (if (< width-cm 1) 1 width-cm)) "") ))
	   ;; Check left border for the first cell only.
	   (when (and (memq 'left borders) (not alignment))
	     (push "|" alignment))
	   (push 
	    (if (not align)
		(case (org-export-table-cell-alignment cell info)
		  (left (concat "l" width divider) )
		  (right (concat "r" width divider))
		  (center (concat "c" width divider)))
	      (concat align divider))
	    alignment)
	   (when (memq 'right borders) (push "|" alignment))))
       info)
      (apply 'concat (reverse alignment)))

    ))

(defun org-e-man-table--org-table (table contents info)
  "Return appropriate Man code for an Org table.

TABLE is the table type element to transcode.  CONTENTS is its
contents, as a string.  INFO is a plist used as a communication
channel.

This function assumes TABLE has `org' as its `:type' attribute."
  (let* ((label (org-element-property :name table))
	 (caption (org-e-man--caption/label-string
		   (org-element-property :caption table) label info))
	 (attr
	  (read
	   (format
	    "(%s)"
	    (mapconcat
	     #'identity
	     (org-element-property :attr_man table)
	     " "))))

	 (divider (if (plist-get attr :divider)
		      "|"
		    " "))

	 ;; Determine alignment string.
	 (alignment (org-e-man-table--align-string divider table info))
	 ;; Extract others display options.

	 )
    ;; Prepare the final format string for the table.

    (setq lines (org-split-string contents "\n"))

    (setq attr-list
	  (let ((result-list '()))
	    (dolist (attr-item 
		     (list 
		      (if (plist-get attr :expand) 
			  "expand"
			nil
			)

		      (case (plist-get attr :placement)
			('center "center")
			('left nil)
			(t 
			 (if org-e-man-tables-centered  
			     "center" 
			   "" )))

		      (case (plist-get attr :boxtype)
			('box "box")
			('doublebox "doublebox")
			('allbox "allbox")
			('none nil)
			(t "box"))
		      ))

	      (if (not (null attr-item))
		  (add-to-list 'result-list attr-item)
		))
	    result-list ))


    (setq title-line  (plist-get attr :title-line))

    (setq table-format (concat 
			(format "%s"
				(or (car attr-list) "" ))
			(or 
			 (let ((output-list '()))
			   (when (cdr attr-list)
			     (dolist (attr-item (cdr attr-list))
			       (setq output-list (concat output-list  (format ",%s" attr-item )) ) ))
			   output-list)
			 "") ))

    
    (when lines
      (setq first-line (org-split-string (car lines) "\t")))

    (cond
     ;; Others.
     (lines (concat ".TS\n " table-format ";\n" 
		    
		    (format "%s.\n"
			    (let ((final-line ""))

			      (when title-line
				(dotimes (i (length first-line))
				  (setq final-line (concat final-line "cb" divider))
				  ))

			      (setq final-line (concat final-line "\n"))
			      (if alignment
				  (setq final-line (concat final-line alignment))
				(dotimes (i (length first-line))
				  (setq final-line (concat final-line "c" divider))))
			      final-line ))
		    (format "%s.TE"
			    (let ((final-line ""))
			      (dolist (line-item lines)
				(cond 
				 (t	
				  (setq lines (org-split-string contents "\n"))

				  (setq final-line (concat final-line 
							   (car (org-split-string line-item "\\\\")) "\n"))
				  )
				 )
				
				)  final-line) )

		    )))))





;;;; Table Cell

(defun org-e-man-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to Man
CONTENTS is the cell contents.  INFO is a plist used as
a communication channel."
  (concat (if (and contents
		   org-e-man-table-scientific-notation
		   (string-match orgtbl-exp-regexp contents))
	      ;; Use appropriate format string for scientific
	      ;; notation.
	      (format org-e-man-table-scientific-notation
		      (match-string 1 contents)
		      (match-string 2 contents))
	    contents)
	  (when (org-export-get-next-element table-cell) " \t ")))


;;;; Table Row

(defun org-e-man-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to Man
CONTENTS is the contents of the row.  INFO is a plist used as
a communication channel."
  ;; Rules are ignored since table separators are deduced from
  ;; borders of the current row.
  (when (eq (org-element-property :type table-row) 'standard)
    (let* ((attr (mapconcat 'identity
			    (org-element-property
			     :attr_man (org-export-get-parent table-row))
			    " "))
	   ;; TABLE-ROW's borders are extracted from its first cell.
	   (borders
	    (org-export-table-cell-borders
	     (car (org-element-contents table-row)) info)))
      (concat
       ;; Mark "hline" for horizontal lines.
       (cond  ((and (memq 'top borders) (memq 'above borders)) "_\n"))
       contents "\\\\\n"
       (cond
	;; When BOOKTABS are activated enforce bottom rule even when
	;; no hline was specifically marked.
	((and (memq 'bottom borders) (memq 'below borders)) "_\n")
	((memq 'below borders) "_"))))))




;;;; Target

(defun org-e-man-target (target contents info)
  "Transcode a TARGET object from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "\\fI%s\\fP"
	  (org-export-solidify-link-text (org-element-property :value target))))


;;;; Timestamp

(defun org-e-man-timestamp (timestamp contents info)
  "Transcode a TIMESTAMP object from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((value (org-translate-time (org-element-property :value timestamp)))
	(type (org-element-property :type timestamp)))
    (cond ((memq type '(active active-range))
	   (format org-e-man-active-timestamp-format value))
	  ((memq type '(inactive inactive-range))
	   (format org-e-man-inactive-timestamp-format value))
	  (t (format org-e-man-diary-timestamp-format value)))))


;;;; Underline

(defun org-e-man-underline (underline contents info)
  "Transcode UNDERLINE from Org to Man.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (org-e-man--text-markup contents 'underline))


;;;; Verbatim

(defun org-e-man-verbatim (verbatim contents info)
  "Transcode a VERBATIM object from Org to Man.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-e-man--text-markup (org-element-property :value verbatim) 'verbatim))


;;;; Verse Block

(defun org-e-man-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to Man.
CONTENTS is verse block contents. INFO is a plist holding
contextual information."
  (format ".RS\n.ft I\n%s\n.ft\n.RE" contents))



;;; Interactive functions

(defun org-e-man-export-to-man
  (&optional subtreep visible-only body-only ext-plist pub-dir)
  "Export current buffer to a Man file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return output file's name."
  (interactive)
;; TODO needs to change to document class
  (let ((outfile (org-export-output-file-name ".man"  subtreep pub-dir)))
    (org-export-to-file
     'e-man outfile subtreep visible-only body-only ext-plist)))

(defun org-e-man-export-to-pdf
  (&optional subtreep visible-only body-only ext-plist pub-dir)
  "Export current buffer to Groff then process through to PDF.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return PDF file's name."
  (interactive)
  (org-e-man-compile
   (org-e-man-export-to-man
    subtreep visible-only body-only ext-plist pub-dir)))

(defun org-e-man-compile (grofffile)
  "Compile a Groff file.

GROFFFILE is the name of the file being compiled.  Processing is
done through the command specified in `org-e-man-pdf-process'.

Return PDF file name or an error if it couldn't be produced."
  (let* ((wconfig (current-window-configuration))
	 (grofffile (file-truename grofffile))
	 (base (file-name-sans-extension grofffile))
	 errors)
    (message (format "Processing Groff file %s ..." grofffile))
    (unwind-protect
	(progn
	  (cond
	   ;; A function is provided: Apply it.
	   ((functionp org-e-man-pdf-process)
	    (funcall org-e-man-pdf-process (shell-quote-argument grofffile)))
	   ;; A list is provided: Replace %b, %f and %o with appropriate
	   ;; values in each command before applying it.  Output is
	   ;; redirected to "*Org PDF Groff Output*" buffer.
	   ((consp org-e-man-pdf-process)
	    (let* ((out-dir (or (file-name-directory grofffile) "./"))
		   (outbuf (get-buffer-create "*Org PDF Groff Output*")))
	      (mapc
	       (lambda (command)
		 (shell-command
		  (replace-regexp-in-string
		   "%b" (shell-quote-argument base)
		   (replace-regexp-in-string
		    "%f" (shell-quote-argument grofffile)
		    (replace-regexp-in-string
		     "%o" (shell-quote-argument out-dir) command t t) t t) t t)
		  outbuf))
	       org-e-man-pdf-process)
	      ;; Collect standard errors from output buffer.
	      (setq errors (org-e-man-collect-errors outbuf))))
	   (t (error "No valid command to process to PDF")))
	  (let ((pdffile (concat base ".pdf")))
	    ;; Check for process failure.  Provide collected errors if
	    ;; possible.
	    (if (not (file-exists-p pdffile))
		(error (concat (format "PDF file %s wasn't produced" pdffile)
			       (when errors (concat ": " errors))))
	      ;; Else remove log files, when specified, and signal end of
	      ;; process to user, along with any error encountered.
	      (when org-e-man-remove-logfiles
		(dolist (ext org-e-man-logfiles-extensions)
		  (let ((file (concat base "." ext)))
		    (when (file-exists-p file) (delete-file file)))))
	      (message (concat "Process completed"
			       (if (not errors) "."
				 (concat " with errors: " errors)))))
	    ;; Return output file name.
	    pdffile))
      (set-window-configuration wconfig))))

(defun org-e-man-collect-errors (buffer)
  "Collect some kind of errors from \"groff\" output
BUFFER is the buffer containing output.
Return collected error types as a string, or nil if there was
none."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      ;; Find final run
      nil )))


(provide 'org-e-man)
;;; org-e-man.el ends here
