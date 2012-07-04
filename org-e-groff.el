 ;;; org-e-groff.el --- GRoff Back-End For Org Export Engine

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
;; This library implements a Groff back-end for Org generic exporter.
;;
;; To test it, run
;;
;;   M-: (org-export-to-buffer 'e-groff "*Test e-Groff*") RET
;;
;; in an org-mode buffer then switch to the buffer to see the LaTeX
;; export.  See contrib/lisp/org-export.el for more details on how
;; this exporter works.
;;
;; It introduces three new buffer keywords: "GROFF_CLASS",
;; "GROFF_CLASS_OPTIONS" and "GROFF_HEADER".

;;; Code:

(eval-when-compile (require 'cl))

(defvar org-export-groff-default-packages-alist)
(defvar org-export-groff-packages-alist)


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

(defvar org-e-groff-translate-alist
  '((babel-call . org-e-groff-babel-call)
    (bold . org-e-groff-bold)
    (center-block . org-e-groff-center-block)
    (clock . org-e-groff-clock)
    (code . org-e-groff-code)
    (comment . org-e-groff-comment)
    (comment-block . org-e-groff-comment-block)
    (drawer . org-e-groff-drawer)
    (dynamic-block . org-e-groff-dynamic-block)
    (entity . org-e-groff-entity)
    (example-block . org-e-groff-example-block)
    (export-block . org-e-groff-export-block)
    (export-snippet . org-e-groff-export-snippet)
    (fixed-width . org-e-groff-fixed-width)
    (footnote-definition . org-e-groff-footnote-definition)
    (footnote-reference . org-e-groff-footnote-reference)
    (headline . org-e-groff-headline)
    (horizontal-rule . org-e-groff-horizontal-rule)
    (inline-babel-call . org-e-groff-inline-babel-call)
    (inline-src-block . org-e-groff-inline-src-block)
    (inlinetask . org-e-groff-inlinetask)
    (italic . org-e-groff-italic)
    (item . org-e-groff-item)
    (keyword . org-e-groff-keyword)
    (groff-environment . org-e-groff-groff-environment)
    (groff-fragment . org-e-groff-groff-fragment)
    (line-break . org-e-groff-line-break)
    (link . org-e-groff-link)
    (macro . org-e-groff-macro)
    (paragraph . org-e-groff-paragraph)
    (plain-list . org-e-groff-plain-list)
    (plain-text . org-e-groff-plain-text)
    (planning . org-e-groff-planning)
    (property-drawer . org-e-groff-property-drawer)
    (quote-block . org-e-groff-quote-block)
    (quote-section . org-e-groff-quote-section)
    (radio-target . org-e-groff-radio-target)
    (section . org-e-groff-section)
    (special-block . org-e-groff-special-block)
    (src-block . org-e-groff-src-block)
    (statistics-cookie . org-e-groff-statistics-cookie)
    (strike-through . org-e-groff-strike-through)
    (subscript . org-e-groff-subscript)
    (superscript . org-e-groff-superscript)
    (table . org-e-groff-table)
    (table-cell . org-e-groff-table-cell)
    (table-row . org-e-groff-table-row)
    (target . org-e-groff-target)
    (template . org-e-groff-template)
    (timestamp . org-e-groff-timestamp)
    (underline . org-e-groff-underline)
    (verbatim . org-e-groff-verbatim)
    (verse-block . org-e-groff-verse-block))
  "Alist between element or object types and translators.")

(defconst org-e-groff-options-alist
  '((:date "DATE" nil org-e-groff-date-format t)
    (:groff-class "GROFF_CLASS" nil org-e-groff-default-class t)
    (:groff-class-options "GROFF_CLASS_OPTIONS" nil nil t)
    (:groff-header-extra "GROFF_HEADER" nil nil newline))
  "Alist between Groff export properties and ways to set them.
See `org-export-options-alist' for more information on the
structure of the values.")



;;; Internal Variables

(defconst org-e-groff-babel-language-alist
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

(defgroup org-export-e-groff nil
  "Options for exporting Org mode files to Groff."
  :tag "Org Export Groff"
  :group 'org-export)


;;;; Preamble

(defcustom org-e-groff-default-class "internal"
  "The default Groff class."
  :group 'org-export-e-groff
  :type '(string :tag "Groff class"))

(defcustom org-e-groff-classes
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
	 (".H 5 \"%s\"" . ".HU \"%s\"") )
    
    )
  "Alist of Groff classes and associated header and structure.
If #+Groff_CLASS is set in the buffer, use its value and the
associated information.  Here is the structure of each cell:

  \(class-name
    header-string
    \(numbered-section . unnumbered-section\)
    ...\)

The header string
-----------------

The HEADER-STRING is the header that will be inserted into the
Groff file.  It should contain the \\documentclass macro, and
anything else that is needed for this setup.  To this header, the
following commands will be added:

- Calls to \\usepackage for all packages mentioned in the
  variables `org-export-groff-default-packages-alist' and
  `org-export-groff-packages-alist'.  Thus, your header
  definitions should avoid to also request these packages.

- Lines specified via \"#+Groff_HEADER:\"

If you need more control about the sequence in which the header
is built up, or if you want to exclude one of these building
blocks for a particular class, you can use the following
macro-like placeholders.

 [DEFAULT-PACKAGES]      \\usepackage statements for default packages
 [NO-DEFAULT-PACKAGES]   do not include any of the default packages
 [PACKAGES]              \\usepackage statements for packages
 [NO-PACKAGES]           do not include the packages
 [EXTRA]                 the stuff from #+Groff_HEADER
 [NO-EXTRA]              do not include #+Groff_HEADER stuff
 [BEAMER-HEADER-EXTRA]   the beamer extra headers

So a header like

  [NO-DEFAULT-PACKAGES]
  [EXTRA]
  \\fB#1\\fP
  [PACKAGES]

will omit the default packages, and will include the
#+Groff_HEADER lines, then have a call to \\providecommand, and
then place \\usepackage commands based on the content of
`org-export-groff-packages-alist'.

If your header, `org-export-groff-default-packages-alist' or
`org-export-groff-packages-alist' inserts
\"\\usepackage[AUTO]{inputenc}\", AUTO will automatically be
replaced with a coding system derived from
`buffer-file-coding-system'.  See also the variable
`org-e-groff-inputenc-alist' for a way to influence this
mechanism.

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

providing opening and closing strings for a Groff environment
that should represent the document section.  The opening clause
should have a %s to represent the section title.

Instead of a list of sectioning commands, you can also specify
a function name.  That function will be called with two
parameters, the \(reduced) level of the headline, and a predicate
non-nil when the headline should be numbered.  It must return
a format string in which the section title will be added."
  :group 'org-export-e-groff
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

(defcustom org-e-groff-inputenc-alist nil
  "Alist of inputenc coding system names, and what should really be used.
For example, adding an entry

      (\"utf8\" . \"utf8x\")

will cause \\usepackage[utf8x]{inputenc} to be used for buffers that
are written as utf8 files."
  :group 'org-export-e-groff
  :type '(repeat
		  (cons
		   (string :tag "Derived from buffer")
		   (string :tag "Use this instead"))))

(defcustom org-e-groff-date-format
  (format-time-string "%Y-%m-%d")
  "Format string for .ND "
  :group 'org-export-e-groff
  :type 'boolean)

(defcustom org-e-groff-title-command nil
  "The command used to insert the title just before .AU
If this string contains the formatting specification \"%s\" then
it will be used as a formatting string, passing the title as an
argument."
  :group 'org-export-e-groff
  :type 'string)


;;;; Headline


(defcustom org-e-groff-format-headline-function nil
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

\(defun org-e-groff-format-headline (todo todo-type priority text tags)
  \"Default format function for an headline.\"
  \(concat (when todo
            \(format \"\\fB%s\\fP \" todo))
	  \(when priority
            \(format \"[\\#%c] \" priority))
	  text
	  \(when tags
            \(format \" %s \"
              \(mapconcat 'identity tags \":\"))))"
  :group 'org-export-e-groff
  :type 'function)


;;;; Footnotes

(defcustom org-e-groff-footnote-separator "\\*F"
  "Text used to separate footnotes."
  :group 'org-export-e-groff
  :type 'string)


;;;; Timestamps

(defcustom org-e-groff-active-timestamp-format "\\fI%s\\fP"
  "A printf format string to be applied to active timestamps."
  :group 'org-export-e-groff
  :type 'string)

(defcustom org-e-groff-inactive-timestamp-format "\\fI%s\\fP"
  "A printf format string to be applied to inactive timestamps."
  :group 'org-export-e-groff
  :type 'string)

(defcustom org-e-groff-diary-timestamp-format "\\fI%s\\fP"
  "A printf format string to be applied to diary timestamps."
  :group 'org-export-e-groff
  :type 'string)


;;;; Links

(defcustom org-e-groff-image-default-option nil
  "Default option for images."
  :group 'org-export-e-groff
  :type 'string)

(defcustom org-e-groff-default-figure-position nil
  "Default position for groff figures."
  :group 'org-export-e-groff
  :type 'string)

(defcustom org-e-groff-inline-image-rules
  '(("file" . "\\.\\(pdf\\|ps\\|eps\\)\\'"))
  "Rules characterizing image files that can be inlined into Groff.

A rule consists in an association whose key is the type of link
to consider, and value is a regexp that will be matched against
link's path.

Note that, by default, the image extension *actually* allowed
depend on the way the Groff file is processed.  When used with
pdfgroff, pdf, jpg and png images are OK.  When processing
through dvi to Postscript, only ps and eps are allowed.  The
default we use here encompasses both."
  :group 'org-export-e-groff
  :type '(alist :key-type (string :tag "Type")
				:value-type (regexp :tag "Path")))

(defcustom org-e-groff-link-with-unknown-path-format "\\fI%s\\fP"
  "Format string for links with unknown path type."
  :group 'org-export-groff
  :type 'string)


;;;; Tables

(defcustom org-e-groff-default-table-environment nil
  "Default environment used to build tables."
  :group 'org-export-e-groff
  :type 'string)

(defcustom org-e-groff-tables-centered t
  "When non-nil, tables are exported in a center environment."
  :group 'org-export-e-groff
  :type 'boolean)

(defcustom org-e-groff-tables-verbatim nil
  "When non-nil, tables are exported verbatim."
  :group 'org-export-e-groff
  :type 'boolean)

(defcustom org-e-groff-tables-booktabs nil
  "When non-nil, display tables in a formal \"booktabs\" style.
This option assumes that the \"booktabs\" package is properly
loaded in the header of the document.  This value can be ignored
locally with \"booktabs :yes\" and \"booktabs :no\" Groff
attributes."
  :group 'org-export-e-groff
  :type 'boolean)

(defcustom org-e-groff-table-caption-above nil
  "When non-nil, place caption string at the beginning of the table.
Otherwise, place it near the end."
  :group 'org-export-e-groff
  :type 'boolean)

(defcustom org-e-groff-table-scientific-notation "%sE%s"
  "Format string to display numbers in scientific notation.
The format should have \"%s\" twice, for mantissa and exponent
\(i.e. \"%s\\\\times10^{%s}\").

When nil, no transformation is made."
  :group 'org-export-e-groff
  :type '(choice
		  (string :tag "Format string")
		  (const :tag "No formatting")))


;;;; Text markup

(defcustom org-e-groff-text-markup-alist '((bold . "\\fB%s\\fP")
										   ;; from "verb"
										   (code . "\\fC%s\\fP") 
										   (italic . "\\fI%s\\fP")
										   ;;
										   ;; Strike through
										   ;; and underline need to be revised.

										   (strike-through . "\\fC%s\\fP")
										   (underline . "\\fI%s\\fP")
										   (verbatim . protectedtexttt))
  "Alist of Groff expressions to convert text markup.

The key must be a symbol among `bold', `code', `italic',
`strike-through', `underline' and `verbatim'.  The value is
a formatting string to wrap fontified text with.

Value can also be set to the following symbols: `verb' and
`protectedtexttt'.  For the former, Org will use \"\\verb\" to
create a format string and select a delimiter character that
isn't in the string.  For the latter, Org will use \"\\texttt\"
to typeset and try to protect special characters.

If no association can be found for a given markup, text will be
returned as-is."
  :group 'org-export-e-groff
  :type 'alist
  :options '(bold code italic strike-through underline verbatim))


;;;; Drawers

(defcustom org-e-groff-format-drawer-function nil
  "Function called to format a drawer in Groff code.

The function must accept two parameters:
  NAME      the drawer name, like \"LOGBOOK\"
  CONTENTS  the contents of the drawer.

The function should return the string to be exported.

For example, the variable could be set to the following function
in order to mimic default behaviour:

\(defun org-e-groff-format-drawer-default \(name contents\)
  \"Format a drawer element for Groff export.\"
  contents\)"
  :group 'org-export-e-groff
  :type 'function)


;;;; Inlinetasks

(defcustom org-e-groff-format-inlinetask-function nil
  "Function called to format an inlinetask in Groff code.

The function must accept six parameters:
  TODO      the todo keyword, as a string
  TODO-TYPE the todo type, a symbol among `todo', `done' and nil.
  PRIORITY  the inlinetask priority, as a string
  NAME      the inlinetask name, as a string.
  TAGS      the inlinetask tags, as a list of strings.
  CONTENTS  the contents of the inlinetask, as a string.

The function should return the string to be exported.

For example, the variable could be set to the following function
in order to mimic default behaviour:

\(defun org-e-groff-format-inlinetask \(todo type priority name tags contents\)
\"Format an inline task element for Groff export.\"
  \(let ((full-title
	 \(concat
	  \(when todo
            \(format \"\\fB%s\\fP \" todo))
	  \(when priority (format \"[\\#%c] \" priority))
	  title
	  \(when tags
            \(format \":%s:\"
                    \(mapconcat 'identity tags \":\")))))
    \(format (concat \".DS L\\n\"
		    \"%s\\n\\n\"
		    \"%s\"
		    \".DE\")
	    full-title contents))"
  :group 'org-export-e-groff
  :type 'function)


;; Src blocks

(defcustom org-e-groff-listings nil
  "Non-nil means export source code using the listings package.
This package will fontify source code, possibly even with color.
If you want to use this, you also need to make Groff use the
listings package, and if you want to have color, the color
package.  Just add these to `org-export-groff-packages-alist',
for example using customize, or with something like:

  \(require 'org-e-groff)
  \(add-to-list 'org-export-groff-packages-alist '\(\"\" \"listings\"))
  \(add-to-list 'org-export-groff-packages-alist '\(\"\" \"color\"))

Alternatively,

  \(setq org-e-groff-listings 'minted)

causes source code to be exported using the minted package as
opposed to listings.  If you want to use minted, you need to add
the minted package to `org-export-groff-packages-alist', for
example using customize, or with

  \(require 'org-e-groff)
  \(add-to-list 'org-export-groff-packages-alist '\(\"\" \"minted\"))

In addition, it is necessary to install pygments
\(http://pygments.org), and to configure the variable
`org-e-groff-pdf-process' so that the -shell-escape option is
passed to pdfgroff."
  :group 'org-export-e-groff
  :type '(choice
		  (const :tag "Use listings" t)
		  (const :tag "Use minted" 'minted)
		  (const :tag "Export verbatim" nil)))

(defcustom org-e-groff-listings-langs
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
The value is the string that should be inserted as the language
parameter for the listings package.  If the mode name and the
listings name are the same, the language does not need an entry
in this list - but it does not hurt if it is present."
  :group 'org-export-e-groff
  :type '(repeat
		  (list
		   (symbol :tag "Major mode       ")
		   (string :tag "Listings language"))))

(defcustom org-e-groff-listings-options nil
  "Association list of options for the groff listings package.

These options are supplied as a comma-separated list to the
\\lstset command.  Each element of the association list should be
a list containing two strings: the name of the option, and the
value.  For example,

  (setq org-e-groff-listings-options
    '((\"basicstyle\" \"\\small\")
      (\"keywordstyle\" \"\\color{black}\\bfseries\\underbar\")))

will typeset the code in a small size font with underlined, bold
black keywords.

Note that the same options will be applied to blocks of all
languages."
  :group 'org-export-e-groff
  :type '(repeat
		  (list
		   (string :tag "Listings option name ")
		   (string :tag "Listings option value"))))

(defcustom org-e-groff-minted-langs
  '((emacs-lisp "common-lisp")
    (cc "c++")
    (cperl "perl")
    (shell-script "bash")
    (caml "ocaml"))
  "Alist mapping languages to their minted language counterpart.
The key is a symbol, the major mode symbol without the \"-mode\".
The value is the string that should be inserted as the language
parameter for the minted package.  If the mode name and the
listings name are the same, the language does not need an entry
in this list - but it does not hurt if it is present.

Note that minted uses all lower case for language identifiers,
and that the full list of language identifiers can be obtained
with:

  pygmentize -L lexers"
  :group 'org-export-e-groff
  :type '(repeat
		  (list
		   (symbol :tag "Major mode     ")
		   (string :tag "Minted language"))))

(defcustom org-e-groff-minted-options nil
  "Association list of options for the minted package.
  Minted is not supported in groff.

  \(setq org-e-groff-minted-options
    '\((\"bgcolor\" \"bg\") \(\"frame\" \"lines\")))

as the start of the minted environment. Note that the same
options will be applied to blocks of all languages."
  :group 'org-export-e-groff
  :type '(repeat
		  (list
		   (string :tag "Minted option name ")
		   (string :tag "Minted option value"))))

(defvar org-e-groff-custom-lang-environments nil
  "Alist mapping languages to language-specific Groff environments.

It is used during export of src blocks by the listings and minted
groff packages.  For example,

  \(setq org-e-groff-custom-lang-environments
     '\(\(python \"pythoncode\"\)\)\)

would have the effect that if org encounters begin_src python
during groff export it will output

  \\begin{pythoncode}
  <src block body>
  \\end{pythoncode}")


;;;; Plain text

(defcustom org-e-groff-quotes
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
  :group 'org-export-e-groff
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

(defcustom org-e-groff-pdf-process
  '("tbl %f | groff -mm | ps2pdf > %o/%f.pdf"
	"tbl %f | groff -mm | ps2pdf > %o/%f.pdf"
	"tbl %f | groff -mm | ps2pdf > %o/%f.pdf")

  "Commands to process a Groff file to a PDF file.
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
AUCTeX or the Emacs Groff mode.  This function should accept the
file name as its single argument."
  :group 'org-export-pdf
  :type '(choice
		  (repeat :tag "Shell command sequence"
				  (string :tag "Shell command"))
		  (const :tag "2 runs of pdfgroff"
				 ("tbl %f | groff -mm | ps2pdf > %o/%f.pdf"
				  "tbl %f | groff -mm | ps2pdf > %o/%f.pdf" ))
		  (const :tag "3 runs of pdfgroff"
				 ("tbl %f | groff -mm | ps2pdf > %o/%f.pdf"
				  "tbl %f | groff -mm | ps2pdf > %o/%f.pdf"
				  "tbl %f | groff -mm | ps2pdf > %o/%f.pdf"))
		  (function)))

(defcustom org-e-groff-logfiles-extensions
  '("aux" "idx" "log" "out" "toc" "nav" "snm" "vrb")
  "The list of file extensions to consider as Groff logfiles."
  :group 'org-export-e-groff
  :type '(repeat (string :tag "Extension")))

(defcustom org-e-groff-remove-logfiles t
  "Non-nil means remove the logfiles produced by PDF production.
These are the .aux, .log, .out, and .toc files."
  :group 'org-export-e-groff
  :type 'boolean)



;;; Internal Functions


(defun org-e-groff--caption/label-string (caption label info)
  "Return caption and label Groff string for floats.

CAPTION is a cons cell of secondary strings, the car being the
standard caption and the cdr its short form.  LABEL is a string
representing the label.  INFO is a plist holding contextual
information.

If there's no caption nor label, return the empty string.

For non-floats, see `org-e-groff--wrap-label'."
  (let ((label-str "" ))
    (cond
     ((and (not caption) (not label)) "")
     ((not caption) (format "\\fI%s\\fP\n" label))
     ;; Option caption format with short name.
     ((cdr caption)
      (format "\\fR%s\\fP - \\fI%s\\P - %s\n"
			  (org-export-data (cdr caption) info)
			  label-str
			  (org-export-data (car caption) info)))
     ;; Standard caption format.
     (t (format "\\fR%s - %s\\fP\n"
				label-str
				(org-export-data (car caption) info))))))


;; TODO

(defun org-e-groff--guess-babel-language (header info)
  "Set Babel's language according to LANGUAGE keyword.

HEADER is the Groff header string.  INFO is the plist used as
a communication channel.

Insertion of guessed language only happens when Babel package has
explicitly been loaded.  Then it is added to the rest of
package's options.

Return the new header."
  header )


(defun org-e-groff--guess-inputenc (header)
  "Set the coding system in inputenc to what the buffer is.
HEADER is the Groff header string.  Return the new header."
  header )

(defun org-e-groff--find-verb-separator (s)
  "Return a character not used in string S.
This is used to choose a separator for constructs like \\verb."
  (let ((ll "~,./?;':\"|!@#%^&-_=+abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ<>()[]{}"))
    (loop for c across ll
		  when (not (string-match (regexp-quote (char-to-string c)) s))
		  return (char-to-string c))))

(defun org-e-groff--make-option-string (options)
  "Return a comma separated string of keywords and values.
OPTIONS is an alist where the key is the options keyword as
a string, and the value a list containing the keyword value, or
nil."
  (mapconcat (lambda (pair)
			   (concat (first pair)
					   (when (> (length (second pair)) 0)
						 (concat " :" (second pair)))))
			 options
			 ","))

(defun org-e-groff--quotation-marks (text info)
  "Export quotation marks depending on language conventions.
TEXT is a string containing quotation marks to be replaced.  INFO
is a plist used as a communication channel."
  (mapc (lambda(l)
		  (let ((start 0))
			(while (setq start (string-match (car l) text start))
			  (let ((new-quote (concat (match-string 1 text) (cdr l))))
				(setq text (replace-match new-quote  t t text))))))
		(cdr (or (assoc (plist-get info :language) org-e-groff-quotes)
				 ;; Falls back on English.
				 (assoc "en" org-e-groff-quotes))))
  text)

(defun org-e-groff--wrap-label (element output)
  "Wrap label associated to ELEMENT around OUTPUT, if appropriate.
This function shouldn't be used for floats.  See
`org-e-groff--caption/label-string'."
  (let ((label (org-element-property :name element)))
    (if (or (not output) (not label) (string= output "") (string= label ""))
		output
      (concat (format "%s\n" label) output))))

;;; TODO

(defun org-e-groff--text-markup (text markup)
  "Format TEXT depending on MARKUP text markup.
See `org-e-groff-text-markup-alist' for details."
  (let ((fmt (cdr (assq markup org-e-groff-text-markup-alist))))
    (cond
     ;; No format string: Return raw text.
     ((not fmt) text)
     ;; Handle the `verb' special case: Find and appropriate separator
     ;; and use "\\verb" command.
	 ;; Handle the `protectedtexttt' special case: Protect some
     ;; special chars and use "\texttt{%s}" format string.
     ((eq 'protectedtexttt fmt)
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
		(setq text (concat rtn text)
			  fmt "\\fC%s\\fP")
		(while (string-match "--" text)
		  (setq text (replace-match "-{}-" t t text)))
		(format fmt text)))
     ;; Else use format string.
     (t (format fmt text)))))




;;; Template

(defun org-e-groff-template (contents info)
  "Return complete document string after Groff conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((title (org-export-data (plist-get info :title) info)))
    (concat
	 ;; 3. Title
	 (cond 
	  ((string= "" title)
	   (format ".TL\n%s\n" " ")
	   )
	  (t
	   (format ".TL\n%s\n" title)))
     ;; 4. Author.
     ;; In Groff, .AU *MUST* be placed after .TL
     (let ((author (and (plist-get info :with-author)
						(let ((auth (plist-get info :author)))
						  (and auth (org-export-data auth info)))))
		   (email (and (plist-get info :with-email)
					   (org-export-data (plist-get info :email) info))))
       (cond ((and author email (not (string= "" email)))
			  (format ".AU \"%s\" \"%s\"\n" author email))
			 (author (format ".AU \"%s\"\n" author))
			 (t ".AU \"\" \n")))
     ;; 5. Date.
     (let ((date (org-export-data (plist-get info :date) info)))
       (and date (format ".ND \"%s\"\n" date)))
     ;; 2. Document class and packages.

	 (let ((class (plist-get info :groff-class))
		   (class-options (plist-get info :groff-class-options)))
       (org-element-normalize-string
		(let* ((header (nth 1 (assoc class org-e-groff-classes)))
			   (document-class-string
				(and (stringp header)
					 (if class-options
						 (replace-regexp-in-string
						  "^[ \t]*\\.MT\\(\\[.*?\\]\\)"
						  class-options header t nil 1)
					   header))))
		  (when document-class-string
			(org-e-groff--guess-babel-language
			 header info)))))

     ;; 6. Document's body.

     contents

     ;; 8. Table of Content must be placed at the end being
     ;; that it gets collected from all the headers. 

     ".TC")))



;;; Transcode Functions

;;;; Babel Call
;;
;; Babel Calls are ignored.


;;;; Bold

(defun org-e-groff-bold (bold contents info)
  "Transcode BOLD from Org to Groff.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (org-e-groff--text-markup contents 'bold))


;;;; Center Block

(defun org-e-groff-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to Groff.
CONTENTS holds the contents of the center block.  INFO is a plist
holding contextual information."
  (org-e-groff--wrap-label
   center-block
   (format ".DS C \n%s\n.DE" contents)))


;;;; Clock

(defun org-e-groff-clock (clock contents info)
  "Transcode a CLOCK element from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (concat
   (format "\\fB%s\\fP " org-clock-string)
   (format org-e-groff-inactive-timestamp-format
		   (concat (org-translate-time (org-element-property :value clock))
				   (let ((time (org-element-property :time clock)))
					 (and time (format " (%s)" time)))))
   "\\\\"))


;;;; Code

(defun org-e-groff-code (code contents info)
  "Transcode a CODE object from Org to Groff.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-e-groff--text-markup (org-element-property :value code) 'code))


;;;; Comment
;;
;; Comments are ignored.


;;;; Comment Block
;;
;; Comment Blocks are ignored.


;;;; Drawer

(defun org-e-groff-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to Groff.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((name (org-element-property :drawer-name drawer))
		 (output (if (functionp org-e-groff-format-drawer-function)
					 (funcall org-e-groff-format-drawer-function
							  name contents)
				   ;; If there's no user defined function: simply
				   ;; display contents of the drawer.
				   contents)))
    (org-e-groff--wrap-label drawer output)))


;;;; Dynamic Block

(defun org-e-groff-dynamic-block (dynamic-block contents info)
  "Transcode a DYNAMIC-BLOCK element from Org to Groff.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See `org-export-data'."
  (org-e-groff--wrap-label dynamic-block contents))


;;;; Entity

(defun org-e-groff-entity (entity contents info)
  "Transcode an ENTITY object from Org to Groff.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (let ((ent (org-element-property :groff entity)))
    (if (org-element-property :groff-math-p entity) (format "$%s$" ent) ent)))


;;;; Example Block

(defun org-e-groff-example-block (example-block contents info)
  "Transcode an EXAMPLE-BLOCK element from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (org-e-groff--wrap-label
   example-block
   (format ".DS L\n%s\n.DE"
		   (org-export-format-code-default example-block info))))


;;;; Export Block

(defun org-e-groff-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "GROFF")
    (org-remove-indentation (org-element-property :value export-block))))


;;;; Export Snippet

(defun org-e-groff-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'e-groff)
    (org-element-property :value export-snippet)))


;;;; Fixed Width

(defun org-e-groff-fixed-width (fixed-width contents info)
  "Transcode a FIXED-WIDTH element from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-e-groff--wrap-label
   fixed-width
   (format "\\fC\n%s\\fP"
		   (org-remove-indentation
			(org-element-property :value fixed-width)))))


;;;; Footnote Definition
;;
;; Footnote Definitions are ignored.

;; 
;; Footnotes are handled automatically in GROFF. Although
;; manual references can be added, not really required.
;; 

(defun org-e-groff-footnote-reference (footnote-reference contents info)

;; Changing from info to footnote-reference
  (let ((definitions (org-export-collect-footnote-definitions
					  (plist-get info :parse-tree) info)) )
		;; Insert full links right inside the footnote definition
		;; as they have no chance to be inserted later.   
	(when definitions
	  (concat
	   (mapconcat
		(lambda (ref)
		  (let ((id (format "%s" (car ref )) )
				(ref-id  (format "%s" (plist-get (nth 1 footnote-reference) :label )))) 
			;; Distinguish between inline definitions and
			;; full-fledged definitions.
			(org-trim
			 (let ((def (nth 2 ref)))
			   (if (string= id ref-id)
				   (concat "\\*F\n.FS\n" (org-export-data def info) ".FE")
				 ""
				 )
			   ))))
		definitions "\n"))))
  )

;;;; Headline

(defun org-e-groff-headline (headline contents info)
  "Transcode an HEADLINE element from Org to Groff.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((class (plist-get info :groff-class))
		 (level (org-export-get-relative-level headline info))
		 (numberedp (org-export-numbered-headline-p headline info))
		 (class-sectionning (assoc class org-e-groff-classes))
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
		 (full-text (if (functionp org-e-groff-format-headline-function)
						;; User-defined formatting function.
						(funcall org-e-groff-format-headline-function
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
		  (if (functionp org-e-groff-format-headline-function)
			  ;; User-defined formatting function.
			  (funcall org-e-groff-format-headline-function
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
				(format "%s\n" (if numberedp 'enumerate 'itemize)))
			  ;; Itemize headline
			  ".LI\n" full-text "\n" headline-label pre-blanks contents)))
		;; If headline is not the last sibling simply return
		;; LOW-LEVEL-BODY.  Otherwise, also close the list, before any
		;; blank line.
		(if (not (org-export-last-sibling-p headline)) low-level-body
		  (replace-regexp-in-string
		   "[ \t\n]*\\'"
		   (concat ".LE" )
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

(defun org-e-groff-inline-src-block (inline-src-block contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to Groff.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((code (org-element-property :value inline-src-block))
		 (separator (org-e-groff--find-verb-separator code)))
    (cond
     ;; Do not use a special package: transcode it verbatim.
     (t
      (concat ".DS L" "\\fC" separator "\n" code "\n" separator "\\fP\n.DE\n"))
     ;; Use minted package.
	 )))


;;;; Inlinetask


(defun org-e-groff-inlinetask (inlinetask contents info)
  "Transcode an INLINETASK element from Org to Groff.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((title (org-export-data (org-element-property :title inlinetask) info))
		(todo (and (plist-get info :with-todo-keywords)
				   (let ((todo (org-element-property :todo-keyword inlinetask)))
					 (and todo (org-export-data todo info)))))
		(todo-type (org-element-property :todo-type inlinetask))
		(tags (and (plist-get info :with-tags)
				   (org-export-get-tags inlinetask info)))
		(priority (and (plist-get info :with-priority)
					   (org-element-property :priority inlinetask))))
    ;; If `org-e-groff-format-inlinetask-function' is provided, call it
    ;; with appropriate arguments.
    (if (functionp org-e-groff-format-inlinetask-function)
		(funcall org-e-groff-format-inlinetask-function
				 todo todo-type priority title tags contents)
      ;; Otherwise, use a default template.
      (org-e-groff--wrap-label
       inlinetask
       (let ((full-title
			  (concat
			   (when todo (format "\\fB%s\\fP " todo))
			   (when priority (format " [\\#%c] " priority))
			   title
			   (when tags (format " \\fC:%s:\\fP "
								  (mapconcat 'identity tags ":"))))))
		 (format (concat ".DS C\n"
						 "%s\n\n"
						 ".P"
						 "%s"
						 ".DE")
				 full-title contents))))))


;;;; Italic

(defun org-e-groff-italic (italic contents info)
  "Transcode ITALIC from Org to Groff.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (org-e-groff--text-markup contents 'italic))


;;;; Item


(defun org-e-groff-item (item contents info)
  "Transcode an ITEM element from Org to Groff.
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
				 ;; .AL
				 (concat ".BL\n"))))
;; 
;; Not in use for now
;; 
		 (checkbox (case (org-element-property :checkbox item)
					 (on "[+] ")
					 (off "[ ] ")
					 (trans "[-] ")))

		 (tag (let ((tag (org-element-property :tag item)))
				;; Check-boxes must belong to the tag.
				(and tag (format "[%s] "
								 (concat checkbox
										 (org-export-data tag info)))))))

    (concat counter ".LI\n" (or tag (concat " " checkbox))
			(org-trim contents)
			;; If there are footnotes references in tag, be sure to
			;; add their definition at the end of the item.  This
			;; workaround is necessary since "\footnote{}" command is
			;; not supported in tags.
			(and tag
				 (org-e-groff--delayed-footnotes-definitions
				  (org-element-property :tag item) info)))))


;;;; Keyword


(defun org-e-groff-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
		(value (org-element-property :value keyword)))
    (cond
     ((string= key "GROFF") value)
     ((string= key "INDEX") nil)
     ;; Invisible targets.
     ((string= key "TARGET") nil)
     ((string= key "TOC")
      (let ((value (downcase value)))
		(cond
		 ((string-match "\\<headlines\\>" value)
		  (let ((depth (or (and (string-match "[0-9]+" value)
								(string-to-number (match-string 0 value)))
						   (plist-get info :with-toc))))
			(concat
			 ".TC\n")))
		 ((string= "listings" value) (concat ".TC\n") )))))))


;;;; Groff Environment

(defun org-e-groff-groff-environment (groff-environment contents info)
  "Transcode a GROFF-ENVIRONMENT element from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((label (org-element-property :name groff-environment))
		(value (org-remove-indentation
				(org-element-property :value groff-environment))))
    (if (not (org-string-nw-p label)) value
      ;; Environment is labelled: label must be within the environment
      ;; (otherwise, a reference pointing to that element will count
      ;; the section instead).
      (with-temp-buffer
		(insert value)
		(goto-char (point-min))
		(forward-line)
		(insert (format ".P\n%s\n" label))
		(buffer-string)))))


;;;; Groff Fragment

(defun org-e-groff-groff-fragment (groff-fragment contents info)
  "Transcode a GROFF-FRAGMENT object from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-property :value groff-fragment))


;;;; Line Break

(defun org-e-groff-line-break (line-break contents info)
  "Transcode a LINE-BREAK object from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ".br\n")


;;;; Link
;;;;
;;;; Inline images just place a call to .PSPIC and load
;;;; a eps file.
;;;;


(defun org-e-groff-link--inline-image (link info)
  "Return Groff code for an inline image.
LINK is the link pointing to the inline image.  INFO is a plist
used as a communication channel."
  (let* ((parent (org-export-get-parent-element link))
		 (path (let ((raw-path (org-element-property :path link)))
				 (if (not (file-name-absolute-p raw-path)) raw-path
				   (expand-file-name raw-path))))

		 (caption (org-e-groff--caption/label-string
				   (org-element-property :caption parent)
				   (org-element-property :name parent)
				   info))
		 ;; Retrieve groff attributes from the element around.
		 (attr (let ((raw-attr
					  (mapconcat #'identity
								 (org-element-property :attr_groff parent)
								 " ")))
				 (unless (string= raw-attr "") raw-attr)))

		 ;; Attributes are going to be
		 ;; :position (left|center|right)
		 ;; :width 
		 ;; :height 

		 (disposition
		  (cond
		   ((and attr (string-match "\\<:position\\>" attr)) 'position)
		   ((and attr (string-match "\\<:width\\>" attr)) 'width)
		   ((and attr (string-match "\\<:height\\>" attr)) 'height) ))
		 (placement
		  (cond
		   ((eq 'xx 'right) "")
		   (t ""))) )
		 ;; Now clear ATTR from any special keyword and set a default
		 ;; value if nothing is left.
		 (setq attr
			   (if (not attr) ""
				 (org-trim
				  (replace-regexp-in-string
				   "\\(right\\|center\\|right\\)" "" attr))))
		 (setq attr (cond (t (or org-e-groff-image-default-option "")))) )
		 ;; Return proper string, depending on DISPOSITION.
		 ;; TODO Needs to be expanded with attributes
		 (case disposition
		   (t (format "\n.PSPIC \"%s\" " path))))

(defun org-e-groff-link (link desc info)
  "Transcode a LINK object from Org to Groff.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((type (org-element-property :type link))
		 (raw-path (org-element-property :path link))
		 ;; Ensure DESC really exists, or set it to nil.
		 (desc (and (not (string= desc "")) desc))
		 (imagep (org-export-inline-image-p
				  link org-e-groff-inline-image-rules))
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
     ;; Image file.
     (imagep (org-e-groff-link--inline-image link info))
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
		   (if desc (format "\\fIfile://%s\\fP \n %s" destination desc)
			 (format "\\fI file://%s \\fP" destination)))
		  ;; Fuzzy link points nowhere.
		  ('nil
		   (format org-e-groff-link-with-unknown-path-format
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
			   (format "\\fI%s\\fP - %s" path desc)))))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (format (org-export-get-coderef-format path desc)
			  (org-export-resolve-coderef path info)))
     ;; Link type is handled by a special function.
     ((functionp (setq protocol (nth 2 (assoc type org-link-protocols))))
      (funcall protocol (org-link-unescape path) desc 'groff))
     ;; External link with a description part.
     ((and path desc) (format "\\fI%s\\fP - %s" path desc))
     ;; External link without a description part.
     (path (format "\\fI%s\\fP" path))
     ;; No path, only description.  Try to do something useful.
     (t (format org-e-groff-link-with-unknown-path-format desc)))))


;;;; Macro

(defun org-e-groff-macro (macro contents info)
  "Transcode a MACRO element from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;; Use available tools.
  (org-export-expand-macro macro info))


;;;; Paragraph

(defun org-e-groff-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to Groff.
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
			  (setq fixed-paragraph (concat ".P\n" contents) ) )
			 ((eq parent-type 'footnote-definition)
			  (setq fixed-paragraph (concat "" contents) ))
			 (t (setq fixed-paragraph (concat "" contents)) ) 
			 )
	  fixed-paragraph)
  )
)


;;;; Plain List
;; TODO
(defun org-e-groff-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to Groff.
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
						  (org-element-property :attr_groff plain-list)
						  " "))
		 (groff-type (cond
					  ((eq type 'ordered) ".AL")
					  ((eq type 'unordered) ".BL")
					  ((eq type 'descriptive) ".VL"))))
    (org-e-groff--wrap-label
     plain-list
     (format "%s%s\n%s.LE"
			 groff-type
			 ;; Once special environment, if any, has been removed, the
			 ;; rest of the attributes will be optional arguments.
			 ;; They will be put inside square brackets if necessary.
			 (let ((opt (replace-regexp-in-string
						 (format " *%s *" paralist-regexp) "" attr)))
			   (cond ((string= opt "") "")
					 ((string-match "\\`\\[[^][]+\\]\\'" opt) opt)
					 (t (format "[%s]" opt))))
			 contents ))))


;;;; Plain Text

(defun org-e-groff-plain-text (text info)
  "Transcode a TEXT string from Org to Groff.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  ;; Protect %, #, &, $, ~, ^, _,  { and }.
  (while (string-match "\\([^\\]\\|^\\)\\([%$#&{}~^_]\\)" text)
    (setq text
		  (replace-match (format "%s" (match-string 2 text)) nil t text 2)))
  ;; Protect \
  (setq text (replace-regexp-in-string
			  "\\(?:[^\\]\\|^\\)\\(\\\\\\)\\(?:[^%$#&{}~^_\\]\\|$\\)"
			  "$\\" text nil t 1))

  ;; Handle quotation marks
  (setq text (org-e-groff--quotation-marks text info))
  ;; Convert special strings.
  (when (plist-get info :with-special-strings)
    (while (string-match (regexp-quote "...") text)
      (setq text (replace-match "..." nil t text))))
  ;; Handle break preservation if required.
  (when (plist-get info :preserve-breaks)
    (setq text (replace-regexp-in-string "\\(\\\\\\\\\\)?[ \t]*\n" " \\\\\\\\\n"
										 text)))
  ;; Return value.
  text)


;;;; Planning

(defun org-e-groff-planning (planning contents info)
  "Transcode a PLANNING element from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (concat
   (mapconcat
    'identity
    (delq nil
		  (list
		   (let ((closed (org-element-property :closed planning)))
			 (when closed
			   (concat
				(format "\\fC%s\\fP" org-closed-string)
				(format org-e-groff-inactive-timestamp-format
						(org-translate-time closed)))))
		   (let ((deadline (org-element-property :deadline planning)))
			 (when deadline
			   (concat
				(format "\\fB%s\\fP" org-deadline-string)
				(format org-e-groff-active-timestamp-format
						(org-translate-time deadline)))))
		   (let ((scheduled (org-element-property :scheduled planning)))
			 (when scheduled
			   (concat
				(format "\\fC%s\\fP" org-scheduled-string)
				(format org-e-groff-active-timestamp-format
						(org-translate-time scheduled)))))))
    " ")
   "\\\\"))


;;;; Property Drawer

(defun org-e-groff-property-drawer (property-drawer contents info)
  "Transcode a PROPERTY-DRAWER element from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  ;; The property drawer isn't exported but we want separating blank
  ;; lines nonetheless.
  "")


;;;; Quote Block

(defun org-e-groff-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to Groff.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (org-e-groff--wrap-label
   quote-block
   (format ".in 0.5i\n\\fI%s\\fP\n.in 0.5i" contents)))


;;;; Quote Section

(defun org-e-groff-quote-section (quote-section contents info)
  "Transcode a QUOTE-SECTION element from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((value (org-remove-indentation
				(org-element-property :value quote-section))))
    (when value (format ".DS L\n\\fC%s\\fP\n.DE\n" value))))


;;;; Radio Target

(defun org-e-groff-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to Groff.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (format "%s - %s"
		  (org-export-solidify-link-text
		   (org-element-property :value radio-target))
		  text))


;;;; Section

(defun org-e-groff-section (section contents info)
  "Transcode a SECTION element from Org to Groff.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  contents)


;;;; Special Block

(defun org-e-groff-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to Groff.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((type (downcase (org-element-property :type special-block))))
    (org-e-groff--wrap-label
     special-block
     (format ".DS %s\n%s\n.DE" type contents))))


;;;; Src Block

(defun org-e-groff-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to Groff.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((lang (org-element-property :language src-block))
		 (caption (org-element-property :caption src-block))
		 (label (org-element-property :name src-block))
		 (custom-env (and lang
						  (cadr (assq (intern lang)
									  org-e-groff-custom-lang-environments))))
		 (num-start (case (org-element-property :number-lines src-block)
					  (continued (org-export-get-loc src-block info))
					  (new 0)))
		 (retain-labels (org-element-property :retain-labels src-block)))
    (cond
     ;; Case 1.  No source fontification.
     ((not org-e-groff-listings)
      (let ((caption-str (org-e-groff--caption/label-string caption label info))
			(float-env (when caption "\\fC%s\\fP")))
		(format
		 (or float-env "%s")
		 (concat caption-str
				 (format "\\fC%s\\fP"
						 (org-export-format-code-default src-block info))))))
     ;; Case 2.  Custom environment.
     (custom-env (format ".DS L\n\\fC%s\\fP\n.DE"
						 custom-env
						 (src-block)
						 custom-env))


	 )))


;;;; Statistics Cookie

(defun org-e-groff-statistics-cookie (statistics-cookie contents info)
  "Transcode a STATISTICS-COOKIE object from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-property :value statistics-cookie))


;;;; Strike-Through

(defun org-e-groff-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to Groff.
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (org-e-groff--text-markup contents 'strike-through))

;;TODO - Currently stubbed to pass the value.
;; 


;;;; Subscript

(defun org-e-groff-subscript (subscript contents info)
  "Transcode a SUBSCRIPT object from Org to Groff.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format (if (= (length contents) 1) "$_%s$" "%s") contents))

;;TODO

;;;; Superscript

(defun org-e-groff-superscript (superscript contents info)
  "Transcode a SUPERSCRIPT object from Org to Groff.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format (if (= (length contents) 1) "$^%s$" "%s") contents))


;;;; Table
;;
;; `org-e-groff-table' is the entry point for table transcoding.  It
;; takes care of tables with a "verbatim" attribute.  Otherwise, it
;; delegates the job to either `org-e-groff-table--table.el-table' or
;; `org-e-groff-table--org-table' functions, depending of the type of
;; the table.
;;
;; `org-e-groff-table--align-string' is a subroutine used to build
;; alignment string for Org tables.

(defun org-e-groff-table (table contents info)
  "Transcode a TABLE element from Org to Groff.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (cond
   ;; Case 1: verbatim table.
   ((or org-e-groff-tables-verbatim
		(let ((attr (mapconcat 'identity
							   (org-element-property :attr_groff table)
							   " ")))
		  (and attr (string-match "\\<verbatim\\>" attr))))
    (format ".DS L\n\\fC%s\\fP\n.DE"
			;; Re-create table, without affiliated keywords.
			(org-trim
			 (org-element-interpret-data
			  `(table nil ,@(org-element-contents table))))))
   ;; Case 2: table.el table.  Convert it using appropriate tools.
   ((eq (org-element-property :type table) 'table.el)
    (org-e-groff-table--table.el-table table contents info))
   ;; Case 3: Standard table.
   (t (org-e-groff-table--org-table table contents info))))
;;
;; To Check
;; 

(defun org-e-groff-table--align-string (table info)
  "Return an appropriate Groff alignment string.
TABLE is the considered table.  INFO is a plist used as
a communication channel."
  (let ((attr (mapconcat 'identity
						 (org-element-property :attr_groff table)
						 " ")))
    (if (string-match "\\<align :\\(\\S-+\\)" attr) (match-string 1 attr)
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
		   (let ((borders (org-export-table-cell-borders cell info)))
			 ;; Check left border for the first cell only.
			 (when (and (memq 'left borders) (not alignment))
			   (push "|" alignment))
			 (push (case (org-export-table-cell-alignment cell info)
					 (left "left")
					 (right "right")
					 (center "center"))
				   alignment)
			 (when (memq 'right borders) (push "|" alignment))))
		 info)
		(apply 'concat (reverse alignment))))))

(defun org-e-groff-table--org-table (table contents info)
  "Return appropriate Groff code for an Org table.

TABLE is the table type element to transcode.  CONTENTS is its
contents, as a string.  INFO is a plist used as a communication
channel.

This function assumes TABLE has `org' as its `:type' attribute."
  (let* ((label (org-element-property :name table))
		 (caption (org-e-groff--caption/label-string
				   (org-element-property :caption table) label info))
		 (attr (mapconcat 'identity
						  (org-element-property :attr_groff table)
						  " "))
		 ;; Determine alignment string.
		 (alignment (org-e-groff-table--align-string table info))
		 ;; Determine environment for the table: longtable, tabular...
		 (table-env (cond
					 (t org-e-groff-default-table-environment)))
		 ;; If table is a float, determine environment: table, table*
		 ;; or sidewaystable.
		 (float-env nil)
		 ;; Extract others display options.
		 (width (and attr (string-match "\\<width :\\(\\S-+\\)" attr)
					 (org-match-string-no-properties 1 attr)))
		 (placement
		  (if (and attr (string-match "\\<placement :\\(\\S-+\\)" attr))
			  (org-match-string-no-properties 1 attr)
			(format "[%s]" org-e-groff-default-figure-position))))
    ;; Prepare the final format string for the table.

	;;(setq first-line (car contents))
	(setq lines (org-split-string contents "\n"))
	(when lines
	  (setq first-line (org-split-string (car lines) "\t")))
    (cond
     ;; Others.
     (lines (concat (when (not org-e-groff-tables-centered ) ".TS\nbox,center;\n")
				(when org-e-groff-tables-centered ".TS\nbox,center;\n")
				(format "%s.\n"
						(let ((final-line ""))
						  (dotimes (i (length first-line))
							(setq final-line (concat final-line "cb" " "))
							)
						  (setq final-line (concat final-line "\n"))
						  (dotimes (i (length first-line))
							(setq final-line (concat final-line "c" " ")))  final-line ))
				(format "%s.TE"
						(let ((final-line ""))
						  (dolist (line-item lines)
							(cond 
							 ((string-match "\\hline" line-item)
							  (setq final-line (concat final-line "_\n")))
							 (t	
							  (setq lines (org-split-string contents "\n"))

							  (setq final-line (concat final-line 
													   (car (org-split-string line-item "\\\\")) "\n"))
							  )
							 )
							
							)  final-line))

				)))))






(defun org-e-groff-table--table.el-table (table contents info)
  "Return appropriate Groff code for a table.el table.

TABLE is the table type element to transcode.  CONTENTS is its
contents, as a string.  INFO is a plist used as a communication
channel.

This function assumes TABLE has `table.el' as its `:type'
attribute."
  (require 'table)
  ;; Ensure "*org-export-table*" buffer is empty.
  (with-current-buffer (get-buffer-create "*org-export-table*")
    (erase-buffer))
  (let ((output (with-temp-buffer
				  (insert (org-element-property :value table))
				  (goto-char 1)
				  (re-search-forward "^[ \t]*|[^|]" nil t)
				  (table-generate-source 'groff "*org-export-table*")
				  (with-current-buffer "*org-export-table*"
					(org-trim (buffer-string))))))
    (kill-buffer (get-buffer "*org-export-table*"))
    ;; Remove left out comments.
    (while (string-match "^%.*\n" output)
      (setq output (replace-match "" t t output)))
    ;; When the "rmlines" attribute is provided, remove all hlines but
    ;; the the one separating heading from the table body.
    (let ((attr (mapconcat 'identity
						   (org-element-property :attr_groff table)
						   " ")))
      (when (and attr (string-match "\\<rmlines\\>" attr))
		(let ((n 0) (pos 0))
		  (while (and (< (length output) pos)
					  (setq pos (string-match "^\\\\hline\n?" output pos)))
			(incf n)
			(unless (= n 2)
			  (setq output (replace-match "" nil nil output)))))))
    (if (not org-e-groff-tables-centered) output
      (format ".DS C\n \\fC%s\\fP\n.DE\n" output))))



;;;; Table Cell

(defun org-e-groff-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to Groff
CONTENTS is the cell contents.  INFO is a plist used as
a communication channel."
  (concat (if (and contents
		   org-e-groff-table-scientific-notation
		   (string-match orgtbl-exp-regexp contents))
	      ;; Use appropriate format string for scientific
	      ;; notation.
	      (format org-e-groff-table-scientific-notation
		      (match-string 1 contents)
		      (match-string 2 contents))
	    contents)
	  (when (org-export-get-next-element table-cell) " \t ")))


;;;; Table Row

(defun org-e-groff-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to Groff
CONTENTS is the contents of the row.  INFO is a plist used as
a communication channel."
  ;; Rules are ignored since table separators are deduced from
  ;; borders of the current row.
  (when (eq (org-element-property :type table-row) 'standard)
    (let* ((attr (mapconcat 'identity
			    (org-element-property
			     :attr_groff (org-export-get-parent table-row))
			    " "))
	   (longtablep nil)
	   (booktabsp
	    (or (and attr (string-match "\\<booktabs :\\(yes\\|t\\)\\>" attr))
		org-e-groff-tables-booktabs))
	   ;; TABLE-ROW's borders are extracted from its first cell.
	   (borders
	    (org-export-table-cell-borders
	     (car (org-element-contents table-row)) info)))
      (concat
       ;; When BOOKTABS are activated enforce top-rule even when no
       ;; hline was specifically marked.
       (cond ((and booktabsp (memq 'top borders)) "\\toprule\n")
	     ((and (memq 'top borders) (memq 'above borders)) "\\hline\n"))
       contents "\\\\\n"
       (cond
	;; When BOOKTABS are activated enforce bottom rule even when
	;; no hline was specifically marked.
	((and booktabsp (memq 'bottom borders)) "box")
	((and (memq 'bottom borders) (memq 'below borders)) "_")
	((memq 'below borders) (if booktabsp "_" "_")))))))




;;;; Target

(defun org-e-groff-target (target contents info)
  "Transcode a TARGET object from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "\\fI%s\\fP"
		  (org-export-solidify-link-text (org-element-property :value target))))


;;;; Timestamp

(defun org-e-groff-timestamp (timestamp contents info)
  "Transcode a TIMESTAMP object from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((value (org-translate-time (org-element-property :value timestamp)))
		(type (org-element-property :type timestamp)))
    (cond ((memq type '(active active-range))
		   (format org-e-groff-active-timestamp-format value))
		  ((memq type '(inactive inactive-range))
		   (format org-e-groff-inactive-timestamp-format value))
		  (t (format org-e-groff-diary-timestamp-format value)))))


;;;; Underline

(defun org-e-groff-underline (underline contents info)
  "Transcode UNDERLINE from Org to Groff.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (org-e-groff--text-markup contents 'underline))


;;;; Verbatim

(defun org-e-groff-verbatim (verbatim contents info)
  "Transcode a VERBATIM object from Org to Groff.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-e-groff--text-markup (org-element-property :value verbatim) 'verbatim))


;;;; Verse Block

(defun org-e-groff-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to Groff.
CONTENTS is verse block contents. INFO is a plist holding
contextual information."
  content)



;;; Interactive functions

(defun org-e-groff-export-to-groff
  (&optional subtreep visible-only body-only ext-plist pub-dir)
  "Export current buffer to a Groff file.

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
  (let ((outfile (org-export-output-file-name ".groff" subtreep pub-dir)))
    (org-export-to-file
     'e-groff outfile subtreep visible-only body-only ext-plist)))

(defun org-e-groff-export-to-pdf
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
  (org-e-groff-compile
   (org-e-groff-export-to-groff
    subtreep visible-only body-only ext-plist pub-dir)))

(defun org-e-groff-compile (grofffile)
  "Compile a Groff file.

GROFFFILE is the name of the file being compiled.  Processing is
done through the command specified in `org-e-groff-pdf-process'.

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
		   ((functionp org-e-groff-pdf-process)
			(funcall org-e-groff-pdf-process (shell-quote-argument grofffile)))
		   ;; A list is provided: Replace %b, %f and %o with appropriate
		   ;; values in each command before applying it.  Output is
		   ;; redirected to "*Org PDF Groff Output*" buffer.
		   ((consp org-e-groff-pdf-process)
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
			   org-e-groff-pdf-process)
			  ;; Collect standard errors from output buffer.
			  (setq errors (org-e-groff-collect-errors outbuf))))
		   (t (error "No valid command to process to PDF")))
		  (let ((pdffile (concat base ".pdf")))
			;; Check for process failure.  Provide collected errors if
			;; possible.
			(if (not (file-exists-p pdffile))
				(error (concat (format "PDF file %s wasn't produced" pdffile)
							   (when errors (concat ": " errors))))
			  ;; Else remove log files, when specified, and signal end of
			  ;; process to user, along with any error encountered.
			  (when org-e-groff-remove-logfiles
				(dolist (ext org-e-groff-logfiles-extensions)
				  (let ((file (concat base "." ext)))
					(when (file-exists-p file) (delete-file file)))))
			  (message (concat "Process completed"
							   (if (not errors) "."
								 (concat " with errors: " errors)))))
			;; Return output file name.
			pdffile))
      (set-window-configuration wconfig))))

(defun org-e-groff-collect-errors (buffer)
  "Collect some kind of errors from \"groff\" output
BUFFER is the buffer containing output.
Return collected error types as a string, or nil if there was
none."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      ;; Find final run
	  nil )))


(provide 'org-e-groff)
;;; org-e-groff.el ends here