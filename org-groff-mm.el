;; org-groff-mm.el --- Groff Back-End For Org Export Engine

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
;; This library implements a Groff Memorandum Macro  back-end for
;; Org generic exporter.
;;
;; To test it, run
;;
;;   M-: (org-export-to-buffer 'groff-mm "*Test groff-mm*") RET
;;
;; in an org-mode buffer then switch to the buffer to see the Groff
;; export.  See contrib/lisp/org-export.el for more details on how
;; this exporter works.
;;
;; It introduces two new buffer keywords: "GROFF_CLASS" and
;; "GROFF_CLASS_OPTIONS".

;;; Code:

(eval-when-compile (require 'cl))

(defvar org-export-groff-default-packages-alist)
(defvar org-export-groff-packages-alist)

(require 'org-export)


;;; Define Back-End

(defvar org-groff-mm-translate-alist
  '((babel-call . org-groff-mm-babel-call)
    (bold . org-groff-mm-bold)
    (center-block . org-groff-mm-center-block)
    (clock . org-groff-mm-clock)
    (code . org-groff-mm-code)
    (comment . org-groff-mm-comment)
    (comment-block . org-groff-mm-comment-block)
    (drawer . org-groff-mm-drawer)
    (dynamic-block . org-groff-mm-dynamic-block)
    (entity . org-groff-mm-entity)
    (example-block . org-groff-mm-example-block)
    (export-block . org-groff-mm-export-block)
    (export-snippet . org-groff-mm-export-snippet)
    (fixed-width . org-groff-mm-fixed-width)
    (footnote-definition . org-groff-mm-footnote-definition)
    (footnote-reference . org-groff-mm-footnote-reference)
    (headline . org-groff-mm-headline)
    (horizontal-rule . org-groff-mm-horizontal-rule)
    (inline-babel-call . org-groff-mm-inline-babel-call)
    (inline-src-block . org-groff-mm-inline-src-block)
    (inlinetask . org-groff-mm-inlinetask)
    (italic . org-groff-mm-italic)
    (item . org-groff-mm-item)
    (keyword . org-groff-mm-keyword)
    (groff-environment . org-groff-mm-groff-environment)
    (groff-fragment . org-groff-mm-groff-fragment)
    (line-break . org-groff-mm-line-break)
    (link . org-groff-mm-link)
    (macro . org-groff-mm-macro)
    (paragraph . org-groff-mm-paragraph)
    (plain-list . org-groff-mm-plain-list)
    (plain-text . org-groff-mm-plain-text)
    (planning . org-groff-mm-planning)
    (property-drawer . org-groff-mm-property-drawer)
    (quote-block . org-groff-mm-quote-block)
    (quote-section . org-groff-mm-quote-section)
    (radio-target . org-groff-mm-radio-target)
    (section . org-groff-mm-section)
    (special-block . org-groff-mm-special-block)
    (src-block . org-groff-mm-src-block)
    (statistics-cookie . org-groff-mm-statistics-cookie)
    (strike-through . org-groff-mm-strike-through)
    (subscript . org-groff-mm-subscript)
    (superscript . org-groff-mm-superscript)
    (table . org-groff-mm-table)
    (table-cell . org-groff-mm-table-cell)
    (table-row . org-groff-mm-table-row)
    (target . org-groff-mm-target)
    (template . org-groff-mm-template)
    (timestamp . org-groff-mm-timestamp)
    (underline . org-groff-mm-underline)
    (verbatim . org-groff-mm-verbatim)
    (verse-block . org-groff-mm-verse-block))
  "Alist between element or object types and translators.")

(defconst org-groff-mm-options-alist
  '((:date "DATE" nil org-groff-mm-date-format t)
    (:groff-class "GROFF_CLASS" nil org-groff-mm-default-class t)
    (:groff-class-options "GROFF_CLASS_OPTIONS" nil nil t)
    (:groff-header-extra "GROFF_HEADER" nil nil newline))
"Alist between Groff export properties and ways to set them.
See `org-export-options-alist' for more information on the
structure of the values.")


;;; User Configurable Variables

(defgroup org-export-groff-mm nil
  "Options for exporting Org mode files to Groff."
  :tag "Org Export Groff"
  :group 'org-export)

;;; Preamble

(defcustom org-groff-mm-default-class "internal"
  "The default Groff class."
  :group 'org-export-groff-mm
  :type '(string :tag "Groff class"))

(defcustom org-groff-mm-classes
  '(("file" ".MT 1"
     (:heading 'default :type "memo" :last-section "toc"))
    ("internal" ".MT 0"
     (:heading 'default :type "memo" :last-section "toc"))
    ("programmer" ".MT 2"
     (:heading 'default :type "memo" :last-section "toc"))
    ("engineer" ".MT 3"
     (:heading 'default :type "memo" :last-section "toc"))
    ("external" ".MT 4"
     (:heading 'default :type "memo" :last-section "toc"))
    ("letter" ".MT 5"
     (:heading 'default :type "memo" :last-section "sign"))
    ("custom" ".so file"
     (:heading custom-function :type "custom" :last-section "toc"))
    ("dummy" ""
     (:heading 'default :type "memo"))
    ("ms" "ms"
     (:heading 'default :type "cover" :last-section "toc"))
    ("se_ms" "se_ms"
     (:heading 'default :type "cover" :last-section "toc"))
    ("block" "BL"
     (:heading 'default :type "letter" :last-section "sign"))
    ("semiblock" "SB"
     (:heading 'default :type "letter" :last-section "sign"))
    ("fullblock" "FB"
     (:heading 'default :type "letter" :last-section "sign"))
    ("simplified" "SP"
     (:heading 'default :type "letter" :last-section "sign"))
    ("none" "" (:heading 'default :type "custom")))

  ;; none means, no Cover or Memorandum Type and no calls to AU, AT, ND and TL
  ;; This is to facilitate the creation of custom pages.

  ;; dummy means, no Cover or Memorandum Type but calls to AU, AT, ND and TL
  ;; are made. This is to facilitate Abstract Insertion.

  "This list describes the attributes for the documents being created.
   It allows for the creation of new "
  :group 'org-export-groff-mm
  :type '(repeat
          (list (string :tag "Document Type")
                (string :tag "Header")
                (repeat :tag "Options" :inline t
                        (choice
                         (list :tag "Heading")
                         (function :tag "Hook computing sectioning"))))))


(defcustom org-groff-mm-date-format
  (format-time-string "%Y-%m-%d")
  "Format string for .ND "
  :group 'org-export-groff-mm
  :type 'boolean)

;;; Headline

(defconst org-groff-mm-special-tags
  '("FROM" "TO" "ABSTRACT" "APPENDIX" "BODY" "NS"))

(defcustom org-groff-mm-format-headline-function nil
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

\(defun org-groff-mm-format-headline (todo todo-type priority text tags)
  \"Default format function for an headline.\"
  \(concat (when todo
            \(format \"\\fB%s\\fP \" todo))
	  \(when priority
            \(format \"[\\#%c] \" priority))
	  text
	  \(when tags
            \(format \" %s \"
              \(mapconcat 'identity tags \":\"))))"
  :group 'org-export-groff-mm
  :type 'function)

;;; Timestamps

(defcustom org-groff-mm-active-timestamp-format "\\fI%s\\fP"
  "A printf format string to be applied to active timestamps."
  :group 'org-export-groff-mm
  :type 'string)

(defcustom org-groff-mm-inactive-timestamp-format "\\fI%s\\fP"
  "A printf format string to be applied to inactive timestamps."
  :group 'org-export-groff-mm
  :type 'string)

(defcustom org-groff-mm-diary-timestamp-format "\\fI%s\\fP"
  "A printf format string to be applied to diary timestamps."
  :group 'org-export-groff-mm
  :type 'string)

;;; Links

(defcustom org-groff-mm-inline-image-rules
  '(("file" . "\\.\\(pdf\\|ps\\|eps\\|pic\\)\\'")
    ("fuzzy" . "\\.\\(pdf\\|ps\\|eps\\|pic\\)\\'"))
  "Rules characterizing image files that can be inlined into Groff.

A rule consists in an association whose key is the type of link
to consider, and value is a regexp that will be matched against
link's path.

Note that, by default, the image extensions actually allowed
depend on the way the Groff file is processed.  When used with
pdfgroff, pdf, jpg and png images are OK.  When processing
through dvi to Postscript, only ps and eps are allowed.  The
default we use here encompasses both."
  :group 'org-export-groff-mm
  :type '(alist :key-type (string :tag "Type")
                :value-type (regexp :tag "Path")))

(defcustom org-groff-mm-link-with-unknown-path-format "\\fI%s\\fP"
  "Format string for links with unknown path type."
  :group 'org-export-groff
  :type 'string)

;;; Tables

(defcustom org-groff-mm-tables-centered t
  "When non-nil, tables are exported in a center environment."
  :group 'org-export-groff-mm
  :type 'boolean)

(defcustom org-groff-mm-tables-verbatim nil
  "When non-nil, tables are exported verbatim."
  :group 'org-export-groff-mm
  :type 'boolean)

(defcustom org-groff-mm-table-scientific-notation "%sE%s"
  "Format string to display numbers in scientific notation.
The format should have \"%s\" twice, for mantissa and exponent
\(i.e. \"%s\\\\times10^{%s}\").

When nil, no transformation is made."
  :group 'org-export-groff-mm
  :type '(choice
          (string :tag "Format string")
          (const :tag "No formatting")))

;;; Text markup

(defcustom org-groff-mm-text-markup-alist 
   '((bold . "\\fB%s\\fP")
    (code . "\\fC%s\\fP")
    (italic . "\\fI%s\\fP")
    (strike-through . "\\fC%s\\fP")  ; Strike through and underline
    (underline . "\\fI%s\\fP")       ; need to be revised.
    (verbatim .   "protectedtexttt"))
  "Alist of Groff expressions to convert text markup.

The key must be a symbol among `bold', `code', `italic',
`strike-through', `underline' and `verbatim'.  The value is
a formatting string to wrap fontified text with it.

If no association can be found for a given markup, text will be
returned as-is."
  :group 'org-export-groff-mm
  :type 'alist
  :options '(bold code italic strike-through underline verbatim))

;;; Drawers

(defcustom org-groff-mm-format-drawer-function nil
  "Function called to format a drawer in Groff code.

The function must accept two parameters:
  NAME      the drawer name, like \"LOGBOOK\"
  CONTENTS  the contents of the drawer.

The function should return the string to be exported.

For example, the variable could be set to the following function
in order to mimic default behaviour:

\(defun org-groff-mm-format-drawer-default \(name contents\)
  \"Format a drawer element for Groff export.\"
  contents\)"
  :group 'org-export-groff-mm
  :type 'function)

;;; Inlinetasks

(defcustom org-groff-mm-format-inlinetask-function nil
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

\(defun org-groff-mm-format-inlinetask \(todo type priority name tags contents\)
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
  :group 'org-export-groff-mm
  :type 'function)

;; Src blocks

(defcustom org-groff-mm-source-highlight nil
  "Use GNU source highlight to embellish source blocks "
  :group 'org-export-groff-mm
  :type 'boolean)

(defcustom org-groff-mm-source-highlight-langs
  '((emacs-lisp "lisp") (lisp "lisp") (clojure "lisp")
    (scheme "scheme")
    (c "c") (cc "cpp") (csharp "csharp") (d "d")
    (fortran "fortran") (cobol "cobol") (pascal "pascal")
    (ada "ada") (asm "asm")
    (perl "perl") (cperl "perl")
    (python "python") (ruby "ruby") (tcl "tcl") (lua "lua")
    (java "java") (javascript "javascript")
    (tex "latex")
    (shell-script "sh") (awk "awk") (diff "diff") (m4 "m4")
    (ocaml "caml") (caml "caml")
    (sql "sql") (sqlite "sql")
    (html "html") (css "css") (xml "xml")
    (bat "bat") (bison "bison") (clipper "clipper")
    (ldap "ldap") (opa "opa")
    (php "php") (postscript "postscript") (prolog "prolog")
    (properties "properties") (makefile "makefile")
    (tml "tml") (vala "vala") (vbscript "vbscript") (xorg "xorg"))
  "Alist mapping languages to their listing language counterpart.
The key is a symbol, the major mode symbol without the \"-mode\".
The value is the string that should be inserted as the language
parameter for the listings package.  If the mode name and the
listings name are the same, the language does not need an entry
in this list - but it does not hurt if it is present."
  :group 'org-export-groff-mm
  :type '(repeat
          (list
           (symbol :tag "Major mode       ")
           (string :tag "Listings language"))))

(defcustom org-groff-mm-source-highlight-options nil
  "Association list of options for the groff listings package.

These options are supplied as a comma-separated list to the
\\lstset command.  Each element of the association list should be
a list containing two strings: the name of the option, and the
value.  For example,

  (setq org-groff-mm-source-highlight-options
    '((\"basicstyle\" \"\\small\")
      (\"keywordstyle\" \"\\color{black}\\bfseries\\underbar\")))

will typeset the code in a small size font with underlined, bold
black keywords.

Note that the same options will be applied to blocks of all
languages."
  :group 'org-export-groff-mm
  :type '(repeat
          (list
           (string :tag "Listings option name ")
           (string :tag "Listings option value"))))

(defvar org-groff-mm-custom-lang-environments nil
  "Alist mapping languages to language-specific Groff environments.

It is used during export of src blocks by the listings and
groff packages.  For example,

  \(setq org-groff-mm-custom-lang-environments
     '\(\(python \"pythoncode\"\)\)\)

would have the effect that if org encounters begin_src python
during groff export it will use pythoncode as the source-highlight
language.")

;;; Plain text

(defcustom org-groff-mm-quotes
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
  :group 'org-export-groff-mm
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

(defcustom org-groff-mm-special-char
  '(("(c)" . "\\\\(co")
    ("(tm)" . "\\\\(tm")
    ("(rg)" . "\\\\(rg"))
  "CONS list in which the value of the car
  is replace on the value of the CDR. "
  :group 'org-export-groff-mm
  :type '(list
          (cons :tag "Character Subtitute"
                (string :tag "Original Character Group")
                (string :tag "Replacement Character"))))

;;; Compilation

(defcustom org-groff-mm-pdf-process
  '("pic %f | tbl | eqn | groff -mm | ps2pdf - > %b.pdf"
    "pic %f | tbl | eqn | groff -mm | ps2pdf - > %b.pdf"
    "pic %f | tbl | eqn | groff -mm | ps2pdf - > %b.pdf")

  "Commands to process a Groff file to a PDF file.
This is a list of strings, each of them will be given to the
shell as a command.  %f in the command will be replaced by the
full file name, %b by the file base name \(i.e. without
extension) and %o by the base directory of the file."
  :group 'org-export-pdf
  :type '(choice
          (repeat :tag "Shell command sequence"
                  (string :tag "Shell command"))
          (const :tag "2 runs of pdfgroff"
                 ("pic %f | tbl | eqn | groff -mm | ps2pdf - > %b.pdf"
                  "pic %f | tbl | eqn | groff -mm | ps2pdf - > %b.pdf"))
          (const :tag "3 runs of pdfgroff"
                 ("pic %f | tbl | eqn | groff -mm | ps2pdf - > %b.pdf"
                  "pic %f | tbl | eqn | groff -mm | ps2pdf - > %b.pdf"
                  "pic %f | tbl | eqn | groff -mm | ps2pdf - > %b.pdf"))
          (function)))

(defcustom org-groff-mm-logfiles-extensions
  '("aux" "idx" "log" "out" "toc" "nav" "snm" "vrb")
  "The list of file extensions to consider as Groff logfiles."
  :group 'org-export-groff-mm
  :type '(repeat (string :tag "Extension")))

(defcustom org-groff-mm-remove-logfiles t
  "Non-nil means remove the logfiles produced by PDF production.
These are the .aux, .log, .out, and .toc files."
  :group 'org-export-groff-mm
  :type 'boolean)

(defcustom org-groff-mm-organization "Org User"
  "Name of the organization used to populate the .AF command."
  :group 'org-export-groff-mm
  :type 'string)


;; Adding GROFF as a block parser to make sure that its contents
;; does not execute

(add-to-list 'org-element-block-name-alist
             '("GROFF" . org-element-export-block-parser))

(defvar org-groff-mm-registered-references nil)
(defvar org-groff-mm-special-content nil)



;;; Internal Functions

(defun org-groff-mm--caption/label-string (caption label info)
  "Return caption and label Groff string for floats.

CAPTION is a cons cell of secondary strings, the car being the
standard caption and the cdr its short form.  LABEL is a string
representing the label.  INFO is a plist holding contextual
information.

If there's no caption nor label, return the empty string.

For non-floats, see `org-groff-mm--wrap-label'."
  (let ((label-str ""))
    (cond
     ((and (not caption) (not label)) "")
     ((not caption) (format "\\fI%s\\fP" label))
     ;; Option caption format with short name.
     ((cdr caption)
      (format "%s\n.br\n%s - %s\n"
              (org-export-data (cdr caption) info)
              label-str
              (org-export-data (car caption) info)))
     ;; Standard caption format.
     (t (format "\\fR%s\\fP"
                (org-export-data (car caption) info))))))

(defun org-groff-mm--quotation-marks (text info)
  "Export quotation marks depending on language conventions.
TEXT is a string containing quotation marks to be replaced.  INFO
is a plist used as a communication channel."
  (mapc (lambda(l)
          (let ((start 0))
            (while (setq start (string-match (car l) text start))
              (let ((new-quote (concat (match-string 1 text) (cdr l))))
                (setq text (replace-match new-quote  t t text))))))
        (cdr (or (assoc (plist-get info :language) org-groff-mm-quotes)
                 ;; Falls back on English.
                 (assoc "en" org-groff-mm-quotes))))
  text)

(defun org-groff-mm--wrap-label (element output)
  "Wrap label associated to ELEMENT around OUTPUT, if appropriate.
This function shouldn't be used for floats.  See
`org-groff-mm--caption/label-string'."
  (let ((label (org-element-property :name element)))
    (if (or (not output) (not label) (string= output "") (string= label ""))
        output
      (concat (format "%s\n.br\n" label) output))))

(defun org-groff-mm--text-markup (text markup)
  "Format TEXT depending on MARKUP text markup.
See `org-groff-mm-text-markup-alist' for details."
  (let ((fmt (cdr (assq markup org-groff-mm-text-markup-alist))))
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
        (setq text (concat rtn text))
        (format "\\fC%s\\fP" text)))
     ;; Else use format string.
     (t (format fmt text)))))


(defun org-groff-mm--get-tagged-content  (tag info)
  (cdr  (assoc tag org-groff-mm-special-content)))

(defun org-groff-mm--mt-head (title contents attr info)
  (concat

   ;; 1. Insert Organization
   (let ((firm-option (plist-get attr :firm)))
     (cond
      ((stringp firm-option)
       (format ".AF \"%s\" \n" firm-option))
      (t (format ".AF \"%s\" \n" (or org-groff-mm-organization "")))))

   ;; 2. Title
   (let ((subtitle1 (plist-get attr :subtitle1))
         (subtitle2 (plist-get attr :subtitle2)))

     (cond
      ((string= "" title)
       (format ".TL \"%s\" \"%s\" \n%s\n"
               (or subtitle1 "")
               (or subtitle2 "") " "))

      ((not (or subtitle1 subtitle2))
       (format ".TL\n%s\n"
               (or title "")))
      (t
       (format ".TL \"%s\" \"%s \" \n%s\n"
               (or subtitle1 "")
               (or subtitle2 "") title))))

   ;; 3. Author.
   ;; In Groff, .AU *MUST* be placed after .TL
   ;; If From, populate with data from From else
   ;;
   (let ((author (and (plist-get info :with-author)
                      (let ((auth (plist-get info :author)))
                        (and auth (org-export-data auth info)))))
         (email (and (plist-get info :with-email)
                     (org-export-data (plist-get info :email) info)))
         (from-data  (org-groff-mm--get-tagged-content "FROM" info))

         (to-data  (org-groff-mm--get-tagged-content "TO" info)))

     (cond
      ((and author from-data)
       (let ((au-line
              (mapconcat
               (lambda (from-line)
                 (format " \"%s\" " from-line))
               (split-string
                (setq from-data
                      (replace-regexp-in-string "\\.P\n" "" from-data)) "\n") "")))

         (concat
          (format ".AU \"%s\" " author) au-line "\n")))

      ((and author email (not (string= "" email)))
       (format ".AU \"%s\" \"%s\"\n" author email))

      (author (format ".AU \"%s\"\n" author))

      (t ".AU \"\" \n")))


   ;; 4. Author Title, if present
   (let ((at-item (plist-get attr :author-title)))
     (if (and at-item (stringp at-item))
         (format ".AT \"%s\" \n" at-item)
       ""))

   ;; 5. Date.
   (let ((date (org-export-data (plist-get info :date) info)))
     (and date (format ".ND \"%s\"\n" date)))

   ;;
   ;; If Abstract, then Populate Abstract
   ;;

   (let ((abstract-data (org-groff-mm--get-tagged-content "ABSTRACT" info))
         (to-data (org-groff-mm--get-tagged-content "TO" info)))
     (cond
      (abstract-data
       (format ".AS\n%s\n.AE\n" abstract-data))
      (to-data
       (format ".AS\n%s\n.AE\n" to-data))))))

(defun org-groff-mm--letter-head (title contents attr info)
  (let ((author (and (plist-get info :with-author)
                     (let ((auth (plist-get info :author)))
                       (and auth (org-export-data auth info)))))
        (email (and (plist-get info :with-email)
                    (org-export-data (plist-get info :email) info)))
        (from-data  (org-groff-mm--get-tagged-content "FROM" info))
        (at-item (plist-get attr :author-title))
        (to-data  (org-groff-mm--get-tagged-content "TO" info)))


    ;; If FROM then get data from FROM
    (setq from-data
          (replace-regexp-in-string "\\.P\n" "" from-data))

    (setq to-data
          (replace-regexp-in-string "\\.P\n" "" to-data))

    (concat
     (cond
      (from-data
       (format ".WA \"%s\" \"%s\" \n%s\n.WE\n" author (or at-item "") from-data))
      ((and author email (not (string= "" email)))
       (format ".WA \"%s\"\n \"%s\"\n.WE\n" author email))
      (author (format ".WA \"%s\"\n.WE\n" author))
      (t ".WA \"\" \n.WE\n"))

     ;; If TO then get data from TO

     (when to-data
       (format ".IA \n%s\n.IE\n" to-data)))))


;;; Template

(defun org-groff-mm-template (contents info)
  "Return complete document string after Groff conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let* ((title (org-export-data (plist-get info :title) info))
         (attr (read
                (format "(%s)"
                        (mapconcat
                         #'identity
                         (list (plist-get info :groff-class-options))
                         " "))))
         (class (plist-get info :groff-class))
         (class-options (plist-get info :groff-class-options))
         (classes (assoc class org-groff-mm-classes))
         (classes-options (car (last classes)))
         (heading-option (plist-get classes-options :heading))
         (type-option (plist-get classes-options :type))
         (last-option (plist-get classes-options :last-section))
         (hyphenate (plist-get attr :hyphenate))
         (justify-right (plist-get attr :justify-right))

         (document-class-string
          (progn
            (org-element-normalize-string
             (let* ((header (nth 1 (assoc class org-groff-mm-classes)))
                    (document-class-item (if (stringp header) header "")))
               document-class-item)))))


    (concat
     (if justify-right
         (case justify-right
           ('yes ".SA 1 \n")
           ('no ".SA 0 \n")
           (t ""))
       "")

     (if hyphenate
         (case hyphenate
           ('yes ".nr Hy 1 \n")
           ('no ".nr Hy 0 \n")
           (t ""))
       "")

     (cond
      ((string= type-option "custom") "")

      ((and (stringp document-class-string)
            (string= type-option "cover"))

       (concat
        (format ".COVER %s\n" document-class-string)
        (org-groff-mm--mt-head title contents attr info)
        ".COVEND\n"))

      ((string= type-option "memo")
       (concat
        (org-groff-mm--mt-head title contents attr info)
        document-class-string))
      ((string= type-option "letter")
       (concat
        (org-groff-mm--letter-head title contents attr info)
        (let ((sa-item (plist-get attr :salutation))
              (cn-item (plist-get attr :confidential))
              (sj-item (plist-get attr :subject))
              (rn-item (plist-get attr :reference))
              (at-item (plist-get attr :attention)))

          (concat

           (if (stringp sa-item)
               (format ".LO SA \"%s\" \n"  sa-item)
             ".LO SA\n")

           (when cn-item
             (if (stringp cn-item)
                 (format ".LO CN \"%s\"\n" cn-item)
               ".LO CN\n"))

           (when (and at-item (stringp at-item))
             (format ".LO AT \"%s\" \n"  at-item))
           (when (and title rn-item)
             (format ".LO RN \"%s\"\n" title))

           (when (and sj-item (stringp sj-item))
             (format ".LO SJ \"%s\" \n"  sj-item))


           ".LT " document-class-string  "\n"))))

      (t ""))

     contents

     (cond
      ((string= last-option "toc")
       ".TC")
      ((string= last-option "sign")
       (let ((fc-item (plist-get attr :closing)))
         (concat (if (stringp fc-item)
                     (format ".FC \"%s\" \n" fc-item)
                   ".FC\n")
                 ".SG\n")))
      (t ""))

     (progn
       (mapconcat
        (lambda (item)
          (when (string= (car item) "NS")
            (replace-regexp-in-string
                    "\\.P\n" "" (cdr item))))
        (reverse org-groff-mm-special-content) "\n")))))



;;; Transcode Functions

;;; Babel Call
;;
;; Babel Calls are ignored.


;;; Bold

(defun org-groff-mm-bold (bold contents info)
  "Transcode BOLD from Org to Groff.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (org-groff-mm--text-markup contents 'bold))

;;; Center Block

(defun org-groff-mm-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to Groff.
CONTENTS holds the contents of the center block.  INFO is a plist
holding contextual information."
  (org-groff-mm--wrap-label
   center-block
   (format ".DS C \n%s\n.DE" contents)))

;;; Clock

(defun org-groff-mm-clock (clock contents info)
  "Transcode a CLOCK element from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (concat
   (format "\\fB%s\\fP " org-clock-string)
   (format org-groff-mm-inactive-timestamp-format
           (concat (org-translate-time (org-element-property :value clock))
                   (let ((time (org-element-property :time clock)))
                     (and time (format " (%s)" time)))))))

;;; Code

(defun org-groff-mm-code (code contents info)
  "Transcode a CODE object from Org to Groff.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-groff-mm--text-markup (org-element-property :value code) 'code))

;;; Comments and Comment Blocks are ignored.

;;; Drawer

(defun org-groff-mm-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to Groff.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((name (org-element-property :drawer-name drawer))
         (output (if (functionp org-groff-mm-format-drawer-function)
                     (funcall org-groff-mm-format-drawer-function
                              name contents)
                   ;; If there's no user defined function: simply
                   ;; display contents of the drawer.
                   contents)))
    (org-groff-mm--wrap-label drawer output)))

;;; Dynamic Block

(defun org-groff-mm-dynamic-block (dynamic-block contents info)
  "Transcode a DYNAMIC-BLOCK element from Org to Groff.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See `org-export-data'."
  (org-groff-mm--wrap-label dynamic-block contents))

;;; Entity

(defun org-groff-mm-entity (entity contents info)
  "Transcode an ENTITY object from Org to Groff.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (let ((ent (org-element-property :utf8 entity))) ent))

;;; Example Block

(defun org-groff-mm-example-block (example-block contents info)
  "Transcode an EXAMPLE-BLOCK element from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (org-groff-mm--wrap-label
   example-block
   (format ".DS L\n%s\n.DE"
           (org-export-format-code-default example-block info))))

;;; Export Block

(defun org-groff-mm-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "GROFF")
    (org-remove-indentation (org-element-property :value export-block))))

;;; Export Snippet

(defun org-groff-mm-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'groff-mm)
    (org-element-property :value export-snippet)))

;;; Fixed Width

(defun org-groff-mm-fixed-width (fixed-width contents info)
  "Transcode a FIXED-WIDTH element from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-groff-mm--wrap-label
   fixed-width
   (format "\\fC\n%s\\fP"
           (org-remove-indentation
            (org-element-property :value fixed-width)))))

;;; Footnote Definition
;;
;; Footnote Definitions are ignored.
;;
;; Footnotes are handled automatically in GROFF.  Although manual
;; references can be added, not really required.

(defun org-groff-mm-footnote-reference (footnote-reference contents info)
  ;; Changing from info to footnote-reference
  (let* ((raw (org-export-get-footnote-definition footnote-reference info))
		 (n (org-export-get-footnote-number footnote-reference info))
		 (data (org-trim (org-export-data raw info)))
         (ref-id (plist-get (nth 1 footnote-reference) :label)))
    ;; It is a reference
    (if (string-match "fn:rl" ref-id)
        (if (member ref-id org-groff-mm-registered-references)
            (format "\\*[%s]" ref-id)
          (progn
            (push ref-id org-groff-mm-registered-references)
            (format "\\*(Rf\n.RS \"%s\" \n%s\n.RF\n" ref-id  data)))
      ;; else it is a footnote
      (format "\\u\\s-2%s\\d\\s+2\n.FS %s\n%s\n.FE\n" n n data))))

;;; Headline

(defun org-groff-mm-headline (headline contents info)
  "Transcode an HEADLINE element from Org to Groff.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((class (plist-get info :groff-class))
         (level (org-export-get-relative-level headline info))
         (numberedp (org-export-numbered-headline-p headline info))
         ;; Section formatting will set two placeholders: one for the
         ;; title and the other for the contents.
         (classes (assoc class org-groff-mm-classes))
         (classes-options (car (last classes)))
         (heading-option (plist-get classes-options :heading))
         (section-fmt
          (progn
            (cond
             ((and (symbolp heading-option)
                   (fboundp heading-option))
              (funcall heading-option level numberedp))
             ((> level 7) nil)
             (t (if numberedp
                    (concat ".H " (number-to-string level) " \"%s\"\n%s")
                  ".HU \"%s\"\n%s")))))
         ;; End of section-fmt
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
         (full-text (if (functionp org-groff-mm-format-headline-function)
                        ;; User-defined formatting function.
                        (funcall org-groff-mm-format-headline-function
                                 todo todo-type priority text tags)
                      ;; Default formatting.
                      (concat
                       (when todo
                         (format "\\fB%s\\fP " todo))
                       (when priority (format " [\\#%c] " priority))
                       text
                       (when tags
                         (format " \\fC:%s:\\fP "
                                 (mapconcat 'identity tags ":"))))))
         (full-text-no-tag
          (if (functionp org-groff-mm-format-headline-function)
              ;; User-defined formatting function.
              (funcall org-groff-mm-format-headline-function
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
     ;; Case 1: Special Tag
     ((member (car  tags)  org-groff-mm-special-tags)
      (cond
       ((string= (car tags) "BODY") contents)

       ((string= (car tags) "NS")
        (progn
          (push (cons (car tags)
                      (format ".NS \"%s\" 1 \n%s"
                              (car (org-element-property :title headline))
                              (or contents " ")))
                org-groff-mm-special-content) nil))

       (t
        (progn
          (push (cons  (car tags) contents) org-groff-mm-special-content)
          nil))))

     ;; Case 2: This is a footnote section: ignore it.
     ((org-element-property :footnote-section-p headline) nil)

     ;; Case 3: This is a deep sub-tree: export it as a list item.
     ;;         Also export as items headlines for which no section
     ;;         format has been found.
     ((or (not section-fmt) (org-export-low-level-p headline info))
      ;; Build the real contents of the sub-tree.
      (let ((low-level-body
             (concat
              ;; If the headline is the first sibling, start a list.
              (when (org-export-first-sibling-p headline info)
                (format "%s\n" (if numberedp ".AL 1\n" ".DL \n")))
              ;; Itemize headline
              ".LI\n" full-text "\n" headline-label pre-blanks contents)))
        ;; If headline is not the last sibling simply return
        ;; LOW-LEVEL-BODY.  Otherwise, also close the list, before any
        ;; blank line.
        (if (not (org-export-last-sibling-p headline info)) low-level-body
          (replace-regexp-in-string
           "[ \t\n]*\\'"
           (concat "\n.LE")
           low-level-body))))

     ;; Case 4. Standard headline.  Export it as a section.
     (t
      (format section-fmt full-text
              (concat headline-label pre-blanks contents))))))

;;; Horizontal Rule
;; Not supported

;;; Inline Babel Call
;;
;; Inline Babel Calls are ignored.

;;; Inline Src Block

(defun org-groff-mm-inline-src-block (inline-src-block contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to Groff.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((code (org-element-property :value inline-src-block)))
    (cond
     (org-groff-mm-source-highlight
      (let* ((tmpdir (if (featurep 'xemacs)
                         temp-directory
                       temporary-file-directory))
             (in-file  (make-temp-name
                        (expand-file-name "srchilite" tmpdir)))
             (out-file (make-temp-name
                        (expand-file-name "reshilite" tmpdir)))
             (org-lang (org-element-property :language inline-src-block))
             (lst-lang (cadr (assq (intern org-lang)
                                   org-groff-mm-source-highlight-langs)))

             (cmd (concat (expand-file-name "source-highlight")
                          " -s " lst-lang
                          " -f groff_mm_color "
                          " -i " in-file
                          " -o " out-file)))
        (if lst-lang
            (let ((code-block ""))
              (with-temp-file in-file (insert code))
              (shell-command cmd)
              (setq code-block  (org-file-contents out-file))
              (delete-file in-file)
              (delete-file out-file)
              code-block)
          (format ".DS I\n\\fC\\m[black]%s\\m[]\\fP\n.DE\n"
                  code))))

     ;; Do not use a special package: transcode it verbatim.
     (t
      (concat ".DS I\n" "\\fC" code "\\fP\n.DE\n")))))

;;; Inlinetask

(defun org-groff-mm-inlinetask (inlinetask contents info)
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
    ;; If `org-groff-mm-format-inlinetask-function' is provided, call it
    ;; with appropriate arguments.
    (if (functionp org-groff-mm-format-inlinetask-function)
        (funcall org-groff-mm-format-inlinetask-function
                 todo todo-type priority title tags contents)
      ;; Otherwise, use a default template.
      (org-groff-mm--wrap-label
       inlinetask
       (let ((full-title
              (concat
               (when todo (format "\\fB%s\\fP " todo))
               (when priority (format " [\\#%c] " priority))
               title
               (when tags (format " \\fC:%s:\\fP "
                                  (mapconcat 'identity tags ":"))))))
         (format (concat "\n.DS I\n"
                         "%s\n"
                         ".sp"
                         "%s\n"
                         ".DE")
                 full-title contents))))))

;;; Italic

(defun org-groff-mm-italic (italic contents info)
  "Transcode ITALIC from Org to Groff.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (org-groff-mm--text-markup contents 'italic))

;;; Item

(defun org-groff-mm-item (item contents info)
  "Transcode an ITEM element from Org to Groff.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((bullet (org-element-property :bullet item))
	 (type (org-element-property
		:type (org-element-property :parent item)))
         (checkbox (case (org-element-property :checkbox item)
                     (on "\\o'\\(sq\\(mu'")
                     (off "\\(sq")
                     (trans "\\o'\\(sq\\(mi'")))
         (tag (let ((tag (org-element-property :tag item)))
                ;; Check-boxes must belong to the tag.
                (and tag (format "%s"
                                 (concat checkbox
                                         (org-export-data tag info)))))))

	(cond
	 ((or checkbox tag)
	  (concat ".LI ""\"" (or tag (concat "\\ " checkbox)) "\""
              "\n"
              (org-trim (or contents " "))))
     ((eq type 'ordered)
      (concat ".LI"
              "\n"
              (org-trim (or contents " "))))
     (t
      (let* ((bullet (org-trim bullet))
             (marker (cond  ((string= "-" bullet) "\\(em")
                            ((string= "*" bullet) "\\(bu")
                            (t "\\(dg"))))
        (concat ".LI " marker "\n"
                (org-trim (or contents " "))))))))

;;; Keyword

(defun org-groff-mm-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
        (value (org-element-property :value keyword)))
    (cond
     ((string= key "GROFF") value)
     (t nil))))

;;; Groff Environment

(defun org-groff-mm-groff-environment (groff-environment contents info)
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
        (insert (format "%s\n" label))
        (buffer-string)))))

;;; Groff Fragment

(defun org-groff-mm-groff-fragment (groff-fragment contents info)
  "Transcode a GROFF-FRAGMENT object from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-property :value groff-fragment))

;;; Line Break

(defun org-groff-mm-line-break (line-break contents info)
  "Transcode a LINE-BREAK object from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ".br\n")

;;; Link
;; Inline images just place a call to .PSPIC or .PS/.PE
;;  and load the graph.

(defun org-groff-mm-link--inline-image (link info)
  "Return Groff code for an inline image.
LINK is the link pointing to the inline image.  INFO is a plist
used as a communication channel."
  (let* ((parent (org-export-get-parent-element link))
         (path (let ((raw-path (org-element-property :path link)))
                 (if (not (file-name-absolute-p raw-path)) raw-path
                   (expand-file-name raw-path))))
         (attr (read (format "(%s)"
            (mapconcat
             #'identity
             (org-element-property :attr_groff parent)
             " "))))
         (placement
          (case (plist-get attr :position)
            ('center "")
            ('left "-L")
            ('right "-R")
            (t "")))
    (width  (or (plist-get attr :width) ""))
    (height (or (plist-get attr :height) ""))

    (disable-caption (plist-get attr :disable-caption))

    (caption
          (org-groff-mm--caption/label-string
           (org-element-property :caption parent)
           (org-element-property :name parent)
           info)))

    ;; Now clear ATTR from any special keyword and set a default value
    ;; if nothing is left.  Return proper string.

    (concat
     (cond
      ((string-match ".\.pic$" path)
       (format "\n.PS\ncopy \"%s\"\n.PE" path))
      (t (format "\n.DS L F\n.PSPIC %s \"%s\" %s %s\n.DE "
                 placement path width height)))
     (unless disable-caption (format "\n.FG \"%s\"" caption)))))

(defun org-groff-mm-link (link desc info)
  "Transcode a LINK object from Org to Groff.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."

  (let* ((type (org-element-property :type link))
         (raw-path (org-element-property :path link))
         ;; Ensure DESC really exists, or set it to nil.
         (desc (and (not (string= desc "")) desc))
         (imagep (org-export-inline-image-p
                  link org-groff-mm-inline-image-rules))
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
     (imagep (org-groff-mm-link--inline-image link info))
     ;; import groff files
     ((and (string= type "file")
           (string-match ".\.groff$" raw-path))
      (concat ".so " raw-path "\n"))
     ;; Radio link: transcode target's contents and use them as link's
     ;; description.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
        (when destination
          (format "\\fI [%s] \\fP"
                  (org-export-solidify-link-text path)))))

     ;; Links pointing to an headline: find destination and build
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
           (format org-groff-mm-link-with-unknown-path-format
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
     ;; External link with a description part.
     ((and path desc) (format "%s \\fBat\\fP \\fI%s\\fP" path desc))
     ;; External link without a description part.
     (path (format "\\fI%s\\fP" path))
     ;; No path, only description.  Try to do something useful.
     (t (format org-groff-mm-link-with-unknown-path-format desc)))))

;;; Macro

(defun org-groff-mm-macro (macro contents info)
  "Transcode a MACRO element from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;; Use available tools.
  (org-export-expand-macro macro info))

;;; Paragraph

(defun org-groff-mm-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to Groff.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (let ((parent (plist-get (nth 1 paragraph) :parent)))
    (when parent
      (let* ((parent-type (car parent))
             (fixed-paragraph "")
             (class (plist-get info :groff-class))
             (class-options (plist-get info :groff-class-options))
             (classes (assoc class org-groff-mm-classes))
             (classes-options (car (last classes)))
             (paragraph-option (plist-get classes-options :paragraph)))
        (cond
         ((and (symbolp paragraph-option)
               (fboundp paragraph-option))
          (funcall paragraph-option parent-type parent contents))
         ((and (eq parent-type 'item)
               (plist-get (nth 1 parent) :bullet))
          (setq fixed-paragraph (concat "" contents)))
         ((eq parent-type 'section)
          (setq fixed-paragraph (concat ".P\n" contents)))
         ((eq parent-type 'footnote-definition)
          (setq fixed-paragraph (concat "" contents)))
         (t (setq fixed-paragraph (concat "" contents))))
        fixed-paragraph))))

;;; Plain List

(defun org-groff-mm-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to Groff.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* ((type (org-element-property :type plain-list))
         (attr (mapconcat #'identity
                          (org-element-property :attr_groff plain-list)
                          " "))
         (groff-type (cond
                      ((eq type 'ordered) ".AL")
                      ((eq type 'unordered) ".BL")
                      ((eq type 'descriptive) ".VL 2.0i"))))
    (org-groff-mm--wrap-label
     plain-list
     (format "%s\n%s\n.LE" groff-type contents))))

;;; Plain Text

(defun org-groff-mm-plain-text (text info)
  "Transcode a TEXT string from Org to Groff.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  ;; Protect
  (setq text (replace-regexp-in-string
              "\\(?:[^\\]\\|^\\)\\(\\\\\\)\\(?:[^%$#&{}~^_\\]\\|$\\)"
              "$\\" text nil t 1))
  ;; Handle quotation marks
  (setq text (org-groff-mm--quotation-marks text info))
  ;; Handle Special Characters
  (if org-groff-mm-special-char
      (dolist (special-char-list org-groff-mm-special-char)
        (setq text
              (replace-regexp-in-string (car special-char-list)
                                        (cdr special-char-list) text))))
  ;; Handle break preservation if required.
  (when (plist-get info :preserve-breaks)
    (setq text (replace-regexp-in-string
		"\\(\\\\\\\\\\)?[ \t]*\n" " \\\\\\\\\n" text)))
  ;; Return value.
  text)

;;; Planning

(defun org-groff-mm-planning (planning contents info)
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
                (format "\\fR %s \\fP" org-closed-string)
                (format org-groff-mm-inactive-timestamp-format
                        (org-translate-time closed)))))
           (let ((deadline (org-element-property :deadline planning)))
             (when deadline
               (concat
                (format "\\fB %s \\fP" org-deadline-string)
                (format org-groff-mm-active-timestamp-format
                        (org-translate-time deadline)))))
           (let ((scheduled (org-element-property :scheduled planning)))
             (when scheduled
               (concat
                (format "\\fR %s \\fP" org-scheduled-string)
                (format org-groff-mm-active-timestamp-format
                        (org-translate-time scheduled)))))))
    "")
   ""))

;;; Property Drawer

(defun org-groff-mm-property-drawer (property-drawer contents info)
  "Transcode a PROPERTY-DRAWER element from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  ;; The property drawer isn't exported but we want separating blank
  ;; lines nonetheless.
  "")

;;; Quote Block

(defun org-groff-mm-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to Groff.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (org-groff-mm--wrap-label
   quote-block
   (format ".DS I\n.I\n%s\n.R\n.DE" contents)))

;;; Quote Section

(defun org-groff-mm-quote-section (quote-section contents info)
  "Transcode a QUOTE-SECTION element from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((value (org-remove-indentation
                (org-element-property :value quote-section))))
    (when value (format ".DS L\n\\fI%s\\fP\n.DE\n" value))))

;;; Radio Target

(defun org-groff-mm-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to Groff.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (format "%s - %s"
          (org-export-solidify-link-text
           (org-element-property :value radio-target))
          text))

;;; Section

(defun org-groff-mm-section (section contents info)
  "Transcode a SECTION element from Org to Groff.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  contents)

;;; Special Block

(defun org-groff-mm-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to Groff.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((type (downcase (org-element-property :type special-block))))
    (org-groff-mm--wrap-label
     special-block
     (format "%s\n" contents))))

;;; Src Block

(defun org-groff-mm-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to Groff.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((lang (org-element-property :language src-block))
         (caption (org-element-property :caption src-block))
         (label (org-element-property :name src-block))
         (code (org-element-property :value src-block))
         (custom-env (and lang
                          (cadr (assq (intern lang)
                                      org-groff-mm-custom-lang-environments))))
         (num-start (case (org-element-property :number-lines src-block)
                      (continued (org-export-get-loc src-block info))
                      (new 0)))
         (retain-labels (org-element-property :retain-labels src-block))
         (attr
          (read (format "(%s)"
                   (mapconcat #'identity
                              (org-element-property :attr_groff src-block)
                              " "))))
         (disable-caption (plist-get attr :disable-caption)))

    (cond
     ;; Case 1.  No source fontification.
     ((not org-groff-mm-source-highlight)
      (let ((caption-str (org-groff-mm--caption/label-string caption label info)))
        (concat
         (format ".DS I\n\\fC%s\\fP\n.DE\n"
                 (org-export-format-code-default src-block info))
         (unless  disable-caption (format ".EX \"%s\" "  caption-str)))))

     ;; Case 2.  Source fontification.
     (org-groff-mm-source-highlight
       (let* ((tmpdir (if (featurep 'xemacs)
                          temp-directory
                        temporary-file-directory))
              (caption-str (org-groff-mm--caption/label-string caption label info))
              (in-file  (make-temp-name
                         (expand-file-name "srchilite" tmpdir)))
              (out-file (make-temp-name
                         (expand-file-name "reshilite" tmpdir)))

              (org-lang (org-element-property :language src-block))
              (lst-lang (cadr (assq (intern org-lang)
                                    org-groff-mm-source-highlight-langs)))

              (cmd (concat "source-highlight"
                           " -s " lst-lang
                           " -f groff_mm_color "
                           " -i " in-file
                           " -o " out-file)))

         (concat
          (if lst-lang
              (let ((code-block ""))
                (with-temp-file in-file (insert code))
                (shell-command cmd)
                (setq code-block  (org-file-contents out-file))
                (delete-file in-file)
                (delete-file out-file)
                (format "%s\n"  code-block))
            (format ".DS I\n\\fC\\m[black]%s\\m[]\\fP\n.DE\n"
                    code))
          (unless disable-caption (format ".EX \"%s\" " caption-str))))))))


;;; Statistics Cookie

(defun org-groff-mm-statistics-cookie (statistics-cookie contents info)
  "Transcode a STATISTICS-COOKIE object from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-property :value statistics-cookie))


;;; Strike-Through

(defun org-groff-mm-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to Groff.
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (org-groff-mm--text-markup contents 'strike-through))

;;; Subscript

(defun org-groff-mm-subscript (subscript contents info)
  "Transcode a SUBSCRIPT object from Org to Groff.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format  "\\d\\s-2%s\\s+2\\u" contents))

;;; Superscript "^_%s$

(defun org-groff-mm-superscript (superscript contents info)
  "Transcode a SUPERSCRIPT object from Org to Groff.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format  "\\u\\s-2%s\\s+2\\d" contents))


;;; Table
;;
;; `org-groff-mm-table' is the entry point for table transcoding.  It
;; takes care of tables with a "verbatim" attribute.  Otherwise, it
;; delegates the job to  `org-groff-mm-table--org-table' function,
;; depending of the type of the table.
;;
;; `org-groff-mm-table--align-string' is a subroutine used to build
;; alignment string for Org tables.

(defun org-groff-mm-table (table contents info)
  "Transcode a TABLE element from Org to Groff.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (cond
   ;; Case 1: verbatim table.
   ((or org-groff-mm-tables-verbatim
        (let ((attr (read (format "(%s)"
                 (mapconcat
                  #'identity
                                   (org-element-property :attr_groff table) " ")))))
          (and attr (plist-get attr :verbatim))))

    (format ".DS L\n\\fC%s\\fP\n.DE"
            ;; Re-create table, without affiliated keywords.
            (org-trim
             (org-element-interpret-data
              `(table nil ,@(org-element-contents table))))))

   ;; Case 2: Standard table.
   (t (org-groff-mm-table--org-table table contents info))))

(defun org-groff-mm-table--align-string (divider table info)
  "Return an appropriate Groff alignment string.
TABLE is the considered table.  INFO is a plist used as
a communication channel."
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
                (width (if raw-width (format "w(%dc)"
                                             (if (< width-cm 1) 1 width-cm)) "")))
           ;; Check left border for the first cell only.
         ;; Alignment is nil on assignment

           (when (and (memq 'left borders) (not alignment))
             (push "|" alignment))
           (push
                (case (org-export-table-cell-alignment cell info)
                  (left (concat "l" width divider))
                  (right (concat "r" width divider))
                  (center (concat "c" width divider)))
            alignment)
           (when (memq 'right borders) (push "|" alignment))))
       info)
    (apply 'concat (reverse alignment))))

(defun org-groff-mm-table--org-table (table contents info)
  "Return appropriate Groff code for an Org table.

TABLE is the table type element to transcode.  CONTENTS is its
contents, as a string.  INFO is a plist used as a communication
channel.

This function assumes TABLE has `org' as its `:type' attribute."
  (let* ((label (org-element-property :name table))
         (caption (org-groff-mm--caption/label-string
                   (org-element-property :caption table) label info))
         (attr (read (format "(%s)"
                             (mapconcat #'identity
             (org-element-property :attr_groff table)
             " "))))
         (divider (if (plist-get attr :divider) "|" " "))

         ;; Determine alignment string.
         (alignment (org-groff-mm-table--align-string divider table info))

         ;; Extract others display options.

         (lines (org-split-string contents "\n"))

         (attr-list
          (let (result-list)
            (dolist (attr-item
                     (list
                      (if (plist-get attr :expand)
                          "expand" nil)

                      (case (plist-get attr :placement)
                        ('center "center")
                        ('left nil)
                        (t
                         (if org-groff-mm-tables-centered
                             "center" "")))

                      (case (plist-get attr :boxtype)
                        ('box "box")
                        ('doublebox "doublebox")
                        ('allbox "allbox")
                        ('none nil)
                        (t "box"))))

              (if (not (null attr-item))
                  (add-to-list 'result-list attr-item)))
            result-list))

         (title-line  (plist-get attr :title-line))
         (disable-caption (plist-get attr :disable-caption))
         (long-cells (plist-get attr :long-cells))

         (table-format
          (concat
           (format "%s"
                   (or (car attr-list) ""))
           (or
            (let (output-list)
                           (when (cdr attr-list)
                             (dolist (attr-item (cdr attr-list))
                  (setq output-list (concat output-list
					    (format ",%s" attr-item)))))
              output-list) "")))
         (first-line
          (when lines (org-split-string (car lines) "\t"))))
    ;; Prepare the final format string for the table.


    (cond
     ;; Others.
     (lines
      (concat ".TS\n " table-format ";\n"
                    (format "%s.\n"
                            (let ((final-line ""))
                              (when title-line
                                (dotimes (i (length first-line))
                                  (setq final-line (concat final-line "cb" divider))))

                              (setq final-line (concat final-line "\n"))

                              (if alignment
                                  (setq final-line (concat final-line alignment))
                                (dotimes (i (length first-line))
                                  (setq final-line (concat final-line "c" divider))))
                              final-line))

                    (format "%s\n.TE\n"
                            (let ((final-line "")
                                  (long-line "")
                                  (lines (org-split-string contents "\n")))

                              (dolist (line-item lines)
                                (setq long-line "")

                                (if long-cells
                                    (progn
                                      (if (string= line-item "_")
                                          (setq long-line (format "%s\n" line-item))
                                        ;; else string =
                                        (let ((cell-item-list (org-split-string line-item "\t")))
                                          (dolist (cell-item cell-item-list)

                                            (cond  ((eq cell-item (car (last cell-item-list)))
                                                    (setq long-line (concat long-line
                                                                            (format "T{\n%s\nT}\t\n"  cell-item))))
                                                   (t
                                                    (setq long-line (concat long-line
                                                                            (format "T{\n%s\nT}\t"  cell-item))))))
                                        long-line))
                                     ;; else long cells
                                  (setq final-line (concat final-line long-line)))

                                  (setq final-line (concat final-line line-item "\n"))))
                              final-line))

                    (if (not disable-caption)
                        (format ".TB \"%s\""
                                caption) ""))))))

;;; Table Cell

(defun org-groff-mm-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to Groff
CONTENTS is the cell contents.  INFO is a plist used as
a communication channel."
  (progn
    (concat (if (and contents
                     org-groff-mm-table-scientific-notation
                     (string-match orgtbl-exp-regexp contents))
                ;; Use appropriate format string for scientific
                ;; notation.
                (format org-groff-mm-table-scientific-notation
                        (match-string 1 contents)
                        (match-string 2 contents))
              contents)
            (when (org-export-get-next-element table-cell info) "\t"))))


;;; Table Row

(defun org-groff-mm-table-row (table-row contents info)
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
           ;; TABLE-ROW's borders are extracted from its first cell.
           (borders
            (org-export-table-cell-borders
             (car (org-element-contents table-row)) info)))
      (concat
       ;; Mark horizontal lines
       (cond  ((and (memq 'top borders) (memq 'above borders)) "_\n"))
       contents
       (cond
        ;; When BOOKTABS are activated enforce bottom rule even when
        ;; no hline was specifically marked.
        ((and (memq 'bottom borders) (memq 'below borders)) "\n_")
        ((memq 'below borders) "\n_"))))))

;;; Target

(defun org-groff-mm-target (target contents info)
  "Transcode a TARGET object from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "\\fI%s\\fP"
          (org-export-solidify-link-text (org-element-property :value target))))

;;; Timestamp

(defun org-groff-mm-timestamp (timestamp contents info)
  "Transcode a TIMESTAMP object from Org to Groff.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((value (org-translate-time (org-element-property :value timestamp)))
        (type (org-element-property :type timestamp)))
    (cond ((memq type '(active active-range))
           (format org-groff-mm-active-timestamp-format value))
          ((memq type '(inactive inactive-range))
           (format org-groff-mm-inactive-timestamp-format value))
          (t (format org-groff-mm-diary-timestamp-format value)))))

;;; Underline

(defun org-groff-mm-underline (underline contents info)
  "Transcode UNDERLINE from Org to Groff.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (org-groff-mm--text-markup contents 'underline))

;;; Verbatim

(defun org-groff-mm-verbatim (verbatim contents info)
  "Transcode a VERBATIM object from Org to Groff.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-groff-mm--text-markup (org-element-property :value verbatim) 'verbatim))

;;; Verse Block

(defun org-groff-mm-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to Groff.
CONTENTS is verse block contents. INFO is a plist holding
contextual information."
  (format ".DS C\n.ft HI\n%s\n.ft\n.DE" contents))


;;; Interactive functions

(defun org-groff-mm-export-to-groff
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

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return output file's name."
  (interactive)
  (setq org-groff-mm-registered-references nil)
  (setq org-groff-mm-special-content nil)
  (let ((outfile (org-export-output-file-name ".groff" subtreep pub-dir)))
    (org-export-to-file
     'groff-mm outfile subtreep visible-only body-only ext-plist)))

(defun org-groff-mm-export-to-pdf
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

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return PDF file's name."
  (interactive)
  (org-groff-mm-compile
   (org-groff-mm-export-to-groff
    subtreep visible-only body-only ext-plist pub-dir)))

(defun org-groff-mm-compile (grofffile)
  "Compile a Groff file.

GROFFFILE is the name of the file being compiled.  Processing is
done through the command specified in `org-groff-mm-pdf-process'.

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
           ((functionp org-groff-mm-pdf-process)
            (funcall org-groff-mm-pdf-process (shell-quote-argument grofffile)))
           ;; A list is provided: Replace %b, %f and %o with appropriate
           ;; values in each command before applying it.  Output is
           ;; redirected to "*Org PDF Groff Output*" buffer.
           ((consp org-groff-mm-pdf-process)
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
                     "%o" (shell-quote-argument out-dir) command t t)
		    t t) t t)
                  outbuf))
               org-groff-mm-pdf-process)
              ;; Collect standard errors from output buffer.
              (setq errors (org-groff-mm-collect-errors outbuf))))
           (t (error "No valid command to process to PDF")))
          (let ((pdffile (concat base ".pdf")))
            ;; Check for process failure.  Provide collected errors if
            ;; possible.
            (if (not (file-exists-p pdffile))
                (error (concat (format "PDF file %s wasn't produced" pdffile)
                               (when errors (concat ": " errors))))
              ;; Else remove log files, when specified, and signal end of
              ;; process to user, along with any error encountered.
              (when org-groff-mm-remove-logfiles
                (dolist (ext org-groff-mm-logfiles-extensions)
                  (let ((file (concat base "." ext)))
                    (when (file-exists-p file) (delete-file file)))))
              (message (concat "Process completed"
                               (if (not errors) "."
                                 (concat " with errors: " errors)))))
            ;; Return output file name.
            pdffile))
      (set-window-configuration wconfig))))

(defun org-groff-mm-collect-errors (buffer)
  "Collect some kind of errors from \"groff\" output
BUFFER is the buffer containing output.
Return collected error types as a string, or nil if there was
none."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      ;; Find final run
      nil)))


(provide 'org-groff-mm)
;;; org-groff-mm.el ends here
