;; org-groff-mom.el --- Mom Back-End For Org Export Engine

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
;; This library implements a Mom Memorandum Macro  back-end for
;; Org generic exporter.
;;
;; To test it, run
;;
;;   M-: (org-export-to-buffer 'groff-mom "*Test groff-mom*") RET
;;
;; in an org-mode buffer then switch to the buffer to see the Mom
;; export.  See contrib/lisp/org-export.el for more details on how
;; this exporter works.
;;
;; It introduces two new buffer keywords: "MOM_CLASS" and
;; "MOM_CLASS_OPTIONS".

;;; Code:

(eval-when-compile (require 'cl))

(defvar org-export-mom-default-packages-alist)
(defvar org-export-mom-packages-alist)

(require 'org-export)


;;; Define Back-End

(defvar org-groff-mom-translate-alist
  '((babel-call . org-groff-mom-babel-call)
    (bold . org-groff-mom-bold)
    (center-block . org-groff-mom-center-block)
    (clock . org-groff-mom-clock)
    (code . org-groff-mom-code)
    (comment . org-groff-mom-comment)
    (comment-block . org-groff-mom-comment-block)
    (drawer . org-groff-mom-drawer)
    (dynamic-block . org-groff-mom-dynamic-block)
    (entity . org-groff-mom-entity)
    (example-block . org-groff-mom-example-block)
    (export-block . org-groff-mom-export-block)
    (export-snippet . org-groff-mom-export-snippet)
    (fixed-width . org-groff-mom-fixed-width)
    (footnote-definition . org-groff-mom-footnote-definition)
    (footnote-reference . org-groff-mom-footnote-reference)
    (headline . org-groff-mom-headline)
    (horizontal-rule . org-groff-mom-horizontal-rule)
    (inline-babel-call . org-groff-mom-inline-babel-call)
    (inline-src-block . org-groff-mom-inline-src-block)
    (inlinetask . org-groff-mom-inlinetask)
    (italic . org-groff-mom-italic)
    (item . org-groff-mom-item)
    (keyword . org-groff-mom-keyword)
    (mom-environment . org-groff-mom-mom-environment)
    (mom-fragment . org-groff-mom-mom-fragment)
    (line-break . org-groff-mom-line-break)
    (link . org-groff-mom-link)
    (macro . org-groff-mom-macro)
    (paragraph . org-groff-mom-paragraph)
    (plain-list . org-groff-mom-plain-list)
    (plain-text . org-groff-mom-plain-text)
    (planning . org-groff-mom-planning)
    (property-drawer . org-groff-mom-property-drawer)
    (quote-block . org-groff-mom-quote-block)
    (quote-section . org-groff-mom-quote-section)
    (radio-target . org-groff-mom-radio-target)
    (section . org-groff-mom-section)
    (special-block . org-groff-mom-special-block)
    (src-block . org-groff-mom-src-block)
    (statistics-cookie . org-groff-mom-statistics-cookie)
    (strike-through . org-groff-mom-strike-through)
    (subscript . org-groff-mom-subscript)
    (superscript . org-groff-mom-superscript)
    (table . org-groff-mom-table)
    (table-cell . org-groff-mom-table-cell)
    (table-row . org-groff-mom-table-row)
    (target . org-groff-mom-target)
    (template . org-groff-mom-template)
    (timestamp . org-groff-mom-timestamp)
    (underline . org-groff-mom-underline)
    (verbatim . org-groff-mom-verbatim)
    (verse-block . org-groff-mom-verse-block))
  "Alist between element or object types and translators.")

(defconst org-groff-mom-options-alist
  '((:date "DATE" nil org-groff-mom-date-format t)
    (:mom-class "MOM_CLASS" nil org-groff-mom-default-class t)
    (:mom-class-options "MOM_CLASS_OPTIONS" nil nil t)
    (:mom-header-extra "MOM_HEADER" nil nil newline))
"Alist between Mom export properties and ways to set them.
See `org-export-options-alist' for more information on the
structure of the values.")



;;; User Configurable Variables

(defgroup org-export-groff-mom nil
  "Options for exporting Org mode files to Mom."
  :tag "Org Export Mom"
  :group 'org-export)


;;; Preamble

(defcustom org-groff-mom-default-class "typeset"
  "The default Mom class."
  :group 'org-export-groff-mom
  :type '(string :tag "Mom class"))

(defcustom org-groff-mom-classes
  '(("typeset" ".PRINTSTYLE TYPESET\n"
     (:heading 'default :type "DEFAULT" :paper "LETTER" :last-section "toc"))
    ("typewrite" ".PRINTSTYLE TYPEWRITE\n"
     (:heading 'default :type "DEFAULT" :paper "LETTER" :last-section "toc"))
    ("letterset" ".PRINTSTYLE TYPESET\n"
     (:heading 'default :type "LETTER" :paper "LETTER" :last-section "sign"))
    ("lettertype" ".PRINTSTYLE TYPEWRITE\n"
     (:heading 'default :type "LETTER" :paper "LETTER" :last-section "sign"))

    ("none" "" (:heading 'default :type "custom")))


  "This list describes the attributes for the documents being created.
   It allows for the creation of new "
  :group 'org-export-groff-mom
  :type '(repeat
          (list (string :tag "Document Type")
                (string :tag "Header")
                (repeat :tag "Options" :inline t
                        (choice
                         (list :tag "Heading")
                         (function :tag "Hook computing sectioning"))))))

(defcustom org-groff-mom-date-format
  (format-time-string "%Y-%m-%d")
  "Format string for .ND "
  :group 'org-export-groff-mom
  :type 'boolean)

;;; Headline

(defconst org-groff-mom-special-tags
  '("FROM" "TO" "ABSTRACT" "APPENDIX" "BODY" "NS"))


(defcustom org-groff-mom-format-headline-function nil
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

\(defun org-groff-mom-format-headline (todo todo-type priority text tags)
  \"Default format function for an headline.\"
  \(concat (when todo
            \(format \"\\fB%s\\fP \" todo))
	  \(when priority
            \(format \"[\\#%c] \" priority))
	  text
	  \(when tags
            \(format \" %s \"
              \(mapconcat 'identity tags \":\"))))"
  :group 'org-export-groff-mom
  :type 'function)


;;; Timestamps

(defcustom org-groff-mom-active-timestamp-format "\\*[IT]%s\\*[PREV]"
  "A printf format string to be applied to active timestamps."
  :group 'org-export-groff-mom
  :type 'string)

(defcustom org-groff-mom-inactive-timestamp-format "\\*[IT]%s\\*[PREV]"
  "A printf format string to be applied to inactive timestamps."
  :group 'org-export-groff-mom
  :type 'string)

(defcustom org-groff-mom-diary-timestamp-format "\\*[IT]%s\\*[PREV]"
  "A printf format string to be applied to diary timestamps."
  :group 'org-export-groff-mom
  :type 'string)


;;; Links


(defcustom org-groff-mom-inline-image-rules
  '(("file" . "\\.\\(pdf\\|ps\\|eps\\|pic\\)\\'")
    ("fuzzy" . "\\.\\(pdf\\|ps\\|eps\\|pic\\)\\'"))
  "Rules characterizing image files that can be inlined into Mom.

A rule consists in an association whose key is the type of link
to consider, and value is a regexp that will be matched against
link's path.

Note that, by default, the image extensions actually allowed
depend on the way the Mom file is processed.  When used with
pdfmom, pdf, jpg and png images are OK.  When processing
through dvi to Postscript, only ps and eps are allowed.  The
default we use here encompasses both."
  :group 'org-export-groff-mom
  :type '(alist :key-type (string :tag "Type")
                :value-type (regexp :tag "Path")))

(defcustom org-groff-mom-link-with-unknown-path-format "\\*[IT]%s\\*[PREV]"
  "Format string for links with unknown path type."
  :group 'org-export-mom
  :type 'string)


;;; Tables


(defcustom org-groff-mom-tables-centered t
  "When non-nil, tables are exported in a center environment."
  :group 'org-export-groff-mom
  :type 'boolean)

(defcustom org-groff-mom-tables-verbatim nil
  "When non-nil, tables are exported verbatim."
  :group 'org-export-groff-mom
  :type 'boolean)


(defcustom org-groff-mom-table-scientific-notation "%sE%s"
  "Format string to display numbers in scientific notation.
The format should have \"%s\" twice, for mantissa and exponent
\(i.e. \"%s\\\\times10^{%s}\").

When nil, no transformation is made."
  :group 'org-export-groff-mom
  :type '(choice
          (string :tag "Format string")
          (const :tag "No formatting")))


;;; Text markup

(defcustom org-groff-mom-text-markup-alist '((bold . "\\*[BD]%s\\*[PREV]")
                                           ;; from "verb"
                                           (code . "\\f[C]%s\\fP")
                                           (italic . "\\*[IT]%s\\*[PREV]")
    (strike-through . "\\fC%s\\fP")  ; Strike through and underline
    (underline . "\\*[IT]%s\\*[PREV]")       ; need to be revised.
                                           (verbatim .   "protectedtexttt"))
  "Alist of Mom expressions to convert text markup.

The key must be a symbol among `bold', `code', `italic',
`strike-through', `underline' and `verbatim'.  The value is
a formatting string to wrap fontified text with it.

If no association can be found for a given markup, text will be
returned as-is."
  :group 'org-export-groff-mom
  :type 'alist
  :options '(bold code italic strike-through underline verbatim))


;;; Drawers

(defcustom org-groff-mom-format-drawer-function nil
  "Function called to format a drawer in Mom code.

The function must accept two parameters:
  NAME      the drawer name, like \"LOGBOOK\"
  CONTENTS  the contents of the drawer.

The function should return the string to be exported.

For example, the variable could be set to the following function
in order to mimic default behaviour:

\(defun org-groff-mom-format-drawer-default \(name contents\)
  \"Format a drawer element for Mom export.\"
  contents\)"
  :group 'org-export-groff-mom
  :type 'function)


;;; Inlinetasks

(defcustom org-groff-mom-format-inlinetask-function nil
  "Function called to format an inlinetask in Mom code.

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

\(defun org-groff-mom-format-inlinetask \(todo type priority name tags contents\)
\"Format an inline task element for Mom export.\"
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
  :group 'org-export-groff-mom
  :type 'function)


;; Src blocks

(defcustom org-groff-mom-source-highlight nil
  "Use GNU source highlight to embellish source blocks "
  :group 'org-export-groff-mom
  :type 'boolean)


(defcustom org-groff-mom-source-highlight-langs
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
  :group 'org-export-groff-mom
  :type '(repeat
          (list
           (symbol :tag "Major mode       ")
           (string :tag "Listings language"))))

(defcustom org-groff-mom-source-highlight-options nil
  "Association list of options for the mom listings package.

These options are supplied as a comma-separated list to the
\\lstset command.  Each element of the association list should be
a list containing two strings: the name of the option, and the
value.  For example,

  (setq org-groff-mom-source-highlight-options
    '((\"basicstyle\" \"\\small\")
      (\"keywordstyle\" \"\\color{black}\\bfseries\\underbar\")))

will typeset the code in a small size font with underlined, bold
black keywords.

Note that the same options will be applied to blocks of all
languages."
  :group 'org-export-groff-mom
  :type '(repeat
          (list
           (string :tag "Listings option name ")
           (string :tag "Listings option value"))))



(defvar org-groff-mom-custom-lang-environments nil
  "Alist mapping languages to language-specific Mom environments.

It is used during export of src blocks by the listings and
mom packages.  For example,

  \(setq org-groff-mom-custom-lang-environments
     '\(\(python \"pythoncode\"\)\)\)

would have the effect that if org encounters begin_src python
during mom export it will use pythoncode as the source-highlight
language.")



;;; Plain text

(defcustom org-groff-mom-quotes
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
  :group 'org-export-groff-mom
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



(defcustom org-groff-mom-special-char
  '(("(c)" . "\\\\(co")
    ("(tm)" . "\\\\(tm")
    ("(rg)" . "\\\\(rg"))
  "CONS list in which the value of the car
  is replace on the value of the CDR. "
  :group 'org-export-groff-mom
  :type '(list
          (cons :tag "Character Subtitute"
                (string :tag "Original Character Group")
                (string :tag "Replacement Character"))))

;;; Compilation

(defcustom org-groff-mom-pdf-process
  '("pic %f | tbl | eqn | groff -mom  | ps2pdf - > %b.pdf"
    "pic %f | tbl | eqn | groff -mom  | ps2pdf - > %b.pdf"
    "pic %f | tbl | eqn | groff -mom  | ps2pdf - > %b.pdf")

  "Commands to process a Mom file to a PDF file.
This is a list of strings, each of them will be given to the
shell as a command.  %f in the command will be replaced by the
full file name, %b by the file base name \(i.e. without
extension) and %o by the base directory of the file."

  :group 'org-export-pdf
  :type '(choice
          (repeat :tag "Shell command sequence"
                  (string :tag "Shell command"))
          (const :tag "2 runs of groff -mom"
                 ("pic %f | tbl | eqn | groff -mom  | ps2pdf - > %b.pdf"
                  "pic %f | tbl | eqn | groff -mom  | ps2pdf - > %b.pdf" ))
          (const :tag "3 runs of groff -mom"
                 ("pic %f | tbl | eqn | groff -mom  | ps2pdf - > %b.pdf"
                  "pic %f | tbl | eqn | groff -mom  | ps2pdf - > %b.pdf"
                  "pic %f | tbl | eqn | groff -mom  | ps2pdf - > %b.pdf"))
          (function)))

(defcustom org-groff-mom-logfiles-extensions
  '("aux" "idx" "log" "out" "toc" "nav" "snm" "vrb")
  "The list of file extensions to consider as Mom logfiles."
  :group 'org-export-groff-mom
  :type '(repeat (string :tag "Extension")))

(defcustom org-groff-mom-remove-logfiles t
  "Non-nil means remove the logfiles produced by PDF production.
These are the .aux, .log, .out, and .toc files."
  :group 'org-export-groff-mom
  :type 'boolean)


(defcustom org-groff-mom-default-quad ".QUAD JUSTIFY"
  "Defines default justification to be used after a non
filled mode is used."
  :group 'org-export-groff-mom
  :type 'string)


(defcustom org-groff-mom-default-closing "Sincerely yours,"
  "Defines default justification to be used after a non
filled mode is used."
  :group 'org-export-groff-mom
  :type 'string)



;; Preamble

;; Adding MOM as a block parser to make sure that its contents
;; does not execute

(add-to-list 'org-element-block-name-alist
             '("MOM" . org-element-export-block-parser))

(defvar org-groff-mom-registered-references nil)
(defvar org-groff-mom-special-content nil)



;;; Internal Functions


(defun org-groff-mom--caption/label-string (caption label info)
  "Return caption and label Mom string for floats.

CAPTION is a cons cell of secondary strings, the car being the
standard caption and the cdr its short form.  LABEL is a string
representing the label.  INFO is a plist holding contextual
information.

If there's no caption nor label, return the empty string.

For non-floats, see `org-groff-mom--wrap-label'."
  (let ((label-str "" ))
    (cond
     ((and (not caption) (not label)) "")
     ((not caption) (format "\\*[IT]%s\\*[PREV]" label))
     ;; Option caption format with short name.
     ((cdr caption)
      (format "%s\n.BR\n%s - %s\n"
              (org-export-data (cdr caption) info)
              label-str
              (org-export-data (car caption) info)))
     ;; Standard caption format.
     (t (format "\\*[ROM]%s\\*[PREV]"
                (org-export-data (car caption) info))))))


(defun org-groff-mom--quotation-marks (text info)
  "Export quotation marks depending on language conventions.
TEXT is a string containing quotation marks to be replaced.  INFO
is a plist used as a communication channel."
  (mapc (lambda(l)
          (let ((start 0))
            (while (setq start (string-match (car l) text start))
              (let ((new-quote (concat (match-string 1 text) (cdr l))))
                (setq text (replace-match new-quote  t t text))))))
        (cdr (or (assoc (plist-get info :language) org-groff-mom-quotes)
                 ;; Falls back on English.
                 (assoc "en" org-groff-mom-quotes))))
  text)

(defun org-groff-mom--wrap-label (element output)
  "Wrap label associated to ELEMENT around OUTPUT, if appropriate.
This function shouldn't be used for floats.  See
`org-groff-mom--caption/label-string'."
  (let ((label (org-element-property :name element)))
    (if (or (not output) (not label) (string= output "") (string= label ""))
        output
      (concat (format "%s\n.BR\n" label) output))))

(defun org-groff-mom--text-markup (text markup)
  "Format TEXT depending on MARKUP text markup.
See `org-groff-mom-text-markup-alist' for details."
  (let ((fmt (cdr (assq markup org-groff-mom-text-markup-alist))))
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


(defun org-groff-mom--get-tagged-content  (tag info)
  (cdr  (assoc tag org-groff-mom-special-content)))


(defun org-groff-mom--mt-head (title contents attr info)
  (concat

   ;; 2. Title
   (let ((subtitle1 (plist-get attr :subtitle1))
         (subtitle2 (plist-get attr :subtitle2)))

     (cond
      ((string= "" title)
       (format ".TITLE \"%s\" \"%s\"\n.SUBTITLE \"%s\" \n"
               (or subtitle1 "")
               (or subtitle2 "") " ")
       )
      ((not (or subtitle1 subtitle2))
       (format ".TITLE \"%s\" \n"
               (or title "" )))
      (t
       (format ".TITLE \"%s\" \"%s\"\n.SUBTITLE \"%s\"\n"
               title
               (or subtitle1 "")
               (or subtitle2 "")))))))


(defun org-groff-mom--letter-head (title contents attr info)
  (let ((author (and (plist-get info :with-author)
                     (let ((auth (plist-get info :author)))
                       (and auth (org-export-data auth info)))))
        (email (and (plist-get info :with-email)
                    (org-export-data (plist-get info :email) info)))
        (from-data  (org-groff-mom--get-tagged-content "FROM" info))
        (at-item (plist-get attr :author-title)  )
        (to-data  (org-groff-mom--get-tagged-content "TO" info)))

    ;; If FROM then get data from FROM

    (when from-data 
      (setq from-data
          (replace-regexp-in-string "\\.PP\n" "" from-data)))
    
    (when to-data
      (setq to-data
          (replace-regexp-in-string "\\.PP\n" "" to-data)))

    (concat

     (let ((date (org-export-data (plist-get info :date) info)))
       (and date (format ".DATE \n%s\n.SP\n" date)))

     (cond
      (from-data
       (format ".FROM\n%s\n.SP\n" from-data ))
      ((and author email (not (string= "" email)))
       (format ".FROM\n%s\n%s\n.SP\n" author email))
      (author (format ".FROM\n%s\n.SP\n" author))
      (t ""))

     ;; If TO then get data from TO

     (when to-data
       (format ".TO\n%s\n.SP\n" to-data)))))


;;; Template

(defun org-groff-mom-template (contents info)
  "Return complete document string after Mom conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let* ((title (org-export-data (plist-get info :title) info))
         (author (and (plist-get info :with-author)
                      (let ((auth (plist-get info :author)))
                        (and auth (org-export-data auth info)))))
         (email (and (plist-get info :with-email)
                     (org-export-data (plist-get info :email) info)))
         (attr (read
                (format "(%s)"
                        (mapconcat
                         #'identity
                         (list (plist-get info :mom-class-options))
                         " "))))
         (class (plist-get info :mom-class))
         (class-options (plist-get info :mom-class-options))
         (classes (assoc class org-groff-mom-classes))
         (classes-options (car (last classes)))
         (heading-option (plist-get classes-options :heading ))
         (type-option (plist-get classes-options :type ))
         (paper-option (plist-get classes-options :paper))
         (last-option (plist-get classes-options :last-section ))
         (hyphenate (plist-get attr :hyphenate))
         (justify-right (plist-get attr :justify-right))

         (document-class-string
          (progn
            (org-element-normalize-string
             (let* ((header (nth 1 (assoc class org-groff-mom-classes)))
                    (document-class-item (if (stringp header) header "")))
               document-class-item))))
         (headline-type (car 
                         (org-element-map
                          (plist-get info :parse-tree) 'headline 
                          #'identity info)))
         (numberedp (org-export-numbered-headline-p headline-type info)))

    (concat
     (cond
      ((and author email (not (string= "" email)))
       (format ".AUTHOR \"%s\" \"%s\"\n" author email))
      (author (format ".AUTHOR \"%s\"\n" author))
      (t ".AUTHOR \"\" \n"))

     (cond
      ((string= type-option "CUSTOM") "")
      ((string= type-option "DEFAULT")
       (concat
        document-class-string "\n"
        (concat ".DOCTYPE " type-option "\n")
        (concat ".PAPER " paper-option "\n")
        (org-groff-mom--mt-head title contents attr info)
        ".START\n"))
        
      ((string= type-option "LETTER")
       (concat
        (let ((sa-item (plist-get attr :salutation)))
          (concat
           document-class-string  "\n"
           (concat ".DOCTYPE " type-option "\n")
           (concat ".PAPER " paper-option "\n")
           ".START\n"
           (org-groff-mom--letter-head title contents attr info)
           (if (stringp sa-item)
             (format ".GREETINGS\n%s\n.SP\n"  sa-item) 
             (format ".GREETINGS\nTo whom it may concern,\n.SP\n"))
           "\n" ))))

      (t ""))

     (when numberedp 
       (concat 
        ".NUMBER_HEADS\n"
        ".NUMBER_SUBHEADS\n"
        ".NUMBER_SUBSUBHEADS\n"
        ".NUMBER_PARAHEADS\n"))

     contents

     (when org-groff-mom-registered-references ".ENDNOTES\n")

     (cond
      ((string= last-option "toc")
       ".TOC")
      ((string= last-option "sign")
       (let ((fc-item (plist-get attr :closing)))
         (concat (if (stringp fc-item)
                     (format ".CLOSING\n%s\n.SP\n" fc-item)
                   (format  ".CLOSING\n%s\n.SP\n" org-groff-mom-default-closing)))))
      (t "")))))



;;; Transcode Functions

;;
; Babel Call
;;
;; Babel Calls are ignored.


;;; Bold

(defun org-groff-mom-bold (bold contents info)
  "Transcode BOLD from Org to Mom.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (org-groff-mom--text-markup contents 'bold))


;;; Center Block

(defun org-groff-mom-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to Mom.
CONTENTS holds the contents of the center block.  INFO is a plist
holding contextual information."
  (org-groff-mom--wrap-label
   center-block
   (format ".CENTER\n%s\n%s\n" contents
           org-groff-mom-default-quad)))

;;; Clock

(defun org-groff-mom-clock (clock contents info)
  "Transcode a CLOCK element from Org to Mom.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (concat
   (format "\\fB%s\\fP " org-clock-string)
   (format org-groff-mom-inactive-timestamp-format
           (concat (org-translate-time (org-element-property :value clock))
                   (let ((time (org-element-property :time clock)))
                     (and time (format " (%s)" time)))))))


;;; Code

(defun org-groff-mom-code (code contents info)
  "Transcode a CODE object from Org to Mom.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-groff-mom--text-markup (org-element-property :value code) 'code))


;;; Comment
;;
;; Comments are ignored.


;;; Comment Blockp
;;
;; Comment Blocks are ignored.


;;; Drawer

(defun org-groff-mom-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to Mom.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((name (org-element-property :drawer-name drawer))
         (output (if (functionp org-groff-mom-format-drawer-function)
                     (funcall org-groff-mom-format-drawer-function
                              name contents)
                   ;; If there's no user defined function: simply
                   ;; display contents of the drawer.
                   contents)))
    (org-groff-mom--wrap-label drawer output)))


;;; Dynamic Block

(defun org-groff-mom-dynamic-block (dynamic-block contents info)
  "Transcode a DYNAMIC-BLOCK element from Org to Mom.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See `org-export-data'."
  (org-groff-mom--wrap-label dynamic-block contents))


;;; Entity

(defun org-groff-mom-entity (entity contents info)
  "Transcode an ENTITY object from Org to Mom.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (let ((ent (org-element-property :utf8 entity))) ent))


;;; Example Block

(defun org-groff-mom-example-block (example-block contents info)
  "Transcode an EXAMPLE-BLOCK element from Org to Mom.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (org-groff-mom--wrap-label
   example-block
   (format ".QUOTE\n%s\n.QUOTE OFF"
           (org-export-format-code-default example-block info))))


;;; Export Block

(defun org-groff-mom-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to Mom.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "MOM")
    (org-remove-indentation (org-element-property :value export-block))))


;;; Export Snippet

(defun org-groff-mom-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to Mom.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'groff-mom)
    (org-element-property :value export-snippet)))

;;; Fixed Width

(defun org-groff-mom-fixed-width (fixed-width contents info)
  "Transcode a FIXED-WIDTH element from Org to Mom.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-groff-mom--wrap-label
   fixed-width
   (format "\\fC\n%s\\fP"
           (org-remove-indentation
            (org-element-property :value fixed-width)))))


;;; Footnote Definition
;;
;; Footnote Definitions are ignored.

;;
;; Footnotes are handled automatically in MOM.  Although manual
;; references can be added, not really required.

(defun org-groff-mom-footnote-reference (footnote-reference contents info)
  ;; Changing from info to footnote-reference
  (let* (( raw (org-export-get-footnote-definition footnote-reference info))
		 (n (org-export-get-footnote-number footnote-reference info))
		 (data (org-trim (org-export-data raw info)))
         (ref-id (plist-get (nth 1 footnote-reference) :label)))
    ;;
    ;; It is a reference
    ;;

    (if (string-match "fn:rl" ref-id)
        (if (member ref-id org-groff-mom-registered-references)
            (format "\\*[%s]" ref-id)
          (progn
            (push ref-id org-groff-mom-registered-references)
            (format "\\c\n.ENDNOTE\n%s\n.ENDNOTE OFF\n" data)))
      ;;
      ;; else it is a footnote
      ;;

      (format "\\c\n.FOOTNOTE\n%s\n.FOOTNOTE OFF\n" data))))

;;; Headline

(defun org-groff-mom-headline (headline contents info)
  "Transcode an HEADLINE element from Org to Mom.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((class (plist-get info :mom-class))
         (level (org-export-get-relative-level headline info))
         (numberedp (org-export-numbered-headline-p headline info))

         ;; Section formatting will set two placeholders: one for the
         ;; title and the other for the contents.

         (classes (assoc class org-groff-mom-classes))
         (classes-options (car (last classes)))
         (heading-command
          (case level
            (1 ".HEAD")
            (2 ".SUBHEAD")
            (3 ".SUBSUBHEAD")
            (4 ".PARAHEAD")))
         (heading-option (plist-get classes-options :heading ))
         (section-fmt
          (progn
            (cond
             ((and (symbolp heading-option)
                   (fboundp heading-option))
              (funcall heading-option level numberedp))
             ((> level 7) nil)
             (t (concat heading-command  " \"%s\"\n%s")))))
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
         (full-text (if (functionp org-groff-mom-format-headline-function)
                        ;; User-defined formatting function.
                        (funcall org-groff-mom-format-headline-function
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
          (if (functionp org-groff-mom-format-headline-function)
              ;; User-defined formatting function.
              (funcall org-groff-mom-format-headline-function
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
     ((member (car  tags)  org-groff-mom-special-tags)
      (cond
       ((string= (car tags) "BODY") contents )

       (t
        (progn
          (push (cons  (car tags) contents) org-groff-mom-special-content)
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
                (format "%s\n" (if numberedp ".LIST DIGIT\n" ".LIST BULLET \n")))
              ;; Itemize headline
              ".ITEM\n" full-text "\n" headline-label pre-blanks contents)))
        ;; If headline is not the last sibling simply return
        ;; LOW-LEVEL-BODY.  Otherwise, also close the list, before any
        ;; blank line.
        (if (not (org-export-last-sibling-p headline info)) low-level-body
          (replace-regexp-in-string
           "[ \t\n]*\\'"
           (concat "\n.LIST OFF" )
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

(defun org-groff-mom-inline-src-block (inline-src-block contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to Mom.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((code (org-element-property :value inline-src-block)))
    (cond
     (org-groff-mom-source-highlight
      (let* ((tmpdir (if (featurep 'xemacs)
                         temp-directory
                       temporary-file-directory ))
             (in-file  (make-temp-name
                        (expand-file-name "srchilite" tmpdir))  )
             (out-file (make-temp-name
                        (expand-file-name "reshilite" tmpdir)))
             (org-lang (org-element-property :language inline-src-block))
             (lst-lang (cadr (assq (intern org-lang)
                                   org-groff-mom-source-highlight-langs)))

             (cmd (concat (expand-file-name "source-highlight")
                          " -s " lst-lang
                          " -f groff_mom_color "
                          " -i " in-file
                          " -o " out-file )))

        (if lst-lang
            (let ((code-block "" ))
              (with-temp-file in-file (insert code))
              (shell-command cmd)
              (setq code-block  (org-file-contents out-file))
              (delete-file in-file)
              (delete-file out-file)
              code-block)
          (format ".LEFT\n.CODE BR \\m[black]%s\\m[]\n.CODE OFF\n%s\n"
                  code org-groff-mom-default-quad))))

     ;; Do not use a special package: transcode it verbatim.
     (t
      (concat ".LEFT\n.CODE BR\n" code 
              "\n.CODE OFF\n"  
              org-groff-mom-default-quad "\n")))))


;;; Inlinetask


(defun org-groff-mom-inlinetask (inlinetask contents info)
  "Transcode an INLINETASK element from Org to Mom.
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
    ;; If `org-groff-mom-format-inlinetask-function' is provided, call it
    ;; with appropriate arguments.
    (if (functionp org-groff-mom-format-inlinetask-function)
        (funcall org-groff-mom-format-inlinetask-function
                 todo todo-type priority title tags contents)
      ;; Otherwise, use a default template.
      (org-groff-mom--wrap-label
       inlinetask
       (let ((full-title
              (concat
               (when todo (format "\\fB%s\\fP " todo))
               (when priority (format " [\\#%c] " priority))
               title
               (when tags (format " \\fC:%s:\\fP "
                                  (mapconcat 'identity tags ":"))))))
         (format (concat "\n.LEFT\n"
                         "%s\n"
                         ".SP"
                         "%s\n"
                         org-groff-mom-default-quad)
                 full-title contents))))))


;;; Italic

(defun org-groff-mom-italic (italic contents info)
  "Transcode ITALIC from Org to Mom.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (org-groff-mom--text-markup contents 'italic))


;;; Item


(defun org-groff-mom-item (item contents info)
  "Transcode an ITEM element from Org to Mom.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((bullet (org-element-property :bullet item))
	 (type (org-element-property
		:type (org-element-property :parent item)))
         (checkbox (case (org-element-property :checkbox item)
                     (on "\\o'\\(sq\\(mu'\\ ")
                     (off "\\(sq\\ ")
                     (trans "\\o'\\(sq\\(mi'\\ ")))

         (tag (let ((tag (org-element-property :tag item)))
                ;; Check-boxes must belong to the tag.
                (and tag (format "%s"
                                 (concat checkbox
                                         (org-export-data tag info)))))))
	(cond
     ((eq type 'descriptive)
      (concat ".HI 1.0i\n \\*[BD]" tag "\\*[PREV]\\ \\ \\ \\ "
              contents "\n.BR\n"))
	 ((or checkbox tag)
	  (concat ".ITEM \n" (or tag (concat "\\ " checkbox))
              (org-trim (or contents " " )))  )
     (t
      (concat ".ITEM"
              "\n"
              (org-trim (or contents " " )))))))



;;; Keyword


(defun org-groff-mom-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to Mom.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
        (value (org-element-property :value keyword)))
    (cond
     ((string= key "MOM") value)
     (t nil))))


;;; Mom Environment

(defun org-groff-mom-mom-environment (mom-environment contents info)
  "Transcode a MOM-ENVIRONMENT element from Org to Mom.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((label (org-element-property :name mom-environment))
        (value (org-remove-indentation
                (org-element-property :value mom-environment))))
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


;;; Mom Fragment

(defun org-groff-mom-mom-fragment (mom-fragment contents info)
  "Transcode a MOM-FRAGMENT object from Org to Mom.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-property :value mom-fragment))


;;; Line Break

(defun org-groff-mom-line-break (line-break contents info)
  "Transcode a LINE-BREAK object from Org to Mom.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ".BR\n")


;;; Link
;;;
;;; Inline images just place a call to .PSPIC or .PS/.PE
;;;  and load the graph.
;;;


(defun org-groff-mom-link--inline-image (link info)
  "Return Mom code for an inline image.
LINK is the link pointing to the inline image.  INFO is a plist
used as a communication channel."
  (let* ((parent (org-export-get-parent-element link))
         (path (let ((raw-path (org-element-property :path link)))
                 (if (not (file-name-absolute-p raw-path)) raw-path
                   (expand-file-name raw-path))))
         (attr (read (format "(%s)"
            (mapconcat
             #'identity
             (org-element-property :attr_mom parent)
             " "))))
         (placement
          (case (plist-get attr :position)
            ('center "")
            ('left "-L")
            ('right "-R")
            (t "")))
    (width  (or (plist-get attr :width) "" ))
    (height (or (plist-get attr :height) "" ))

    (disable-caption (plist-get attr :disable-caption))

    (caption
          (org-groff-mom--caption/label-string
           (org-element-property :caption parent)
           (org-element-property :name parent)
           info)))

    ;; Now clear ATTR from any special keyword and set a default value
    ;; if nothing is left.  Return proper string.

    (concat
     (cond
      ((string-match ".\.pic$" path)
       (format "\n.PS\ncopy \"%s\"\n.PE" path ))
      (t (format "\n.QUAD LEFT\n.PSPIC %s \"%s\" %s %s\n%s"
                 placement path width height org-groff-mom-default-quad)))
     (unless disable-caption 
       (format "\n.CENTER\n%s\n%s" caption org-groff-mom-default-quad)))))



(defun org-groff-mom-link (link desc info)
  "Transcode a LINK object from Org to Mom.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."

  (let* ((type (org-element-property :type link))
         (raw-path (org-element-property :path link))
         ;; Ensure DESC really exists, or set it to nil.
         (desc (and (not (string= desc "")) desc))
         (imagep (org-export-inline-image-p
                  link org-groff-mom-inline-image-rules))
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
     (imagep (org-groff-mom-link--inline-image link info))
     ;; import mom files
     ((and (string= type "file")
           (string-match ".\.mom$" raw-path))
      (concat ".so " raw-path "\n"))
     ;; Radio link: Transcode target's contents and use them as link's
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
           (format org-groff-mom-link-with-unknown-path-format
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
     (t (format org-groff-mom-link-with-unknown-path-format desc)))))


;;; Macro

(defun org-groff-mom-macro (macro contents info)
  "Transcode a MACRO element from Org to Mom.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;; Use available tools.
  (org-export-expand-macro macro info))


;;; Paragraph

(defun org-groff-mom-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to Mom.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (let ((parent (plist-get (nth 1 paragraph) :parent)))
    (when parent
      (let* ((parent-type (car parent))
             (fixed-paragraph "")
             (class (plist-get info :mom-class))
             (class-options (plist-get info :mom-class-options))
             (classes (assoc class org-groff-mom-classes))
             (classes-options (car (last classes)))
             (paragraph-option (plist-get classes-options :paragraph )))
        (cond
         ((and (symbolp paragraph-option)
               (fboundp paragraph-option))
          (funcall paragraph-option parent-type parent contents))

         ((and (eq parent-type 'item)
               (plist-get (nth 1 parent) :bullet ))
          (setq fixed-paragraph (concat "" contents)))

         ((eq parent-type 'section)
          (setq fixed-paragraph (concat ".PP\n" contents)))

         ((eq parent-type 'footnote-definition)
          (setq fixed-paragraph (concat "" contents)))

         (t (setq fixed-paragraph (concat "" contents))))
        fixed-paragraph))))

;;; Plain List

(defun org-groff-mom-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to Mom.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* ((type (org-element-property :type plain-list))
         (attr (mapconcat #'identity
                          (org-element-property :attr_mom plain-list)
                          " "))

         (bullet (org-trim
                  (org-element-property :bullet (nth 2 plain-list))))
         (marker (cond  ((string= "-" bullet) "DASH")
                            ((string= "*" bullet) "BULLET")
                            (t "USER \"\\(dg\"")))
         (mom-type (cond
                    ((eq type 'ordered) ".LIST DIGIT")
                    ((eq type 'unordered) (concat ".LIST " marker))
                    ((eq type 'descriptive) ".IL 2.0i \n"))))

    (if (eq type 'descriptive)
        (org-groff-mom--wrap-label
         plain-list
         (format "%s\n%s\n.IQ CLEAR\n"
                 mom-type
                 contents))
      (org-groff-mom--wrap-label
       plain-list
       (format "%s\n%s\n.LIST OFF"
               mom-type
               contents)))))


;;; Plain Text

(defun org-groff-mom-plain-text (text info)
  "Transcode a TEXT string from Org to Mom.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  ;; Protect
  (setq text (replace-regexp-in-string
              "\\(?:[^\\]\\|^\\)\\(\\\\\\)\\(?:[^%$#&{}~^_\\]\\|$\\)"
              "$\\" text nil t 1))


  ;; Handle quotation marks
  (setq text (org-groff-mom--quotation-marks text info))

  (if org-groff-mom-special-char
      (dolist (special-char-list org-groff-mom-special-char)
        (setq text
              (replace-regexp-in-string (car special-char-list)
                                        (cdr special-char-list) text ))))

  ;; Handle Special Characters

  ;; Handle break preservation if required.
  (when (plist-get info :preserve-breaks)
    (setq text (replace-regexp-in-string
		"\\(\\\\\\\\\\)?[ \t]*\n" " \\\\\\\\\n" text)))
  ;; Return value.
  text)



;;; Planning

(defun org-groff-mom-planning (planning contents info)
  "Transcode a PLANNING element from Org to Mom.
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
                (format org-groff-mom-inactive-timestamp-format
                        (org-translate-time closed)))))
           (let ((deadline (org-element-property :deadline planning)))
             (when deadline
               (concat
                (format "\\fB %s \\fP" org-deadline-string)
                (format org-groff-mom-active-timestamp-format
                        (org-translate-time deadline)))))
           (let ((scheduled (org-element-property :scheduled planning)))
             (when scheduled
               (concat
                (format "\\fR %s \\fP" org-scheduled-string)
                (format org-groff-mom-active-timestamp-format
                        (org-translate-time scheduled)))))))
    "")
   ""))


;;; Property Drawer

(defun org-groff-mom-property-drawer (property-drawer contents info)
  "Transcode a PROPERTY-DRAWER element from Org to Mom.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  ;; The property drawer isn't exported but we want separating blank
  ;; lines nonetheless.
  contents)


;;; Quote Block

(defun org-groff-mom-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to Mom.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (org-groff-mom--wrap-label
   quote-block
   (format ".BLOCKQUOTE\n.FT I\n%s\n.FT R\n.BLOCKQUOTE OFF" contents)))


;;; Quote Section

(defun org-groff-mom-quote-section (quote-section contents info)
  "Transcode a QUOTE-SECTION element from Org to Mom.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((value (org-remove-indentation
                (org-element-property :value quote-section))))
    (when value (format ".QUOTE\n\\fI%s\\fP\n.QUOTE OFF\n" value))))


;;; Radio Target

(defun org-groff-mom-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to Mom.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (format "%s - %s"
          (org-export-solidify-link-text
           (org-element-property :value radio-target))
          text))


;;; Section

(defun org-groff-mom-section (section contents info)
  "Transcode a SECTION element from Org to Mom.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  contents)


;;; Special Block

(defun org-groff-mom-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to Mom.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((type (downcase (org-element-property :type special-block))))
    (org-groff-mom--wrap-label
     special-block
     (format "%s\n" contents))))


;;; Src Block

(defun org-groff-mom-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to Mom.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."

  (let* ((lang (org-element-property :language src-block))
         (caption (org-element-property :caption src-block))
         (label (org-element-property :name src-block))
         (code (org-element-property :value src-block))
         (custom-env (and lang
                          (cadr (assq (intern lang)
                                      org-groff-mom-custom-lang-environments))))
         (num-start (case (org-element-property :number-lines src-block)
                      (continued (org-export-get-loc src-block info))
                      (new 0)))
         (retain-labels (org-element-property :retain-labels src-block))

         (attr
          (read (format "(%s)"
                   (mapconcat #'identity
                              (org-element-property :attr_mom src-block)
                              " "))))
         (disable-caption (plist-get attr :disable-caption)))


    (cond
     ;; Case 1.  No source fontification.
     ((not org-groff-mom-source-highlight)
      (let ((caption-str (org-groff-mom--caption/label-string caption label info)))
        (concat
         (format ".LEFT\n.CODE BR\n%s\n.CODE OFF\n%s\n"
                 (org-export-format-code-default src-block info) 
                 org-groff-mom-default-quad)
         (unless  disable-caption (format "%s"  caption-str )))))

     ;; Case 2.  Source fontification.
     (org-groff-mom-source-highlight
       (let* ((tmpdir (if (featurep 'xemacs)
                          temp-directory
                        temporary-file-directory ))
              (caption-str (org-groff-mom--caption/label-string caption label info))
              (in-file  (make-temp-name
                         (expand-file-name "srchilite" tmpdir)))
              (out-file (make-temp-name
                         (expand-file-name "reshilite" tmpdir)))

              (org-lang (org-element-property :language src-block))
              (lst-lang (cadr (assq (intern org-lang)
                                    org-groff-mom-source-highlight-langs)))

              (cmd (concat "source-highlight"
                           " -s " lst-lang
                           " -f groff_mom_color "
                           " -i " in-file
                           " -o " out-file )))

         (concat
          (if lst-lang
              (let ((code-block "" ))
                (with-temp-file in-file (insert code))
                (shell-command cmd)
                (setq code-block  (org-file-contents out-file))
                (delete-file in-file)
                (delete-file out-file)
                (format ".LEFT\n%s\n%s\n"  code-block 
                        org-groff-mom-default-quad))
            (format ".LEFT\n.CODE BR\n%s\n.CODE OFF\n%s\n"
                    code org-groff-mom-default-quad))
          (unless disable-caption (format "%s" caption-str))))))))


;;; Statistics Cookie

(defun org-groff-mom-statistics-cookie (statistics-cookie contents info)
  "Transcode a STATISTICS-COOKIE object from Org to Mom.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-property :value statistics-cookie))


;;; Strike-Through

(defun org-groff-mom-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to Mom.
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (org-groff-mom--text-markup contents 'strike-through))

;;; Subscript

(defun org-groff-mom-subscript (subscript contents info)
  "Transcode a SUBSCRIPT object from Org to Mom.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format  "\\d\\s-2 %s\\s+2\\u" contents))

;;; Superscript "^_%s$

(defun org-groff-mom-superscript (superscript contents info)
  "Transcode a SUPERSCRIPT object from Org to Mom.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format  "\\*[SUP]%s\\*[SUPX]" contents))


;;; Table
;;
;; `org-groff-mom-table' is the entry point for table transcoding.  It
;; takes care of tables with a "verbatim" attribute.  Otherwise, it
;; delegates the job to  `org-groff-mom-table--org-table' function,
;; depending of the type of the table.
;;
;; `org-groff-mom-table--align-string' is a subroutine used to build
;; alignment string for Org tables.

(defun org-groff-mom-table (table contents info)
  "Transcode a TABLE element from Org to Mom.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
  (cond
   ;; Case 1: verbatim table.
   ((or org-groff-mom-tables-verbatim
        (let ((attr (read (format "(%s)"
                 (mapconcat
                  #'identity
                                   (org-element-property :attr_mom table) " ")))))
          (and attr (plist-get attr :verbatim))))

    (format ".LEFT\n\\fC%s\\fP\n%s"
            ;; Re-create table, without affiliated keywords.
            (org-trim
             (org-element-interpret-data
              `(table nil ,@(org-element-contents table))))
            org-groff-mom-default-quad))

   ;; Case 2: Standard table.
   (t (org-groff-mom-table--org-table table contents info))))

(defun org-groff-mom-table--align-string (divider table info)
  "Return an appropriate Mom alignment string.
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

(defun org-groff-mom-table--org-table (table contents info)
  "Return appropriate Mom code for an Org table.

TABLE is the table type element to transcode.  CONTENTS is its
contents, as a string.  INFO is a plist used as a communication
channel.

This function assumes TABLE has `org' as its `:type' attribute."
  (let* ((label (org-element-property :name table))
         (caption (org-groff-mom--caption/label-string
                   (org-element-property :caption table) label info))
         (attr (read (format "(%s)"
                             (mapconcat #'identity
             (org-element-property :attr_mom table)
             " "))))
         (divider (if (plist-get attr :divider) "|" " "))

         ;; Determine alignment string.
         (alignment (org-groff-mom-table--align-string divider table info))

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
                         (if org-groff-mom-tables-centered
                             "center"
                           "" )))

                      (case (plist-get attr :boxtype)
                        ('box "box")
                        ('doublebox "doublebox")
                        ('allbox "allbox")
                        ('none nil)
                        (t "box"))))

              (if (not (null attr-item))
                  (add-to-list 'result-list attr-item)))
            result-list ))


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
                              final-line ))

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
                                                                            (format "T{\n%s\nT}\t\n"  cell-item ))))
                                                   (t
                                                    (setq long-line (concat long-line
                                                                            (format "T{\n%s\nT}\t"  cell-item ))))))
                                        long-line))
                                     ;; else long cells
                                  (setq final-line (concat final-line long-line )))

                                  (setq final-line (concat final-line line-item "\n"))))
                              final-line))

                    (if (not disable-caption)
                        (format "\n.CENTER\n%s\n%s\n"
                                caption 
                                org-groff-mom-default-quad) ""))))))

;;; Table Cell

(defun org-groff-mom-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to Mom
CONTENTS is the cell contents.  INFO is a plist used as
a communication channel."
  (progn
    (concat (if (and contents
                     org-groff-mom-table-scientific-notation
                     (string-match orgtbl-exp-regexp contents))
                ;; Use appropriate format string for scientific
                ;; notation.
                (format org-groff-mom-table-scientific-notation
                        (match-string 1 contents)
                        (match-string 2 contents))
              contents )
            (when (org-export-get-next-element table-cell info) "\t"))))


;;; Table Row

(defun org-groff-mom-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to Mom
CONTENTS is the contents of the row.  INFO is a plist used as
a communication channel."
  ;; Rules are ignored since table separators are deduced from
  ;; borders of the current row.
  (when (eq (org-element-property :type table-row) 'standard)
    (let* ((attr (mapconcat 'identity
                            (org-element-property
                             :attr_mom (org-export-get-parent table-row))
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

(defun org-groff-mom-target (target contents info)
  "Transcode a TARGET object from Org to Mom.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "\\fI%s\\fP"
          (org-export-solidify-link-text (org-element-property :value target))))


;;; Timestamp

(defun org-groff-mom-timestamp (timestamp contents info)
  "Transcode a TIMESTAMP object from Org to Mom.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((value (org-translate-time (org-element-property :value timestamp)))
        (type (org-element-property :type timestamp)))
    (cond ((memq type '(active active-range))
           (format org-groff-mom-active-timestamp-format value))
          ((memq type '(inactive inactive-range))
           (format org-groff-mom-inactive-timestamp-format value))
          (t (format org-groff-mom-diary-timestamp-format value)))))


;;; Underline

(defun org-groff-mom-underline (underline contents info)
  "Transcode UNDERLINE from Org to Mom.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (org-groff-mom--text-markup contents 'underline))


;;; Verbatim

(defun org-groff-mom-verbatim (verbatim contents info)
  "Transcode a VERBATIM object from Org to Mom.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (org-groff-mom--text-markup (org-element-property :value verbatim) 'verbatim))


;;; Verse Block

(defun org-groff-mom-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to Mom.
CONTENTS is verse block contents. INFO is a plist holding
contextual information."
  (format ".CENTER\n%s\n%s" contents org-groff-mom-default-quad))



;;; Interactive functions

(defun org-groff-mom-export-to-mom
  (&optional subtreep visible-only body-only ext-plist pub-dir)
  "Export current buffer to a Mom file.

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

  (setq org-groff-mom-registered-references nil)
  (setq org-groff-mom-special-content nil)

  (let ((outfile (org-export-output-file-name ".mom" subtreep pub-dir)))
    (org-export-to-file
     'groff-mom outfile subtreep visible-only body-only ext-plist)))

(defun org-groff-mom-export-to-pdf
  (&optional subtreep visible-only body-only ext-plist pub-dir)
  "Export current buffer to Mom then process through to PDF.

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
  (org-groff-mom-compile
   (org-groff-mom-export-to-mom
    subtreep visible-only body-only ext-plist pub-dir)))

(defun org-groff-mom-compile (momfile)
  "Compile a Mom file.

MOMFILE is the name of the file being compiled.  Processing is
done through the command specified in `org-groff-mom-pdf-process'.

Return PDF file name or an error if it couldn't be produced."
  (let* ((wconfig (current-window-configuration))
         (momfile (file-truename momfile))
         (base (file-name-sans-extension momfile))
         errors)
    (message (format "Processing Mom file %s ..." momfile))
    (unwind-protect
        (progn
          (cond
           ;; A function is provided: Apply it.
           ((functionp org-groff-mom-pdf-process)
            (funcall org-groff-mom-pdf-process (shell-quote-argument momfile)))
           ;; A list is provided: Replace %b, %f and %o with appropriate
           ;; values in each command before applying it.  Output is
           ;; redirected to "*Org PDF Mom Output*" buffer.
           ((consp org-groff-mom-pdf-process)
            (let* ((out-dir (or (file-name-directory momfile) "./"))
                   (outbuf (get-buffer-create "*Org PDF Mom Output*")))
              (mapc
               (lambda (command)
                 (shell-command
                  (replace-regexp-in-string
                   "%b" (shell-quote-argument base)
                   (replace-regexp-in-string
                    "%f" (shell-quote-argument momfile)
                    (replace-regexp-in-string
                     "%o" (shell-quote-argument out-dir) command t t)
		    t t) t t)
                  outbuf))
               org-groff-mom-pdf-process)
              ;; Collect standard errors from output buffer.
              (setq errors (org-groff-mom-collect-errors outbuf))))
           (t (error "No valid command to process to PDF")))
          (let ((pdffile (concat base ".pdf")))
            ;; Check for process failure.  Provide collected errors if
            ;; possible.
            (if (not (file-exists-p pdffile))
                (error (concat (format "PDF file %s wasn't produced" pdffile)
                               (when errors (concat ": " errors))))
              ;; Else remove log files, when specified, and signal end of
              ;; process to user, along with any error encountered.
              (when org-groff-mom-remove-logfiles
                (dolist (ext org-groff-mom-logfiles-extensions)
                  (let ((file (concat base "." ext)))
                    (when (file-exists-p file) (delete-file file)))))
              (message (concat "Process completed"
                               (if (not errors) "."
                                 (concat " with errors: " errors)))))
            ;; Return output file name.
            pdffile))
      (set-window-configuration wconfig))))

(defun org-groff-mom-collect-errors (buffer)
  "Collect some kind of errors from \"mom\" output
BUFFER is the buffer containing output.
Return collected error types as a string, or nil if there was
none."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      ;; Find final run
      nil )))


(provide 'org-groff-mom)
;;; org-groff-mom.el ends here
