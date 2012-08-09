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
;; It introduces one new buffer keywords:
;; "MAN_CLASS_OPTIONS".

;;; Code:

(require 'org-export)

(eval-when-compile (require 'cl))

(defvar org-export-man-default-packages-alist)
(defvar org-export-man-packages-alist)




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
  '((:date "DATE" nil nil t)
    (:man-class "MAN_CLASS" nil nil t)
    (:man-class-options "MAN_CLASS_OPTIONS" nil nil t)
    (:man-header-extra "MAN_HEADER" nil nil newline))
  "Alist between Man export properties and ways to set them.
See `org-export-options-alist' for more information on the
structure of the values.")



;;; User Configurable Variables

(defgroup org-export-e-man nil
  "Options for exporting Org mode files to Man."
  :tag "Org Export Man"
  :group 'org-export)

;;; Tables


(defcustom org-e-platypus-table-scientific-notation "%sE%s"
  "Format string to display numbers in scientific notation.
The format should have \"%s\" twice, for mantissa and exponent
\(i.e. \"%s\\\\times10^{%s}\").

When nil, no transformation is made."
  :group 'org-export-e-platypus
  :type '(choice
          (string :tag "Format string")
          (const :tag "No formatting")))


;;; Inlinetasks
;; Src blocks

(defcustom org-e-platypus-source-highlight nil
  "Use GNU source highlight to embellish source blocks "
  :group 'org-export-e-platypus
  :type 'boolean)


(defcustom org-e-platypus-source-highlight-langs
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
  :group 'org-export-e-platypus
  :type '(repeat
          (list
           (symbol :tag "Major mode       ")
           (string :tag "Listings language"))))



(defvar org-e-platypus-custom-lang-environments nil
  "Alist mapping languages to language-specific Man environments.

It is used during export of src blocks by the listings and
man packages.  For example,

  \(setq org-e-platypus-custom-lang-environments
     '\(\(python \"pythoncode\"\)\)\)

would have the effect that if org encounters begin_src python
during man export."
)




;;; Plain text

(defcustom org-e-platypus-quotes
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
  :group 'org-export-e-platypus
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


;;; Compilation

(defcustom org-e-platypus-pdf-process
  '("platyrun -verbose  %f %b.pdf")

  "Commands to process a Man file to a PDF file.
This is a list of strings, each of them will be given to the
shell as a command.  %f in the command will be replaced by the
full file name, %b by the file base name \(i.e. without
extension) and %o by the base directory of the file.


By default, Org uses 3 runs of to do the processing.

Alternatively, this may be a Lisp function that does the
processing.  This function should accept the file name as
its single argument."
  :group 'org-export-pdf
  :type '(choice
          (repeat :tag "Shell command sequence"
                  (string :tag "Shell command"))
          (const :tag "2 runs of pdfgroff"
                 ("tbl %f | eqn | groff -mm | ps2pdf - > %b.pdf"
                  "tbl %f | eqn | groff -mm | ps2pdf - > %b.pdf"))
          (const :tag "3 runs of pdfgroff"
                 ("tbl %f | eqn | groff -mm | ps2pdf - > %b.pdf"
                  "tbl %f | eqn | groff -mm | ps2pdf - > %b.pdf"
                  "tbl %f | eqn | groff -mm | ps2pdf - > %b.pdf"))
          (function)))

(defcustom org-e-platypus-logfiles-extensions nil
  "The list of file extensions to consider as Man logfiles."
  :group 'org-export-e-platypus
  :type '(repeat (string :tag "Extension")))

(defcustom org-e-platypus-remove-logfiles nil
  "Non-nil means remove the logfiles produced by PDF production.
These are the .aux, .log, .out, and .toc files."
  :group 'org-export-e-platypus
  :type 'boolean)

(defcustom org-e-platypus-default-font "TIMES_ROMAN"
  "Default Font"
  :group 'org-export-e-platypus
  :type 'string)




;; Preamble

;; Adding PLATYPUSas a block parser to make sure that its contents
;; does not execute

(add-to-list 'org-element-block-name-alist
             '("PLATYPUS" . org-element-export-block-parser))



;;; Internal Functions


(defun org-e-platypus--caption/label-string (caption label info)
  "Return caption and label Man string for floats.

CAPTION is a cons cell of secondary strings, the car being the
standard caption and the cdr its short form.  LABEL is a string
representing the label.  INFO is a plist holding contextual
information.

If there's no caption nor label, return the empty string.

For non-floats, see `org-e-platypus--wrap-label'."
  (let ((label-str "" ))
    (cond
     ((and (not caption) (not label)) "")
     ((not caption) (format "[+i]%s[-i]" label))
     ;; Option caption format with short name.
     ((cdr caption)
      (format "%s - %s - %s\n"
              (org-export-data (cdr caption) info)
              label-str
              (org-export-data (car caption) info)))
     ;; Standard caption format.
     (t (format "%s"
                (org-export-data (car caption) info))))))



(defun org-e-platypus--quotation-marks (text info)
  "Export quotation marks depending on language conventions.
TEXT is a string containing quotation marks to be replaced.  INFO
is a plist used as a communication channel."
  (mapc (lambda(l)
          (let ((start 0))
            (while (setq start (string-match (car l) text start))
              (let ((new-quote (concat (match-string 1 text) (cdr l))))
                (setq text (replace-match new-quote  t t text))))))
        (cdr (or (assoc (plist-get info :language) org-e-platypus-quotes)
                 ;; Falls back on English.
                 (assoc "en" org-e-platypus-quotes)))) text)

(defun org-e-platypus--wrap-label (element output)
  "Wrap label associated to ELEMENT around OUTPUT, if appropriate.
This function shouldn't be used for floats.  See
`org-e-platypus--caption/label-string'."
  (let ((label (org-element-property :name element)))
    (if (or (not output) (not label) (string= output "") (string= label ""))
        output
      (concat (format "%s[]\n" label) output))))


;;; Template

(defun org-e-platypus-template (contents info)
  "Return complete document string after Man conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let* ((title (org-export-data (plist-get info :title) info))
        (attr (read (format "(%s)"
                            (mapconcat
                             #'identity
                             (list (plist-get info :man-class-options))
                             " "))))
        (section-item (plist-get attr :section-id)))

    (concat
     "[pagesize:LETTER]\n"
     "[paraindent:0.25i]\n"
     (format "[ff:%s]\n" org-e-platypus-default-font)
     (cond
      (title
       (format "\\n\n\n\n[align:center]%s\n\n[align:left]\n[pg]\n" title)))
     contents)))




;;; Transcode Functions

;;; Babel Call
;;
;; Babel Calls are ignored.


;;; Bold

(defun org-e-platypus-bold (bold contents info)
  "Transcode BOLD from Org to Man.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (format "[+b]%s[-b]" contents))


;;; Center Block

(defun org-e-platypus-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to Man.
CONTENTS holds the contents of the center block.  INFO is a plist
holding contextual information."
  (org-e-platypus--wrap-label
   center-block
   (format "[align:center]%s\n\n[align:left]"
           contents)))


;;; Clock

(defun org-e-platypus-clock (clock contents info)
  "Transcode a CLOCK element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  contents)


;;; Code

(defun org-e-platypus-code (code contents info)
  "Transcode a CODE object from Org to Man.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "[code]%s[-code]" code))


;;; Comment
;;
;; Comments are ignored.


;;; Comment Block
;;
;; Comment Blocks are ignored.


;;; Drawer

(defun org-e-platypus-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to Man.
   DRAWER holds the drawer information
   CONTENTS holds the contents of the block.
   INFO is a plist holding contextual information. "
  contents)


;;; Dynamic Block

(defun org-e-platypus-dynamic-block (dynamic-block contents info)
  "Transcode a DYNAMIC-BLOCK element from Org to Man.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See `org-export-data'."
  (org-e-platypus--wrap-label dynamic-block contents))


;;; Entity

(defun org-e-platypus-entity (entity contents info)
  "Transcode an ENTITY object from Org to Man.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (let ((ent (org-element-property :utf8 entity))) ent))


;;; Example Block

(defun org-e-platypus-example-block (example-block contents info)
  "Transcode an EXAMPLE-BLOCK element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (org-e-platypus--wrap-label
   example-block
   (format "[code]%s[-code]"
           (org-export-format-code-default example-block info))))
;;; Export Block

(defun org-e-platypus-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "PLATYPUS")
    (org-remove-indentation (org-element-property :value export-block))))


;;; Export Snippet

(defun org-e-platypus-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'e-platypus)
    (org-element-property :value export-snippet)))


;;; Fixed Width

(defun org-e-platypus-fixed-width (fixed-width contents info)
  "Transcode a FIXED-WIDTH element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-e-platypus--wrap-label
   fixed-width
   (format "[ff:COURIER]%s[ff:%s]"
           (org-remove-indentation
            (org-element-property :value fixed-width))
           org-e-platypus-default-font)))


;;; Footnote Definition
;;
;; Footnote Definitions are ignored.

;;; Footnote References
;;
;; Footnote References are Ignored


;;; Headline

(defun org-e-platypus-headline (headline contents info)
  "Transcode an HEADLINE element from Org to Man.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((level (org-export-get-relative-level headline info))
		 (numberedp (org-export-numbered-headline-p headline info))
		 ;; Section formatting will set two placeholders: one for the
		 ;; title and the other for the contents.
		 (section-fmt
		  (case level
			(1 "[fsize:16pt]%s[fsize:12pt]\n%s")
			(2 "[fsize:14pt]%s[fsize:12pt]\n%s")
			(3 "[+i]%s[-i]\n%s")
			(t nil)))
		 (text (org-export-data (org-element-property :title headline) info)))

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
			  (when (org-export-first-sibling-p headline info)
				(format "%s\n" "[list]"))
			  ;; Itemize headline
			   text "[]\n"
			  contents "[-list]\n")))
		;; If headline is not the last sibling simply return
		;; LOW-LEVEL-BODY.  Otherwise, also close the list, before any
		;; blank line.
		(if (not (org-export-last-sibling-p headline info)) low-level-body
		  (replace-regexp-in-string
		   "[ \t\n]*\\'" ""
		   low-level-body))))

     ;; Case 3. Standard headline.  Export it as a section.
     (t (format section-fmt text contents )))))

;;; Horizontal Rule
;; Not supported

;;; Inline Babel Call
;;
;; Inline Babel Calls are ignored.

;;; Inline Src Block

(defun org-e-platypus-inline-src-block (inline-src-block contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to Man.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((code (org-element-property :value inline-src-block)))
    (cond
     (org-e-platypus-source-highlight
      (let* ((tmpdir (if (featurep 'xemacs)
                         temp-directory
                       temporary-file-directory ))
             (in-file  (make-temp-name
                        (expand-file-name "srchilite" tmpdir)))
             (out-file (make-temp-name
                        (expand-file-name "reshilite" tmpdir)))
             (org-lang (org-element-property :language inline-src-block))
             (lst-lang (cadr (assq (intern org-lang)
                                   org-e-platypus-source-highlight-langs)))

             (cmd (concat (expand-file-name "source-highlight")
                          " -s " lst-lang
                          " -f groff_man"
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
          (format "[code]%s[-code]"
                  code))))

     ;; Do not use a special package: transcode it verbatim.
     (t
      (concat "[code]" code "\n"
              "[-code]\n")))))


;;; Inlinetask
;;; Italic

(defun org-e-platypus-italic (italic contents info)
  "Transcode ITALIC from Org to Man.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (format "[+i]%s[-i]" contents))


;;; Item


(defun org-e-platypus-item (item contents info)

  "Transcode an ITEM element from Org to Man.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."

  (let* ((bullet (org-element-property :bullet item))
         (type (org-element-property :type (org-element-property :parent item)))
         (checkbox (case (org-element-property :checkbox item)
                     (on "[X]")			;;
                     (off "[ ]")					;;
                     (trans "[-]"))) ;;

         (tag (let ((tag (org-element-property :tag item)))
                ;; Check-boxes must belong to the tag.
                (and tag (format "[+b]%s[-b]"
                                 (concat checkbox
                                         (org-export-data tag info)))))))

    (if (and (null tag )
			 (null checkbox))
		(let* ((bullet (org-trim bullet))
			   (marker (cond  ((string= "-" bullet) "-")
							  ((string= "*" bullet) "*")
							  ((eq type 'ordered)
							   (format "%s " (org-trim bullet)))
							  (t "*"))))
		  (concat marker "[]\n"
				  (org-trim (or contents " " "[]\n"))))
      (concat (or tag (concat " " checkbox)) "[]\n"
              (org-trim (or contents " " "[]\n"))))))

;;; Keyword


(defun org-e-platypus-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
        (value (org-element-property :value keyword)))
    (cond
     ((string= key "MAN") value))))


;;; Man Environment

(defun org-e-platypus-man-environment (man-environment contents info)
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


;;; Man Fragment

(defun org-e-platypus-man-fragment (man-fragment contents info)
  "Transcode a MAN-FRAGMENT object from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-property :value man-fragment))


;;; Line Break

(defun org-e-platypus-line-break (line-break contents info)
  "Transcode a LINE-BREAK object from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  "[]\n")


;;; Link


(defun org-e-platypus-link (link desc info)
  "Transcode a LINK object from Org to Man.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."

  (let* ((type (org-element-property :type link))
         (raw-path (org-element-property :path link))
         ;; Ensure DESC really exists, or set it to nil.
         (desc (and (not (string= desc "")) desc))

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
     ;; External link with a description part.
     ((and path desc) (format "[+url:%s]%s[-url]" path desc))
     ;; External link without a description part.
     (path (format "[url:%s]" path))
     ;; No path, only description. 
     (t (format "[+i]%s[-i]" desc)))))

;;; Macro

(defun org-e-platypus-macro (macro contents info)
  "Transcode a MACRO element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;; Use available tools.
  (org-export-expand-macro macro info))


;;; Paragraph

(defun org-e-platypus-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to Man.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (let ((parent (plist-get (nth 1 paragraph) :parent)))
    (when parent
      (let ((parent-type (car parent))
            (fixed-paragraph ""))
        (cond ((and (eq parent-type 'item)
                    (plist-get (nth 1 parent) :bullet ))
               (setq fixed-paragraph (concat "" contents)))
              ((eq parent-type 'section)
               (setq fixed-paragraph (concat "\n\n" contents)))
              ((eq parent-type 'footnote-definition)
               (setq fixed-paragraph contents))
              (t (setq fixed-paragraph (concat "" contents))))
        fixed-paragraph ))))


;;; Plain List

(defun org-e-platypus-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to Man.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (concat "[list]" contents "[-list]")
)


;;; Plain Text

(defun org-e-platypus-plain-text (text info)
  "Transcode a TEXT string from Org to Man.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  ;; Protect
  ;; Handle quotation marks
  (setq text (org-e-platypus--quotation-marks text info))
  ;; Return value.
  text)



;;; Planning


;;; Property Drawer


;;; Quote Block

(defun org-e-platypus-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to Man.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (org-e-platypus--wrap-label
   quote-block
   (format "[code]\n%s\n[-code]" contents)))

;;; Quote Section

(defun org-e-platypus-quote-section (quote-section contents info)
  "Transcode a QUOTE-SECTION element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((value (org-remove-indentation
                (org-element-property :value quote-section))))
    (when value (format "[code]\n%s[-code]\n" value))))


;;; Radio Target

(defun org-e-platypus-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to Man.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  text )


;;; Section

(defun org-e-platypus-section (section contents info)
  "Transcode a SECTION element from Org to Man.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  contents)


;;; Special Block

(defun org-e-platypus-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to Man.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((type (downcase (org-element-property :type special-block))))
    (org-e-platypus--wrap-label
     special-block
     (format "%s\n" contents))))


;;; Src Block

(defun org-e-platypus-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to Man.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."

  (let* ((lang (org-element-property :language src-block))
         (caption (org-element-property :caption src-block))
         (label (org-element-property :name src-block))
         (code (org-element-property :value src-block))
         (custom-env (and lang
                          (cadr (assq (intern lang)
                                      org-e-platypus-custom-lang-environments))))
         (num-start (case (org-element-property :number-lines src-block)
                      (continued (org-export-get-loc src-block info))
                      (new 0)))
         (retain-labels (org-element-property :retain-labels src-block)))
    (cond
     ;; Case 1.  No source fontification.
     ((not org-e-platypus-source-highlight)
      (let ((caption-str (org-e-platypus--caption/label-string caption label info)))
         (concat
          (format "[code]%s[-code]"
                  (org-export-format-code-default src-block info)))))
     ( (and org-e-platypus-source-highlight)
       (let* ((tmpdir (if (featurep 'xemacs)
                          temp-directory
                        temporary-file-directory ))

              (in-file  (make-temp-name
                         (expand-file-name "srchilite" tmpdir)))
              (out-file (make-temp-name
                         (expand-file-name "reshilite" tmpdir)))

              (org-lang (org-element-property :language src-block))
              (lst-lang (cadr (assq (intern org-lang)
                                    org-e-platypus-source-highlight-langs)))

              (cmd (concat "source-highlight"
                           " -s " lst-lang
                           " -f groff_man "
                           " -i " in-file
                           " -o " out-file)))

         (if lst-lang
             (let ((code-block "" ))
               (with-temp-file in-file (insert code))
               (shell-command cmd)
               (setq code-block  (org-file-contents out-file))
               (delete-file in-file)
               (delete-file out-file)
               code-block)
           (format "[code]%s[-code]"
                   code)))))))


;;; Statistics Cookie

(defun org-e-platypus-statistics-cookie (statistics-cookie contents info)
  "Transcode a STATISTICS-COOKIE object from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-property :value statistics-cookie))


;;; Strike-Through

(defun org-e-platypus-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to Man.
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (format "[+st]%s[-st]" contents))

;;; Subscript

(defun org-e-platypus-subscript (subscript contents info)
  "Transcode a SUBSCRIPT object from Org to Man.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format  "%s" contents))

;;; Superscript "^_%s$

(defun org-e-platypus-superscript (superscript contents info)
  "Transcode a SUPERSCRIPT object from Org to Man.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format  "%s" contents))


;;; Table
;;
;; `org-e-platypus-table' is the entry point for table transcoding.  It
;; takes care of tables with a "verbatim" attribute.  Otherwise, it
;; delegates the job to either `org-e-platypus-table--table.el-table' or
;; `org-e-platypus-table--org-table' functions, depending of the type of
;; the table.
;;
;; `org-e-platypus-table--align-string' is a subroutine used to build
;; alignment string for Org tables.

(defun org-e-platypus-table (table contents info)
  "Transcode a TABLE element from Org to Man.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
(format "[code]%s[-code]"
            ;; Re-create table, without affiliated keywords.
            (org-trim
             (org-element-interpret-data
              `(table nil ,@(org-element-contents table))))))

;;; Table Cell

(defun org-e-platypus-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to Man
CONTENTS is the cell contents.  INFO is a plist used as
a communication channel."
    (concat (if (and contents
                     org-e-platypus-table-scientific-notation
                     (string-match orgtbl-exp-regexp contents))
                ;; Use appropriate format string for scientific
                ;; notation.
              (format org-e-platypus-table-scientific-notation
                        (match-string 1 contents)
                        (match-string 2 contents))
              contents )
            (when (org-export-get-next-element table-cell info) "\t")))


;;; Table Row

(defun org-e-platypus-table-row (table-row contents info)
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
       ;; Mark horizontal lines
       (cond  ((and (memq 'top borders) (memq 'above borders)) "_\n"))
       contents

       (cond
        ;; When BOOKTABS are activated enforce bottom rule even when
        ;; no hline was specifically marked.
        ((and (memq 'bottom borders) (memq 'below borders)) "\n_")
        ((memq 'below borders) "\n_"))))))


;;; Target

(defun org-e-platypus-target (target contents info)
  "Transcode a TARGET object from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "[+i]%s[-i]"
          (org-export-solidify-link-text (org-element-property :value target))))


;;; Timestamp

(defun org-e-platypus-timestamp (timestamp contents info)
  "Transcode a TIMESTAMP object from Org to Man.
  CONTENTS is nil.  INFO is a plist holding contextual
  information."
  "contents" )


;;; Underline

(defun org-e-platypus-underline (underline contents info)
  "Transcode UNDERLINE from Org to Man.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (format "[+u]%s[-i]" contents))


;;; Verbatim

(defun org-e-platypus-verbatim (verbatim contents info)
  "Transcode a VERBATIM object from Org to Man.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "[code]%s[-code]" contents))


;;; Verse Block

(defun org-e-platypus-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to Man.
CONTENTS is verse block contents. INFO is a plist holding
contextual information."
  (format ".RS\n.ft I\n%s\n.ft\n.RE" contents))



;;; Interactive functions

(defun org-e-platypus-export-to-pla
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

When optional argument BODY-ONLY is non-nil, only the body
without any markers.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".pla"  subtreep pub-dir)))
    (org-export-to-file
     'e-platypus outfile subtreep visible-only body-only ext-plist)))

(defun org-e-platypus-export-to-pdf
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

When optional argument BODY-ONLY is non-nil, only write between
markers.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return PDF file's name."
  (interactive)
  (org-e-platypus-compile
   (org-e-platypus-export-to-man
    subtreep visible-only body-only ext-plist pub-dir)))

(defun org-e-platypus-compile (grofffile)
  "Compile a Groff file.

GROFFFILE is the name of the file being compiled.  Processing is
done through the command specified in `org-e-platypus-pdf-process'.

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
           ((functionp org-e-platypus-pdf-process)
            (funcall org-e-platypus-pdf-process (shell-quote-argument grofffile)))
           ;; A list is provided: Replace %b, %f and %o with appropriate
           ;; values in each command before applying it.  Output is
           ;; redirected to "*Org PDF Groff Output*" buffer.
           ((consp org-e-platypus-pdf-process)
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
               org-e-platypus-pdf-process)
              ;; Collect standard errors from output buffer.
              (setq errors (org-e-platypus-collect-errors outbuf))))
           (t (error "No valid command to process to PDF")))
          (let ((pdffile (concat base ".pdf")))
            ;; Check for process failure.  Provide collected errors if
            ;; possible.
            (if (not (file-exists-p pdffile))
                (error (concat (format "PDF file %s wasn't produced" pdffile)
                               (when errors (concat ": " errors))))
              ;; Else remove log files, when specified, and signal end of
              ;; process to user, along with any error encountered.
              (when org-e-platypus-remove-logfiles
                (dolist (ext org-e-platypus-logfiles-extensions)
                  (let ((file (concat base "." ext)))
                    (when (file-exists-p file) (delete-file file)))))
              (message (concat "Process completed"
                               (if (not errors) "."
                                 (concat " with errors: " errors)))))
            ;; Return output file name.
            pdffile))
      (set-window-configuration wconfig))))

(defun org-e-platypus-collect-errors (buffer)
  "Collect some kind of errors from \"groff\" output
BUFFER is the buffer containing output.
Return collected error types as a string, or nil if there was
none."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      ;; Find final run
      nil )))


(provide 'org-e-platypus)
;;; org-e-platypus.el ends here
