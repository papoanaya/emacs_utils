;; org-platypus.el --- Man Back-End For Org Export Engine

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
;;   M-: (org-export-to-buffer 'platypus "*Test platypus*") RET
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

(defvar org-export-platypus-default-packages-alist)
(defvar org-export-platypus-packages-alist)
(defvar org-export-platypus-header-alist
  '(("1" . 1)
    ("2" . 1)
    ("3" . 1)
    ("4" . 1)))

(defvar org-export-platypus-current-level 1)



;;; Define Back-End

(defvar org-platypus-translate-alist
  '((babel-call . org-platypus-babel-call)
    (bold . org-platypus-bold)
    (center-block . org-platypus-center-block)
    (clock . org-platypus-clock)
    (code . org-platypus-code)
    (comment . org-platypus-comment)
    (comment-block . org-platypus-comment-block)
    (drawer . org-platypus-drawer)
    (dynamic-block . org-platypus-dynamic-block)
    (entity . org-platypus-entity)
    (example-block . org-platypus-example-block)
    (export-block . org-platypus-export-block)
    (export-snippet . org-platypus-export-snippet)
    (fixed-width . org-platypus-fixed-width)
    (footnote-definition . org-platypus-footnote-definition)
    (footnote-reference . org-platypus-footnote-reference)
    (headline . org-platypus-headline)
    (horizontal-rule . org-platypus-horizontal-rule)
    (inline-babel-call . org-platypus-inline-babel-call)
    (inline-src-block . org-platypus-inline-src-block)
    (inlinetask . org-platypus-inlinetask)
    (italic . org-platypus-italic)
    (item . org-platypus-item)
    (keyword . org-platypus-keyword)
    (man-environment . org-platypus-platypus-environment)
    (man-fragment . org-platypus-platypus-fragment)
    (line-break . org-platypus-line-break)
    (link . org-platypus-link)
    (paragraph . org-platypus-paragraph)
    (plain-list . org-platypus-plain-list)
    (plain-text . org-platypus-plain-text)
    (planning . org-platypus-planning)
    (property-drawer . org-platypus-property-drawer)
    (quote-block . org-platypus-quote-block)
    (quote-section . org-platypus-quote-section)
    (radio-target . org-platypus-radio-target)
    (section . org-platypus-section)
    (special-block . org-platypus-special-block)
    (src-block . org-platypus-src-block)
    (statistics-cookie . org-platypus-statistics-cookie)
    (strike-through . org-platypus-strike-through)
    (subscript . org-platypus-subscript)
    (superscript . org-platypus-superscript)
    (table . org-platypus-table)
    (table-cell . org-platypus-table-cell)
    (table-row . org-platypus-table-row)
    (target . org-platypus-target)
    (template . org-platypus-template)
    (timestamp . org-platypus-timestamp)
    (underline . org-platypus-underline)
    (verbatim . org-platypus-verbatim)
    (verse-block . org-platypus-verse-block))
  "Alist between element or object types and translators.")

(defconst org-platypus-options-alist
  '((:date "DATE" nil nil t)
    (:man-class "MAN_CLASS" nil nil t)
    (:man-class-options "MAN_CLASS_OPTIONS" nil nil t)
    (:man-header-extra "MAN_HEADER" nil nil newline))
  "Alist between Man export properties and ways to set them.
See `org-export-options-alist' for more information on the
structure of the values.")



;;; User Configurable Variables

(defgroup org-export-platypus nil
  "Options for exporting Org mode files to Man."
  :tag "Org Export Man"
  :group 'org-export)

;;; Tables


(defcustom org-platypus-table-scientific-notation "%sE%s"
  "Format string to display numbers in scientific notation.
The format should have \"%s\" twice, for mantissa and exponent
\(i.e. \"%s\\\\times10^{%s}\").

When nil, no transformation is made."
  :group 'org-export-platypus
  :type '(choice
          (string :tag "Format string")
          (const :tag "No formatting")))


;;; Inlinetasks
;; Src blocks

(defcustom org-platypus-source-highlight nil
  "Use GNU source highlight to embellish source blocks "
  :group 'org-export-platypus
  :type 'boolean)


(defcustom org-platypus-source-highlight-langs
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
  :group 'org-export-platypus
  :type '(repeat
          (list
           (symbol :tag "Major mode       ")
           (string :tag "Listings language"))))



(defvar org-platypus-custom-lang-environments nil
  "Alist mapping languages to language-specific Man environments.

It is used during export of src blocks by the listings and
man packages.  For example,

  \(setq org-platypus-custom-lang-environments
     '\(\(python \"pythoncode\"\)\)\)

would have the effect that if org encounters begin_src python
during man export."
)




;;; Plain text

(defcustom org-platypus-quotes
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
  :group 'org-export-platypus
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

(defcustom org-platypus-pdf-process
  '("~/platypus/platyrun -verbose  %f %b.pdf")

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
                 ("platyrun -verbose %f %b.pdf"
                  "platyrun -verbose %f %b.pdf"))
          (const :tag "3 runs of pdfgroff"
                 ("platyrun -verbose %f %b.pdf"
                  "platyrun -verbose %f %b.pdf"
                  "platyrun -verbose %f %b.pdf"))
          (function)))

(defcustom org-platypus-logfiles-extensions nil
  "The list of file extensions to consider as Man logfiles."
  :group 'org-export-platypus
  :type '(repeat (string :tag "Extension")))

(defcustom org-platypus-remove-logfiles nil
  "Non-nil means remove the logfiles produced by PDF production.
These are the .aux, .log, .out, and .toc files."
  :group 'org-export-platypus
  :type 'boolean)

(defcustom org-platypus-default-font "TIMES_ROMAN"
  "Default Font"
  :group 'org-export-platypus
  :type 'string)




;; Preamble

;; Adding PLATYPUSas a block parser to make sure that its contents
;; does not execute

(add-to-list 'org-element-block-name-alist
             '("PLATYPUS" . org-element-export-block-parser))



;;; Internal Functions


(defun org-platypus--caption/label-string (caption label info)
  "Return caption and label Man string for floats.

CAPTION is a cons cell of secondary strings, the car being the
standard caption and the cdr its short form.  LABEL is a string
representing the label.  INFO is a plist holding contextual
information.

If there's no caption nor label, return the empty string.

For non-floats, see `org-platypus--wrap-label'."
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



(defun org-platypus--quotation-marks (text info)
  "Export quotation marks depending on language conventions.
TEXT is a string containing quotation marks to be replaced.  INFO
is a plist used as a communication channel."
  (mapc (lambda(l)
          (let ((start 0))
            (while (setq start (string-match (car l) text start))
              (let ((new-quote (concat (match-string 1 text) (cdr l))))
                (setq text (replace-match new-quote  t t text))))))
        (cdr (or (assoc (plist-get info :language) org-platypus-quotes)
                 ;; Falls back on English.
                 (assoc "en" org-platypus-quotes)))) text)

(defun org-platypus--wrap-label (element output)
  "Wrap label associated to ELEMENT around OUTPUT, if appropriate.
This function shouldn't be used for floats.  See
`org-platypus--caption/label-string'."
  (let ((label (org-element-property :name element)))
    (if (or (not output) (not label) (string= output "") (string= label ""))
        output
      (concat (format "%s[]\n" label) output))))


(defun org-platypus--get-level (level)
  "Formats a level numbered header to simulate numbered headers"
  (let* ((level-count (assoc (number-to-string level) 
                             org-export-platypus-header-alist))
         (result ""))

    (if level-count
        (progn
          (dotimes (i level)
            (setq result 
                  (concat result 
                          (number-to-string 
                           (cdr (assoc (number-to-string (+ i 1))
                                       org-export-platypus-header-alist )))
                          ".")))
          (if (and (> org-export-platypus-current-level
                      1)              
                (< level org-export-platypus-current-level))
              (progn 
                (setcdr (assoc (number-to-string 
                                org-export-platypus-current-level)
                               org-export-platypus-header-alist)
                        1)
                (setq org-export-platypus-current-level level))
               (progn 
                 (setcdr (assoc (number-to-string level)
                                org-export-platypus-header-alist)
                         (+ (cdr level-count) 1 ))
                 (setq org-export-platypus-current-level level))))
               "") (concat result "  ")))




;;; Template

(defun org-platypus-template (contents info)
  "Return complete document string after Man conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let* ((title (org-export-data (plist-get info :title) info))
         (author (and (plist-get info :with-author)
                      (let ((auth (plist-get info :author)))
                        (and auth (org-export-data auth info)))))
         (email (and (plist-get info :with-email)
                     (org-export-data (plist-get info :email) info)))
         (date (org-export-data (plist-get info :date) info)))

         (concat
          "[pagesize:LETTER]\n"
          (format "[ff:%s]" org-platypus-default-font)
          (when title
           (format 
            "\n\n\n\n\n[align:center][fsize:18pt]%s\n\n[fsize:12pt][align:left]\n" 
            title))
          (when author (format  "[align:center]%s\n\n[align:left]" author))
          (when date (format  "[align:center]%s\n\n[align:left]" date))
          "\n[pg]\n"
          contents)))




;;; Transcode Functions

;;; Babel Call
;;
;; Babel Calls are ignored.


;;; Bold

(defun org-platypus-bold (bold contents info)
  "Transcode BOLD from Org to Man.
CONTENTS is the text with bold markup.  INFO is a plist holding
contextual information."
  (format "[+b]%s[-b]" contents))


;;; Center Block

(defun org-platypus-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to Man.
CONTENTS holds the contents of the center block.  INFO is a plist
holding contextual information."
  (org-platypus--wrap-label
   center-block
   (format "[align:center]%s\n\n[align:left]"
           contents)))


;;; Clock

(defun org-platypus-clock (clock contents info)
  "Transcode a CLOCK element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual
information."
(concat "[+i]" contents "[-i]\n\n"))


;;; Code

(defun org-platypus-code (code contents info)
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

(defun org-platypus-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to Man.
   DRAWER holds the drawer information
   CONTENTS holds the contents of the block.
   INFO is a plist holding contextual information. "
  contents)


;;; Dynamic Block

(defun org-platypus-dynamic-block (dynamic-block contents info)
  "Transcode a DYNAMIC-BLOCK element from Org to Man.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See `org-export-data'."
  (org-platypus--wrap-label dynamic-block contents))


;;; Entity

(defun org-platypus-entity (entity contents info)
  "Transcode an ENTITY object from Org to Man.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
  (let ((ent (org-element-property :utf8 entity))) ent))


;;; Example Block

(defun org-platypus-example-block (example-block contents info)
  "Transcode an EXAMPLE-BLOCK element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (org-platypus--wrap-label
   example-block
   (format "[code]%s[-code]"
           (org-export-format-code-default example-block info))))
;;; Export Block

(defun org-platypus-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (string= (org-element-property :type export-block) "PLATYPUS")
    (org-remove-indentation (org-element-property :value export-block))))


;;; Export Snippet

(defun org-platypus-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'platypus)
    (org-element-property :value export-snippet)))


;;; Fixed Width

(defun org-platypus-fixed-width (fixed-width contents info)
  "Transcode a FIXED-WIDTH element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-platypus--wrap-label
   fixed-width
   (format "[ff:COURIER]%s[ff:%s]"
           (org-remove-indentation
            (org-element-property :value fixed-width))
           org-platypus-default-font)))


;;; Footnote Definition
;;
;; Footnote Definitions are ignored.

;;; Footnote References
;;
;; Footnote References are Ignored


;;; Headline

(defun org-platypus-headline (headline contents info)
  "Transcode an HEADLINE element from Org to Man.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let* ((level (org-export-get-relative-level headline info))
		 (numberedp (org-export-numbered-headline-p headline info))
		 ;; Section formatting will set two placeholders: one for the
		 ;; title and the other for the contents.
		 (section-fmt
		  (case level
			(1 "\n[fsize:16pt]%s[fsize:12pt]\n%s")
			(2 "\n[fsize:14pt]%s[fsize:12pt]\n%s")
			(3 "\n[+i]%s[-i]\n%s")
			(t nil)))
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

         (full-text  (concat
                       (when todo
                         (format "[+b]%s\[-b]  " todo))
                       (when priority (format "  [\\#%c]  " priority))
                       text
                       (when tags
                         (format " [+u]:%s:[-u] "
                                 (mapconcat 'identity tags ":"))))))

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
				"[list]\n")
			  ;; Itemize headline
			   text "[]\n"
			  contents "[-list]\n\n")))
		;; If headline is not the last sibling simply return
		;; LOW-LEVEL-BODY.  Otherwise, also close the list, before any
		;; blank line.
		(if (not (org-export-last-sibling-p headline info)) low-level-body
		  (replace-regexp-in-string
		   "[ \t\n]*\\'" ""
		   low-level-body))))

     ;; Case 3. Standard headline.  Export it as a section.
;;       (numberedp  
;;        (format section-fmt
;;                (concat  (org-platypus--get-level level)
;;                         full-text) (or contents " ")))
     (t (format section-fmt full-text (or contents " ") )))))

;;; Horizontal Rule
;; Not supported

;;; Inline Babel Call
;;
;; Inline Babel Calls are ignored.

;;; Inline Src Block

(defun org-platypus-inline-src-block (inline-src-block contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to Man.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((code (org-element-property :value inline-src-block)))
    (cond
     (org-platypus-source-highlight
      (let* ((tmpdir (if (featurep 'xemacs)
                         temp-directory
                       temporary-file-directory ))
             (in-file  (make-temp-name
                        (expand-file-name "srchilite" tmpdir)))
             (out-file (make-temp-name
                        (expand-file-name "reshilite" tmpdir)))
             (org-lang (org-element-property :language inline-src-block))
             (lst-lang (cadr (assq (intern org-lang)
                                   org-platypus-source-highlight-langs)))

             (cmd (concat (expand-file-name "source-highlight")
                          " -s " lst-lang
                          " -f platypus"
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

(defun org-platypus-italic (italic contents info)
  "Transcode ITALIC from Org to Man.
CONTENTS is the text with italic markup.  INFO is a plist holding
contextual information."
  (format "[+i]%s[-i]" contents))


;;; Item


(defun org-platypus-item (item contents info)

  "Transcode an ITEM element from Org to Man.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."

  (let* ((bullet (org-element-property :bullet item))
         (type (org-element-property :type (org-element-property :parent item)))
         (checkbox (case (org-element-property :checkbox item)
                     (on "[X]")
                     (off "[ ]")
                     (trans "[-]")))

         (tag (let ((tag (org-element-property :tag item)))
                ;; Check-boxes must belong to the tag.
                (and tag (format "%s"
                                 (concat checkbox
                                         (org-export-data tag info))))))
         (marker (cond  ((eq type 'ordered)  
                         (format "%s  " (org-trim bullet)))
                        (t "") )))

    (cond ((eq type 'descriptive)
           (concat 
            (format "\n[paraindent:0][+b][+u]%s[-b][-u]\n\n" tag )
            "[paraindent:36pt]"
            (org-trim (or contents " ")) 
            "[]"))
           (t
            (concat marker
                    (when tag (format "    [+u]%s[-u]    " tag ))
                    (when checkbox (format "   [code]%s[-code]   " checkbox))
                    (org-trim (or contents " " )) "[]\n"))) ))

;;; Keyword


(defun org-platypus-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
        (value (org-element-property :value keyword)))
    (cond
     ((string= key "PLATYPUS") value))))


;;; Platypus Environment

(defun org-platypus-platypus-environment (platypus-environment contents info)
  "Transcode a MAN-ENVIRONMENT element from Org to Platypus
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

(defun org-platypus-platypus-fragment (man-fragment contents info)
  "Transcode a MAN-FRAGMENT object from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-property :value man-fragment))


;;; Line Break

(defun org-platypus-line-break (line-break contents info)
  "Transcode a LINE-BREAK object from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  "[]\n")


;;; Link


(defun org-platypus-link (link desc info)
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


;;; Paragraph

(defun org-platypus-paragraph (paragraph contents info)
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
               (setq fixed-paragraph (concat "\n" contents)))
              ((eq parent-type 'footnote-definition)
               (setq fixed-paragraph contents))
              (t (setq fixed-paragraph (concat "" contents))))
        fixed-paragraph ))))


;;; Plain List

(defun org-platypus-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to Man.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* ((type (org-element-property :type plain-list))
         (bullet (org-trim
                  (org-element-property :bullet (nth 2 plain-list))))
         (marker 
          (cond
           ((string= "-" bullet) "{--}")
           ((string= "*" bullet) "{bullet}")
           (t "{ring}")))

         (platypus-format (cond
                           ((eq type 'unordered) 
                            (concat 
                             "\n[list|bullet:" 
                             marker
                             "]\n%s[-list]\n\n"))
                           ((eq type 'descriptive)
                            "\n%s\n[paraindent:0]\n\n")
                           (t "\n%s"))))
    (format platypus-format contents)))


;;; Plain Text

(defun org-platypus-plain-text (text info)
  "Transcode a TEXT string from Org to Man.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  ;; Protect
  ;; Handle quotation marks
  (setq text (org-platypus--quotation-marks text info))
  ;; Return value.
  text)



;;; Planning


;;; Property Drawer


;;; Quote Block

(defun org-platypus-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to Man.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (org-platypus--wrap-label
   quote-block
   (format "[code]\n%s\n[-code]" contents)))

;;; Quote Section

(defun org-platypus-quote-section (quote-section contents info)
  "Transcode a QUOTE-SECTION element from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((value (org-remove-indentation
                (org-element-property :value quote-section))))
    (when value (format "[code]\n%s[-code]\n" value))))


;;; Radio Target

(defun org-platypus-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to Man.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  text )


;;; Section

(defun org-platypus-section (section contents info)
  "Transcode a SECTION element from Org to Man.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  contents)


;;; Special Block

(defun org-platypus-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to Man.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let ((type (downcase (org-element-property :type special-block))))
    (org-platypus--wrap-label
     special-block
     (format "%s\n" contents))))


;;; Src Block

(defun org-platypus-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to Man.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."

  (let* ((lang (org-element-property :language src-block))
         (caption (org-element-property :caption src-block))
         (label (org-element-property :name src-block))
         (code (org-element-property :value src-block))
         (custom-env (and lang
                          (cadr (assq (intern lang)
                                      org-platypus-custom-lang-environments))))
         (num-start (case (org-element-property :number-lines src-block)
                      (continued (org-export-get-loc src-block info))
                      (new 0)))
         (retain-labels (org-element-property :retain-labels src-block)))
    (cond
     ;; Case 1.  No source fontification.
     ((not org-platypus-source-highlight)
      (let ((caption-str (org-platypus--caption/label-string caption label info)))
         (concat
          (format "[code]%s[-code]"
                  (org-export-format-code-default src-block info)))))
     ( (and org-platypus-source-highlight)
       (let* ((tmpdir (if (featurep 'xemacs)
                          temp-directory
                        temporary-file-directory ))

              (in-file  (make-temp-name
                         (expand-file-name "srchilite" tmpdir)))
              (out-file (make-temp-name
                         (expand-file-name "reshilite" tmpdir)))

              (org-lang (org-element-property :language src-block))
              (lst-lang (cadr (assq (intern org-lang)
                                    org-platypus-source-highlight-langs)))

              (cmd (concat "source-highlight"
                           " -s " lst-lang
                           " -f platypus "
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

(defun org-platypus-statistics-cookie (statistics-cookie contents info)
  "Transcode a STATISTICS-COOKIE object from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (org-element-property :value statistics-cookie))


;;; Strike-Through

(defun org-platypus-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to Man.
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (format "[+st]%s[-st]" contents))

;;; Subscript

(defun org-platypus-subscript (subscript contents info)
  "Transcode a SUBSCRIPT object from Org to Man.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format  "%s" contents))

;;; Superscript "^_%s$

(defun org-platypus-superscript (superscript contents info)
  "Transcode a SUPERSCRIPT object from Org to Man.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (format  "%s" contents))


;;; Table
;;
;; `org-platypus-table' is the entry point for table transcoding.  It
;; takes care of tables with a "verbatim" attribute.  Otherwise, it
;; delegates the job to either `org-platypus-table--table.el-table' or
;; `org-platypus-table--org-table' functions, depending of the type of
;; the table.
;;
;; `org-platypus-table--align-string' is a subroutine used to build
;; alignment string for Org tables.

(defun org-platypus-table (table contents info)
  "Transcode a TABLE element from Org to Man.
CONTENTS is the contents of the table.  INFO is a plist holding
contextual information."
(format "\n\n[code]\n%s\n[-code]\n\n"
            ;; Re-create table, without affiliated keywords.
            (org-trim
             (org-element-interpret-data
              `(table nil ,@(org-element-contents table))))))

;;; Table Cell

(defun org-platypus-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL element from Org to Man
CONTENTS is the cell contents.  INFO is a plist used as
a communication channel."
    (concat (if (and contents
                     org-platypus-table-scientific-notation
                     (string-match orgtbl-exp-regexp contents))
                ;; Use appropriate format string for scientific
                ;; notation.
              (format org-platypus-table-scientific-notation
                        (match-string 1 contents)
                        (match-string 2 contents))
              contents )
            (when (org-export-get-next-element table-cell info) "\t")))


;;; Table Row

(defun org-platypus-table-row (table-row contents info)
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

(defun org-platypus-target (target contents info)
  "Transcode a TARGET object from Org to Man.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "[+i]%s[-i]"
          (org-export-solidify-link-text (org-element-property :value target))))


;;; Timestamp

(defun org-platypus-timestamp (timestamp contents info)
  "Transcode a TIMESTAMP object from Org to Man.
  CONTENTS is nil.  INFO is a plist holding contextual
  information."
contents)


;;; Underline

(defun org-platypus-underline (underline contents info)
  "Transcode UNDERLINE from Org to Man.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (format "[+u]%s[-u]" contents))


;;; Verbatim

(defun org-platypus-verbatim (verbatim contents info)
  "Transcode a VERBATIM object from Org to Man.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "[ff:COURIER]%s[ff:%s]" 
          (org-element-property :value verbatim)
          org-platypus-default-font))


;;; Verse Block

(defun org-platypus-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to Man.
CONTENTS is verse block contents. INFO is a plist holding
contextual information."
  (format "[code][+i]%s[-i][-code]" contents))



;;; Interactive functions

(defun org-platypus-export-to-platypus
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
  (setq  org-export-platypus-header-alist
         '(("1" . 1)
           ("2" . 1)
           ("3" . 1)
           ("4" . 1)))
  (setq org-export-platypus-current-level 1)

  (let ((outfile (org-export-output-file-name ".pla"  subtreep pub-dir)))
    (org-export-to-file
     'platypus outfile subtreep visible-only body-only ext-plist)))

(defun org-platypus-export-to-pdf
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
  (org-platypus-compile
   (org-platypus-export-to-platypus
    subtreep visible-only body-only ext-plist pub-dir)))

(defun org-platypus-compile (platypusfile)
  "Compile a Platypus file.

PLATYPUSFILE is the name of the file being compiled.  Processing is
done through the command specified in `org-platypus-pdf-process'.

Return PDF file name or an error if it couldn't be produced."
  (let* ((wconfig (current-window-configuration))
         (platypusfile (file-truename platypusfile))
         (base (file-name-sans-extension platypusfile))
         errors)
    (message (format "Processing Platypus file %s ..." platypusfile))
    (unwind-protect
        (progn
          (cond
           ;; A function is provided: Apply it.
           ((functionp org-platypus-pdf-process)
            (funcall org-platypus-pdf-process (shell-quote-argument platypusfile)))
           ;; A list is provided: Replace %b, %f and %o with appropriate
           ;; values in each command before applying it.  Output is
           ;; redirected to "*Org PDF Platypus Output*" buffer.
           ((consp org-platypus-pdf-process)
            (let* ((out-dir (or (file-name-directory platypusfile) "./"))
                   (outbuf (get-buffer-create "*Org PDF Platypus Output*")))
              (mapc
               (lambda (command)
                 (shell-command
                  (replace-regexp-in-string
                   "%b" (shell-quote-argument base)
                   (replace-regexp-in-string
                    "%f" (shell-quote-argument platypusfile)
                    (replace-regexp-in-string
                     "%o" (shell-quote-argument out-dir) command t t) t t) t t)
                  outbuf))
               org-platypus-pdf-process)
              ;; Collect standard errors from output buffer.
              (setq errors (org-platypus-collect-errors outbuf))))
           (t (error "No valid command to process to PDF")))
          (let ((pdffile (concat base ".pdf")))
            ;; Check for process failure.  Provide collected errors if
            ;; possible.
            (if (not (file-exists-p pdffile))
                (error (concat (format "PDF file %s wasn't produced" pdffile)
                               (when errors (concat ": " errors))))
              ;; Else remove log files, when specified, and signal end of
              ;; process to user, along with any error encountered.
              (when org-platypus-remove-logfiles
                (dolist (ext org-platypus-logfiles-extensions)
                  (let ((file (concat base "." ext)))
                    (when (file-exists-p file) (delete-file file)))))
              (message (concat "Process completed"
                               (if (not errors) "."
                                 (concat " with errors: " errors)))))
            ;; Return output file name.
            pdffile))
      (set-window-configuration wconfig))))

(defun org-platypus-collect-errors (buffer)
  "Collect some kind of errors from \"platypus\" output
BUFFER is the buffer containing output.
Return collected error types as a string, or nil if there was
none."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      ;; Find final run
      nil )))


(provide 'org-platypus)
;;; org-platypus.el ends here
