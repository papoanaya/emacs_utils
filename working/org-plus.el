;;; org-plus.el --- Google Plus Back-End for Org Export Engine

;; Copyright (C) 2012  Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <n.goaziou@gmail.com>
;; Keywords: org, wp, tex

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

;; This library implements a Google Plus back-end (vanilla flavour) for
;; Org exporter, based on `e-html'.
;;
;; It provides two commands for export, depending on the desired
;; output: `org-plus-export-as-markdown' (temporary buffer) and
;; `org-plus-export-to-markdown' ("plus" file).

;;; Code:

(require 'org-e-html)



;;; User-Configurable Variables

(defgroup org-export-plus nil
  "Options specific to Google Plus export back-end."
  :tag "Org GPlus Markup"
  :group 'org-export
  :version "24.2")



;;; Define Back-End

(org-export-define-derived-backend plus e-html
  :export-block ("PLUS" "PLUS")
  :filters-alist ((:filter-parse-tree . org-plus-separate-elements))
  :translate-alist ((bold . org-plus-bold)
		    (code . org-plus-verbatim)
		    (example-block . org-plus-example-block)
		    (footnote-definition . ignore)
		    (footnote-reference . ignore)
		    (headline . org-plus-headline)
		    (horizontal-rule . org-plus-horizontal-rule)
		    (inline-src-block . org-plus-verbatim)
		    (italic . org-plus-italic)
		    (item . org-plus-item)
		    (line-break . org-plus-line-break)
		    (link . org-plus-link)
            (strike-through . org-e-plus-strike-through)
		    (paragraph . org-plus-paragraph)
		    (plain-list . org-plus-plain-list)
		    (plain-text . org-plus-plain-text)
		    (quote-block . org-plus-quote-block)
		    (quote-section . org-plus-example-block)
		    (section . org-plus-section)
		    (src-block . org-plus-example-block)
		    (template . org-plus-template)
		    (verbatim . org-plus-verbatim)))



;;; Filters

(defun org-plus-separate-elements (tree backend info)
  "Make sure elements are separated by at least one blank line.

TREE is the parse tree being exported.  BACKEND is the export
back-end used.  INFO is a plist used as a communication channel.

Assume BACKEND is `plus'."
  (org-element-map
   tree org-element-all-elements
   (lambda (elem)
     (unless (eq (org-element-type elem) 'org-data)
       (org-element-put-property
	elem :post-blank
	(let ((post-blank (org-element-property :post-blank elem)))
	  (if (not post-blank) 1 (max 1 post-blank)))))))
  ;; Return updated tree.
  tree)



;;; Transcode Functions

;;;; Bold

(defun org-plus-bold (bold contents info)
  "Transcode BOLD object into Google Plus format.
CONTENTS is the text within bold markup.  INFO is a plist used as
a communication channel."
  (format "*%s*" contents))


;;;; Code and Verbatim

(defun org-plus-verbatim (verbatim contents info)
  "Transcode VERBATIM object into Google Plus format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((value (org-element-property :value verbatim))) value))


;;;; Example Block and Src Block

(defun org-plus-example-block (example-block contents info)
  "Transcode EXAMPLE-BLOCK element into Google Plus format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (replace-regexp-in-string
   "^" "   "
   (org-remove-indentation
    (org-element-property :value example-block))))

;;;; Headline

(defun org-plus-headline (headline contents info)
  "Transcode HEADLINE element into Google Plus format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
	   (title (org-export-data (org-element-property :title headline) info))
	   (todo (and (plist-get info :with-todo-keywords)
		      (let ((todo (org-element-property :todo-keyword
							headline)))
			(and todo (concat (org-export-data todo info) " ")))))
	   (tags (and (plist-get info :with-tags)
		      (let ((tag-list (org-export-get-tags headline info)))
			(and tag-list
			     (format "     :%s:"
				     (mapconcat 'identity tag-list ":"))))))
	   (priority
	    (and (plist-get info :with-priority)
		 (let ((char (org-element-property :priority headline)))
		   (and char (format "[#%c] " char)))))
	   ;; Headline text without tags.
	   (heading (concat todo priority title)))
      (cond
       ;; Cannot create an headline.  Fall-back to a list.
       ((or (org-export-low-level-p headline info) (> level 3))
	(let ((bullet
	       (if (not (org-export-numbered-headline-p headline info)) "-"
		 (concat (number-to-string
			  (car (last (org-export-get-headline-number
				      headline info))))
			 "."))))
	  (concat bullet (make-string (- 4 (length bullet)) ? ) heading tags
		  "\n\n"
		  (and contents
		       (replace-regexp-in-string "^" "    " contents)))))
       ;; Use "Setext" style.
       (t
        (case level
          (1 (concat "*" heading "*\n\n" contents))
          (2 (concat "_*" heading "*_\n\n" contents))
          (t (concat "_" heading "_\n\n" contents))))))))


;;;; Horizontal Rule

(defun org-plus-horizontal-rule (horizontal-rule contents info)
  "Transcode HORIZONTAL-RULE element into Google Plus format.
CONTENTS is the horizontal rule contents.  INFO is a plist used
as a communication channel."
  "---")

;;;; Italic

(defun org-plus-italic (italic contents info)
  "Transcode ITALIC object into Google Plus format.
CONTENTS is the text within italic markup.  INFO is a plist used
as a communication channel."
  (format "_%s_" contents))

;;;; Strikethrough
(defun org-plus-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH object into Google Plus format.
CONTENTS is the text within italic markup.  INFO is a plist used
as a communication channel."
  (format "-%s-" contents))

;;;; Item

(defun org-plus-item (item contents info)
  "Transcode ITEM element into Google Plus format.
CONTENTS is the item contents.  INFO is a plist used as
a communication channel."
  (let* ((type (org-element-property :type (org-export-get-parent item)))
	 (struct (org-element-property :structure item))
	 (bullet (if (not (eq type 'ordered)) "*"
		   (concat (number-to-string
			    (car (last (org-list-get-item-number
					(org-element-property :begin item)
					struct
					(org-list-prevs-alist struct)
					(org-list-parents-alist struct)))))
			   "."))))
    (concat bullet
	    (make-string (- 4 (length bullet)) ? )
	    (case (org-element-property :checkbox item)
	      (on "[X] ")
	      (trans "[-] ")
	      (off "[ ] "))
	    (let ((tag (org-element-property :tag item)))
	      (and tag (format "*%s:* "(org-export-data tag info))))
	    (org-trim (replace-regexp-in-string "^" "    " contents)))))

;;;; Line Break

(defun org-plus-line-break (line-break contents info)
  "Transcode LINE-BREAK object into Google Plus format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  "  ")


;;;; Link

(defun org-plus-link (link contents info)
  "Transcode LINE-BREAK object into Google Plus format.
CONTENTS is the link's description.  INFO is a plist used as
a communication channel."
  (let ((--link-org-files-as-html-maybe
	 (function
	  (lambda (raw-path info)
	    ;; Treat links to `file.org' as links to `file.html', if
            ;; needed.  See `org-e-html-link-org-files-as-html'.
	    (cond
	     ((and org-e-html-link-org-files-as-html
		   (string= ".org"
			    (downcase (file-name-extension raw-path "."))))
	      (concat (file-name-sans-extension raw-path) "."
		      (plist-get info :html-extension)))
	     (t raw-path)))))
	(type (org-element-property :type link)))
    (cond ((member type '("custom-id" "id"))
	   (let ((destination (org-export-resolve-id-link link info)))
	     (if (stringp destination)	; External file.
		 (let ((path (funcall --link-org-files-as-html-maybe
				      destination info)))
		   (if (not contents) (format "<%s>" path)
		     (format " %s (%s)" contents path)))
	       (concat
		(and contents (concat contents " "))
		(format "(%s)"
			(format (org-export-translate "See section %s" :html info)
				(mapconcat 'number-to-string
					   (org-export-get-headline-number
					    destination info)
					   ".")))))))
	  ((org-export-inline-image-p link org-e-html-inline-image-rules)
	   (format "%s (%s)"
		   (let ((caption
			  (org-element-property
			   :caption (org-export-get-parent-element link))))
		     (when caption (org-export-data (car caption) info)))
		   path))
	  ((string= type "coderef")
	   (let ((ref (org-element-property :path link)))
	     (format (org-export-get-coderef-format ref contents)
		     (org-export-resolve-coderef ref info))))
	  ((equal type "radio")
	   (let ((destination (org-export-resolve-radio-link link info)))
	     (org-export-data (org-element-contents destination) info)))
	  ((equal type "fuzzy")
	   (let ((destination (org-export-resolve-fuzzy-link link info)))
	     ;; Ignore invisible "#+TARGET: path".
	     (unless (eq (org-element-type destination) 'keyword)
	       (if (org-string-nw-p contents) contents
		 (when destination
		   (let ((number (org-export-get-ordinal destination info)))
		     (when number
		       (if (atom number) (number-to-string number)
			 (mapconcat 'number-to-string number ".")))))))))
	  (t (let* ((raw-path (org-element-property :path link))
		    (path (cond
			   ((member type '("http" "https" "ftp"))
			    (concat type ":" raw-path))
			   ((equal type "file")
			    ;; Extract just the file path and strip
			    ;; all other components.
			    (when (string-match "\\(.+\\)::.+" raw-path)
			      (setq raw-path (match-string 1 raw-path)))
			    ;; Treat links to ".org" files as ".html",
			    ;; if needed.
			    (setq raw-path
				  (funcall --link-org-files-as-html-maybe
					   raw-path info))
			    ;; If file path is absolute, prepend it
			    ;; with protocol component - "file://".
			    (if (not (file-name-absolute-p raw-path)) raw-path
			      (concat "file://" (expand-file-name raw-path))))
			   (t raw-path))))
	       (if (not contents) (format "<%s>" path)
		 (format " %s (%s)" contents path)))))))


;;;; Paragraph

(defun org-plus-paragraph (paragraph contents info)
  "Transcode PARAGRAPH element into Google Plus format.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  contents)


;;;; Plain List

(defun org-plus-plain-list (plain-list contents info)
  "Transcode PLAIN-LIST element into Google Plus format.
CONTENTS is the plain-list contents.  INFO is a plist used as
a communication channel."
  contents)


;;;; Plain Text

(defun org-plus-plain-text (text info)
  "Transcode a TEXT string into Google Plus format.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  ;; Protect ambiguous #.  This will protect # at the beginning of
  ;; a line, but not at the beginning of a paragraph.  See
  ;; `org-plus-paragraph'.
  ;; Protect `, *, _ and \
  (setq text
	(replace-regexp-in-string
	 "[`*_\\]" (lambda (rep) (concat "\\\\" (match-string 1 rep))) text))
  ;; Handle break preservation, if required.
   (when (plist-get info :preserve-breaks)
     (setq text (replace-regexp-in-string "[ \t]*\n" "  \n" text)))
  ;; Return value.
  text)


;;;; Quote Block

(defun org-plus-quote-block (quote-block contents info)
  "Transcode QUOTE-BLOCK element into Google Plus format.
CONTENTS is the quote-block contents.  INFO is a plist used as
a communication channel."
  (concat "_" contents "_"))  



;;;; Section

(defun org-plus-section (section contents info)
  "Transcode SECTION element into Google Plus format.
CONTENTS is the section contents.  INFO is a plist used as
a communication channel."
  contents)


;;;; Template

(defun org-plus-template (contents info)
  "Return complete document string after Google Plus conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  contents)



;;; Interactive function

;;;###autoload
(defun org-plus-export-as-plus (&optional subtreep visible-only)
  "Export current buffer to a text buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org MD Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (let ((outbuf (org-export-to-buffer
		 'plus "*Org Plus Export*" subtreep visible-only)))
    (with-current-buffer outbuf (text-mode))
    (when org-export-show-temporary-export-buffer
      (switch-to-buffer-other-window outbuf))))


;;;###autoload
(defun org-plus-export-to-plus (&optional subtreep visible-only pub-dir)
  "Export current buffer to a Google Plus file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".plus" subtreep pub-dir)))
    (org-export-to-file 'plus outfile subtreep visible-only)))


(provide 'org-plus)
;;; org-plus.el ends here
