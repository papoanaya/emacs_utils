;;; muse-groff.el --- publish groff -mm files

;; Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2012
;;   Free Software Foundation, Inc.

;; Author: Luis Roberto Anaya
;; Date: Sun 24-Jul-2012

;; This file is part of Emacs Muse.  It is not part of GNU Emacs.

;; Emacs Muse is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; Emacs Muse is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs Muse; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Contributors:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Muse Publishing Using groff -mom -mwww
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'muse-publish)

(defgroup muse-mm nil
  "Rules for marking up a Muse file with groff -mm macros."
  :group 'muse-publish)

(defcustom muse-mm-extension ".mm"
  "Default file extension for publishing mm -mom -mwww files."
  :type 'string
  :group 'muse-mm)

(defcustom muse-mm-pdf-extension ".pdf"
  "Default file extension for publishing mm -mm files to PDF."
  :type 'string
  :group 'muse-mm)

(defcustom muse-mm-header
  ".AF \" \" 
.TL\n<lisp>(muse-publishing-directive \"title\")</lisp>
.AU \"<lisp>(muse-publishing-directive \"author\")</lisp>\"
.ND \"<lisp>(muse-publishing-directive \"date\")</lisp>\"
.MT 4\n"
  "Header used for publishing mm -mm files."
  :type '(choice string file)
  :group 'muse-mm)

(defcustom muse-mm-footer ".TC\n"
  "Footer used for publishing groff -mm files."
  :type '(choice string file)
  :group 'muse-mm)

(defcustom muse-mm-markup-regexps
  `((10400 ,(concat "\\(\n</\\(blockquote\\|center\\)>\\)?\n"
                    "\\(["
                    muse-regexp-blank
                    "]*\n\\)+\\(<\\(blockquote\\|center\\)>\n\\)?")
           0 muse-mm-markup-paragraph))
"List of markup regexps for identifying regions in a Muse page.
For more on the structure of this list, see `muse-publish-markup-regexps'."
  :type '(repeat (choice
                  (list :tag "Markup rule"
                        integer
                        (choice regexp symbol)
                        integer
                        (choice string function symbol))
                  function))
  :group 'muse-mm)

(defcustom muse-mm-markup-functions
  '((table . muse-mm-markup-table))
  "An alist of style types to custom functions for that kind of text.
For more on the structure of this list, see
`muse-publish-markup-functions'."
  :type '(alist :key-type symbol :value-type function)
  :group 'muse-mm)

(defcustom muse-mm-markup-tags
  '()
  "A list of tag specifications, for specially marking up MM."
  :type '(repeat (list (string :tag "Markup tag")
                       (boolean :tag "Expect closing tag" :value t)
                       (boolean :tag "Parse attributes" :value nil)
                       (boolean :tag "Nestable" :value nil)
                       function))
  :group 'muse-mm)

(defcustom muse-mm-markup-strings
  `((image-with-desc . "\n.MPIMG -R %s.%s\n")
    (image           . "\n.MPIMG -R %s.%s\n")
    (image-link      . "\n.\\\" %s\n.MPIMG -R %s.%s")
    (url             . "\n.URL %s %s\n\\z")
    (link            . "\n.URL %s %s\n\\z")
    (email-addr      . "\f[C]%s\f[]")
    (emdash          . "\\(em")
    (rule            . "\n.RULE\n")
    (no-break-space  . "\\h")
    (line-break      . "\\p")
    (enddots         . "....")
    (dots            . "...")
;;     (part            . "\\part{")
;;     (part-end        . "}")
;;     (chapter         . "\\chapter{")
;;     (chapter-end     . "}")
    (section         . ".H 1 \"")
    (section-end     . "\"")
    (subsection      . ".H 2 \"")
    (subsection-end  . "\"")
    (subsubsection   . ".H 3 \"")
    (subsubsection-end . "\"")
     (footnote        . "\n.FS\n")
     (footnote-end    . "\n.FE\n")
;;     (footnotemark    . "\\footnotemark[%d]")
;;     (footnotetext    . "\\footnotetext[%d]{")
;;     (footnotetext-end . "}")
    (begin-underline . "\n.I \"")
    (end-underline   . "\"\n.R")
    (begin-literal   . "\\fC")
    (end-literal     . "\\fP")
    (begin-emph      . "\\fI")
    (end-emph        . "\\fP")
    (begin-more-emph . "\\fB")
    (end-more-emph   . "\\fP")
    (begin-most-emph . "\\f(BI")
    (end-most-emph   . "\\fP")
    (begin-verse     . "")
    (end-verse       . "")
    (begin-center    . "\n.DS C\n")
    (end-center      . "\n.DE\n")
    (begin-example   . "\n.in +5\n.I\n")
    (end-example     . "\n.in -5\n.R\n")
    (begin-quote     . "\n.in +5\n.I\n")
    (end-quote       . "\n.in -5\n.R\n")
    (begin-cite     . "")
    (begin-cite-author . "")
    (begin-cite-year . "")
    (end-cite        . "")
    (begin-uli       . ".BL\n")
    (end-uli         . "\n.LE")
    (begin-uli-item  . ".LI\n")
    (begin-oli-item  . ".LI\n")
    (begin-oli       . ".AL\n")
    (end-oli         . "\n.LE")
    (begin-dl       . ".VL 0.5i\n")
    (end-dl         . "\n.LE")
    (begin-ddt       . ".LI \"")
    (end-ddt         . "\"\n")
    (begin-dde       . "")

)
  "Strings used for marking up text.
These cover the most basic kinds of markup, the handling of which
differs little between the various styles."
  :type '(alist :key-type symbol :value-type string)
  :group 'muse-mm)

(defcustom muse-mm-markup-specials
  '((?\\ . "\\e"))
  "A table of characters which must be represented specially."
  :type '(alist :key-type character :value-type string)
  :group 'muse-mm)

(defun muse-mm-markup-paragraph ()
  (let ((end (copy-marker (match-end 0) t)))
    (goto-char (1+ (match-beginning 0)))
    (delete-region (point) end)
    (unless (looking-at "\.\\(\\(\\(H\\|PARA\\)?HEAD \\)\\|RULE$\\)")
      (muse-insert-markup ".P\n"))))

(defun muse-mm-protect-leading-chars ()
  "Protect leading periods and apostrophes from being interpreted as
command characters."
  (while (re-search-forward "^[.']" nil t)
    (replace-match "\\\\&\\&" t)))

(defun muse-mm-concat-lists ()
  "Join like lists."
  (let ((type "")
        arg begin)
    (while (re-search-forward "^\.LI[ \t]+\\(.*\\)\n" nil t)
      (setq arg (match-string 1))
      (if (string= arg "OFF")
          (setq begin (match-beginning 0))
        (if (and begin (string= type arg))
            (delete-region begin (match-end 0))
          (setq type arg
                begin 0))))))

(defun muse-mm-fixup-dquotes ()
  "Fixup double quotes."
  (let ((open t))
    (while (search-forward "\"" nil t)
      (unless (get-text-property (match-beginning 0) 'read-only)
        (if (and (bolp) (eq (char-before) ?\n))
            (setq open t))
        (if open
            (progn
              (replace-match "``")
              (setq open nil))
          (replace-match "''")
          (setq open t))))))

(defun muse-mm-prepare-buffer ()
  (goto-char (point-min))
  (muse-mm-protect-leading-chars))

(defun muse-mm-munge-buffer ()
  (goto-char (point-min))
  (muse-mm-concat-lists))

(defun muse-mm-pdf-browse-file (file)
  (shell-command (concat "open " file)))

(defun muse-mm-pdf-generate (file output-path final-target)
  (muse-publish-transform-output
   file output-path final-target "PDF"
   (function
    (lambda (file output-path)
      (let ((command
             (format
              (concat "file=%s; ext=%s; cd %s && cp $file$ext $file.ref && "
                      "groff -mm -t $file$ext > $file.ps && "
                      "ps2pdf $file.ps")
              (file-name-sans-extension file)
              muse-mm-extension
              (file-name-directory output-path))))
        (shell-command command))))
   ".ps"))

;;; Register the Muse MM Publisher

(muse-define-style "mm"
                   :suffix    'muse-mm-extension
                   :regexps   'muse-mm-markup-regexps
;;;		   :functions 'muse-mm-markup-functions
                   :strings   'muse-mm-markup-strings
                   :tags      'muse-mm-markup-tags
                   :specials  'muse-mm-markup-specials
                   :before    'muse-mm-prepare-buffer
                   :before-end 'muse-mm-munge-buffer
                   :header    'muse-mm-header
                   :footer    'muse-mm-footer
                   :browser   'find-file)

(muse-derive-style "mm-pdf" "mm"
                   :final   'muse-mm-pdf-generate
                   :browser 'muse-mm-pdf-browse-file
                   :osuffix 'muse-mm-pdf-extension)

(provide 'muse-mm)

;;; muse-mm.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
