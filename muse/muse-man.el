;;; muse-man.el --- publish groff -man files

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

(defgroup muse-man nil
  "Rules for marking up a Muse file with groff -mm macros."
  :group 'muse-publish)

(defcustom muse-man-extension ".1"
  "Default file extension for publishing man files"
  :type 'string
  :group 'muse-man)

(defcustom muse-man-pdf-extension ".pdf"
  "Default file extension for publishing mm -mm files to PDF."
  :type 'string
  :group 'muse-man)

(defcustom muse-man-header
  ".TH \"<lisp>(muse-publishing-directive \"title\")</lisp> \" 1 \n"
  "Header used for publishing man page files."
  :type '(choice string file)
  :group 'muse-man)

(defcustom muse-man-footer ""
  "Footer used for publishing groff -mm files."
  :type '(choice string file)
  :group 'muse-man)

(defcustom muse-man-markup-regexps
  `((10400 ,(concat "\\(\n</\\(blockquote\\|center\\)>\\)?\n"
                    "\\(["
                    muse-regexp-blank
                    "]*\n\\)+\\(<\\(blockquote\\|center\\)>\n\\)?")
           0 muse-man-markup-paragraph))
"List of markup regexps for identifying regions in a Muse page.
For more on the structure of this list, see `muse-publish-markup-regexps'."
  :type '(repeat (choice
                  (list :tag "Markup rule"
                        integer
                        (choice regexp symbol)
                        integer
                        (choice string function symbol))
                  function))
  :group 'muse-man)

(defcustom muse-man-markup-functions
  '((table . muse-man-markup-table))
  "An alist of style types to custom functions for that kind of text.
For more on the structure of this list, see
`muse-publish-markup-functions'."
  :type '(alist :key-type symbol :value-type function)
  :group 'muse-man)

(defcustom muse-man-markup-tags
  '()
  "A list of tag specifications, for specially marking up MM."
  :type '(repeat (list (string :tag "Markup tag")
                       (boolean :tag "Expect closing tag" :value t)
                       (boolean :tag "Parse attributes" :value nil)
                       (boolean :tag "Nestable" :value nil)
                       function))
  :group 'muse-man)

(defcustom muse-man-markup-strings
  `(
;;(image-with-desc . "\n.MPIMG -R %s.%s\n")
;;    (image           . "\n.MPIMG -R %s.%s\n")
;;    (image-link      . "\n.\\\" %s\n.MPIMG -R %s.%s")
    (url             . "\n%s - %s\n")
    (link            . "\n%s - %s\n")
    (email-addr      . "\f[C]%s\f[]")
    (emdash          . "\\(em")
;;    (rule            . "\n.RULE\n")
    (no-break-space  . "\\h")
    (line-break      . "\\p")
    (enddots         . "....")
    (dots            . "...")
;;     (part            . "\\part{")
;;     (part-end        . "}")
;;     (chapter         . "\\chapter{")
;;     (chapter-end     . "}")
    (section         . ".SH \"")
    (section-end     . "\"")
    (subsection      . ".SS \"")
    (subsection-end  . "\"")
    (subsubsection   . ".SS \"")
    (subsubsection-end . "\"")
;;     (footnote        . "\n.FS\n")
;;     (footnote-end    . "\n.FE\n")
;;     (footnotemark    . "\\footnotemark[%d]")
;;     (footnotetext    . "\\footnotetext[%d]{")
;;     (footnotetext-end . "}")
    (begin-underline . "\\fI")
    (end-underline   . "\\fP")
    (begin-literal   . "\\fC")
    (end-literal     . "\\fP")
    (begin-emph      . "\\fB")
    (end-emph        . "\\fP")
    (begin-more-emph . "\\fB")
    (end-more-emph   . "\\fP")
    (begin-most-emph . "\\f(BI")
    (end-most-emph   . "\\fP")
    (begin-verse     . "")
    (end-verse       . "")
    (begin-center    . ".RS\n")
    (end-center      . "\n.RE")
    (begin-example   . ".HP")
    (end-example     . "")
    (begin-quote     . ".RS\n")
    (end-quote       . "\n.RE")
    (begin-cite     . ".RS\n")
    (begin-cite-author . "")
    (begin-cite-year . "")
    (end-cite        . "\n.RE")
    (begin-uli       . "")
    (end-uli         . "")
    (begin-uli-item  . ".br\n")
;;    (begin-oli-item  . "")
;;    (begin-oli       . "")
;;    (end-oli         . "")
    (begin-ddt       . ".IP \ \ ")
    (begin-dde       . "")
    (end-ddt         . ""))
  "Strings used for marking up text.
These cover the most basic kinds of markup, the handling of which
differs little between the various styles."
  :type '(alist :key-type symbol :value-type string)
  :group 'muse-man)

(defcustom muse-man-markup-specials
  '((?\\ . "\\e"))
  "A table of characters which must be represented specially."
  :type '(alist :key-type character :value-type string)
  :group 'muse-man)

(defun muse-man-markup-paragraph ()
  (let ((end (copy-marker (match-end 0) t)))
    (goto-char (1+ (match-beginning 0)))
    (delete-region (point) end)
    (unless (looking-at "\.\\(\\(\\(SH\\|SS\\)?HEAD \\)\\|RULE$\\)")
      (muse-insert-markup ".PP\n"))))

(defun muse-man-protect-leading-chars ()
  "Protect leading periods and apostrophes from being interpreted as
command characters."
  (while (re-search-forward "^[.']" nil t)
    (replace-match "\\\\&\\&" t)))

(defun muse-man-concat-lists ()
  "Join like lists."
  (let ((type "")
        arg begin)
    (while (re-search-forward "^\.IP[ \t]+\\(.*\\)\n" nil t)
      (setq arg (match-string 1))
      (if (string= arg "OFF")
          (setq begin (match-beginning 0))
        (if (and begin (string= type arg))
            (delete-region begin (match-end 0))
          (setq type arg
                begin 0))))))

(defun muse-man-fixup-dquotes ()
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

(defun muse-man-prepare-buffer ()
  (goto-char (point-min))
  (muse-man-protect-leading-chars))

(defun muse-man-munge-buffer ()
  (goto-char (point-min))
  (muse-man-concat-lists))

(defun muse-man-pdf-browse-file (file)
  (shell-command (concat "open " file)))

(defun muse-man-pdf-generate (file output-path final-target)
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
              muse-man-extension
              (file-name-directory output-path))))
        (shell-command command))))
   ".ps"))

;;; Register the Muse MM Publisher

(muse-define-style "man"
                   :suffix    'muse-man-extension
                   :regexps   'muse-man-markup-regexps
;;;		   :functions 'muse-man-markup-functions
                   :strings   'muse-man-markup-strings
                   :tags      'muse-man-markup-tags
                   :specials  'muse-man-markup-specials
                   :before    'muse-man-prepare-buffer
                   :before-end 'muse-man-munge-buffer
                   :header    'muse-man-header
                   :footer    'muse-man-footer
                   :browser   'find-file)

(muse-derive-style "man-pdf" "man"
                   :final   'muse-man-pdf-generate
                   :browser 'muse-man-pdf-browse-file
                   :osuffix 'muse-man-pdf-extension)

(provide 'muse-man)

;;; muse-man.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
