 ;;; regina-mode.el --- highlight and indent Regina program files.
 ;;
 ;; Author Luis Anaya <papoanaya@hotmail.com>
 ;; Keywords: regina
 ;; 
 ;; This file is a combination of netrexx mode from Arjan Bos and rexx 
 ;; mode from Scott Maxwell. 
 ;; 
 ;; I just modified the mode map to get the electric
 ;; modes from rexx mode  and the colorization from netrexx mode. 
 ;; Honestly, credit should go to those folks that wrote the
 ;; original code. 
 ;; This is for my personal use.
 ;;
 ;;
 ;; Author Arjan Bos <Arjan.Bos@icu.nl>
 ;; Keywords: netrexx
 ;; Version:
 (defconst regina-mode-version "1.0")

 ;; Since this file is the, completely rewritten, follow-up to the
 ;; original regina-mode.el which was posted to gno.emacs.sources on
 ;; 18 Jun 2002, its version number is 2.0.
 ;;
 ;;      Copyright (C) 2003 Arjan Bos.
 ;;
 ;;      This file is NOT part of GNU Emacs (yet).
 ;;
 ;;
 ;; DISTRIBUTION
 ;; Copyright (C) 2002-2003 Arjan Bos
 ;; This file is free software; you can redistribute it and/or modify
 ;; it under the terms of the GNU General Public License as published by
 ;; the Free Software Foundation; either version 2, or (at your option)
 ;; any later version.
 ;; It is distributed in the hope that it will be useful, but WITHOUT
 ;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 ;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
 ;; License for more details.
 ;; You should have received a copy of the GNU General Public License
 ;; along with GNU Emacs; see the file COPYING.  If not, write to the
 ;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 ;; Boston, MA 02111-1307, USA.
 ;;
 ;;
 ;; Comentary:
 ;; After working for roughly a year with an adapted REXX-mode, I decided
 ;; it was time for a dedicated regina-mode. Particularly, the
 ;; indentation engine was beyond my simple elisp comprehension and it
 ;; had a few oddities that made it less than perfect. Taking the
 ;; mode-tutorial on emacs wiki as a starting point, I built this mode
 ;; from scratch.
 ;;
 ;; Inspiration:
 ;; The main inspiration for writing this file as it is came from the
 ;; mode-tutorial by Scott Andrew Borton ( http://two-wugs.net/scott/
 ;; ), which I found on emacs-wiki.
 ;; The idea for the command regina-select-current-block was taken
 ;; from rexx-mode.el by Anders Lindgren / James Perrin. 
 ;; The idea for a Regina pull-down menu and how to implement it was
 ;; taken straight from fortran.el.  Stefan Monier helped when I was
 ;; stuck by answering some questions on news://gnu.emacs.help
 ;;
 ;; Usage:
 ;;      This file contains code for a GNU Emacs major mode for
 ;;      editing REGINA program files.
 ;;
 ;;     Type C-h m in Emacs for information on how to configurate
 ;;      the regina-mode.
 ;;
 ;;      Put the following lines into your .emacs and rexx-mode will be
 ;;      automatically loaded when editing a REGINA program.  If
 ;;      regina-mode shall be used for files with other extensions you
 ;;      can create more (cons ...) lines with these extensions.
 ;;
 ;;      (autoload 'regina-mode "regina-mode" "REGINA mode" nil t)
 ;;      (setq auto-mode-alist
 ;;            (append
 ;;             (list (cons "\\.nrx$"  'regina-mode)
 ;;                   (cons "\\.nry$"  'regina-mode)
 ;;                   )
 ;;             auto-mode-alist))
 ;;
 ;; To have regina-mode indent two columns a time your new lines as
 ;; you type them, enter the following in your .emacs
 ;; (setq regina-mode-hook '(lambda ()
 ;;                      (setq regina-indent-amount 2)
 ;;                      (local-set-key "\C-m" 'regina-indent-newline-indent)
 ;;                      ))
 ;;
 ;; Regina mode can automatically insert a little comment after the
 ;; keyword "end" indicating what it is ending. To do this, replace
 ;; 'regina-indent-newline-indent
 ;; by
 ;; 'regina-indent-newline-indent-with-end-comment
 ;;
 ;; Functions that should make live a bit easier:
 ;;
 ;; M-x regina-sanitize-region
 ;;   To make sure that there are no unintentional "trace results" or
 ;;   "trace methods" statements in your cvs check in, select a region
 ;;   and use the commant M-x regina-sanitize-region on it. This will
 ;;   also change all white lines by a single one, and it will remove
 ;;   all trailing whitespace.
 ;;
 ;; M-x regina-select-current-block
 ;;   This will select all lines of the "block" point is in. A block is
 ;;   defined to start either with "do", "loop" or "select" and end
 ;;   with "end", or defined to be the current method when no
 ;;   surrounding "do", "loop", "select" with "end" is found.  For this
 ;;   command, the comments right before a method are considered to be
 ;;   part of that method.
 ;;
 ;; A note on indentation:
 ;; Regina has a human oriented syntax, meaning basically "anything
 ;; goes" for the lay-out. To get the fullest of the indentation
 ;; engine, it makes a few assumptions about the lay-out of
 ;; source-code:
 ;; Statements like "if", "else", "end", "loop" and "catch" should always
 ;; be found as the first text on a line.
 ;; Bugs / To do:
 ;; 1. When using continued lines in combination with
 ;;    statements like "then do", the indentation is wrong.
 ;; 2. Other combinations of continued lines might prove to be wrong
 ;;    too.
 ;; 3. Auto-fill mode does not work. However, filling of comments with
 ;;    M-q works.
 ;; 4. M-q does not split coding lines at appropriate places with a
 ;;    continuation character. This is something that's low on my to
 ;;    do list.
 ;; 5. regina-insert-javadoc-for-method goes into a loop if a
 ;;    parameter name ends with more than one underscore (_).
 ;; HISTORY
 ;;     14-09-03 V2.0 AB         First version. Inspired by the ModeTutorial
 ;;                              on emacs wiki. Version number is 2.0, since
 ;;                              it replaces another regina-mode.
 ;;
 ;;     06-04-04 V2.1 AB         Added skeletons, some minor bug fixes in the
 ;;                              indentation code.

;;; rexx-mode.el --- REXX code editing commands for Emacs
;; Copyright (C) 1994 Scott Maxwell

;; Maintainer: Scott Maxwell - scottmax@netcom.com
;; Keywords: rexx

;;------------------------------------------------------------------------
;;; IMPORTANT NOTE: Since there is no difference between line labels and
;;		    procedure name labels in REXX, there is no way for
;;		    rexx-mode to know the difference.  Therefore, I have
;;		    adopted the convention that a '_' preceding a label
;;		    indicates a line label.  Otherwise, it's a procedure
;;		    name.  If you don't precede your line labels with '_',
;;		    I can't predict what will happen to your formatting.
;;------------------------------------------------------------------------

;; This is a full featured rexx-mode.  That means that it should format
;; your REXX code correctly in all cases (except the one mentioned above.)
;; Please let me know if it doesn't.

;; Everyone should probably look at the docs for this as it has detailed
;; information on all rexx-mode features.  These include:
;;	* indentation styles
;;	* command/function name completion
;;	* automatic capitalization options
;;	* REXX command/function online help
;;	* single key DO ('{') and END ('}')
;;	* keymaps for going to the start/end of procedures/blocks
;;	* keymaps for reindenting procedures/blocks/regions of code
;; Get into REXX mode and do M-x describe-mode for details.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This REXX mode code began as the EMACS 19.22 build c-mode.el.

;; A smart editing mode for REXX code.  It knows a lot about REXX syntax
;; and tries to position the cursor according to REXX layout conventions.
;; You can change the details of the layout style with option variables.
;; It also supports completion, auto-capitalization and REXX function help.
;; Load it and do M-x describe-mode for details.

;; If you want to enable all features of rexx-mode, as well as VX-Rexx
;; support, you can just add these lines to your .emacs:
;;
;;   (setq auto-mode-alist (cons '("\\.cmd$" . rexx-mode) auto-mode-alist))
;;   (setq auto-mode-alist (cons '("\\.vrx$" . rexx-mode) auto-mode-alist))
;;   (setq auto-mode-alist (cons '("\\.vrm$" . rexx-mode) auto-mode-alist))
;;   (autoload 'rexx-mode "rexx-mode")
;;
;;   (setq rexx-command-auto-upper 2)
;;   (setq rexx-external-function-auto-capitilize t)
;;   (setq rexx-auto-build-procedure-table t)
;;   (setq rexx-super-completion-mode t)
;;   (setq rexx-additional-doc-files '("rexx-os2" "vxrexx-doc"))
;;   (load "vxrexx-mode")
;;   (setq vxrexx-build-master-table t)


;; To autoload, add this to your .emacs file:
;;
;;   (setq auto-mode-alist (cons '("\\.cmd$" . rexx-mode) auto-mode-alist))
;;   (autoload 'rexx-mode "rexx-mode")
;;
;; If you are not using OS/2, this "\\.cmd$" will probably be
;; something different.  If you are using VX-REXX, you might also
;; find it valuable to add "\\.vrx$" and maybe even "\\.vrm$".

;; If you want to take advantage of automatic capitalization, you might
;; also want to add:
;;
;;   (setq rexx-command-auto-upper t)
;;   (setq rexx-external-function-auto-capitilize t)
;;
;; This will automatically convert all internal commands/functions to
;; uppercase and capitalize all external function names that it knows
;; about.  To capitalize instead of uppercasing internal commands/
;; functions, change rexx-command-auto-upper to 1 instead of t.
;; I personally set this to 2 which will uppercase commands and
;; capitalize functions.  Function names will only be capitalized
;; when rexx-mode thinks it is appropriate.  This means after a
;; CALL or when followed by a '('.  It also will not do completion
;; on function names in double quotes(") but will do so in single
;; quotes(').  The reason for this is that quoted non-function names
;; may be added to external function expansion tables such as VX-Rexx
;; method and property names.  These will only expand after quotes
;; (single or double.)  See the rexx-doc.el file for more information.
;;
;; If you want rexx-mode to automatically add new procedures you write
;; to its local expansion table, add:
;;    (setq rexx-auto-build-procedure-table t)
;;
;; Then, when you load a rexx file, rexx-mode will automatically scan
;; the file, adding any procedure names it finds to its local table.
;; Also, everytime you hit colon after a procedure name, it will add
;; the new procedure to the table as well.  If you want to rebuild this
;; table from scratch, use M-x rexx-build-procedure-table.  This is also
;; bound to M-C-Tab by default.

;; If you want to enable very high level completion, add:
;;   (setq rexx-super-completion-mode)
;;
;; This will enable command specific completion.  For instance, after
;; typing RxFuncAdd, the command specific completion function for
;; RxFuncAdd will kick in and prompt you for the name of the function
;; you want to add and the package it came from, defaulting to the
;; package name it finds in its expansion table if present.
;;
;; Also, you should be aware that both [ESC Tab] and [Ctrl Tab] are
;; mapped to REXX completion.  Just type part of the command name and
;; hit one of these combinations and rexx-mode will spit out the rest
;; of the word or a list of choices.  You can hit [Ctrl-x 1] to get
;; rid of the completion window.
;;
;; If you want help about any REXX command, try [Ctrl h] [Ctrl f].
;; If you don't plan to use this feature, you can save some memory
;; by using the nodoc completion file.  Just add this to your .emacs:
;;   (setq rexx-doc-file "rexx-sml")


 ;; Code:
 (require 'skeleton)

 (defgroup regina nil
   "Groups together all customization possiblities for regina-mode."
   :group 'languages)

 (defcustom regina-indent-amount 2
   "*This variable contains the indentation in regina-mode."
   :group 'regina
   :type 'number)

 (defcustom regina-end-comment-treshold 5
   "*Number of lines to search backward before an end comment is included.

 For example, a value of 5 means that if the matching \"do\", \"loop\",
 or \"select\" statement is 5 or more lines backward, an end-comment
 gets included when either \\[regina-insert-end-comment] or
 \\[regina-indent-newline-indent-with-end-comment] is called."
   :group 'regina
   :type 'number)

 (defcustom regina-beep-annoyingly t
   "* If true, then beep on regina syntax errors.

 Currently, it beeps on unmatched \"end\" and \"else\" statements
 and when the point is past the last \"method\" statement and
 \\[regina-next-method] is evaluated, or when the point is before
 the first \"method\" statement and \\[regina-previous-method] is
 evaluated."
   :group 'regina
   :type 'boolean)

 (defcustom regina-auto-insert-javadoc nil
   "If true, then javadoc skeletons are inserted when a method is created.
 When this variable is true, then finishing a \"method\"
 statement (by pressing the enter-key) will automatically insert a
 javadoc skeleton above the method."
   :group 'regina
   :type 'boolean)

 (defcustom regina-package-path ""
   "Expanded path to the root of the package currently being edited.
 This is used by \\[regina-initial-template] to re-model the
 directory-name of the current file into a package statement.

 It will change:
 <regina-package-path>com/abnamro/midms/server/

 to:
 package com.abnamro.midms.server

 It will need a trailing / or \\, depending on the filesystem,
  to make this work correctly."
   :group 'regina
   :type  'directory)

 (define-skeleton regina-do-skeleton
   "Insert skeleton do / end statement"
   ()
   > "do"  \n
   > _ \n
   > "end" '(regina-insert-end-comment)
           '(regina-indent-line)
           '(regina-from-end-goto-matching-do))

 (defun regina-do ()
   "Undoes unwanted site-effects from `[regina-do-skeleton]'"
   (regina-do-skeleton)
   (forward-line -1)
   (delete-blank-lines))

 (define-skeleton regina-loop
   "Insert skeleton loop statement."
   ()
   > "loop" \n
   > _ \n
   > "end" '(regina-insert-end-comment))

 (defvar regina-mode-hook nil)
 (defvar regina-mode-map nil
   "Keymap for Regina major mode")

 (if regina-mode-map
     nil
   (progn
     (setq regina-mode-map (make-keymap))
     (define-key regina-mode-map "\t"   'regina-indent-line)
 ;    (define-key regina-mode-map "\177" 'backward-delete-char-untabify)
 ;    (define-key regina-mode-map "\C-c\C-p" 'regina-previous-method)
 ;    (define-key regina-mode-map "\C-c\C-n" 'regina-next-method)
 ;    (define-key regina-mode-map "\C-\M-a" 'regina-beginning-of-method)
 ;    (define-key regina-mode-map "\C-\M-e" 'regina-end-of-method)
 ;    (define-key regina-mode-map "\C-ce" 'regina-close-block)
 ;    (define-key regina-mode-map "(" 'skeleton-pair-insert-maybe)
 ;    (define-key regina-mode-map "{" 'skeleton-pair-insert-maybe)
 ;    (define-key regina-mode-map "[" 'skeleton-pair-insert-maybe)
 ;    (define-key regina-mode-map "'" 'skeleton-pair-insert-maybe)
 ;    (define-key regina-mode-map "\"" 'skeleton-pair-insert-maybe)

					; From rexx mode
     (define-key regina-mode-map "\C-c\C-c"  'compile)
     (define-key regina-mode-map ":" 'electric-rexx-colon)
     (define-key regina-mode-map "{" 'electric-rexx-do)
     (define-key regina-mode-map "}" 'electric-rexx-end)
     (define-key regina-mode-map " " 'electric-rexx-space)
     (define-key regina-mode-map ";" 'electric-rexx-space)
     (define-key regina-mode-map "(" 'electric-rexx-paren)
     (define-key regina-mode-map ")" 'electric-rexx-space)
     (define-key regina-mode-map "'" 'electric-rexx-space)
     (define-key regina-mode-map "\"" 'electric-rexx-space)
     (define-key regina-mode-map "" 'electric-rexx-newline)
     (define-key regina-mode-map "\177" 'backward-delete-char-untabify)
     (define-key regina-mode-map "\t" 'rexx-indent-command)
     (define-key regina-mode-map [C-tab] 'rexx-complete-symbol)
     (define-key regina-mode-map "\e\t" 'rexx-complete-symbol)
     (define-key regina-mode-map [M-C-tab] 'rexx-build-procedure-table)
     (define-key regina-mode-map "\M-\C-\\" 'rexx-indent-region)
     (define-key regina-mode-map [M-C-space] 'rexx-capitalize-sexp)
     (define-key regina-mode-map "\M-\C-q" 'rexx-indent-sexp)
     (define-key regina-mode-map "\M-\C-a" 'rexx-beginning-of-procedure)
     (define-key regina-mode-map "\M-\C-e" 'rexx-end-of-procedure)
     (define-key regina-mode-map "\M-\C-f" 'rexx-forward-sexp)
     (define-key regina-mode-map "\M-\C-b" 'rexx-backward-sexp)

     (easy-menu-define regina-menu regina-mode-map "Menu for Regina mode."
       `("Regina"
;         ["Next Method\t\tC-c C-n" (regina-next-method)]
;         ["Previous Method\tC-c C-p" (regina-previous-method)]
         ["Select Block" (regina-select-current-block)]
         ["Sanitize Region" (regina-sanitize-region (region-beginning) (region-end))]
         ["Insert End Comment" (regina-insert-end-comment)]
         ["End Comments Region" (regina-insert-end-comment-region (region-beginning) (region-end))]
         ["Insert javadoc" (regina-insert-javadoc-for-method)]
         "--"
         ["Customize" (customize-group 'regina)]
         ["Version" (regina-version)]
         ))))

 ;; (setq regina-mode-map nil)
 ;; font-lock patterns
 ;; (defvar regina-font-lock-keywords nil
 ;;  "Expressions to highlight in V code mode")



 (defvar regina-font-lock-keywords-1 nil
  "Level 1 expressions to highlight in V code mode")

 (defvar regina-font-lock-keywords-2 nil
  "Level 2 expressions to highlight in V code mode")

 (defvar regina-font-lock-keywords-3 nil
  "Level 3 expressions to highlight in V code mode")

 (defcustom font-lock-regina-method-face 'font-lock-regina-method-face
   "*Specify face used to color the rexx provided method calls."
   :type 'face
   :group 'faces
   :group 'regina)

 (defface font-lock-regina-method-face
   '((((class color) (background light)) (:bold t :foreground "blue")))
     "Face used to color the regina provided method calls.")

 (defcustom font-lock-method-face 'font-lock-method-face
   "*Specify face used to color the method calls."
   :type 'face
   :group 'faces
   :group 'regina)

 (defface font-lock-method-face
   '((((class color) (background light)) (:foreground "dark blue")))
     "Face used to color the method calls.")

 ;; Level 1 - comments and strings
 (setq regina-font-lock-keywords-1
       (list
        '("\\<\\(a\\(bstract\\|dapter\\|ddress\\)\\|b\\(inary\\|y\\)\\|c\\(ase\\|atch\\|lass\\|onstant\\)\\|d\\(ep\\(endent\\|recated\\)\\|igits\\|o\\)\\|e\\(lse\\|n\\(d\\|gineering\\)\\|x\\(it\\|tends\\)\\)\\|f\\(inal\\(\\|ly\\)\\|or\\(\\|\\(ever\\|m\\)\\)\\)\\|i\\(f\\|mp\\(lements\\|ort\\)\\|n\\(direct\\|heritable\\|terface\\)\\|terate\\)\\|l\\(abel\\|eave\\|oop\\)\\|m\\(ethod\\)\\|n\\(ative\\|op\\|umeric\\)\\|o\\(ptions\\|therwise\\|ver\\)\\|p\\(a\\(ckage\\|r\\(ent\\|se\\)\\)\\|r\\(ivate\\|o\\(perties\\|tect\\)\\)\\|ublic\\)\\|[Rr]\\(e\\(turn\\(\\|s\\)\\|set\\|xx\\)\\)\\|s\\(ay\\|cientific\\|e\\(t\\(digits\\|form\\)\\|lect\\)\\|hared\\|ignal\\(\\|s\\)\\|ourceline\\|tatic\\|uper\\)\\|t\\(h\\(en\\|is\\)\\|o\\|ra\\(ce\\|nsient\\)\\)\\|u\\(n\\(til\\|used\\)\\|pper\\)\\|v\\(olatile\\)\\|w\\(h\\(en\\|ile\\)\\)\\)\\>" 1 font-lock-keyword-face nil)
        '("\\.\\(a\\(b\\(brev\\|s\\)\\|ddlib\\)\\|b\\(2x\\)\\|c\\(2\\(d\\|x\\)\\|ent\\(re\\|er\\)\\|ha\\(ngestr\\|rat\\)\\|lose\\|o\\(mpare\\|p\\(ies\\|yindexed\\)\\|untstr\\)\\)\\|d\\(2\\(c\\|x\\)\\|at\\(atype\\|e\\)\\|el\\(str\\|word\\)\\)\\|e\\(quals\\|xists\\)\\|f\\(orm\\(at\\|word\\)\\)\\|h\\(ashcode\\)\\|i\\(nsert\\)\\|l\\(astpos\\|e\\(ft\\|ngth\\)\\|ower\\)\\|m\\(ax\\|in\\)\\|o\\(p\\(a\\(dd\\|nd\\)\\|cc\\|ccblank\\|div\\|divl\\|eq\\|eqs\\|gt\\|gt\\(eq\\|eqs\\|s\\)\\|lt\\|lt\\(eq\\|eqs\\|s\\)\\|m\\(inus\\|ult\\)\\|not\\|not\\(eq\\|eqs\\)\\|or\\|p\\(lus\\|ow\\)\\|rem\\|sub\\|xor\\)verlay\\)\\|p\\(os\\)\\|r\\(everse\\|ight\\)\\|s\\(equence\\|ign\\|pace\\|trip\\|ub\\(str\\|word\\)\\)\\|t\\(o\\(b\\(oolean\\|yte\\)\\|char\\|chararray\\|double\\|float\\|int\\|long\\|short\\|string\\)\\|r\\(anslate\\|unc\\)\\)\\|u\\(pper\\)\\|v\\(erify\\)\\|w\\(ord\\(\\|index\\|lengh\\|pos\\|s\\)\\|rite\\(ch\\|ln\\)\\)\\|x\\(2\\(b\\|c\\|d\\)\\)\\)\\>" 1 font-lock-regina-method-face nil)
     '( "\\.\\([a-zA-Z0-9_]+\\)(" 1 font-lock-method-face nil)
     ))
  
 ;; Level 3 -  ports
 (setq regina-font-lock-keywords-3
       (append
        regina-font-lock-keywords-1
        (list
         ;; class statement
         (list
          "class *\\(\\<\\w*\\>\\)" '(1 font-lock-variable-name-face nil))
         ;; exit statement
         (list
          "exit \\(\\<.*\\>\\)" '(1 font-lock-variable-name-face nil))
         ;; extends keyword
         (list
          "extends \\(\\<.*?\\>\\)" '(1 font-lock-variable-name-face nil))
         ;;       (list
         ;;        "extends \\(\\<.*?\\>\\) implements \\(\\<.*?\\>\\)" '(1 font-lock-variable-name-face nil))
         ;; implements keyword
         (list
          "implements \\(\\<.*?\\(,[ \t]*.*?\\)*?\\>\\)" '(1 font-lock-variable-name-face nil))
         ;;       (list
         ;;        "\\(extends \\(\\<.*?\\>\\)\\)* implements \\(\\<.*?\\>\\)" '(3 font-lock-variable-name-face nil))
         ;; import statements
         (list
          "import \\(\\<.*\\>\\)" '(1 font-lock-constant-face nil))
         ;; user function names
         (list
          "method \\(\\<.*\\>\\)(" '(1 font-lock-function-name-face nil))
         ;; options statement (note: the binary and the trace keyword clash with the ones
         ;; in the first list, which is needed for the class keyword.
         (list
          (concat
           "options \\(\\( ?\\|\\<\\(no\\)?"
           "\\(binary\\|"
           "c\\(o\\(m\\(ments\\|pact\\)\\|nsole\\)\\|rossref\\)\\|"
           "d\\(ecimal\\|iag\\)\\|"
           "explicit\\|"
           "java\\|"
           "format\\|"
           "logo\\|"
           "replace\\|"
           "s\\(avelog\\|ourcedir\\|trict\\(args\\|assign\\|case\\|import\\|props\\|signal\\)\\)\\|"
           "trace\\(\\|1\\|2\\)\\|"
           "utf8\\|"
           "verbose\\(\\|0\\|1\\|2\\|3\\|4\\|5\\)"
           "\\)\\>\\)+\\)")
          '(1 font-lock-variable-name-face nil))
         ;; package statement
         (list
          "\\<package[ \t]+\\(\\<.*\\>\\)" '(1 font-lock-constant-face nil))
         ;; returns statement
         (list
          "return[s]?[ \t]+\\(this\\.\\)?\\(\\<.*?\\>\\)" '(2 font-lock-variable-name-face nil))
         ;; signals keyword
         (list
          "signals[ \t]+\\(\\<.*?\\>\\)" '(1 font-lock-variable-name-face nil))
         ;; trace options
         (list
          "trace \\(\\<all\\|methods\\|off\\|results\\>\\)" '(1 font-lock-variable-name-face nil)))))


 (defvar regina-font-lock-keywords regina-font-lock-keywords-3
   "Default highlighting expressions for Regina mode")

 (defun regina-indent-line ()
   "Indent the current line as REGINA code.
 The following rules apply:

  0- All keywords are matched from the beginning of the line.

  1- If we are at the beginning of the buffer, indent to column 0.

  2- If we see the keyword \"class\" without the keyword
     \"dependent\", then indent to column 0.

  3- If we see the keyword \"class\" with the keyword
     \"dependent\", then indent to `regina-indent-amount'.

  4- If we see the keyword \"method\" then indent to
     `regina-indent-amount'.

  5- If we see the keyword \"when\" or \"otherwise\", the indent
     amount is relative to the matching \"select\" statement, plus
     `regina-indent-amount'.

  6- If we look at the statement \"else\", indent it to the same
      amount as the corresponding \"if\", taking nested ifs into
      account.

  7- If we see the statement \"catch\" or \"finally\", look for
     the matching \"do\" and set the indent amount to the same as
     that \"do\". Take nested blocks into account. See rule 11 for
     a definition of a block start.

  8- \"end\" should be matched to the corresponding \"do\",
     \"loop\" or \"select\".

  9- The first line of a multi-line comment should be indented
     like a normal line. When the second line of a multi-line
     comment starts with a \"*\", then align that \"*\" with the
     first \"*\" of the first line of the multi-line comment,
     otherwise indent it so that it starts two positions after the
     \"*\" of the first comment line. This will make sure that the
     comment-text is aligned in a correct way.

 10- If the previous non-empty line contains the uncommented
     keyword \"then\", then indent the next line. The line after
     that should not be indented, except when it is part of a
     \"do\"-construct.

 11- If the previous line is ended with the continuation character
     \"-\", then find the first line with that continuation
     character. Find the characther \"(\" on that line and set the
     indentation to the next column. If the \"(\" couldn't be
     found, then look for the \" character and indent to there.

 12- If a line follows a \"block\"-start, increase the indentation
     with `regina-indent-amount'.  A \"block\"-start consists of
     one of the following keywords: \"catch\", \"class\", \"do\",
     \"then\", \"else\", \"loop\", \"method\", \"select\" or
     \"otherwise\".  This should consider nested \"if\" constructs
     and do the right thing.

 13- If we first see an \"do\"-construct ending, the we should
     indent the current line to the same indentation as the
     \"do\"-construct ending. Except when the keyword \"end\"
     matches a keyword \"do\" which comes right after an \"else\",
     \"then\" or \"otherwise\".

 14- If no indentation rule matches, then indent the same amount
     as the previous line.

 15- Javadoc comments should be indented to the same amount as the
     class or method they belong to."
   (interactive)
   ;; save current position, relative to point-max
   (let ((pos (- (point-max) (point))))
     (beginning-of-line)
     (cond ((bobp)
            (indent-line-to 0))
           ((looking-at "^[ \t]*class\\([ \t]\\|$\\)+") ; check for rule 2
            (if (looking-at "^[ \t]*class .*? dependent") ;check for rule 3
                (indent-line-to regina-indent-amount)
              (indent-line-to 0)))
           ((regina-looking-at-method-p) ; check for rule 4
            (indent-line-to regina-indent-amount))
           ((looking-at "^[ \t]*/\\*\\*") ; javadoc check for rule 15
            (if (save-excursion
                  (< (save-excursion
                       (let ((method-point (re-search-forward "^[ \t]*method\\b" nil t 1)))
                         (if method-point
                             method-point
                           (point))))
                     (let ((class-point (re-search-forward "^[ \t]*class\\b" nil t 1)))
                       (if class-point
                           class-point
                         (+ (point-max) 1)))))
                  ;; javadoc belongs to a method
                  (indent-line-to regina-indent-amount)
                ;; javadoc belongs to a class
                (if (looking-at "^[ \t]*class .*? dependent")
                    (indent-line-to regina-indent-amount)
                  (indent-line-to 0))))
           (t
            (let ((not-indented t) cur-indent)
              ;; check for rule 5
              ;; when commands should be aligned underneath each other and
              ;; and they should be indented relative to the select statement
              (when (or (looking-at "^[ \t]*when\\b")
                        (looking-at "^[ \t]*otherwise\\b"))
                (save-excursion
                  (let ((still-looking t))
                    (while still-looking
                      (forward-line -1)
                      (when (regina-looking-at-end-p)
                        (regina-from-end-goto-matching-do))
                      (cond ((looking-at "^[ \t]*when\\b")
                            (setq still-looking nil
                              not-indented nil
                              cur-indent (current-indentation)))
                            ((looking-at "^[ \t]*select\\b")
                             (setq still-looking nil
                                   not-indented nil
                                   cur-indent (+ (current-indentation) regina-indent-amount))))))))
             
              ;; check for rule 6
              ;; else should be aligned to the correct if, taking nested ifs
              ;; into account.
              (when (and not-indented
                       (regina-looking-at-else-p))
                  (save-excursion
                    (if (regina-from-else-goto-matching-if)
                        (setq cur-indent (current-indentation))
                      (regina-beep)
                      (message "Dangling else!")
                      (setq cur-indent 0))
                    (setq not-indented nil)))
             
              ;; check for rule 7
              ;; catch and finally should be aligned to their own do,
              ;; taking nesting into account
              (when (and not-indented
                         (looking-at "^[ \t]*\\(catch\\|finally\\)\\b"))
                (save-excursion
                  (let ((still-looking t)
                        (nesting-level 0))
                    (while still-looking
                      (forward-line -1)
                      (when (and (not (regina-looking-at-comment-p))
                                 (or (bobp)
                                     (looking-at "^[ \t]*\\(.*\\)?\\bthen do")
                                     (looking-at "^[ \t]*do\\b")
                                     (looking-at "^[ \t]*else do\\b")
                                     (looking-at "^[ \t]*\\(select\\|loop\\)\\b")))
                        (setq still-looking nil
                              not-indented nil
                              cur-indent (current-indentation)))
                      (when (regina-looking-at-end-p)
                        (regina-from-end-goto-matching-do))
                      ))))
             
              (when (and not-indented  ; check for rule 8
                         (regina-looking-at-end-p))
                ;; find the matching do, select, loop or catch. It should
                ;; take nested do / end pairs into account.
                (save-excursion
                  (setq not-indented nil
                        cur-indent (if (regina-from-end-goto-matching-do)
                                       (current-indentation)
                                     0))))
             
              ;; check for rule 9
              ;; check to see if we are within a comment
              (when (and not-indented
                         (regina-looking-at-comment-p))
                (save-excursion
                  ;; check to see if we're looking at a single line, or
                  ;; a multi-line comment.
                  (if (looking-at "^[ \t]*--") ; looking at --
                      ;; align -- to the -- on the previous line, if any
                      (let ((cur-comment (current-column))
                            (prev-comment nil)
                            (bol-pos      nil)) ;; beginning-of-line point pos
                        (forward-line -1)
                        ;; find out if there is still a -- on this line
                        (if (looking-at "^[ \t]*.*--")
                            (progn
                              (re-search-forward "--" nil t 1)
                              (setq prev-comment (- (current-column) 2))
                              (when (> prev-comment cur-comment)
                                (setq cur-indent prev-comment
                                      not-indented nil))))
 ;;                       ;; else, single -- line, align to the
 ;;                       ;; previous, non empty, non comment line.
 ;;                       (while (or (regina-looking-at-comment-p)
 ;;                                  (looking-at "^[ \t]*$"))
 ;;                         (forward-line -1))
 ;;                       (setq cur-indent (current-indentation)
 ;;                             not-indented nil)))
                        )
                      ;; else, not lookng at --
                      (when (not (looking-at "^[ \t]*/\\*")) ; looking at /*
                        (let (extra-indent)
                          (setq extra-indent 0)
                          (if (looking-at "^[ \t]*\\*") ; looking at *
                              (setq extra-indent 1)
                            (setq extra-indent 3))
                           
                          (while (not (or (looking-at "^[ \t]*/\\*")
                                          (bobp)))
                            (forward-line -1))
                            (when (looking-at "^[ \t]*/\\*")
                              (setq cur-indent (+ (current-indentation ) extra-indent)
                                    not-indented nil)))))))
             
              (when not-indented ; check for rule 10
                (save-excursion
                  (forward-line -1)
                  (while (and (not (bobp))
                              (or (regina-looking-at-comment-p)
                                  (save-excursion
                                    (forward-line -1)
                                    (regina-looking-at-continuation-p))))
                    (forward-line -1))
                  (when (and (not (looking-at "^[ \t].*\\(--\\|/\\*\\)[ \t]*\\(else\\|then\\)"))
                             (or (looking-at "^[ \t]*\\(.*\\)?\\bthen\\( do\\|[ \t]*\\(--.*\\|/\\*.*\\)?$\\)")
                                 (and (looking-at "^[ \t]*\\(if\\|when\\)\\b")
                                      (not (looking-at "^.*then\\b.+$")))
                                 (looking-at "^[ \t]*else\\( do\\|[ \t]*$\\)"))
                             (not (regina-looking-at-comment-p)))
                    (when (not (looking-at "^[ \t]*.*-[ \t]*$"))
                      (setq cur-indent (+ (current-indentation) regina-indent-amount)
                            not-indented nil)))))

              ;; continued lines should be aligned as much as possible
              ;; rule 11
              (when (and not-indented
                         (save-excursion
                           (forward-line -1)
                           (regina-looking-at-continuation-p)))

                (save-excursion
                  (forward-line -1)
                  (while (regina-looking-at-continuation-p)
                    (forward-line -1))
                  (forward-line 1) ;; we went back one row to many
                  (beginning-of-line)
                  (let ((beg (point))
                        (end (save-excursion
                               (end-of-line)
                               (point))))
                    (cond ((looking-at "^[ \t]*\\(if\\|when\\)\\b")
                           ;; indent to the first character after the
                           ;; first word
                           (beginning-of-line)
                           (forward-word 1)
                           (while (looking-at "[ \t]")
                             (forward-char 1))
                           (setq cur-indent (current-column)))
                          ((and (progn
                                  (beginning-of-line)
                                  (re-search-forward "(" end t 1))
                                  (not (regina-inside-comment-or-string-p)))
                           (setq cur-indent (current-column)))
                          ((and (progn
                                  (beginning-of-line)
                                  (re-search-forward "=" end t 1))
                                  (not (regina-inside-comment-or-string-p)))
                           (while (looking-at "[ \t]")
                             (forward-char 1))
                           (setq cur-indent (current-column)))
                          ((progn
                             (goto-char beg)
                             (re-search-forward "\"" end t 1))
                           (setq cur-indent (- (current-column) 1)))
                          (t
                           (setq cur-indent (+ (current-indentation) regina-indent-amount))))))
                (setq not-indented nil))

              (if not-indented ;check for rule 12 and 13
                  (progn
                    (save-excursion
                      (forward-line -1)
                      (while (not (or (regina-looking-at-end-p)
                                      (and (looking-at "^[ \t]*\\(c\\(atch\\|lass[ \t]\\)\\|\\(do\\|.*?then\\( do\\)?\\)\\|\\(else\\( do\\)?\\)\\|gl\\.glBegin\\|loop\\|method\\|select\\|otherwise\\)\\b")
                                           (not (regina-looking-at-comment-p)))
                                      (bobp)))
                        (forward-line -1))
                      (let ((still-looking t)
                            (nesting-level 1))
                        (if (regina-looking-at-end-p) ; rule 13
                            ;; now look for a matching "do" that follows an
                            ;; if or else.
                            (save-excursion
                              (if (regina-from-end-goto-matching-do)
                                  (save-excursion
                                    (if (regina-previous-line-else-or-then-p)
                                        (progn
                                          (when (regina-looking-at-else-p)
                                            (progn
                                              (regina-from-else-goto-matching-if)
                                              (while (regina-previous-line-else-or-then-p))))
                                          (setq still-looking nil
                                                not-indented nil
                                                cur-indent (current-indentation)))
                                      ;; else, previous line is no else or then
                                      (setq still-looking nil
                                            not-indented nil
                                            cur-indent (current-indentation))))
                                ;; else, no matching do found
                                (setq still-looking nil
                                      not-indented nil
                                      cur-indent 0))))
                        (when still-looking
                          (cond ((bobp)
                                 (setq cur-indent (current-indentation)
                                       not-indented nil))
                                ((looking-at "^[ \t]*\\(c\\(atch\\|lass[ \t]\\)\\|\\(do\\|.*?then do\\)\\|\\(else do\\)\\|gl\\.glBegin\\|loop\\|method\\|select\\|otherwise\\)") ; rule 12
                                 (setq cur-indent (+ (current-indentation) regina-indent-amount)
                                       not-indented nil))
                                ((and (regina-looking-at-else-p)
                                      (not (regina-looking-at-else-with-statement-p)))
                                 (regina-from-else-goto-matching-if)
                                 (while (regina-previous-line-else-or-then-p))
                                 (setq cur-indent (current-indentation)
                                       not-indented nil))
                               
                                ((and (looking-at "^[ \t]*.*?then\\b")
                                      (not (looking-at "^[ \t]*.*?then\\b.+$")))
                                 (while (regina-previous-line-else-or-then-p))
                                 ;; indentation
                                 (forward-line -1)
                                 (while (looking-at "^[ \t]*.*[^-]-[ \t]*$")
                                   (forward-line -1))
                                 (forward-line 1)
                                 (if (looking-at "^[ \t]*\\(if\\|when\\).*[^-]-[ \t]*$")
                                   (setq cur-indent (+ (current-indentation) regina-indent-amount)
                                         not-indented nil)
                                   ;; else
                                   (setq cur-indent (current-indentation)
                                       not-indented nil) ))))))))
             
              (when not-indented ; check for rule 14
                (save-excursion
                  (forward-line -1)
                  (setq cur-indent (current-indentation))))
             
              (when (< cur-indent 0)
                (setq cur-indent 0))
              (indent-line-to cur-indent)
              )))
     (when (> (- (point-max) pos) (point))
       (goto-char (- (point-max) pos)))))
  
 (defun regina-inside-comment-p ()
   "Checks if the point is inside a comment. 
 It returns true if the point is inside it, else it returns nil."
   (let ((origpoint (point))
         state)
     (save-excursion
       (goto-char 1)
       (while (> origpoint (point))
         (setq state (parse-partial-sexp (point) origpoint 0))))
       (nth 4 state)))

 (defun regina-inside-comment-or-string-p ()
   "Check if the point is inside a comment or a string.
 It returns the state from parse-partial-sexp for the search that
 terminated on the points position"
   (let ((origpoint (point))
         state)
     (save-excursion
       (goto-char 1)
       (while (> origpoint (point))
         (setq state (parse-partial-sexp (point) origpoint 0))))
     (or (nth 3 state)
         (nth 4 state))))

 (defun regina-inside-string-p ()
   "Check if the point is inside a comment or a string.
 It returns the state from parse-partial-sexp for the search that
 terminated on the points position"
   (let ((origpoint (point))
         state)
     (save-excursion
       (goto-char 1)
       (while (> origpoint (point))
         (setq state (parse-partial-sexp (point) origpoint 0))))
         (nth 3 state)))

 (defun regina-inside-javadoc-p ()
   "Checks if the point is inside a javadoc style comment. 
 It returns true if the point is inside it, otherwise it returns
 nil."
   (let ((retval (regina-looking-at-comment-p)))
     (when retval ;; now check to see if we are within a javadoc style
                  ;; comment. 
       ;; first we need to check if we are within
       ;; a multi-line comment
       (save-excursion
         (beginning-of-line)
         (setq retval (regina-inside-comment-p))
         (when retval
           ;; we are within a multi-line comment
           (let ((still-looking t))
             ;; find the start of the javadoc comments
             (while still-looking
               (forward-line -1)
               (setq still-looking (not (looking-at "^[ \t]*/\\*\\(\\*\\)?"))))))
         ;; Are we really looking at some javadoc style comment?
         (setq retval (looking-at "^[ \t]*/\\*\\*")))) ;; javadoc start
     retval))

 (defun regina-looking-at-comment-p ()
   "Returns true if the current line contains a comment."
   (let ((retval (regina-inside-comment-p)))
     (if (not retval)
         (save-excursion
           (beginning-of-line)
           (setq retval (or (looking-at "^[ \t]*--")
                            (looking-at "^[ \t]*/\\*"))))
     retval)))
    
 (defun regina-indent-newline-indent ()
   "Indents the current line before doing a regular newline-and-indent.
 If point is at the end of the line, or at the beginning of an
 emtpy line, then it only does a \\[new-line-indent]. Otherwise it
 moves the point to the beginning of the first word on the new
 line."
   (interactive)
   (save-excursion
     (when (and (regina-looking-at-comment-p)
                (save-excursion
                  (beginning-of-line)
                  (and (looking-at "^[ \t]*\\(\\*\\|/\\*\\)")
                       (not (looking-at "^.*\\*/")))))
       (insert "* "))
     (regina-indent-line))
   (when (and regina-auto-insert-javadoc
              (regina-looking-at-method-p))
     (regina-insert-javadoc-for-method))
   (if (or (eolp)
           (looking-at "[ \t]*$")
           (regina-looking-at-comment-p))
       (progn
         (newline-and-indent)
         (cond ((looking-at "\\*")
                (forward-char 1))
               ((looking-at "  ")
                (delete-char 1))
               ((looking-at " ")
                (forward-char 1))
               (t (save-excursion
                    ;;(regina-skeleton-insert)
                    ()))))
     ;; else
     (newline-and-indent)))

 (defun regina-indent-newline-indent-with-end-comment ()
   "Performs a \\[regina-indent-newline-indent], but before doing
 that, it checks to see if the current line contains the \"end\"
 statement. If that is the case, then the function
 \\[regina-insert-end-comment] is executed.  This results in a
 small comment behind the end, showing which statement it
 matches."
   (interactive)
   (when (regina-looking-at-end-p)
     (save-excursion
       (regina-insert-end-comment))
     (end-of-line))
   (regina-indent-newline-indent))

 (defun regina-looking-at-method-p ()
   "Returns t if the current line is the method statement"
   (save-excursion
     (beginning-of-line)
     (looking-at "^[ \t]*method\\b")))

 (defun regina-looking-at-end-p ()
   "Returns t if the current line matches the regexp \"^[ \t]*end\>\""
   (save-excursion
     (beginning-of-line)
     (looking-at "^[ \t]*\\(end\\>\\|gl\\.glEnd()\\)")))

 (defun regina-looking-at-do-p ()
   "Returns t if the current line contains the \"do\"-statement"
   (save-excursion
     (beginning-of-line)
     (or (looking-at "^[ \t]*do[ \t]*\\(\\(--\\|/\\*\\|label\\|protect\\).*\\)?$")
         (looking-at "^[ \t]*.*?\\(else\\|then\\)[ \t]do[ \t]*\\(\\(--\\|/\\*\\|label\\|protect\\).*\\)?$"))))

 (defun regina-looking-at-else-or-then-p ()
   "Returns t if the current line contains \"else\", \"then\" or
 \"otherwise\"."
   (save-excursion
     (beginning-of-line)
     (looking-at "^[ \t]*.*?\\(else\\|then\\|otherwise\\)\\b")))

 (defun regina-looking-at-else-p ()
   "Returns t if the current line contains \"else\"."
   (save-excursion
     (beginning-of-line)
     (looking-at "^[ \t]*else\\b")))

 (defun regina-looking-at-else-with-statement-p ()
   "Returns t if the current line contains \"else\" with a statement after it.
 The statement cannot be the keyword \"do\"."
 (save-excursion
   (beginning-of-line)
   (and (looking-at "^[ \t]*else[ \t]+[a-zA-Z0-9]+")
        (not (looking-at "^[ \t]*else[ \t]+do")))))

 (defun regina-previous-line-else-or-then-p ()
   "Returns t if the line before the current line matches
 \\[regina-looking-at-else-or-then-p]. If that is true, the point
 will be placed on that line, otherwise, the point won't be
 moved."
   (let ((retval)
         (cur-point))
     (save-excursion
       (forward-line -1)
       ;; disregard comments and disregard continued lines
       (while (or (regina-looking-at-comment-p)
                  (save-excursion
                    (forward-line -1)
                    (looking-at "^[ \t]*.*[^-]-[ \t]*$")))
         (forward-line -1))
       (setq retval (or (regina-looking-at-else-or-then-p)
                        (looking-at "^[ \t]*\\(if\\|when\\)[ \t]+"))
             cur-point (point)))
     (when retval
         (goto-char cur-point))
     retval))

 (defun regina-looking-at-continuation-p ()
   "Returns t if the current line ends with the continuation character."
   (save-excursion
     (beginning-of-line)
     (when (re-search-forward "-" (save-excursion (end-of-line) (point)) t 1)
       ;; now find out if it is within a string or comment, and if it
       ;; really is a continuation character and not a minus
       ;; character. For this to find out, anything after the `-' must be
       ;; either blanks, tabs, comments or a combination of those.
       (and (not (regina-inside-comment-or-string-p))
            (not (looking-at "-"))
            (looking-at "[ \t]*\\(\\(--.*$\\)\\|\\(/\\*.*$\\)\\|$\\)")))))

 ;;     (and (looking-at "^[ \t]*.*[^-]-[ \t]*\\(\\(--\\)\\|\\(/\\*\\)\\)?")
 ;;       (re-search-forward "-" nil t 1)
 ;;       (not (regina-inside-comment-p)))))
 (defun regina-beep ()
   "Beeps when `regina-beep-annoyingly' is not nil."
   (when regina-beep-annoyingly
     (ding)))

 (defun regina-looking-at-block-begin-p ()
   "Returns t if the current line contains the \"do\", \"loop\" or
 \"select\" statement."
 (save-excursion
   (beginning-of-line)
   (or (regina-looking-at-do-p)
       (looking-at "^[ \t]*gl\\.glBegin")
       (looking-at "^[ \t]*\\(else[ \t]*\\)?\\(select\\|loop\\)\\b\\(label\\)?"))))

 (defun regina-from-end-goto-matching-do ()
   "Finds the \"do\" matching the current \"end\".
 Starts from a line. Returns nil if the current line isn't an
 \"end\" statement. If, however, it is an end statement, it moves
 point to the line that contains the \"do\", \"loop\" or
 \"select\" statement that the starting end closes. It returns the
 number of lines searched backwards if it finds such a
 statement. If it encounters the beginning of the buffer, it will
 return nil and the cursor will be there, at the beginning of the
 buffer."
   ;; find the matching do, select, loop or catch. It should take
   ;; nested do / end pairs into account.
   (let ( (still-looking (regina-looking-at-end-p))
          (nesting-level 0)
          (retval (if (regina-looking-at-end-p)
                      0
                    nil )) )
     (setq nesting-level 0)
     (while still-looking
       (forward-line -1)
       (setq retval (+ retval 1))
       ;; we started from "end", so if we encounter another end, bump
       ;; up type nesting-level
       (if (regina-looking-at-end-p)
           (setq nesting-level (+ nesting-level 1))
         ;; else
         (when (and (regina-looking-at-block-begin-p)
                    (not (regina-looking-at-comment-p)))
           (if (eq nesting-level 0)
               (setq still-looking nil)
             ;; else
             (setq nesting-level (- nesting-level 1)))))
       ;; test to make sure we don't run away past the
       ;; beginning of the buffer
       (when (and (or (bobp)
                      (regina-looking-at-method-p))
                  still-looking)
         (setq still-looking nil
               retval nil)))
     retval))

 (defun regina-from-else-goto-matching-if ()
   "Finds the \"if\" matching the current \"else\".
 Starts from a line, returns nil if the current line isn't an
 \"else\" statement. If, however it is an else statement, it moves
 point to the line that contains the corresponding \"if\". It
 returns t if it finds such a statement. If it encounters the
 begin of the buffer, it will return nil and the cursor will be
 there, at the beginning of the buffer."
   (let ((still-looking t)
         (else-count (if (looking-at "^[ \t]*else\\b") 1 0))
         (retval nil))
     (while still-looking
       (if (or (bobp)
               (and (looking-at "^[ \t]*\\(else[ \t]+\\)?if\\b")
                    (eq else-count 0)))
           (setq still-looking nil
                 ;; return nil if we are at the beginning of buffer
                 retval (not (bobp)))
         ;; else
         (when (and (looking-at "^[ \t]*\\(else[ \t]+\\)?if\\b")
                    (> else-count 0))
           (setq else-count (- else-count 1)))
         (forward-line -1)
         (when (regina-looking-at-end-p) ; find the matching do
           (regina-from-end-goto-matching-do))
         (when (looking-at "^[ \t]*else\\b")
           (setq else-count (+ else-count 1)))
         (when (looking-at "^[ \t]*if\\b")
           (setq else-count (- else-count 1)))))
     retval))

 (defun regina-return-word ()
   "Returns the first word it encounters."
   (let ((beg) (end) (retval))
     ;; move to the beginning of the word
     (forward-word 1)
     (backward-word 1)
     ;; determine the word boundries
     (setq beg (point))
     (forward-word 1)
     (setq end (point))
     ;; copy the word into save-text
     (setq retval (buffer-substring-no-properties beg end))
     ;; underscores are part of the word
     (while (looking-at "_")
       (setq retval (concat retval "_"))
       (forward-char 1)
       (when (looking-at "[a-zA-Z0-9]")
         (setq retval (concat retval (regina-return-word)))))
     retval))

 (defun regina-return-previous-word ()
   "Returns the first word it encounters, looking backwards."
   (let ((beg) (end) (retval))
     ;; move to the beginning of the word
     (backward-word 1)
     ;; determine the word boundries
     (setq beg (point))
     (forward-word 1)
     (setq end (point))
     ;; copy the word into save-text
     (setq retval (buffer-substring-no-properties beg end))
     ;; underscores are part of the word
     (while (looking-at "_")
       (setq retval (concat retval "_"))
       (forward-char 1)
       (when (looking-at "[a-zA-Z0-9]")
         (setq retval (concat retval (regina-return-word)))))
     retval))


 (defun regina-insert-end-comment ()
   "inserts a comment right after an regina \"end\" statement
 that shows which \"do\", \"loop\" or \"select\" it matches."
   (interactive)
   (if (regina-looking-at-end-p)
       (let ((save-text) (lines-searched))
         (save-excursion
           (setq lines-searched (regina-from-end-goto-matching-do))
           (when (and lines-searched ;; make sure it's not nil
                      (> lines-searched regina-end-comment-treshold))
             (when (regina-looking-at-block-begin-p)
               (setq save-text (regina-return-word)))
             (when (string= save-text "do")
               (forward-line -1)
               (when (regina-looking-at-else-or-then-p)
                 (setq save-text (concat (regina-return-word) " " save-text)))
               )))
         ;; was there something put into save-text?
         (if save-text
             (progn
               (beginning-of-line)
               (forward-word 1)
               ;; no double end comments that are equal
               (if (looking-at (concat " -- " save-text))
                   (progn
                     (end-of-line)
                     (message "End comment \" -- %s\" is already in place" save-text))
                 ;;else
                 (end-of-line)
                 (insert " -- " save-text)))
           ;; else
           (when (not lines-searched)
             (regina-beep)
             (message "No match found for current \"end\"-statement")
             (end-of-line)
             (insert "-- Unmatched end!"))))
     (message "Not on an line with an \"end\"-statement")) )

 (defun regina-insert-end-comment-region (beg end )
   "Every end-statement in the region that matches
 \\[regina-looking-at-end-p] will get an end comment.

 See also \\[regina-insert-end-comment]."
   (interactive "*r")
   (when (> beg end)
     (let (mid)
       (setq mid beg
             beg end
             end mid)))
   (goto-char beg)
   (beginning-of-line)
   (while (< (point) end)
     (when (regina-looking-at-end-p)
       (regina-insert-end-comment))
     (forward-line)))

 (defun regina-sanitize-region ( beg end )
   "Removes double empty lines and trailing whitespaces and will
 comment out all \"trace results\" and \"trace methods\"
 statements that are not part of an \"if\" statement. All other
 lines are indented with \\[regina-indent-line].

 All blank lines between a multi-line comment and a method are
 removed."
   (interactive "r")
   (when (> beg end)
     (let (mid)
       (setq mid beg
             beg end
             end mid)))
   ;; need to convert e into a marker so that it moves
   ;; with buffer changes
   (goto-char end)
   (setq end (point-marker))
   ;; now convert the region
   (goto-char beg)
   (beginning-of-line)
   (while (and (< (point) end)
              (not (eobp)))
     (when (looking-at "[ \t]*$")
       (save-excursion
         (forward-line 1)
         (when (looking-at "[ \t]*$")
           (delete-blank-lines))))
     (when (looking-at "[ \t]*trace \\(results\\|methods\\)")
       (let ((i 0)
             (found-if nil))
         ;;look back only 5 rows
         (save-excursion
           (while (< i 4)
             (forward-line -1)
             (if (regina-looking-at-end-p)
                 (setq found-if nil
                       i 4)
               (if (looking-at "[ \t]*if\\b")
                   (setq found-if t
                         i 4)
                 (setq i (+ i 1))))))
         (when (not found-if)
           (forward-word 1)
           (backward-word 1)
           (insert "-- "))))
     (when (or (regina-looking-at-method-p)
               (looking-at "^[ \t]*class\\b"))
       ;; see if there is a blank line above separating a multi-line comment
       ;; and the current line.
       (save-excursion
         (when (and (save-excursion
                      (forward-line -1)
                      (looking-at "^[ \t]*$"))
                    (progn
                      (forward-line -1)
                      (while (looking-at "^[ \t]*$")
                        (forward-line -1))
                      (or (regina-inside-comment-p)
                          (looking-at "^[ \t]*/\\*"))))
           (delete-blank-lines))))
     (regina-indent-line)   
     (forward-line 1)
     )
   (delete-trailing-whitespace))

 (defvar regina-boundry-hit
   "used by `regina-next-method' and `regina-previous-method'"
   nil)

 (defun regina-next-method ()
   "Jumps to the next method definition."
   (interactive)
   (setq regina-boundry-hit (and (eq last-command 'regina-next-method)
                                  regina-boundry-hit))
   ;; should we look forward once or twice?
   (let ((i (if (regina-looking-at-method-p) 2 1)))
     (if (re-search-forward "^[ \t]*method\\b" nil t i)
         (progn
           (beginning-of-line)
           (setq regina-boundry-hit nil))
       (regina-beep)
       (if regina-boundry-hit
           (progn
             (message "Wrapping to beginning of buffer...")
             (goto-char (point-min))
             (if (re-search-forward "^[ \t]*method\\b" nil t 1)
                 (beginning-of-line)
               (message "No method available in buffer")
               (regina-beep))
             (setq regina-boundry-hit nil))
         (message "End of buffer...")
         (setq regina-boundry-hit t))))
   (setq last-command 'regina-next-method))
  
 (defun regina-previous-method ()
   "Jumps to the previous method definition."
   (interactive)
   (setq regina-boundry-hit (and (eq last-command 'regina-previous-method)
                                  regina-boundry-hit))
   ;; should we look backward once or twice?
   (let ((i  1))
     (if (re-search-backward "^[ \t]*method\\b" nil t i)
         (progn
           (beginning-of-line)
           (setq regina-boundry-hit nil))
       (regina-beep)
       (if regina-boundry-hit
           (progn
             (message "Wrapping to end of buffer...")
             (goto-char (point-max))
             (if (re-search-backward "^[ \t]*method\\b" nil t 1)
                 (beginning-of-line)
               (message "No method available in buffer")
               (regina-beep))
             (setq regina-boundry-hit nil))
         (message "End of buffer...")
         (setq regina-boundry-hit t))))
   (setq last-command 'regina-previous-method))

 (defun regina-beginning-of-method (&optional arg)
   "Jumps to the beginning of the method. 
 ARG repeats the search ARG times. It always returns t, unless no
 method is found."
   (interactive "p")
   (let ((beg (point)))
     (or (save-excursion
           (beginning-of-line)
           (regina-looking-at-method-p))
         (re-search-backward "^[ \t]*method\\b" nil t (or arg 1))
         (progn
           (goto-char beg)
           nil))))

 (defun regina-end-of-method (&optional arg)
   "Jumps to the end of the method.
 ARG repeats to search ARG times. It always returns t, unless no method
 end is found.

 Comments before the method are reckoned to be part of that method.
 Meaning that if point is at a comment that describes a method, this
 function will bring you to the end of that method."
   (interactive "p")
   (let ((beg (point)))
     (while (regina-looking-at-comment-p)
       (forward-line 1))
     (if (regina-beginning-of-method)
         (progn
           (re-search-forward "^[ \t]*method\\b" nil t (+ (or arg 1) 1))
           (beginning-of-line)
           (forward-line -1)
           (while (or (looking-at "^[ \t]*$")
                      (regina-looking-at-comment-p))
             (forward-line -1)))
       (progn
         (goto-char beg)
         nil))))

 (defun regina-select-current-block ()
   "Selects all lines between matching do (loop / select) and end.

 It will return t if it can find an \"end\" statement below the point
 and that \"end\" statement has a matching \"do\", \"loop\" or
 \"select\" statement.  The matching is done with
 \\[regina-from-end-goto-matching-do].

 If it cannot find such a statement, it will select the whole method.
 Belonging to that method are the comments written directly before the
 method statement. Normally these are the javadoc style comments, but
 it could be any kind of comment. This means that if point is at a line
 that contains a comment, it will skip forward until it finds a
 non-comment line. It will then select the whole method, including the
 comments before the method statement.

 When even that fails, it will return nil. "
   (interactive)
   (let (start)
     (setq start (point))
     (beginning-of-line)
     ;; store the current position of point
     (let ((beg (point))
           (still-looking t)
           (search-count 1)
           (retval nil))
       (while still-looking
         ;; find an end-statement below the point when looking back,
         ;; the point should be equal to or smaller than beg
         (goto-char beg)
         ;; we keep on looking until we pass another method definition
         (if (and (re-search-forward "^[ \t]*end\\b" nil t search-count)
                  (not (re-search-backward "^[ \t]*method\\b" beg t 1)))
             (progn
               (set-mark-command nil)
               (if (regina-from-end-goto-matching-do)
                   (progn
                     (setq still-looking (< beg (point)))
                     (if still-looking
                         (setq search-count (+ search-count 1))
                       (setq retval t)
                       ;; it makes more sense to have point at the
                       ;; "end" statement, so swap point and mark.
                       (exchange-point-and-mark)))
                 (pop-global-mark)
                 (setq retval nil)))
           (setq still-looking nil)))
       ;; if nothing found, try to match the whole method
       (if (not retval)
           (progn
             (goto-char beg)
             (if (or (regina-looking-at-method-p)
                      ;; include comments above the method, if found
                      ;; then return t
                      (progn
                        (while (regina-looking-at-comment-p)
                          (forward-line 1))
                        (regina-looking-at-method-p))
                      (re-search-backward "^[ \t]*method\\b" nil t 1))
                 (progn ;; now select all until next method. This
                   ;; includes all comments belonging to that method,
                   ;; written directly above the method.
                   (beginning-of-line)
                   (forward-line -1)
                   (while (regina-looking-at-comment-p)
                     (forward-line -1))
                   ;; we one line to far up, so compensate
                   (forward-line 1)
                   (set-mark-command nil)
                   ;; first find next method. eobp will do just fine
                   ;; too.  the first one it will find is the one we
                   ;; just jumped back to, hence the search count of 2.
                   (if (re-search-forward "^[ \t]*method\\b" nil t 2)
                       ;;select upto the next method, but not
                       ;; inclusive. Also do not select the comments
                       ;; that are direct before the method definition.
                       (progn
                         (beginning-of-line)
                         (forward-line -1)
                         (while (regina-looking-at-comment-p)
                           (forward-line -1))
                         ;; we one line to far up, so compensate
                         (forward-line 1))
                     (goto-char (point-max)))
                   (setq retval t))
               (setq retval nil))))
       ;; warn that something went awry
       (if (not retval)
           (progn
             (regina-beep)
             ;;    (set-mark-command nil)
             (goto-char start)
             (message "Not within a block!")))
       retval)))

 ;;; Line breaking and paragraph filling.
 (defun regina-fill-paragraph-function (&optional arg)
   "Function to assign to `fill-paragraph-function' that fills javadocs.
 It will reflow the comments listed in the javadoc. All lines that
 begin with @keyword get a special indentation.

 It will reflow strings, but it only works for strings on the
 current line. Otherwise, it might get confused about the string
 delimiters. In Regina it is possible to concatenate strings
 where the first one uses \" and the second one uses ' as a
 delimiter.

 "
   (interactive "*P")
   (save-excursion
     (cond ((regina-inside-javadoc-p)
            (regina-fill-comments))
           ((save-excursion
              (beginning-of-line)
              (looking-at "^[ \t]*.*--")) ;; this is done before the
                                          ;; normal comments because of
                                          ;; sequence errors.
            (regina-fill-single-line-comments))
           ((regina-inside-comment-p)
            (regina-fill-comments))
           ((regina-looking-at-method-p)
            (regina-fill-method))
           ((regina-inside-string-p)
            (regina-fill-string))
           (t ())
           ))
   ;; Always return true. This has the effect that if filling isn't
   ;; done above, it isn't done at all, and it's therefore effectively
   ;; disabled in normal code.
   t)

 (defun regina-fill-comments ()
   "called from `regina-fill-paragraph-function' to fill comments"
    ;; first find begin and end of the region we have to fill
   (let ((beg (point))
         (end (point))
         (still-looking t))
     ;; find the beginning
     (beginning-of-line)
     (while still-looking
       (when (or (looking-at "^[ \t]*/\\*") ;; looking at comment
                 ;; start
                 (looking-at "^[ \t]*\\(\\*\\)?[ \t]*@")) ;; looking at
         ;; javadoc tag
         (setq beg (point)
               still-looking nil))
       (forward-line -1))
     ;; find the end
     (setq still-looking t)
     (goto-char beg)
     (forward-line 1)
     (while still-looking
       (when (or (looking-at "^[ \t]*\\*/") ;; looking at comment end
                   (not (regina-looking-at-comment-p))
                   (looking-at "^[ \t]*\\(\\*\\)?[ \t]*@")) ;; looking at
         ;; javadoc tag
         (forward-line -1)
         (end-of-line)
         (setq end (point)
               still-looking nil))
       (forward-line 1))
     ;; now fill-out the paragraph between beg and end
     ;; mark the last line by inserting an empty one
     (goto-char end)
     (insert "\n")
     ;; strip the leading *, if any
     (replace-regexp "^[ \t]*\\*" " " nil beg end)
     ;; now join the lines together in one big line
     (goto-char beg)
     (forward-line 1)
     (while (not (looking-at "^[ \t]*$"))
       (delete-indentation)
       (forward-line 1))
     ;; now split the line at or around `fill-column'
     (goto-char beg)
     ;; are we dealing with a javadoc tag? If so, then calculate the
     ;; number of spaces to insert so we can line up after the tag.
     (let ((java-tag-p (looking-at "^[ \t]*\\(\\*\\)?[ \t]*@"))
           (java-tag-len 5))
       (when java-tag-p
         (save-excursion
           (let ((a (re-search-forward "@" end t 1))
                 (b (re-search-forward "[ \t]" end t 1)))
             (setq java-tag-len (+ (- b a) 1)))))
       ;; start splitting the line.
       ;; goto the end of the javadoc start definition and split from there.
       (let ((line-end (save-excursion (end-of-line) (point))))
         (cond ((re-search-forward "^[ \t]*/\\*\\*" line-end t 1)
                (forward-char 1)
                (regina-indent-newline-indent))
               ((re-search-forward "^[ \t]*/\\*" line-end t 1)
                (regina-indent-line))
               ((re-search-forward "^[ \t]*\\*" line-end t 1)
                (regina-indent-line))
               (t
                (regina-indent-line)
                (insert "* ")
                (regina-indent-line))))
       ;; now split the rest
       (while (eq (move-to-column fill-column) fill-column)
         ;; find a whitespace or tab at or before point, keep on
         ;; looking until you find one.
         (while (not (looking-at "[ \t]+"))
           (backward-char 1))
         (forward-char 1)
         ;; insert some spaces for the javadoc tag
         (when java-tag-p
           (insert-char ?\  java-tag-len)
           (backward-char java-tag-len))
         ;; now indent the line. This will insert the leading *
         (regina-indent-newline-indent)))
       ;; remove the inserted empty line.
       (delete-blank-lines)))

 (defun regina-fill-single-line-comments ()
   "Fills out single line comments based on the column the comment starts.

 Called from \\[regina-fill-paragraph]."
   ;; first find begin and end of the region we have to fill
   (let ((beg (point))
         (end (point))
         (still-looking t))
     ;; find the beginning
     ;; First line might be after some code. The rest must be on
     ;; otherwise empty lines.
     (beginning-of-line)
     (while still-looking
       (if (looking-at "^[ \t]*--")
           (forward-line -1)
         (re-search-forward "--" nil t 1)
         (setq beg (- (point) 2)
               still-looking nil)))
     ;; find the ending
     (forward-line 1)
     (beginning-of-line)
     (setq still-looking t)
     (while still-looking
       (if (looking-at "^[ \t]*--")
           (forward-line 1)
         (re-search-backward "--" beg t 1)
         (end-of-line)
         (setq end (point)
               still-looking nil)))
     ;; mark the ending
     (goto-char end)
     (end-of-line)
     (insert "\n")
     ;; remove all superfluous -- characters
     (replace-regexp "^[ \t]*--" " " nil (+ beg 2) end)
     ;; now join all lines together into one big line
     (goto-char beg)
 ;;     (when (bolp)
 ;;       (delete-indentation))
     (forward-line 1)
     (while (not (looking-at "^[ \t]*$"))
       (delete-indentation)
       (forward-line 1))
     ;; now split the line at or before `fill-column'
     (goto-char beg)
     (when (not (looking-at "[ \t]*--"))
       (insert "--")
       (backward-char 2)
       (regina-indent-newline-indent))
     ;; while there is still text on the fill-column
     (setq still-looking  (eq (move-to-column fill-column) fill-column))
     (while still-looking
       ;; find a whitespace or tab at or before point
       (while (not (looking-at "[ \t]+"))
         (backward-char 1))
       ;; make sure we still can split the line
       (if (save-excursion
                    (backward-char 3)
                    (looking-at "[ \t]*--"))
           (setq still-looking nil)
         ;; else
         (insert "--")
         (backward-char 2)
         (regina-indent-newline-indent)
         (setq still-looking  (eq (move-to-column fill-column) fill-column))))
     ;; remove the inserted empty line.
     (end-of-line)
     (forward-char 1)
     (kill-line)
 ))

 (defun regina-fill-method ()
   "Divides method parameters with continuation characters .

 When the method definition goes beyond `fill-column' and the
 method statement has parts that can be continued on the next
 line, then this is done. The result is not checked for crossing
 the `fill-column' border.

 Parts that can be continued on the next line are:
 - Parameters, separated by comma's
 - returns statement
 - signals statement
 - signals list, separated by comma's"
   (interactive)
   (when (regina-looking-at-method-p)
     ;; we have to see if we can split the line.
     ;; It must not be continued already
     ;; It must contain text at the fill-column
     (when (and (not (looking-at "[^-]-[ \t]*$"))
                (eq (move-to-column fill-column) fill-column))
       ;; temporarily turn of the automatic regina-auto-insert-javadoc
       (let ((old-value 'regina-auto-insert-javadoc)
             (end (end-of-line))
             (beg (beginning-of-line))
             (still-looking t))
         (setq regina-auto-insert-javadoc nil)
         ;; start from the beginning and find the first comma or )
         (while still-looking
           (if (re-search-forward ",\\|)" end t 1)
               (progn
                 (backward-char 1)
                 (if (looking-at ",")
                     (progn
                       (forward-char 1)
                       (insert " -")
                       (newline-and-indent)
                       (save-excursion
                         (setq end (end-of-line))))
                   ;; else
                   (setq still-looking nil)))
             ;; else
             (setq still-looking nil)))
         ;; find out whether we first should re-flow 'signal' or
         ;; 'returns'
         (let ((signals-pos (save-excursion (re-search-forward "signals" end t 1)))
               (returns-pos (save-excursion (re-search-forward "returns" end t 1))))
           (cond ((and signals-pos returns-pos)
                  (if (< signals-pos returns-pos)
                      (progn
                        (regina-fill-method-signals)
                        (regina-fill-method-returns)
                    ;; else
                    (regina-fill-method-returns)
                    (regina-fill-method-signals))))
                 (signals-pos
                  (regina-fill-method-signals))
                 (returns-pos
                  (regina-fill-method-returns))
                 (t
                  ())))
         ;; restore the regina-auto-insert-javadoc value
         (setq regina-auto-insert-javadoc old-value)
         ))))

 (defun regina-fill-method-signals ()
   "Fills out the signals clause of a method. Called from
 \\[regina-fill-method]. Starts at the beginning of the current
 line."
   (beginning-of-line)
   (let ((end (end-of-line))
         (still-looking t))
     (if (re-search-forward "signals" end t 1)
         (progn
           (while still-looking
             (insert " -")
             (newline-and-indent)
             (if (re-search-forward ",\\|returns" end t 1)
                 (setq still-looking (looking-at ","))
               ;; else
               (setq still-looking nil))))
       ;; else
       (message "Expected \"signals\"-statement, but none found when filling"))))
                      
 (defun regina-fill-method-returns ()
   "Fills out the returns clause of a method. Called from
 \\[regina-fill-method]."
 ;; don't know how to implement this yet.
 )

 (defun regina-fill-string()
   "Fills out a string by putting in continuation characters.
 It is called from `regina-fill-paragraph-funtion'.

 It only works for strings on the current line. Otherwise, it
 might get confused about the string delimiters. In Regina it is
 possible to concatenate strings where the first one uses \" and
 the second one uses ' as a delimiter. "
   (let ((beg (point))
         (end (point))
         (still-looking t))
     ;; find the beginning
     (while (regina-inside-string-p)
       (forward-char -1))
     ;; determine the string delimiter
     (let ((string-delim
            (buffer-substring-no-properties (point) (+ (point) 1))))
       (setq beg (point))
     ;; find the end
       (forward-char 1)
       (while (regina-inside-string-p)
         (forward-char 1))
       (setq end (point))
     ;; now fill-out the paragraph between beg and end. Mark the last
     ;; line by inserting an empty one.
 ;;     (goto-char end)
 ;;     (insert "\n")
     (goto-char beg)
     (while (eq (move-to-column fill-column) fill-column)
       ;; find the first blank preceding point
       (while (not (or (looking-at "[ \t]")
                       (bolp)))
         (forward-char -1))
       (if (bolp)
           (message "beginning of line reached, auto-fill not possible")
         ;; else, looking at [ \t]
         (forward-char 1)
         (insert string-delim " -" string-delim)
         (forward-char -1)
         (newline-and-indent))))))

 (defun regina-initial-template ()
   "Inserts default package, javadoc and class statements in a new file.

 The classpath to the root of the package is stored in
 `regina-package-path'
 It also generates a default constructor statement without any
 arguments. "
   (interactive)
   (let (beg end class-name)
     (goto-char (point-min))
     (insert "package " (buffer-file-name))
     (replace-regexp regina-package-path "" nil (point-min) (point-max))
     (save-excursion
       (replace-regexp "/" "." nil (point-min) (point-max)))
     ;; remove the last "."
     (end-of-line)
     (backward-word 2)
     (delete-char -1)
     (insert "\n\n")
     ;; save the class name
     (setq class-name (regina-return-word))
     ;; remove the current line containing the file name
     (delete-region (progn
                      (beginning-of-line)
                      (point))
                    (progn
                      (end-of-line)
                      (point)))
 ;;     (when regina-auto-insert-javadoc
     (when t
       (insert "/**\n")
       (insert " * Class " class-name " implements... \n")
       (insert " * <BR>\n")
       (insert "* Created on: " (format-time-string "%a, %d, %b %Y %H:%M:%S %z") "\n")
       (insert " * @version $id: $\n")
       (insert " */\n"))
     (goto-char (point-max))
     (insert "class " class-name "\n")
     (insert "\n")
 ;;     (when regina-auto-insert-javadoc
     (when t
       (insert "/**\n")
       (insert " * Default constructor\n")
       (insert " */\n"))
     (insert "method " class-name "()\n")
     (insert "return\n")
     (indent-region (point-min) (point-max) nil)
     ))

 (defun regina-insert-javadoc-for-method ()
   "Inserts an appropriate javadoc statement for the method.

 The javadoc based on:
 - the name of the method,
 - the name and type of the parameters,
 - the return type of the method."
   (interactive)
   (save-excursion
     (beginning-of-line)
     (save-excursion
       (when (and (regina-looking-at-method-p)
                  (save-excursion
                    (forward-line -1)
                    (not (regina-inside-javadoc-p))))
         (insert "/**\n")
         (insert "*\n")
         (insert "*/\n")
         ;; go back to the method definition
         (re-search-forward "^[ \t]*method[ \t]+" nil t 1)
         (let ((method-name (regina-return-word)))
           (forward-line -2)
           (end-of-line)
           (insert " Method " method-name " ..."))
         ;; go back to the method definition
         (re-search-forward "^[ \t]*method[ \t]+" nil t 1)
         (let ((param-count 1)
               (beg (point))
               (end (progn
                      (end-of-line)
                      (point))))
           (goto-char beg)
           (when (re-search-forward "-[ \t]*$" end t 1)
             (setq end (re-search-forward ")" nil t 1)))
           ;; need to convert end into a marker so that it moves with
           ;; buffer changes
           (goto-char end)
           (setq end (point-marker))

           ;; repeat this for all parameters. parameters are separated
           ;; by a , and optionally have a type
           (let ((still-looking t))
             (while still-looking
               (goto-char beg)
               (when (re-search-forward "(" end t 1)
                 (if (looking-at ")") ;;  looking at a method without
                     ;;  parameters.
                     (setq still-looking nil)
                   ;; else
                   (let ((param-name nil)
                         (param-type nil))
                     (or (re-search-forward "," end t param-count)
                         (re-search-forward ")" end t 1))
                     (backward-char 1)
                     ;; is there a type? Or is the param alone and of type Rexx?
                     (if (and (re-search-backward "=\\|," beg t 1)
                              (looking-at "="))
                         (progn
                           (backward-word 1)
                           (setq param-name (regina-return-word))
                           (forward-word 1)
                           (backward-word 1)
                           (setq param-type (regina-return-word))
                           (while (looking-at "\\.") ;; type is qualified
                             (setq param-type (concat param-type "." (regina-return-word))))
                           ;; is it an array?
                           (when (looking-at "\\[\\]")
                             (setq param-type (concat param-type "[]"))
                             (forward-char 2)))
                       ;; else, no parameter type, so it's Rexx
                       (when (looking-at ",\\|)")
                         (setq param-type "Rexx")
                         (or (looking-at ")")
                             (forward-word 1))
                         (backward-word 1)
                         (setq param-name (regina-return-word))))
                     (setq still-looking (not (looking-at ")"))
                           param-count (+ param-count 1))
                     (while (not (looking-at "^[ \t]*\\*/"))
                       (forward-line -1))
                     (insert "* @param " param-name " is a " param-type "\n")
                     ;; go back to the method definition
                     (re-search-forward "^[ \t]*method[ \t]+" nil t 1)
 ;;                  (save-excursion
 ;;                    (setq beg (progn (beginning-of-line) (point))
 ;;                          end (progn (end-of-line) (point)))
 ;;                    (goto-char beg)
 ;;                    (when (re-search-forward "-[ \t]*$" end t 1)
 ;;                      (setq end (re-search-forward ")" nil t 1)))
 ;;                    )
                     )))))
          
           ;; next, insert the @return javadoc tag, if needed
 ;;        (save-excursion
 ;;          (setq end (progn (forward-line 1) (end-of-line) (point))))
           (when (re-search-forward ") returns " end t 1)
             (let ((return-type (regina-return-word)))
               (while (looking-at "\\.") ;; return type is qualified
                 (setq return-type (concat return-type "." (regina-return-word))))
               (forward-line -1)
               (insert "* @return " return-type " containing ...\n")))
           )))
     ;; indent the new stuff
     (let ((beg (point))
           (end (progn
                  (re-search-forward "^[ \t]*method\\b" nil t 1)
                  (point))))
       (indent-region beg end nil))))


 (defun regina-close-block()
   "Closes the current do / select / loop with an end."
   (interactive)
   (let (found)
     (save-excursion
       (while (not (or (regina-looking-at-block-begin-p)
                       (regina-looking-at-method-p)))
         (forward-line -1))
       (setq found (or (looking-at "^[ \t]*\\(select\\|loop\\)\\b")
                       (regina-looking-at-do-p))))
     (if found
         (progn
           (beginning-of-line)
           (if (looking-at "^[ \t]*$")
               (insert "end")
             ;; else
             (end-of-line)
             (insert "\nend"))
           (funcall (local-key-binding "\C-m")))
       ;; else
       (message "No block available for closing."))))

 (defun regina-version ()
   "Displays the current version of regina mode."
   (interactive)
   (message "Regina-mode version %s." regina-mode-version))

 ;; ------------ speedbar additions ------------
 (require 'speedbar)

 (defcustom regina-imenu-generic-expression
   (list
    '("method" "^[ \t]*method \\([a-zA-Z0-9_]*\\)" 1)
    '("class" "^[ \t]*class \\([a-zA-Z0-9_]*\\)" 1))
   "Value for `imenu-generic-expression' in Regina mode."
   :type 'regexp
   :group 'regina)

 ;; (eval-when-compile (require 'speedbar))
 (speedbar-add-supported-extension ".nr[xy]")
 (add-to-list 'speedbar-fetch-etags-parse-list '("\\.nr[xy]\\'" . "^[ \t]*method [a-zA-Z0-9]*"))

 (defun regina-speedbar-buttons (buffer)
   "Create a speedbar display to help navigation in a regina file.
 BUFFER is the buffer speedbar is requesting buttons for."

 )

 (defun regina-skeleton-insert ()
   "Inserts skeleton do, select, if and loop statements."
   (let ((word (regina-return-previous-word)))
     (cond ((equal word "do")
            (regina-do))
           ((equal word "loop")
            (regina-loop))
           (t nil))))

 (defvar regina-mode-syntax-table
   (let ((st (make-syntax-table)))
     (modify-syntax-entry ?. "." st)
     (modify-syntax-entry ?- ". 12b" st)
     (modify-syntax-entry ?/ ". 14a" st)
     (modify-syntax-entry ?* ". 23a" st)
     (modify-syntax-entry ?\n "> b" st)
     (modify-syntax-entry ?\' "\"" st)
     st)
   "Syntax table in use in REGINA-mode buffers.")


 ;; (defun regina-create-syntax-table ()
 ;;   (if regina-mode-syntax-table
 ;;       ()
 ;;     (setq regina-mode-syntax-table (make-syntax-table))
 ;;     (modify-syntax-entry ?. "." regina-mode-syntax-table)
 ;;     (modify-syntax-entry ?- ". 12b" regina-mode-syntax-table)
 ;;     (modify-syntax-entry ?/ ". 14" regina-mode-syntax-table)
 ;;     (modify-syntax-entry ?* ". 23" regina-mode-syntax-table)
 ;;     (modify-syntax-entry ?\n "> b" regina-mode-syntax-table)
 ;;     (modify-syntax-entry ?\' "\"" regina-mode-syntax-table))
 ;;   (set-syntax-table regina-mode-syntax-table))
 ;;     (modify-syntax-entry ?\\ "\\" regina-mode-syntax-table)
 ;;     (modify-syntax-entry ?/ ". 14" regina-mode-syntax-table)
 ;;     (modify-syntax-entry ?* ". 23" regina-mode-syntax-table)
 ;;     (modify-syntax-entry ?+ "." regina-mode-syntax-table)
 ;;     ;;   (modify-syntax-entry ?- ". 12b" regina-mode-syntax-table)
 ;;     ;;   (modify-syntax-entry ?\n "> b" regina-mode-syntax-table)
 ;;     (modify-syntax-entry ?= "." regina-mode-syntax-table)
 ;;     (modify-syntax-entry ?% "." regina-mode-syntax-table)
 ;;     (modify-syntax-entry ?< "." regina-mode-syntax-table)
 ;;     (modify-syntax-entry ?> "." regina-mode-syntax-table)
 ;;     (modify-syntax-entry ?& "." regina-mode-syntax-table)
 ;;     (modify-syntax-entry ?| "." regina-mode-syntax-table)
 ;;     (modify-syntax-entry ?\' "\"" regina-mode-syntax-table))


 (defun regina-mode ()
 "Major mode for editing REGINA code.

 Variable controlling indentation style:
  `regina-indent-amount'
         The basic indentation for do-blocks.

 Variable controlling end-comments:
 `regina-end-comment-treshold'
    Number of lines the matching block beginning has to be away from
    the end statement.

 Turning on REGINA mode calls the value of the variable
 regina-mode-hook with no args, if that value is non-nil.

 ;;For example:
 ;; (setq regina-mode-hook  '(lambda ()
 ;;                       (setq regina-indent-amount 2)
 ;;                      (local-set-key `\\C-m' 'regina-indent-newline-indent)
 ;;                      ))

 Two extra keymappings are defined:
 C-c C-n maps to M-x `regina-next-method' and
 C-c C-p maps to M-x `regina-previous-method'.

 For convenience it is possible to map
 `regina-indent-newline-indent-with-end-comment' instead of
 \\[regina-indent-newline-indent] to C-m in the above example. This
 will place a small -- style comment right after every
 \"end\"-statement, containing the matching do, if, et cetera
 statement.

 If the variable `regina-auto-insert-javadoc' is true, then invoking
 `regina-indent-new-line-indent' or
 `regina-indent-newline-indent-with-end-comment' on a line contatining
 a method definition will insert a javadoc skeleton for that method.

 A further convenience method is \\[regina-sanitize-region], which
 will remove all non-single blank lines from the file. It will also
 look for \"trace results\" and \"trace methods\" statements and
 comment them if within 4 lines back no \"if\" statement is found.

 "
   (interactive)
   (kill-all-local-variables)
   (set-syntax-table regina-mode-syntax-table)
   (setq font-lock-keywords-case-fold-search nil)
   (make-local-variable 'font-lock-defaults)
   (setq font-lock-defaults '(regina-font-lock-keywords))
   (make-local-variable 'indent-line-function)
   (setq indent-line-function 'regina-indent-line)
   (make-local-variable 'comment-start)
   (setq comment-start "/*")
   (make-local-variable 'comment-end)
   (setq comment-end "*/")
   (make-local-variable 'imenu-case-fold-search)
   (setq imenu-case-fold-search t)
   (make-local-variable 'imenu-generic-expression)
   (setq imenu-generic-expression regina-imenu-generic-expression)
   (make-local-variable 'fill-paragraph-function)
   (setq fill-paragraph-function 'regina-fill-paragraph-function)
   (setq major-mode 'regina-mode)
   (setq mode-name "Regina")
   (use-local-map regina-mode-map)
   (imenu-add-menubar-index)
   (setq skeleton-pair t)
 ;;   (make-local-variable 'skeleton-pair-alist)
 ;;   (make-local-variable 'skeleton-pair-filter)
   (run-hooks 'regina-mode-hook))



;;; Code:

(defvar rexx-developing-mode-docs nil
  "This should only be true if you are working on doc/completion tables
and you need them to be rebuilt every time you re-evaluate rexx-mode.")

(provide 'regina-mode)

(defvar rexx-additional-doc-files nil
  "*This specifies any additional doc/completion files you would
like loaded.  For instance, I have included a completion
file for Watcom VX-Rexx with this package.  If you wish to
define your own list, you should define a structure that
looks like this in your .emacs file:
	(setq rexx-additional-doc-files '(\"rexx-os2\"
					  \"vxrexx-doc\"
					  \"unzipapi-doc\"
					  \"rxextras-doc\"))")

;(defvar rexx-doc-file "rexx-doc"
;  "*Specifies file to use for REXX documentation and completion.\n
;This currently defaults to "rexx-os2".  The file "rexx-os2.el"
;should have been included with this packet and contains
;fairly complete documentation for OS/2 internal and external
;commands as well as nicer capitalization for many function
;names.  If you wish to use stripped down tables to conserve
;memory, add
;	(setq rexx-doc-file \"rexx-sml\")
;to you .emacs file.  Alternatively, if you wish to replace
;this file with your own, add
;	(setq rexx-doc-file \"my-file\")
;or something along those lines to your .emacs.
;You can disable this feature altogether with
;	(setq rexx-doc-file nil)")
(defvar rexx-doc-file nil)

(defvar rexx-command-auto-upper nil
  "*If this is t, rexx-mode will automatically convert all
REXX command and internal function names to upper case.
If it is 1, it will capitalize.  If 2 or higher, it will
capitalize functions and uppercase commands.")

(defvar rexx-external-function-auto-capitilize nil
  "*If non-nil, rexx-mode will automagically fix capitalization
for any external functions it knows about.")

(defvar rexx-auto-build-procedure-table nil
  "*If non-nil, rexx-mode will automatically build a local table
of procedures defined in the current buffer.  These are then
added to the completion table for this buffer.")

(defvar rexx-super-completion-mode nil
  "*If non-nill, enables command specific completion functions.")

(defvar rexx-command-table nil
  "Table of REXX commands for rexx-command-auto-upper.")

(defvar rexx-external-function-table nil
  "Table of REXX external functions for rexx-external-function-auto-capitilize.")

(defconst rexx-user-procedure-table nil
  "Table of REXX user procedures defined in the current file.  This is
created automatically for each buffer if rexx-auto-build-procedure-table
is non-nil.")

(make-variable-buffer-local 'rexx-user-procedure-table)

(if (or (not rexx-command-table)
	rexx-developing-mode-docs)
    (progn
      (if rexx-developing-mode-docs
	  (progn
	    (setq rexx-command-table nil)
	    (setq rexx-external-function-table nil)
	    (setq rexx-command-and-function-table nil)))
      (if rexx-doc-file
	  (load rexx-doc-file))
      (let ((scan rexx-additional-doc-files))
	(while (car scan)
	  (load (car scan))
	  (setq scan (cdr scan))))))

(defconst rexx-command-and-function-table nil
  "Combined table of REXX commands and external functions for help.")
(setq rexx-command-and-function-table
      (append rexx-command-table rexx-external-function-table))

(defvar rexx-build-eval nil
  "*If this is defined, it is evaluated (executed) just after the
rexx-user-procedure-table is cleared and recreated by scanning
the buffer but before it is appended to the command and external
function lists.  I am using this instead of a hook so that this
can be buffer local.

You can use this to add names of your own from some other source
or to change the way the scan works altogether.  My VX-Rexx
extensions take advantage of this.")

(defvar rexx-warn-illegal-line-label t
  "*Warn the user if he might be using a line label that is not
preceded by a '_'.")

(defvar rexx-mode-abbrev-table nil
  "Abbrev table in use in REXX mode.")
(define-abbrev-table 'rexx-mode-abbrev-table ())


(defvar rexx-mode-syntax-table nil
  "Syntax table in use in REXX-mode buffers.")

(if rexx-mode-syntax-table
    ()
  (setq rexx-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "." rexx-mode-syntax-table)
  (modify-syntax-entry ?/ ". 14" rexx-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" rexx-mode-syntax-table)
  (modify-syntax-entry ?+ "." rexx-mode-syntax-table)
  (modify-syntax-entry ?- "." rexx-mode-syntax-table)
  (modify-syntax-entry ?= "." rexx-mode-syntax-table)
  (modify-syntax-entry ?% "." rexx-mode-syntax-table)
  (modify-syntax-entry ?< "." rexx-mode-syntax-table)
  (modify-syntax-entry ?> "." rexx-mode-syntax-table)
  (modify-syntax-entry ?& "." rexx-mode-syntax-table)
  (modify-syntax-entry ?| "." rexx-mode-syntax-table)
  (modify-syntax-entry ?. "_" rexx-mode-syntax-table)
  (modify-syntax-entry ?! "_" rexx-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" rexx-mode-syntax-table))

(defvar rexx-indent-level 3
  "*Indentation of REXX statements with respect to containing block.")
(defvar rexx-procedure-indent-level 3
  "*Indentation of REXX statements after a procedure label.")
(defvar rexx-procedure-arg-indent-level 0
  "*Indentation of ARG or PARSE ARG immediately after a procedure label.")
(defvar rexx-return-indent 0
  "*Indentation for RETURN.")
(defvar rexx-do-offset -3
  "*Extra indentation for DO after a THEN or ELSE.")
(defvar rexx-end-offset 0
  "*Indentation for END, compared with text being enclosed.")
(defvar rexx-when-offset 1
  "*Indentation for WHEN relative to SELECT.")
(defvar rexx-continued-statement-offset 1
  "*Extra indent for lines not starting new statements.")
(defvar rexx-expose-string "(Globals)"
  "*String to automatically display after PROCEDURE.")

(defconst rexx-style-alist
  '(("Maxwell"
     (rexx-indent-level               .  3)
     (rexx-procedure-indent-level     .  3)
     (rexx-procedure-arg-indent-level .  0)
     (rexx-return-indent	      .  0)
     (rexx-do-offset                  . -3)
     (rexx-end-offset		      .  0)
     (rexx-when-offset		      .  1)
     (rexx-continued-statement-offset .  1))
    ("Cowlishaw"
     (rexx-indent-level               .  3)
     (rexx-procedure-indent-level     .  0)
     (rexx-procedure-arg-indent-level .  0)
     (rexx-return-indent	      .  0)
     (rexx-do-offset                  . -3)
     (rexx-end-offset		      .  3)
     (rexx-when-offset		      .  3)
     (rexx-continued-statement-offset .  2))
    ("Wide"
     (rexx-indent-level               .  4)
     (rexx-procedure-indent-level     .  4)
     (rexx-procedure-arg-indent-level .  2)
     (rexx-return-indent	      .  0)
     (rexx-do-offset                  . -2)
     (rexx-end-offset		      .  0)
     (rexx-when-offset		      .  2)
     (rexx-continued-statement-offset .  2))))

(defvar rexx-tab-always-indent t
  "*Non-nil means TAB in REXX mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used.")



;; This is used by indent-for-comment
;; to decide how much to indent a comment in REXX code
;; based on its context.
(defun rexx-comment-indent ()
  (if (or
       (looking-at "^/\\*")		;Existing comment at bol stays there.
       (and (= comment-column 0)
	    (save-excursion
	      (skip-chars-backward " \t")
	      (bolp))))
      ;; If comment-column is 0, and nothing but space
      ;; before the comment, align it at 0 rather than 1.
      0
    (max (1+ (current-column))	;Else indent at comment column
	 comment-column)))	; except leave at least one space.


(defun electric-rexx-paren (arg)
  "If rexx-command-auto-upper is t, set all REXX commands to uppercase.
If it is 1 capitalize commands.  If it is any other number, it will
uppercase commands and capitalize functions.  It will also warn the
user if he/she typed 'CALL funcname('."
  (interactive "P")
  (if (not arg)
      (progn
	(if (save-excursion
	      (and
	       last-command-char
	       (condition-case nil
		   (progn
		     (forward-sexp -2)
		     t)
		 (error nil))
	       (looking-at "call[ \t]+\\w")))
	    (progn
	      (message "Are you sure you want a '(' after a CALL?")
	      (beep))
	  (rexx-do-auto-upper))))
  (if last-command-char
      (self-insert-command (prefix-numeric-value arg))))


(defun electric-rexx-space (arg)
  "If rexx-command-auto-upper is t, set all REXX commands to uppercase.
If it is 1 capitalize commands.  If it is any other number, it will
uppercase commands and capitalize functions."
  (interactive "P")
  (if (not arg)
      (rexx-do-auto-upper))
  (if last-command-char
      (self-insert-command (prefix-numeric-value arg))))


(defun electric-rexx-newline (arg)
  "If rexx-command-auto-upper is t, set all REXX commands to uppercase.
If it is non-nil and not t, capitalize commands."
  (interactive "P")
  (if (not arg)
      (progn
	(rexx-do-auto-upper)
	(rexx-indent-line)))
  (if arg
      (setq arg (prefix-numeric-value arg))
    (setq arg 1))
  (while (> arg 0)
    (newline)
    (setq arg (1- arg)))
  (rexx-indent-line))


(defun electric-rexx-do (arg)
  "Insert 'do' and correct line's indentation."
  (interactive "P")
  (if (and (not arg)
	   (eolp))
      (progn
	(if (= (char-syntax (preceding-char)) ?\w)
	    (let ((last-command-char ?\ ))
	      (electric-rexx-space nil)))
	(insert "do")
	(rexx-do-auto-upper))
    (self-insert-command (prefix-numeric-value arg))))


(defun electric-rexx-end (arg)
  "Insert 'end' and correct line's indentation.\n
Blink 'do' if blink-matching-paren is non-nil."
  (interactive "P")
  (if (and (not arg)
	   (eolp))
      (progn
	(if (= (char-syntax (preceding-char)) ?\w)
	    (let ((last-command-char ?\ ))
	      (electric-rexx-space nil)))
	(insert "end")
	(rexx-do-auto-upper)
	(rexx-indent-line)
	(if blink-matching-paren
	    (save-excursion
	      (if (rexx-backward-sexp)
		  (sit-for 1)))))
    (self-insert-command (prefix-numeric-value arg))))


(defun electric-rexx-colon (arg)
  "Insert colon and correct line's indentation."
  (interactive "P")
  (let ((here (point))
	(state (parse-partial-sexp (point-min) (point)))
	remainder)
    (if (and
	 (not arg)
	 (eolp)
	 (not (nth 4 state)))
	(progn
	  (save-excursion
	    (beginning-of-line)
	    (skip-chars-forward " \t")
	    (skip-chars-forward "a-zA-Z0-9_")
	    (setq remainder (upcase (buffer-substring (point) here))))
	  (cond
	   ((string= remainder "")
	    (let ((hold (copy-marker (point))))
	      (save-excursion
		(beginning-of-line)
		(delete-horizontal-space)
		(if (and rexx-auto-build-procedure-table
			 (not (= (following-char) ?_)))
		    (rexx-add-to-procedure-table (buffer-substring (point) hold)))
		(let* ((here (point))
		       (state (rexx-get-state (rexx-beginning-of-procedure) here)))
		  (if (car (cdr state))
		      (progn
			(if (or (not rexx-warn-illegal-line-label)
				(= (following-char) ?_))
			    (message "Label name within DO/END block. This may not work correctly.  See SIGNAL doc.")
			  (message "Label name within DO/END block. Non-procedure names should start with '_'."))
			(beep)))))
	      (self-insert-command 1)))
	   ((string= remainder ":")
	    (insert " PROCEDURE"))
	   ((string= remainder ": PROCEDURE")
	    (insert " EXPOSE"))
	   ((string= remainder ": PROCEDURE EXPOSE")
	    (insert " ")
	    (insert rexx-expose-string))
	   (t
	    (self-insert-command 1))))
      (self-insert-command (prefix-numeric-value arg)))))

(defun rexx-inside-parens-p ()
  (condition-case ()
      (save-excursion
	(save-restriction
	  (narrow-to-region (point)
			    (progn (rexx-beginning-of-procedure) (point)))
	  (goto-char (point-max))
	  (= (char-after (or (scan-lists (point) -1 1) (point-min))) ?\()))
    (error nil)))


(defun rexx-indent-command (&optional whole-exp)
  "Indent current line as REXX code, or in some cases insert a tab character.
If `rexx-tab-always-indent' is non-nil (the default), always indent current
line.  Otherwise, indent the current line only if point is at the left margin
or in the line's indentation; otherwise insert a tab.
A numeric argument, regardless of its value, means always indent line."
  (interactive "P")
  (rexx-do-auto-upper)
  (if whole-exp
      ;; If arg, always indent this line as C
      ;; and shift remaining lines of expression the same amount.
      (let ((shift-amt (rexx-indent-line))
	    beg end)
	(save-excursion
	  (if rexx-tab-always-indent
	      (beginning-of-line))
	  ;; Find beginning of following line.
	  (save-excursion
	    (forward-line 1) (setq beg (point)))
	  ;; Find first beginning-of-sexp for sexp extending past this line.
	  (while (< (point) beg)
	    (rexx-forward-sexp 1)
	    (setq end (point))
	    (skip-chars-forward " \t\n")))
	(if (> end beg)
	    (indent-code-rigidly beg end shift-amt "#")))
    (if (and (not rexx-tab-always-indent)
	     (save-excursion
	       (skip-chars-backward " \t")
	       (not (bolp))))
	(insert-tab)
      (rexx-indent-line))))


(defun rexx-indent-line ()
  "Indent current line as REXX code.
Return the amount the indentation changed by."
  (let ((indent (calculate-rexx-indent))
	beg shift-amt
	(case-fold-search t)
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (cond ((eq indent nil)
	   (setq indent (current-indentation)))
	  ((eq indent t)
	   (setq indent (calculate-rexx-indent-within-comment)))
	  (t
	   (skip-chars-forward " \t")
	   (cond ((or
		   (and (looking-at "when\\b")
			(not (looking-at "when\\s_")))
		   (and (looking-at "otherwise\\b")
			(not (looking-at "otherwise\\s_"))))
		  (setq indent
			(+
			 (save-excursion
			   (goto-char (rexx-start-of-block))
			   (rexx-forward-sexp -1 t)
			   (rexx-back-to-indentation))
			 rexx-when-offset)))
		 ((and (looking-at "else\\b")
		       (not (looking-at "else\\s_")))
		  (setq indent (save-excursion
				 (rexx-backward-to-start-of-if)
				 (current-indentation))))
		 ((and (looking-at "do\\b")
		       (save-excursion
			 (end-of-line 0)
			 (rexx-forward-sexp -1 t)
			 (and (looking-at "\\(then\\|\\else\\|otherwise\\)\\b"))))
		  (setq indent (+ indent rexx-do-offset))))))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    shift-amt))


(defun calculate-rexx-indent ()
  "Return appropriate indentation for current line as REXX code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment."
  (save-excursion
    (catch 'break
      (beginning-of-line)
      (let ((indent-point (point))
	    (case-fold-search t)
	    (start-of-prev (save-excursion
			     (rexx-backward-to-noncomment (point-min) t)
			     (rexx-start-of-line)))
	    state
	    temp
	    top-is-procedure
	    parse-start
	    calced-container
	    containing-sexp)
	(rexx-beginning-of-procedure)
	(if (looking-at "[a-z][a-z_0-9]*:")
	    (setq top-is-procedure t))
	(if (>= (point) indent-point)
	    (throw 'break nil)
	  (setq parse-start (point))
	  (setq state (rexx-get-state (point) indent-point))
	  (setq containing-sexp (car (cdr state)))
	  (setq calced-container t)
	  (if (or (nth 3 state) (nth 4 state))
	      (throw 'break (nth 4 state))))
	(cond 
	 ((looking-at "[ \t]*[a-z_][a-z_0-9]*:")
	     0)
	 ;; See if this is a continuation line
	 ((save-excursion
	    (end-of-line 0)
	    (eq (preceding-char) ?\,))
	  (+ (save-excursion
	       (goto-char start-of-prev)
	       (current-column))
	     rexx-continued-statement-offset))
	 ;; See if line is after a label
	 ((and (null containing-sexp)
	       (or
		(cond
		 ((and (looking-at "[ \t]*\\(return\\|\\exit\\)\\b")
		       (<= (point)
			   (rexx-start-of-unenclosed-block)))
		  rexx-return-indent)
		 ((save-excursion
		    (rexx-backward-to-noncomment parse-start t)
		    (beginning-of-line)
		    (looking-at "[a-z][a-z_0-9]*:"))
		  (if (or (looking-at "[ \t]*arg[ \t]")
			  (looking-at "[ \t]*parse[ \t]"))
		      rexx-procedure-arg-indent-level
		    rexx-procedure-indent-level))
		 ((and top-is-procedure
		       (save-excursion
			 (rexx-backward-to-noncomment (or parse-start (point-min)) t)
			 (beginning-of-line)
			 (and
			  (looking-at "[ \t]*\\(arg\\|parse\\)[ \t]")
			  (progn
			    (rexx-backward-to-noncomment (or parse-start (point-min)) t)
			    (beginning-of-line)
			    (looking-at "[a-z][a-z_0-9]*:")))))
		  rexx-procedure-indent-level)))))
	 ((looking-at "[ \t]*end\\b")
	  (goto-char (or containing-sexp
			 (rexx-calc-container)))
	  (+ (rexx-back-to-indentation) rexx-end-offset))
	 (t
	  ;; Statement level.  Is it a continuation or a new statement?
	  ;; Find previous non-comment character.
	  (if containing-sexp
	      (setq parse-start containing-sexp)
	    (or parse-start (setq parse-start (point-min))))
	  (rexx-backward-to-noncomment parse-start t)
	  (rexx-forward-sexp -1 t)
	     
	  ;; Now let's see what we're looking at
	  (cond
	   ((looking-at "\\(then\\|\\else\\|otherwise\\)\\b")
	    (goto-char start-of-prev)
	    (+ (current-column)
	       rexx-indent-level))
	   ((progn
	      (goto-char start-of-prev)
	      (save-excursion
		(rexx-backward-to-noncomment)
		(bobp)))
	    nil)
	   ((or (looking-at "[a-z][a-z_0-9]*:")
		(<= (point) parse-start))
	    (+ (current-column) rexx-indent-level))
	   (t
	    (goto-char start-of-prev)
	    (goto-char (rexx-start-of-unenclosed-block))
	    (rexx-back-to-indentation)))))))))


(defun rexx-calc-container ()
  (if calced-container
      containing-sexp
    (setq calced-container t)
    (setq containing-sexp
	  (save-excursion
	    (goto-char (rexx-start-of-block))
	    (cond ((looking-at "^[a-z][a-z_0-9]*:")
		   nil)
		  ((save-excursion
		     (rexx-forward-sexp -1 t)
		     (looking-at "\\(select\\|do\\)\\b"))
		   (rexx-forward-sexp -1 t)))))))


(defun rexx-start-of-unenclosed-block (&optional parse-start)
  (save-excursion
    (or parse-start (setq parse-start (point-min)))
    (let (temp)
      (while
	  (progn
	    (beginning-of-line)
	    (setq temp (point))
	    (rexx-backward-to-noncomment parse-start t)
	    (forward-sexp -1)
	    (and (looking-at "\\(then\\|\\else\\|otherwise\\)\\b")
		 (> (rexx-start-of-line) parse-start))))
      (goto-char (rexx-start-of-line temp)))))

(defun rexx-back-to-indentation ()
  (back-to-indentation)
  (current-column))

(defun rexx-start-of-line (&optional from)
  (if from
      (goto-char from))
  (if (bobp)
      (rexx-forward-to-noncomment)
    (save-excursion
      (end-of-line)
      (let (botl botw)
	(while
	    (and
	     (not (bobp))
	     (or
	      (progn
		(setq botl (save-excursion (beginning-of-line)
					   (rexx-back-to-indentation)
					   (point)))
		(setq botw -1)
		(while (> (point) botl)
		  (setq botw (point))
		  (rexx-forward-sexp -1 t)
		  (if (= botw (point))
		      (goto-char (point-min))))
		(if (= (point) botl)
		    (if
			(save-excursion
			  (end-of-line 0)
			  (eq (preceding-char) ?\,))
			(progn
			  (end-of-line 0)
			  t))
		  t)))))
	(if (bobp)
	    (rexx-forward-to-noncomment)
	  botl)))))


(defun calculate-rexx-indent-within-comment (&optional after-star)
  "Return the indentation amount for line inside a block comment.
Optional arg AFTER-STAR means, if lines in the comment have a leading star,
return the indentation of the text that would follow this star."
  (let (end star-start)
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (setq star-start (= (following-char) ?\*))
      (skip-chars-backward " \t\n")
      (setq end (point))
      (beginning-of-line)
      (skip-chars-forward " \t")
      (if after-star
	  (and (looking-at "\\*")
	       (re-search-forward "\\*[ \t]*")))
      (and (re-search-forward "/\\*[ \t]*" end t)
	   star-start
	   (not after-star)
	   (goto-char (1+ (match-beginning 0))))
      (if (and (looking-at "[ \t]*$") (= (preceding-char) ?\*))
	  (1+ (current-column))
	(current-column)))))


(defun rexx-backward-to-noncomment (&optional lim ignore-line-labels)
  (or lim (setq lim (point-min)))
  (let (opoint stop)
    (while (not stop)
      (setq opoint (point))
      (skip-chars-backward " \t\n\f;:" lim)
      (if ignore-line-labels
	  (if (save-excursion
		(beginning-of-line)
		(looking-at "_[a-z_0-9]*:"))
	    (beginning-of-line)))
      (while (and (>= (point) (+ 2 lim))
		  (progn
		    (save-excursion
		      (forward-char -2)
		      (looking-at "\\*/"))))
	(search-backward "/*" lim 'move))
      (setq stop (or (<= (point) lim)
		     (= (point) opoint)))))
  (point))

(defun rexx-forward-to-noncomment (&optional lim ignore-line-labels)
  (or lim (setq lim (point-max)))
  (let (opoint stop)
    (while (not stop)
      (setq opoint (point))
      (skip-chars-forward " \t\n\f;:" lim)
      (if ignore-line-labels
	  (while (looking-at "_[a-z_0-9]*:")
	    (rexx-forward-sexp)))
      (while (and (<= (point) (+ 2 lim))
		  (looking-at "/\\*"))
	(search-forward "*/" lim 'move))
      (setq stop (or (>= (point) lim)
		     (= (point) opoint)))))
  (point))

(defun rexx-looking-at-ignore-whitespace (str &optional lim ignore-line-labels)
  (save-excursion
    (rexx-forward-to-noncomment lim ignore-line-labels)
    (looking-at str)))

(defun rexx-backward-to-start-of-if (&optional limit)
  "Move to the start of the last \"unbalanced\" `if'."
  (or limit (setq limit (save-excursion (rexx-beginning-of-procedure) (point))))
  (let ((if-level 1)
	(case-fold-search t))
    (while (and (not (bobp)) (not (zerop if-level)))
      (rexx-backward-sexp)
      (cond ((looking-at "else\\b")
	     (setq if-level (1+ if-level)))
	    ((looking-at "if\\b")
	     (setq if-level (1- if-level)))
	    ((< (point) limit)
	     (setq if-level 0)
	     (goto-char limit))))))


(defun mark-rexx-procedure ()
  "Put mark at end of REXX procedure, point at beginning."
  (interactive)
  (push-mark (point))
  (rexx-end-of-procedure)
  (let ((where (point)))
    (rexx-beginning-of-procedure)
    (push-mark where nil t)))


(defun rexx-start-of-block ()
  "Find start of the block containing point."
  (interactive)
  (save-excursion
    (let ((from (point)))
      (while (and (not (bobp))
		  (not (looking-at "^[a-z][a-z_0-9]*:"))
		  (progn
		    (condition-case nil
			(rexx-forward-sexp -1)
		      (error
		       (if (save-excursion
			     (rexx-forward-sexp -1 t)
			     (not (looking-at "\\(do\\|select\\)\\b")))
			   (rexx-forward-sexp -1 t)))))
		  (not (= from (point))))
	(setq from (point)))
      (point))))


(defun rexx-indent-sexp ()
  "Reindent the current block of REXX."
  (interactive)
  (save-excursion
    (if
	(save-excursion
	  (skip-chars-forward "^\"'\n")
	  (or (= (following-char) ?')
	      (= (following-char) ?\")))
	(beginning-of-line 2))
    (let ((from (rexx-start-of-block)) to)
      (goto-char from)
      (cond ((looking-at "^[a-z][a-z_0-9]*:")
	     (rexx-end-of-procedure)
	     (setq to (point)))
	    ((save-excursion
	       (rexx-forward-sexp -1 t)
	       (looking-at "\\(select\\|do\\)\\b"))
	     (rexx-forward-sexp -1 t)
	     (rexx-forward-sexp 1)
	     (setq to (point)))
	    (t
	     (setq from (point-min))
	     (setq to (point-max))))
      (rexx-indent-region from to))))


(defun rexx-indent-region (start end)
  "Indent every line whose first char is between START and END inclusive."
  (interactive "r")
  (save-excursion
    (condition-case nil
	(progn
	  (goto-char start)
	  ;; Advance to first nonblank line.
	  (skip-chars-forward " \t\n")
	  (beginning-of-line)
	  (let ((rexx-tab-always-indent t)
		(endmark (copy-marker end))
		line last moved)
	    (if (> (- end start) 1000)
		(progn
		  (setq line 0)
		  (setq last (count-lines start end))))
	    (while (< (point) endmark)
	      (cond ((looking-at "[ \t]*/\\*")
		     (let ((here (point)))
		       (rexx-forward-to-noncomment)
		       (setq moved (1- (count-lines here (point))))
		       (if (= moved 0)
			   (progn
			     (beginning-of-line 2)
			     (setq moved 1)))))
		    ((looking-at "[ \t]*\n")
		     (forward-line)
		     (setq moved 1))
		    (t
		     (rexx-indent-line)
		     (forward-line)
		     (setq moved 1)))
	      (if line
		  (progn
		    (setq line (+ line moved))
		    (message "Line %d of %d" line last))))
	    (message "Done.")))
      (error
       (beep)
       (message "Parsing error around line %d" (count-lines (point-min) (point)))))))


(defun rexx-capitalize-sexp ()
  "Recapitalize the current block of REXX."
  (interactive)
  (save-excursion
    (if
	(save-excursion
	  (skip-chars-forward "^\"'\n")
	  (or (= (following-char) ?')
	      (= (following-char) ?\")))
	(beginning-of-line 2))
    (let ((from (rexx-start-of-block)) to)
      (goto-char from)
      (cond ((looking-at "^[a-z][a-z_0-9]*:")
	     (rexx-end-of-procedure)
	     (setq to (point)))
	    ((save-excursion
	       (rexx-forward-sexp -1 t)
	       (looking-at "\\(select\\|do\\)\\b"))
	     (rexx-forward-sexp -1 t)
	     (rexx-forward-sexp 1)
	     (setq to (point)))
	    (t
	     (setq from (point-min))
	     (setq to (point-max))))
      (rexx-capitalize-region from to))))


(defun rexx-capitalize-region (start end)
  "Correctly capitalize every command or function whose first char is between
START and END inclusive."
  (interactive "r")
  (let ((total (- end start))
	(rexx-warn-illegal-line-label nil))
    (save-excursion
      (goto-char start)
      (while (and (forward-word 1)
		  (<= (point) end))
	(message "Scanned %d of %d characters." (- (point) start) total)
	(rexx-do-auto-upper))
      (message "Scanned %d characters." total))))
      


(defun set-rexx-style (style &optional global)
  "Set REXX-mode variables to use one of several different indentation styles.
The arguments are a string representing the desired style
and a flag which, if non-nil, means to set the style globally.
\(Interactively, the flag comes from the prefix argument.)
Available styles are Maxwell, Cowlishaw and Wide."
  (interactive (list (completing-read "Use which REXX indentation style? "
                                      rexx-style-alist nil t)
		     current-prefix-arg))
  (let ((vars (cdr (assoc style rexx-style-alist))))
    (or vars
	(error "Invalid REXX indentation style `%s'" style))
    (while vars
      (or global
	  (make-local-variable (car (car vars))))
      (set (car (car vars)) (cdr (car vars)))
      (setq vars (cdr vars)))))



(defmacro sign (count)
  (list 'max -1 (list 'min 1 count)))

(defun rexx-forward-sexp (&optional count noerr)
  "REXX mode replacement for forward-sexps so it will recognize DO/END pairs."
  (interactive "p")
  (or count (setq count 1))
  (if (= count 0)
      (setq count 1))
  (let ((parse-sexp-ignore-comments t)	;always ignore comments
	(dir (sign count))		;dir should be either 1 or -1
	hold)				;this will track the current retval
    (while (/= count 0)			;we have to loop here, not in old func.
      (setq count (- count dir))
      (if (> dir 0)			;pick a direction and scan once
	  (setq hold (rexx-scan-forward-sexp (point) noerr))
	(setq hold (rexx-scan-backward-sexp (point) noerr)))
      (if (not hold)			;if we got nil, bail out
	  (setq count 0)))
    (if hold
	(goto-char hold))))

(defun rexx-backward-sexp (&optional arg noerr)
  "REXX mode replacement for forward-sexps so it will recognize DO/END pairs."
  (interactive "p")
  (or arg (setq arg 1))
  (rexx-forward-sexp (- arg) noerr))


(defun rexx-scan-sexps (from count &optional noerr)
  (if noerr
      (condition-case nil
	  (or (scan-sexps from count)
	      (if (> count 0)
		  (save-excursion
		    (goto-char from)
		    (beginning-of-line 2)
		    (point))
		nil))
	(error
	 (save-excursion
	   (if (> count 0)
	       (re-search-forward "\\(\\s\"\\|\\s\(\\)")
	     (re-search-backward "\\(\\s\"\\|\\s\(\\)"))
	   (point))))
    (or (scan-sexps from count)
	(if (> count 0)
	    (save-excursion
	      (goto-char from)
	      (beginning-of-line 2)
	      (point))
	  nil))))

(defun rexx-scan-forward-sexp (from &optional noerr)
  ;;get simple value from old func.
  (save-excursion
    (goto-char from)
    (cond ((and (not noerr)
		(rexx-looking-at-ignore-whitespace "end\\b"))
	   (error "Block ends prematurely"))
	  ((not
	    (rexx-looking-at-ignore-whitespace "\\(select\\|do\\)\\b"))
	   (rexx-scan-sexps from 1 noerr)) ;if this isn't 'do', return scan-sexps
	  ;;if 'do' or 'select', skip to matching 'end'
	  (t
	   (let ((depth 1))
	     (while (and (> depth 0)
			 (not (eobp)))
	       (goto-char (rexx-scan-sexps (point) 1 t))
	       (cond ((rexx-looking-at-ignore-whitespace "\\(select\\|do\\)\\b")
		      (setq depth (1+ depth)))
		     ((rexx-looking-at-ignore-whitespace "end\\b")
		      (setq depth (1- depth))))))
	   (if (eobp)
	       (if noerr
		   nil
		 (error "Containing message ends prematurely"))
	     (goto-char (scan-sexps (point) 1))
	     (point))))))

(defun rexx-scan-backward-sexp (from &optional noerr)
  (save-excursion
    (let (hold last)
      ;;get simple value from old func.
      (setq hold (rexx-scan-sexps from -1 noerr))
      (if (not hold)			;if old func returned nil, bail out
	  ()
	(goto-char hold)
	(cond
	 ;;are we trying to back out of a sexp illegally
	 ((and (not noerr)
	       (looking-at "\\(select\\|do\\)\\b"))
	  (error "Block ends prematurely"))
	 ;;see if we just skipped over 'end'; if not, return hold
	 ((looking-at "end\\b")
	  ;;if so, skip to matching 'do'
	  (let ((depth 1))
	    (while (> depth 0)
	      (goto-char (scan-sexps (point) -1))
	      (cond ((looking-at "\\(select\\|do\\)\\b")
		     (setq depth (1- depth)))
		    ((looking-at "end\\b")
		     (setq depth (1+ depth))))))
	  (setq hold (point)))
	;;if we're not looking at anything special, just return hold
	 (t hold))))))

(defun rexx-beginning-of-procedure ()
  "Move backward to the beginning of a REXX procedure or
to the top if point is not in a procedure.  Returns t.

A REXX procedure begins with a label followed by ':' i.e.
main:

Unfortunately, there is no distinction in REXX between the
beginning of a procedure and a line label.  Since line labels
are rarely used in REXX, I have adopted the convention that
a label preceeded by a '_' (i.e. '_aack:') is a line label,
anything else is a procedure label."
  (interactive)
  (if (not (bolp))
      (progn
	(beginning-of-line)
	(condition-case nil
	    (forward-sexp 1)
	  (error nil))))
  (condition-case nil
      (forward-sexp -1)
    (error nil))
  (re-search-backward "^[a-z][a-z_0-9]*:" nil 1)
  (point))


(defun rexx-end-of-procedure ()
  "Move forward to the end of a REXX procedure.  Returns t.

Since there is no definitive marker for the end of a procedure,
rexx-mode will assume that the current procedure ends before the
next one begins.  This is not always true but should usually
result in correct formatting anyway. (I hope-:)"
  (interactive)
  (condition-case nil
      (forward-sexp 1)
    (error nil))
  (if (re-search-forward "^[a-z][a-z_0-9]*:" nil 1)
      (condition-case nil
	  (forward-sexp -2)
	(error nil)))
  (forward-line 1)
  (point))


(defun rexx-get-state (from to)
  "Parse REXX syntax starting at FROM until TO; return status of parse at TO.
Parsing stops at TO or when certain criteria are met;
Point is set to where parsing stops.
Parsing assumes that FROM is the beginning of a function.
Value is a list of eight elements describing final state of parsing:
 0. depth in parens.
 1. character address of start of innermost containing block; nil if none.
 2. character address of start of last complete block terminated.
 3. non-nil if inside a string.
    (it is the character that will terminate the string.)
 4. t if inside a comment.
 5. t if following a quote character.
 6. the minimum paren-depth encountered during this scan.
 7. t if in a comment of style `b'.

arguments: (from to)"
  (let (state
	stack
	(next from)
	(depth 0))
    (save-excursion
      (goto-char from)
      (setq state (parse-partial-sexp from to -1))
      (or (nth 3 state)
	  (nth 4 state)
	  (progn
	    (goto-char to)
	    (setq stack (rexx-start-of-block))
	    (if (= stack from)
		(setq state nil)
	      (if (car state)
		  (setcar (cdr state) (scan-sexps stack -1)))))))
    (goto-char to)
    state))
	   

(defun rexx-do-auto-upper (&optional arg)
  (interactive "P")
  (if (or (not (= (char-syntax (preceding-char)) ?w))
	  (and (not rexx-command-auto-upper)
	       (not rexx-external-function-auto-capitilize)))
      ()
    (let* ((to (point))
	   (state (rexx-get-state (rexx-beginning-of-procedure) to))
	   lookfunc)
      (if (nth 4 state)
	  ()
	(setq lookfunc
	      (or (and
		   (char-or-string-p last-command-char)
		   (= last-command-char ?\())
		  (= (following-char) ?\()
		  (save-excursion
		    (and
		     (condition-case nil
			 (progn
			   (forward-sexp -2)
			   t)
		       (error nil))
		     (and
		      (looking-at "call[ \t]+\\w")
		      (not (looking-at "call[ \t]+\\(on\\|off\\)\\b")))))))
	(let* ((from
		(condition-case nil
		    (scan-sexps (point) -1)
		  (error to)))
	       (word (downcase (buffer-substring from to)))
	       (pmark (copy-marker (point)))
	       comm
	       scan
	       scanstr
	       precap)
	  (if (and
	       rexx-command-auto-upper
	       (not (nth 3 state))
	       (setq precap (assoc word rexx-command-table)))
	      (progn
		(setq comm (or
			    (null (elt precap 2))
			    (and
			     (integerp (string-match "mm" (elt precap 2)))
			     (or
			      (not (setq scan (string-match " *sub-command" (elt precap 2))))
			      (save-excursion
				(setq scanstr (substring (elt precap 2) (string-match "[a-z/A-Z]+ +sub-command" (elt precap 2)) scan))
				(while
				    (setq scan (string-match "/" scanstr))
				  (setq scanstr (concat (substring scanstr  0 scan) "\\|" (substring scanstr (1+ scan)))))
				(beginning-of-line)
				(re-search-forward scanstr from t))))))
		(if (and comm
			 lookfunc
			 (elt precap 2)
			 (not (string-match "function" (elt precap 2))))
		    (setq lookfunc nil))
		(cond ((or (and
			    (eq rexx-command-auto-upper t)
			    (or lookfunc comm))
			   (and
			    (> rexx-command-auto-upper 1)
			    (not lookfunc)
			    comm))
		       (upcase-region from to)
		       (rexx-do-super-completion precap))
		      ((or
			lookfunc
			(and
			 (eq rexx-command-auto-upper 1)
			 comm))
		       (if (stringp (car (cdr precap)))
			   (progn
			     (goto-char from)
			     (delete-region from to)
			     (insert-before-markers (car (cdr precap)))
			     (goto-char pmark))
			 (capitalize-region from to))
		       (rexx-do-super-completion precap))))
	    (if (and rexx-external-function-auto-capitilize
		     (setq precap (assoc word rexx-user-procedure-table))
		     (or arg
			 (if (and (nth 3 state)
				  (= (char-syntax (char-after (1- from))) ?\"))
			     (elt precap 4)
			   (and (null (elt precap 4))
				lookfunc))))
		(progn
		  (if (stringp (car (cdr precap)))
		      (progn
			(goto-char from)
			(delete-region from to)
			(insert-before-markers (car (cdr precap)))
			(goto-char pmark))
		    (capitalize-region from to))
		  (if rexx-developing-mode-docs
		      (setq precap (assoc word rexx-external-function-table)))
		  (rexx-do-super-completion precap))))
	  (if (and
	       rexx-warn-illegal-line-label
	       (not lookfunc)
	       (save-excursion
		 (and
		  (condition-case nil
		      (progn
			(forward-sexp -2)
			t)
		    (error nil))
		  (looking-at "signal[ \t]+\\w")
		  (not (string= word "on"))
		  (not (string= word "off")))))
	      (progn
		(message "Be sure you put a '_' before all non-procedure names.")
		(beep))))))))


(defun rexx-do-super-completion (precap)
  "If rexx-super-completion-mode is non-nil, point is at eol and
last-command-char is either \" \" or \"(\" then insert the
character, execute any commands in (elt precap 5) and, if
the last-command-char was \"(\", insert \")\""
  (if (and (elt precap 5)
	   rexx-super-completion-mode
	   (eolp)
	   (char-or-string-p last-command-char)
	   (or
	    (= last-command-char ?\ )
	    (= last-command-char ?\()))
      (progn
	(self-insert-command 1)
	(let ((last-command-char nil))
	  (funcall (eval (elt precap 5))))
	(if (= last-command-char ?\()
	    (insert ")"))
	(setq last-command-char nil))))



(defun rexx-complete-symbol ()
  "Perform completion on Lisp symbol preceding point.  That symbol is
compared against the symbols that exist and any additional characters
determined by what is there are inserted.
   If the symbol starts just after an open-parenthesis, only symbols
with function definitions are considered.  Otherwise, all symbols with
function definitions, values or properties are considered."
  (interactive)
  (let* ((end (point))
	 (beg (save-excursion
		(condition-case nil
		    (backward-sexp 1)
		  (error nil))
		(while (= (char-syntax (following-char)) ?\')
		  (forward-char 1))
		(point)))
	 (pattern (downcase (buffer-substring beg end)))
	 (predicate-char
	  (save-excursion
	    (goto-char beg)
	    (preceding-char)))
	 (predicate-char-syntax (char-syntax predicate-char))
	 (predicate
	  (function (lambda (sym)
		      (if (null (elt sym 4))
			  (not (= predicate-char ?\"))
			(= predicate-char-syntax ?\")))))
	 (completion (try-completion pattern rexx-user-procedure-table predicate)))
    (cond ((eq completion t)
	   (rexx-do-auto-upper t)
	   (while (get-buffer-window " *Completions*")
	     (delete-window (get-buffer-window " *Completions*"))))
	  ((null completion)
	   (message "Can't find completion for \"%s\"" pattern)
	   (ding))
	  ((not (string= pattern completion))
	   (delete-region beg end)
	   (insert completion)
	   (rexx-do-auto-upper t)
	   (while (get-buffer-window " *Completions*")
	     (delete-window (get-buffer-window " *Completions*"))))
	  (t
	   (message "Making completion list...")
	   (let ((list (all-completions pattern rexx-user-procedure-table predicate)))
	     (with-output-to-temp-buffer " *Completions*"
	       (display-completion-list list)))
	   (message "Making completion list...%s" "done")))))


(defun rexx-function-at-point()
  (if (not (or (= (char-syntax (following-char)) ?w)
	       (= (char-syntax (preceding-char)) ?w)))
      nil
    (save-excursion
      (let* ((beg (progn
		    (if (= (char-syntax (preceding-char)) ?w)
			(backward-sexp 1))
		    (while (= (char-syntax (following-char)) ?\')
		      (forward-char 1))
		    (point)))
	     (end (progn (forward-sexp 1) (point)))
	     (pattern (downcase (buffer-substring beg end)))
	     (precap (assoc pattern rexx-user-procedure-table)))
	(if precap
	    (if (elt precap 1)
		(elt precap 1)
	      (car precap)))))))
  
(defun rexx-function-help (function)
  "Display the full documentation of FUNCTION (a symbol)."
  (interactive
   (let ((fn (rexx-function-at-point))
	 (enable-recursive-minibuffers t)	     
	 val)
     (setq val
	   (completing-read
	    (if fn
		(format "Describe function (default %s): " fn)
	      "Describe function: ")
	    rexx-user-procedure-table nil t))
     (list (if (equal val "")
	       fn val))))
  (with-output-to-temp-buffer "*Help*"
    (princ function)
    (princ ": ")
    (let* ((var (assoc (downcase function) rexx-user-procedure-table))
	   (doc (elt var 3))
	   (type (elt var 2)))
      (cond ((assoc (downcase function) rexx-command-table)
	     (if type
		 (princ (format "an internal %s\n" type))
	       (princ "an internal command or function\n")))
	    (type
	     (if rexx-developing-mode-docs
		 (progn
		   (setq var (assoc (downcase function) rexx-external-function-table))
		   (setq doc (elt var 3))
		   (setq type (elt var 2))))
	     (cond ((not (= (char-syntax (string-to-char type)) ?w))
		    (princ (substring type 1))
		    (princ "\n"))
		   ((string-match " " type)
		    (princ type)
		    (princ "\n"))
		   (t
		    (princ (format "an external function from the %s package" type)))))
	    (t
	     (princ "an external command or function\n")))
      (princ "\n")
      (if doc
	  (princ doc)
	(princ "not documented")))))

(defun rexx-complete-external (desc)
  "Reads the name of an external function name from the minibuffer
with completion."
  (let ((enable-recursive-minibuffers t)	     
	(val
	 (completing-read desc rexx-external-function-table nil nil)))
    (rexx-capitalize-string val)))

(defun rexx-capitalize-string (str)
  "Capitalize string based on rexx-external-function-auto-capitilize."
  (if rexx-external-function-auto-capitilize
      (let ((ass (assoc (downcase str) rexx-user-procedure-table)))
	(if (elt ass 1)
	    (elt ass 1)
	  (capitalize str)))))

(defun rexx-clear-procedure-table ()
  "Clears the local procedure table."
  (interactive)
  (setq rexx-user-procedure-table rexx-command-and-function-table))

(defun rexx-build-procedure-table ()
  "Builds the local procedure table."
  (interactive)
  (setq rexx-user-procedure-table nil)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[a-z][a-z_0-9]*:" nil t)
      (rexx-add-to-procedure-table
       (buffer-substring (match-beginning 0) (1- (match-end 0))))))
  (eval rexx-build-eval)
  (setq rexx-user-procedure-table (append rexx-user-procedure-table rexx-command-and-function-table)))

(defun rexx-add-to-procedure-table (name)
  "Check the function table for the function name.  If it is not
there yet, add it."
  (if (assoc (downcase name) rexx-user-procedure-table)
      ()
    (setq rexx-user-procedure-table (cons (list (downcase name) name "User procedure") rexx-user-procedure-table))))

; 
; default compile
;


(add-hook 'regina-mode-hook
	  (lambda ()
	    (set (make-local-variable 'compile-command)
		 (concat "regina " buffer-file-name))))

;;; rexx-mode.el ends here



; Commented out for now.

;; (defun rexx-mode ()
;;   "Major mode for editing REXX code.
;; Comments are delimited with /* ... */.
;; Delete converts tabs to spaces as it moves back.
;; Line labels MUST start with '_' and procedure labels MUST not.

;; \\[rexx-indent-command]	indents for REXX code.
;; \\[rexx-complete-symbol]	attempts completion on a partial command or function name.
;; \\[electric-rexx-do]	inserts DO
;; \\[electric-rexx-end]	insert END.
;; \\[rexx-function-help]	displays help for REXX commands and functions.

;; Additional colons after a procedure name will insert PROCEDURE, then
;; EXPOSE and finally the contents of rexx-expose-string (this defaults
;; to \"(Globals)\".)

;; \\{rexx-mode-map}

;; Variables controlling automatic capitalization and completion:
;;  rexx-command-auto-upper
;;     If this is t, rexx-mode will automatically convert all REXX command and
;;     internal function names to upper case.  If it is 1, it will capitalize.
;;     If 2 or higher, it will capitalize functions and uppercase commands.
;;  rexx-external-function-auto-capitilize
;;     If non-nil, rexx-mode will automagically fix capitalization for any
;;     external functions it knows about.
;;  rexx-auto-build-procedure-table
;;     If non-nil, rexx-mode will automatically build a local table of
;;     procedures defined in the current buffer.  These are then added
;;     to the completion table for this buffer.

;; Variables controlling indentation style:
;;  rexx-tab-always-indent
;;     Non-nil means TAB in REXX mode should always reindent the current line,
;;     regardless of where in the line point is when the TAB command is used.
;;  rexx-indent-level
;;     Indentation of REXX statements within surrounding block.
;;     The surrounding block's indentation is the indentation
;;     of the line on which the open-brace appears.
;;  rexx-procedure-indent-level
;;     Indentation of REXX statements after a procedure label.
;;  rexx-procedure-arg-indent-level
;;     Indentation of ARG or PARSE ARG immediately after a procedure label.
;;  rexx-return-indent
;;     Indentation for return.
;;  rexx-do-offset
;;     Extra indentation for line if it starts with a DO.
;;  rexx-end-offset
;;     Indentation for END relative to DO.
;;  rexx-when-offset
;;     Indentation for WHEN relative to SELECT.
;;  rexx-continued-statement-offset
;;     Extra indentation given to a substatement, such as the
;;     then-clause of an if or body of a while.
;;  rexx-expose-string
;;     Specifies what string to electrically put after PROCEDURE EXPOSE.

;; Settings for Maxwell, Cowlishaw and Wide indentation styles are:
;;   rexx-indent-level                3    3    4
;;   rexx-procedure-indent-level      3    0    4
;;   rexx-procedure-arg-indent-level  0	0    2
;;   rexx-return-indent	           0    0    0
;;   rexx-do-offset                  -3   -3   -2
;;   rexx-end-offset		   0    3    0
;;   rexx-when-offset		   1	3    2
;;   rexx-continued-statement-offset  1    2    2
;; Use set-rexx-style to choose.

;; Turning on REXX mode calls the value of the variable rexx-mode-hook with no
;; args, if that value is non-nil."
;;   (interactive)
;;   (if (and (bobp) (eobp))
;;       (progn
;; 	(insert "/* */")
;; 	(set-buffer-modified-p nil)))
;;   (save-excursion
;;     (goto-char (point-min))
;;     (if (looking-at "/\\*")
;; 	(progn
;; 	  (kill-all-local-variables)
;; 	  (use-local-map rexx-mode-map)
;; 	  (setq major-mode 'rexx-mode)
;; 	  (setq mode-name "Rexx")
;; 	  (setq local-abbrev-table rexx-mode-abbrev-table)
;; 	  (set-syntax-table rexx-mode-syntax-table)
;; 	  (make-local-variable 'rexx-expose-string)
;; 	  (local-set-key "\C-h\C-f" 'rexx-function-help)
;; 	  (make-local-variable 'paragraph-start)
;; 	  (setq paragraph-start (concat "^$\\|" page-delimiter))
;; 	  (make-local-variable 'paragraph-separate)
;; 	  (setq paragraph-separate paragraph-start)
;; 	  (make-local-variable 'paragraph-ignore-fill-prefix)
;; 	  (setq paragraph-ignore-fill-prefix t)
;; 	  (make-local-variable 'indent-line-function)
;; 	  (setq indent-line-function 'rexx-indent-line)
;; 	  (make-local-variable 'indent-region-function)
;; 	  (setq indent-region-function 'rexx-indent-region)
;; 	  (make-local-variable 'require-final-newline)
;; 	  (setq require-final-newline t)
;; 	  (make-local-variable 'comment-start)
;; 	  (setq comment-start "/* ")
;; 	  (make-local-variable 'comment-end)
;; 	  (setq comment-end " */")
;; 	  (make-local-variable 'comment-column)
;; 	  (setq comment-column 32)
;; 	  (make-local-variable 'comment-start-skip)
;; 	  (setq comment-start-skip "/\\*+ *")
;; 	  (make-local-variable 'comment-indent-function)
;; 	  (setq comment-indent-function 'rexx-comment-indent)
;; 	  (make-local-variable 'parse-sexp-ignore-comments)
;; 	  (setq parse-sexp-ignore-comments t)
;; 	  (make-local-variable 'rexx-build-eval)
;; 	  (run-hooks 'rexx-mode-hook)
;; 	  (if rexx-auto-build-procedure-table
;; 	      (rexx-build-procedure-table)
;; 	    (rexx-clear-procedure-table)))
;;       (fundamental-mode))))

