;;; ob-eukleides.el --- org-babel functions for eukleides evaluation

;; Copyright (C) 2010-2012  Free Software Foundation, Inc.

;; Author: Luis Anaya
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating eukleides script.
;;
;; Inspired by Ian Yang's org-export-blocks-format-eukleides
;; http://www.emacswiki.org/emacs/org-export-blocks-format-eukleides.el

;;; Requirements:

;; eukleides     | http://eukleides.org
;; eukleides     | `org-eukleides-path' should point to the eukleides executablexs

;;; Code:
(require 'ob)
(require 'ob-eval)

;; (defvar org-babel-default-header-args:eukleides
;;   '((:results . "file") (:exports . "results"))
;;   "Default arguments for evaluating a eukleides source block.")


 (defvar org-babel-default-header-args:eukleides nil
   "Default arguments for evaluating a eukleides source block.")

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("eukleides" . "euk"))

(defcustom org-eukleides-path nil
  "Path to the eukleides executable file."
  :group 'org-babel
  :type 'string)


(defcustom org-eukleides-eps-to-raster nil
  "Command used to convert EPS to raster. Nil for no conversion."
  :group 'org-babel
  :type '(choice
         (repeat :tag "Shell Command Sequence" (string :tag "Shell Command"))
         (const :tag "sam2p" "a=%s;b=%s;sam2p ${a} ${b}" )
         (const :tag "NetPNM"  "a=%s;b=%s;pstopnm -stdout ${a} | pnmtopng  > ${b}" )
         (const :tag "None" nil)))


(defun org-babel-execute:eukleides (body params)
  "Execute a block of eukleides code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((result-params (split-string (or (cdr (assoc :results params)) "")))
	 (out-file (cdr (assoc :file params)))
	 (cmdline (cdr (assoc :cmdline params)))
     (session (cdr (assoc :session params)))
     (result-type (cdr (assoc :result-type params)))
     (full-body (org-babel-expand-body:generic
                 body params (org-babel-variable-assignments:eukleides
                              params)))
	 (in-file  (org-babel-temp-file "eukleides-" ))
     (session (org-babel-eukleides-initiate-session session))

	 (cmd (if (not org-eukleides-path)
		  (error "`org-eukleides-path' is not set")
          (if out-file
              (concat (expand-file-name org-eukleides-path)
                      " -b --output="
                      (org-babel-process-file-name
                       (concat
                        (file-name-sans-extension out-file) ".eps"))
                      " "
                      (org-babel-process-file-name in-file))

            (concat (expand-file-name org-eukleides-path)
                      " -b --output="
                      (org-babel-process-file-name
                       (concat
                              (file-name-sans-extension in-file) ".eps"))
                      " "
                      (org-babel-process-file-name in-file))))))

    (unless (file-exists-p org-eukleides-path)
      (error "Could not find eukleides at %s" org-eukleides-path))

    (cond (out-file
           (progn
             (if (string= (file-name-extension out-file) "png")
                 (if org-eukleides-eps-to-raster
                     (shell-command (format org-eukleides-eps-to-raster
                                            (concat (file-name-sans-extension out-file) ".eps")
                                            (concat (file-name-sans-extension out-file) ".png")))
                   (error "Conversion to PNG not supported. use a file with an EPS name")))
             (with-temp-file in-file (insert full-body))
             (message "%s" cmd) (org-babel-eval cmd "")
             nil))
           (t
            (progn
              (with-temp-file in-file (insert full-body))
              (message "%s" cmd)
              (org-babel-reassemble-table
               (org-babel-eukleides-evaluate session full-body cmd result-type)
               (org-babel-pick-name
                (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
               (org-babel-pick-name
                (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params)))))))))


;; signal that output has already been written to file

(defun org-babel-prep-session:eukleides (session params)
  "Return an error because eukleides does not support sessions."
  (error "Eukleides does not support sessions"))


(defun org-babel-variable-assignments:eukleides (params)
  "Return list of eukleides statements assigning the block's variables."
  (mapcar
   (lambda (pair)
     (format "%s = %s"
	     (car pair)
	     (org-babel-eukleides-var-to-eukleides (cdr pair))))
   (mapcar #'cdr (org-babel-get-header params :var))))

;; helper functions

(defun org-babel-eukleides-var-to-eukleides (var)
  "Convert an elisp value to an eukleides variable.
The elisp value, VAR, is converted to a string of eukleides source code
specifying a var of the same value."
  (if (listp var)
      (concat "cat("(mapconcat #'org-babel-eukleides-var-to-eukleides var ",") ")" )  
    (format "%s" var)))

(defun org-babel-eukleides-evaluate (session body cmd &optional result-type)
  "Pass BODY to the Eukleidesprocess in SESSION.
If RESULT-TYPE equals 'output then return a list of the outputs
of the statements in BODY, if RESULT-TYPE equals 'value then
return the value of the last statement in BODY, as elisp."
  (when session (error "Sessions are not supported for Eukleides."))


  (case result-type
    (output (org-babel-eval cmd ""))
    (value (org-babel-eval cmd ""))

    ;; (value (let ((tmp-file (org-babel-temp-file "eukleides-")))
    ;;          (org-babel-eval
    ;;           cmd
    ;;           (format org-babel-eukleides-wrapper-method body
    ;;                   (org-babel-process-file-name tmp-file 'noquote)))
    ;;          (org-babel-eval-read-file tmp-file)))

))

(defun org-babel-eukleides-initiate-session (&optional session params)
  "Return nil because sessions are not supported by eukleides."
nil)


(provide 'ob-eukleides)



;;; ob-eukleides.el ends here