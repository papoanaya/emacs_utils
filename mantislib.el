;;; mantis.el -- Provide connectivity to Mantis SOAP service

;; Copyright (C) 2009  Alex Harsanyi <AlexHarsanyi@gmail.com>
;; Copyright (C) 2012  Luis R. Anaya <papoanaya@hotmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Luis R. Anaya <papoanaya@hotmail.com>
;; Created: June, 2012
;; Keywords: soap, web-services, mantis
;; Homepage: http://github.com/papoanaya/emacs_utils/

;; This file provides a programatic interface to MANTIS.  It provides access to
;; MANTIS from other programs, but no user level functionality.


(require 'soap-client)

(defgroup mantis nil
  "Access MANTIS from emacs."
  :group 'tools)

(defcustom mantis-wsdl-descriptor-url 
  "http://mantis/api/soap/mantisconnect.php?wsdl"
  "The location for the WSDL descriptor for the JIRA service.
This is specific to your local JIRA installation.  The URL is
tipically:

  http://YOUR_INSTALLATION/rpc/soap/jirasoapservice-v2?wsdl

The default value works if JIRA is located at a hostname named
'jira'."
  :type 'string
  :group 'mantis)

(defcustom mantis-host-url
  "http://mantis"
  "The address of the jira host."
  :type 'string
  :group 'mantis)


; for testing


(defvar mantis-token nil
  "Mantis token used for authentication")

(defvar mantis-user-login-name nil
  "The name of the user logged into JIRA.
This is maintained by `mantis-login'.")

(defvar mantis-user-login-password nil
  "password to be kept for the session"
)
(defvar mantis-wsdl nil)

(defun mantis-load-wsdl ()
  "Load the MANTIS WSDL descriptor."
  (setq mantis-wsdl (soap-load-wsdl-from-url mantis-wsdl-descriptor-url)))

(defun mantis-login (username password)
  "Login into MANTIS and store the authentication token in `mantis-token'"
  ;; NOTE that we cannot rely on `mantis-call' because `mantis-call' relies on
  ;; us ;-)
  (interactive
   (list (read-string 
          (format "Mantis Username [%s]: " user-login-name) nil nil user-login-name)
	 (read-passwd "Mantis Password: ")))

  (unless mantis-wsdl 
    (mantis-load-wsdl))

  ;; Mantis requires to have the username and password sent in every transaction.
  ;; (setq mantis-token 
  ;;       (car (soap-invoke mantis-wsdl "jirasoapservice-v2" "login" username password)))

  (setq mantis-user-login-name username)
  (setq mantis-user-login-password password)

  ;; At this point, soap-invoke didn't raise an error, so the login
  ;; credentials are OK.  use them to log into the web interface as
  ;; well, as this will be used to link issues (an operation which is
  ;; not exposed to the SOAP interface.  
  ;;
  ;; Note that we don't validate the response at all -- not sure how we
  ;; would do it...

  (let ((url (format "%s" mantis-host-url )))

    (let ((url-request-method "POST")
          (url-package-name "Emacs mantis.el")
          (url-package-version "1.0")
          (url-mime-charset-string "utf-8;q=1, iso-8859-1;q=0.5")
          (url-request-data "abc")
          (url-request-coding-system 'utf-8)
          (url-http-attempt-keepalives t))

      (let ((buffer (url-retrieve-synchronously url)))
        ;; This is just a basic check that the page was retrieved
        ;; correctly.  No error does not indicate a succesfull login,
        ;; we would have to parse the HTML page to find that out...
        (with-current-buffer buffer
          (declare (special url-http-response-status))
          (if (> url-http-response-status 299)
              (error "Error logging into MANTIS Web interface %s" 
                     url-http-response-status)))
        (kill-buffer buffer)))))

(defun mantis-call (method &rest params)
  "Invoke the MANTIS METHOD with supplied PARAMS.
This should be used for all JIRA inteface calls, as the method
ensures the user is logged in and invokes `soap-invoke' with the
correct service name and authentication token.

All JIRA inteface methods take an authentication token as the
first argument.  The authentication token is supplied by this
function, so PARAMS should omit this parameter. For example, the
\"getIssue\" method takes two parameters: auth and key, however,
when invoking it through `mantis-call', the call shoulbe be:

  (mantis-call \"getIssue\" KEY)
"
  (unless (and mantis-user-login-name mantis-user-login-password)
    (call-interactively 'mantis-login))


  (condition-case data
      (apply 'soap-invoke mantis-wsdl "MantisConnectPort"
            method params)
    (soap-error
     ;; If we are here, we had a token, but it expired.  Re-login and try
     ;; again.
     (setq mantis-user-login-name nil)
     (setq mantis-user-login-password nil)

     (call-interactively 'mantis-login)

     (apply 'soap-invoke mantis-wsdl "MantisConnectPort" method params
            ))))


;;;; Some utility functions

(defun mantis-make-assoc-list (data key-field value-field)
  "Create an association list from a SOAP structure array.

DATA is a list of association lists (a SOAP array-of type)
KEY-FIELD is the field to use as the key in the returned alist
VALUE-FIELD is the field to use as the value in the returned alist"
  (loop for element in data
     collect (cons (cdr (assoc key-field element))
		   (cdr (assoc value-field element)))))

(defun mantis-make-remote-field-values (fields)
  "Transform a (KEY . VALUE) list into a RemoteFieldValue structure.

Each (KEY . VALUE) pair is transformed into 
 ((id . KEY) (values . (VALUE)))

This method exists because Several JIRA methods require a
RemoteFieldValue list, but it is easier to work with ALISTS in
emacs-lisp"
  (let ((remote-field-values))

    ;; we accept an ALIST of field-name field-values parameter, but we need to
    ;; construct a structure that encodes as a RemoteFieldValue which is what
    ;; updateIssue wants
    (dolist (field fields)
      (let ((name (car field))
            (value (cdr field)))
        (when (symbolp name)
          (setq name (symbol-name name)))
        ;; Value must be an "array" (for which soap-client accepts lists) even
        ;; if it is just one value
        (unless (vectorp value)
          (setq value (vector value)))
        (push `((id . ,name) (values . ,value)) 
              remote-field-values)))
    
    (apply 'vector (nreverse remote-field-values))))

;;;; Wrappers around MANTIS  methods

(defun mantis-get-version ()
  (car (apply 'soap-invoke 
              mantis-wsdl "MantisConnectPort" (list "mc_version")) )
)



(defun mantis-get-projects (username password)
; Returns cons list. Nth 0 = id, nth 1 = name
  (apply 'soap-invoke mantis-wsdl  "MantisConnectPort" 
       (list "mc_projects_get_user_accessible" username password) )
 )


(defun mantis-get-enum-status (username password)
 (mantis-call "mc_enum_status" username password))



(defun mantis-get-issue (username password issue-id)
  (mantis-call "mc_issue_get" username password issue-id)
)

;; comments are related to issues.

(defun mantis-get-comments (username password issue-id)
; 1. Need to parse the results and get the notes.
; 2. Get the notes from the resulting list
; 3. Return the resultant array. 
  (let ((result-list nil))
    (setq issue  
          (mantis-call "mc_issue_get" username password issue-id) )
    (dolist (issue-item (car issue))
;      (message "issue-item: %s \n" issue-item)
      (if (eq (car issue-item) 'notes) 
          (setq result-list issue-item)
        )
      
      )
     result-list)
)

(defun mantis-add-comment (username password issue-id comment)
  (setq comment-data
        (list '(id) 
              '(reporter) 
              (cons 'text comment) 
              '(view_state)
              '(date_submitted)
              '(last_modified)
              '(time_tracking)
              '(note_type)
              '(note_attr)) )

  (car (mantis-call "mc_issue_note_add" username password issue-id comment-data))
   
)


(defun mantis-update-comment (username password issue-id note-id comment)
  (setq comment-data
        (list (cons 'id note-id) 
              '(reporter) 
              (cons 'text comment)
              '(view_state)
              '(date_submitted)
              '(last_modified)
              '(time_tracking)
              '(note_type)
              '(note_attr)) )

  (mantis-call "mc_issue_note_update" username password issue-id comment-data)
)


(defun mantis-update-issue (username password issue-id fields)
  (setq issue-data
	(list 
	 (cons 'id issue-id)
	 '(project)
	 '(category)
	 '(priority)
	 '(severity)
	 '(status)
	 '(summary)
	 '(version)
	 '(build)
	 '(platform)
	 '(os)
	 '(os_build)
	 '(description)
	 '(additional_information)
	 )
	)
  (mantis-call "mc_issue_update" username password issue-id fields)
  )

(defun mantis-create-issue (username password summary-data 
				     description-data category-data project-name)
  (setq issue-data
	(list 
	 (cons 'summary summary-data )
	 (cons 'description description-data)
	 (cons 'category category-data)
	 (list 'project 
	       (cons 'id  (mantis-get-project-id username password project-name ) )
	       (cons 'name  project-name) )
  
	 ))
  (car (mantis-call "mc_issue_add" username password issue-data))
  
  )

(defvar mantis-status-codes-cache nil)

(defun mantis-get-statuses (username password)
  "Return an assoc list mapping a status code to its name.
NOTE: Status codes are stored as strings, not numbers.

This function will only ask JIRA for the list of codes once, than
will cache it."
  (unless mantis-status-codes-cache
    (setq mantis-status-codes-cache
	  (mantis-make-assoc-list (car (mantis-call "mc_enum_project_status" username password)) 'id 'name)))
  mantis-status-codes-cache)

(defvar mantis-issue-types-cache nil)

(defun mantis-get-issue-types (username password)
  "Return an assoc list mapping an issue type code to its name.
NOTE: Issue type codes are stored as strings, not numbers.

This function will only ask JIRA for the list of codes once, than
will cache it."
  (unless mantis-issue-types-cache
    (setq mantis-issue-types-cache
	  (mantis-make-assoc-list (car (mantis-call "mc_enum_status" username password)) 'id 'name)))
  mantis-issue-types-cache)

(defvar mantis-priority-codes-cache nil)

(defun mantis-get-project-id (username password project-name)
  (car  (mantis-call "mc_project_get_id_from_name" username password project-name) )

)


;; Need to add an optional for the page and per page.

(defun mantis-get-project-issues (username password project-id page)
  (mantis-call "mc_project_get_issue_headers" username password project-id page 100)
)


(defun mantis-get-priorities (username password)
  "Return an assoc list mapping a priority code to its name.
NOTE: Priority codes are stored as strings, not numbers.

This function will only ask JIRA for the list of codes once, than
will cache it."
  (unless mantis-priority-codes-cache
    (setq mantis-priority-codes-cache
	  (mantis-make-assoc-list (car (mantis-call "mc_enum_priorities" username password)) 'id 'name)))
  mantis-priority-codes-cache)

(defvar mantis-resolution-code-cache nil)

(defun mantis-get-resolutions (username password)
  "Return an assoc list mapping a resolution code to its name.
NOTE: Resolution codes are stored as strings, not numbers.

This function will only ask JIRA for the list of codes once, than
will cache it."
  (unless mantis-resolution-code-cache
    (setq mantis-resolution-code-cache
	  (mantis-make-assoc-list (car (mantis-call "mc_enum_resolutions" username password)) 'id 'name)))
  mantis-resolution-code-cache)

(defvar mantis-issue-regexp nil)

;; NOTE: it is not such a good ideea to use this, as it needs a JIRA
;; connection to construct the regexp (the user might be prompted for a JIRA
;; username and password).
;;
;; The best use of this function is to generate the regexp once-off and
;; persist it somewhere.

(defun mantis-get-issue-regexp ()
  "Return a regexp that matches an issue name.
The regexp is constructed from the project keys in the JIRA
database.  An issue is assumed to be in the format KEY-NUMBER,
where KEY is a project key and NUMBER is the issue number."
  (unless mantis-issue-regexp
    (let ((projects (mapcar (lambda (e) (downcase (cdr (assoc 'key e))))
                            (car (mantis-call "getProjectsNoSchemes")))))
      (setq mantis-issue-regexp (concat "\\<" (regexp-opt projects) "-[0-9]+\\>"))))
  mantis-issue-regexp)

(defun mantis-do-jql-search (jql &optional limit)
  "Run a JQL query and return the list of issues that matched.
LIMIT is the maximum number of queries to return.  Note that JIRA
has an internal limit of how many queries to return, as such, it
might not be possible to find *ALL* the issues that match a
query." 
  (unless (or limit (numberp limit))
    (setq limit 100))
  (car (mantis-call "getIssuesFromJqlSearch" jql limit)))

(defun mantis-get-available-actions (issue-key)
  "Return the available workflow actions for ISSUE-KEY.
This runs the getAvailableActions SOAP method."
  (mantis-make-assoc-list 
   (car (mantis-call "getAvailableActions" issue-key))
   'id 'name))

(defun mantis-get-fields-for-action (issue-key action-id)
  "Return the required fields for the ACTION-ID."
  (mantis-make-assoc-list
   (car (mantis-call "getFieldsForAction" issue-key action-id))
   'id 'name))

(defun mantis-progress-workflow-action (issue-key action-id params)
  (car (mantis-call "progressWorkflowAction" issue-key action-id params)))

(defun mantis-add-worklog-and-autoadjust-remaining-estimate (issue-key start-date time-spent comment)
  "Log time spent on ISSUE-KEY to its worklog.
The time worked begings at START-DATE and has a TIME-SPENT
duration. JIRA will automatically update the remaining estimate
by subtracting TIME-SPENT from it.

START-DATE should be in the format 2010-02-05T14:30:00Z 

TIME-SPENT can be in one of the following formats: 10m, 120m
hours; 10h, 120h days; 10d, 120d weeks."
  (car (mantis-call "addWorklogAndAutoAdjustRemainingEstimate"
                   issue-key
                   `((startDate . ,start-date)
                     (timeSpent . ,time-spent)
                     (comment   . ,comment)))))

(defun mantis-link-issue (issue-key link-type other-issue-key)
  "Create a link between ISSUE-KEY and OTHER-ISSUE-KEY.
LINK-TYPE is a string representing the type of the link, e.g
\"requires\", \"depends on\", etc.  I believe each JIRA
installation can define its own link types."
  
  ;; IMPLEMENTATION NOTES: The linking jira issues functionality is
  ;; not exposed through the SOAP api, we must use the web interface
  ;; to do the linking.  Unfortunately, we cannot parse the result, so
  ;; we don't know that the linking was succesfull or not.  To reduce
  ;; the risk, we use the SOAP api to retrieve the issues for
  ;; ISSUE-KEY and OTHER-ISSUE-KEY.  This will ensure that we are
  ;; logged in (see also mantis-login) and that both issues exist. We
  ;; don't validate the LINK-TYPE, not sure how to do it.
  ;;

  (let ((issue (mantis-get-issue issue-key))
        (other-issue (mantis-get-issue other-issue-key)))
    (let ((url (concat mantis-host-url 
                       "/secure/LinkExistingIssue.jspa?"
                       (format "linkDesc=%s&linkKey=%s&id=%s&Link=Link" 
                               link-type other-issue-key (cdr (assq 'id issue))))))
      (let ((url-request-method "POST")
            (url-package-name "Emacs scratch.el")
            (url-package-version "1.0")
            (url-mime-charset-string "utf-8;q=1, iso-8859-1;q=0.5")
            (url-request-data "abc")
            (url-request-coding-system 'utf-8)
            (url-http-attempt-keepalives t)
            ;; see http://confluence.atlassian.com/display/JIRA/Form+Token+Handling
            (url-request-extra-headers '(("X-Atlassian-Token" . "no-check"))))

       (let ((buffer (url-retrieve-synchronously url)))
        ;; This is just a basic check that the page was retrieved
         ;; correctly.  No error does not indicate a success as we
         ;; have to parse the HTML page to find that out...
         (with-current-buffer buffer
           (declare (special url-http-response-status))
           (if (> url-http-response-status 299)
               (error "Error linking issue through JIRA Web interface %s" 
                      url-http-response-status)))
           (kill-buffer buffer))))))


;................................................ issue field accessors ....

(defun mantis-issue-key (issue)
  "Return the key of ISSUE."
  (cdr (assoc 'key issue)))

(defun mantis-issue-owner (issue)
  "Return the owner of ISSUE."
  (cdr (assq 'assignee issue)))

(defun mantis-issue-status (issue)
  "Return the status of ISSUE as a status string (not as a number!)"
  (let ((status-code (cdr (assq 'status issue))))
    (cdr (assoc status-code (mantis-get-statuses)))))

(defun mantis-custom-field-value (custom-field issue)
  "Return the value of CUSTOM-FIELD for ISSUE.
Return nil if the field is not found"
  (catch 'found
    (dolist (field (cdr (assq 'customFieldValues issue)))
      (when (equal (cdr (assq 'customfieldId field)) custom-field)
        (throw 'found (cadr (assq 'values field)))))))
  
(provide 'mantis)
