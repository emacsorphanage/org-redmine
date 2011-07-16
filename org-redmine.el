;; Author: Wataru MIYAGUNI <gonngo@gmail.com>
;;
;; License: MAHALO License (based on MIT License)
;; 
;;   Copyright (c) 2011 Wataru MIYAGUNI
;;
;;   Permission is hereby granted, free of charge, to any person obtaining a copy
;;   of this software and associated documentation files (the "Software"), to deal
;;   in the Software without restriction, including without limitation the rights
;;   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;   copies of the Software, and to permit persons to whom the Software is
;;   furnished to do so, subject to the following conditions:
;;   
;;     1. The above copyright notice and this permission notice shall be included in
;;        all copies or substantial portions of the Software.
;;     2. Shall be grateful for something (including, but not limited this software).
;;   
;;   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;   THE SOFTWARE.

(eval-when-compile
  (require 'cl))
(require 'org)
(require 'json)
(require 'anything)

(defvar org-redmine-uri "http://redmine120.dev")
(defvar org-redmine-api-key nil)
(defvar org-redmine-curl-buffer "*Org redmine curl buffer*"
  "Buffer curl output")
(defvar org-redmine-template-header nil
  "")
(defvar org-redmine-template-property nil
  "")
(defvar org-redmine-template-set
  '(nil
    nil
    "%d"))

(setq org-redmine-template-header-default "#%i% %s% :%t_n%:")
(setq org-redmine-template-%-sequences
      '(("%as_i%"    "assigned_to" "id")
        ("%as_n%"    "assigned_to" "name")
        ("%au_i%"    "author" "id")
        ("%au_n%"    "author" "name")
        ("%c_i%"     "category" "id")
        ("%c_n%"     "category" "name")
        ("%c_date%"  "created_on")
        ("%d%"       "description")
        ("%done%"    "done_ratio")
        ("%d_date%"  "due_date")
        ("%i%"       "id")
        ("%pr_i%"    "priority" "id")
        ("%pr_n%"    "priority" "name")
        ("%p_i%"     "project" "id")
        ("%p_n%"     "project" "name")
        ("%s_date%"  "stard_date")
        ("%s_i%"     "status" "id")
        ("%s_n%"     "status" "name")
        ("%s%"       "subject")
        ("%t_i%"     "tracker" "id")
        ("%t_n%"     "tracker" "name")
        ("%u_date%"  "updated_on")
        ("%v_n%"     "fixed_version" "name")
        ("%v_i%"     "fixed_version" "id")))


;;------------------------------
;; org-redmine utility functions
;;------------------------------
(defun orutil-join (list &optional sep func)
  (mapconcat (lambda (x) (if func (funcall func x) (format "%s" x))) list (or sep ",")))

(defun orutil-http-query (alist)
  (orutil-join alist "&"
                  (lambda (x) 
                    (format "%s=%s"
                            (url-hexify-string (car x))
                            (url-hexify-string (cdr x))))))

(defun orutil-gethash (table k &rest keys)
  "Execute `gethash' recursive to TABLE.

Example:
  hashtable = {
                \"a\" : 3 ,
                \"b\" : {
                          \"c\" : \"12\",
                          \"d\" : { \"e\" : \"31\" }
                      }
              } ;; => pseudo hash table like json format
  (orutil-gethash hashtable \"a\")
      ;; => 3
  (orutil-gethash hashtable \"b\")
      ;; => { \"c\" : \"12\", \"d\" : { \"e\" : \"31\" } }
  (orutil-gethash hashtable \"b\" \"c\")
      ;; => \"12\"
  (orutil-gethash hashtable \"b\" \"d\" \"e\")
      ;; => \"31\"
  (orutil-gethash hashtable \"b\" \"a\")
      ;; => nil
  (orutil-gethash hashtable \"a\" \"c\")
      ;; => nil
"
  (save-match-data
    (let ((ret (gethash k table)))
      (while (and keys ret)
        (if (hash-table-p ret)
            (progn
              (setq ret (gethash (car keys) ret))
              (setq keys (cdr keys)))
          (setq ret nil)))
      ret)))

;;------------------------------
;; org-redmine connection functions
;;------------------------------
(defun org-redmine-curl-get (uri)
  ""
  (condition-case err
      (progn
        (ignore-errors (kill-buffer org-redmine-curl-buffer))
        (call-process "curl" nil `(,org-redmine-curl-buffer nil) nil
                      "-X" "GET"
                      "-H" "Content-Type:application/json"
                      uri)
        (message uri)
        (save-current-buffer
          (set-buffer org-redmine-curl-buffer)
          (let ((json-object-type 'hash-table)
                (json-array-type 'list))
            (condition-case err
                (json-read-from-string (buffer-string))
              (json-readtable-error
               (message "%s: Non JSON data because of a server side exception. See %s" (error-message-string err) org-redmine-curl-buffer))))))
    (file-error (message (format "%s" (error-message-string err))))))

;;------------------------------
;; org-redmine template functions
;;------------------------------
(defun org-redmine-template-%-to-attrkey (sequence)
  "Transform %-sequence to issue attribute list (see `org-redmine-template-%-sequences').

Example.
  (setq org-redmine-template-%-sequences
        '((\"%as_i%\"  \"assigned_to\" \"id\")
          (\"%s%\"     \"subject\")
          (\"%au_n%\"  \"author\" \"name\")))

  (org-redmine-template-%-to-attrkey \"%as_i%\") ;; => '(\"assigned_to\" \"id\")
  (org-redmine-template-%-to-attrkey \"%s%\")    ;; => '(\"subject\")
"
  (cdr (assoc sequence org-redmine-template-%-sequences)))

;;------------------------------
;; org-redmine issue function
;;------------------------------
(defun org-redmine-issue-attrvalue (issue attrkey)
  "Get attribute value for ATTRKEY of ISSUE

Example:
  issue = {
           \"subject\" : \"Subject\",
           \"project\" : {
                    \"id\"   : 1,
                    \"name\" : \"PrijectName\"
                   }
          } ;; => pseudo issue like json format

  (org-redmine-issue-attrvalue issue '(\"subject\"))      ;; => \"Subject\"
  (org-redmine-issue-attrvalue issue '(\"project\" \"id\")) ;; => 1
"
  (format "%s" (apply 'orutil-gethash issue attrkey)))

(defun org-redmine-issue-attrvalue-from-% (issue seq)
  "Get attribute value of ISSUE using %-sequence SEQ

Example:
  issue = {
           \"subject\" : \"Subject\",
           \"project\" : {
                    \"id\"   : 1,
                    \"name\" : \"PrijectName\"
                   }
          } ;; => pseudo issue like json format

  (setq org-redmine-template-%-sequences
        '((\"%p_i%\"  \"project\" \"id\")
          (\"%p_i%\"  \"project\" \"name\")
          (\"%s%\"    \"subject\")))

  (org-redmine-issue-attrvalue issue \"%s%\"))   ;; => \"Subject\"
  (org-redmine-issue-attrvalue issue \"%p_i%\")) ;; => 1
"
  (org-redmine-issue-attrvalue issue (org-redmine-template-%-to-attrkey seq)))


(defun org-redmine-issue-uri (issue)
  "Return uri of ISSUE with `org-redmine-uri'.

Example.
  (setq org-redmine-uri \"http://redmine.org\")
  (org-redmine-issue-uri issue) ;; => \"http://redmine.org/issues/1\"

  (setq org-redmine-uri \"http://localhost/redmine\")
  (org-redmine-issue-uri issue) ;; => \"http://localhost/redmine/issues/1\""
  (format "%s/issues/%s" org-redmine-uri (orutil-gethash issue "id")))

;;------------------------------
;; org-redmine buffer function
;;------------------------------
(defun org-redmine-insert-header (issue level)
  ""
  (let ((template (or org-redmine-template-header
                      (nth 0 org-redmine-template-set)
                      org-redmine-template-header-default))
        (stars (make-string level ?*)))
    (insert
     (with-temp-buffer
       (erase-buffer)
       (insert (concat stars " "))
       (insert template)
       (goto-char (point-min))
       (while (re-search-forward "\\(%[a-z_]+%\\)" nil t)
         (let ((attr (org-redmine-template-%-to-attrkey (match-string 1))))
           (if attr (replace-match (org-redmine-issue-attrvalue issue attr) t t))))
       (buffer-string)))
  ))

(defun org-redmine-insert-property (issue)
  ""
  (let* ((properties (or org-redmine-template-property
                         (nth 1 org-redmine-template-set)
                         '()))
         property key value)
    (while properties
      (setq property (car properties))
      (org-set-property (car property)
                        (org-redmine-issue-attrvalue-from-% issue (cdr property)))
      (setq properties (cdr properties)))
  ))

(defun org-redmine-escaped-% ()
  "Check if % was escaped - if yes, unescape it now."
  (if (equal (char-before (match-beginning 0)) ?\\)
      (progn
	(delete-region (1- (match-beginning 0)) (match-beginning 0))
	t)
    nil))

(defun org-redmine-insert-subtree (issue)
  ""
  (let ((level (or (org-current-level) 1)))
    (outline-next-visible-heading 1)
    (org-redmine-insert-header issue level)
    (insert "\n")
    (outline-previous-visible-heading 1)
    (org-redmine-insert-property issue)
    ))

;;------------------------------
;; org-redmine sources for user function
;;------------------------------
(defun org-redmine-get-issue-all (me)
  "Return the recent issues.

if ME is t, return issues are assigned to user.
"
  (let* ((querylist (list (cons "key" (or org-redmine-api-key ""))))
         query issue-all)

    (if me (progn
             (add-to-list 'querylist (cons "assigned_to_id" "me"))
             (unless org-redmine-api-key
               (message "Warning: To use, required API Key"))))
    (setq query (orutil-http-query querylist))
    (setq issue-all (org-redmine-curl-get
                     (concat org-redmine-uri "/issues.json?" query)))
    (orutil-gethash issue-all "issues")))

(defun org-redmine-transformer-issues-source (issues)
  "Transform issues to `anything' source.

First, string that combined issue id, project name, subject, and member assinged to issue.
Second, issue (hash table).

Example.
  (setq issues '(issue1 issue2 issue3)) ;; => issue[1-3] is hash table
  (org-redmine-transformer-issues-source issues)
  ;; => '((issue1-string . issue1) (issue2-string . issue2) (issue3-string . issue3))
"
  (mapcar
   (lambda (i)
     (let (display-value action-value)
       (setq display-value (format "#%s [%s] %s / %s"
                                   (orutil-gethash i "id")
                                   (orutil-gethash i "project" "name")
                                   (orutil-gethash i "subject")
                                   (or (orutil-gethash i "assigned_to" "name")
                                       "未割り当て")))
       (setq action-value i)
       (cons display-value action-value)))
   issues))

;;------------------------------
;; org-redmine user function
;;------------------------------
(defun org-redmine-get-issue ()
  ""
  (interactive)
  (let* ((issue-id (read-from-minibuffer "Issue ID:"))
         (query (concat "key=" org-redmine-api-key))
         (issue (org-redmine-curl-get (format "%s/issues/%s.json?%s"
                                              org-redmine-uri issue-id query))))
    (org-redmine-insert-subtree (orutil-gethash issue "issue"))))

(defun org-redmine-anything-show-issue-all (&optional me)
  "Display recent issues using `anything'"
  (interactive "P")
  (anything 
   `(((name . "Issues")
      (candidates . ,(org-redmine-get-issue-all me))
      (candidate-transformer . org-redmine-transformer-issues-source)
      (volatile)
      (action . (("Open Browser"
                  . (lambda (issue) (browse-url (org-redmine-issue-uri issue))))
                 ("Insert Subtree"
                  . (lambda (issue) (org-redmine-insert-subtree issue)))))))
   ))

(provide 'org-redmine)
