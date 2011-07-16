(require 'org-redmine)

;; setup
(require 'org-redmine-test-fixture)

(defun change-buffer-to (mode)
  (if (version< "23.2" emacs-version)
      (setq default-major-mode mode)
    (setq major-mode mode))
  (set-buffer-major-mode (current-buffer)))

(expectations
  (desc "orutil-gethash")
  (expect 3
    (orutil-gethash hash-json "a"))
  (expect (type hash-table)
    (orutil-gethash hash-json "b"))
  (expect "12"
    (orutil-gethash hash-json "b" "c"))
  (expect "31"
    (orutil-gethash hash-json "b" "d" "e"))
  (expect nil
    (orutil-gethash hash-json "b" "a"))
  (expect nil
    (orutil-gethash hash-json "a" "c"))

  (desc "orutil-join")
  (expect "a,b,c"
    (orutil-join '("a" "b" "c")))
  (expect "a-b-c"
    (orutil-join '("a" "b" "c") "-"))
  (expect "3%2%1"
    (orutil-join '(3 "2" 1) "%"))

  (desc "org-redmine-template-%-to-attrkey")
  (expect '("id")
    (org-redmine-template-%-to-attrkey "%i%"))
  (expect '("status" "name")
    (org-redmine-template-%-to-attrkey "%s_n%"))
  (expect '("status" "id")
    (org-redmine-template-%-to-attrkey "%s_i%"))

  (desc "org-redmine-issue-attrvalue")
  (expect "新規"
    (org-redmine-issue-attrvalue fixture-issue '("status" "name")))
  (expect "1"
    (org-redmine-issue-attrvalue fixture-issue '("id")))

  (desc "org-redmine-insert-header")
  (expect "* #1 軌跡検知 :機能:"
    (with-current-buffer (exps-tmpbuf)
      (change-buffer-to 'org-mode)
      (org-redmine-insert-header fixture-issue 1)
      (buffer-string)))

  (desc "org-redmine-insert-header change template-header")
  (expect "* [肉体言語 Tython] #1 by Wataru MIYAGUNI"
    (with-current-buffer (exps-tmpbuf)
      (let ((org-redmine-template-header "[%p_n%] #%i% by %as_n%"))
        (change-buffer-to 'org-mode)
        (org-redmine-insert-header fixture-issue 1)
        (buffer-string))))

  (desc "org-redmine-insert-property")
  (expect "\
* hoge
  :PROPERTIES:
  :project_name: 肉体言語 Tython
  :author:   Wataru MIYAGUNI
  :END:
"
    (with-current-buffer (exps-tmpbuf)
      (let ((org-redmine-template-property '(("project_name" . "%p_n%")
                                             ("author"       . "%au_n%"))))
        (change-buffer-to 'org-mode)
        (insert "* hoge\n")
        (org-redmine-insert-property fixture-issue)
        (buffer-string))))

  (expect "\
* hoge
  :PROPERTIES:
  :subject:  軌跡検知
  :END:
"
    (with-current-buffer (exps-tmpbuf)
      (let ((org-redmine-template-property '(("subject" . "%s%"))))
        (change-buffer-to 'org-mode)
        (insert "* hoge\n")
        (org-redmine-insert-property fixture-issue)
        (buffer-string))))

  (desc "org-redmine-insert-property template-property is empty")
  (expect "\
* hoge
"
    (with-current-buffer (exps-tmpbuf)
      (change-buffer-to 'org-mode)
      (insert "* hoge\n")
      (org-redmine-insert-property fixture-issue)
      (buffer-string)))

  (desc "org-redmine-get-issue to blank buffer")
  (expect "\
* #1 軌跡検知 :機能:
  :PROPERTIES:
  :project_name: 肉体言語 Tython
  :author:   Wataru MIYAGUNI
  :END:
"
    (stub org-redmine-curl-get => fixture-issue-json)
    (stub read-from-minibuffer => "1")
    (with-current-buffer (exps-tmpbuf)
      (let ((org-redmine-template-header "#%i% %s% :%t_n%:")
            (org-redmine-template-property '(("project_name" . "%p_n%")
                                             ("author"       . "%au_n%"))))
        (change-buffer-to 'org-mode)
        (org-redmine-get-issue)
        (buffer-string))))

  (desc "org-redmine-get-issue to end of subtree")
  (expect "\
* hoge
** fuga
** #1 軌跡検知 :機能:
   :PROPERTIES:
   :author:   Wataru MIYAGUNI
   :END:
"
    (stub org-redmine-curl-get => fixture-issue-json)
    (stub read-from-minibuffer => "1")
    (with-current-buffer (exps-tmpbuf)
      (let ((org-redmine-template-header "#%i% %s% :%t_n%:")
            (org-redmine-template-property '(("author" . "%au_n%"))))
        (change-buffer-to 'org-mode)
        (insert "* hoge\n")
        (insert "** fuga\n")
        (org-redmine-get-issue)
        (buffer-string))))

  (desc "org-redmine-get-issue to between subtree")
  (expect "\
* hoge
** fuga
** 肉体言語 Tython / [1] 新規 :バージョン 0.3:
   :PROPERTIES:
   :subject:  軌跡検知
   :END:
** hago
"
    (stub org-redmine-curl-get => fixture-issue-json)
    (stub read-from-minibuffer => "1")
    (with-current-buffer (exps-tmpbuf)
      (let ((org-redmine-template-header "%p_n% / [%i%] %s_n% :%c_n%:")
            (org-redmine-template-property '(("subject" . "%s%"))))
        (change-buffer-to 'org-mode)
        (insert "* hoge\n")
        (insert "** fuga\n")
        (insert "** hago\n")
        (outline-previous-visible-heading 2)
        (org-redmine-get-issue)
        (buffer-string))))

  (desc "org-redmine-issue-uri")
  (expect "http://localhost/issues/1"
    (let ((org-redmine-uri "http://localhost"))
      (org-redmine-issue-uri fixture-issue)))

  (desc "org-redmine-transformer-issues-source")
  (expect '(("#3 [肉体言語 Tython] サマーソルトキックを認識 / Wataru MIYAGUNI"
             . "http://localhost/issues/3")
            ("#2 [Gongo Kinect Diet] 走る / 未割り当て"
             . "http://localhost/issues/2")
            ("#1 [肉体言語 Tython] 軌跡検知 / Wataru MIYAGUNI"
             . "http://localhost/issues/1"))
    (let ((org-redmine-uri "http://localhost"))
      (mapcar 
       (lambda (i)
         (cons (car i) (org-redmine-issue-uri (cdr i))))
       (org-redmine-transformer-issues-source fixture-issue-all))))

  (desc "org-redmine-config-get-limit")
  (expect 25
    (let ((org-redmine-limit 25))
          (org-redmine-config-get-limit)))
  (expect 1
    (let ((org-redmine-limit 1))
      (org-redmine-config-get-limit)))
  (expect 100
    (let ((org-redmine-limit 100))
      (org-redmine-config-get-limit)))

  (desc "org-redmine-config-get-limit : with arg")
  (expect (type string)
    (org-redmine-config-get-limit t))

  (desc "org-redmine-config-get-limit : out of range (1-100)")
  (expect 25
    (let ((org-redmine-limit 0))
      (org-redmine-config-get-limit)))
  (expect 25
    (let ((org-redmine-limit 101))
      (org-redmine-config-get-limit)))

  (desc "org-redmine-config-get-limit : not integer")
  (expect 25
    (let ((org-redmine-limit "a"))
      (org-redmine-config-get-limit)))
  (expect 25
    (let ((org-redmine-limit '()))
      (org-redmine-config-get-limit)))
)
