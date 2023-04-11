org-redmine
==============================

**This repository has been archived because org-redmine is no longer under development.**

[![Build Status](https://travis-ci.org/gongo/org-redmine.svg?branch=master)](https://travis-ci.org/gongo/org-redmine)

Overview
--------------------

![](images/org-redmine.gif)

Install
--------------------

### Using git

1. You can clone the git repository

    ```
    $ git clone git://github.com/gongo/org-redmine.git
    ```

2. Then add this to your ~/.emacs.el

    ```lisp
    (add-to-list 'load-path "/path/to/org-redmine/")
    (require 'org-redmine)
    ```

### Using auto-install

```lisp
;; Eval this
(auto-install-from-url "https://raw.github.com/gongo/org-redmine/master/org-redmine.el")

;; Or run
;; M-x auto-install-from-url RET https://raw.github.com/gongo/org-redmine/master/org-redmine.el
```

Setup
--------------------

### URL (Required)

```lisp
;; Target Redmine URI
;;   eg. Redmine Project
(setq org-redmine-uri "http://www.redmine.org")
;;   eg. Ruby Project
(setq org-redmine-uri "http://redmine.ruby-lang.org")
```

### Authentication (Optional)

Presented in order of highest priority setting.

1. REST API Key

    ```lisp
    (setq org-redmine-auth-api-key "xxxxxxxxxxxxxxxxxxxx") ;; nil default
    ```

2. username/password

    ```lisp
    (setq org-redmine-auth-username "gongo")
    (setq org-redmine-auth-password "secret")
    ```

3. use `$HOME/.netrc`

    ```lisp
    ;; if t, read $HOME/.netrc
    (setq org-redmine-auth-netrc-use t) ;; nil default
    ```

### Template Sequences

| %-sequence | mean               |
|------------|--------------------|
| `%as_i%`   | assigned_to id     |
| `%as_n%`   | assigned_to name   |
| `%au_i%`   | author id          |
| `%au_n%`   | author name        |
| `%c_i%`    | category id        |
| `%c_n%`    | category name      |
| `%c_date%` | created_on         |
| `%d%`      | description        |
| `%done%`   | done_ratio         |
| `%d_date%` | due_date           |
| `%i%`      | issue id           |
| `%pr_i%`   | priority id        |
| `%pr_n%`   | priority name      |
| `%p_i%`    | project id         |
| `%p_n%`    | project name       |
| `%s_date%` | start_date         |
| `%s_i%`    | status id          |
| `%s_n%`    | status name        |
| `%s%`      | subject            |
| `%t_i%`    | tracker id         |
| `%t_n%`    | tracker name       |
| `%u_date%` | updated_on         |
| `%v_n%`    | fixed_version name |
| `%v_i%`    | fixed_version id   |

### Template of insert subtree

```lisp
;; default template
;; (defvar org-redmine-template-header "#%i% %s% :%t_n%:")
;; (defvar org-redmine-template-property nil)

;; * [#333] Subject :Tag:

(setq org-redmine-template-header "[%p_n%] #%i% %s% by %as_n%")
(setq org-redmine-template-property
      '(("assigned_to" . "%as_n%")
        ("version" . "%v_n%")))

;; * [ProjectName] #333 Subject by gongo
;;   :PROPERTIES:
;;   :assigned_to:  dududu
;;   :version: 1.2
;;   :END:

(setq org-redmine-template-header "[#%i%] %s%")
(setq org-redmine-template-property
      '(("project_name" . "%p_n%")))

;; * [#333] Subject
;;   :PROPERTIES:
;;   :project_name:  ProjectName
;;   :END:
```

See org-redmine.el for other % sequence list

LICENSE
--------------------

MIT
