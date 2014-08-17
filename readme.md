# Emacs DBI

This program connects the database server through Perl's DBI,
and provides DB-accessing API and the simple management UI.

## Installation:

### Package installation

If you use package.el with [Marmalade](http://marmalade-repo.org/) or [MELPA](http://melpa.milkbox.net/), you just select the package 'edbi' and install it.

Next, you install the perl module `RPC::EPC::Service', DBI and Database drivers with CPAN. 

Example:
```
$ cpan RPC::EPC::Service DBI DBD::SQLite DBD::Pg DBD::mysql
```

Here, SQLite, Postgresql and MySQL drivers will be installed, in addition to the EPC module.
Of course, you can choose the drivers for your environment.

### Manual installation

This program depends on following programs:

- deferred.el, concurrent.el / https://github.com/kiwanami/emacs-deferred
- epc.el      / https://github.com/kiwanami/emacs-epc
- ctable.el   / https://github.com/kiwanami/emacs-ctable
- Perl/CPAN
 - RPC::EPC::Service (and some dependent modules)
 - DBI and drivers, DBD::Sqlite, DBD::Pg, DBD::mysql

Place this program (edbi.el and edbi-bridge.pl) in your load path
and add following code.

```lisp
(require 'edbi)
```

### Helper application

One can install some helper applications:

- [edbi-django](https://github.com/proofit404/edbi-django)
-- This program connects django project database quickly.
-- (You can find more edbi projects at the author's repo, https://github.com/proofit404/edbi-sqlite3, https://github.com/proofit404/edbi-database-url)

## Usage:

M-x `edbi:open-db-viewer' opens a dialog for DB connection.

![DB connection](https://cacoo.com/diagrams/VdRPw8hjXiezJJud-2B7F2.png)

- Data Source : URI string for DBI::connect (Ex. dbi:SQLite:dbname=/path/db.sqlite )
- User Name, Auth : user name and password for DBI::connect
- History button : you can choose a data source from your connection history.
- OK button : connect DB and open the database view

### Database view

This buffer enumerates tables and views.

![DB Tables](https://cacoo.com/diagrams/VdRPw8hjXiezJJud-30BC3.png)

Check the key-bind `edbi:dbview-keymap'.

- j,k, n,p : navigation for rows
- c        : switch to query editor buffer
- RET      : show table data
- SPC      : show table definition
- q        : quit and disconnect

### Table definition view

This buffer shows the table definition information.

![Table Definition](https://cacoo.com/diagrams/VdRPw8hjXiezJJud-B5E39.png)

Check the key-bind `edbi:dbview-table-keymap'.
- j,k, n,p : navigation for rows
- c        : switch to query editor buffer
- V        : show table data
- q        : kill buffer

### Query editor

You can edit SQL in this buffer, which supports SQL syntax
highlight and auto completion by auto-complete.el.

![SQL Editor](https://cacoo.com/diagrams/VdRPw8hjXiezJJud-30392.png)

Check the key-bind `edbi:sql-mode-map'.

- C-c C-c  : Execute SQL
- C-c q    : kill buffer
- M-p      : SQL history back
- M-n      : SQL history forward

### Query result viewer

You can browser the results for executed SQL.

![Query Results](https://cacoo.com/diagrams/VdRPw8hjXiezJJud-E9A0C.png)

Check the key-bind `edbi:dbview-query-result-keymap'.
- j,k, n,p : navigation for rows
- SPC      : display the whole data at the current cell. (hitting the SPC again, it clears the popup.)
- q        : kill buffer

## Navigation summary

![Navigation Summary](https://cacoo.com/diagrams/VdRPw8hjXiezJJud-8D899.png)


## E2WM perspective

![E2WM perspective](https://cacoo.com/diagrams/VdRPw8hjXiezJJud-23532.png?width=450)

Here is an example setup to open EDBI perspective with Super-d.

```cl
(autoload 'e2wm:dp-edbi "e2wm-edbi" nil t)
(global-set-key (kbd "s-d") 'e2wm:dp-edbi)
```

# Emacs DBI API

TODO...

----
(C) 2012,2013,2014 SAKURAI Masashi All rights reserved. m.sakurai at kiwanami.net
