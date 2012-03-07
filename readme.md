# Emacs DBI

This program connects the database server through Perl's DBI,
and provides DB-accessing API and the simple management UI.

## Installation:

This program depends on following programs:

- deferred.el, concurrent.el / https://github.com/kiwanami/emacs-deferred
- epc.el      / https://github.com/kiwanami/emacs-epc
- ctable.el   / https://github.com/kiwanami/emacs-ctable
- Perl/CPAN
 - RPC::EPC::Service (and some dependent modules)
 - DBI and drivers, DBD::Sqlite, DBD::Pg, DBD::mysql

Place this program (edbi.el and edbi-bridge.pl) in your load path
and add following code.

(require 'edbi)

## Usage:

M-x `edbi:open-db-viewer' opens a dialog for DB connection.

- Data Source : URI string for DBI::connect (Ex. dbi:SQLite:dbname=/path/db.sqlite )
- User Name, Auth : user name and password for DBI::connect
- History button : you can choose a data source from your connection history.
- OK button : connect DB and open the database view

### Database view

This buffer enumerates tables and views.
Check the key-bind `edbi:dbview-keymap'.

- j,k, n,p : navigation for rows
- c        : switch to query editor buffer
- RET      : show table data
- SPC      : show table definition
- q        : quit and disconnect

### Table definition view

This buffer shows the table definition information.
Check the key-bind `edbi:dbview-table-keymap'.
- j,k, n,p : navigation for rows
- c        : switch to query editor buffer
- V        : show table data
- q        : kill buffer

### Query editor

You can edit SQL in this buffer, which supports SQL syntax
highlight and auto completion by auto-complete.el.
Check the key-bind `edbi:sql-mode-map'.

- C-c C-c  : Execute SQL
- C-c q    : kill buffer
- M-p      : SQL history back
- M-n      : SQL history forward

### Query result viewer

You can browser the results for executed SQL.
Check the key-bind `edbi:dbview-query-result-keymap'.
- j,k, n,p : navigation for rows
- q        : kill buffer

----
(C) 2012 SAKURAI Masashi All rights reserved. m.sakurai at kiwanami.net
