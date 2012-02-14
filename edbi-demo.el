(require edbi)

;; start
(setq conn1 (edbi:start))

;; connect
(edbi:connect conn1 '("dbi:SQLite:dbname=./test.sqlite" "" ""))

;; select
(ctbl:popup-table-buffer-easy
 (edbi:sync edbi:select-all-d conn1 "select * from test"))

;; prepare - execute - fetch
(progn
  (edbi:sync edbi:prepare-d conn1 "select * from test")
  (edbi:sync edbi:execute-d conn1 nil)
  (ctbl:popup-table-buffer-easy
   (edbi:sync edbi:fetch-d conn1)
   (edbi:sync edbi:fetch-columns-d conn1)))

;; sequence notation
(lexical-let (rows header (conn1 conn1))
  (edbi:seq
   (edbi:prepare-d conn1 "select * from test")
   (edbi:execute-d conn1 nil)
   (rows <- (edbi:fetch-d conn1))
   (header <- (edbi:fetch-columns-d conn1))
   (lambda (x)
     (ctbl:popup-table-buffer-easy rows header))))

;; insert
(edbi:sync edbi:do-d conn1
   "insert into test (name,comment) values ('aaaa','bbbbbbb')")

(edbi:sync edbi:prepare-d conn1
   "insert into test (name,comment) values ('aaaa','bbbbbbb')")
(edbi:sync edbi:execute-d conn1 nil)


;; delete
(edbi:sync edbi:do-d conn1
   "delete from test where name = 'aaaa'")

;; transaction
(lexical-let (a b (conn1 conn1))
  (edbi:seq
   (edbi:auto-commit-d conn1 "false")
   (edbi:do-d conn1 "insert into test (name,comment) values ('aaaa','bbbbbbb')")
   (a <- (edbi:liftd caar (edbi:select-all-d conn1 "select count(id) from test")))
   (edbi:rollback-d conn1)
   (b <- (edbi:liftd caar (edbi:select-all-d conn1 "select count(id) from test")))
   (edbi:auto-commit-d conn1 "true")
   (lambda (x) (message "result %S %S" a b))))

;; table info
(lexical-let (results (conn1 conn1))
  (edbi:seq
   (results <- (edbi:table-info-d conn1 nil nil nil nil))
   (lambda (x) 
     (ctbl:popup-table-buffer-easy (cadr results) (car results)))))

;; column info
(lexical-let (results (conn1 conn1))
  (edbi:seq
   (results <- (edbi:column-info-d conn1 nil nil nil nil))
   (lambda (x) 
     (ctbl:popup-table-buffer-easy (cadr results) (car results)))))

;; primary key info

(lexical-let (results (conn1 conn1))
  (edbi:seq
   (results <- (edbi:primary-key-info-d conn1 nil nil "participants"))
   (lambda (x) 
     (ctbl:popup-table-buffer-easy (cadr results) (car results)))))

;; foreign key info
(lexical-let (results (conn1 conn1))
  (edbi:seq
   (results <- (edbi:foreign-key-info-d conn1 nil nil nil nil nil nil))
   (lambda (x)
     (when results
       (ctbl:popup-table-buffer-easy (cadr results) (car results))))))

;; status
(edbi:sync edbi:status-info-d conn1)

;; disconnect
(edbi:finish conn1)

