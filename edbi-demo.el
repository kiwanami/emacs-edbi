(require edbi)

;; start
(setq conn1 (edbi:start))

;; connect
(edbi:connect conn1 '("dbi:SQLite:dbname=./test.sqlite" "" ""))

;; select
(ctbl:popup-table-buffer-easy
 (epc:sync conn1 (edbi:select-all-d conn1 "select * from test")))

;; prepare - execute - fetch
(progn
  (epc:sync conn1 (edbi:prepare-d conn1 "select * from test"))
  (epc:sync conn1 (edbi:execute-d conn1 nil))
  (ctbl:popup-table-buffer-easy
   (epc:sync conn1 (edbi:fetch-d conn1))
   (epc:sync conn1 (edbi:fetch-columns-d conn1))))

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
(epc:sync conn1 (edbi:do-d conn1
   "insert into test (name,comment) values ('aaaa','bbbbbbb')"))

;; delete
(epc:sync conn1 (edbi:do-d conn1
   "delete from test where name = 'aaaa'"))

;; transaction
(lexical-let (a b (conn1 conn1))
  (edbi:seq
   (edbi:auto-commit-d conn1 "false")
   (edbi:do-d conn1 "insert into test (name,comment) values ('aaaa','bbbbbbb')")
   (a <- (edbi:select-all-d conn1 "select count(id) from test"))
   (edbi:rollback-d conn1)
   (b <- (edbi:select-all-d conn1 "select count(id) from test"))
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
   (results <- (edbi:primary-key-info-d conn1 nil nil nil))
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
(epc:sync conn1 (edbi:status-info-d conn1))

;; disconnect
(epc:stop-epc conn1)

