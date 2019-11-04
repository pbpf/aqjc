#lang racket/base

(require db/base
         db/postgresql
         db/sqlite3
         racket/class
         racket/match
         racket/date
         "user.rkt"
         "tools.rkt"
         
        ; "odbc.rkt"
        ; "function/tools.rkt"
         )

(provide login-context%
         current-login-context
         init-database!
         create-sqlite3
         get-sqlite3
         init-sqlite3-table!
         insert-sqlite3!
         backup-postgresql-to-sqlite3
         backup-postgresql-to-sqlite3-all
         restore-sqlite3-to-postgresql
         get-backup-info
         good-backup-file?)
;(define cnn (odbc-driver-connect "DRIVER={SQL Server Native Client 11.0};SERVER=localhost;UID=sa;PWD=12345;database=testdb"))
(define-main-config database-name "aqjc")
(define login-context%
  (class object%
    (field [cnn #f]
           [current-user #f]
           [group #f]
           [owner #f])
    (super-new)
    (define/public(login server user password)
      (define mcnn
        (with-handlers([exn:fail? (lambda(e) #f)])
                      (postgresql-connect #:user user
                            #:server server
                            #:database database-name
                            #:password password)))
      (cond
        [mcnn (set! cnn mcnn)
              (set! current-user user)
              (cond
                [(is-database-owner cnn  user)(set! group 3)(set! owner #t)]
                [else(set! group (query-value cnn "select igroup from aqjcuserinfo where name=$1" current-user))])
              #t]
        [else #f]))
    (define/public(logout)
      (set! cnn #f))
    (define/public(get-cnn)
      cnn)
    (define/public(get-user)
      current-user)
    (define/public(get-group)
      group)
    (define/public(is-owner)
      owner)
    ))
(define current-login-context (new login-context%))
(define(drop-table cnn name)
  (when (table-exists? cnn name)
    (query-exec cnn (format "DROP TABLE ~a" name))))
(define(drop-view cnn name)
  (when (table-exists? cnn name)
    (query-exec cnn (format "DROP VIEW ~a" name))))



(define(init-table-plus! cnn)
  (drop-view cnn "aqjcuserinfo")
  (drop-table cnn "aqjcusers")
;  (drop-table cnn "aqjcfiles_metadata")
  (drop-table cnn "aqjcfiles")
  (drop-table cnn "aqjcmzbg")
  (drop-table cnn "storage")
  (drop-role cnn "aqjcpublic")
  (drop-role cnn "aqjcuser")
  (drop-role cnn "aqjcsuper")
 ; (query-exec cnn "create table storage(id serial primary key,md5 char(32) NOT NULL,sha1 char(40) NOT NULL,size integer NOT NULL,info bytea,unique(md5,sha1))")
  ;(query-exec cnn "create table aqjcfiles(id interger primary key,subindex integer NOT NULL,sortindex integer NOT NULL,filename varchar(60) NOT NULL,date date NOT NULL,metadata bytea)")
  (query-exec cnn "create table aqjcfiles(id serial primary key,
                                          subindex integer NOT NULL,
                                          sortindex integer NOT NULL,
                                          filename text NOT NULL,
                                          filesize integer NOT NULL,
                                          md5hash text NOT NULL,
                                          date date NOT NULL,
                                          info bytea,
                                          metadata bytea,
                                          unique(filename,subindex,md5hash,filesize),
                                          unique(subindex,sortindex))")
  (query-exec cnn "CREATE OR REPLACE FUNCTION addfile(section integer,name text,filemd5 text,inf bytea,metadata bytea) RETURNS integer AS
                   $$
                   DECLARE
                   users_rec RECORD;
                   size integer;
                   o_id integer;
                   BEGIN
                     size:=octet_length(inf);
                     SELECT into users_rec * FROM  aqjcfiles where subindex=section and filename=name;
                    IF FOUND THEN
                      RETURN -1;
                    ELSE
                      update aqjcfiles set sortindex=sortindex+1 where subindex=section;
                      INSERT INTO aqjcfiles(subindex,sortindex,filename,filesize,md5hash,date,info,metadata) values(section,0,name,size,filemd5,'now',inf,metadata) RETURNING id into o_id;
                      RETURN o_id;
                    END IF;
                   END;
                   $$
                   LANGUAGE plpgsql;")

(query-exec cnn "CREATE OR REPLACE FUNCTION swapindex(section integer,id1 integer,id2 integer) returns integer AS
                   $$
                   DECLARE
                   index1 integer;
                   index2 integer;
                   BEGIN
                     SELECT sortindex into index1 from aqjcfiles where id=id1 and subindex=section;
                    IF FOUND THEN
                      SELECT sortindex into index2 from aqjcfiles where id=id2 and subindex=section;
                      IF FOUND THEN
                       update aqjcfiles set sortindex=-1 where id=id1;
                       update aqjcfiles set sortindex=index1 where id=id2;
                       update aqjcfiles set sortindex=index2 where id=id1;
                       RETURN 0;
                      ELSE
                       RETURN id2;
                      END IF;
                    ELSE
                     RETURN id1;
                   END IF;
                  END;
                  $$
                  LANGUAGE plpgsql;")
                   
 

  (query-exec cnn "CREATE OR REPLACE FUNCTION restore(section integer,name text,editdate date,filemd5 text,size integer,inf bytea,metadata bytea) RETURNS integer AS
                   $$
                   DECLARE
                   users_rec RECORD;
                   o_id integer;
                   BEGIN
                     SELECT INTO users_rec * FROM  aqjcfiles where subindex=section and filesize=size and filename=name and md5hash=filemd5;
                    IF FOUND THEN
                      RETURN -1;
                    ELSE
                      update aqjcfiles set sortindex=sortindex+1 where subindex=section;
                      insert into aqjcfiles(subindex,sortindex,filename,filesize,md5hash,date,info,metadata) values(section,0,name,size,filemd5,editdate,inf,metadata) RETURNING id INTO o_id;
                      RETURN o_id;
                    END IF;
                   END;
                   $$
                   LANGUAGE plpgsql;")
                     

              
  ;(query-exec cnn "create table aqjcfiles_metadata(metadata bytea)INHERITS(aqjcfiles)")
  (query-exec cnn "create table aqjcmzbg(id int primary key,errortype int,date date)")
  (query-exec cnn "create table aqjcusers(name varchar(60) primary key,pass text,igroup int,info text)")
  (query-exec cnn "CREATE VIEW aqjcuserinfo AS SELECT a.name, a.igroup ,a.info FROM aqjcusers AS a")
  (create-group cnn "aqjcpublic")
  (create-group cnn "aqjcuser")
  (create-group cnn "aqjcsuper")
  
  (give-all cnn "aqjcfiles_id_seq" "aqjcsuper")
  (give-all cnn "aqjcfiles" "aqjcsuper")
  (give-all cnn "aqjcmzbg" "aqjcsuper")
  (give-all cnn "aqjcusers" "aqjcsuper")
   (give-all cnn "aqjcuserinfo" "aqjcsuper")
  
  (give-select-update-insert cnn "aqjcfiles_id_seq" "aqjcuser")
  (give-select-update-insert cnn "aqjcfiles" "aqjcuser")
  (give-select-update-insert cnn "aqjcmzbg" "aqjcuser")
  (give-select cnn "aqjcuserinfo" "aqjcuser")
 
  (give-select cnn "aqjcfiles" "aqjcpublic")
  (give-select cnn "aqjcmzbg" "aqjcpublic")
  (give-select cnn "aqjcuserinfo" "aqjcpublic")
  ;-----------------------------------
  ;(give-select cnn "files" "aqjcpublic")
  ;(give-select cnn "files_editable" "aqjcpublic")
 )
; (define cnn (postgresql-connect #:user "connor" #:password "12345" #:database "test"))
(define(database-exists? cnn name)
  (not (null?(query-list cnn "select datname from pg_database where datname=$1" name))))

(define(init-database! cnn server user password)
  (when (database-exists? cnn database-name)
      (query-exec cnn (format "DROP DATABASE ~a" database-name)))
  (query-exec cnn (format "CREATE DATABASE aqjc WITH OWNER = ~a" user))
  (define cnn2 (postgresql-connect #:user user
                            #:server server
                            #:database database-name
                            #:password password))
  (init-table-plus! cnn2)
  #t
  )

(define (postgres-type-to-sqlite-type t)
  (case t
    [("integer")"integer"]
    [("real" "double")"real"]
    [("varchar" "char" "text" "date" "timestamp")"text"]
    [("bytea") "blob"]
    [else (error 'postgres-type-to-sql-type "type ~a can not convert to sqlite3" t)]))
(define(sql-date->text d)
  (format "~a-~a-~a" (sql-date-year d)(sql-date-month d)(sql-date-day d)))
(define(text->sql-date t)
  (match t
    [(pregexp  #px"^([0-9]{4})-([0-9]{1,2})-([0-9]{1,2})$" (list _ a b c)) (apply sql-date(map string->number(list a b c)))]
    [else #f]))

(define(create-sqlite3 file)
  (sqlite3-connect 	#:database  file #:mode 'create))
(define(get-sqlite3 file)
  (sqlite3-connect 	#:database  file #:mode 'read-only))
(define(init-sqlite3-table! cnn);v0.1.1
  #|
id serial primary key,
                                          subindex integer NOT NULL,
                                          sortindex integer NOT NULL,
                                          filename varchar(60) NOT NULL,
                                          filesize integer NOT NULL,
                                          md5hash text NOT NULL,
                                          date date NOT NULL,
                                          info bytea,
                                          metadata bytea,
|#
  (query-exec cnn "create table aqjcfiles(subindex integer,filesize integer,filename text,md5hash text,date text,info blob,metadata blob)")
  (query-exec cnn "create table info(name text primary key,val text)")
  (insert-info cnn "version" "0.1.1")
  (insert-info cnn "date" (parameterize([date-display-format 'iso-8601])
                                                                   (date->string (current-date))))
  )
(define(insert-info cnn n v)
  (query-exec cnn "insert or replace into info(name,val) values($1,$2)" n v))

(define(good-backup-file? b)
  (define cnn (get-sqlite3 b) )
  (define x (and(table-exists? cnn "info")
    (for/and([i (in-list (list "date" "version" "type"))])
                  (= 1 (query-value cnn "select count(*) from info where name=$1" i)))))
  (disconnect cnn)
  x)
(define(get-backup-info backupf)
  (define cnn (get-sqlite3 backupf))
 (define x(for/list([i (in-list (list "date" "version" "type"))])
                  (query-value cnn "select val from info where name=$1" i)))
  (disconnect cnn)
  x)
                

(define(insert-sqlite3! cnn a1 a2 a3 a4 a5 a6 a7)
  (query-exec cnn "insert into aqjcfiles(subindex,filename,filesize,md5hash,date,info,metadata)values($1,$2,$3,$4,$5,$6,$7)"
              a1 a2 a3 a4 (sql-date->text a5) a6 a7))
(define(backup-postgresql-to-sqlite3 pcnn scnn id)
  (define a (query-row pcnn "select subindex,filename,filesize,md5hash,date,info,metadata from aqjcfiles where id=$1" id))
  (apply insert-sqlite3! scnn (vector->list a)))
(define(backup-postgresql-to-sqlite3-part pcnn scnn idlist)
  (call-with-transaction	  scnn
  (lambda()(for([id (in-list idlist)])
    (backup-postgresql-to-sqlite3 pcnn scnn id))))
  (insert-info scnn "type" "part"))
(define(backup-postgresql-to-sqlite3-all pcnn scnn)
  (call-with-transaction	  scnn
  (lambda()(for([(a1 a2 a3 a4 a5 a6 a7)(in-query pcnn "select subindex,filename,filesize,md5hash,date,info,metadata from aqjcfiles")])
    (insert-sqlite3! scnn  a1 a2 a3 a4 a5 a6 a7)
    )))
    (insert-info scnn "type" "full"))
(define(restore-sqlite3-to-postgresql scnn pcnn)
  (for([(a1 a2 a3 a4 a5 a6 a7)(in-query scnn "select subindex,filename,date,md5hash,filesize,info,metadata from aqjcfiles")])
    ;(section integer,name varchar(60),editdate date,filemd5 text,size integer,inf bytea,metadata bytea)
    (query-value pcnn "select restore($1,$2,$3,$4,$5,$6,$7)"
                  a1 a2 (text->sql-date a3) a4 a5 a6 a7)))
#|
(module+ test
(define pcnn(postgresql-connect #:user "postgres"
                            #:server "localhost"
                            #:database "aqjc"
                            #:password "sa"))
;(backup-postgresql-to-sqlite3-all pcnn a)
 (define a (create-sqlite3  "D:\\text2.dbf"))
(init-sqlite3-table! a))
|#
;(init-database! cnn "127.0.0.1" "postgres" "sa")
;(define cnn (postgresql-connect #:user "postgres" #:password "sa" #:database "posgres"))
;(query-exec cnn "select addfile($1,$2,$3,$4,$5)" 1 "a" "aa" #"aa" #"ddd")