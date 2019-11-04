#lang racket/base
(require db/base)
(provide (all-defined-out))
;user management
(define(user-right cnn user)
  (query-value cnn "SELECT * FROM pg_roles where rolname=$1" user))
(define(current-user cnn)
  (query-value cnn "select current_user from current_user"))
(define(user-super? cnn user)
  (query-value cnn "SELECT rolsuper FROM pg_roles where rolname=$1" user))

(define(user-cancreateuser? cnn user)
  (query-value cnn "SELECT rolcreaterole FROM pg_roles where rolname=$1" user))

(define(create-user cnn name pass)
  (query-exec cnn (format "CREATE role ~a  with login password '~a' inherit" name pass)))

(define(create-super-user cnn name pass)
  (query-exec cnn "CREATE role $1 login SUPERUSER password=$2" name pass))

(define(create-rolec-user cnn name pass)
  (query-exec cnn "CREATE role $1  login CREATEROLE password=$2" name pass))

(define(is-database-owner cnn user)
  (query-value cnn "select rolname=$1 from pg_roles where oid=(select datdba from pg_database where datname=(select current_database()))" user))

(define(give-select cnn table user)
  (query-exec cnn (format "GRANT SELECT ON ~a TO GROUP ~a" table user)))
(define(give-select-update cnn table user)
  (query-exec cnn (format "GRANT SELECT,UPDATE ON ~a TO GROUP ~a" table user)))
(define(give-select-update-insert cnn table user)
  (query-exec cnn (format "GRANT SELECT,UPDATE,INSERT ON ~a TO GROUP ~a" table user)))
(define(give-all cnn table user)
  (query-exec cnn (format "GRANT ALL PRIVILEGES ON ~a TO GROUP ~a" table user)))
(define(role-exists? cnn name)
  (not(null?(query-rows cnn "SELECT rolname FROM pg_roles where rolname=$1" name))))
(define(drop-role cnn name)
  (when (role-exists? cnn name)
    (query-exec cnn (format "DROP role ~a" name))))
(define(create-group cnn name)
  ;must filter username
  (query-exec cnn (format "CREATE ROLE ~a NOSUPERUSER NOCREATEDB NOCREATEROLE REPLICATION" name)))

  ;VALID UNTIL '2016-08-18 00:20:00'
(define(move-to-group cnn name group)
  ;filter name and group
  (query-exec cnn (format "GRANT ~a TO ~a"   group name)))

(define(list-group-members cnn group)
  (query-list cnn "select rolname from pg_roles where oid in (select member from pg_auth_members where roleid=(select oid from pg_roles where rolname=$1))" group))
(define(list-ones-groups cnn my)
  (query-list cnn "select rolname from pg_roles where oid in (select roleid from pg_auth_members where member=(select oid from pg_roles where rolname=$1))" my))

(define(remove-from-group cnn name group)
  (query-exec cnn (format "REVOKE ~a FROM ~a"   group name)))

(define(user-pass cnn name)
  (query-value cnn "select rolpassword from pg_authid where rolname=$1" name))

