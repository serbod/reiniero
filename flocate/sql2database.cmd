@echo off
rem database2sql:
rem Create SQL script with DDL statements used to recreate an empty Firebird
rem database. In other words, get rid of all data and start with a fresh db.
rem isql-fb flocate.fdb -o flocateddl.sql -x -u SYSDBA -p masterkey
rem sql2database:
rem Create Firebird database from SQL DDL script named flocate_database_schema.sql
rem don't forget to comment out this line in the script first:
rem CREATE DATABASE 'flocate.fdb' page_size 16384 user 'SYSDBA' password 'masterkey' default character set UTF8;
echo Cleaning up any existing database
del flocate.fdb.bak
ren flocate.fdb flocate.fdb.bak
isql -e -i flocate_database_schema.sql