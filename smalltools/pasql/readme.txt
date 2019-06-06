Pasql
=====
Pasql is a sample tool that lets you connect to an SQLDB supported database and run SQL commands.

It logs your commands and can print them at the end of the session.

To run this, you need to have the relevant database client drivers/dlls installed (e.g. in Windows, you can put them in the directory where pasql.exe is)

For embedded/file based databases: if the database does not exist, pasql creates a database.

You can enter your SQL statement, press enter, and GO and enter, and the statement will be executed or the query results opened.

It supports parameters, and uses : just like sqldb:
(Oracle)
select :hi from dual
or
(most other dbs)
select :hi
or
(Firebird/Interbase)
select :hi from rdb$database
etc.

Special commands:
SHOW TABLES - show list of all tables using sqldb. However, on MySQL it will pass on the command as is.

Run pasql -h for help.

Concepts
========
This program demonstrates:
- handling command line parameters
- Using parameters in queries
- use of a general class (TSQLConnection) for one of several specialized classes (TIBConnection, TMSSQLConnection,...) and use of the as keyword to call specialized code.
- a trick to find out whether a TSQLQuery returns records or not



Improvement ideas
=================
Support for more databases

Rewrite program so it is a bit more object oriented and variables are limited/scoped (the program originated as a procedural program)

Better format of returned records; scrolling support

Repeating queries with (or without) parameters

Pasql command mode next to the existing SQL mode: e.g. by typing ' at the beginning of a line.
File contents support for parameters (useful for blobs):
'insert into blobtable values (:blobparam)
then find some way to let the user indicate he wants to use a file

'
(single quote to exit command mode and forget parametrized query)
Command mode could also have output formatting commands like
'SET HEADER OFF
'SET HEADER ON
and metadata commands like:
'SHOW TABLES
'SHOW TABLE X =>can show e.g. columns, indexes, constraints
'SHOW PROCEDURES
... look into e.g. SQLQuery.SetSchemaInfo for that

Some way to insert blobs - e.g. using command mode above with the @file notation

Intercept keypresses (e.g. Alt-1, Alt-2) and show help/enter command mode



Reinier Olislagers
