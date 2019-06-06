Attribute VB_Name = "basExportImportSQL"
Option Compare Binary 'Required for name translation comparison
Option Explicit
' exportSQL, importSQL and auxilliary functions. Please see respective
' comments for details. You probably want to start with exportSQL.
' Reinier Olislagers: many additions. See changelog below.
' Comments/improvements/patches welcome at reinier<_removethis_>olislagers@g[thistoo]mail.com

' basExportImportSQL version 5.00
' (c) 2009-2011 Reinier Olislagers: Firebird, other database support. Import support.
' License equal to Cynergi license below
' but copyright of additions/changes rests with me, Reinier Olislagers.
' Incorporates exportSQL 3.2-dev by Dobrica Pavlinusic, exportSQL 2.0 by Pedro Freire/Cynergi
' and importSQL version 1.0 by Laurent Bossavit/NetDive, as well as original code for
' e.g. multiple database support, and refactoring of existing code.
'
' exportSQL version 3.2-dev
' www.rot13.org/~dpavlin/projects.html#sql
' (c) 2000-2001 Dobrica Pavlinusic <dpavlin@rot<remove this>13.org> - added PostgreSQL support
'
' exportSQL version 2.0
' www.cynergi.net/prod/exportsql/
'
' (C) 1997-98 CYNERGI - www.cynergi.net, info@cynergi.net
' (C) Pedro Freire - pedro.freire@cynergi.net  (do not add to mailing lists without permission)
'
' This code is provided free for anyone's use and is therefore without guarantee or support.
' This does NOT mean CYNERGI delegates its copyright to anyone using it! You may change the
' code in any way, as long as this notice remains on the code and CYNERGI is notified (if you
' publish the changes: if your changes/corrections prove valuable and are added to the code,
' you will be listed in a credit list on this file).
'
' You may NOT sell this as part of a non-free package:
' IF YOU HAVE PAID FOR THIS CODE, YOU HAVE BEEN ROBBED! CONTACT admin@cynergi.net!

' MODULE
'   Name: basExportImportSQL
'   Original module in Pedro Freire's code: "exportSQL"
'
' GOAL
'   Export all tables in a MS-Access database file to text files:
'   one with SQL instructions to optionally clear, then create tables
'   and insert data into the new tables. The table structure and data
'   will resemble as much as possible the current Access database.
'   A mapping file specifying names in the Access databae and the
'   destination database will also be supplied for reference.
'
' HOW TO USE
'   Copy-and-paste this text file into an Access module and run the
'   exportSQL function. In more detail, you:
'   * Open the Access .mdb file you wish to export
'   * in the default database objects window, click on "Modules", and then on "New"
'   * The code window that opens has some pre-written text (code). Delete it.
'   * Copy-and-paste this entire file to the code module window
'   * You may hit the compile button (looks like 3 sheets of paper with an arrow on
'     top of them, pressing down on them), or select Debug, Compile Loaded Modules
'     from the top menu, just to make sure there are no errors, and that this code
'     works on your Access version (it works on Access XP/2002 and should work on other versions)
'   * If you have a newer version of Access you need to set a reference to the Data Access
'     Objects library (could be called Microsoft DAO 3.6 Object Library)
'     (menu Extra/References). Otherwise compiling will fail with an error message.
'   * Close the code module window - windows will prompt you to save the code:
'     answer "Yes", and when promped for a name for the module, type anything
'     (say, "basExportImportSQL")
'   The module is now part of your Access database. To run the export, you:
'   * Re-open the code module (by double-clicking on it, or clicking "Design"
'     with it selected). Press F5 to run code, a window will pop up asking you
'     what code you want to run. Select the exportSQL "macro" and click Run/Execute.
'   * Alternatively, click on "Macros" on the database objects window,
'     and then on "New". On the macro window, select "RunCode" as the macro action,
'     and "exportSQL" as the function name, below. Save the macro similarly to the
'     module, and this time double-clicking on it, or clicking "Run" will run the export.
'
' BEFORE RUNNING THE EXPORT
'   Before running the export, be sure to check out the Export Options just below this
'   text, and change any according to your wishes and specs.
'   Note that you can export to various databases; the options are commented to help
'   you set the right ones for your target database.
'===============================================================================================================================================================
' Export/Import Options - change at will
' General:
Private Const DEF_DB_ENGINE As String = "MSSQL"
Public Const DES_DB_ENGINE As String = _
    "Target/output database, 1 of:" & vbCrLf & _
    "ACCESS    Microsoft Access script" & vbCrLf & _
    "CSV       Comma-separated values - tables only" & vbCrLf & _
    "DERBY     Apache Derby" & vbCrLf & _
    "DB2       IBM DB2" & vbCrLf & _
    "FIREBIRD  Firebird 2+" & vbCrLf & _
    "M1        mSQL v1" & vbCrLf & _
    "M2        mSQL v2" & vbCrLf & _
    "MSSQL     Microsoft SQL Server 2005 and higher" & vbCrLf & _
    "MY        MySQL" & vbCrLf & _
    "ORACLE    Oracle 8? and higher" & vbCrLf & _
    "PG        Postgresql" & vbCrLf & _
    "SQL2003   standard SQL" & vbCrLf & _
    "SQLITE    SQLite" & vbCrLf & _
    "SYBASE    Sybase ASE 12+" & vbCrLf
Private Const DEF_DB_NAME As String = ""
Public Const DES_DB_NAME As String = "Access source database to use." & vbCrLf & _
    "Use empty string for current. Else use filename or DSN name of database to export"
    
Private Const DEF_DB_CONNECT As String = "MS Access;"
Public Const DES_DB_CONNECT As String = _
    "Used only if above string is not empty" & vbCrLf & _
    "Default: MS Access;"

'-----------------------------------------------------------------------------------------------------------------------------------------------------------------
' What to export:
Private Const DEF_EXPORT_STRUCTURE As Boolean = True
Public Const DES_EXPORT_STRUCTURE As String = _
    "False if you don't want to export database structure/table definitions." & vbCrLf & _
    "True if you want to create a new database." & vbCrLf & _
    "Default: True"
    
Private Const DEF_EXPORT_DELETE_EXISTING_STRUCTURE As Boolean = True
Public Const DES_EXPORT_DELETE_EXISTING_STRUCTURE As String = _
    "True if you want to delete existing database structure before creating a new structure. Handy for recreating an existing database." & vbCrLf & _
    "Only valid if EXPORT_SCTRUCTURE=True." & vbCrLf & _
    "Default: False"
    
Private Const DEF_EXPORT_QUERIES As Boolean = True
Public Const DES_EXPORT_QUERIES As String = _
    "True if you want to try to export queries to views/stored procedures, even if we can only export the name and some comments." & vbCrLf & _
    "False if you don't want to export queries." & vbCrLf & _
    "Only valid if EXPORT_STRUCTURE=True." & vbCrLf & _
    "Default: False"
    
Private Const DEF_EXPORT_DATA As Boolean = True
Public Const DES_EXPORT_DATA As String = _
    "True if you want to export the data in the database." & vbCrLf & _
    "False if you don't want to export data (e.g. if you want to recreate an empty database)" & vbCrLf & _
    "Default: True"
    
Private Const DEF_FORMS_EXPORT As Boolean = False
Public Const DES_FORMS_EXPORT As String = _
    "True if you want to export forms (note: mostly structure/layout only, no code translation)" & vbCrLf & _
    "False if you don't want to export forms" & vbCrLf & _
    "Default: False"
 
'-----------------------------------------------------------------------------------------------------------------------------------------------------------------
' Naming:
Private Const DEF_PRESERVE_CASE As Boolean = False
Public Const DES_PRESERVE_CASE As String = _
    "Preserve the case of tables and fields. Will be overridden for" & vbCrLf & _
    "certain conversions; e.g. Access=>Access will always preserve case, and" & vbCrLf & _
    "Access=>Firebird will never preserve case due to extensive quoting required." & vbCrLf & _
    "Recommendation: leave at false." & vbCrLf & _
    "Default: False"
    
Private Const DEF_PREFIX_ON_KEYWORD As String = ""
Public Const DES_PREFIX_ON_KEYWORD As String = _
    "Prefix to put before identifier, if it is a reserved word" & vbCrLf & _
    "May be an empty string." & vbCrLf & _
    "Default: empty string "
    
Private Const DEF_SUFFIX_ON_KEYWORD As String = "_"
Public Const DES_SUFFIX_ON_KEYWORD As String = _
    "Suffix to add after identifier, if it is a reserved word" & vbCrLf & _
    "May be an empty string." & vbCrLf & _
    "Default: _"

Private Const DEF_PREFIX_ON_VIEW As String = ""
Public Const DES_PREFIX_ON_VIEW As String = _
    "Prefix to put before view names (every db except Access)" & vbCrLf & _
    "May be an empty string." & vbCrLf & _
    "Probably not necessary to set this as (query+table) names" & vbCrLf & _
    "in Access are unique, so generated view names will probably" & vbCrLf & _
    "be, too." & vbCrLf & _
    "Default: empty string"
    
Private Const DEF_SUFFIX_ON_VIEW As String = ""
Public Const DES_SUFFIX_ON_VIEW As String = _
    "Suffix to add after view names (every db except Access)" & vbCrLf & _
    "May be an empty string." & vbCrLf & _
    "Probably not necessary to set this as (query+table) names" & vbCrLf & _
    "in Access are unique, so generated view names will probably" & vbCrLf & _
    "be, too." & vbCrLf & _
    "Default: empty string"
    
Private Const DEF_PREFIX_ON_STOREDPROC As String = ""
Public Const DES_PREFIX_ON_STOREDPROC As String = _
    "Prefix to put before stored procedure names (every db except Access)" & vbCrLf & _
    "May be an empty string." & vbCrLf & _
    "Probably not necessary to set this as (query+table) names" & vbCrLf & _
    "in Access are unique, so generated procedure names will probably" & vbCrLf & _
    "be, too." & vbCrLf & _
    "Default: empty string"
    
Private Const DEF_SUFFIX_ON_STOREDPROC As String = ""
Public Const DES_SUFFIX_ON_STOREDPROC As String = _
    "Suffix to add after stored procedure (every db except Access)" & vbCrLf & _
    "May be an empty string." & vbCrLf & _
    "Probably not necessary to set this as (query+table) names" & vbCrLf & _
    "in Access are unique, so generated procedure names will probably" & vbCrLf & _
    "be, too." & vbCrLf & _
    "Default: empty string"

Private Const DEF_PREFIX_ON_INDEX As String = "ix"
Public Const DES_PREFIX_ON_INDEX As String = _
    "Prefix to put before index names (every db except Access)" & vbCrLf & _
    "May be an empty string." & vbCrLf & _
    "Default: ix"

Private Const DEF_SUFFIX_ON_INDEX As String = ""
Public Const DES_SUFFIX_ON_INDEX As String = _
    "Suffix to add after index names (every db except Access)" & vbCrLf & _
    "May be an empty string." & vbCrLf & _
    "Default: empty string"

Private Const DEF_PREFIX_ON_CONSTRAINT As String = "cs"
Public Const DES_PREFIX_ON_CONSTRAINT As String = _
    "Prefix to add before primary/foreign key/check constraint names" & vbCrLf & _
    "May be an empty string." & vbCrLf & _
    "Default: cs"

Private Const DEF_SUFFIX_ON_CONSTRAINT As String = ""
Public Const DES_SUFFIX_ON_CONSTRAINT As String = _
    "Suffix to add after primary/foreign key/check constraints" & vbCrLf & _
    "May be an empty string." & vbCrLf & _
    "Default: empty string"

Private Const DEF_PREFIX_ON_SEQUENCE As String = "sq"
Public Const DES_PREFIX_ON_SEQUENCE As String = _
    "Prefix to add before sequence/generator names." & vbCrLf & _
    "Not all databases support or require sequences, so you may never see a sequence in the output." & vbCrLf & _
    "Default: sq"

Private Const DEF_PREFIX_ON_TRIGGER As String = "t"
Public Const DES_PREFIX_ON_TRIGGER As String = _
    "Prefix to add before trigger names" & vbCrLf & _
    "Not all databases require triggers in an export, so you may never see a trigger in the output." & vbCrLf & _
    "Default: t"

'-----------------------------------------------------------------------------------------------------------------------------------------------------------------
' CSV specific
Private Const DEF_CSV_DELIMITER As String = ";"
Public Const DES_CSV_DELIMITER As String = _
    "Separator to use in CSV output." & vbCrLf & _
    "Use vbTab for the tab character" & vbCrLf & _
    "Default: ,"
    
Private Const DEF_CSV_QUOTE As String = """"
Public Const DES_CSV_QUOTE As String = _
    "Optional quote character you want to surround" & vbCrLf & _
    "your text fields with." & vbCrLf & _
    "May be an empty string." & vbCrLf & _
    "Default: "" (note you have to escape the quotes in the definition)"
    
Private Const DEF_CSV_ESCAPE As String = ""
Public Const DES_CSV_ESCAPE As String = _
    "Optional escape character you want to use" & vbCrLf & _
    "if there is e.g. a delimiter in a text field." & vbCrLf & _
    "If you use nothing, the code will escape with" & vbCrLf & _
    "a double delimiter, or double quote, depending" & vbCrLf & _
    "on field contents." & vbCrLf & _
    "May be an empty string." & vbCrLf & _
    "Default: empty string"
    
'-----------------------------------------------------------------------------------------------------------------------------------------------------------------
' Firebird specific
Private Const DEF_FB_BINARY_OUTPUT As String = "BASE64"
Public Const DES_FB_BINARY_OUTPUT As String = _
    "Determines how binary values will be exported." & vbCrLf & _
    "BLOB: use blob sub_type binary fields and store binary data using hex literals." & vbCrLf & _
    "This is supported from Firebird 2.5 and higher." & vbCrLf & _
    "BASE64: use blob sub_type text fields and store base64 encoded data in it." & vbCrLf & _
    "You can/should translate the values yourself." & vbCrLf & _
    "Default: BASE64"

Private Const DEF_FB_WHICH_TOOL As String = "FLAMEROBIN"
Public Const DES_FB_WHICH_TOOL As String = _
    "Used to add tool-specific directives, e.g. an SQL" & vbCrLf & _
    "script can be used with the isql command line application" & vbCrLf & _
    "supplied by Firebird or a GUI tool such as Flamerobin." & vbCrLf & _
    "Use ISQL for Firebird's isql or fb-isql tools" & vbCrLf & _
    "Use FLAMEROBIN for the Flamerobin GUI tool or other GUI tools"

'-----------------------------------------------------------------------------------------------------------------------------------------------------------------
' MySQL specific:
Private Const DEF_MY_ENGINE_TYPE As String = "InnoDB"
Public Const DES_MY_ENGINE_TYPE As String = _
    "Determine database table engine type." & vbCrLf & _
    "This text will be literally used in the CREATE TABLE statement." & vbCrLf & _
    "Use any supported engine type for your MySQL server. Note that" & vbCrLf & _
    "not all engines support foreign keys, autoincrement fields and other features." & vbCrLf & _
    "Default: InnoDB"
    
'-----------------------------------------------------------------------------------------------------------------------------------------------------------------
' PostgreSQL specific:
Private Const DEF_PG_MS_ACCESS_LINK_COMPATIBLE As Boolean = True
Public Const DES_PG_MS_ACCESS_LINK_COMPATIBLE As String = _
    "True if you intend on linking from MS Access to the table. Affects how fields are converted." & vbCrLf & _
    "False if you don't want Access compatibility" & vbCrLf & _
    "Default: True"
    
'-----------------------------------------------------------------------------------------------------------------------------------------------------------------
' Legacy:
Private Const DEF_MSQL_64kb_AVG As Long = 2048
Public Const DES_MSQL_64kb_AVG As String = _
    "Something to do with mSQL?" & vbCrLf & _
    "ALWAYS < 65536 (to be consistent with MS Access)." & vbCrLf & _
    "Set to max expected size of Access MEMO field (to preserve space in mSQL v1)"
    
Private Const DEF_WS_REPLACEMENT As String = ""
Public Const DES_WS_REPLACEMENT As String = _
    "This text will be used to replace whitespace in identifiers such as table names, field names." & vbCrLf & _
    "Note that depending on the output database, specific rules for whitespace are in place:" & vbCrLf & _
    "e.g. Microsoft SQL Server supports whitespace if quoting the names." & vbCrLf & _
    "Could use _ or use an empty string to simply eat whitespaces in identifiers (table and field names)." & vbCrLf & _
    "Default: empty string"
    
Private Const DEF_MAN_IDENT_MAX_SIZE As Integer = -1
Public Const DES_MAN_IDENT_MAX_SIZE As String = _
    "Manual specification of maximum size/length of identifiers such as table and field names." & vbCrLf & _
    "Suggestion: use -1 (let script figure out the possible size per database affected)" & vbCrLf & _
    "18 for DB2 v7" & vbCrLf & _
    "19 for mSQL(?)" & vbCrLf & _
    "30 for Oracle" & vbCrLf & _
    "31 for Firebird" & vbCrLf & _
    "64 for Access 2007, probably other versions, too" & vbCrLf & _
    "64 for MySQL;" & vbCrLf & _
    "64 for PostgreSQL(?)" & vbCrLf & _
    "128 for MS SQL Server 2000+" & vbCrLf & _
    "128 for DB2 v8" & vbCrLf & _
    "Default: -1 (let script figure out the possible size depending on the target database)"

'-----------------------------------------------------------------------------------------------------------------------------------------------------------------
'Forms:
Private Const DEF_FORMS_EXPORT_FORMAT As String = "ACCESS"
Public Const DES_FORMS_EXPORT_FORMAT As String = _
    "Format for forms export:" & vbCrLf & _
    "ACCESS          Microsoft Access" & vbCrLf & _
    "HTML            HTML page" & vbCrLf & _
    "LAZARUS_DELPHI  Delphi/Lazarus RAD environment" & vbCrLf & _
    "LIBREOFFICE     LibreOffice/OpenOffice MS Office alternative" & vbCrLf & _
    "VB.NET          .NET forms" & vbCrLf & _
    "Default: ACCESS"
      
'-----------------------------------------------------------------------------------------------------------------------------------------------------------------
' Output:
Private Const DEF_COMMENT_PREFIX As String = "-- "
Public Const DES_COMMENT_PREFIX As String = _
    "Use empty string for no comments" & vbCrLf & _
    "MySQL: requires -- with a space after it" & vbCrLf & _
    "SQL standard specifies -- for single line comments. Should be safe for all databases." & vbCrLf & _
    "Default: -- (-- with a space)"
    
Private Const DEF_DISPLAY_WARNINGS As Boolean = False
Public Const DES_DISPLAY_WARNINGS As String = _
    "True to interrupt the program and display warning message." & vbCrLf & _
    "This is probably only useful for programmers." & vbCrLf & _
    "False to only write the warning into the output file(s)" & vbCrLf & _
    "Default: False"
    
Private Const DEF_PARA_INSERT_AFTER As Integer = 3
Public Const DES_PARA_INSERT_AFTER As String = _
    "Field count after which data INSERT commands prints multiple lines for one insert statement" & vbCrLf & _
    "Useful to break up large strings into more visible strings." & vbCrLf & _
    "Default: 3"

Private Const DEF_INDENT_SIZE As Integer = 5
Public Const DES_INDENT_SIZE As String = _
    "Number of spaces on indents" & vbCrLf & _
    "Default: 5"

Private Const DEF_INSERT_QUERY_SEPARATOR_EVERY As Long = -1
Public Const DES_INSERT_QUERY_SEPARATOR_EVERY As String = _
    "Commit changes after every x INSERTs for data." & vbCrLf & _
    "Intended to speed up import for databases that support batches of inserts." & vbCrLf & _
    "Use -1 to set to recommended value for database in use." & vbCrLf & _
    "Use 0 to disable and to avoid committing before the end of the data load." & vbCrLf & _
    "MSSQL: suggest to use 1, otherwise you may get String or binary data would be truncated errors" & vbCrLf & _
    "Default: -1"
    
Private Const DEF_FILE_DIRECTORY As String = "C:\Windows\Temp\"
Public Const DES_FILE_DIRECTORY As String = _
    "Output (or input) directory - depending on export or import." & vbCrLf & _
    "When exporting, files will be overwritten if they exist." & vbCrLf & _
    "When importing, the program will look for the relevant files in this directory" & vbCrLf & _
    "Note that on Windows Vista and up permissions are tightened, so" & vbCrLf & _
    "you may get file errors if using the wrong filename." & vbCrLf & _
    "Default: c:\windows\temp\"
    
Private Const DEF_TRANSLATION_FILENAME As String = "AccessExport_name_translations.csv"
Public Const DES_TRANSLATION_FILENAME As String = _
    "File name for output file of names in source and destination. Will be overwritten if exists!" & vbCrLf & _
    "Useful for rewriting your data access code to account for changed names." & vbCrLf & _
    "Default: AccessExport_name_translations.csv"

' End of export options. Don't change items below this line unless you know what you're doing!
'===============================================================================================================================================================
' TECH DATA
'   Public identifiers:
'   * "exportSQL", a function taking and returning no arguments. It runs the export.
'   * for importSQL: see below
'   Functionality:
'   * Can export to scripts for:
'   - Access - we need a read implementation, too
'   - Comma-separated values & text files with other separators
'   - Apache Derby 10.x
'   - IBM DB2
'   - Firebird 2
'   - Microsoft SQL Server 2005+
'   - mSQL v1
'   - mSQL v2
'   - MySQL
'   - Oracle 8+
'   - PostgreSQL
'   - SQL2003 standard SQL
'   - SQLite
'   - Sybase ASE 12+
'     NB: Not all databases have been tested rigorously. Please report
'     improvements/problems.
'   * Excellent respect for name conversion, namespace verification, type matching, etc.
'   * Detects default values "=Now()", "=Date()" and "=Time()" to create types like "TIMESTAMP"
'   * Fully configurable via private constants on top of code.
'     Sensible options will be enforced depending on output database.
'   * Generates compatibility warnings in script output when necessary
'   * Generated files are paragraphed and easy to read
'   * Access text and memo fields can have any type of line termination: \n\r, \r\n, \n or \r
'   * Properly escapes text and memo fields, besides all types of binary fields
'   * Closes all open objects and files on error
'   * Known bugs / incomplete constructs are signalled with comments starting with "todo"
'   * Two alternatives on absent date/time type on mSQL: REAL or CHAR field
'   * For databases that support this: directly commits DDL statements; commits data
'     after a batch of x INSERT statements (see INSERT_QUERY_SEPARATOR_EVERY constant)
'   * Error reporting including line number for quick determination of problems.
'     Hint to developers: you can install MZTools, rightclick on the project and enable/disable
'     line numbers.
'
' Although the code works, there can always be some improvements:
' ***todo/improvements (for entire module):
' - maybe use msysqueries table to parse queries:
' all tables in queries:
'SELECT MSysObjects.Name
'FROM MSysObjects INNER JOIN MSysQueries ON MSysObjects.Id =
'MSysQueries.ObjectId
'WHERE (((MSysQueries.Name1) Like "*" & [Enter Form Name] & "*"));
'type of queries:
'SELECT DISTINCT MSysObjects.Name,
'   IIf([Flags]=0,"Select",IIf([Flags]=16,"Crosstab",IIf([Flags]=32,"Delete",IIf
'([Flags]=48,"Update",IIf([flags]=64,"Append",IIf([flags]=128,"Union",[Flags])))))) AS Type
'FROM MSysObjects INNER JOIN MSysQueries ON MSysObjects.Id =
'MSysQueries.ObjectId;
' - drop structure script does not take foreign key relations into account.
'   Therefore, e.g. Firebird errors out. Easier to just use a new database before importing.
' - verify all calls to name_conf. Not critical, but some calls use target object names instead
'   of source object names, muddying the translation table
' - postgres literal identifier/case preservation; mssql table/column name quoting => unify with objectquote constants
'   use LITERAL_IDENTIFIER variable for this but maybe begin and end quote to support mssql [ ]
' - Firebird: doesn't seem to want to delete & recreate database if you use isql to run the script.
' - clean up variable names (Leszynski naming convention) for importsql
' - verify with mssql2mysql script to see if new functionality necessary in this script
' - character set support for Firebird, Postgresql, ms sql....
' - improve query export. Use mapped export field/table
'   names to search/replace fields and table names in the query sql.
' - name capitalization preservation should be tested.


' Changelog:
' ----: February 15, 2011:
' Fixes for translationfilename acting up with GUI
' Name translation improved
' Started forms export
' 5.00: February 12, 2011:
' GUI for exporting added.
' Fixed stupid error of mine: Dmax wouldn't work on external database.
' 4.06: February 10, 2011:
' Started Sybase, SQL2003, SQLite support.
' Automatically calculate maximum identifier length based on database type
' User can still override length with a constant if they really want to
' Changed quoting behaviour for MSSQL/allowed identifier names
' 4.05: February 9, 2011:
' Changed quoting behaviour for MSSQL (don't always use [ and ]); started adding descriptions to objects in MSSQL
' ----: February 9, 2011:
' Added Firebird tool selection, minor edits.
' 4.04: February 8, 2011:
' Fixed error in using DMax with Access tables with spaces in it to get starting sequence/generator/autoincrement values.
' ----: March 20, 2010:
' Output of NULL data into MySQL now uses NULL instead of an empty field in csv reports.
' See: http://lists.mysql.com/commits/12487
' Thanks Richard Winter for the bug report.
' 4.03: January 22, 2010:
' Added initial DB2 support based on Derby output.
' ----: November 19, 2009:
' START WITH supported for Derby primary keys thanks to Richard Winter
' ----: November 18, 2009:
' Dropped import script option as it would be much easier to write
' in .Net using regex etc.
' Changed Derby autonumbers from GENERATED ALWAYS to GENERATED BY DEFAULT
' Some code cleanup.
' ----: November 15, 2009:
' Import function improved for Access dbs.
' todo: improve Derby output
' 4.02: November 14, 2009. Released.
' Added preliminary Derby support.
' Fixes for CSV export (proper use of delimiter, quote only text fields, 1 file per table
' Fixes for MySQL export (memo output to longtext instead of longblob, standard use of innodb, foreign keys should now work)
' Thanks to Richard Winter for testing and improvement proposals.
' 4.01: October 31, 2009. Released.
' ----: October 30, 2009:
' Added line number error handling, csv export tweaks, MS Access output tweaks.
' ----: October 29, 2009:
' Fixed MySQL table generation putting closing ) before index creation thanks to Richard Winter's bug report.
' Updated CSV code thanks to bug report by Richard Winter
' Updated default value processing for fields
' : October 24, 2009:
' Incorporated most of PostgreSQL code by Dobrica Pavlinusic
' Added experimental Access, Oracle and SQL Server output; now we need a corresponding read method for Access.
' Don't generate indexes on blobs in Firebird as that is not supported.
' Reorganize DDL into separate subs for clarity/maintainability
' Split up target field code into separate function for clarity/maintainability
' Reworked name translation.
' no public release: October 18, 2009:
' fixed default value true and false for booleans
' no public release: August 5, 2009:
' - added object name translation table output
' no public release: August 4, 2009:
' - tightened up Firebird allowed characters in identifiers
' - messagebox on export complete
' no public release: May 11, 2009:
' - fix for Dmax function in empty tables generating incorrect sequences
' 3.01: May 10, 2009:
' - Firebird export: fix for sequences starting at 0 for existing data with autonumbers
' - Firebird export: sequence and trigger naming improved
' Reinier Olislagers June 2008: explicitly defined DAO identifiers to avoid confusion
' if both DAO and ADO object library selected.
'
' If you have improvements/updates to the code, could you please send them to me at
' reinier<_removethis_>olislagers@g[thistoo]mail.com
' Pedro Freire has a new site at http://www.pedrofreire.com/
'===============================================================================================================================================================
' Module level vars to store inter-function data:
Enum LineState 'For importing lines of script
   lsComment 'Single comment line
   lsCommentMultiline 'Multiline comment
   lsCommentMultilineLastline 'Last line of multiline comment
   lsStatement 'Line is a statement
   lsExecuteBlock 'Execute buffer of previous statements
End Enum
Enum WarningSeverity
' Used to indicate how severe a problem is
' when calling the warn function
    warning
    remark
End Enum
' These variables are used to store options that may be reset to the constants defined above as DEF_*
Public COMMENT_PREFIX As String
Public CSV_DELIMITER As String
Public CSV_ESCAPE As String
Public CSV_QUOTE As String
Public DB_CONNECT As String
Public DB_ENGINE As String
Public DB_NAME As String
Public DISPLAY_WARNINGS As Boolean
Public EXPORT_DATA As Boolean
Public EXPORT_DELETE_EXISTING_STRUCTURE As Boolean
Public EXPORT_QUERIES As Boolean
Public EXPORT_STRUCTURE As Boolean
Public FB_BINARY_OUTPUT As String
Public FB_WHICH_TOOL As String
Public FORMS_EXPORT As Boolean
Public FORMS_EXPORT_FORMAT As String
Public FILE_DIRECTORY As String
Public INDENT_SIZE As Integer
Public INSERT_QUERY_SEPARATOR_EVERY As Long
Public MAN_IDENT_MAX_SIZE As Integer
Public MSQL_64kb_AVG As Long
Public MY_ENGINE_TYPE As String
Public PARA_INSERT_AFTER As Integer
Public PG_MS_ACCESS_LINK_COMPATIBLE As Boolean
Public PREFIX_ON_CONSTRAINT As String
Public PREFIX_ON_INDEX As String
Public PREFIX_ON_KEYWORD As String
Public PREFIX_ON_SEQUENCE As String
Public PREFIX_ON_STOREDPROC As String
Public PREFIX_ON_TRIGGER As String
Public PREFIX_ON_VIEW As String
Public PRESERVE_CASE As Boolean
Public SUFFIX_ON_CONSTRAINT As String
Public SUFFIX_ON_INDEX As String
Public SUFFIX_ON_KEYWORD As String
Public SUFFIX_ON_STOREDPROC As String
Public SUFFIX_ON_VIEW As String
Public TRANSLATION_FILENAME As String
' Filename (no path) for translation mapping output; FILE_DIRECTORY contents prepended to this to get:
Public TRANSLATION_FILE As String
Public WS_REPLACEMENT As String
Private m_strWarningOutput As String
' List of m_strWarningOutput generated when running import/export.
' Will be output to file
Private IDENT_MAX_SIZE As Long
' Maximum identifier/object name length. Will be set depending on export db (or overridden using MAN_IDENT_MAX_SIZE)
Private m_fPreserveCase As Boolean
' Sometimes we want to preserve case (e.g. Access=>Access conversion) regardless
' of user setting ;)
Private m_strInputFile As String
' File for input; FILE_DIRECTORY contents prepended to this.
Public OUTPUTFILE As String
' File for output; FILE_DIRECTORY contents prepended to this.
' We need GUI code to read/write this, so it is public.
Private m_QuerySep As String
' Terminator/separator of SQL queries (to instruct some monitor program to execute them).
' Will be set in InitializeModuleVars procedure depending on database type.
Private m_strExportDBName As String
' Name for exported database
Private m_strDBUser As String
' Username for database server
Private m_strDBPassword As String
' Password for database server.
Private NameTranslations As New Collection
'Translation table for table, field names etc:
' Contains translation(4) strings:
' (0) scope for name (e.g. table name, or field name within a table)
' (1) name of parent of object (e.g. the table name of a certain field)
' (2) original object name (in source db)
' (3) translated/new object name (in target db)
' (4) CHANGED if name was translated, nothing if not
' useful when trying to translate queries.
' Because we want unique translations (no double names in target db)
' the key is scope<ascii9>parent<ascii9>translation
Private Const IGNOREFORTRANSLATION As String = "//IgnoreOnlyInNewDb" 'If this identifier is used for TranslateName, the name will not appear in the translation table
Private LITERAL_IDENTIFIER As String ' Escape used to force literal identifier names (for eg to preserve case)

Private Function Binary2Hex(ByRef arrData() As Byte) As String
On Error GoTo ConvError:
Dim strHex As String
Dim Output As String
Dim i As Integer
Output = ""
For i = 0 To UBound(arrData())
    strHex = Hex$(arrData(i))
    If Len(strHex) < 2 Then ' Byte < 16
        strHex = "0" & strHex  ' ensure it is 2 digits
    End If
    Output = Output & strHex
Next i
Binary2Hex = Output
Exit Function

ConvError:
' could be a fatal error, but possibly not. Log & ignore
Dim ErrDesc As String, Errno As Long
Errno = Err.Number
ErrDesc = Err.Description & " (at program line: " & Erl & ")"
Warn "Binary2Hex: conversion error: " & ErrDesc
Debug.Print Now() & " - Binary2Hex: Conversion error: " & ErrDesc & " (Error number: " & Errno & ")"
Binary2Hex = ""
End Function

Private Function Binary2SQL(ByRef arrData() As Byte, _
                    DB_ENGINE As String) As String
' Tries to convert value to database-specific binary
' output
On Error GoTo ConvError:
Dim strTemp As String
Dim bytASCII As Byte
Select Case DB_ENGINE
    Case "ACCESS"
        'Encode it to base64, with indicator
        ' Apparently MSXML parser base64 charset is
        ' ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/
        ' so we don't need to worry about 's
        strTemp = "base64:'" & EncodeBase64(arrData) & "'"
    Case "CSV"
        ' To base64, no identifiers
        strTemp = EncodeBase64(arrData)
    Case "DB2", "DERBY"
        ' We don't know yet, so assume we use base64
        strTemp = "'" & EncodeBase64(arrData) & "'"
    Case "FIREBIRD"
        ' Depending on user selection:
        Select Case UCase(FB_BINARY_OUTPUT)
        Case "BASE64"
            ' Translate characters to base64 which we can stuff
            ' into a blob subtype text.
            strTemp = "'" & EncodeBase64(arrData) & "'"
        Case "BLOB"
            ' Firebird 2.5+ suppors hex literals, e.g. X'dead0Fbeef'
            strTemp = "X'" & Binary2Hex(arrData) & "'"
            'Warn "Hex literals only supported in Firebird 2.5 and higher", warning
        Case "ELSE"
            Warn "Unknown option for FB_BINARY_OUTPUT. Aborting.", warning
            Err.Raise vbObjectError + 239, "Field2SQL", "Unknown option for FB_BINARY_OUTPUT. Aborting."
        End Select ' firebird options
    Case "MSSQL", "SYBASE" 'todo: check if Sybase accepts this
        '   We could generate vbs scripts or similar that go:
        '--Write
        'Set oStream = New ADODB.Stream
        'oStream.Type = adTypeBinary
        'oStream.Open
        'oStream.LoadFromFile YourFilePath
        'YourRecordset("FieldName").Value = oStream.Read
        'YourRecordset.Update
        'or perhaps manipulate xml capability in newer sql servers (sql2005+)
        ' to read in base64 encoded stuff:
        'CAST(N'' AS XML).value('xs:base64Binary(sql:variable("@Base64"))', 'VARBINARY(MAX)')
        'and then dump all the binary stuff as files in the output dir.
        ' But apparently we can use binary constants:
        ' Binary constants have the prefix 0x and are a string of hexadecimal numbers.
        'They are not enclosed in quotation marks.
        strTemp = "0x" & Binary2Hex(arrData)
    Case "MY", "M1", "M2" 'Mysql
        ' Escape using MySQL syntax. We could double up our
        ' quotes, but escaping is clearer and we can also
        ' use it for binary fields.
        strTemp = CStr(arrData)
        strTemp = Replace(strTemp, Chr$(0), "\0") 'ASCII NUL
        strTemp = Replace(strTemp, vbBack, "") 'Backspace, ASCII 8
        strTemp = Replace(strTemp, vbTab, "\t") 'Tab, ascii 9
        strTemp = Replace(strTemp, vbLf, "\l") 'Line feed, ascii 10
        strTemp = Replace(strTemp, vbCr, "\r") 'Carriage return, ascii 13
        strTemp = Replace(strTemp, "\", "\\")
        strTemp = Replace(strTemp, Chr$(26), "\Z") 'Ctrl-Z, ASCII 26, Windows end of file marker
        strTemp = Replace(strTemp, "'", "\'")
        ' Now surround output with single quotes.
        strTemp = "'" & strTemp & "'"
    Case "PG" 'Postgres
        ' Use dollar quoting of result to keep us sane: otherwise
        ' we'd have to use multiple escapes
        
        ' Escape all special characters - an alternative for this
        ' approach would be converting it to base 64 and
        ' passing it to the decode('" & strtemp & "','base64'
        ' function in sql
        strTemp = CStr(arrData)
        For bytASCII = 0 To 31
            ' E.g. for ASCII 9 (tab) we get
            ' \\011
            strTemp = Replace$(strTemp, Chr$(bytASCII), "\" & Right$("000" & Oct(bytASCII), 3))
        Next bytASCII
        For bytASCII = 127 To 255
            strTemp = Replace$(strTemp, Chr$(bytASCII), "\" & Right$("000" & Oct(bytASCII), 3))
        Next bytASCII
        strTemp = "E$$" & strTemp & "$$"
    Case Else '"ORACLE",
        ' At least perform string conversion to avoid
        ' quoting problems=> choose hex, delimited by 's
        strTemp = "'0x" & Binary2Hex(arrData) & "'"
        Warn "Could not find database specific conversion for binary fields. Data is listed as hexadecimal values. Data may come out wrong."
    End Select
Binary2SQL = strTemp
Exit Function

ConvError:
' could be a fatal error, but possibly not. Log & ignore
Dim ErrDesc As String, Errno As Long
Errno = Err.Number
ErrDesc = Err.Description & " (at program line: " & Erl & ")"
Warn "Binary2SQL: conversion error: " & ErrDesc
Debug.Print Now() & " - Binary2SQL: Conversion error: " & ErrDesc & " (Error number: " & Errno & ")"
Binary2SQL = ""
End Function

Private Function Boolean2SQL( _
    ByVal fBoolean As Boolean, _
    DB_ENGINE As String, _
    AccessLinkCompatible As Boolean) As String
' Converts boolean value to output string
' Although we can mostly use field2sql, sometimes we don't
' have a field value that we want to convert, but a regular
' variable.
' Take care to synchronize this code with the main code
' in Field2SQL
' Set default for this field type
'todo: fix accesslinkcompatible stuff
On Error GoTo ConvError:
Dim strTemp As String
If fBoolean = True Then
    strTemp = "1"
Else
    strTemp = "0"
End If
Select Case DB_ENGINE
    Case "ACCESS", "DB2", "DERBY", "FIREBIRD", "MSSQL", "MY", "SYBASE"
        'use default
        ' For Access, we might want to use #TRUE# and #FALSE#; for now stick with default
    Case "CSV", "ORACLE", "PG"
        If strTemp = "1" Then
            strTemp = "TRUE"
        Else
            strTemp = "FALSE"
        End If
    Case "SQLITE"
        If strTemp = "1" Then
            strTemp = "1" 'true
        Else
            strTemp = "0" 'false
        End If
    Case Else
        'for now just keep the default
        Warn "Could not find conversion for boolean datatype. Used a default conversion. Data may come out wrong."
End Select
Boolean2SQL = strTemp
Exit Function

ConvError:
' could be a fatal error, but possibly not. Log & ignore
Dim ErrDesc As String, Errno As Long
Errno = Err.Number
ErrDesc = Err.Description & " (at program line: " & Erl & ")"
Warn "Boolean2SQL: conversion error: " & ErrDesc
Debug.Print Now() & " - Boolean2SQL: Conversion error: " & ErrDesc & " (Error number: " & Errno & ")"
Boolean2SQL = ""
End Function
Private Sub Comment(intOutputHandle As Integer, _
    strComment As String)
' Prints comment to output file if there's a comment marker
' defined. Otherwise prints nothing.
' Can handle multi-line comments.
Dim strCleanComments As String
If COMMENT_PREFIX <> "" Then
    If intOutputHandle > 0 Then
        strCleanComments = Replace(strComment, vbCrLf, vbCrLf & COMMENT_PREFIX)
        Print #intOutputHandle, COMMENT_PREFIX & strCleanComments
    End If
End If
End Sub
Private Function Date2SQL( _
    DateValue As Variant, _
    DB_ENGINE As String) As String
' Although we can mostly use field2sql, sometimes we don't
' have a field value that we want to convert, but a regular
' variable.
' Take care to synchronize this code with the main code
' in Field2SQL
On Error GoTo ConvError:
Dim strTemp As String
If IsNull(DateValue) Then
    strTemp = "NULL"
Else
    Select Case DB_ENGINE
    Case "CSV"
        strTemp = Format$(DateValue, "YYYY-MM-DD HH:MM:SS")
    Case "DB2", "DERBY", "Firebird", "MY", "SQLITE"
        strTemp = "'" & Format$(DateValue, "YYYY-MM-DD HH:MM:SS") & "'"
    Case "MSSQL", "SYBASE"
        strTemp = "CAST ('" & Format$(DateValue, "YYYY-MM-DD HH:MM:SS") & "' AS DateTime)"
    Case Else
    '  Guess a default
        strTemp = "'" & Format$(DateValue, "YYYY-MM-DD HH:MM:SS") & "'"
    End Select
End If
Date2SQL = strTemp
Exit Function

ConvError:
' could be a fatal error, but possibly not. Log & ignore
Dim ErrDesc As String, Errno As Long
Errno = Err.Number
ErrDesc = Err.Description & " (at program line: " & Erl & ")"
Warn "Date2SQL: conversion error: " & ErrDesc
Debug.Print Now() & " - Date2SQL: Conversion error: " & ErrDesc & " (Error number: " & Errno & ")"
Date2SQL = ""
End Function

Private Function DecodeBase64(ByVal strData As String) As Byte()
' Adapted from
'http://www.nonhostile.com/howto-encode-decode-base64-vb6.asp
' translated to late binding so we don't need to set references
' and can depend on any ms xml parser.
' We use an external dll for speed.
On Error GoTo ConvError:
Dim objXML As Object 'MSXML2.DOMDocument 'use late binding
Dim objNode As Object 'MSXML2.IXMLDOMElement 'use late binding
' help from MSXML
'Set objXML = New MSXML2.DOMDocument
Set objXML = CreateObject("MSXML2.DOMDocument.3.0") 'could be later version?
Set objNode = objXML.createElement("b64")
objNode.DataType = "bin.base64"
objNode.Text = strData
DecodeBase64 = objNode.nodeTypedValue
' thanks, bye
Set objNode = Nothing
Set objXML = Nothing
Exit Function

ConvError:
' could be a fatal error, but possibly not. Log & ignore
Dim ErrDesc As String, Errno As Long
Errno = Err.Number
ErrDesc = Err.Description & " (at program line: " & Erl & ")"
Warn "DecodeBase64: conversion error: " & ErrDesc
Debug.Print Now() & " - DecodeBase64: Conversion error: " & ErrDesc & " (Error number: " & Errno & ")"
DecodeBase64 = ""
End Function

Private Function DefaultField(InputField As DAO.Field, _
                            DB_ENGINE As String) As String
' Try to translate Access default values for fields.
On Error GoTo ConvError:

Dim strDefault As String 'Access default string
Dim strTemp As String 'Buffer to build result in.

' First test for some exceptions that needs getting out of the function:
strDefault = Trim(InputField.DefaultValue)
If strDefault = "" Then
    ' No default specified
    DefaultField = ""
    Exit Function
End If
strTemp = ""
If DB_ENGINE = "MY" And InputField.Required = False Then
    Warn "In field '" & InputField.Name & "', MySQL needs NOT NULL to support default values! - default can't be set."
    DefaultField = ""
    Exit Function
End If

' Advanced testing:
Select Case DB_ENGINE
Case "ACCESS"
    strTemp = " DEFAULT " & InputField.DefaultValue
Case "CSV"
    Warn "You could manually add support for default value for field " & InputField.Name & ". The default text from Access is: " & strDefault, remark
Case Else 'all other database engines
    If Left$(strDefault, 1) = Chr$(34) Then 'Double quote: "
        'Literal string value
        Select Case DB_ENGINE
        Case "CSV", "M1"
            Warn "You should manually add support for default value for field " & InputField.Name & ". The default text from Access is: " & strDefault, warning
        Case Else ' Just use it
            strTemp = _
                " DEFAULT " & _
                String2SQL( _
                    Mid$(strDefault, 2, Len(strDefault) - 2), _
                    DB_ENGINE)
        End Select
    Else
        Select Case LCase(strDefault)
        Case "no", "false"
            Select Case DB_ENGINE
            Case Else
                strTemp = " DEFAULT " & Boolean2SQL(False, DB_ENGINE, PG_MS_ACCESS_LINK_COMPATIBLE)
            End Select
        Case "yes", "true"
            Select Case DB_ENGINE
            Case Else
                strTemp = " DEFAULT " & Boolean2SQL(True, DB_ENGINE, PG_MS_ACCESS_LINK_COMPATIBLE)
            End Select
        Case "now()", "=now()", "time()", "=time()" 'current time
            Select Case DB_ENGINE
            Case "FIREBIRD", "PG"
                strTemp = " DEFAULT CURRENT_TIMESTAMP" 'or NOW
            Case "MSSQL", "SYBASE"
                strTemp = " DEFAULT CURRENT_TIMESTAMP" 'ANSI SQL equivalent to MSSQL GETDATE
            Case "MY"
                If TranslateFieldType(DB_ENGINE, InputField) = "TIMESTAMP" Then
                    strTemp = " DEFAULT CURRENT_TIMESTAMP" 'only for timestamp data though.
                Else
                    Warn "Cannot assign current time as default value to non-timestamp fields. Please work around this in your application code.", remark
                End If
            Case Else
                Warn "You should manually add support for default value for field " & InputField.Name & ". The default text from Access is: " & strDefault, warning
            End Select
        Case "date()", "=date()" 'current date
            Select Case DB_ENGINE
            Case "FIREBIRD"
                strTemp = " DEFAULT CURRENT_DATE" 'or TODAY
            Case "MSSQL", "SYBASE"
                strTemp = " DEFAULT CURRENT_TIMESTAMP" 'ANSI SQL equivalent to MSSQL GETDATE
            Case "PG"
                strTemp = " DEFAULT CURRENT_TIMESTAMP"
            Case Else
                Warn "You should manually add support for default value for field " & InputField.Name & ". The default text from Access is: " & strDefault, warning
            End Select
        Case "date()-1", "=date()-1" 'yesterday
            Select Case DB_ENGINE
            Case "FIREBIRD"
                strTemp = " DEFAULT (CURRENT_DATE-1)"
            Case "MSSQL", "SYBASE"
                strTemp = " DEFAULT DATEADD(DAY, -1, CURRENT_TIMESTAMP)"
            Case Else
                Warn "You should manually add support for default value for field " & InputField.Name & ". The default text from Access is: " & strDefault, warning
            End Select
        Case "date()+1", "=date()+1" 'tomorrow
            Select Case DB_ENGINE
            Case "FIREBIRD"
                strTemp = " DEFAULT (CURRENT_DATE+1)"
            Case "MSSQL", "SYBASE"
                strTemp = " DEFAULT DATEADD(DAY, 1, CURRENT_TIMESTAMP)"
            Case Else
                Warn "You should manually add support for default value for field " & InputField.Name & ". The default text from Access is: " & strDefault, warning
            End Select
        Case Else
            ' Just try and hope.
            Select Case InputField.Type
                Case DAO.dbBoolean
                    strTemp = " DEFAULT " & Boolean2SQL(strDefault, DB_ENGINE, False)
                Case DAO.dbChar, DAO.dbText, DAO.dbMemo, _
                  DAO.dbBinary, DAO.dbLongBinary, DAO.dbVarBinary, _
                  DAO.dbGUID
                   strTemp = " DEFAULT " & String2SQL(strDefault, DB_ENGINE)
                Case DAO.dbDate, DAO.dbTimeStamp
                    strTemp = " DEFAULT " & Date2SQL(strDefault, DB_ENGINE)
                Case DAO.dbTime
                    strTemp = " DEFAULT " & Time2SQL(strDefault, DB_ENGINE)
                Case DAO.dbInteger, DAO.dbLong, DAO.dbBigInt, DAO.dbByte
                    strTemp = " DEFAULT " & strDefault
                Case DAO.dbCurrency, DAO.dbDecimal, DAO.dbDouble, DAO.dbFloat, DAO.dbNumeric, DAO.dbSingle
                    strTemp = " DEFAULT " & Float2SQL(strDefault, DB_ENGINE)
                Case Else
                    Warn "Unknown field type for default field. Making up some plausible default.", warning
                    strTemp = " DEFAULT " & String2SQL(strDefault, DB_ENGINE)
            End Select
        End Select
    End If '
End Select
DefaultField = strTemp
Exit Function

ConvError:
' could be a fatal error, but possibly not. Log & ignore
Dim ErrDesc As String, Errno As Long
Errno = Err.Number
ErrDesc = Err.Description & " (at program line: " & Erl & ")"
Warn "DefaultField: conversion error: " & ErrDesc
Debug.Print Now() & " - DefaultField: Conversion error: " & ErrDesc & " (Error number: " & Errno & ")"
DefaultField = ""
End Function
Private Function DMaxReplacement( _
    strTableToSearch As String, _
    strFieldToSearch As String, _
    Optional ByRef DBSearch As DAO.Database) As Integer
' Replacement for DMax that also works for non-currentdb databases,
' in other words, that works on other databases opened in code.
'Something like this, but we need to close the recordset
'Nz(currentdb.OpenRecordset("Select Max(ID) from MSysAccessXML;")(0),0) '(0): weird, but takes 0th element out of collection resulting from recordset - the result
Dim rstSearch As DAO.Recordset
On Error GoTo DMaxReplacement_Error

If DBSearch Is Nothing Then
    Set DBSearch = CurrentDb
End If

Set rstSearch = DBSearch.OpenRecordset("SELECT MAX([" & strFieldToSearch & "]) " & _
    "FROM [" & strTableToSearch & "];")
DMaxReplacement = Nz(rstSearch(0).Value, 0) 'If no records, SQL will return Null. Nz converts this to 0

On Error Resume Next
DMaxReplacement_Finalize:
rstSearch.Close
Set rstSearch = Nothing
Exit Function

DMaxReplacement_Error:
Select Case Err.Number
Case Else
    MsgBox "Error: " & Err.Number & vbCrLf & _
        Err.Description & vbCrLf & _
        " in procedure DMaxReplacement of Module basExportImportSQL" & IIf(Erl > 0, " in line " & Erl, "")
    Debug.Print Now() & " - basExportImportSQL.DMaxReplacement error: " & Err.Description & " (Error number: " & Err.Number & ")" & IIf(Erl > 0, " in line " & Erl, "")
    'Log "Error " & Err.Number & vbCrLf & _
        Err.Description & vbCrLf & _
        "in procedure basExportImportSQL.DMaxReplacement" & _
        Iif(Erl > 0, vbCrLf & "in line " & Erl, "")
End Select
Resume DMaxReplacement_Finalize
End Function
Private Function EncodeBase64(ByRef arrData() As Byte) As String
' Adapted from
'http://www.nonhostile.com/howto-encode-decode-base64-vb6.asp
' translated to late binding so we don't need to set references
' and can depend on any ms xml parser.
' We use an external dll for speed.
On Error GoTo ConvError:
Dim objXML As Object 'MSXML2.DOMDocument 'use late binding
Dim objNode As Object 'MSXML2.IXMLDOMElement 'use late binding
' help from MSXML
'Set objXML = New MSXML2.DOMDocument
Set objXML = CreateObject("MSXML2.DOMDocument.3.0") 'could be later version?
' byte array to base64
Set objNode = objXML.createElement("b64")
objNode.DataType = "bin.base64"
objNode.nodeTypedValue = arrData
EncodeBase64 = objNode.Text
' thanks, bye
Set objNode = Nothing
Set objXML = Nothing
Exit Function

ConvError:
' could be a fatal error, but possibly not. Log & ignore
Dim ErrDesc As String, Errno As Long
Errno = Err.Number
ErrDesc = Err.Description & " (at program line: " & Erl & ")"
Warn "EncodeBase64: conversion error: " & ErrDesc
Debug.Print Now() & " - EncodeBase64: Conversion error: " & ErrDesc & " (Error number: " & Errno & ")"
EncodeBase64 = ""
End Function

Private Sub ExportData( _
    dbDB As DAO.Database, _
    ByVal DB_ENGINE As String)
'Go through the table definitions and export all data
On Error GoTo ExportError:
Dim rsObjects As DAO.Recordset
Dim lngTable As Long
Dim lngRecordCounter As Long
Dim lngFieldCounter As Long
Dim strExportTableName As String
Dim strSQLCode As String
Dim strOutputFile As String 'Output file for csv tables
Dim intOutputFile As Integer 'Handle for file
' Initialize vars

' Setup environment before inserting any data:
Select Case DB_ENGINE
Case Else
    'nothing
End Select

For lngTable = 0 To dbDB.TableDefs.Count - 1

    ' Let's take only the visible, non-system tables
    If (((dbDB.TableDefs(lngTable).Attributes And DB_SYSTEMOBJECT) Or _
        (dbDB.TableDefs(lngTable).Attributes And DB_HIDDENOBJECT))) = 0 Then
        strExportTableName = GetTranslatedTable(dbDB.TableDefs(lngTable).Name)
        If strExportTableName = "" Then
            ' Note that GetTranslatedTable may come up empty
            ' if we haven't exported our database structure first.
            ' In that case, call TranslateName to get a translation and
            ' fill the translation table.
            strExportTableName = TranslateName( _
                dbDB.TableDefs(lngTable).Name, _
                "Database", _
                "Table", _
                DB_ENGINE)
        End If
        ' INSERT clause
        Set rsObjects = dbDB.OpenRecordset(dbDB.TableDefs(lngTable).Name, DAO.dbOpenForwardOnly, DAO.dbReadOnly)
        ' Apparently forward-only recordset is fater than table type recordset, but stores everything
        ' in memory.
        If rsObjects.RecordCount > 0 Then
            ' Text before insert block for this table:
            Select Case DB_ENGINE
            Case "CSV"
                ' Output table data to a separate file.
                intOutputFile = FreeFile()
                strOutputFile = FILE_DIRECTORY & strExportTableName & ".CSV"
                Open strOutputFile For Output As intOutputFile
                Comment 1, "Table: " & strExportTableName & " exported to: " & strOutputFile
            Case "FIREBIRD"
                StatusMessage 1, "select 'Inserting data for table " & strExportTableName & ":"
            Case "MSSQL", "SYBASE"
                Print #1, "BEGIN TRY"
                Print #1, "  SET IDENTITY_INSERT " & strExportTableName & " ON"
                Print #1, "END TRY"
                Print #1, "BEGIN CATCH"
                Print #1, "print 'Error setting IDENTITY_INSERT to ON for table " & strExportTableName & ". We may not be able to insert records if this does not work.';"
                Print #1, "END CATCH;" & m_QuerySep
                StatusMessage 1, "Inserting data for table " & strExportTableName & ":"
            Case Else
                'do nothing
            End Select
            ' If data in table, loop through each record in the table:
            'rsObjects.MoveFirst 'Not allowed for forward only
            lngRecordCounter = 0
            Do Until rsObjects.EOF

                ' INSERT INTO started.
                ' If long field list, start paragraphing:
                ' columns on separate lines if large number of columns.
                ' only for rdbms systems - for csv and text, it's a different story.
                Select Case DB_ENGINE
                    Case "CSV"
                        ' do nothing except initialize variable. No INSERT INTO necessary
                        strSQLCode = ""
                    Case Else
                        strSQLCode = "INSERT INTO " & strExportTableName
                        If rsObjects.Fields.Count > PARA_INSERT_AFTER Then
                            Print #1, strSQLCode
                            Print #1, Space$(INDENT_SIZE) & "(" & _
                                GetFieldList(rsObjects, dbDB.TableDefs(lngTable).Name, DB_ENGINE) _
                                & ")"
                            Print #1, Space$(INDENT_SIZE) & "VALUES ("
                            strSQLCode = Space$(INDENT_SIZE)
                        Else
                            strSQLCode = strSQLCode & " (" & _
                                GetFieldList(rsObjects, dbDB.TableDefs(lngTable).Name, DB_ENGINE) _
                                & ")"
                            strSQLCode = strSQLCode & " VALUES ("
                        End If
                End Select

                ' loop through each field in each record
                For lngFieldCounter = 0 To rsObjects.Fields.Count - 1
                    ' based on type, prepare the field value
                    ' First split between csv and all other db formats
                    ' as we need different formatting functions
                    If DB_ENGINE = "CSV" Then
                        strSQLCode = strSQLCode & _
                            Field2CSV(rsObjects.Fields(lngFieldCounter), _
                                CSV_DELIMITER, _
                                CSV_QUOTE, _
                                CSV_ESCAPE)
                    Else 'Some sort of database
                        strSQLCode = strSQLCode & _
                            Field2SQL(rsObjects.Fields(lngFieldCounter), DB_ENGINE)
                    End If 'db engine csv or other
                    
                    ' Field separators
                    If lngFieldCounter < rsObjects.Fields.Count - 1 Then
                        Select Case DB_ENGINE
                            Case "CSV"
                                ' Separator can be anything
                                strSQLCode = strSQLCode & CSV_DELIMITER
                            Case Else
                                strSQLCode = strSQLCode & ", "
                                ' Only do paragraphing for "real" databases.
                                If rsObjects.Fields.Count > PARA_INSERT_AFTER Then
                                    Print #1, strSQLCode
                                    strSQLCode = Space$(INDENT_SIZE)
                                End If
                            End Select
                    End If
                Next lngFieldCounter
                
                ' print out result
                Select Case DB_ENGINE
                    Case "CSV"
                        ' be silent
                    Case "M1", "M2" 'todo don't know whether these dbs really don't accept closing ;s
                        strSQLCode = strSQLCode & _
                            IIf(rsObjects.Fields.Count > PARA_INSERT_AFTER, " )", ")")
                    Case Else 'MySQL, Firebird,...
                        strSQLCode = strSQLCode & _
                            IIf(rsObjects.Fields.Count > PARA_INSERT_AFTER, " );", ");")
                End Select
                
                ' Done with one insert:
                Select Case DB_ENGINE
                Case "CSV"
                    ' Print to separate file
                    Print #intOutputFile, strSQLCode
                Case Else
                    Print #1, strSQLCode
                End Select
                lngRecordCounter = lngRecordCounter + 1
                ' Commit every INSERT_QUERY_SEPARATOR_EVERY records, if set:
                If INSERT_QUERY_SEPARATOR_EVERY > 0 Then
                    If lngRecordCounter Mod INSERT_QUERY_SEPARATOR_EVERY = 0 Then
                        ' If INSERT_QUERY_SEPARATOR_EVERY< 0
                        ' This will probably help speed up insert performance for large tables
                        Print #1, m_QuerySep
                        StatusMessage 1, "Just inserted record: " & lngRecordCounter 'Handy for troubleshooting insert errors
                    End If 'lngRecordCounter Mod...
                End If 'INSERT_QUERY_SEPARATOR_EVERY
                
                If COMMENT_PREFIX <> "" And m_strWarningOutput <> "" Then
                    Print #1, m_strWarningOutput
                    m_strWarningOutput = ""
                End If
                rsObjects.MoveNext
            Loop
            ' At end of insert block for table, print this:
            Select Case DB_ENGINE
            Case "MSSQL", "SYBASE"
                ' If we have identity columns, we want to be able
                ' to explicitly insert data into them. For this we
                ' need SET IDENTITY_INSERT, but the server will generate
                ' an error if you don't have that column. As this
                ' error is harmless, we swallow it in order not to
                ' annoy/confuse people checking the logs.
                Print #1, "BEGIN TRY"
                Print #1, "  SET IDENTITY_INSERT " & strExportTableName & " OFF"
                Print #1, "END TRY"
                Print #1, "BEGIN CATCH"
                'Print #1, "print ''" 'basically print empty line, but do something.
                Print #1, "END CATCH;" & m_QuerySep
            Case Else
                'do nothing
            End Select
        Else ' if there is no data on the table (recordcount=0)
            Comment 1, " Table " & strExportTableName & " has no data"
        End If 'If rsObjects.RecordCount
        rsObjects.Close
        Set rsObjects = Nothing
    End If  ' only unhidden/non system tables
    ' After inserting all table data, commit changes, regardless of
    ' setting for INSERT_QUERY_SEPARATOR_EVERY:

Select Case DB_ENGINE
Case "CSV"
    Close #intOutputFile 'Close our per table output file
Case Else
    If m_QuerySep <> "" Then
        Print #1, m_QuerySep
    End If
End Select
Next lngTable
' After inserting all tables, commit changes, regardless of
' setting for INSERT_QUERY_SEPARATOR_EVERY:
Print #1, m_QuerySep
Exit Sub

ExportError:
' Probably a fatal error
Dim ErrDesc As String, Errno As Long
Errno = Err.Number
ErrDesc = Err.Description & " (at program line: " & Erl & ")"
Warn "ExportData: Fatal error. Aborting. Error details: " & ErrDesc & " (program line: " & Erl & ")"
Debug.Print Now() & " - ExportData: Fatal error. Aborting. Error details: " & ErrDesc & " (program line: " & Erl & ")" & " (Error number: " & Errno & ")"
MsgBox ("Fatal error in ExportData. Aborting. Error details: " & vbCrLf & ErrDesc)
End Sub

Sub ExportForms(dbDB As DAO.Database, DB_ENGINE As String)
On Error GoTo ExportForms_Error

Select Case FORMS_EXPORT_FORMAT
Case "ACCESS"
    ' we could do something with transferdatabase for export to access
    Warn "Sorry, forms export for " & FORMS_EXPORT_FORMAT & " is not yet written", remark
Case "HTML"
    Warn "Sorry, forms export for " & FORMS_EXPORT_FORMAT & " is not yet written", remark
Case "LAZARUS_DELPHI"
    Warn "Sorry, forms export for " & FORMS_EXPORT_FORMAT & " is not yet written", remark
Case "LIBREOFFICE"
    Warn "Sorry, forms export for " & FORMS_EXPORT_FORMAT & " is not yet written", remark
Case "VB.NET", ".NET", "VBDOTNET"
    Warn "Sorry, forms export for " & FORMS_EXPORT_FORMAT & " is not yet written", remark
Case Else
    Warn "Unknown form export format: " & FORMS_EXPORT_FORMAT & ". Will not export forms.", warning
End Select

On Error Resume Next 'Ignore further errors when finalizing
ExportForms_Finalize:
Exit Sub

ExportForms_Error:
Select Case Err.Number
Case Else
    MsgBox "Error: " & Err.Number & vbCrLf & _
        Err.Description & vbCrLf & _
        "in procedure ExportForms of Module basExportImportSQL" & IIf(Erl > 0, " in line " & Erl, "")
    Debug.Print Now() & " - basExportImportSQL.ExportForms error: " & Err.Description & " (Error number: " & Err.Number & ")" & IIf(Erl > 0, " in line " & Erl, "")
    'Log "Error " & Err.Number & vbCrLf & _
        Err.Description & vbCrLf & _
        "in procedure basExportImportSQL.ExportForms" & _
        Iif(Erl > 0, vbCrLf & "in line " & Erl, "")
End Select
Resume ExportForms_Finalize
End Sub

Private Sub ExportQueries(dbDB As DAO.Database, DB_ENGINE As String)
On Error GoTo ExportQueriesError:
Dim qdfQuery As DAO.QueryDef
Dim fldField As DAO.Field
Dim strSQLCode As String
Dim strTargetName As String 'View or SP name
For Each qdfQuery In dbDB.QueryDefs
    ' Apparently, system temporary queries in Access start with ~, so ignore these:
    If Left$(qdfQuery.Name, Len("~")) <> "~" Then
        If IsSelectQuery(qdfQuery) Then
                strTargetName = TranslateName( _
                    qdfQuery.Name, _
                    "Database", _
                    "Query", _
                    DB_ENGINE, _
                    PREFIX_ON_VIEW, _
                    SUFFIX_ON_VIEW)
            ' This for ... each loop tries to get a field list. We don't do anything with that
            ' yet but could be used as starting point for SQL conversion.
            For Each fldField In qdfQuery.Fields
                If strSQLCode = "" Then
                    strSQLCode = TranslateName( _
                                    fldField.Name, _
                                    qdfQuery.Name, _
                                    "Query", _
                                    DB_ENGINE)
                Else
                    strSQLCode = strSQLCode & _
                                    ", " & TranslateName( _
                                    fldField.Name, _
                                    qdfQuery.Name, _
                                    "Query", _
                                    DB_ENGINE)
                End If 'strSQLCode
            Next fldField
            Select Case DB_ENGINE
                Case "ACCESS"
                    If Right$(Trim$(qdfQuery.SQL), Len(";")) = ";" Then
                        Print #1, "CREATE VIEW " & strTargetName & " AS " & qdfQuery.SQL
                    Else
                        Print #1, "CREATE VIEW " & strTargetName & " AS " & qdfQuery.SQL & ";"
                    End If
                Case "MSSQL", "PG", "SYBASE"
                    ' Create view; note the query translation will provide a SELECT.
                    Print #1, "CREATE VIEW " & strTargetName & " (" & strSQLCode & ") AS "
                    'Print #1, String2SQL("Fixme: original SQL: " & qdfQuery.SQL, DB_ENGINE) & "; " & m_QuerySep
                    ' Assume false=0, true=1
                    Print #1, TranslateQuery(qdfQuery.Name, qdfQuery.SQL, DB_ENGINE) & m_QuerySep
                Case "FIREBIRD"
                    ' Create dummy view, but add original sql into description field
                    Print #1, "CREATE VIEW " & strTargetName & " (DummyField) AS SELECT "
                    Print #1, "'Fixme: original SQL in object comment.' FROM RDB$DATABASE; " & m_QuerySep
                    Print #1, "COMMENT ON VIEW " & strTargetName & " IS " & String2SQL(qdfQuery.SQL, DB_ENGINE) & "; " & m_QuerySep
                Case "ORACLE"
                    ' Create dummy view with source code
                    Print #1, "CREATE VIEW " & strTargetName & " (DummyField) AS SELECT "
                    Print #1, String2SQL("Fixme: original SQL: " & qdfQuery.SQL, DB_ENGINE) & " FROM DUAL; " & m_QuerySep
                Case Else '"CSV", "DB2", "DERBY", "SQL2003", "SQLITE"
                    Comment 1, "Please create a view for query " & qdfQuery.Name & " with source SQL: " & vbCrLf & _
                        qdfQuery.SQL
            End Select
            strSQLCode = ""
        Else
        'no select, try to turn it into an SP
            strTargetName = TranslateName( _
                        qdfQuery.Name, _
                        "Database", _
                        "Query", _
                        DB_ENGINE, _
                        PREFIX_ON_STOREDPROC, _
                        SUFFIX_ON_STOREDPROC)
            Select Case DB_ENGINE
                Case "ACCESS"
                    If Right$(Trim$(qdfQuery.SQL), Len(";")) = ";" Then
                        Print #1, "CREATE PROCEDURE " & strTargetName & " AS " & qdfQuery.SQL
                    Else
                        Print #1, "CREATE PROCEDURE " & strTargetName & " AS " & qdfQuery.SQL & ";"
                    End If
                Case "FIREBIRD"
                    Print #1, "SET TERM !! ;"
                    Print #1, "CREATE PROCEDURE " & strTargetName & " AS BEGIN END!! "
                    Print #1, "SET TERM ; !!" & m_QuerySep
                    Print #1, "COMMENT ON PROCEDURE " & strTargetName & " IS " & _
                        String2SQL(qdfQuery.SQL, DB_ENGINE) & "; " & _
                        m_QuerySep
                Case "MY" 'MySQL 5.??? or later?
                    ' Create dummy procedure with source code
                    Print #1, "DELIMITER $$" & m_QuerySep
                    Print #1, "CREATE PROCEDURE " & strTargetName & " (OUT OutVar LONGTEXT) BEGIN SELECT " & _
                        String2SQL("Fixme: original SQL: " & qdfQuery.SQL, DB_ENGINE) & " INTO Outvar; END;" & _
                        m_QuerySep
                    Print #1, "$$" & m_QuerySep
                    Print #1, "DELIMITER ;" & m_QuerySep
                Case "MSSQL", "SYBASE"
                    ' Create dummy procedure with source code
                    Print #1, "CREATE PROCEDURE " & strTargetName & " @OutVar varchar OUT AS "
                    Print #1, "SET NOCOUNT ON SELECT @OutVar="
                    Print #1, String2SQL("Fixme: original SQL: " & qdfQuery.SQL, DB_ENGINE) & "; " & m_QuerySep
                Case "ORACLE"
                    ' Create dummy procedure with source code
                    Print #1, "CREATE PROCEDURE " & strTargetName & _
                        " (DummyField) AS SELECT " & _
                        String2SQL("Fixme: original SQL: " & qdfQuery.SQL, DB_ENGINE) & _
                        " FROM DUAL; " & m_QuerySep
                Case "PG"
                    ' Create dummy procedure with source code
                    ' note: requires PL/pgSQL to be loaded in
                    Print #1, "CREATE FUNCTION " & strTargetName & "(outvar varchar) RETURNS varchar AS $$"
                    Print #1, "DECLARE "
                    Print #1, " outputvar varchar; "
                    Print #1, "BEGIN "
                    Print #1, " SELECT " & _
                        String2SQL("Fixme: original SQL: " & qdfQuery.SQL, DB_ENGINE) & _
                        " INTO outputvar "
                    Print #1, " RETURN outputvar; "
                    Print #1, "END; "
                    Print #1, "$$ LANGUAGE plpgsql; "
                Case Else '"CSV", "DB2", "DERBY", "SQL2003", "SQLITE"
                    Comment 1, "Please create a stored procedure or view for query " & qdfQuery.Name & " with source SQL: " & vbCrLf & _
                        qdfQuery.SQL
            End Select
        End If
    End If
Next qdfQuery
Exit Sub

ExportQueriesError:
' Probably a fatal error
Dim ErrDesc As String, Errno As Long
Errno = Err.Number
ErrDesc = Err.Description & " (at program line: " & Erl & ")" & " (at program line: " & Erl & ")"
Warn "ExportQueries: Fatal error. Aborting. Error details: " & ErrDesc
Debug.Print Now() & " - ExportQueries: Fatal error. Aborting. Error details: " & ErrDesc & " (program line: " & Erl & ")" & " (Error number: " & Errno & ")"
MsgBox ("Fatal error in ExportQueries. Aborting. Error details: " & vbCrLf & ErrDesc)
End Sub

'
'===============================================================================================================================================================
' Primary Export Function
' If SetDefaultOptions is set false, module level variables representing export options won't be set to the values
' specified in the constants. This allows GUIs etc. to set the variables/options themselves
Public Sub exportSQL(Optional SetDefaultOptions As Boolean = True)
Dim dbDB As DAO.Database
Dim strExportFormatDescription As String
Dim strPostDataExportDDL As String ' Actions to perform after data has been dumped, e.g. triggers in Firebird.
Dim TransItem As Variant ' Translation between names for 1 name
Dim i As Long ' General counter

' Error handling:
On Error GoTo exportSQL_error

DoCmd.Hourglass True
Debug.Print Now() & " - Export started. This may take a while for large databases."
' Initialize:
InitializeModuleVars (SetDefaultOptions) ' Initialize some variables based on database type

If DB_NAME = "" Then
    Set dbDB = CurrentDb()
Else
    Set dbDB = OpenDatabase(Name:=DB_NAME, Options:=False, ReadOnly:=True, Connect:=DB_CONNECT) 'Shared mode
End If
Select Case DB_ENGINE
    Case "ACCESS"
        strExportFormatDescription = "Microsoft Access"
    Case "CSV"
        strExportFormatDescription = "Text output"
    Case "DB2"
        strExportFormatDescription = "IBM DB2"
    Case "DERBY"
        strExportFormatDescription = "Apache Derby 10.x"
    Case "FIREBIRD"
        strExportFormatDescription = "Firebird 2.x"
    Case "M1"
        strExportFormatDescription = "mSQL v1"
    Case "M2"
        strExportFormatDescription = "mSQL v2"
    Case "MSSQL"
        strExportFormatDescription = "Microsoft SQL Server 2005/2008 or higher"
    Case "MY", "MYSQL"
        strExportFormatDescription = "MySQL"
    Case "ORACLE"
        strExportFormatDescription = "Oracle 8 or higher"
    Case "PG"
        strExportFormatDescription = "PostgreSQL"
    Case "SQL2003"
        strExportFormatDescription = "Standard SQL"
    Case "SQLITE"
        strExportFormatDescription = "SQLite"
    Case "SYBASE"
        strExportFormatDescription = "Sybase ASE 12 or higher"
    Case Else
        strExportFormatDescription = "Error: unknown DB_ENGINE " & DB_ENGINE
        MsgBox "Unknown database engine specified: " & DB_ENGINE & vbCrLf & _
            "Make sure you correctly edited this line in the code: Private Const DB_ENGINE As String =" & vbCrLf & _
            "Aborting."
        Err.Raise vbObjectError + 234, "ExportSQL", "Unknown database engine specified: " & DB_ENGINE
End Select

If OUTPUTFILE <> "" Then
    Open OUTPUTFILE For Output As #1
Else
    Warn "You did not specify a proper output file, so we can't generate a script for you."
End If

If TRANSLATION_FILE <> "" Then
    Open TRANSLATION_FILE For Output As #2
End If
strPostDataExportDDL = ""

' Informative header for user:
Comment 1, "Exported from MS Access to " & strExportFormatDescription & " by:"
Comment 1, "basExportImportSQL"
Comment 1, "(C) 1997-2009 by Pedro Freire, Dobrica Pavlinusic, Laurent Bossavit and Reinier Olislagers"
Comment 1, "Conversion errors, warnings and remarks are included in this script after a comment marker: " & COMMENT_PREFIX
Comment 1, "Conversion started at: " & Format(Now, "YYYY-MM-DD HH:MM:SS") & " (" & Now & " in local notation)."


' Let the import script show some messages to the user, if the database supports it.
' Also set up environment
Select Case DB_ENGINE
Case "FIREBIRD"
    Select Case (UCase(FB_WHICH_TOOL))
    Case "ISQL"
        Comment 1, "Exported for the ISQL/isql-fb tool."
        Comment 1, "You can run this script with e.g. isql or isql-fb, e.g.:"
        Comment 1, "isql-fb -user SYSDBA -password masterkey -input " & OUTPUTFILE & " -output " & OUTPUTFILE & "_errors.txt"
        Print #1, "SET BAIL OFF; -- Don't exit script on error. Useful when deleting non-existent objects."
    Case "FLAMEROBIN"
        Comment 1, "Exported for Flamerobin and other GUI tools."
    Case Else
        Comment 1, "Unknown tool " & FB_WHICH_TOOL & " selected in the FB_WHICH_TOOL constant. Please fix. Results may be bad."
    End Select
    'This is actually a valid comment regardless of tool used:
    Comment 1, "If you run a GUI tool such as Flamerobin, remove any drop database, create database"
    Comment 1, "and connect statements as you're supposed to create/delete/connect to databases with the GUI."
    'comment(1,"SET NAMES xxx; -- Set character set"
Case "MSSQL"
    Comment 1, "You can run this script using e.g.:"
    Comment 1, "sqlcmd -S .\SQLEXPRESS -E -i " & OUTPUTFILE & " -o " & OUTPUTFILE & "_errors.txt"
    Comment 1, "for a local SQL Express default install."
    Print #1, "print 'Starting import.'" 'Notify user running script that we're startin the import
End Select

' Export structure/DDL.
If EXPORT_STRUCTURE = True Then
    ' Creates table definitions, and other things required before data export happens.
    ' Only useful for "true" database systems.
    ExportStructure _
        dbDB, _
        DB_ENGINE, _
        strPostDataExportDDL, _
        EXPORT_DELETE_EXISTING_STRUCTURE
Else
    ' No creation of structure, but if we're going to export data
    ' we need to connect to the db for some databases
    Select Case DB_ENGINE
    Case "FIREBIRD"
        If FB_WHICH_TOOL = "ISQL" Then
            Print #1, "CONNECT '" & m_strExportDBName & "' user '" & m_strDBUser & "' password '" & m_strDBPassword & "';" & m_QuerySep
        End If
    End Select
End If

If EXPORT_DATA = True Then
    'Export database contents if requested:
    ExportData _
        dbDB, _
        DB_ENGINE
End If 'EXPORT_DATA

' Indices, triggers etc - handy to export after data export, so
' database engine can efficiently create indices:
' Also useful for primary key autonumbers/sequences:
If EXPORT_STRUCTURE = True Then
    ExportStructureAfterData _
    dbDB, _
    DB_ENGINE, _
    strPostDataExportDDL
End If

If EXPORT_STRUCTURE = True Then
    ' Access queries: only create the name with the original SQL between multiline comment
    ' - SELECT queries => VIEWS (but not SELECT....INTO...)
    ' - INSERT, DELETE queries => SPs
    If EXPORT_QUERIES = True Then
        ExportQueries dbDB, DB_ENGINE
    End If 'EXPORT_QUERIES
End If

' Export forms if required
If FORMS_EXPORT = True Then
    ExportForms dbDB, DB_ENGINE
End If
    
' Print translation/mapping file:
Print #2, "Scope;Parent;Original name;New name;Status"
For i = 1 To NameTranslations.Count
    TransItem = NameTranslations(i)
    Print #2, TransItem(0) & ";" & TransItem(1) & ";" & TransItem(2) & ";" & TransItem(3); ";" & TransItem(4)
Next

' Let the import script show some messages to the user, if the database supports it.
Select Case DB_ENGINE
Case "MSSQL", "SYBASE"
    Print #1, "print 'Import finished.'" 'Notify user running script that we're startin the import
End Select
' Informative footer for user:
Comment 1, "Conversion finished at: " & Format(Now, "YYYY-MM-DD HH:MM:SS") & " (" & Now & " in local notation)."


exportSQL_exit:
DoCmd.Hourglass False
Debug.Print Now() & " - Export complete"
Debug.Print Now() & " - Result files: " & vbCrLf & _
    OUTPUTFILE & vbCrLf & _
    TRANSLATION_FILE
MsgBox ("Export complete. Result files: " & vbCrLf & _
    OUTPUTFILE & vbCrLf & _
    TRANSLATION_FILE)

exportSQL_finalize:
' We don't want any trouble
' If objects don't properly close, we don't care anymore
On Error Resume Next

Close #2
Close #1

dbDB.Close
Set dbDB = Nothing
Exit Sub

exportSQL_error:
DoCmd.Hourglass False
Dim ErrDesc As String, Errno As Long
Errno = Err.Number
ErrDesc = Err.Description & " (at program line: " & Erl & ")"
MsgBox "Error: " & ErrDesc
Debug.Print Now() & " - ExportSQL fatal error: " & ErrDesc & " (Error number: " & Errno & ")"
Resume exportSQL_finalize
End Sub

Private Sub ExportStructure( _
    dbDB As DAO.Database, _
    ByVal DB_ENGINE As String, _
    ByRef strPostDataImportDDL As String, _
    ByVal DropBeforeCreate As Boolean)
' Exports DDL/table structure. Stores index info/primary key info in
' strPostDataImportDDL, ready to run after possible data export.
Dim fDatabaseDeleted As Boolean 'If yes, we tried to delete the database, so no need to delete tables etc.
Dim lngTable As Long ' Counter for iterating through Access tables.
Dim strExportTableName As String ' Table name that has been translated to comply with output database standards
Dim strExportFieldName As String
Dim strType As String ' Field type
Dim strDefault As String ' Field default value
Dim strValidationRule As String 'Access validation rule
Dim strValidationText As String 'Access validation text
Dim strFieldDescription As String 'Access field description, if any
Dim strTargetFieldDescrSQL As String 'Output for field descriptions
Dim lngFieldCounter As Long 'Iterates through fields in access table
Dim idxIndex As DAO.Index 'Access index object
Dim fPrimaryFound As Boolean 'Primary key found?
Dim fFound_ix As Boolean 'Flag for finding index
Dim fldField As DAO.Field 'Access field object (in table or index)
Dim strSQLCode As String ' Buffer for output.
Dim strRunAfterCreate As String ' Script part to run after the create table
' part is finished, but before closing this procedure.
Dim strTemp As String 'Temporary storage; don't depend on values of this
Dim i As Long ' General counter
' Load script initialization
On Error GoTo ExportStructure_Error

fDatabaseDeleted = False

' Output that is run at a database level, before creating tables:
Select Case DB_ENGINE
    Case "FIREBIRD"
        'todo: fix this. shouldn't be necessary to drop db before dropping/creating domain
        If DropBeforeCreate = True Then
            'Comment 1, "Make sure you don't drop objects that don't exist. Adjust script if necessary."
            'should no longer be necessary as we use workarounds in Firebird 2.x
        Else
            Comment 1, "Make sure you don't create objects that already exist. Adjust script if necessary."
        End If
        ' Create domain for boolean datatypes
        If DropBeforeCreate = True Then
            If fDatabaseDeleted = False Then
                Select Case (UCase(FB_WHICH_TOOL))
                Case "ISQL"
                    Print #1, "CONNECT '" & m_strExportDBName & "' user '" & m_strDBUser & "' password '" & m_strDBPassword & "';" & m_QuerySep
                    Print #1, "DROP DATABASE;" & m_QuerySep 'Firebird can apparently only drop the current db.
                    fDatabaseDeleted = True
                    'Not necessary any more
                    'Print #1, "DROP DOMAIN BOOLEAN; " & m_QuerySep
                Case Else
                    Comment 1, "Please drop database yourself."
                End Select
            End If
        End If
        Select Case (UCase(FB_WHICH_TOOL))
        Case "ISQL"
            Comment 1, "Adjust for your environment: file location, username, password etc."
            Print #1, "CREATE DATABASE '" & m_strExportDBName & "' " & _
                "PAGE_SIZE=16384 DEFAULT CHARACTER SET NONE;" & m_QuerySep
                '"USER '" & m_strDBUser & "' Password '" & m_strDBPassword & "' " & _
                '
            Print #1, "CONNECT '" & m_strExportDBName & "' user '" & m_strDBUser & "' password '" & m_strDBPassword & "';" & m_QuerySep
        Case Else
            Comment 1, "Please (re)create database yourself."
        End Select
        Print #1, "CREATE DOMAIN BOOLEAN AS SMALLINT CHECK (VALUE IS NULL OR VALUE IN (0,1));" & m_QuerySep
    Case "MSSQL"
        Print #1, "SET ANSI_NULLS ON" & m_QuerySep
        Print #1, "SET QUOTED_IDENTIFIER ON" & m_QuerySep
        Print #1, "SET NOCOUNT ON " & m_QuerySep 'Don't show (x rows affected) output
        Print #1, "USE [master]" & m_QuerySep
        If DropBeforeCreate = True Then
            ' Check if processes have the db open.
            ' sys.sysprocesses is deprecated as from SQL2005, so don't use it.
            'Print #1, "DECLARE @dbid INT"
            'Print #1, "SELECT @dbid = dbid FROM sys.sysdatabases WHERE name = '" & m_strExportDBName & "'"""
            'Print #1, "IF EXISTS (SELECT spid FROM sys.sysprocesses WHERE dbid = @dbid)"""
            'Print #1, "BEGIN"
            'Print #1, "RAISERROR ('The import cannot delete the database because some processes are blockint it', 16, 1)"
            'Print #1, "SELECT 'These processes are blocking the script from occurring' AS Note, spid, last_batch, status, hostname, loginame FROM sys.sysprocesses WHERE dbid = @dbid"
            'Print #1, "END"
            StatusMessage 1, "If the database exists, kick off other users:"
            ' Note: no m_queryseps after these lines as we cannot add GO commands; the
            ' try block is a.... block ;)
            Print #1, "BEGIN TRY"
            Print #1, "  IF DB_ID('" & m_strExportDBName & "') IS NOT NULL ALTER DATABASE " & m_strExportDBName & " SET SINGLE_USER WITH ROLLBACK IMMEDIATE;"
            Print #1, "END TRY"
            Print #1, "BEGIN CATCH"
            ' ... also why we can't use statusmessage
            Print #1, "  print 'Error setting database to single user mode. Let''s continue regardless...'"
            Print #1, "END CATCH" & m_QuerySep
            StatusMessage 1, "If the database exists, drop the database:"
            Print #1, "IF DB_ID('" & m_strExportDBName & "') IS NOT NULL DROP DATABASE  " & m_strExportDBName & ";" & m_QuerySep
            fDatabaseDeleted = True ' we don't need to explicitly delete tables etc. anymore
        End If
        Comment 1, "Note: Check file names, size and location and adapt to your requirements!"
        Print #1, "CREATE DATABASE [" & m_strExportDBName & "] ON PRIMARY"
        Print #1, "( NAME = N'" & m_strExportDBName & "_Data', FILENAME = N'C:\WINDOWS\TEMP\" & m_strExportDBName & "_Data.mdf' , SIZE = 14976KB , MAXSIZE = UNLIMITED, FILEGROWTH = 10%)"
        Print #1, " LOG ON"
        Print #1, "( NAME = N'" & m_strExportDBName & "_Log', FILENAME = N'C:\WINDOWS\TEMP\" & m_strExportDBName & "_Log.ldf' , SIZE = 1024KB , MAXSIZE = 2048GB , FILEGROWTH = 1024KB )" & m_QuerySep
        Comment 1, "Compatibility level may be lower for MSSQL2005?"
        Print #1, "ALTER DATABASE [" & m_strExportDBName & "] SET COMPATIBILITY_LEVEL = 100" & m_QuerySep
        Print #1, "USE [" & m_strExportDBName & "]" & m_QuerySep
        Print #1, ""
    Case "MY" 'MySQL
        If DropBeforeCreate = True Then
            Comment 1, "Make sure you don't drop objects that don't exist. Adjust script if necessary."
        Else
            Comment 1, "Make sure you don't create objects that already exist. Adjust script if necessary."
        End If
        ' Create domain for boolean datatypes
        If DropBeforeCreate = True Then
            If fDatabaseDeleted = False Then
                Print #1, "USE " & m_strExportDBName & ";" & m_QuerySep
                Print #1, "DROP DATABASE ;" & m_QuerySep 'Firebird can apparently only drop the current db.
                fDatabaseDeleted = True
                'Not necessary any more
                'Print #1, "DROP DOMAIN BOOLEAN; " & m_QuerySep
            End If
        End If
        Comment 1, "You can adjust character set etc."
        Print #1, "CREATE DATABASE IF NOT EXISTS " & m_strExportDBName & " " & _
            "CHARACTER SET UTF8;" & m_QuerySep
        Print #1, "USE " & m_strExportDBName & " ;" & m_QuerySep
    Case "SYBASE"
        Comment 1, "Note: Check file names, size and location and adapt to your requirements!"
        Comment 1, "todo: check lines below, shamelessly ripped from MSSQL"
        Print #1, "SET ANSI_NULLS ON" & m_QuerySep
        Print #1, "SET QUOTED_IDENTIFIER ON" & m_QuerySep
        Print #1, "SET NOCOUNT ON " & m_QuerySep 'Don't show (x rows affected) output
        Print #1, "USE [master]" & m_QuerySep
        If DropBeforeCreate = True Then
            ' Check if processes have the db open.
            Print #1, "DECLARE @dbid INT"
            Print #1, "SELECT @dbid = dbid FROM sys.sysdatabases WHERE name = '" & m_strExportDBName & "'"""
            Print #1, "IF EXISTS (SELECT spid FROM sys.sysprocesses WHERE dbid = @dbid)"""
            Print #1, "BEGIN"
            Print #1, "RAISERROR ('The import cannot delete the database because some processes are blockint it', 16, 1)"
            Print #1, "SELECT 'These processes are blocking the script from occurring' AS Note, spid, last_batch, status, hostname, loginame FROM sys.sysprocesses WHERE dbid = @dbid"
            Print #1, "END"
            Print #1, "DROP DATABASE [" & m_strExportDBName & "];" & m_QuerySep
            fDatabaseDeleted = True ' we don't need to explicitly delete tables etc. anymore
        End If
        Print #1, "CREATE DATABASE [" & m_strExportDBName & "] ON PRIMARY"
        Print #1, "( NAME = N'" & m_strExportDBName & "_Data', FILENAME = N'C:\WINDOWS\TEMP\" & m_strExportDBName & "_Data.mdf' , SIZE = 14976KB , MAXSIZE = UNLIMITED, FILEGROWTH = 10%)"
        Print #1, " LOG ON"
        Print #1, "( NAME = N'" & m_strExportDBName & "_Log', FILENAME = N'C:\WINDOWS\TEMP\" & m_strExportDBName & "_Log.ldf' , SIZE = 1024KB , MAXSIZE = 2048GB , FILEGROWTH = 1024KB )" & m_QuerySep
        Comment 1, "Compatibility level may be lower for MSSQL2005?"
        Print #1, "ALTER DATABASE [" & m_strExportDBName & "] SET COMPATIBILITY_LEVEL = 100" & m_QuerySep
        Print #1, "USE [" & m_strExportDBName & "]" & m_QuerySep
        Print #1, ""
End Select
'Go through the table definitions
For lngTable = 0 To dbDB.TableDefs.Count - 1

    ' Let's take only the visible, non-system tables
    If (((dbDB.TableDefs(lngTable).Attributes And DB_SYSTEMOBJECT) Or _
        (dbDB.TableDefs(lngTable).Attributes And DB_HIDDENOBJECT))) = 0 Then
        'Initialize
        strRunAfterCreate = ""
        strTargetFieldDescrSQL = ""
        strExportTableName = TranslateName( _
                                dbDB.TableDefs(lngTable).Name, _
                                "Database", _
                                "Table", _
                                DB_ENGINE)
                                
        If DropBeforeCreate = True Then
            If fDatabaseDeleted = False Then
                ' Only delete tables if we've asked for it and
                ' if we have an existing database.
                Select Case DB_ENGINE
                Case "ACCESS", "DB2", "DERBY"
                    Print #1, "DROP TABLE " & strExportTableName & "; " & m_QuerySep
                Case "FIREBIRD"
                    'Firebird errors out if the object doesn't exist.
                    ' we need to find a workaround for this:
                    'http://www.firebirdfaq.org/faq69/
                    Print #1, "SET TERM !! ;"
                    Print #1, "EXECUTE BLOCK AS BEGIN"
                    Print #1, "-- Only drop table if it exists"
                    Print #1, "IF (EXISTS(SELECT * FROM RDB$RELATIONS WHERE RDB$RELATION_NAME = '" & strExportTableName & "')) THEN EXECUTE STATEMENT 'DROP TABLE " & strExportTableName & "';"
                    Print #1, "END!!"
                    Print #1, "SET TERM ; !!" & m_QuerySep
                Case "M1", "M2"
                    Print #1, "DROP TABLE " & strExportTableName & m_QuerySep
                Case "ORACLE"
                    Print #1, "Begin  "
                    Print #1, "Execute immediate 'DROP TABLE " & strExportTableName & "';"
                    Print #1, "Exception when others then null; --ignores possible error when table exists: simpler than querying data dictionary"
                    Print #1, "End;"
                    Print #1, "/"
                Case Else ' Includes mysql, sybase, sqlite, sql2003
                    Print #1, "DROP TABLE " & strExportTableName & "; " & m_QuerySep
                End Select
            End If
        End If 'dropbeforecreate
        
        ' CREATE clause
        Print #1,
        Print #1, "CREATE TABLE " & strExportTableName
        Print #1, Space$(INDENT_SIZE) & "("
        m_strWarningOutput = ""
        fPrimaryFound = False
        
        ' loop through each field in the table
        For lngFieldCounter = 0 To dbDB.TableDefs(lngTable).Fields.Count - 1
            ' If this is not the first iteration, add separators
            If lngFieldCounter > 0 Then
                Print #1, ","
            End If
            ' get field name
            strExportFieldName = TranslateName( _
                                    dbDB.TableDefs(lngTable).Fields(lngFieldCounter).Name, _
                                    dbDB.TableDefs(lngTable).Name, _
                                    "TableField", _
                                    DB_ENGINE)
            ' Add field description/comment
            strFieldDescription = GetDescription(dbDB.TableDefs(lngTable).Fields(lngFieldCounter))
            If strFieldDescription <> "" Then
                ' Only process if there actually is a description
                Select Case DB_ENGINE
                    Case "CSV"
                        Warn "You can manually add description for field: " & strExportFieldName & " - description: " & strFieldDescription, remark
                    Case "FIREBIRD"
                        'Previous comment may have ended with COMMIT; so add a line feed.
                        strTargetFieldDescrSQL = _
                            strTargetFieldDescrSQL & vbCrLf & _
                            "COMMENT ON COLUMN " & strExportTableName & "." & strExportFieldName & " IS " & _
                                String2SQL( _
                                    strFieldDescription, _
                                    DB_ENGINE) & _
                                ";" & m_QuerySep
                    Case "MSSQL"
                        ' Use a system stored procedure to add descriptions later.
                        ' Don't know for which versions these are supported.
                        strPostDataImportDDL = strPostDataImportDDL & _
                            vbCrLf & _
                            "EXEC sys.sp_addextendedproperty @name=N'MS_Description', @value=" & _
                            String2SQL( _
                                strFieldDescription, _
                                DB_ENGINE) & _
                            ", @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'TABLE',@level1name=N'" & _
                            strExportTableName & _
                            "', @level2type=N'COLUMN',@level2name=N'" & _
                            strExportFieldName & "'" & _
                            m_QuerySep
                    Case Else
                        Warn "You can manually add description for field: " & strExportFieldName & " - description: " & strFieldDescription, remark
                End Select
            End If
            
            ' translate field types
            strType = TranslateFieldType( _
                DB_ENGINE, _
                dbDB.TableDefs(lngTable).Fields(lngFieldCounter))
            strDefault = DefaultField(dbDB.TableDefs(lngTable).Fields(lngFieldCounter), DB_ENGINE)
            If strDefault <> "" Then
                strType = strType & strDefault
            End If
            
            ' check not null and auto-increment properties
            ' Needs to be after default value code, so we get output like
            ' DEFAULT 'hello' NOT NULL
            If ((dbDB.TableDefs(lngTable).Fields(lngFieldCounter).Attributes And _
                dbAutoIncrField) <> 0) Then
                ' Auto-increment field
                Select Case DB_ENGINE
                    Case "CSV"
                        ' do nothing: not supported
                    Case "DB2", "DERBY" 'not sure if this works for db2
                        'Get maximum value of autoincrement field
                        i = DMaxReplacement(dbDB.TableDefs(lngTable).Name, _
                            dbDB.TableDefs(lngTable).Fields(lngFieldCounter).Name, dbDB)
                        strType = strType & " NOT NULL GENERATED BY DEFAULT AS IDENTITY (START WITH " & Trim(CStr(i + 1)) & ", INCREMENT BY 1)"
                    Case "FIREBIRD"
                        strType = strType & " NOT NULL"
                        ' We need to create a sequence/generator and a before insert trigger to emulate
                        ' autoincrement. We'll do that after data import
                        ' Note: if there is existing data, we need to start the generator at <highest autonumber>+1
                        ' If we set the generator to highest autonumber of all data, the value for the next insert
                        ' should be <highest autonumber>+1.
                        ' Sequence:
                        ' Get our name first
                        strTemp = TranslateName( _
                                strExportTableName, _
                                "Database", _
                                "Sequence", _
                                DB_ENGINE, _
                                PREFIX_ON_SEQUENCE)
                        ' First generate sequence delete script, and print it out now to
                        ' make sure we perform it before creating
                        If DropBeforeCreate Then
                            If fDatabaseDeleted = False Then
                                strPostDataImportDDL = strPostDataImportDDL & _
                                    "SET TERM !! ;" & vbCrLf & _
                                    "EXECUTE BLOCK AS BEGIN " & vbCrLf & _
                                    "-- Only drop sequence if it exists." & vbCrLf & _
                                    "IF (EXISTS(SELECT RDB$GENERATOR_NAME FROM RDB$GENERATORS WHERE RDB$GENERATOR_NAME='" & PREFIX_ON_SEQUENCE & strExportTableName & "')) THEN EXECUTE STATEMENT 'DROP SEQUENCE " & strTemp & "'; " & vbCrLf & _
                                    "END!! " & vbCrLf & _
                                    "SET TERM ; !!" & vbCrLf & _
                                    m_QuerySep
                            End If
                        End If 'dropbeforecreate
                        ' Sequence start value-1. Note that Dmax may return null:
                        i = DMaxReplacement(dbDB.TableDefs(lngTable).Name, _
                            dbDB.TableDefs(lngTable).Fields(lngFieldCounter).Name, dbDB)
                        
                        strPostDataImportDDL = strPostDataImportDDL & _
                            vbCrLf & _
                            "CREATE SEQUENCE " & _
                            strTemp & _
                            ";" & m_QuerySep & _
                            vbCrLf & _
                            "ALTER SEQUENCE " & _
                            strTemp & _
                            " RESTART WITH " & _
                            i & _
                            ";" & m_QuerySep
                        strPostDataImportDDL = strPostDataImportDDL & _
                            vbCrLf & _
                            "COMMENT ON SEQUENCE " & _
                            strTemp & _
                            " IS " & _
                            String2SQL( _
                                "Generator/sequence for autonumber emulation in table " & strExportTableName, _
                                DB_ENGINE) & _
                            ";" & m_QuerySep
                        ' Trigger:
                        'note we could have looked up the sequence name in our translation table.
                        ' note that we DON'T run commit after issuing the first
                        ' set term command otherwise this won't work.
                        strPostDataImportDDL = strPostDataImportDDL & _
                            vbCrLf & _
                            "set term !! ;" & vbCrLf & _
                            "CREATE TRIGGER " & _
                            TranslateName( _
                                strExportTableName, _
                                dbDB.TableDefs(lngTable).Name, _
                                "Trigger", _
                                DB_ENGINE, _
                                PREFIX_ON_TRIGGER) & _
                            " FOR " & strExportTableName & " " & _
                            "ACTIVE BEFORE INSERT POSITION 0 " & _
                            "AS " & _
                            "BEGIN " & _
                            "if (NEW." & strExportFieldName & " is NULL) then NEW." & strExportFieldName & _
                            " = NEXT VALUE FOR " & _
                            strTemp & _
                            "; " & vbCrLf & _
                            "END!! " & vbCrLf & _
                            "set term ; !! " & m_QuerySep
                        strPostDataImportDDL = strPostDataImportDDL & _
                            vbCrLf & _
                            "COMMENT ON TRIGGER " & _
                            TranslateName( _
                                strExportTableName, _
                                dbDB.TableDefs(lngTable).Name, _
                                "Trigger", _
                                DB_ENGINE, _
                                PREFIX_ON_TRIGGER) & _
                            " IS " & _
                            String2SQL( _
                                "Trigger for autonumber emulation in table " & strExportTableName, _
                                DB_ENGINE) & _
                            ";" & m_QuerySep
                    Case "M1", "M2"
                        strType = strType & " NOT NULL"
                        Warn "In new field '" & strExportFieldName & "', mSQL does not support auto-increment fields! - they will be pure INTs." & IIf(DB_ENGINE = "M2", " Consider using pseudo field '_rowid' or SEQUENCEs.", "")
                    Case "MSSQL", "SYBASE"
                        ' Figure out starting position for identity/autoincrement field.
                        ' ASSUMING the field increments by 1
                        i = DMaxReplacement(dbDB.TableDefs(lngTable).Name, _
                            dbDB.TableDefs(lngTable).Fields(lngFieldCounter).Name, dbDB)
                        strType = strType & " IDENTITY ( " & i & ",1 ) NOT NULL "
                    Case "MY" 'mysql
                        strType = strType & " NOT NULL AUTO_INCREMENT"
                    Case "ORACLE"
                        strType = strType & " NOT NULL"
                        ' We need to create a sequence/generator and a before insert trigger to emulate
                        ' autoincrement. We'll do that after data import
                        ' Note: if there is existing data, we need to start the generator at <highest autonumber>+1
                        ' If we set the generator to highest autonumber of all data, the value for the next insert
                        ' should be <highest autonumber>+1.
                        ' Sequence:
                        ' Generate sequence delete script, and print it out now to
                        ' make sure we perform it before creating
                        If DropBeforeCreate Then
                            If fDatabaseDeleted = False Then
                                strPostDataImportDDL = strPostDataImportDDL & "DROP SEQUENCE " & PREFIX_ON_SEQUENCE & strExportTableName & ";" & m_QuerySep
                            End If
                        End If 'dropbeforecreate
                        
                        ' Sequence start value - by reading highest existing value in table
                        i = DMaxReplacement(dbDB.TableDefs(lngTable).Name, _
                            dbDB.TableDefs(lngTable).Fields(lngFieldCounter).Name, dbDB)
                        
                        strPostDataImportDDL = strPostDataImportDDL & _
                            vbCrLf & _
                            "CREATE SEQUENCE " & _
                            TranslateName( _
                                strExportTableName, _
                                "Database", _
                                "Sequence", _
                                DB_ENGINE, _
                                PREFIX_ON_SEQUENCE) & _
                            " MINVALUE 0 " & _
                            " MAXVALUE 999999999999999999" & _
                            " START WITH " & _
                            i & _
                            " INCREMENT BY 1 " & _
                            " NOCACHE; " & _
                            "/" & m_QuerySep
                        ' Trigger:
                        strPostDataImportDDL = strPostDataImportDDL & _
                            vbCrLf & _
                            "CREATE OR REPLACE TRIGGER " & _
                            TranslateName( _
                                strExportTableName, _
                                dbDB.TableDefs(lngTable).Name, _
                                "Trigger", _
                                DB_ENGINE, _
                                PREFIX_ON_TRIGGER) & _
                            " BEFORE INSERT ON " & strExportTableName & " " & _
                            " FOR EACH ROW " & _
                            " BEGIN " & _
                            "   IF :new." & strExportFieldName & " IS NULL) THEN " & _
                            "      SELECT " & _
                                    TranslateName( _
                                        strExportTableName, _
                                        "Database", _
                                        "Sequence", _
                                        DB_ENGINE, _
                                        PREFIX_ON_SEQUENCE) & _
                                    ".nextval INTO :new." & strExportFieldName & " FROM DUAL; " & _
                            "   END IF; " & _
                            "END; " & m_QuerySep
                    Case "PG"
                        strType = " bigserial"
                        ' Get current max value for field
                        i = DMaxReplacement(dbDB.TableDefs(lngTable).Name, _
                            dbDB.TableDefs(lngTable).Fields(lngFieldCounter).Name, dbDB)
                        ' Apparently PostgreSQL doesn't need to create a sequence??
                        ' only reset it to correct value
                        ' Also, how does pg know what sequence to bind to
                        ' what field? I suspect we need more statements.
                        strPostDataImportDDL = strPostDataImportDDL & _
                            vbCrLf & _
                            "SELECT setval('" & _
                            TranslateName( _
                                strExportTableName, _
                                "Database", _
                                "Sequence", _
                                DB_ENGINE, _
                                PREFIX_ON_SEQUENCE) & _
                            "_seq" & _
                            "'::regclass, MAX(" & strExportFieldName & ")) " & _
                            " FROM " & strExportTableName & _
                            ";" & m_QuerySep
                    Case "SQLITE"
                        Warn "Not null/autoincrement handling: unimplemented database format " & DB_ENGINE & " specified. Export will probably fail."
                    Case "SQL2003"
                        Warn "Not null/autoincrement handling: unimplemented database format " & DB_ENGINE & " specified. Export will probably fail."
                    Case Else
                        Warn "Not null/autoincrement handling: unknown database format " & DB_ENGINE & " specified. Export will probably fail."
                End Select
            ElseIf dbDB.TableDefs(lngTable).Fields(lngFieldCounter).Required = True Then
            ' Only required, no autoincrement
                strType = strType & " NOT NULL"
            End If
            
            ' Validation rules
            strValidationRule = dbDB.TableDefs(lngTable).Fields(lngFieldCounter).ValidationRule
            strValidationText = dbDB.TableDefs(lngTable).Fields(lngFieldCounter).ValidationText
            If strValidationRule <> "" Then
                Select Case DB_ENGINE
                Case "ACCESS"
                    strType = strType & "do something with validation rule: " & strValidationRule & " - validation text: " & strValidationText
                Case Else
                    ' Manual conversion required
                    Warn "Field '" & strExportFieldName & "' has constraint '" & strValidationRule & _
                        "' with text '" & strValidationText & "' which you have to convert manually"
                End Select
            End If
            
            ' Check if primary key (for mSQL v1), and
            ' error check NOT NULL primary keys for databases that need this.
            Select Case DB_ENGINE
                Case "M1"
                    fFound_ix = False
                    For Each idxIndex In dbDB.TableDefs(lngTable).Indexes
                        If idxIndex.Primary Then
                            For Each fldField In idxIndex.Fields
                                If fldField.Name = dbDB.TableDefs(lngTable).Fields(lngFieldCounter).Name Then
                                    fFound_ix = True
                                    Exit For
                                End If
                            Next fldField
                            If fFound_ix Then
                                ' We found it a second time for a different field, or
                                ' found the other field first.
                                Exit For
                            End If 'fFound_ix
                        End If 'idxIndex.Primary
                    Next idxIndex
                    If fFound_ix Then
                        If fPrimaryFound Then
                            Warn "On new table '" & strExportTableName & "', mSQL v1 does not support more than one PRIMARY KEY! Only first key was set."
                        Else
                            strType = strType & " PRIMARY KEY"
                            fPrimaryFound = True
                        End If 'fPrimaryFound
                    End If
                Case "FIREBIRD", "MSSQL", "SYBASE"
                    ' We need to make sure that all columns in a primary key have NOT NULL.
                    ' In Access, we can have a multicolumn PK where some fields can be NULL.
                    ' In Firebird, at least <=2.5, and MSSQL (at least 2008), this is not allowed.
                    ' probably the same applies for Sybase? todo: check
                    For Each idxIndex In dbDB.TableDefs(lngTable).Indexes
                        If idxIndex.Primary Then
                            For Each fldField In idxIndex.Fields
                                If fldField.Name = dbDB.TableDefs(lngTable).Fields(lngFieldCounter).Name Then
                                ' Check for the field we're currently dealing with.
                                    If InStr(strType, "NOT NULL") = 0 Then
                                        Warn "Primary key field " & dbDB.TableDefs(lngTable).Fields(lngFieldCounter).Name & _
                                        " does not have NOT NULL. This is required in this database, so this will be set."
                                        strType = strType & " NOT NULL"
                                    End If ' InStr(strType, "NOT NULL")
                                End If ' fldField.Name
                            Next fldField
                        End If 'idxIndex.Primary
                    Next idxIndex
            End Select
            
            ' Print out all field info we've been collecting:
            Print #1, Space$(INDENT_SIZE) & _
                strExportFieldName & _
                Space$(IDENT_MAX_SIZE - Len(strExportFieldName) + 2) & _
                strType;
        Next lngFieldCounter
                    
             
        ' Primary key and other index declaration for some dbs that don't support
        ' altering these after the data has been inserted.
        'todo: probably sqlite as well
        Select Case DB_ENGINE
            Case "M2", "MY"
                For Each idxIndex In dbDB.TableDefs(lngTable).Indexes
                    strSQLCode = ""
                    For Each fldField In idxIndex.Fields
                        strTemp = _
                            TranslateName( _
                            fldField.Name, _
                            idxIndex.Name, _
                            "IndexField", _
                            DB_ENGINE)
                        Select Case IndexTargetFieldDataType( _
                        DB_ENGINE, _
                        dbDB.TableDefs(lngTable), _
                        fldField _
                        )
                            Case "LONGBLOB", "LONGTEXT"
                                If DB_ENGINE = "MY" Then
                                    Warn "MySQL can't index on field " & fldField.Name, warning
                                Else
                                    strSQLCode = strSQLCode & IIf(strSQLCode = "", "", ", ") & strTemp
                                End If
                            Case Else
                                strSQLCode = strSQLCode & IIf(strSQLCode = "", "", ", ") & strTemp
                        End Select
                    Next fldField
                    If DB_ENGINE = "M2" Then
                        Print #1, "CREATE " & IIf(idxIndex.Unique, "UNIQUE ", "") & "INDEX " & _
                        TranslateName( _
                            idxIndex.Name, _
                            strExportTableName, _
                            "Index", _
                            DB_ENGINE, _
                            PREFIX_ON_INDEX, _
                            SUFFIX_ON_INDEX) & _
                        " ON " & _
                        strExportTableName & " (" & strSQLCode & ")" & m_QuerySep
                    Else 'MySQL, M1
                        If strSQLCode > "" Then
                            Print #1, ","
                            Print #1, Space$(INDENT_SIZE) & IIf(idxIndex.Primary, "PRIMARY ", "") & _
                            "KEY (" & strSQLCode & ")";
                        End If
                    End If 'DB_ENGINE = "M2"
                Next idxIndex
        End Select

        ' terminate CREATE clause; optionally add some table-level specifications
        Select Case DB_ENGINE
            Case "FIREBIRD"
                Print #1, "" 'Empty line to close off column definition
                Print #1, Space$(INDENT_SIZE) & "); " & m_QuerySep
            Case "M2"
                Print #1, "" 'Empty line to close off column definition
                Print #1, Space$(INDENT_SIZE) & ");" & m_QuerySep
            Case "MSSQL"
                Print #1, "" 'Empty line to close off column definition
                Print #1, Space$(INDENT_SIZE) & ") ON [PRIMARY];" & m_QuerySep
            Case "MY"
                Print #1, "" 'Empty line to close of column definition
                Print #1, Space$(INDENT_SIZE) & ") ENGINE=" & MY_ENGINE_TYPE & " ;" & m_QuerySep
            Case Else
                Print #1, "" 'Empty line to close off column definition
                Print #1, Space$(INDENT_SIZE) & ");" & m_QuerySep
        End Select
        
        ' After table creation, add table description, column descriptions
        Select Case DB_ENGINE
            Case "ACCESS"
                If GetDescription(dbDB.TableDefs(lngTable)) <> "" Then
                    Print #1, "COMMENT ON TABLE " & strExportTableName & " IS " & _
                        String2SQL( _
                            GetDescription(dbDB.TableDefs(lngTable)), _
                            DB_ENGINE) & _
                        ";" & m_QuerySep
                    Print #1, strTargetFieldDescrSQL
                End If
            Case "FIREBIRD"
                If GetDescription(dbDB.TableDefs(lngTable)) <> "" Then
                    Print #1, "COMMENT ON TABLE " & strExportTableName & " IS " & _
                        String2SQL( _
                            GetDescription(dbDB.TableDefs(lngTable)), _
                            DB_ENGINE) & _
                        ";" & m_QuerySep
                    Print #1, strTargetFieldDescrSQL
                End If
            Case Else
                'todo: add for other databases if possible in their sql syntax
                If GetDescription(dbDB.TableDefs(lngTable)) <> "" Then
                    Warn "You can manually add description for table: " & _
                        strExportTableName & ". Description is " & _
                        GetDescription(dbDB.TableDefs(lngTable)), remark
                End If
                
        End Select
                
        ' Add post create stuff we've generated above.
        If strRunAfterCreate <> "" Then
            Print #1, strRunAfterCreate
        End If
        
        ' print any m_strWarningOutput below it
        If COMMENT_PREFIX <> "" And m_strWarningOutput <> "" Then
            If DB_ENGINE = "M2" Then
                Comment 1, ""
            End If
            Print #1, m_strWarningOutput
            m_strWarningOutput = ""
        End If
    End If  ' only unhidden/non system tables
Next lngTable

On Error GoTo 0
Exit Sub

ExportStructure_Error:
DoCmd.Hourglass False
If Erl > 0 Then
    ' Program uses line numbers
    MsgBox "Error: " & Err.Number & vbCrLf & _
  Err.Description & vbCrLf & _
  "in line " & Erl & " of procedure ExportStructure of Module basExportImportSQL"
Else
    MsgBox "Error: " & Err.Number & vbCrLf & _
  Err.Description & vbCrLf & _
  "in procedure ExportStructure of Module basExportImportSQL"
End If
Debug.Print Now() & " - ExportStructure fatal error: " & Err.Description & " (Error number: " & Err.Number & ")" & " at line " & Erl
End
End Sub
Private Sub ExportStructureAfterData( _
    dbDB As DAO.Database, _
    ByVal DB_ENGINE As String, _
    ByRef strPostDataImportDDL As String)
Dim strExportTableName As String ' Table name that has been translated to comply with output database standards
Dim lngTable As Long ' Counter for iterating through Access tables.
Dim idxIndex As DAO.Index 'Access index object
Dim strOurFieldsList As String 'List of fields??
Dim fldField As DAO.Field 'Access field object (in table or index)
Dim relRelation As DAO.Relation ' Access foreign key relation object
Dim strCascadeDelete As String
Dim strCascadeUpdate As String
Dim strSQLCode As String ' Buffer for output.
Dim i As Long ' General counter
Dim strTemp As String 'String for temporary use. Don't rely on the content of this string between operations.

' Normal indices and primary keys. Primary keys at least should be run before foreign keys.
On Error GoTo ExportStructureAfterData_Error

For lngTable = 0 To dbDB.TableDefs.Count - 1
    ' Let's take only the visible, non-system tables
    If (((dbDB.TableDefs(lngTable).Attributes And DB_SYSTEMOBJECT) Or _
        (dbDB.TableDefs(lngTable).Attributes And DB_HIDDENOBJECT))) = 0 Then
        strExportTableName = GetTranslatedTable( _
            dbDB.TableDefs(lngTable).Name)
            
            For Each idxIndex In dbDB.TableDefs(lngTable).Indexes
                'Index fields:
                strSQLCode = ""

                ' Output index field names:
                For Each fldField In idxIndex.Fields
                    strTemp = IndexTargetFieldDataType( _
                        DB_ENGINE, _
                        dbDB.TableDefs(lngTable), _
                        fldField _
                        )
                        Select Case DB_ENGINE
                            Case "FIREBIRD"
                                ' We cannot index on blobs...
                                If Left$(strTemp, Len("BLOB")) <> "BLOB" Then
                                    strSQLCode = strSQLCode & _
                                        IIf(strSQLCode = "", "", ", ") & _
                                        TranslateName( _
                                            fldField.Name, _
                                            idxIndex.Name, _
                                            "IndexField", _
                                            DB_ENGINE) & _
                                        IIf(fldField.Attributes And DAO.dbDescending, " DESC", "")
                                Else
                                    Warn "BLOB indexes in Firebird are not supported. Could not set index on field " & fldField.Name & " in table " & dbDB.TableDefs(lngTable).Name
                                End If
                        Case Else ' for all other dbs - note possible duplication with mysql
                            strSQLCode = strSQLCode & _
                                IIf(strSQLCode = "", "", ", ") & _
                                TranslateName( _
                                    fldField.Name, _
                                    idxIndex.Name, _
                                    "IndexField", _
                                    DB_ENGINE) & _
                                IIf(fldField.Attributes And DAO.dbDescending, " DESC", "")
                        End Select
                Next fldField
                
                ' Only continue if we have some valid fields:
                If strSQLCode <> "" Then
                    ' We'll deal with primary and foreign key constraints further below.
                    ' Regular indices (can be unique):
                    If idxIndex.Primary = False And idxIndex.Foreign = False Then
                    ' Simple index, but can be unique. We use a combination of
                    ' table and index name to try and generate a unique index name.
                        Select Case DB_ENGINE
                            Case "ACCESS"
                                Print #1, "CREATE " & IIf(idxIndex.Unique, "UNIQUE ", "") & "INDEX " & _
                                    idxIndex.Name & _
                                    " ON " & _
                                    strExportTableName & " (" & strSQLCode & "); " & m_QuerySep
                            Case "FIREBIRD", "MSSQL", "SYBASE"
                                Print #1, "CREATE " & IIf(idxIndex.Unique, "UNIQUE ", "") & "INDEX " & _
                                    TranslateName( _
                                        strExportTableName & idxIndex.Name, _
                                        strExportTableName, _
                                        "Index", _
                                        DB_ENGINE, _
                                        PREFIX_ON_INDEX, _
                                        SUFFIX_ON_INDEX) & _
                                    " ON " & _
                                    strExportTableName & " (" & strSQLCode & "); " & m_QuerySep
                            Case Else
                                Warn "Index " & idxIndex.Name & " may be lost (non-primary, non foreign index. todo: update code."
                        End Select
                    ElseIf idxIndex.Primary = True Then
                        ' Primary key
                        Select Case DB_ENGINE
                        Case "FIREBIRD"
                            ' In Firebird, index names must be unique across the database.
                            ' In Access, it's unique within a table.
                            ' Check to see whether our proposed combination already exists:
                            If GetTranslatedObject( _
                                dbDB.TableDefs(lngTable).Name & idxIndex.Name, _
                                "", _
                                "Index", _
                                DB_ENGINE) = "" Then
                                ' Note that we use Index for our translation scope, as it's close enough
                                ' to an index.
                                '
                                Print #1, "ALTER TABLE " & strExportTableName & " ADD CONSTRAINT " & _
                                TranslateName( _
                                    dbDB.TableDefs(lngTable).Name & idxIndex.Name, _
                                    strExportTableName, _
                                    "Index", _
                                    DB_ENGINE, _
                                    PREFIX_ON_CONSTRAINT, _
                                    SUFFIX_ON_CONSTRAINT) & _
                                " PRIMARY KEY" & _
                                " (" & strSQLCode & ");" & m_QuerySep
                            Else 'The name already exists so we need to make something up
                                Print #1, "ALTER TABLE " & strExportTableName & " ADD CONSTRAINT " & _
                                TranslateName( _
                                    dbDB.TableDefs(lngTable).Name & CInt(1 + Rnd() * 9) & CInt(1 + Rnd() * 9) & idxIndex.Name, _
                                    strExportTableName, _
                                    "Index", _
                                    DB_ENGINE, _
                                    PREFIX_ON_CONSTRAINT, _
                                    SUFFIX_ON_CONSTRAINT) & _
                                " PRIMARY KEY" & _
                                " (" & strSQLCode & ");" & m_QuerySep
                            End If
                        Case "MSSQL"
                            ' Index names must probably be unique across the database,
                            ' so we make up a name. In Access, it's unique within a table.
                            ' Note that we use Index for our translation scope, as it's close enough
                            ' to an index.
                            ' Check to see whether our proposed combination already exists:
                            If GetTranslatedObject( _
                                dbDB.TableDefs(lngTable).Name & idxIndex.Name, _
                                "", _
                                "Index", _
                                DB_ENGINE) = "" Then
                                Print #1, "ALTER TABLE " & strExportTableName & " ADD CONSTRAINT " & _
                                TranslateName( _
                                     dbDB.TableDefs(lngTable).Name & idxIndex.Name, _
                                     strExportTableName, _
                                    "Index", _
                                    DB_ENGINE, _
                                    PREFIX_ON_CONSTRAINT, _
                                    SUFFIX_ON_CONSTRAINT) & _
                                " PRIMARY KEY CLUSTERED " & _
                                " (" & strSQLCode & ");" & m_QuerySep
                            Else 'The name already exists so we need to make something up
                                Print #1, "ALTER TABLE " & strExportTableName & " ADD CONSTRAINT " & _
                                TranslateName( _
                                     dbDB.TableDefs(lngTable).Name & CInt(1 + Rnd() * 9) & CInt(1 + Rnd() * 9) & idxIndex.Name, _
                                     strExportTableName, _
                                    "Index", _
                                    DB_ENGINE, _
                                    PREFIX_ON_CONSTRAINT, _
                                    SUFFIX_ON_CONSTRAINT) & _
                                " PRIMARY KEY CLUSTERED " & _
                                " (" & strSQLCode & ");" & m_QuerySep
                            End If
                        Case "MY" 'MySQL
                            ' We've created the primary keys when we created
                            ' the table, so do nothing here.
                        Case Else
                            ' All other databases.
                            ' Index names must probably be unique across the database,
                            ' so we make up a name. In Access, it's unique within a table.
                            ' Check to see whether our proposed combination already exists:
                            If GetTranslatedObject( _
                                dbDB.TableDefs(lngTable).Name & idxIndex.Name, _
                                "", _
                                "Index", _
                                DB_ENGINE) = "" Then
                                Print #1, "ALTER TABLE " & strExportTableName & " ADD CONSTRAINT " & _
                                TranslateName( _
                                     dbDB.TableDefs(lngTable).Name & CInt(1 + Rnd() * 9) & CInt(1 + Rnd() * 9) & idxIndex.Name, _
                                     strExportTableName, _
                                    "Index", _
                                    DB_ENGINE, _
                                    PREFIX_ON_CONSTRAINT, _
                                    SUFFIX_ON_CONSTRAINT) & _
                                " PRIMARY KEY " & _
                                " (" & strSQLCode & ");" & m_QuerySep
                            Else
                                Print #1, "ALTER TABLE " & strExportTableName & " ADD CONSTRAINT " & _
                                TranslateName( _
                                     dbDB.TableDefs(lngTable).Name & idxIndex.Name, _
                                     strExportTableName, _
                                    "Index", _
                                    DB_ENGINE, _
                                    PREFIX_ON_CONSTRAINT, _
                                    SUFFIX_ON_CONSTRAINT) & _
                                " PRIMARY KEY " & _
                                " (" & strSQLCode & ");" & m_QuerySep
                            End If
                        End Select
                    End If 'idxIndex.Primary
                End If 'strSQLCode
            Next idxIndex
    End If 'If (((dbDB.TableDefs(lngTable).Attributes And DB_SYSTEMOBJECT) ...
Next lngTable

' Foreign keys need to be extracted from relationships.
For Each relRelation In dbDB.Relations
            strSQLCode = ""
            strOurFieldsList = ""
            strCascadeUpdate = ""
            
            If (relRelation.Attributes And DAO.dbRelationDontEnforce) = 0 Then
                ' Only convert real foreign key relationships
                For i = 0 To relRelation.Fields.Count - 1
                ' We're really dealing with table field names, not
                ' relation field names....
                    strSQLCode = strSQLCode & _
                        IIf(strSQLCode = "", "", ",") & _
                        TranslateName( _
                            relRelation.Fields(i).Name, _
                            relRelation.Table, _
                            "TableField", _
                            DB_ENGINE) 'Note: relation name, not foreign name property!
                    strOurFieldsList = strOurFieldsList & _
                        IIf(strOurFieldsList = "", "", ",") & _
                        TranslateName( _
                            relRelation.Fields(i).ForeignName, _
                            relRelation.ForeignTable, _
                            "TableField", _
                            DB_ENGINE)
                Next i
                strSQLCode = TranslateName(relRelation.Table, _
                    "Database", _
                    "Table", _
                    DB_ENGINE) & _
                    " (" & strSQLCode & ")"
                If (relRelation.Attributes And DAO.dbRelationDeleteCascade) <> 0 Then
                    strCascadeDelete = " ON DELETE CASCADE "
                Else
                    strCascadeDelete = ""
                End If
                If (relRelation.Attributes And DAO.dbRelationUpdateCascade) <> 0 Then
                    strCascadeUpdate = " ON UPDATE CASCADE "
                Else
                    strCascadeUpdate = ""
                End If
                ' Note the strange use of foreign table etc in Access.
                Select Case DB_ENGINE
                    Case "MY"
                        Print #1, _
                            "ALTER TABLE " & _
                            TranslateName( _
                                relRelation.ForeignTable, _
                                "Database", _
                                "Table", _
                                DB_ENGINE) & _
                            " ADD CONSTRAINT " & _
                            TranslateName( _
                                 relRelation.Name, _
                                 "Database", _
                                "Relation", _
                                DB_ENGINE, _
                                PREFIX_ON_CONSTRAINT, _
                                SUFFIX_ON_CONSTRAINT) & _
                            " FOREIGN KEY " & TranslateName( _
                                 relRelation.Name, _
                                 "Database", _
                                "Relation", _
                                DB_ENGINE, _
                                PREFIX_ON_CONSTRAINT, _
                                SUFFIX_ON_CONSTRAINT) & _
                            " (" & strOurFieldsList & ") REFERENCES " & _
                            strSQLCode & strCascadeDelete & strCascadeUpdate & "; " & _
                            m_QuerySep
                    Case Else ' All other dbs
                        ' We force database-wide unique constraint names.
                        ' Check to see whether our proposed combination already exists:
                        If GetTranslatedObject( _
                            strName:=relRelation.Name, _
                            strParentName:="", _
                            strScope:="Index", _
                            DB_ENGINE:=DB_ENGINE) = "" Then
                            Print #1, _
                                "ALTER TABLE " & _
                                TranslateName( _
                                    relRelation.ForeignTable, _
                                    "Database", _
                                    "Table", _
                                    DB_ENGINE) & _
                                " ADD CONSTRAINT " & _
                                TranslateName( _
                                     relRelation.Name, _
                                     "Database", _
                                    "Relation", _
                                    DB_ENGINE, _
                                    PREFIX_ON_CONSTRAINT, _
                                    SUFFIX_ON_CONSTRAINT) & _
                                " FOREIGN KEY " & _
                                " (" & strOurFieldsList & ") REFERENCES " & _
                                strSQLCode & strCascadeDelete & strCascadeUpdate & "; " & _
                                m_QuerySep
                        Else ' Constraint name already exists
                            Print #1, _
                                "ALTER TABLE " & _
                                TranslateName( _
                                    relRelation.ForeignTable, _
                                    "Database", _
                                    "Table", _
                                    DB_ENGINE) & _
                                " ADD CONSTRAINT " & _
                                TranslateName( _
                                     relRelation.Name & CInt(1 + Rnd() * 9) & CInt(1 + Rnd() * 9), _
                                     "Database", _
                                    "Relation", _
                                    DB_ENGINE, _
                                    PREFIX_ON_CONSTRAINT, _
                                    SUFFIX_ON_CONSTRAINT) & _
                                " FOREIGN KEY " & _
                                " (" & strOurFieldsList & ") REFERENCES " & _
                                strSQLCode & strCascadeDelete & strCascadeUpdate & "; " & _
                                m_QuerySep
                        
                        End If
                End Select
            End If '(relRelation.Attributes And DAO.dbRelationDontEnforce) = 0
Next relRelation
' Finally output the delayed definitions recorded
' in the ExportStructure procedure:
Print #1, strPostDataImportDDL

On Error GoTo 0
Exit Sub

ExportStructureAfterData_Error:
If Erl > 0 Then
    ' Program uses line numbers
    MsgBox "Error: " & Err.Number & vbCrLf & _
        Err.Description & vbCrLf & _
        "in line " & Erl & " of procedure ExportStructureAfterData of Module basExportImportSQL"
Else
    MsgBox "Error: " & Err.Number & vbCrLf & _
        Err.Description & vbCrLf & _
        "in procedure ExportStructureAfterData of Module basExportImportSQL"
End If
Debug.Print Now() & " - ExportStructureAfterData_Error fatal error: " & Err.Description & " (Error number: " & Err.Number & ") at line " & Erl
End
End Sub
Private Function Field2CSV(OutputField As DAO.Field, _
    strDelimiter As String, _
    Optional strFieldQuote As String = "", _
    Optional strEscape As String = "") As String
' Gives formatted field data.
Dim strTemp As String
On Error GoTo Field2CSV_Error
If IsNull(OutputField.Value) Then
    Field2CSV = Null2CSV(OutputField, DB_ENGINE)
    '"" 'Nothing.
    Exit Function
End If
Select Case OutputField.Type
    Case DAO.dbBoolean
        strTemp = Boolean2SQL(OutputField.Value, DB_ENGINE, False)
    Case DAO.dbChar, DAO.dbText, DAO.dbMemo, _
      DAO.dbGUID
        strTemp = CStr(OutputField.Value)
        'Embedded quote characters
        If strEscape = "" Then
            'Replace with double quote
            strTemp = Replace(strTemp, strFieldQuote, strFieldQuote & strFieldQuote)
        Else
            strTemp = Replace(strTemp, strFieldQuote, strEscape & strFieldQuote)
        End If
        strTemp = strFieldQuote & strTemp & strFieldQuote
    Case DAO.dbBinary, DAO.dbLongBinary, DAO.dbVarBinary
        ' Arbitrary decision to encode to base64, but we can hardly
        ' leave all kind of control characters here.
        strTemp = EncodeBase64(OutputField.Value)
        If InStr(strTemp, strFieldQuote) > 0 Then
            ' We need to quote and escape this field
            strTemp = strFieldQuote & strTemp & strFieldQuote
        End If
    Case DAO.dbDate, DAO.dbTimeStamp
          strTemp = Format$(OutputField.Value, "YYYY-MM-DD HH:MM:SS")
    Case DAO.dbTime
          strTemp = Format$(OutputField.Value, "HH:MM:SS")
    Case DAO.dbBigInt, DAO.dbByte, DAO.dbInteger, DAO.dbLong
        strTemp = Trim$(CStr(OutputField.Value))
    Case DAO.dbCurrency, DAO.dbDecimal, DAO.dbFloat, DAO.dbNumeric, DAO.dbSingle
      strTemp = Replace(Trim(CStr(OutputField.Value)), ",", ".")
    Case Else
      Warn "Unknown data type (code: " & OutputField.Type & ") for field " & _
      OutputField.Name & ". Used a default conversion.", warning
      strTemp = OutputField.Value
      strTemp = strFieldQuote & strTemp & strFieldQuote
End Select

' Embedded delimiters:
If strEscape = "" Then
    'Replace with double delimiter
    strTemp = Replace(strTemp, strDelimiter, strDelimiter & strDelimiter)
Else
    strTemp = Replace(strTemp, strDelimiter, strEscape & strDelimiter)
End If
Field2CSV = strTemp

On Error GoTo 0
Exit Function

Field2CSV_Error:
' We hope we can continue, but there's probably data lost.
Dim Errno As Long: Dim ErrDesc As String
Errno = Err.Number
ErrDesc = Err.Description & " (at program line: " & Erl & ")"
Warn "Field2CSV: error: " & ErrDesc & ". Data may be lost.", warning
Debug.Print Now() & " - Field2CSV: error: " & ErrDesc & " (Error number: " & Errno & "). Data may be lost."
Field2CSV = ""

End Function

Private Function Field2SQL(OutputField As DAO.Field, _
    DB_ENGINE As String _
) As String
' Converts value of a field into relevant SQL snippet
' for target database
'todo: use translatefield function to return an array of:
' field name, target field type, target field size.
' use mapping to determine outputfield=>parent table=>
' => mapping => translated field => target field size and type
' dependent on this, check max size.
' very complicated, should be much easier!
On Error GoTo ConvError:
Dim strTemp As String
' Always return NULL if field is null,
' regardless of field type
If IsNull(OutputField.Value) Then
    Field2SQL = "NULL"
    Exit Function
End If
Select Case OutputField.Type
    Case DAO.dbBoolean
        strTemp = Boolean2SQL(OutputField.Value, DB_ENGINE, PG_MS_ACCESS_LINK_COMPATIBLE)
    Case DAO.dbChar, DAO.dbText, DAO.dbMemo
        strTemp = String2SQL(OutputField.Value, DB_ENGINE)
    Case DAO.dbBinary, DAO.dbLongBinary, DAO.dbVarBinary
        strTemp = Binary2SQL(OutputField.Value, DB_ENGINE)
    Case DAO.dbGUID
        strTemp = GUID2SQL(OutputField.Value, DB_ENGINE)
    Case DAO.dbDate, DAO.dbTimeStamp
        strTemp = Date2SQL(OutputField.Value, DB_ENGINE)
    Case DAO.dbTime
        strTemp = Time2SQL(OutputField.Value, DB_ENGINE)
    Case DAO.dbBigInt, DAO.dbByte, DAO.dbInteger, DAO.dbLong
        ' Just dump the value. Nothing special
        ' We might have overflow issues though.
        ' If so, truncate value and show a warning.
        strTemp = Trim(CStr(OutputField.Value))
    Case DAO.dbCurrency, DAO.dbDecimal, DAO.dbFloat, DAO.dbNumeric, DAO.dbSingle
        strTemp = Float2SQL(OutputField.Value, DB_ENGINE)
    Case Else 'other field type
        strTemp = CStr(OutputField.Value)
        Warn "Unknown data type (code: " & OutputField.Type & ") for field " & _
        OutputField.Name & ". Used a default conversion.", warning
        'Keep strTemp
End Select
Field2SQL = strTemp
Exit Function

ConvError:
' could be a fatal error, but possibly not. Log & ignore
Dim ErrDesc As String, Errno As Long
Errno = Err.Number
ErrDesc = Err.Description & " (at program line: " & Erl & ")"
Warn "Field2SQL: conversion error: " & ErrDesc
Debug.Print Now() & " - Field2SQL: Conversion error: " & ErrDesc & " (Error number: " & Errno & ")"
Field2SQL = ""
End Function

Private Function Float2SQL(Output As Variant, _
    DB_ENGINE As String)
' Although we can mostly use field2sql, sometimes we don't
' have a field value that we want to convert, but a regular
' variable.
' Take care to synchronize this code with the main code
' in Field2SQL
On Error GoTo ConvError:
If IsNull(Output) Then
    Float2SQL = "NULL"
    Exit Function
End If
' Replace decimal , with decimal .
' Depending on Windows country settings, we could even
' have other separators....
Float2SQL = Replace(Trim(Output), ",", ".")
Exit Function

ConvError:
' could be a fatal error, but possibly not. Log & ignore
Dim ErrDesc As String, Errno As Long
Errno = Err.Number
ErrDesc = Err.Description & " (at program line: " & Erl & ")"
Warn "Float2SQL: conversion error: " & ErrDesc
Debug.Print Now() & " - Float2SQL: Conversion error: " & ErrDesc & " (Error number: " & Errno & ")"
Float2SQL = ""
End Function

Private Function GetDescription(ByVal obj As Object) As String
' Get description of an object if it exists. Otherwise: nothing
Dim strDescription As String
strDescription = ""
On Error Resume Next
strDescription = obj.Properties("Description")
On Error GoTo 0
GetDescription = strDescription
End Function

Private Function GetFieldList( _
    rst As DAO.Recordset, _
    strTableName As String, _
    DB_ENGINE As String) As String
' Builds a list of translated fields in order. Used
' to generate insert statements where we specify the column names.
' strTableName is the name of the parent table from which the recordset
' must be derived: therefore no support for joins between various tables.
Dim strList As String
Dim strField As String
Dim lngFieldCount As Long
For lngFieldCount = 1 To rst.Fields.Count
    'Note that in DAO, we start numbering collections from 0, not 1.
    strField = GetTranslatedField(rst.Fields(lngFieldCount - 1).Name, strTableName)
    If strField = "" Then
        Err.Raise vbObjectError + 237, "GetFieldList", _
            "Cannot find translated field name for field: " & _
            rst.Fields(lngFieldCount - 1).Name & _
            " in table " & strTableName & ". Please fix the code. Aborting."
    End If
    If lngFieldCount = 1 Then
        Select Case DB_ENGINE
            Case Else
                strList = strField
        End Select
    Else
        Select Case DB_ENGINE
            Case Else
                strList = strList & ", " & strField
        End Select
    End If
Next
GetFieldList = strList
End Function

Private Function GetTranslatedObject(ByVal strName As String, _
    ByVal strParentName As String, _
    ByVal strScope As String, _
    ByVal DB_ENGINE As String) As String
' Returns previously translated name, given object name in current db.
' If strParentName or strScope are empty, the code will ignore these fields when matching.
On Error GoTo GetTranslatedObjectError:
Dim TranslationItem As Variant 'Will contain 0-based string array: name scope, parent name, original name, new name, change indication
Dim Translation As String
Dim i As Long
Translation = ""

' We can't use a collection lookup by key because the key contains the translated name, too.
For Each TranslationItem In NameTranslations
    If TranslationItem(2) = strName Then 'object name
        If (TranslationItem(1) = strParentName) Or strParentName = "" Then 'parent name, e.g. a table name or not specified
            If (TranslationItem(0) = strScope) Or strScope = "" Then 'scope
                Translation = TranslationItem(3)
                Exit For
            End If
        End If
    End If
Next
GetTranslatedObject = Translation
Exit Function

GetTranslatedObjectError:
' could be a fatal error, but possibly not. Log & ignore
Dim ErrDesc As String, Errno As Long
Errno = Err.Number
ErrDesc = Err.Description & " (at program line: " & Erl & ")"
Warn "GetTranslatedObject: error: " & ErrDesc
Debug.Print Now() & " - GetTranslatedObject: error: " & ErrDesc & " (Error number: " & Errno & ")"
GetTranslatedObject = ""
End Function

Private Function GetTranslatedTable(strTableName As String) As String
' Returns previously translated table name, given current table name.
GetTranslatedTable = GetTranslatedObject(strTableName, "Database", "Table", DB_ENGINE)
End Function

Private Function GetTranslatedField(strFieldName As String, strTableName As String) As String
' Returns previously translated table name, given current table name.
GetTranslatedField = GetTranslatedObject(strFieldName, _
    strTableName, _
    "TableField", _
    DB_ENGINE)
End Function
Private Function GUID2SQL(VarGUID As Variant, _
                        DB_ENGINE As String) _
                        As String
Dim strTemp As String
On Error GoTo ConvError:
strTemp = CStr(VarGUID)
Select Case DB_ENGINE
Case "ACCESS", "CSV"
    ' Keep the input as-is
Case "FIREBIRD"
    'for now just keep the input
    ' suggest storing it in char field.
    Warn "todo: implement guid conversion for this database"
Case "MSSQL", "SYBASE", "SQL2003", "SQLITE"
    'for now just keep the input
    Warn "todo: implement guid conversion for this database"
Case "MY", "M1", "M2"  'mysql
    strTemp = "CONCAT(UNHEX(LEFT(" & strTemp & ",8)),UNHEX(MID( & strTemp & ,10,4)),UNHEX(MID( & strTemp & ,15,4)),UNHEX(MID( & strTemp & ,20,4)),UNHEX(RIGHT( & strTemp & ,12)))"
    'for storage as binary(16).
    'FYI, the opposite (binary16=>text):
    'RETURN concat(HEX(LEFT(uuid,4)),'-', HEX(MID(uuid,5,2)),'-', HEX(MID(uuid,7,2)),'-',HEX(MID(uuid,9,2)),'-',HEX(RIGHT(uuid,6)));
Case "ORACLE"
    'for now just keep the input
    Warn "todo: implement guid conversion for this database"
Case "PG" 'Postgres
    'for now just keep the input
    'should be 36 char uuid, e.g. 'a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11
    Warn "todo: verify guid conversion for this database works", remark
Case Else
    'for now just keep the input
    Warn "Could not find database specific conversion for GUID fields. Used a default conversion. Data may come out wrong."
End Select
GUID2SQL = strTemp
Exit Function

ConvError:
' could be a fatal error, but possibly not. Log & ignore
Dim ErrDesc As String, Errno As Long
Errno = Err.Number
ErrDesc = Err.Description & " (at program line: " & Erl & ")"
Warn "GUID2SQL: conversion error: " & ErrDesc
Debug.Print Now() & " - GUID2SQL: Conversion error: " & ErrDesc & " (Error number: " & Errno & ")"
GUID2SQL = ""
End Function
Private Function ExecuteSQL(strSQL As String, _
    Optional dbDB As DAO.Database = Nothing) As String
Dim Errno As Long
Dim ErrDesc As String
If Trim(strSQL) = "" Then
    ' Just don't execute empty strings. There's no point.
    ExecuteSQL = ""
    Exit Function
End If

If dbDB Is Nothing Then
    ' This could only work if we're exporting our own database
    'Set dbDB = CurrentDB
    Stop 'we need to fix this.
End If

On Error Resume Next
dbDB.Execute strSQL, DAO.dbFailOnError
' Log errors:
Errno = Err.Number
ErrDesc = Err.Description
If Errno > 0 Then
    ' Assume this is a database error:
    ExecuteSQL = _
        "Error running SQL statement: " & vbCrLf & _
        strSQL & vbCrLf & _
        "The error was: " & vbCrLf & _
        Errno & " - " & ErrDesc
Else
    ExecuteSQL = ""
End If
End Function

' Reinier Olislagers, June 2008: explicit definition of DAO identifiers.
' importSQL version 1.0
' www.netdive.com/freebies/importsql/
'
' (C) 1998 NetDIVE - www.netdive.com
'
' (C) Laurent Bossavit - laurent@netdive.com  (do not add to mailing lists without permission)
'
' This code is free for commercial and non-commercial use without permission. However, one
' restriction applies : charging for a service that makes use of this code is OK, but selling
' this code, with or without modifications, is not. You may NOT sell this as part of a non-free
' package. Also, please report any bugs, significant improvements, or helpful comments to
' the author.
'
' ACKNOWLEDGEMENTS
'   To Pedro Freire from Cynergi (http://www.cynergi.net), who created exportSQL, after
'   which importSQL is patterned - down to the format of this documentation. ;)
'
' MODULE
'   "importSQL"
'
' GOAL
'   Import data into an Access database from a MySQL database via ODBC. The
'   MySQL database name used is the title of the Access database, converted
'   to lowercase; for all tables present in the Access database, the records
'   from the table of the same name in MySQL are imported. Tables which do not
'   exist in the MySQL database are left alone; for all others, existing records
'   are DELETEd first. On execution, you will be queried for the host machine
'   where your MySQL strServer is located.
'
'   Combined with exportSQL, this is extremely handy : you can use Access to
'   design your database, run exportSQL to create a MySQL database, add data
'   to it. If you ever need to change some fields, add tables, etc. just run
'   importSQL to retrieve your data, tweak the database to your heart's content,
'   then use exportSQL again to update the data.
'
' HOW TO USE
'   Copy-and-paste this text file into an Access module and run the importSQL
'   subroutine. in more detail, you:
'   * Open the Access .mdb file you wish to synchronize
'   * in the default database objects window, click on "Modules", and then on "New"
'   * The code window that opens has some pre-written text (code). Delete it.
'   * Copy-and-paste this entire file to the code module window
'   * You may hit the compile button (looks like 3 sheets of paper with an arrow on
'     top of them, pressing down on them), or select Debug, Compile Loaded Modules
'     from the top menu, just to make sure there are no errors, and that this code
'     works on your Access version (it works on Access'97 and should work on Access'95)
'   * Close the code module window - windows will prompt you to save the code:
'     answer "Yes", and when promped for a name for the module, type anything
'     (say, "importSQL")
'   The module is now part of your Access database. To run the import, you:
'   * Re-open the code module (by double-clicking on it, or clicking "Design"
'     with it selected). Move the cursor to where the Public Sub importSQL() keywords appear.
'     Press F5 or select Run, Go/Continue from the top menu.
' If SetDefaultOptions is set false, module level variables representing export options won't be set to the values
' specified in the constants. This allows GUIs etc. to set the variables/options themselves
Public Sub importSQL(Optional SetDefaultOptions As Boolean = True)
On Error GoTo importSQL_error
Dim strServer As String
Dim strErrors As String
Dim dbCDB As DAO.Database
Dim idxTableIndex As Integer
Dim tdfTable As TableDef
Dim strTableName As String
Dim wsConnection As Workspace
Dim itsdb As DAO.Database
Dim intIndexCount As Integer
Dim strDBS As String
Dim strFirstField As String
Dim cnt As DAO.Container
Dim doc As DAO.Document
Dim strTit As String
Dim dbsExtDB As DAO.Database
Dim qdfTempRemote As DAO.QueryDef
Dim intFieldIndex As Integer
Dim intFieldCount As Integer
Dim qryRemote As DAO.QueryDef
'Dim rstRemote As DAO.Recordset 'apparently not used.
InitializeModuleVars (SetDefaultOptions)
Set dbCDB = CurrentDb()
Set wsConnection = CreateWorkspace("MySQL", "", "", dbUseODBC)

strServer = InputBox("Server to import from ?")

'Go through the table definitions
For idxTableIndex = 0 To dbCDB.TableDefs.Count - 1

    ' Let's take only the visible tables
    If (((dbCDB.TableDefs(idxTableIndex).Attributes And DB_SYSTEMOBJECT) Or _
    (dbCDB.TableDefs(idxTableIndex).Attributes And DB_HIDDENOBJECT))) = 0 Then

        Set tdfTable = dbCDB.TableDefs(idxTableIndex)
        strTableName = tdfTable.Name
        strFirstField = tdfTable.Fields(0).Name

        intIndexCount = InStr(tdfTable.Connect, "DATABASE=")
        strDBS = Mid$(tdfTable.Connect, intIndexCount + 9)
        Set itsdb = Workspaces(0).OpenDatabase(strDBS)
        Set cnt = itsdb.Containers!Databases
        Set doc = cnt.Documents!SummaryInfo
        strTit = doc.Properties!Title
        strTit = Format$(strTit, "<")

        ' This is just to make sure the connection won't pop up many ODBC dialogs
        Set dbsExtDB = wsConnection.OpenDatabase("MySQL", dbDriverNoPrompt, False, "ODBC;DSN=MySQL;DATABASE=" + strTit + ";USER=;PASSWORD=;PORT=3306;OPTIONS=0;SERVER=" + strServer + ";")

        On Error Resume Next
        Set qdfTempRemote = dbCDB.CreateQueryDef("Remote_" + strTableName, "SELECT * FROM " + strTableName)
        Set qryRemote = dbCDB.QueryDefs("Remote_" + strTableName)
        qryRemote.Connect = "ODBC;DSN=MySQL;DATABASE=" + strTit + ";USER=;PASSWORD=;PORT=3306;OPTIONS=0;SERVER=" + strServer + ";"
        qryRemote.SQL = "SELECT * FROM " + strTableName + " ORDER BY " + strFirstField

        On Error GoTo recordError

        ' Apparently MySQL's ODBC driver reports zero-length strings as nulls,
        ' which is baaaad. This lets us avoid validation errors due to that
        intFieldCount = itsdb.TableDefs(strTableName).Fields.Count
        ReDim notNulls(intFieldCount) As Boolean
        For intFieldIndex = 0 To intFieldCount - 1
            notNulls(intFieldIndex) = itsdb.TableDefs(strTableName).Fields(intFieldIndex).Required
            itsdb.TableDefs(strTableName).Fields(intFieldIndex).Required = False
        Next intFieldIndex

        ' Try executing the query once, this will prevent our deleting data in new
        ' tables that are not on the remote database
        qryRemote.OpenRecordset
        ' If the query can be executed, delete current data and import
        dbCDB.Execute ("DELETE FROM " + strTableName)
        dbCDB.Execute ("INSERT INTO " + strTableName + " SELECT * FROM Remote_" + strTableName)

recorded:
        On Error GoTo importSQL_error

        qryRemote.Close
        dbsExtDB.Close

        dbCDB.QueryDefs.Delete (qryRemote.Name)

        For intFieldIndex = 0 To intFieldCount - 1
            itsdb.TableDefs(strTableName).Fields(intFieldIndex).Required = notNulls(intFieldIndex)
        Next intFieldIndex

    End If

Next idxTableIndex

If Not (strErrors = "") > 0 Then
    MsgBox "There were errors " + strErrors
End If


importSQL_exit:
dbCDB.Close
Set dbCDB = Nothing
DoCmd.Hourglass False
Exit Sub

importSQL_error:
MsgBox Err.Description
Resume importSQL_exit

recordError:
strErrors = strErrors + " [" + Err.Description + " (" + strTableName + ")]"
Resume recorded
End Sub


Function IndexTargetFieldDataType( _
    TargetEngine As String, _
    tdfMother As DAO.TableDef, _
    fldIndexField As DAO.Field) As String
' Given an index field, determine what field matches in the target database.
Dim fldSource As New DAO.Field
Dim strTargetType As String
Set fldSource = tdfMother.Fields(fldIndexField.Name)
strTargetType = TranslateFieldType( _
    TargetEngine, _
    fldSource)
Set fldSource = Nothing
IndexTargetFieldDataType = strTargetType
End Function
Public Sub SetOptionsToDefault()
    ' Set up module level variables according to defaults
    COMMENT_PREFIX = DEF_COMMENT_PREFIX
    CSV_DELIMITER = DEF_CSV_DELIMITER
    CSV_ESCAPE = DEF_CSV_ESCAPE
    CSV_QUOTE = DEF_CSV_QUOTE
    DB_CONNECT = DEF_DB_CONNECT
    DB_ENGINE = DEF_DB_ENGINE
    DB_NAME = DEF_DB_NAME
    DISPLAY_WARNINGS = DEF_DISPLAY_WARNINGS
    EXPORT_DATA = DEF_EXPORT_DATA
    EXPORT_DELETE_EXISTING_STRUCTURE = DEF_EXPORT_DELETE_EXISTING_STRUCTURE
    EXPORT_QUERIES = DEF_EXPORT_QUERIES
    EXPORT_STRUCTURE = DEF_EXPORT_STRUCTURE
    FB_BINARY_OUTPUT = DEF_FB_BINARY_OUTPUT
    FB_WHICH_TOOL = DEF_FB_WHICH_TOOL
    FILE_DIRECTORY = DEF_FILE_DIRECTORY
    FORMS_EXPORT = DEF_FORMS_EXPORT
    FORMS_EXPORT_FORMAT = DEF_FORMS_EXPORT_FORMAT
    INDENT_SIZE = DEF_INDENT_SIZE
    INSERT_QUERY_SEPARATOR_EVERY = DEF_INSERT_QUERY_SEPARATOR_EVERY
    MAN_IDENT_MAX_SIZE = DEF_MAN_IDENT_MAX_SIZE
    MSQL_64kb_AVG = DEF_MSQL_64kb_AVG
    MY_ENGINE_TYPE = DEF_MY_ENGINE_TYPE
    PARA_INSERT_AFTER = DEF_PARA_INSERT_AFTER
    PG_MS_ACCESS_LINK_COMPATIBLE = DEF_PG_MS_ACCESS_LINK_COMPATIBLE
    PREFIX_ON_CONSTRAINT = DEF_PREFIX_ON_CONSTRAINT
    PREFIX_ON_INDEX = DEF_PREFIX_ON_INDEX
    PREFIX_ON_KEYWORD = DEF_PREFIX_ON_KEYWORD
    PREFIX_ON_SEQUENCE = DEF_PREFIX_ON_SEQUENCE
    PREFIX_ON_STOREDPROC = DEF_PREFIX_ON_STOREDPROC
    PREFIX_ON_TRIGGER = DEF_PREFIX_ON_TRIGGER
    PREFIX_ON_VIEW = DEF_PREFIX_ON_VIEW
    PRESERVE_CASE = DEF_PRESERVE_CASE
    SUFFIX_ON_CONSTRAINT = DEF_SUFFIX_ON_CONSTRAINT
    SUFFIX_ON_INDEX = DEF_SUFFIX_ON_INDEX
    SUFFIX_ON_KEYWORD = DEF_SUFFIX_ON_KEYWORD
    SUFFIX_ON_STOREDPROC = DEF_SUFFIX_ON_STOREDPROC
    SUFFIX_ON_VIEW = DEF_SUFFIX_ON_VIEW
    TRANSLATION_FILENAME = DEF_TRANSLATION_FILENAME
    WS_REPLACEMENT = DEF_WS_REPLACEMENT
End Sub
Sub InitializeModuleVars(SetDefaultOptions As Boolean)
' Initialize module-level variables. Optionally resets
' all relevant module-level variables to defaults as defined in top
' section of code. This is not useful if we're using a GUI to set the
' options, so we should disable it then
Dim strTemp As String

If SetDefaultOptions = True Then
    SetOptionsToDefault
End If
' Files:
OUTPUTFILE = FILE_DIRECTORY & "AccessExport.sql"
m_strInputFile = FILE_DIRECTORY & "AccessExport.sql" 'At least for testing,

' Fix translation file with full path
If TRANSLATION_FILENAME <> "" Then
    TRANSLATION_FILE = FILE_DIRECTORY & TRANSLATION_FILENAME
Else
    TRANSLATION_FILE = ""
    Debug.Print "No translation mapping file specified. Ok, continuing without file."
End If

' Default export db name:
Select Case DB_NAME
Case "" 'using current database
    strTemp = Mid$(CurrentDb.Name, InStrRev(CurrentDb.Name, "\") + 1)
Case Else 'We specified a different database
    strTemp = Mid$(DB_NAME, InStrRev(DB_NAME, "\") + 1)
End Select
'todo: we could do this for .accdb, .mde, ... etc, probably easier using instrrev
If Right$(strTemp, Len(".mdb")) = ".mdb" Then
    strTemp = Left$(strTemp, Len(strTemp) - Len(".mdb"))
End If
m_strExportDBName = strTemp
m_fPreserveCase = PRESERVE_CASE 'By default, use user-supplied value

Select Case UCase(DB_ENGINE)
' Set sanitized db_engine
' Set terminator/separator of SQL queries (to instruct some monitor program to execute them).
' Warn if IDENT_MAX_SIZE is incorrect for db in question
    Case "ACCESS", "MSACCESS", "MS ACCESS", "MS_ACCESS", "JET", "MICROSOFTACCESS", "MICROSOFT ACCESS", "MICROSOFT_ACCESS", "MDB"
        DB_ENGINE = "ACCESS"
        m_QuerySep = vbCrLf
        If INSERT_QUERY_SEPARATOR_EVERY = -1 Then
            INSERT_QUERY_SEPARATOR_EVERY = 1
        Else
            INSERT_QUERY_SEPARATOR_EVERY = INSERT_QUERY_SEPARATOR_EVERY
        End If
        ' We preserve case here regardless of user setting
        m_fPreserveCase = True
        Select Case IDENT_MAX_SIZE
        Case Is < 64
            Warn "The IDENT_MAX_SIZE constant is set to " & IDENT_MAX_SIZE & ", while this database supports object names up to 64 characters. You might want to increase this for clearer, longer names."
        Case Is > 64
            Warn "The IDENT_MAX_SIZE constant is set to " & IDENT_MAX_SIZE & ", while this database supports object names up to 64 characters. You might want to decrease this as the database will probably error out on the script."
        End Select
        If MAN_IDENT_MAX_SIZE < 0 Then
            ' User has not overridden, so set database-specific length:
            IDENT_MAX_SIZE = 64
        End If
    Case "CSV"
        DB_ENGINE = "CSV"
        m_QuerySep = vbCrLf
        INSERT_QUERY_SEPARATOR_EVERY = 0 'Don't insert extraneous line feeds
        If MAN_IDENT_MAX_SIZE < 0 Then
            ' User has not overridden, so set database-specific length:
            IDENT_MAX_SIZE = 16384
        End If
    Case "DB2"
        DB_ENGINE = "DB2"
        m_QuerySep = vbCrLf
        INSERT_QUERY_SEPARATOR_EVERY = 0 'Don't insert extraneous line feeds
        If MAN_IDENT_MAX_SIZE < 0 Then
            ' User has not overridden, so set database-specific length:
            IDENT_MAX_SIZE = 128 'V8+, 18 for V7
        End If
    Case "DERBY", "JAVADB", "JAVA DB", "JAVA_DB"
        DB_ENGINE = "Derby"
        m_QuerySep = vbCrLf
        INSERT_QUERY_SEPARATOR_EVERY = 0 'Don't insert extraneous line feeds
        If MAN_IDENT_MAX_SIZE < 0 Then
            ' User has not overridden, so set database-specific length:
            IDENT_MAX_SIZE = 64
        End If
    Case "FIREBIRD"
        DB_ENGINE = "FIREBIRD"
        m_QuerySep = vbCrLf & "COMMIT;"
        If INSERT_QUERY_SEPARATOR_EVERY = -1 Then
            INSERT_QUERY_SEPARATOR_EVERY = 1000 'Optimum value for this database?
        Else
            INSERT_QUERY_SEPARATOR_EVERY = INSERT_QUERY_SEPARATOR_EVERY
        End If
        ' Default username/password for database
        If m_strDBUser = "" Then
            m_strDBUser = "SYSDBA" 'Default superuser/administrator account for Firebird/Interbase
        End If
        If m_strDBPassword = "" Then
            m_strDBPassword = "masterkey" 'default password for Firebird/Interbase
        End If
        m_strExportDBName = LCase(m_strExportDBName) & ".fdb" 'Firebird database extension by convention.
        m_fPreserveCase = False ' We cannot preserve case without extensive quoting
        If MAN_IDENT_MAX_SIZE < 0 Then
            ' User has not overridden, so set database-specific length:
            IDENT_MAX_SIZE = 31
        End If
    Case "M1"
        DB_ENGINE = "M1"
        m_QuerySep = vbCrLf
        If INSERT_QUERY_SEPARATOR_EVERY = -1 Then
            INSERT_QUERY_SEPARATOR_EVERY = 1
        Else
            INSERT_QUERY_SEPARATOR_EVERY = INSERT_QUERY_SEPARATOR_EVERY
        End If
        If MAN_IDENT_MAX_SIZE < 0 Then
            ' User has not overridden, so set database-specific length:
            IDENT_MAX_SIZE = 19
        End If
    Case "M2"
        DB_ENGINE = "M2"
        m_QuerySep = vbCrLf
        If INSERT_QUERY_SEPARATOR_EVERY = -1 Then
            INSERT_QUERY_SEPARATOR_EVERY = 1
        Else
            INSERT_QUERY_SEPARATOR_EVERY = INSERT_QUERY_SEPARATOR_EVERY
        End If
        If MAN_IDENT_MAX_SIZE < 0 Then
            ' User has not overridden, so set database-specific length:
            IDENT_MAX_SIZE = 19
        End If
    Case "MSSQL", "MICROSOFT SQL", "MICROSOFTSQL", "SQL2000", "SQL2005", "SQL2008", "SQL2K", "SQL2K5", "SQL2K8"
        DB_ENGINE = "MSSQL"
        m_QuerySep = vbCrLf & "GO"
        If INSERT_QUERY_SEPARATOR_EVERY = -1 Then
            INSERT_QUERY_SEPARATOR_EVERY = 1 'Required: low value to avoid
            ' errors on insert
        Else
            INSERT_QUERY_SEPARATOR_EVERY = INSERT_QUERY_SEPARATOR_EVERY
        End If
        If MAN_IDENT_MAX_SIZE < 0 Then
            ' User has not overridden, so set database-specific length:
            IDENT_MAX_SIZE = 128
        End If
    Case "MY", "MYSQL"
        DB_ENGINE = "MY"
        m_QuerySep = ""
        ' Doesn't make sense to perform commits as mysql probably
        ' doesn't support it.
        INSERT_QUERY_SEPARATOR_EVERY = 0 'Don't insert extraneous line feeds
        If MAN_IDENT_MAX_SIZE < 0 Then
            ' User has not overridden, so set database-specific length:
            IDENT_MAX_SIZE = 64
        End If
    Case "ORACLE"
        DB_ENGINE = "ORACLE"
        m_QuerySep = vbCrLf & "/"
        If INSERT_QUERY_SEPARATOR_EVERY = -1 Then
            INSERT_QUERY_SEPARATOR_EVERY = 1000
        Else
            INSERT_QUERY_SEPARATOR_EVERY = INSERT_QUERY_SEPARATOR_EVERY
        End If
        If MAN_IDENT_MAX_SIZE < 0 Then
            ' User has not overridden, so set database-specific length:
            IDENT_MAX_SIZE = 30
        End If
    Case "PG", "POSTGRESQL", "POSTGRES"
        DB_ENGINE = "PG"
        m_QuerySep = vbCrLf
        If INSERT_QUERY_SEPARATOR_EVERY = -1 Then
            INSERT_QUERY_SEPARATOR_EVERY = 1
        Else
            INSERT_QUERY_SEPARATOR_EVERY = INSERT_QUERY_SEPARATOR_EVERY
        End If
        If MAN_IDENT_MAX_SIZE < 0 Then
            ' User has not overridden, so set database-specific length:
            IDENT_MAX_SIZE = 64
        End If
    Case "SQLITE" 'Derived from Derby
        DB_ENGINE = "SQLITE"
        m_QuerySep = vbCrLf
        INSERT_QUERY_SEPARATOR_EVERY = 0 'Don't insert extraneous line feeds
        If MAN_IDENT_MAX_SIZE < 0 Then
            ' User has not overridden, so set database-specific length:
            IDENT_MAX_SIZE = 64
        End If
    Case "SQL2003" 'Derived from Derby
        DB_ENGINE = "SQL2003"
        m_QuerySep = vbCrLf
        INSERT_QUERY_SEPARATOR_EVERY = 0 'Don't insert extraneous line feeds
        If MAN_IDENT_MAX_SIZE < 0 Then
            ' User has not overridden, so set database-specific length:
            IDENT_MAX_SIZE = 255 'todo: what does the standard say?
        End If
    Case "SYBASE", "SYBASE12", "SYBASE15", "SYBASEASE", "SYBASE_ASE"
        DB_ENGINE = "SYBASE"
        m_QuerySep = vbCrLf & "GO"
        If INSERT_QUERY_SEPARATOR_EVERY = -1 Then
            INSERT_QUERY_SEPARATOR_EVERY = 1 'Required: low value to avoid
            ' errors on insert
        Else
            INSERT_QUERY_SEPARATOR_EVERY = INSERT_QUERY_SEPARATOR_EVERY
        End If
        If MAN_IDENT_MAX_SIZE < 0 Then
            ' User has not overridden, so set database-specific length:
            IDENT_MAX_SIZE = 128
        End If
    Case Else
        DB_ENGINE = "UNKNOWN DATABASE SPECIFIED: " & DB_ENGINE
        m_QuerySep = vbCrLf
        If INSERT_QUERY_SEPARATOR_EVERY = -1 Then
            INSERT_QUERY_SEPARATOR_EVERY = 1
        Else
            INSERT_QUERY_SEPARATOR_EVERY = INSERT_QUERY_SEPARATOR_EVERY
        End If
        If MAN_IDENT_MAX_SIZE < 0 Then
            ' User has not overridden, so set database-specific length:
            IDENT_MAX_SIZE = 64
        End If
        Err.Raise vbObjectError + 238, "InitializeModuleVars", "Unknown database engine specified: " & DB_ENGINE
End Select

' Check for user override on object length
' This will trump the database-specific setting we just made.
Select Case MAN_IDENT_MAX_SIZE
Case 0
    Warn "IDENT_MAX_SIZE must not be 0. Please set it to -1. Aborting now.", warning
    Err.Raise vbObjectError + 2323, "InitializeModuleVars", "Invalid value for IDENT_MAX_SIZE."
Case Is > 0
    ' User has overridden selection, so we bow to his wishes:
    IDENT_MAX_SIZE = MAN_IDENT_MAX_SIZE
End Select
End Sub

Private Function IsSelectQuery(ByVal qdfQuery As DAO.QueryDef) As Boolean
' Tries to determine if a query is a select query.
On Error GoTo QErr:
Select Case qdfQuery.Type
    Case DAO.dbQAction, DAO.dbQAppend, DAO.dbQDDL, DAO.dbQDelete, _
        DAO.dbQMakeTable, DAO.dbQProcedure, DAO.dbQSPTBulk, DAO.dbQUpdate
        IsSelectQuery = False
    Case Else
        If qdfQuery.Parameters.Count = 0 Then
            ' Select by default...
            IsSelectQuery = True
        Else
            ' If we need to specify parameters, it can't be a view
            ' but must be a stored procedure
            IsSelectQuery = False
        End If
End Select
Exit Function

QErr:
' could be a fatal error, but possibly not. Log & ignore
Dim ErrDesc As String, Errno As Long
Errno = Err.Number
ErrDesc = Err.Description & " (at program line: " & Erl & ")"
Warn "IsSelectQuery: non-fatal error: " & ErrDesc
Debug.Print Now() & " - IsSelectQuery: non-fatal error error: " & ErrDesc & " (Error number: " & Errno & ")"
IsSelectQuery = True
End Function

Private Function Null2CSV( _
    OutputField As DAO.Field, _
    DB_ENGINE As String) As String
' Converts Null value to output string
' This will mostly just be blank.
On Error GoTo ConvError:
Dim strTemp As String
Select Case DB_ENGINE
Case "MY"
    ' MySQL has funny notions about null fields in CSV data.
    ' It apparently barfs on empty fields but needs NULL specified explicitly.
    Select Case OutputField.Type
    Case DAO.dbBoolean
        strTemp = "NULL"
    Case DAO.dbChar, DAO.dbText, DAO.dbMemo, _
      DAO.dbGUID
        strTemp = "NULL"
    Case DAO.dbBinary, DAO.dbLongBinary, DAO.dbVarBinary
        strTemp = "NULL"
    Case DAO.dbDate, DAO.dbTimeStamp
        strTemp = "NULL"
    Case DAO.dbTime
        strTemp = "NULL"
    Case DAO.dbBigInt, DAO.dbByte, DAO.dbInteger, DAO.dbLong
        strTemp = "NULL"
    Case DAO.dbCurrency, DAO.dbDecimal, DAO.dbFloat, DAO.dbNumeric, DAO.dbSingle
        strTemp = "NULL"
    Case Else
        ' Text or whatever, but we need to fix this code so it properly recognizes this field type.
        Debug.Print Now() & " - Null2CSV: Unknown field type: " & OutputField.Type & " for field " & OutputField.Name & ". Please fix the code so it recognizes this field."
        strTemp = "NULL"
    End Select
Case Else
    strTemp = "" 'Empty string
End Select
Null2CSV = strTemp
Exit Function

ConvError:
' could be a fatal error, but possibly not. Log & ignore
Dim ErrDesc As String, Errno As Long
Errno = Err.Number
ErrDesc = Err.Description & " (at program line: " & Erl & ")"
Warn "Null2CSV: conversion error: " & ErrDesc
Debug.Print Now() & " - Null2CSV: Conversion error: " & ErrDesc & " (Error number: " & Errno & ")"
Null2CSV = ""
End Function
Private Sub StatusMessage(intOutputHandle As Integer, _
    strMessage As String)
' Prints a status message to the output file so that
' the script will output a message when run on the target database.
' Use it to indicate progress, or warn for actions that might give error messages
' but are actually not a problem.
Dim strStatusPrefix As String
Dim strStatusPostfix As String
Dim strCleanStatus As String
'Defaults for safety
If intOutputHandle > 0 Then
    Select Case DB_ENGINE
    Case "FIREBIRD"
        strStatusPrefix = "select '"
        strStatusPostfix = "' as Status from rdb$database;"
    Case "MSSQL", "SYBASE"
        strStatusPrefix = "print '"
        strStatusPostfix = "';"
    Case Else
        strStatusPrefix = "-- Status message: "
        strStatusPostfix = ""
    End Select
    strCleanStatus = Replace(strMessage, "'", "''") ' Escape single quotes if necessary
    strCleanStatus = Replace(strCleanStatus, vbCrLf, strStatusPostfix & vbCrLf & strStatusPrefix) 'Multiline support
    If Right(strCleanStatus, 1) <> strStatusPostfix Then
        strCleanStatus = strCleanStatus & strStatusPostfix
    End If
    strCleanStatus = strStatusPrefix & strCleanStatus
    Print #intOutputHandle, strCleanStatus & m_QuerySep
Else
    'warn "Could not find output file for status message"
End If
End Sub
Private Function String2SQL(Output As String, _
    DB_ENGINE As String _
) As String
' Although we can mostly use field2sql, sometimes we don't
' have a field value that we want to convert, but a regular
' variable.
' Take care to synchronize this code with the main code
' in Field2SQL
Dim strTemp As String
strTemp = Output
' Note we can't test for NULL, only generate empty strings.
Select Case DB_ENGINE
    Case "ACCESS", "CSV", "DB2", "DERBY", "FIREBIRD", "ORACLE", "PG", "SQLITE", "SQL2003"
        ' General case
        ' Just get rid of common non-printable stuff
        ' Let tabs (ascii 9) remain
        strTemp = Replace(strTemp, Chr$(0), "") 'ASCII NUL
        strTemp = Replace(strTemp, vbBack, "") 'Backspace, ASCII 8
        ' Now replace single quotes with two single quotes
        ' and surround output with single quotes.
        strTemp = Replace(strTemp, "'", "''")
        strTemp = "'" & strTemp & "'"
    Case "MSSQL", "SYBASE"
        'Same as general case, except we want to prefix output with N'
        'Just get rid of common non-printable stuff
        ' Let tabs (ascii 9) remain
        strTemp = Replace(strTemp, Chr$(0), "") 'ASCII NUL
        strTemp = Replace(strTemp, vbBack, "") 'Backspace, ASCII 8
        ' Now replace single quotes with two single quotes
        ' and surround output with single quotes.
        strTemp = Replace(strTemp, "'", "''")
        strTemp = "N'" & strTemp & "'"
    Case "MY", "M1", "M2"
        ' Escape using MySQL syntax. We could double up our
        ' quotes, but escaping is clearer and we can also
        ' use it for binary fields.
        strTemp = Replace(strTemp, Chr$(0), "\0") 'ASCII NUL
        strTemp = Replace(strTemp, vbBack, "") 'Backspace, ASCII 8
        strTemp = Replace(strTemp, vbTab, "\t") 'Tab, ascii 9
        strTemp = Replace(strTemp, vbLf, "\l") 'Line feed, ascii 10
        strTemp = Replace(strTemp, vbCr, "\r") 'Carriage return, ascii 13
        strTemp = Replace(strTemp, "\", "\\")
        strTemp = Replace(strTemp, Chr$(26), "\Z") 'Ctrl-Z, ASCII 26, Windows end of file marker
        strTemp = Replace(strTemp, "'", "\'")
        ' Now surround output with single quotes.
        strTemp = "'" & strTemp & "'"
    Case Else
        ' General case
        ' Just get rid of common non-printable stuff
        ' Let tabs (ascii 9) remain
        strTemp = Replace(strTemp, Chr$(0), "") 'ASCII NUL
        strTemp = Replace(strTemp, vbBack, "") 'Backspace, ASCII 8
        ' Now replace single quotes with two single quotes
        ' and surround output with single quotes.
        strTemp = Replace(strTemp, "'", "''")
        strTemp = "'" & strTemp & "'"
        Warn "Could not find conversion for string with value " & Output & ". Used a default conversion. Data may come out wrong.", warning
End Select
String2SQL = strTemp
End Function

Private Function Time2SQL( _
    TimeValue As Variant, _
    DB_ENGINE As String) As String
On Error GoTo ConvError:
Dim strTemp As String
If IsNull(TimeValue) Then
    strTemp = "NULL"
Else
    Select Case DB_ENGINE
    Case "CSV"
        strTemp = Format$(TimeValue, "HH:MM:SS")
    Case "DERBY", "FIREBIRD", "MY", "MSSQL"
        strTemp = "'" & Format$(TimeValue, "HH:MM:SS") & "'"
    Case "MSSQL", "SYBASE"
        strTemp = "CAST ('" & Format$(TimeValue, "HH:MM:SS") & "' AS DateTime)"
    Case Else
    '  Guess a default
        Warn "Using default time conversion function for this database. todo: implement a specific function for this."
        strTemp = Format$(TimeValue, "YYYY-MM-DD HH:MM:SS")
    End Select
End If
Time2SQL = strTemp
Exit Function

ConvError:
' could be a fatal error, but possibly not. Log & ignore
Dim ErrDesc As String, Errno As Long
Errno = Err.Number
ErrDesc = Err.Description & " (at program line: " & Erl & ")"
Warn "Time2SQL: conversion error: " & ErrDesc
Debug.Print Now() & " - Date2SQL: Conversion error: " & ErrDesc & " (Error number: " & Errno & ")"
Time2SQL = ""
End Function
Public Function TranslateFieldType( _
    TargetEngine As String, _
    SourceField As DAO.Field _
    ) As String
On Error GoTo TranslateError:
Dim strType As String
Dim lngFieldLength As Long
Dim TargetFieldName As String
' Find out what our translated field
' name will be. Necessary for check constraints within
' field type definition.
TargetFieldName = TranslateName( _
    SourceField.Name, _
    "Irrelevant, coming from TranslateFieldType function.", _
    IGNOREFORTRANSLATION, _
    TargetEngine)
Select Case TargetEngine
    Case "ACCESS", "CSV"
        ' Simple 1:1 text output
        ' We use the Access sql data type info from
        'http://msdn.microsoft.com/en-us/library/bb208866%28printer%29.aspx
        Select Case SourceField.Type
            Case dbBinary
                strType = "BINARY" '1 byte per character
            Case dbBoolean
                strType = "BIT" '1 byte  Yes and No values and fields that contain only one of two values.
            Case dbByte '1 byte
                strType = "TINYINT" '1 byte An integer value between 0 and 255.
            Case dbChar
                strType = "CHARACTER(" & SourceField.Size & ")" 'Zero to 255 characters.
            Case dbCurrency
                strType = "MONEY" '8 bytes A scaled integer between &#x2013; 922,337,203,685,477.5808 and 922,337,203,685,477.5807.
            Case dbDate
                strType = "DATETIME" '8 bytes A date or time value between the years 100 and 9999.
            Case dbDecimal '10^28&#x2013;1 - 10^28&#x2013;1, 12 bytes, 28 decimal places
                strType = "DECIMAL" '17 bytes    An exact numeric data type that holds values from 1028 - 1 through - 1028 - 1. You can define both precision (1 - 28) and scale (0 - defined precision). The default precision and scale are 18 and 0, respectively.
            Case dbDouble ' 8 bytes; &#x2013;1.79769313486231E308 - &#x2013;4.94065645841247E&#x2013;324 and 4.94065645841247E&#x2013;324 - 1.79769313486231E308
                strType = "DOUBLE"
            Case dbFloat 'FLOAT???
                strType = "FLOAT" '8 bytes A double-precision floating-point value with a range of &#x2013; 1.79769313486232E308 to &#x2013; 4.94065645841247E-324 for negative values, 4.94065645841247E-324 to 1.79769313486232E308 for positive values, and 0.
            Case dbInteger '2 bytes &#x2013;32.768 - 32.767
                strType = "SMALLINT" '2 bytes A short integer between &#x2013; 32,768 and 32,767.
            Case dbLong '4 bytes -2.147.483.648 -  2.147.483.647
                strType = "INTEGER" '4 bytes A long integer between &#x2013; 2,147,483,648 and 2,147,483,647. (See Notes)
            Case dbLongBinary
                strType = "IMAGE" 'Zero to a maximum of 2.14 gigabytes. Used for OLE objects.
            Case dbMemo
                strType = "TEXT" 'Zero to a maximum of 2.14 gigabytes.
            Case dbNumeric
                strType = "NUMERIC"
            Case dbSingle '4 bytes; &#x2013;3.402823E38 - &#x2013;1.401298E&#x2013;45 and 1.401298E&#x2013;45 - 3.402823E38
                strType = "REAL" '4 bytes A single-precision floating-point value with a range of &#x2013; 3.402823E38 to &#x2013; 1.401298E-45 for negative values, 1.401298E-45 to 3.402823E38 for positive values, and 0.
            Case dbText
                lngFieldLength = SourceField.Size
                If lngFieldLength = 0 Then
                    'Just output text
                    strType = "TEXT" 'Zero to a maximum of 2.14 gigabytes.
                Else
                    strType = "TEXT(255)" 'Zero to a maximum of 2.14 gigabytes.
                End If
            Case dbTime
                strType = "DATETIME" '8 bytes A date or time value between the years 100 and 9999.
            Case dbTimeStamp
                strType = "DATETIME" '8 bytes A date or time value between the years 100 and 9999.
            Case dbVarBinary
                strType = "IMAGE" 'Zero to a maximum of 2.14 gigabytes. Used for OLE objects.
            Case dbBigInt
                strType = "BIGINT" 'unknown what kind of datatype we should make this.
            Case dbGUID
                strType = "UNIQUEIDENTIFIER" '128 bits    A unique identification number used with remote procedure calls.
            Case Else
                Warn "Unknown datatype for field " & SourceField.Name & ". I had to choose some sensible type."
                strType = "TEXT" 'assume text blob
        End Select   'sourcefield type
    Case "DB2", "DERBY"
        'http://db.apache.org/derby/docs/10.1/ref/crefsqlj21305.html
        Select Case SourceField.Type
            Case dbBinary
                strType = "BLOB" ' up to 2,147,483,647 characters long
            Case dbBoolean
                strType = "CHAR(1) FOR BIT DATA" '1 byte
            Case dbByte '1 byte
                strType = "SMALLINT" '2 bytes &#x2013;32.768 - 32.767
            Case dbChar
                strType = "CHAR(" & SourceField.Size & ")" 'Zero to 255 characters. CHARACTER is an alias
            Case dbCurrency
                strType = "DECIMAL" 'Default scale 0, precision 5. Numeric is an alias for decimal
            Case dbDate
                strType = "TIMESTAMP" 'combined DATE and TIME value, e.g. yyyy-mm-dd hh[:mm[:ss[.nnnnnn]]
            Case dbDecimal '10^28&#x2013;1 - 10^28&#x2013;1, 12 bytes, 28 decimal places
                strType = "DECIMAL (31, 28)" 'The precision must be between 1 and 31. The scale must be less than or equal to the precision.
            Case dbDouble ' 8 bytes; &#x2013;1.79769313486231E308 - &#x2013;4.94065645841247E&#x2013;324 and 4.94065645841247E&#x2013;324 - 1.79769313486231E308
                strType = "DOUBLE PRECISION" '8 bytes
            Case dbFloat 'FLOAT???
                strType = "REAL" '4 bytes
            Case dbInteger '2 bytes &#x2013;32.768 - 32.767
                strType = "SMALLINT" '2 bytes &#x2013;32.768 - 32.767
            Case dbLong '4 bytes -2.147.483.648 -  2.147.483.647
                strType = "INTEGER" '4 bytes integer between &#x2013; 2,147,483,648 and 2,147,483,647
            Case dbLongBinary
                strType = "BLOB" ' up to 2,147,483,647 characters long
            Case dbMemo
                strType = "CLOB(2147483647)" 'up to 2,147,483,647 characters long
            Case dbNumeric
                strType = "DECIMAL" 'Default scale 0, precision 5. Numeric is an alias for decimal
            Case dbSingle '4 bytes; &#x2013;3.402823E38 - &#x2013;1.401298E&#x2013;45 and 1.401298E&#x2013;45 - 3.402823E38
                strType = "REAL" '4 bytes
            Case dbText
                lngFieldLength = SourceField.Size
                If lngFieldLength = 0 Then
                    lngFieldLength = 255
                End If
                strType = "VARCHAR(" & lngFieldLength & ")"
            Case dbTime
                strType = "TIME" 'time of day, hh:mm[:ss]
            Case dbTimeStamp
                strType = "TIMESTAMP" 'date/time
            Case dbVarBinary
                strType = "BLOB" ' up to 2,147,483,647 characters long
            Case dbBigInt 'what datatype is this?
                strType = "BIGINT" '8 bytes
            Case dbGUID
                strType = "CHAR(16) FOR BIT DATA" '16 bytes
            Case Else
                Warn "Unknown datatype for field " & SourceField.Name & ". I had to choose some sensible type."
                strType = "TEXT" 'assume text blob
        End Select   'sourcefield type
    Case "FIREBIRD"
        Select Case SourceField.Type
            Case dbBinary
                strType = "BLOB"
            Case dbBoolean
                strType = "BOOLEAN" ' Use domain we created. If not using domains, you could use SMALLINT
            Case dbByte
                strType = "SMALLINT"
            Case dbChar
                strType = "VARCHAR(" & SourceField.Size & ")"
            Case dbCurrency
                strType = "DECIMAL(18,4)" '18,4 is max precision in Firebird.
            Case dbDate
                strType = "TIMESTAMP"
            Case dbDecimal
                strType = "DECIMAL(18,4)"
            Case dbDouble
                strType = "DOUBLE PRECISION"
            Case dbFloat
                strType = "DOUBLE PRECISION" 'Double precision = 64 bits. could also be FLOAT; that's 32 bit
            Case dbInteger
                strType = "SMALLINT"
            Case dbLong
                strType = "INTEGER"
            Case dbLongBinary
                strType = "BLOB SUB_TYPE 0" 'subtype 0 is binary
            Case dbMemo
                strType = "BLOB SUB_TYPE TEXT" 'subtype 1 is text. Note we cannot index blobs
            Case dbNumeric
                strType = "DECIMAL(18,4)"
            Case dbSingle
                strType = "FLOAT"
            Case dbText
                lngFieldLength = SourceField.Size
                If lngFieldLength = 0 Then
                    lngFieldLength = 255
                End If
                strType = "VARCHAR(" & lngFieldLength & ")"
            Case dbTime
                strType = "TIME"
            Case dbTimeStamp
                strType = "TIMESTAMP"
            Case dbVarBinary
                strType = "BLOB SUB_TYPE 0"
            Case dbBigInt
                strType = "BIGINT" 'Firebird BIGINT=64 bits
            Case dbGUID
                strType = "CHAR(16)" ' we can store the guid in hex.
                'see http://www.firebirdfaq.org/faq98/
            Case Else
                Warn "Unknown datatype for field " & SourceField.Name & ". I had to choose some sensible type."
                strType = "BLOB SUB_TYPE TEXT" 'assume text blob
        End Select   'sourcefield type
    Case "M1", "M2"
        Select Case SourceField.Type
            Case dbChar
                strType = "CHAR(" & SourceField.Size & ")"
            Case dbText
                lngFieldLength = SourceField.Size
                If lngFieldLength = 0 Then
                    lngFieldLength = 255
                End If
                strType = "CHAR(" & lngFieldLength & ")"
            Case dbBoolean, dbByte, dbInteger, dbLong
                strType = "INT"
            Case dbDouble, dbFloat, dbSingle
                strType = "REAL"
            Case dbCurrency, dbDecimal, dbNumeric
                strType = "REAL"
                Warn "In field '" & SourceField.Name & "', currency/BCD will be converted to REAL - there may be precision loss!", remark
            Case dbDate
                strType = "CHAR(19)"
                'Warn "In field '" & SourceField.Name & "', date/time/timestamp will be converted to " & strType & "."
            Case dbTime
                strType = "CHAR(8)"
                'Warn "In field '" & SourceField.Name & "', date/time/timestamp will be converted to " & strType & "."
            Case dbTimeStamp
                strType = "CHAR(19)"
                'Warn "In field '" & SourceField.Name & "', date/time/timestamp will be converted to " & strType & "."
                'If TargetEngine = "M2" then warn "Consider using pseudo field '_timestamp'."
            Case dbMemo
                If TargetEngine = "M2" Then
                    strType = "TEXT(" & MSQL_64kb_AVG & ")"
                Else
                    strType = "CHAR(" & MSQL_64kb_AVG & ")"
                    Warn "In field '" & SourceField.Name & "', dbMemo is not supported by mSQL v1 - fields larger than MSQL_64kb_AVG (" & MSQL_64kb_AVG & ") will not be accepted!", warning
                End If
            Case dbBinary, dbVarBinary
                strType = "CHAR(255)"
                Warn "In field '" & SourceField.Name & "', dbBinary and dbVarBinary are not supported by mSQL! - will use a text (CHAR(255)) field.", warning
            Case dbLongBinary
                strType = "CHAR(" & MSQL_64kb_AVG & ")"
                Warn "In field '" & SourceField.Name & "', dbLongBinary is not supported by mSQL! - will use a text (CHAR(" & MSQL_64kb_AVG & ")) field.", warning
            Case dbGUID
                strType = "CHAR(36)"
            Case dbBigInt
                strType = "CHAR(255)"
                Warn "In field '" & SourceField.Name & "', dbBigInt and is not currently supported. Using char datatype instead, but unexpected results may occur!", warning
            Case Else
                strType = "CHAR(255)"
                Warn "Unknown datatype for field " & SourceField.Name & ". I had to choose some sensible type.", warning
        End Select
    Case "MSSQL", "SYBASE"
        'Corrected for MSSQL 2008
        'http://msdn.microsoft.com/en-us/library/ms187752.aspx
        'todo: split out Sybase
        Select Case SourceField.Type
            Case dbBoolean
                strType = ("BIT")
            Case dbByte '1 byte
                strType = ("BYTE") 'same as smallint
            Case dbInteger ' 2 bytes signed int
                strType = ("SMALLINT")
            Case dbLong '4 bytes -2.147.483.648 -  2.147.483.647
                strType = ("INTEGER") '4 bytes signed int
            'case verylongintegerdoesn'texist
            '   strType = ("bigint")  from -2^63 (-9,223,372,036,854,775,808) through 2^63-1 (9,223,372,036,854,775,807). Storage size is 8 bytes.
            Case dbSingle
                strType = ("REAL") '4 bytes, - 3.40E + 38 to -1.18E - 38, 0 and 1.18E - 38 to 3.40E + 38
            Case dbDouble ' 8 bytes; &#x2013;1.79769313486231E308 - &#x2013;4.94065645841247E&#x2013;324 and 4.94065645841247E&#x2013;324 - 1.79769313486231E308
                strType = ("FLOAT (53)")
                'floate: '- 1.79E+308 to -2.23E-308, 0 and 2.23E-308 to 1.79E+308
                'Where n is the number of bits that are used to store the mantissa of the float number in scientific notation and, therefore, dictates the precision and storage size. If n is specified, it must be a value between 1 and 53. The default value of n is 53.
                'useful n=24, n=53
                'default n 53, precision 15 digits, storage 8 bytes
            Case dbFloat, dbDecimal
                strType = ("DECIMAL")
                'Fixed precision and scale numbers. When maximum precision is used, valid values are from - 10^38 +1 through 10^38 - 1. The ISO synonyms for decimal are dec and dec(p, s)
                'The default precision is 18, 9 bytes storage
            Case dbCurrency
                strType = ("MONEY") '8 bytes, -922,337,203,685,477.5808 to 922,337,203,685,477.5807
            Case dbDate
                strType = ("DATETIME") 'YYYY-MM-DD hh:mm:ss[.nnn] '1753-01-01 through 9999-12-31
                'strType = ("DATETIME2") 'SQL2008+ same as datetime, except 0001-01-01 00:00:00.0000000 through 9999-12-31 23:59:59.9999999
            Case dbText, dbChar
                Select Case SourceField.Size
                    Case Is <= 0
                        strType = "NVARCHAR(MAX)" 'Safe default. We could have very long columns
                    Case Is > 255
                        strType = "NVARCHAR(MAX)"
                    Case Else 'between 0 and 255 inclusive
                        strType = "NVARCHAR(" & SourceField.Size & ")"
                End Select
            Case dbMemo
                strType = "NVARCHAR(MAX)"
            Case dbGUID
                strType = "UNIQUEIDENTIFIER" 'Is a 16-byte GUID.
            Case dbBinary, dbLongBinary
                strType = "varbinary (max)" ' Binary data types of either fixed length or variable length. The ANSI SQL synonym for varbinary is binary varying.
                'image could be used to but is deprecated in SQL 2008R2
            Case Else
                Warn "Unknown datatype for field " & SourceField.Name & ". I had to choose some sensible type.", warning
                strType = "NVARCHAR(MAX)"
        End Select
    Case "MY" 'Mysql
        Select Case SourceField.Type
            Case dbBinary
                strType = "TINYBLOB"
            Case dbBoolean
                strType = "TINYINT"
            Case dbByte
                strType = "TINYINT UNSIGNED"
            Case dbChar
                strType = "CHAR(" & SourceField.Size & ")"
            Case dbCurrency
                strType = "DECIMAL(20,4)"
            Case dbDate
                strType = "DATETIME"
            Case dbDecimal
                strType = "DECIMAL(20,4)"
            Case dbDouble
                strType = "REAL"
            Case dbFloat
                strType = "REAL"
            Case dbInteger
                strType = "SMALLINT"
            Case dbLong
                strType = "INT"
            Case dbLongBinary
                strType = "LONGBLOB"
            Case dbMemo
                strType = "LONGTEXT"
                'Longtext: A TEXT column with a maximum length of 4,294,967,295 or 4GB (232 &#x2013; 1) characters.
                ' However, we can't run an index on this. So we need to strip indices
                ' on LONGTEXT fields.
            Case dbNumeric
                strType = "DECIMAL(20,4)"
            Case dbSingle
                strType = "FLOAT"
            Case dbText
                lngFieldLength = SourceField.Size
                If lngFieldLength = 0 Then
                    lngFieldLength = 255
                End If
                strType = "CHAR(" & lngFieldLength & ")"
            Case dbTime
                strType = "TIME"
            Case dbTimeStamp
                strType = "TIMESTAMP"
            Case dbVarBinary
                strType = "TINYBLOB"
            Case dbBigInt, dbGUID
                Warn "In field '" & SourceField.Name & "', dbBigInt and dbGUID are not currently supported!"
                Error 5  ' invalid Procedure Call
            Case dbGUID
                'From MySQL 5 docs:
                'A UUID is a 128-bit number represented by a utf8 string of five hexadecimal numbers in aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee  format:
                'UUID() function from 4.1.2 onwards
                'sample: 6ccd780c-baba-1026-9564-0040f4311e29
                '36 characters. But better to store as a binary value
                strType = "BINARY(16)" 'experimental
            Case Else
                Warn "Unknown datatype for field " & SourceField.Name & ". I had to choose some sensible type."
                strType = "LONGBLOB" 'default value
        End Select 'source field type
    Case "Oracle"
        strType = "varchar"
        Warn "todo: Oracle datatype conversion needs implementing"
    Case "PG" 'PostgreSQL
        Select Case SourceField.Type
            Case dbBinary
                strType = "bytea"
            Case dbBoolean
                If PG_MS_ACCESS_LINK_COMPATIBLE Then
                    ' Pg bool is incompatible with MS access when linking tables
                    strType = "int2 CHECK(" & TargetFieldName & " = -1 OR " & TargetFieldName & " = 0)"
                    ' Original code
                    'http://www.computron.gr/exportSQL3.txt
                    'added index for these fields; we'll leave it for now.
                    'Index will need to be generated elsewhere anyway.
                Else
                    strType = "bool"
                End If
            Case dbByte
                strType = "int2"
            Case dbChar
                strType = "varchar(" & SourceField.Size & ")"
            Case dbCurrency
                strType = "DECIMAL(20,4)"
            Case dbDate
                strType = "DATE"
            Case dbDecimal
                strType = "DECIMAL(20,4)"
            Case dbDouble
                strType = "float8"
            Case dbFloat
                strType = "float4"
            Case dbInteger
                strType = "int8"
            Case dbLong
                strType = "int8"
            Case dbLongBinary
                strType = "bytea"
            Case dbMemo
                strType = "text"
            Case dbNumeric
                strType = "DECIMAL(20,4)"
            Case dbSingle
                strType = "float4"
            Case dbText
                lngFieldLength = SourceField.Size
                If lngFieldLength = 0 Then
                    lngFieldLength = 255
                End If
                strType = "varchar(" & lngFieldLength & ")"
            Case dbTime
                strType = "TIME"
            Case dbTimeStamp
                strType = "TIMESTAMP"
            Case dbVarBinary
                strType = "bytea"
            Case dbBigInt
                strType = "int8"
            Case dbGUID
                'warn "In new field '" & SourceField.Name & "', dbGUID is not currently supported!", True
                'PostgreSQL 8.4 docs:
                ' A UUID is written as a sequence of lower-case hexadecimal digits, in several groups separated by hyphens, specifically a group of 8 digits followed by three groups of 4 digits followed by a group of 12 digits, for a total of 32 digits representing the 128 bits. An example of a UUID in this standard form is:
                'a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11
                '=>36 characters
                strType = "UUID"
            Case Else
                Warn "Unknown datatype for field " & SourceField.Name & ". I had to choose some sensible type."
                strType = "text"
        End Select 'source field type
    Case "SQL2003"
        strType = "varchar"
        Warn "todo: standard SQL/SQL2003 datatype conversion needs implementing"
    Case "SQLITE"
        strType = "varchar"
        Warn "todo: SQLITE datatype conversion needs implementing"
        'see eg http://www.sqlite.org/datatype3.html
    Case Else
        Warn "TranslateFieldType: Unknown database engine " & TargetEngine & ". Export will probably fail."
End Select 'database type
TranslateFieldType = strType
Exit Function

TranslateError:
' This should be a fatal error as this is critical for
' the conversion
Dim ErrDesc As String, Errno As Long
Errno = Err.Number
ErrDesc = Err.Description & " (at program line: " & Erl & ")"
Warn "TranslateFieldType: conversion error: " & ErrDesc
Debug.Print Now() & " - TranslateFieldType: Conversion error: " & ErrDesc & " (Error number: " & Errno & ")"
MsgBox "Fatal error in TranslateFieldType: " & vbCrLf & ErrDesc & vbCrLf & "Aborting."
End
End Function
Private Function TranslateName( _
    ByVal strName As String, _
    ByVal strParent As String, _
    ByVal strScope As String, _
    ByVal DB_ENGINE As String, _
    Optional ByVal strPrefix As String = "", _
    Optional ByVal strSuffix As String = "") As String
' Converts MS Access allowed name to an allowed name for the target database engine.
' Takes reserved words and database supported characters into account.
' Tries to avoid having to use quoted/delimited identifier, so strips out special characters if required.
' strParent: parent object of object to be translated
' strScope: object type, e.g. table. Use IGNOREFORTRANSLATION if you don't want to add values to the translation list,
' e.g. when... yeah when? It's somewhere in the code :(
' Possible values below in instr line.
' Updates collection of translation terms, handy for translating queries etc.

Dim NeedsQuotes As Boolean ' Specifically for Access=>Access conversion.
Dim i As Integer, str As String
Dim TranslationItem(4) As String
' 0-based string: (0) name scope, (1) parent name, (2) original name, (3) new name, (4) change indication
' We include foreign scopes like sequence, trigger, stored procedure, view so
' we can show all objects to be created in list
On Error GoTo TranslateName_Error

If InStr("|" & IGNOREFORTRANSLATION & _
    "|Database|Table|TableField|Index|IndexField" & _
    "|Query|QueryField|Relation" & _
    "|Sequence|Trigger|StoredProcedure|View|", _
    strScope) = 0 Then
    Debug.Print "Invalid scope: " & strScope & " for parent " & strParent & ", name " & strName & ". Aborting. Please fix the code."
    Err.Raise vbObjectError + 235, "TranslateName", "Invalid scope: " & strScope & " for parent " & strParent & ", name " & strName & ". Please fix the code."
End If

' First, check to see whether this object has already been translated; then just return the name
' This saves a lot of processing.
str = GetTranslatedObject(strName, strParent, strScope, DB_ENGINE)
If str <> "" Then
    ' Found; it's been already translated in the same scope, so we're done
    TranslateName = str
    Exit Function
End If


NeedsQuotes = False 'Let's assume we happen to have a well-formed name. We'll check for nasties below.

TranslationItem(0) = strScope
TranslationItem(1) = strParent
TranslationItem(2) = strName

    
If m_fPreserveCase = True Then
    str = strName
Else
    ' We like uppercase for our objects.
    ' So, if the user doesn't specify anything else...
    str = UCase(strName)
    strPrefix = UCase(strPrefix)
    strSuffix = UCase(strSuffix)
End If

' Test for first character
Select Case DB_ENGINE
Case "DERBY", "FIREBIRD"
    'First letter must start with a-zA-Z
    If Left(str, 1) < "A" _
        And Left(str, 1) > "Z" _
        And Left(str, 1) < "a" _
        And Left(str, 1) > "z" Then
        ' Just put a letter in front of it
        str = "A" & str
    End If
Case "MSSQL", "SYBASE"
    ' First letter must start with a-zA-Z_ if it is to be unquoted
    If Left(str, 1) < "A" _
        And Left(str, 1) > "Z" _
        And Left(str, 1) < "a" _
        And Left(str, 1) > "z" _
        And Left(str, 1) <> "_" Then
        'NeedsQuotes = True
        ' Just put a letter in front of it
        str = "A" & str
    End If
Case "MY"
    ' Names can start with letters or numbers
    If Left(str, 1) < "A" _
        And Left(str, 1) > "Z" _
        And Left(str, 1) < "a" _
        And Left(str, 1) > "z" _
        And Left(str, 1) < "0" _
        And Left(str, 1) > "9" Then
        ' Just put a letter in front of it
        str = "A" & str
    End If
Case Else
    'todo
End Select

i = 1
While i <= Len(str)
    Select Case Asc(Mid$(str, i, 1))
    Case 0 To 31 ' includes tab, newline, carriage return
        Select Case DB_ENGINE
        Case Else
            ' replace inner spaces/special characters with WS_REPLACEMENT
            str = Left$(str, i - 1) & WS_REPLACEMENT & Right$(str, Len(str) - i)
            i = i + Len(WS_REPLACEMENT)
        End Select
    Case 32 'space
        Select Case DB_ENGINE
        Case "ACCESS"
            ' Keep spaces, mark quotes needed
            i = i + 1
            NeedsQuotes = True
        Case Else
            ' replace inner spaces/special characters with WS_REPLACEMENT
            str = Left$(str, i - 1) & WS_REPLACEMENT & Right$(str, Len(str) - i)
            i = i + Len(WS_REPLACEMENT)
        End Select
    Case 33 '!
        Select Case DB_ENGINE
            Case "DERBY", "FIREBIRD", "MY"
                ' Firebird only allows A-Z, a-z,0-9,$,_
                ' Derby allows only A-Z, a-z, 0-9, _
                ' MySQL allows Firebird character set
                str = Left$(str, i - 1) & WS_REPLACEMENT & Right$(str, Len(str) - i)
                i = i + Len(WS_REPLACEMENT)
            Case "ACCESS"
                ' Keep it, but mark special char. todo: check if really necessary
                NeedsQuotes = True
                i = i + 1
            Case "MSSQL", "SYBASE"
                ' Not allowed if we want to avoid quotes; replace with WS_REPLACEMENT
                str = Left$(str, i - 1) & WS_REPLACEMENT & Right$(str, Len(str) - i)
                i = i + Len(WS_REPLACEMENT)
            Case Else
                ' Keep it
                i = i + 1
        End Select
    Case 34 '"
        Select Case DB_ENGINE
            Case "DERBY", "FIREBIRD", "MY"
                ' Firebird only allows A-Z, a-z,0-9,$,_
                ' Derby allows only A-Z, a-z, 0-9, _
                ' MySQL allows Firebird character set
                str = Left$(str, i - 1) & WS_REPLACEMENT & Right$(str, Len(str) - i)
                i = i + Len(WS_REPLACEMENT)
            Case "ACCESS"
                ' Keep it, but mark special char. todo: check if really necessary
                NeedsQuotes = True
                i = i + 1
            Case "MSSQL", "SYBASE"
                ' Not allowed if we want to avoid quotes; replace with WS_REPLACEMENT
                str = Left$(str, i - 1) & WS_REPLACEMENT & Right$(str, Len(str) - i)
                i = i + Len(WS_REPLACEMENT)
            Case Else
                ' Keep it
                i = i + 1
        End Select
    Case 35 '#
        Select Case DB_ENGINE
            Case "DERBY", "FIREBIRD", "MY"
                ' Firebird only allows A-Z, a-z,0-9,$,_
                ' Derby allows only A-Z, a-z, 0-9, _
                ' MySQL allows Firebird character set
                str = Left$(str, i - 1) & WS_REPLACEMENT & Right$(str, Len(str) - i)
                i = i + Len(WS_REPLACEMENT)
            Case "ACCESS"
                ' Keep it, but mark special char. todo: check if really necessary
                NeedsQuotes = True
                i = i + 1
            Case Else ' MSSQL allows #
                ' Keep it
                i = i + 1
        End Select
    Case 36 '$
        Select Case DB_ENGINE
            Case "DERBY"
                str = Left$(str, i - 1) & WS_REPLACEMENT & Right$(str, Len(str) - i)
                i = i + Len(WS_REPLACEMENT)
            Case "ACCESS", "MSSQL", "SYBASE"
                ' Keep it. todo: check if Access, Sybase supports this
                i = i + 1
            Case Else 'MSSQL, MySQL allow $
                ' Keep it
                i = i + 1
        End Select
    Case 37 To 47 ' % to /
        Select Case DB_ENGINE
            Case "DERBY", "FIREBIRD", "MY"
                ' Firebird only allows A-Z, a-z,0-9,$,_
                ' Derby allows only A-Z, a-z, 0-9, _
                ' MySQL allows Firebird character set
                str = Left$(str, i - 1) & WS_REPLACEMENT & Right$(str, Len(str) - i)
                i = i + Len(WS_REPLACEMENT)
            Case "ACCESS"
                ' Keep it, but mark special char. todo: check if really necessary
                NeedsQuotes = True
                i = i + 1
            Case "MSSQL", "SYBASE"
                ' Not allowed if we want to avoid quotes; replace with WS_REPLACEMENT
                str = Left$(str, i - 1) & WS_REPLACEMENT & Right$(str, Len(str) - i)
                i = i + Len(WS_REPLACEMENT)
            Case Else
                ' Keep it
                i = i + 1
        End Select
    Case 48 To 57 '0 to 9
        Select Case DB_ENGINE
            Case Else
                ' Keep it
                i = i + 1
        End Select
    Case 58 To 63 ': to ?
        Select Case DB_ENGINE
            Case "DERBY", "FIREBIRD", "MY", "MSSQL", "SYBASE"
                ' Firebird only allows A-Z, a-z,0-9,$,_
                ' Derby allows only A-Z, a-z, 0-9, _
                ' MySQL allows Firebird character set
                ' MSSQL, Sybase: not allowed if we want to avoid quotes
                str = Left$(str, i - 1) & WS_REPLACEMENT & Right$(str, Len(str) - i)
                i = i + Len(WS_REPLACEMENT)
            Case "ACCESS"
                ' Keep it, but mark special char. todo: check if really necessary
                NeedsQuotes = True
                i = i + 1
            Case Else
                ' Keep it
                i = i + 1
            End Select
    Case 64 '@
        Select Case DB_ENGINE
            Case "DERBY", "FIREBIRD", "MY"
                ' Firebird only allows A-Z, a-z,0-9,$,_
                ' Derby allows only A-Z, a-z, 0-9, _
                ' MySQL allows Firebird character set
                str = Left$(str, i - 1) & WS_REPLACEMENT & Right$(str, Len(str) - i)
                i = i + Len(WS_REPLACEMENT)
            Case "ACCESS"
                ' Keep it, but mark special char. todo: check if really necessary
                NeedsQuotes = True
                i = i + 1
            Case Else ' MSSQL allows @, sybase probably too
                ' Keep it
                i = i + 1
            End Select
    Case 65 To 90 ' A to Z
        Select Case DB_ENGINE
            Case Else
                ' Keep it
                i = i + 1
            End Select
    Case 91 To 94 ' [ to ^
        Select Case DB_ENGINE
            Case "DERBY", "FIREBIRD", "MY", "MSSQL", "SYBASE"
                ' Firebird only allows A-Z, a-z,0-9,$,_
                ' Derby allows only A-Z, a-z, 0-9,_
                ' MySQL allows Firebird character set
                ' MSSQL, Sybase: not allowed if we want to avoid quotes
                str = Left$(str, i - 1) & WS_REPLACEMENT & Right$(str, Len(str) - i)
                i = i + Len(WS_REPLACEMENT)
            Case "ACCESS"
                ' Keep it, but mark special char.
                ' todo: we'll probably need to escape [ and ]!!?!
                NeedsQuotes = True
                i = i + 1
            Case Else
                ' Keep it
                i = i + 1
            End Select
    Case 95 ' _ (underscore)
        Select Case DB_ENGINE
            Case Else
                ' Keep it
                i = i + 1
            End Select
    Case 96 ' `
        Select Case DB_ENGINE
            Case "DERBY", "FIREBIRD", "MY", "MSSQL", "SYBASE"
                ' Firebird only allows A-Z, a-z,0-9,$,_
                ' Derby allows only A-Z, a-z, 0-9,_
                ' MySQL allows Firebird character set
                ' MSSQL, Sybase: not allowed if we want to avoid quotes
                str = Left$(str, i - 1) & WS_REPLACEMENT & Right$(str, Len(str) - i)
                i = i + Len(WS_REPLACEMENT)
            Case "ACCESS"
                ' Keep it, but mark special char.
                NeedsQuotes = True
                i = i + 1
            Case Else
                ' Keep it
                i = i + 1
            End Select
    Case 97 To 122 ' a to z
        Select Case DB_ENGINE
            Case Else
                ' Keep it
                i = i + 1
            End Select
    Case 123 To 255 ' { to accented chars
        Select Case DB_ENGINE
            Case "DERBY", "FIREBIRD", "MSSQL", "MY", "SYBASE"
                ' Firebird only allows A-Z, a-z,0-9,$,_
                ' Derby allows only A-Z, a-z, 0-9,_
                ' MySQL allows Firebird character set
                ' MSSQL, Sybase: not allowed if we want to avoid quotes
                ' - note perhaps allow accented characters
                str = Left$(str, i - 1) & WS_REPLACEMENT & Right$(str, Len(str) - i)
                i = i + Len(WS_REPLACEMENT)
            Case "ACCESS"
                ' Keep it, but mark special char. todo: check if really necessary
                NeedsQuotes = True
                i = i + 1
            Case Else
                ' Keep it
                i = i + 1
            End Select
    Case Else
        Debug.Print "todo: explicitly code translatename for ASCII character " & Asc(Mid$(str, i, 1))
        i = i + 1
    End Select
Wend
' restrict name to IDENT_MAX_SIZE chars, *after* eating spaces and adding prefix & suffix:
str = Left$(strPrefix & str, IDENT_MAX_SIZE - Len(strSuffix)) & strSuffix

' Reserved words
If DB_ENGINE = "DERBY" Then
    Select Case LCase$(str)
    'Derby 10.5 reserved words:
    'http://db.apache.org/derby/docs/10.5/ref/rrefkeywords29722.html#rrefkeywords29722
    Case _
        "add", "all", "allocate", "alter", "and", "any", "are", "as", "asc", "assertion", "at", "authorization", _
        "avg", "begin", "between", "bigint", "bit", "boolean", "both", "by", "call", "cascade", "cascaded", "case", _
        "cast", "char", "character", "check", "close", "coalesce", "collate", "collation", "column", "commit", "connect", _
        "connection", "constraint", "constraints", "continue", "convert", "corresponding", "create", "current", _
        "current_date", "current_role", "current_time", "current_timestamp", "current_user", "cursor", "deallocate", _
        "dec", "decimal", "declare", "default", "deferrable", "deferred", "delete", "desc", "describe", "diagnostics", _
        "disconnect", "distinct", "double", "drop", "else", "end", "end-exec", "escape", "except", "exception", "exec", _
        "execute", "exists", "explain", "external", "false", "fetch", "first", "float", "for", "foreign", "found", "from", _
        "full", "function", "get", "getcurrentconnection", "global", "go", "goto", "grant", "group", "having", "hour", _
        "identity", "immediate", "in", "indicator", "initially", "inner", "inout", "input", "insensitive", "insert", _
        "int", "integer", "intersect", "into", "is", "isolation", "join", "key", "last", "left", "like", _
        "lower", "ltrim", _
        "match", "max", "min", "minute", "national", "natural", "nchar", "nvarchar", "next", "no", _
        "none", "not", "null", "nullif", "numeric", "of", "on", "only", "open", "option", "or", "order", _
        "outer", "output", "over", "overlaps", "pad", "partial", "prepare", "preserve", _
        "primary", "prior", "privileges", "procedure", "public", "read", "real", "references", _
        "relative", "restrict", "revoke", "right", "rollback", "rows", "row_number", "rtrim", _
        "schema", "scroll", "second", "select", "session_user", "set", "smallint", "some", "space", _
        "sql", "sqlcode", "sqlerror", "sqlstate", "substr", "substring", "sum", "system_user", _
        "table", "temporary", "timezone_hour", "timezone_minute", "to", "transaction", "translate", _
        "translation", "trim", "true", "union", "unique", "unknown", "update", "upper", "user", _
        "using", "values", "varchar", "varying", "view", "whenever", "where", "with", "work", _
        "write", "xml", "xmlexists", "xmlparse", "xmlquery", "xmlserialize", "year"
            str = Left$(PREFIX_ON_KEYWORD & str, IDENT_MAX_SIZE - Len(SUFFIX_ON_KEYWORD)) & SUFFIX_ON_KEYWORD
    End Select
End If

If DB_ENGINE = "FIREBIRD" Then
    Select Case LCase$(str)
    'Firebird 2.1 reserved words. Source: Firebird 2.1 Release notes, Firebird 1.5 Release notes
    ' Interbase 6 documentation
    ' First line: FB 2 newly reserved words
    ' Second line: FB 1.5 newly reserved words
    ' Third line and further: Interbase 6 reserved words
        Case _
              "bit_length", "both", "char_length", "character_length", "close", "connect", "cross", "disconnect", "fetch", "global", "insensitive", "leading", "lower", "octet_length", "open", "rows", "start", "trailing", "trim", "using", _
              "bigint", "case", "current_connection", "current_role", "current_transaction", "current_user", "recreate", "row_count", "release", "savepoint", _
              "active add", "admin", "after", "all", "alter", "and", "any", "as", "asc", "ascending", "at", "auto", "autoddl", "avg", "based", "base_name", "before", "begin", "between", "blob", "blobedit", "buffer", "by", "cast", "char", "character", "character_length", "char_length", "check", "check_point_length", "collate", "collation", "column", "commit", "committed", "compiletime", "computed", "close", "conditional", "connect", "constraint", "containing", "continue", "count", "create", "cstring", "current", "current_date", "current_time", "current_timestamp", "cursor", "database", "date", "day", "db_key", "debug", "dec", "decimal", "declare", "default", "delete", "desc", "descending", "describe", "descriptor", "disconnect", _
              "display", "distinct", "do", "domain", "double", "drop", "echo", "edit", "else", "end", "entry_point", "escape", "event", "exception", "execute", "exists", "exit", "extern", "external", "extract", "fetch", "file", "filter", "float", "for", "foreign", "found", "from", "full", "function", "gdscode", "generator", "gen_id", "global", "goto", "grant", "group", "group_commit_", "wait_time", "having", "help", "hour", "if", "immediate", "in", "inactive", "index", "indicator", "init", "inner", "input", "input_type", "insert", "int", "integer", "into", "is", "isolation", "isql", "join", "key", "lc_messages", "lc_type", "left", "length", "lev", "level", "like", "log_buffer_size", "long", "manual", _
              "max", "maximum", "maximum_segment", "max_segment", "merge", "message", "min", "minimum", "minutemodule_name", "month names", "national", "natural", "nchar", "no noauto", "not", "null", "numeric", "num_log_buffers", "octet_length of", "on", "only open", "option", "or", "order", "outer", "output", "output_type", "overflow", "page", "pagelength", "pages", "page_size", "parameter", "password", "plan", "position", "post_event", "precision", "prepare", "procedure", "protected", "primary", "privileges", "public", "quit", "rdb$db_key", "read", "real", "record_version", "references", "release", "reserv", "reserving", "retain", "return", "returning_values", "returns", "revoke", "right", "rollback", "runtime", "schema", "second", "segment", "select", "set", _
              "shadow", "shared", "shell", "show", "singular", "size", "smallint", "snapshot", "some", "sort", "sqlcode", "sqlerror", "sqlwarning", "stability", "starting", "starts", "statement", "static", "statistics", "sub_type", "sum", "suspend", "table", "terminator", "then", "time", "timestamp", "to", "transaction", "translate", "translation", "trigger", "trim", "uncommitted", "union", "unique", "update", "upper", "user", "using", "value", "values", "varchar", "variable", "varying", "version", "view", "wait", "when", "whenever", "where", "while", "with", "work", "write", "year"
            str = Left$(PREFIX_ON_KEYWORD & str, IDENT_MAX_SIZE - Len(SUFFIX_ON_KEYWORD)) & SUFFIX_ON_KEYWORD
    End Select
End If

If DB_ENGINE = "MSSQL" Then
    ' Surround name with [ and ] would seem to be simplest.
    ' However, crazy MSSQL doesn't seem to like [s when running sys.sp_addextendedproperty
    ' so let's not used quoted/delimited identifiers. (Or possibly I'm doing something wrong,
    ' but quoted identifiers is a bad idea anyway).
    ' List compiled of all words in:
    ' SQL Server 2000:
    ' http://msdn.microsoft.com/en-us/library/aa238507%28SQL.80%29.aspx
    ' SQL Server 2005:
    ' http://msdn.microsoft.com/en-us/library/ms189822%28v=SQL.90%29.aspx
    ' SQL Server 2008:
    ' http://msdn.microsoft.com/en-us/library/ms189822%28v=SQL.100%29.aspx
    ' SQL Server 2008R2:
    ' http://msdn.microsoft.com/en-us/library/ms189822%28v=SQL.105%29.aspx
    Select Case LCase$(str)
        Case _
            "add", "all", "alter", "and", "any", "as", "asc", "authorization", "backup", "begin", "between", "break", "browse", "bulk", "by", "cascade", "case", _
            "check", "checkpoint", "close", "clustered", "coalesce", "collate", "column", "commit", "compute", "constraint", "contains", "containstable", _
            "continue", "convert", "create", "cross", "current", "current_date", "current_time", "current_timestamp", "current_user", "cursor", "database", _
            "dbcc", "deallocate", "declare", "default", "delete", "deny", "desc", "disk", "distinct", "distributed", "double", "drop", "dummy", "dump", "else", _
            "end", "errlvl", "escape", "except", "exec", "execute", "exists", "exit", "external", "fetch", "file", "fillfactor", "for", "foreign", "freetext", _
            "freetexttable", "from", "full", "function", "goto", "grant", "group", "having", "holdlock", "identity", "identitycol", "identity_insert", "if", "in", _
            "index", "inner", "insert", "intersect", "into", "is", "join", "key", "kill", "left", "like", "lineno", "load", "merge", "national", "nocheck", _
            "nonclustered", "not", "null", "nullif", "of", "off", "offsets", "on", "open", "opendatasource", "openquery", "openrowset", "openxml", "option", "or", _
            "order", "outer", "over", "percent", "pivot", "plan", "precision", "primary", "print", "proc", "procedure", "public", "raiserror", "read", "readtext", _
            "reconfigure", "references", "replication", "restore", "restrict", "return", "revert", "revoke", "right", "rollback", "rowcount", "rowguidcol", _
            "rule", "save", "schema", "securityaudit", "select", "session_user", "set", "setuser", "shutdown", "some", "statistics", "system_user", "table", _
            "tablesample", "textsize", "then", "to", "top", "tran", "transaction", "trigger", "truncate", "tsequal", "union", "unique", "unpivot", "update", _
            "updatetext", "use", "user", "values", "varying", "view", "waitfor", "when", "where", "while", "with", "writetext"
                str = Left$(PREFIX_ON_KEYWORD & str, IDENT_MAX_SIZE - Len(SUFFIX_ON_KEYWORD)) & SUFFIX_ON_KEYWORD
    End Select
    ' As mentioned, we don't use this, but we could ;)
    'If NeedsQuotes = True Then
    '    str = Left$("[" & str, IDENT_MAX_SIZE - Len("[")) & "]"
    'End If
    TranslateName = str
End If

If DB_ENGINE = "MY" Then
    Select Case LCase$(str)
        Case "add", "all", "alter", "and", "as", "asc", "auto_increment", "between", _
        "bigint", "binary", "blob", "both", "by", "cascade", "char", "character", _
        "change", "check", "column", "columns", "create", "data", "datetime", "dec", _
        "decimal", "default", "delete", "desc", "describe", "distinct", "double", _
        "drop", "escaped", "enclosed", "explain", "fields", "float", "float4", _
        "float8", "foreign", "from", "for", "full", "grant", "group", "having", _
        "ignore", "in", "index", "infile", "insert", "int", "integer", "interval", _
        "int1", "int2", "int3", "int4", "int8", "into", "is", "key", "keys", _
        "leading", "like", "lines", "limit", "lock", "load", "long", "longblob", _
        "longtext", "match", "mediumblob", "mediumtext", "mediumint", "middleint", _
        "numeric", "not", "null", "on", "option", "optionally", "or", "order", _
        "outfile", "partial", "precision", "primary", "procedure", "privileges", _
        "read", "real", "references", "regexp", "repeat", "replace", "restrict", _
        "rlike", "select", "set", "show", "smallint", "sql_big_tables", _
        "sql_big_selects", "sql_select_limit", "straight_join", "table", "tables", _
        "terminated", "tinyblob", "tinytext", "tinyint", "trailing", "to", "unique", _
        "unlock", "unsigned", "update", "usage", "values", "varchar", "varying", _
        "with", "write", "where", "zerofill"
            str = Left$(PREFIX_ON_KEYWORD & str, IDENT_MAX_SIZE - Len(SUFFIX_ON_KEYWORD)) & SUFFIX_ON_KEYWORD
            Warn "Identifier " & strName & " is a reserved word for the target database. Converted to " & TranslateName
    End Select
End If

If DB_ENGINE = "ORACLE" Then
    Select Case LCase$(str)
    'todo: add reserved words; for now use firebird list
        Case _
              "bit_length", "both", "char_length", "character_length", "close", "connect", "cross", "disconnect", "fetch", "global", "insensitive", "leading", "lower", "octet_length", "open", "rows", "start", "trailing", "trim", "using", _
              "bigint", "case", "current_connection", "current_role", "current_transaction", "current_user", "recreate", "row_count", "release", "savepoint", _
              "active add", "admin", "after", "all", "alter", "and", "any", "as", "asc", "ascending", "at", "auto", "autoddl", "avg", "based", "base_name", "before", "begin", "between", "blob", "blobedit", "buffer", "by", "cast", "char", "character", "character_length", "char_length", "check", "check_point_length", "collate", "collation", "column", "commit", "committed", "compiletime", "computed", "close", "conditional", "connect", "constraint", "containing", "continue", "count", "create", "cstring", "current", "current_date", "current_time", "current_timestamp", "cursor", "database", "date", "day", "db_key", "debug", "dec", "decimal", "declare", "default", "delete", "desc", "descending", "describe", "descriptor", "disconnect", _
              "display", "distinct", "do", "domain", "double", "drop", "echo", "edit", "else", "end", "entry_point", "escape", "event", "exception", "execute", "exists", "exit", "extern", "external", "extract", "fetch", "file", "filter", "float", "for", "foreign", "found", "from", "full", "function", "gdscode", "generator", "gen_id", "global", "goto", "grant", "group", "group_commit_", "wait_time", "having", "help", "hour", "if", "immediate", "in", "inactive", "index", "indicator", "init", "inner", "input", "input_type", "insert", "int", "integer", "into", "is", "isolation", "isql", "join", "key", "lc_messages", "lc_type", "left", "length", "lev", "level", "like", "log_buffer_size", "long", "manual", _
              "max", "maximum", "maximum_segment", "max_segment", "merge", "message", "min", "minimum", "minutemodule_name", "month names", "national", "natural", "nchar", "no noauto", "not", "null", "numeric", "num_log_buffers", "octet_length of", "on", "only open", "option", "or", "order", "outer", "output", "output_type", "overflow", "page", "pagelength", "pages", "page_size", "parameter", "password", "plan", "position", "post_event", "precision", "prepare", "procedure", "protected", "primary", "privileges", "public", "quit", "rdb$db_key", "read", "real", "record_version", "references", "release", "reserv", "reserving", "retain", "return", "returning_values", "returns", "revoke", "right", "rollback", "runtime", "schema", "second", "segment", "select", "set", _
              "shadow", "shared", "shell", "show", "singular", "size", "smallint", "snapshot", "some", "sort", "sqlcode", "sqlerror", "sqlwarning", "stability", "starting", "starts", "statement", "static", "statistics", "sub_type", "sum", "suspend", "table", "terminator", "then", "time", "timestamp", "to", "transaction", "translate", "translation", "trigger", "trim", "uncommitted", "union", "unique", "update", "upper", "user", "using", "value", "values", "varchar", "variable", "varying", "version", "view", "wait", "when", "whenever", "where", "while", "with", "work", "write", "year"
            str = Left$(PREFIX_ON_KEYWORD & str, IDENT_MAX_SIZE - Len(SUFFIX_ON_KEYWORD)) & SUFFIX_ON_KEYWORD
            Warn "todo: proper reserved word checking for this database. For now, we're using the Firebird list."
    End Select
End If

If DB_ENGINE = "PG" Then
    Select Case LCase$(str)
    'todo: add reserved words; for now use firebird list
        Case _
              "bit_length", "both", "char_length", "character_length", "close", "connect", "cross", "disconnect", "fetch", "global", "insensitive", "leading", "lower", "octet_length", "open", "rows", "start", "trailing", "trim", "using", _
              "bigint", "case", "current_connection", "current_role", "current_transaction", "current_user", "recreate", "row_count", "release", "savepoint", _
              "active add", "admin", "after", "all", "alter", "and", "any", "as", "asc", "ascending", "at", "auto", "autoddl", "avg", "based", "base_name", "before", "begin", "between", "blob", "blobedit", "buffer", "by", "cast", "char", "character", "character_length", "char_length", "check", "check_point_length", "collate", "collation", "column", "commit", "committed", "compiletime", "computed", "close", "conditional", "connect", "constraint", "containing", "continue", "count", "create", "cstring", "current", "current_date", "current_time", "current_timestamp", "cursor", "database", "date", "day", "db_key", "debug", "dec", "decimal", "declare", "default", "delete", "desc", "descending", "describe", "descriptor", "disconnect", _
              "display", "distinct", "do", "domain", "double", "drop", "echo", "edit", "else", "end", "entry_point", "escape", "event", "exception", "execute", "exists", "exit", "extern", "external", "extract", "fetch", "file", "filter", "float", "for", "foreign", "found", "from", "full", "function", "gdscode", "generator", "gen_id", "global", "goto", "grant", "group", "group_commit_", "wait_time", "having", "help", "hour", "if", "immediate", "in", "inactive", "index", "indicator", "init", "inner", "input", "input_type", "insert", "int", "integer", "into", "is", "isolation", "isql", "join", "key", "lc_messages", "lc_type", "left", "length", "lev", "level", "like", "log_buffer_size", "long", "manual", _
              "max", "maximum", "maximum_segment", "max_segment", "merge", "message", "min", "minimum", "minutemodule_name", "month names", "national", "natural", "nchar", "no noauto", "not", "null", "numeric", "num_log_buffers", "octet_length of", "on", "only open", "option", "or", "order", "outer", "output", "output_type", "overflow", "page", "pagelength", "pages", "page_size", "parameter", "password", "plan", "position", "post_event", "precision", "prepare", "procedure", "protected", "primary", "privileges", "public", "quit", "rdb$db_key", "read", "real", "record_version", "references", "release", "reserv", "reserving", "retain", "return", "returning_values", "returns", "revoke", "right", "rollback", "runtime", "schema", "second", "segment", "select", "set", _
              "shadow", "shared", "shell", "show", "singular", "size", "smallint", "snapshot", "some", "sort", "sqlcode", "sqlerror", "sqlwarning", "stability", "starting", "starts", "statement", "static", "statistics", "sub_type", "sum", "suspend", "table", "terminator", "then", "time", "timestamp", "to", "transaction", "translate", "translation", "trigger", "trim", "uncommitted", "union", "unique", "update", "upper", "user", "using", "value", "values", "varchar", "variable", "varying", "version", "view", "wait", "when", "whenever", "where", "while", "with", "work", "write", "year"
            str = Left$(PREFIX_ON_KEYWORD & str, IDENT_MAX_SIZE - Len(SUFFIX_ON_KEYWORD)) & SUFFIX_ON_KEYWORD
            Warn "todo: proper reserved word checking for this database. For now, we're using the Firebird list."
    End Select
End If

If DB_ENGINE = "SQLITE" Then
    'http://www.sqlite.org/lang_keywords.html
    Select Case LCase$(str)
        Case _
                "abort", "action", "add", "after", "all", "alter", "analyze", "and", "as", "asc", "attach", "autoincrement", _
                "before", "begin", "between", "by", "cascade", "case", "cast", "check", "collate", "column", "commit", "conflict", "constraint", "cross", "current_date", "current_time", "current_timestamp", _
                "database", "default", "deferrable", "deferred", "delete", "desc", "detach", "distinct", "drop", _
                "each", "else", "end", "escape", "except", "exclusive", "exists", "explain", _
                "fail", "for", "foreign", "full", "glob", "group", "having", _
                "if", "ignore", "immediate", "in", "index", "indexed", "initially", "inner", "insert", "instead", "intersect", "into", "is", "isnull", _
                "join", "key", "left", "like", "limit", "match", "no", "not", "notnull", "null", _
                "of", "offset", "on", "or", "order", "outer", "plan", "pragma", "primary", "query", _
                "raise", "references", "regexp", "reindex", "release", "rename", "replace", "restrict", "right", "rollback", _
                "savepoint", "select", "set", "table", "temp", "temporary", "then", "to", "transaction", "trigger", _
                "union", "unique", "update", "using", "vacuum", "values", "view", "virtual", "when", "where"
            str = Left$(PREFIX_ON_KEYWORD & str, IDENT_MAX_SIZE - Len(SUFFIX_ON_KEYWORD)) & SUFFIX_ON_KEYWORD
    End Select
End If

If DB_ENGINE = "SYBASE" Then
    ' Blatantly stolen from MSSQL. Check whether this applies to Sybase
    ' Surround name with [ and ] would seem to be simplest.
    ' However, crazy MSSQL doesn't seem to like [s when running sys.sp_addextendedproperty
    ' so we might need to guess a bit more.
    'todo: expand this, list of reserved words
    ' We choose to not use quoted identifiers, so we need to deal with them:
    ' for sybase 12.5:
    'http://manuals.sybase.com/onlinebooks/group-as/asg1250e/refman/@Generic__BookTextView/26603
    ' We don't include SQL92 words right now
    'To find the names of existing objects that are reserved words, use sp_checkreswords.
    Select Case LCase$(str)
        Case _
                "add", "all", "alter", "and", "any", "arith_overflow", "as", "asc", "at", "authorization", "avgbegin", _
                "between", "break", "browse", "bulk", "by", "cascade", "case", "char_convert", "check", "checkpoint", "close", "clustered", _
                "coalesce", "commit", "compute", "confirm", "connect", "constraint", "continue", "controlrow", "convert", "count", "create", "current", _
                "cursor", "database", "dbcc", "deallocate", "declare", "default", "delete", "desc", "deterministic", "disk distinct", "double", "drop", "dummy", _
                "dump", "else", "end", "endtran", "errlvl", "errordata", "errorexit", "escape", "except", "exclusive", "exec", "execute", "exists", _
                "exit", "exp_row_size", "external", "fetch", "fillfactor", "for", "foreign", "from", "func", "function", "goto", "grant", "group", _
                "having", "holdlock", "identity", "identity_gap", "identity_insert", "identity_start", "if", "in", "index", "inout", "insert", "install", _
                "intersect", "into", "is", "isolation", "jar", "join", "key", "kill", "level", "like", "lineno", "load", "lock", "max", "max_rows_per_page", _
                "min", "mirror", "mirrorexit", "modify", "national", "new", "noholdlock", "nonclustered", "not", "null", "nullif", "numeric_truncation", "of", _
                "off", "offsets", "on", "once", "online", "only", "open", "option", "or", "order", "out", "output", "over", "partition", "perm", "permanent", _
                "plan", "precision", "prepare", "primary", "print", "privileges", "proc", "procedure", "processexit", "proxy_table", "public", "quiesce", _
                "raiserror", "read", "readpast", "readtext", "reconfigure", "references", "remove", "reorg", "replace", "replication", "reservepagegap", "return", _
                "returns", "revoke", "role", "rollback", "rowcount", "rows", "rule", "save", "schema", "select", "set", "setuser", "shared", "shutdown", "some", _
                "statistics", "stringsize", "stripe", "sum", "syb_identity", "syb_restree", "syb_terminate", "table", "temp", "temporary", "textsize", "to", "tran", _
                "transaction", "trigger", "truncate", "tsequal", "union", "unique", "unpartition", "update", "use", "user", "user_option", "using", "values", _
                "varying", "view", "waitfor", "when", "where", "while", "with", "work", "writetext"
        str = Left$(PREFIX_ON_KEYWORD & str, IDENT_MAX_SIZE - Len(SUFFIX_ON_KEYWORD)) & SUFFIX_ON_KEYWORD
    End Select
    ' If we do use quotes....
    'If NeedsQuotes = True Then
    '    str = Left$("[" & str, IDENT_MAX_SIZE - Len("[")) & "]"
    'End If
End If



Debug.Assert (Len(str) <= IDENT_MAX_SIZE) 'Verify translation doesn't exceed IDENT_MAX_SIZE

If strScope <> IGNOREFORTRANSLATION Then
    TranslationItem(3) = str 'translated name
    If TranslationItem(2) <> TranslationItem(3) Then
        ' The object name was translated
        TranslationItem(4) = "CHANGED"
    Else
        TranslationItem(4) = "UNCHANGED"
    End If

    'add translation, using (0) name scope, (1) parent name, (3) new name as key
    ' This makes sure not more than one object in the source db map to a certain object in the target db
    On Error Resume Next
    NameTranslations.Add TranslationItem, TranslationItem(0) & vbTab & TranslationItem(1) & vbTab & TranslationItem(3)
    Select Case Err.Number
    Case 0
        ' do nothing
    Case 457 ' item already exists in collection=>probably name too long so we get the same translations
    ' for different long source names
        ' for now, remove a random character in the middle, add 1 at the end and try again
        '(0) name scope, (1) parent name, (2) original name, (3) new name, (4) change indication
        Err.Number = 0
        Do
            i = 2 + (Rnd() * (Len(str) - 1)) 'character to remove
            str = Left$(str, i - 1) & Mid$(str, i + 1) & CStr(Int(Rnd() * 9))
            TranslationItem(3) = str
            TranslationItem(4) = "CHANGED"
            NameTranslations.Add TranslationItem, TranslationItem(0) & vbTab & TranslationItem(1) & vbTab & TranslationItem(3)
        Loop Until Err.Number = 0
    Case Else
        Err.Raise Err.Number, Err.Description
    End Select
    On Error GoTo TranslateName_Error
    
End If 'strObjectType <> IGNOREFORTRANSLATION
TranslateName = str 'Translated name

On Error Resume Next 'Ignore further errors when finalizing
TranslateName_Finalize:
Exit Function

TranslateName_Error:
Select Case Err.Number
Case Else
    MsgBox "Error: " & Err.Number & vbCrLf & _
        Err.Description & vbCrLf & _
        "in procedure TranslateName of Module basExportImportSQL" & IIf(Erl > 0, " in line " & Erl, "")
    Debug.Print Now() & " - basExportImportSQL.TranslateName error: " & Err.Description & " (Error number: " & Err.Number & ")" & IIf(Erl > 0, " in line " & Erl, "")
    'Log "Error " & Err.Number & vbCrLf & _
        Err.Description & vbCrLf & _
        "in procedure basExportImportSQL.TranslateName" & _
        Iif(Erl > 0, vbCrLf & "in line " & Erl, "")
End Select
Close
End 'just kill it
End Function
Private Function TranslateQuery( _
    strQueryName As String, _
    strSQL As String, _
    DB_ENGINE As String) As String
' Tries to translate query SQL depending on target database dialect
' strQueryName: Access query name, needed for looking up fields in translation collection
' strSQL: original Access SQL
' DB_ENGINE: target database

Dim strTempSQL As String
Dim lngFound As Long
On Error GoTo TranslateQuery_Error
strTempSQL = strSQL 'Let's work with this string in mangling the SQL

' Replace original field names with target field names
'todo: iterate through all translations with parent name strqueryname, scope Query,
' Replace original table names with target table names

' Replace functions etc. Note we're using case-sensitive binary compares to try and
' match the way Access capitalizes functions.
Select Case DB_ENGINE
Case "ACCESS"
    ' just use as-is; do nothing
Case "FIREBIRD"
    strTempSQL = Replace(strTempSQL, "Trim(", "LTRIM(RTRIM(", Compare:=vbBinaryCompare) 'note we should add a closing ), really
    strTempSQL = Replace(strTempSQL, Chr$(34) & " " & Chr$(34), "' '", Compare:=vbBinaryCompare) 'assume we're trying to add an empty space
    strTempSQL = Replace(strTempSQL, " & ' ' & ", " || ' ' || ", Compare:=vbBinaryCompare) 'text concatenation with space
    strTempSQL = Replace(strTempSQL, "=False", "=0", Compare:=vbBinaryCompare)
    strTempSQL = Replace(strTempSQL, "=True", "=1", Compare:=vbBinaryCompare)
Case "MSSQL"
    ' see eg http://weblogs.sqlteam.com/jeffs/archive/2007/03/30/Quick-Access-JET-SQL-to-T-SQL-Cheatsheet.aspx
    strTempSQL = Replace(strTempSQL, " & ' ' & ", " + ' ' + ", Compare:=vbBinaryCompare) 'text concatenation with space
    strTempSQL = Replace(strTempSQL, " & ", " + ", Compare:=vbBinaryCompare) 'assuming text concatenation. will go wrong with data such as 'Marks & Spencer'
    strTempSQL = Replace(strTempSQL, "=" & Chr$(34), vbTab & "--\3S~Af -- ='", Compare:=vbBinaryCompare) 'Assume the next " closes off this sequence
    lngFound = InStr(strTempSQL, vbTab & "--\3S~Af -- ='")
    Do Until lngFound = 0
        ' Let's find the first closing " and replace it with '. Of course, this might be wrong, but it
        ' is a step in the right direction anyway
        lngFound = InStr(lngFound, strTempSQL, Chr$(34)) 'look for end
        strTempSQL = Left(strTempSQL, lngFound - 1) & "'" & Mid(strTempSQL, lngFound + 1)
        lngFound = InStr(lngFound + 1, strTempSQL, vbTab & "--\3S~Af -- ='")
    Loop
    strTempSQL = Replace(strTempSQL, vbTab & "--\3S~Af -- ='", "='", Compare:=vbBinaryCompare) 'Correct our crazy replace
    strTempSQL = Replace(strTempSQL, "=False", "=0", Compare:=vbBinaryCompare)
    strTempSQL = Replace(strTempSQL, "=True", "=1", Compare:=vbBinaryCompare)
    strTempSQL = Replace(strTempSQL, "Asc(", "ASCII(", Compare:=vbBinaryCompare)
    strTempSQL = Replace(strTempSQL, "Chr(", "CHAR(", Compare:=vbBinaryCompare)
    strTempSQL = Replace(strTempSQL, "Instr(", "CHARINDEX(", Compare:=vbBinaryCompare)
    strTempSQL = Replace(strTempSQL, "LCase(", "LOWER(", Compare:=vbBinaryCompare) 'note we should add a closing ), really
    strTempSQL = Replace(strTempSQL, "Now()", "GetDate()", Compare:=vbBinaryCompare)
    strTempSQL = Replace(strTempSQL, "Nz(", "ISNULL(", Compare:=vbBinaryCompare)
    strTempSQL = Replace(strTempSQL, "ORDER BY ", "-- !please specify top! ORDER BY ", Compare:=vbBinaryCompare) 'The ORDER BY clause is invalid in views, inline functions, derived tables, subqueries, and common table expressions, unless TOP or FOR XML is also specified.
    strTempSQL = Replace(strTempSQL, "StrReverse(", "REVERSE(", Compare:=vbBinaryCompare)
    strTempSQL = Replace(strTempSQL, "Trim(", "LTRIM(RTRIM(", Compare:=vbBinaryCompare)
    lngFound = InStr(strTempSQL, "LTRIM(RTRIM(")
    Do Until lngFound = 0
        ' Let's find the first closing ) and double it up. Of course, this might be wrong, but it
        ' is a step in the right direction anyway
        lngFound = InStr(lngFound, strTempSQL, ")") 'look for end
        strTempSQL = Left(strTempSQL, lngFound) & ")" & Mid(strTempSQL, lngFound + 1)
        lngFound = InStr(lngFound + 1, strTempSQL, "LTRIM(RTRIM(")
    Loop
    strTempSQL = Replace(strTempSQL, "UCase(", "UPPER(", Compare:=vbBinaryCompare) 'note we should add a closing ), really
    strTempSQL = Replace(strTempSQL, Chr$(34) & " " & Chr$(34), "' '", Compare:=vbBinaryCompare) 'assume we're trying to add an empty space
Case Else
    Warn "Unknown database format for query conversion. Using original SQL.", remark
End Select
TranslateQuery = strTempSQL

On Error Resume Next 'Ignore further errors when finalizing
TranslateQuery_Finalize:
Exit Function

TranslateQuery_Error:
Select Case Err.Number
Case Else
    MsgBox "Error: " & Err.Number & vbCrLf & _
        Err.Description & vbCrLf & _
        "in procedure TranslateQuery of Module basExportImportSQL" & IIf(Erl > 0, " in line " & Erl, "")
    Debug.Print Now() & " - basExportImportSQL.TranslateQuery error: " & Err.Description & " (Error number: " & Err.Number & ")" & IIf(Erl > 0, " in line " & Erl, "")
    'Log "Error " & Err.Number & vbCrLf & _
        Err.Description & vbCrLf & _
        "in procedure basExportImportSQL.TranslateQuery" & _
        Iif(Erl > 0, vbCrLf & "in line " & Erl, "")
End Select
'just bluntly quit for now to aid troubleshooting
Close
End
Resume TranslateQuery_Finalize
    
End Function

Private Sub Warn(ByVal strWarning As String, _
    Optional Severity As WarningSeverity = WarningSeverity.warning)
' Send warning for script user to script file.
' If a warning is urgent enough, and DISPLAY_WARNINGS is true, show it directly to the user.
' Otherwise, store all warnings for one big list.
Dim strSeverityText As String
Select Case Severity
    Case WarningSeverity.warning
        strSeverityText = "Warning"
    Case WarningSeverity.remark
        strSeverityText = "Remark"
    Case Else
        strSeverityText = "Error"
End Select
If DISPLAY_WARNINGS Then
    Select Case Severity
        Case remark
            'do nothing
        Case Else
            MsgBox strWarning, vbOKOnly Or vbExclamation, strSeverityText
    End Select
End If
' Allow multi-line m_strWarningOutput:
strWarning = Replace(strWarning, vbCrLf, vbCrLf & COMMENT_PREFIX & " ")
' Now add text to module level m_strWarningOutput string for further processing.
' Don't forget to add line ending so we don't get one big long line.
m_strWarningOutput = m_strWarningOutput & COMMENT_PREFIX & " " & strSeverityText & ": " & strWarning & vbCrLf
End Sub

