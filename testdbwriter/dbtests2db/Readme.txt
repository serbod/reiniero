dbtests2db is a program that runs the dbtestframework tests for a specified database (e.g. Firebird or bufdataset) and outputs the results to a database using testdbwriter.

Useful for Continuous Integration (CI) environments and for regression testing.

1. You will probably need an existing database to store test results, look in testdbwriter.pas for instructions. You can use an autocreated Firebird embedded database if you wish.
2. Please edit ..\testdbwriter.ini (if needed copy from ..\testdbwriter.ini.txt) to set up the connection to the database where the results will be stored. 
3. Copy:
dbtests2db.lpi
dbtests2db.lpr
..\testdbwriter.pas
..\testdbwriter.ini
..\testdbwriter.rc
..\testdbwriter_firebird.sql
(and any required database dlls)
to your $(fpcdir)\packages\fcl-db\tests directory.
4. Compile dbtests2db.lpi
5. Set up $(fpcdir)\packages\fcl-db\tests\database.ini as usual
6. Run dbtests2db.exe
	
Tested with FPC version: 2.6.1+.
