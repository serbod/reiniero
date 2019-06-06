This code is meant to output fpcunit test results into a normalized database. 

This should allow:
- regression testing
- high volume storage (e.g. storing output from daily builds)

Currently, an fpcunit listener exists that outputs test results to the database 
as they are run.

Please copy testdbwriter.ini.txt to testdbwriter.ini in your project directory 
and adjust to the database you want to store your results in.
Please see the unit testdbwriter for more details.

Examples/useful code in the following directories:
- sampledbwriter: small sample that runs some tests and demonstrates how to work
  with testdbwriter.
- dbtests2db: console test runner that can be placed in the 
  $(fpcdir)\packages\fcl-db\tests\ directory to run the dbtestframework tests 
	and output the results to another database. Useful for regression testing the
	database tests.

In future, additional adapters may be written that take e.g. XML output and 
import it into the database, which is handy when diagnosing e.g. remote customer 
trouble reports.