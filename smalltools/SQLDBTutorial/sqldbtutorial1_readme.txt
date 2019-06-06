1. About the sqldbtutorial1 sample project
==========================================
This example shows the current state of the SQLDB Tutorial 1 page on the Lazarus wiki:
http://wiki.lazarus.freepascal.org/SQLdb_Tutorial1

It demonstrates how to show, filter and edit database data on a grid.

2. Setting it up for your environment
=====================================
2.1 Database system
It is built for the employee.fdb demo database running on the Firebird database system.
You can use another database (e.g. MySQL, PostgreSQL, Oracle, SQLite or another database using ODBC): you'd have to have the proper database /table structure (see below), and use the relevant TSQLConnector descendant.

Please see the wiki for more details.

2.2 Connection options
Please review if the hostname, username and password properties for the IBConnection component on the form (IBConnection1) correspond with your environment. Correct them if necessary.

2.3 Database schema
If you don't have the employee sample database installed or are using a different database, here is a minimal version of the table that is used:
CREATE TABLE CUSTOMER
(
  CUST_NO CUSTNO NOT NULL,
  CUSTOMER VARCHAR(25) NOT NULL,
  CITY VARCHAR(25),
  COUNTRY VARCHAR(15),
  CONSTRAINT INTEG_60 PRIMARY KEY (CUST_NO)
);
 
Some data so you can at least show something:
INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES ('2001', 'Michael Design', 'San Diego', 'USA');
INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES ('2002', 'VC Technologies', 'Dallas', 'USA');
INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES ('2003', 'Klämpfl, Van Canneyt and Co.', 'Boston', 'USA');
INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES ('2004', 'Felipe Bank', 'Manchester', 'England');
INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES ('2005', 'Joost Systems, LTD.', 'Central Hong Kong', 'Hong Kong');
INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES ('2006', 'Van der Voort International', 'Ottawa', 'Canada');
INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES ('2007', 'Mrs. Mauvais', 'Pebble Beach', 'USA');
INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES ('2008', 'Asinine Vacation Rentals', 'Lihue', 'USA');
INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES ('2009', 'Fax', 'Turtle Island', 'Fiji');
INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES ('2010', 'FPC Corporation', 'Tokyo', 'Japan');
INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES ('2011', 'Dynamic Intelligence Corp', 'Zurich', 'Switzerland');
INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES ('2012', '3D-Pad Corp.', 'Paris', 'France');
INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES ('2013', 'Swen Export, Ltd.', 'Milan', 'Italy');
INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES ('2014', 'Graeme Consulting', 'Brussels', 'Belgium');
INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES ('2015', 'Klenin Inc.', 'Den Haag', 'Netherlands');
 
 
3. More information
===================
As mentioned, you can follow the tutorial on the wiki:
http://wiki.lazarus.freepascal.org/SQLdb_Tutorial1
If the wiki has been updated since release of the file, please feel free to submit a patch to the Lazarus source code.

Another very good example of database code is the lazdatadesktop tool in the tools directory.