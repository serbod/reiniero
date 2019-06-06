@rem Compiles and runs flocate.lpr on a test directory and outputs results here
@rem uses fpc heaptrc unit and line numbering to get a detailed list of memory leaks

@rem Make sure we have the correct output directories for Windows:
mkdir output
mkdir output\i386-win32
mkdir output\x86_64-win64
@rem delete previous data in current dir
delp .
delp output\i386-win32
delp output\x86_64-win64
@rem also delete previous executable just to be sure:
del output\i386-win32\flocate.exe
del output\x86_64-win64\flocate.exe

@rem ====================================================================================================
@rem Options files etc that need to be distributed
@rem ====================================================================================================
xcopy /y flocate.ini output\i386-win32
xcopy /y flocate.ini output\x86_64-win64

@rem ====================================================================================================
@rem Database recreation script needs to be put into resource file
@rem ====================================================================================================
rem Create SQL script with DDL statements used to recreate an empty Firebird
rem database. In other words, get rid of all data and start with a fresh db.
rem Requires Firebird embedded exes in current dir.
rem We use the wini386 output for this and overwrite any databases present there
copy /y flocate.fdb output\i386-win32\
cd output\i386-win32
isql flocate.fdb -o flocate_database_schema.sql -x -u SYSDBA -p masterkey
copy /y flocate_database_schema.sql ..\..\
del flocate_database_schema.sql
rem This script will be added as a resource in flocate
cd ..\..

@rem ====================================================================================================
@rem Windows 64 bit
@rem ====================================================================================================
@rem Compile debug build and do a test run.
@rem -b Tells the compiler to show all procedure declarations if an overloaded function error occurs. 
@rem -FU: unit output directory
@rem -FE: executable output directory. We need to have our firebird libs there. Assuming Windows x64
@rem -g: Generate debugging information for debugging with GDB
@rem -gh: heaptrc
@rem -gl: use the lineinfo unit for line information
@rem todo: can we use macros or something? or detect environment ourselves?
@rem -OpATHLON64: optimize for X64=> let's not do this for debug runs; 
@rem (see eg bug 14530 in FreePascal; instead use
@rem -O-: disable optimizations
@rem -vewnh errors warnings notes hints
@rem: Probably don't use -gw together with -gh as it doesn't play nice.
setlocal
set path=c:\lazarus\fpc\2.5.1\bin\x86_64-win64;%path%
ppcx64 flocate.lpr -b -FUoutput\x86_64-win64\ -FEoutput\x86_64-win64\ -g -gh -gl -O- -vewnh 
@rem note we have to add a unit search path for the relevant widget type:
ppcx64 flocategui.lpr -b -FUc:\lazarus\lcl\units\x86_64-win64\win32\ -FUoutput\x86_64-win64\ -FEoutput\x86_64-win64\ -g -gh -gl -O- -vewnh 

@rem extended test; write output to testresult.csv and errors to testerr.txt:
@rem flocate -d %windir% > testresult.csv 2>testerr.txt
@rem faster test (substitute own test dir):
cd output\x86_64-win64
flocate d:\cop\testdir > testresult.csv 2>testerr.txt
endlocal

@rem ====================================================================================================
@rem Windows 32 bit
@rem ====================================================================================================
@rem Compile debug build only, no test run
@rem -b Tells the compiler to show all procedure declarations if an overloaded function error occurs. 
@rem -FU: unit output directory
@rem -FE: executable output directory. We need to have our firebird libs there. Assuming Windows x64
@rem -g: Generate debugging information for debugging with GDB
@rem -gh: heaptrc
@rem -gl: use the lineinfo unit for line information
@rem todo: can we use macros or something? or detect environment ourselves?
@rem -OpPENTIUM4: optimize for Pentium 4 => let's not do this for debug runs; 
@rem (see eg bug 14530 in FreePascal; instead use
@rem -O-: disable optimizations 
@rem -vewnh errors warnings notes hints
@rem: Probably don't use -gw together with -gh as it doesn't play nice.
@rem Uses hardcoded paths for now. Is there a solution for this? the 386 compiler is not in my path
setlocal
rem Old 2.4.x compiler
rem set path=C:\lazarus32\fpc\2.4.0\bin\i386-win32;%path%
rem Now using 2.5.1
set path=c:\fpc\bin\i386-win32;%path%
ppc386.exe flocate.lpr -b -Fi.\lib\i386-win32\ -Fu. -FUoutput\i386-win32\ -FEoutput\i386-win32\ -g -gh -gl -O- -vewnh -TWIN32
@rem note we have to add a unit search path for the relevant widget type:
@rem We're using a dev version of lazarus built with the 2.5.1 compiler as well
ppc386.exe flocategui.lpr -b  -Fi.\lib\i386-win32\ -FuC:\lazarusdev\lazarus\components\sqldb\lib\i386-win32\win32\ -FuC:\lazarusdev\lazarus\ideintf\units\i386-win32\ -FuC:\lazarusdev\lazarus\lcl\units\i386-win32\ -FuC:\lazarusdev\lazarus\lcl\units\i386-win32\win32\ -FuC:\lazarusdev\lazarus\packager\units\i386-win32\ -Fu. -Fulib\i386-win32\ -FUoutput\i386-win32\ -FEoutput\i386-win32\ -dLCL -dLCLwin32 -g -gh -gl -O- -vewnh -TWIN32

@rem extended test; write output to testresult.csv and errors to testerr.txt:
@rem flocate -d %windir% > testresult.csv 2>testerr.txt
@rem faster test (substitute own test dir):
cd output\i386-win32
flocate d:\cop\testdir > testresult.csv 2>testerr.txt

@rem open files with my new favorite text editor, though it's seemingly impossible
@rem to open 2 files with 1 command - whatever the help says.... 
"d:\Program Files (x86)\Notepad++\notepad++.exe" testerr.txt testresult.csv
@rem pspad testerr.txt
@rem pspad testresult.csv

@rem finally open the gui version. note you can't have both gui and command line
@rem open when using embedded firebird as db access will be blocked
flocategui
 
endlocal