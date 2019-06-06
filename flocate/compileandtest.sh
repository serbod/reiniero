#!/bin/bash
# Compiles and runs flocate.lpr on a test directory and outputs results here
# uses fpc heaptrc unit and line numbering to get a detailed list of memory leaks
# make sure we have the correct output directories for linux:
mkdir -p output/x86_64-linux64/
mkdir -p output/i386-linux32/
# delete previous data
rm testresult.csv
rm testerr.txt

# Linux 32 bit
# Compile debug build and perform test run
# -b Tells the compiler to show all procedure declarations if an overloaded function error occurs.
# -g: Generate debugging information for debugging with GDB
# -gh: heaptrc to track memory useage
# -gl: use the lineinfo unit for line information
# -FU: unit output directory
# -FE: executable output directory. We need to have our firebird libs there. Assuming Linux x64
# todo: can we use macros or something? or detect environment ourselves?
# -OpPENTIUM4: optimize for Pentium 4 => let's not do this for debug runs; 
# see eg bug 14530 in FreePascal; instead use
# -O-: disable optimizations
# -vewnh errors warnings notes hints
# Probably don't use -gw together with -gh as it doesn't play nice.
ppc386 flocate.lpr -b  -FEoutput/i386-linux32/ -FUoutput/i386-linux32/ -g -gh -gl -O- -TLinux -vewnh
chmod ugo+rx output/i386-linux32/flocate
output/i386-linux32/flocate -d . > testresult.csv 2>testerr.txt
ppc386 flocategui.lpr -b  -FEoutput/i386-linux32/ -FUoutput/i386-linux32/ -g -gh -gl -O- -TLinux -vewnh
chmod ugo+rx output/i386-linux32/flocategui
output/i386-linux32/flocategui

# Linux 64 bit
# Compile debug build and perform test run.
# -b Tells the compiler to show all procedure declarations if an overloaded function error occurs.
# -g: Generate debugging information for debugging with GDB
# -gh: heaptrc to track memory useage
# -gl: use the lineinfo unit for line information
# -FU: unit output directory
# -FE: executable output directory. We need to have our firebird libs there. Assuming Linux x64
# todo: can we use macros or something? or detect environment ourselves?
# -OpPENTIUM4: optimize for Pentium 4 => let's not do this for debug runs; 
# see eg bug 14530 in FreePascal; instead use
# -O-: disable optimizations
# -vewnh errors warnings notes hints
# Probably don't use -gw together with -gh as it doesn't play nice.
ppcx64 flocate.lpr -b  -FEoutput/x86_64-linux64/ -FUoutput/x86_64-linux64/ -g -gh -gl -O- -TLinux -vewnh
chmod ugo+rx output/x86_64-linux64/flocate
output/x86_64-linux64/flocate -d . > testresult.csv 2>testerr.txt
ppcx64 flocategui.lpr -b  -FEoutput/x86_64-linux64/ -FUoutput/x86_64-linux64/ -g -gh -gl -O- -TLinux -vewnh
chmod ugo+rx output/x86_64-linux64/flocategui
output/x86_64-linux64/flocategui

echo Results in testresult.csv, errors in testerr.txt
echo Now showing testerr.txt:
less testerr.txt
echo Done showing testerr.txt
echo You can look at the results by using cat testresult.csv 
