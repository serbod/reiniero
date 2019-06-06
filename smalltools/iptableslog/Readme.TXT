iptableslogcsv
==============

Yes, I know, it's an awful name.

This freeware program reads IPTables generated rsyslog/syslog log files (e.g. /var/log/messages), filters out the iptables events, parses them and outputs them to a delimited text file with headers.

This tool can be useful for one-off import into spreadsheets and use with data analysis tools such as Microsoft LogParser.

Binaries provided for Windows and 64 bit Linux; should be compilable on Linux/OSX/FreeBSD.
Programmed in FreePascal using the Lazarus IDE (optional) (www.lazarus.freepascal.org). 
Released under the very liberal MIT license; see source code for details.
Get a FreePascal compiler: e.g. via http://freepascal.org/download.var

Free use, including commercial use, permitted, but no warranties or liability, express or implied, accepted: you got it for free, it's your responsibility ;) That said, bug reports are welcome.

https://bitbucket.org/reiniero/smalltools

Reinier Olislagers
2012-2013

Change history:
v1.3
- iptablesloggui: add export functionality

v1.2
- Fixes for shifting columns
- Started GUI viewer: iptablesloggui.exe

v1.1 
1 March 2012
- Fix for lines with early clock stamp values not being detected
- Fix for lines with MAC= field not being detected (Ubuntu UFW?)
- Improved guess for year
- Debug compilation supported; gives a bit more diagnostic output.

v1.0
Released

To do:
======
1. Compile and test on FreeBSD
2. Testing on (large amounts of) firewall logs for correctness (is the extracted data correct), completeness (are all fields extracted) and performance
3. Define sample log file and expected CSV file for automatic regression testing
4. Convert iptablesloggui to use csvdocument+bufdataset instead of sdfdataset so sorting is possible