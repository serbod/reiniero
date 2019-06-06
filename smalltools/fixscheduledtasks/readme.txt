Fixes corrupt scheduled tasks
Basically automates the steps mentioned in
http://support.microsoft.com/kb/2305420

Requires setacl.exe to be present in path
(note: this could perhaps be replaced by cacls/icacls/whatever ships with current Windows?)

Inspired by batch file in
http://gallery.technet.microsoft.com/scriptcenter/Repair-CorruptedTampered-c8d2e975
with some refinements - no need to specify a list of tasks.

For all scheduled tasks, the program
- exports the task XML definition file
- deletes the task from the registry and removes the XML definition file
- recreates the task from the exported file
thereby fixing corrupted tasks errors

Code refers to the steps in that KB article
NOTE:
- MUST be compiled with x64 compiler if you are running x64/64 bit Windows
- MUST be compiled with x86 compiler if you are running x86/32 bit Windows

Code works on my x64 Windows 7 installation.
Use at your own risk; open source (MIT license).