JSON parser/builder units.

Supports UTF8 character set only.

Original source: FPC 2.7.1 r21678 fpjson implementation.
This has disadvantages: it converts JSON to system encoding (which is not UTF8 on Windows)

Applied Luiz Americo's patch in
http://bugs.freepascal.org/view.php?id=22197
to change TJSONStringType to string

Then adapted line jsonscannerutf8.pp line 244 to convert incoming json string data to UTF8 instead of system codepage.

Note: recent FPC trunk has an updated FPJSON that supports UTF8, making this forked version superfluous. fpctwit has been updated with supprot for this.

Todo:
- check rtti stuff