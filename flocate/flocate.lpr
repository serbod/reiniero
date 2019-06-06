program flocate;

{*
Unix locate clone/derivative for multiple platforms (Windows, OS X, Linux).
Flocate stores file names and their directories into a database.
Users can then search for file names, mp3 tags, exif data etc.
It is also useful:
- for a forensic scan of a disk as it can read hashes of all
data
- to keep track of data on removable media/backups
======================================================================================

New architecture:
Directoryentry:
status (property Status):
- deleted (compared to earlier scan)
- same (as earlier scan)
- new
- modified
scanstatus (property Complete):
- incomplete
- complete (all required data present)
Remove filename,path, replace by fullfilename/fullpath
Pointers to additional structures:
- exif
- mp3
- exe info
- storage specific info (e.g. fileid when using databases)
Have a quick lookup data structure on full filename (e.g. hash list)

1. Scan directories, for each directory:
  If existing scan, get all (fullfilename,size,modification date,md5 hash) entries
  If db backend used add fileid
  Mark these status: deleted

2. Lookup entries found in list generated above

2.1 if not found: add as status: new, scanstatus: incomplete
if only metadata required, read metadata, mark scanstatus: complete

2.2 if found: compare modification date,size.
If forensic option on, compare md5 hash (then also read details, mark scanstatus:complete)
  2.2.1 If same: mark status: same as earlier scan, scanstatus: complete
  2.2.3 If not same: mark status: modified.
  If only metadata required, read metadata, mark scanstatus: complete

3. Traverse list, for all (new,modified) where scanstatus incomplete:
   scan metadata+data (metadata should have been covered in step 2 but perhaps nice from a
   resiliency point of view: step 2 can be changed without endangering data fidelity)

4. traverse list, save to storage (e.g. db):
Status: deleted: db: can be ignored
Status: same: db: simply insert file ids with proper scanid
Status: new: db: use stored proc or own code to add all required table info+foreign keys
Status: modified: db: same treatment as new


Questions/remarks
1. Assuming a repeated scan has mostly same files, so retrieving single records is faster than retrieving entire set of scan data
2. Verify this works for other storage mechanisms

Plans for improvement:
- check exif code; we still get read past end of file problems which we've masked using an exception handler
- adapt id3v2, code from http://www.optimasc.com/products/filelib/index.html
- database: create a username with role only for inserting and querying, without insert/delete rights on base tables.
  This will keep programmers honest as long as they don't know the SYSDBA password ;)
- database: add Windows ACLs? See partial implementation (names only, no permissions) http://lazarus.freepascal.org/index.php/topic,20235.0.html
- something like windows unit SysErrorMessage to get last message
- getting Office .doc/.ppt/.xls title/subject/keywords, maybe some more data. See microsoft code to do this in c++
- retrieval of results from database. indexing gui for cds/memory sticks/network shares.
- maybe full text index or plugin system to search for text in office docs and pdfs etc while indexing => look at Michael's indexer code
- implement unicode (unicodestring? utf8 in regular ansistring) when it is clear how to deal with FPC libraries/packages

Infeasible (on all platforms):
- network drive indexing=>no.
a Windows: network drives just drive letter? why not; ip address or hostname can also change; but also record where the share points to (net use)
=> use GetDriveType to detect drive type
http://msdn.microsoft.com/en-us/library/aa364939.aspx
-- use expanduncfilename to get also server name etc for network drives
b Linux/OSX: should be deducible in some way using OS API? (or fpc wrappers?)
=> use the stat system call to get the device a file resides on and go up the tree until you get to a different device.
===> but doesn't tell you if it's a network path or not.
Can't be done!?!
======================================================================================
MIT X11 License: no warranties, express or implied, but all other use permitted:
Copyright (c) 2010-2014 copyright holders

 Permission is hereby granted, free of charge, to any person
 obtaining a copy of this software and associated documentation
 files (the "Software"), to deal in the Software without
 restriction, including without limitation the rights to use,
 copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the
 Software is furnished to do so, subject to the following
 conditions:

 The above copyright notice and this permission notice shall be
 included in all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 OTHER DEALINGS IN THE SOFTWARE.
======================================================================================
*}
{$IFDEF FPC}{$mode objfpc}{$ENDIF}
{$INCLUDE flocatesettings.inc}
//Project-wide options.

//Enable to use our own exception handler to print more details on errors:
{.$DEFINE USEROUROWNEXCEPTIONHANDLER}


uses
  {$IFDEF UNIX}
  cwstring, {* widestring support for unix *}
  {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF UseCThreads}
  {$ENDIF UNIX}
  Classes,
  SysUtils,
  CustApp,
  search,
  flocateshared;

type
  { Tflocate }
  Tflocate = class(TCustomApplication)
  protected
    procedure DoRun;
      override;
    procedure GetOptions;
  public
    constructor Create(TheOwner: TComponent);
      override;
    destructor Destroy;
      override;
    procedure WriteHelp;
      virtual;
  end;

  { Tflocate }

  procedure Tflocate.GetOptions;
  // Get database etc options.

  // algorithm: command line options trump ini file in user dir trump ini file in system dir trump default settings
  // todo: pass this along to search/database objects -> or have them poll it for us!??!
  begin
    //todo: fill this with options code
  end;

  procedure Tflocate.DoRun;

  var
    ErrorMsg: string;
    ScanDirectory: string; // default: current directory where user started executable
    CustomCatalog: string; // If empty, use default catalog. If not: description of the custom catalog destined for the scan
    CustomScan: string; // If empty, use default scan.
    PerformScan: boolean; // Flag resulting from noscan option
    ListScan: boolean; // Flag resulting from listscan option.
    ClearData: boolean; // Flag resulting from cleardata option.
    ClearExistingScan: boolean; // Clear preexisting scan data?
    ReadFileContents: boolean; // Only read metadata (fast) or also file contents (and extract md5,mp3,exif etc info)
    Search: TFileSearch; // Search object
    TimeBegin,TimeEnd: TDateTime;
  begin
    // Set up default settings:
    ErrorMsg := '';
    ClearData := false;
    ClearExistingScan := false;
    ListScan := true;
    PerformScan := true;
    ReadFileContents := true;
    ScanDirectory := '';

    // Get any command line options:
    GetOptions;
    //in time, migrate the stuff below to that proc.
    // Check parameters; parse into results
    ErrorMsg := CheckOptions('cd:hlnpst', 'catalogdescription: clear directory: help listscan:: nocontent noscan scandescription:');
    if ErrorMsg <> '' then
    begin
      //ShowException(Exception.Create(ErrorMsg)); // a bit too harsh
      writeln('Wrong command or option specified: ');
      writeln(ErrorMsg);
      writeln();
      WriteHelp;
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    if HasOption('c', 'clear') then
    begin
      ClearData := true;
    end;

    if HasOption('d', 'directory') then
    begin
      // Use absolute path
      ScanDirectory := ExpandFileName(GetOptionValue('d', 'directory'));
      {$IFDEF DEBUG}
      writeln(stderr, 'Debug: ', DateTimeToStr(Now), ': Specified start directory: ',
        ScanDirectory);
      {$ENDIF}
    end;

    if HasOption('l', 'listscan') then
    begin
      //explicitly set by user
      case UpperCase(GetOptionValue('l', 'listscan')) of
      'N','NO','FALSE','DISABLE':
        ListScan:=false
      else //either yes, or no option given
        ListScan:=true;
      end;
    end;

    if HasOption('n', 'noscan') then
    begin
      PerformScan := false;
      //explicitly set by user
    end
    else
    begin
      //don't change current PerformScan
    end;

    if HasOption('p', 'nocontent') then
      ReadFileContents:=false;

    if HasOption('s', 'scandescription') then
    begin
      CustomScan:=GetOptionValue('s','scandescription');
    end
    else
    begin
      CustomScan:='';
    end;

    if HasOption('t', 'catalogdescription') then
    begin
      CustomCatalog:=GetOptionValue('t','catalogdescription');
    end
    else
    begin
      CustomCatalog:='';
    end;

    if PerformScan = true then
    // todo: For now, clear out existing scan. In future, merge with previous results. see todo list at top of file
      ClearExistingScan := true;

    begin
      if ScanDirectory = '' then
      begin
        // If directory is not assigned by user, use current directory
        // (not the application directory)
        GetDir(0, ScanDirectory);
      end;

      // Just before the actual work, let's check the options/values:
      {$IFDEF CRAZYDEBUG}
      writeln(stderr, 'Debug: ', DateTimeToStr(Now), ': Options:',
        '; Catalog:', CustomCatalog,
        '; ClearData:', ClearData,
        '; ClearExistingScan:', ClearExistingScan,
        '; ListScan:', ListScan,
        '; Nocontent:', not(ReadFileContents),
        '; PerformScan:', PerformScan,
        '; ScanDirectory:', ScanDirectory,
        '; ScanDescription:',CustomScan);
      {$ENDIF}

      // Now perform the actual search/result print
      Search := TFileSearch.Create(CustomCatalog,CustomScan);
      try
        try
          Search.RootDirectory := ScanDirectory;
          Search.Directory := false;
          Search.SearchSubdirectories := true;
          Search.Hidden := true;
          Search.ReadOnly := true;
          Search.System := true;
          Search.ReadFileContents := ReadFileContents;

          if ClearData = true then
          begin
            writeln(stderr, 'Clearing data...');
            // Removes ALL data from database. handy for troubleshooting.
            Search.ClearData;
          end
          else if ClearExistingScan then //options are mutually exclusive
          begin
            writeln(stderr, 'Clearing existing scan...');
            Search.ClearExistingScan;
          end;

          if PerformScan then
          begin
            TimeBegin:=Now;
            writeln(stderr, 'Performing scan...');
            Search.Search;
            TimeEnd:=Now;
            writeln(stderr, 'Finished scan in '+TimeToStr(TimeEnd-TimeBegin)+'... ');
          end;

          if ListScan then
            Search.PrintResults;
        except
          on E: Exception do
          begin
            writeln(stderr, 'Flocate: Error searching for files. Technical details: ',
              E.ClassName, '/', E.Message);
          end;
        end;
      finally
        Search.Free;
      end;
      // stop program loop
    end;
    Terminate;
  end;

  constructor Tflocate.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := true;
  end;

  destructor Tflocate.Destroy;
  begin
    inherited Destroy;
  end;

  procedure Tflocate.WriteHelp;
  begin
    writeln(Title);
    writeln('Usage: ');
    writeln(' -h            help');
    writeln(' -c            clear ALL data from database before search.');
    writeln(' -d directory');
    writeln(' --directory=directory ');
    writeln('               starting directory for search.');
    writeln('               (if no directory specified, start in current directory)');
    writeln(' -l');
    writeln(' --listscan    list/print results of scan to screen');
    writeln(' --listscan=no do not list/print results of scan to screen');
    writeln(' -n');
    writeln(' --noscan      no scan performed: doesn''t search for files;');
    writeln('               useful with -l option');
    writeln(' -p');
    writeln(' --nocontent   do not scan contents/md5 hash etc. Fast, less precise.');
    writeln(' -s description');
    writeln(' --scandescription=description');
    writeln('               specify scan description to select scan.');
    writeln('               If not given, use default (in default catalog).');
    writeln(' -t description');
    writeln(' --catalogdescription=description');
    writeln('               specify catalog description to select catalog.');
    writeln('               If not given, use default.');
    writeln('');
    writeln(' You can use flocate.ini to store database server details. If there');
    writeln(' is no flocate.ini, an embedded Firebird database is used.');
    writeln('');
    writeln('Freeware but no warranties, express or implied.');
    writeln('For full copyright and license, please see the source code.');
  end;

var
  Application: Tflocate;


{$IFDEF USEROUROWNEXCEPTIONHANDLER}
{$HINTS OFF}
  // We use FrameCount and Frames that get never used but are probably required for the exception handler.
  procedure MyExceptionHandler(Obj: TObject;       // ExceptObject
    Addr: Pointer;       // ExceptAddr
    FrameCount: longint; // ExceptFrameCount
    Frames: PPointer     // ExceptFrames
    );
  begin
    //Seems ShowException does not write to stderr but to stdout.
    writeln(stderr, DateTimeToStr(Now),
      ': An unhandled exception occurred. Please see console output for details.');
    writeln(DateTimeToStr(Now),
      ': An error occurred. Technical details (stack trace) below:');
    ShowException(obj, addr);
  end;
{$HINTS ON}
{$ENDIF}

{$R *.res}

begin
  {$IFDEF DEBUG}
  // Set up -gh output for the Leakview package:
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$ENDIF DEBUG}
  Application := Tflocate.Create(nil);
  Application.Title := 'flocate';
  {$IFDEF USEROUROWNEXCEPTIONHANDLER}
  //Custom exception handler.
  //test exception management
  exceptproc := @MyExceptionHandler;
  //Raise Exception.Create ('Program-generated exception/error to test error handling. Please call developer to fix this if you see this.');
  {$ENDIF}
  {$IFDEF CRAZYDEBUG}
  writeln(stderr, 'Debug: ', DateTimeToStr(Now), ': Application started.');
  {$ENDIF}
  Application.Run;
  {$IFDEF CRAZYDEBUG}
  writeln(stderr, 'Debug: ', DateTimeToStr(Now), ': Application finished.');
  {$ENDIF}
  Application.Free;
end.




