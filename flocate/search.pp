unit search;


{*
Windows/Linux/Mac OSX FreePascal code to recursively search a directory or drive for all files.
Returns a list of objects with all retrieved file info, leaving unknown information blank.

adapted/modified from:
- http://cnx.com/TFileSearch.zip
- http://www.latiumsoftware.com/en/pascal/delphi-2.php
- new version: http://www.freepascal.org/docs-html/rtl/sysutils/findfirst.html
- http://forum.lazarus.freepascal.org/index.php/topic,8787.0.html
======================================================================================
MIT X11 License: no warranties, express or implied, but all other use permitted:
Copyright (c) 2010-2013 copyright holders

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
{$IFDEF FPC}{$mode objfpc}{$ENDIF} //If using the FPC compiler, use FPC Object Pascal mode.
{$INCLUDE flocatesettings.inc}
//Project-wide options.

interface

uses Classes, SysUtils, directoryentrylist,
  directoryentry { Found files are stored in these objects }
  {$IFDEF Windows}
  ,Windows { Windows specific file info }
  {$ENDIF}
  ,flocatecommon
  ,flocatedb
  ,sqldb {*temporarily, for print function*}
  ;

const
  InitialFileCapacity: integer = 250000;
//Used to get memory for list with file details. Hope reserving this speeds things up.

type

  { TFileSearch }
  TFileSearch = class
  private
    FCatalogDescription: string; //Uses 'Default catalog' for default
    FDB: TFlocateDB;
    FReadFileContents: boolean;
    FSubdir: boolean;
    FAttributes: longint;
    // Buffer that can be used by e.g. md5 hashes
    FFileBuffer: TFileBuffer; //Buffer used e.g. in md5 calculations
    // List of filters for searching
    FFilterList: TStringList;
    FRootDirectory: string;
    FScanDescription: string; //Uses 'Default scan' for default
    function GetAttributes(Index: longint): boolean;
    procedure SetAttributes(Index: longint; Value: boolean);
    procedure SetRootDirectory(s: String);
    function GetRootDirectory: String;
    procedure SetFilterList(const s: String);
    function GetFilterList: String;
    // Search directory ThisDir using filter ThisFilter
    // The heart of this class.
    procedure SearchPerFilter(const ThisDir, ThisFilter: String);
    // Add file to list of results
    procedure AddFileToResult(Results: TDirectoryEntryList; const ThisDir: String; item: TSearchRec);
  public
    // Pass desired descriptions for catalog and scan
    // Use empty strings for default catalog or scan
    constructor Create(CatalogDescription, ScanDescription: string);
    destructor Destroy;
      override;
    procedure AddFilter(s: string);
    // Runs the actual search and stores results
    procedure Search;
    // Print all found files to screen
    procedure PrintResults;
    procedure ClearFilters;
    procedure ClearData;
    // Removes any existing scan data from database
    procedure ClearExistingScan;
    // Should file contents be read and data extracted (md5, exif, mp3...)
    property ReadFileContents: boolean read FReadFileContents write FReadFileContents;
    // Whether or not to search in directories below the root directory
    property SearchSubdirectories: boolean read FSubdir write FSubdir default true;
    // Directory where the search should start: the search root directory
    property RootDirectory: String read GetRootDirectory write SetRootDirectory;
    property Filters: String read GetFilterList write SetFilterList;
    property ReadOnly: boolean Index faReadOnly read GetAttributes write SetAttributes;
    property Hidden: boolean Index faHidden read GetAttributes write SetAttributes;
    property Archive: boolean Index faArchive read GetAttributes write SetAttributes;
    property System: boolean Index faSysFile read GetAttributes write SetAttributes;
    property Directory: boolean Index faDirectory read GetAttributes write SetAttributes;
  end;

implementation


constructor TFileSearch.Create(CatalogDescription, ScanDescription: string);
begin
  FAttributes := faArchive or faHidden or faReadOnly or faSysFile or faAnyFile;
  if CatalogDescription='' then
    FCatalogDescription:=DEFAULTCATALOGDESCRIPTION
  else
    FCatalogDescription := CatalogDescription;
  //Search for anything we can find except symlinks, directories
  FFilterList := TStringList.Create;
  FFilterList.Duplicates := dupIgnore;
  FReadFileContents:=true;
  FDB := TFlocateDB.Create;
  // Convert here so db code is less complicated
  if ScanDescription='' then
    FScanDescription:=DEFAULTSCANDESCRIPTION
  else
    FScanDescription := ScanDescription;
  FSubdir := false;
  FRootDirectory := '';
end;


destructor TFileSearch.Destroy;
begin
  {$IFDEF CRAZYDEBUG}
  writeln(stderr, 'Debug: ', DateTimeToStr(Now),
    ': going to run ffilterlist.free: ');
  {$ENDIF}
  FFilterList.Free;
  {$IFDEF CRAZYDEBUG}
  writeln(stderr, 'Debug: ', DateTimeToStr(Now),
    ': going to run res.free: ');
  {$ENDIF}
  FDB.Free;
  inherited;
end;


procedure TFileSearch.SetRootDirectory(s: String);
begin
  if s='' then
    raise Exception.Create('Root directory may not be empty.');
  FRootDirectory := IncludeTrailingPathDelimiter(s);
end;

function TFileSearch.GetRootDirectory: String;
begin
  Result := FRootDirectory;
end;

procedure TFileSearch.AddFilter(s: string);
begin
  FFilterList.Add(s);
end;

function TFileSearch.GetAttributes(Index: longint): boolean;
begin
  Result := boolean(FAttributes and Index);
end;


procedure TFileSearch.SetAttributes(Index: longint; Value: boolean);
begin
  if Value then
    FAttributes := FAttributes or Index
  else
    FAttributes := FAttributes and not Index;
end;


procedure TFileSearch.SetFilterList(const s: String);
begin
  FFilterList.CommaText := StringReplace(s, ';', ',', [rfReplaceAll, rfIgnoreCase]);
end;

function TFileSearch.GetFilterList: String;
begin
  Result := FFilterList.CommaText;
end;

procedure TFileSearch.AddFileToResult(Results: TDirectoryEntryList; const ThisDir: String; item: TSearchRec);
// Finds out properties of file specified in item, adds this information
// to a record, and adds a pointer in the Res list to this record.
var
  DirEntry: TDirectoryEntry;
begin
  assert(ThisDir <> '', 'Directory to be searched may not be empty');
  assert(Item.Name <> '', 'File to be searched must have a filename');
  //Create a new file object and let it figure out all properties of the file.
  DirEntry := TDirectoryEntry.Create(ThisDir + Item.Name,
    @FFileBuffer,
    FReadFileContents);
  try
    try
      if Assigned(DirEntry) then
      begin
        Results.Add(DirEntry);
        {$IFDEF HELLFREEZESOVER}
        writeln(stderr, 'properties: ', (Res.Last as TDirectoryEntry).Print);
        {$ENDIF HELLFREEZESOVER}
      end
      else
      begin
{$IFDEF DEBUG}
        writeln(stderr, 'Debug: ', DateTimeToStr(Now),
          ': Could not create file info object for: ', ThisDir, Item.Name);
{$ENDIF}
      end;
    except
      //Error
      on E: Exception do
      begin
    {$IFDEF DEBUG}
        Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
          ': Error creating file info for: ', ThisDir, Item.Name,
          '. Technical details: ',
          E.Message
          );
    {$ENDIF}
        DirEntry.Free;
        //avoid memory leak
      end;
    end;
  finally
    // Clean up resources if required.
    //DON'T free the DirEntry as that will remove the object from the list!!!
  end;
end;

procedure TFileSearch.PrintResults;
// print all items in csv format to screen; include header and footer if specified.
// NOTE: not compatible with Lazarus/GUI code due to use of writeln
var
  ColumnCounter: integer;
  Counter: longword;
  ResultsQuery: TSQLQuery;
begin
  try
    if not(Assigned(FDB)) then
      raise Exception.Create('Problems printing. Technical details: TFDB not assigned');
    if not (IsConsole) then
      raise Exception.Create('This print procedure only works in console mode.');

    ResultsQuery:=TSQLQuery.Create(nil);
    try
      FDB.GetResults(FCatalogDescription,FScanDescription,ResultsQuery);
      if not(assigned(ResultsQuery)) then
      begin
        writeln('Error retrieving results from database.');
        exit;
      end;

      ResultsQuery.Open;
      ResultsQuery.First;
      Counter := 0;
      // Header
      for ColumnCounter := 0 to ResultsQuery.Fields.Count - 1 do
      begin
        case UpperCase(ResultsQuery.Fields[ColumnCounter].FieldName) of
        'CATALOGID','SCANID': { skip over these fields } ;
        else
          begin
            Write(ResultsQuery.Fields[ColumnCounter].DisplayLabel);
            if ColumnCounter < ResultsQuery.Fields.Count - 1 then
              Write(';');
          end;
        end;
      end;
      writeln('');
      //end of line

      while not ResultsQuery.EOF do
      begin
        for ColumnCounter := 0 to ResultsQuery.Fields.Count - 1 do
        begin
          case UpperCase(ResultsQuery.Fields[ColumnCounter].FieldName) of
          'CATALOGID','SCANID': { skip over these fields} ;
          else
            begin
              // FPC 2.6.4 still has a bug that does not trim the md5 field
              Write('"' + Trim(UTF8ToAnsi(ResultsQuery.Fields[ColumnCounter].DisplayText)) + '"');
              if ColumnCounter < ResultsQuery.Fields.Count - 1 then
                Write(';');
            end;
          end;
        end;
        writeln('');
        //end of line
        Counter := Counter + 1;
        ResultsQuery.Next;
      end;
      ResultsQuery.Close;
      // in a proper transaction, this should work
      // Hoping # is recognized as a CSV file comment.... ;)
      writeln('# Total number of files: ', Counter);
    finally
      ResultsQuery.Free;
    end;
  except
    on E: Exception do
    begin
      writeln('PrintResults: an error occurred. Technical details: '+E.Message);
    end;
  end;
end;


procedure TFileSearch.ClearData;
// Clear out data in database, not in this list.
begin
  // Open database. Database code can raise exceptions
  if not(Assigned(FDB)) then
    raise Exception.Create('Problems clearing data in database. Technical details: TFDB not assigned');

  FDB.ClearData;
end;

procedure TFileSearch.ClearExistingScan;
begin
  if not(Assigned(FDB)) then
    raise Exception.Create('Problems clearing scan in database. Technical details: TFDB not assigned');
  FDB.ClearExistingScan(FCatalogDescription, FScanDescription);
end;

procedure TFileSearch.SearchPerFilter(const ThisDir, ThisFilter: String);
// Search the specified directory for specified filter.
// If SearchSubdirectories is set and a directory item is found, then that
// directory is searched recursively.
var
  Item: TSearchRec;
  Att: longint;
  Results: TDirectoryEntryList;
  SearchRes: longint;
begin
  Results:=TDirectoryEntryList.Create(FDB);
  try
    //let DirectoryEntryList delete its own objects. Apparently default but doesn't hurt to specify:
    Results.OwnsObjects := true;
    //Reserve memory to store file results in:
    Results.Capacity := InitialFileCapacity;
    Results.Root:=FRootDirectory;
    Att := FAttributes;
    if SearchSubdirectories then
    begin
      SearchRes := SysUtils.FindFirst(ThisDir + '*', (faDirectory or Att or faSymlink), Item);
      try
        //also search in hidden, readonly etc dirs if we search for those files
        //We must not follow symlinks to directories as they
        //may lead to endless loops.
        while SearchRes = 0 do
        begin
          {$IFDEF HELLFREEZESOVER}
          writeln(stderr, 'Debug: ', DateTimeToStr(Now),
            ': find says ', Item.Name, ' is a directory? ',
            (Item.Attr and faDirectory) = faDirectory);
          writeln(stderr, 'Debug: ', DateTimeToStr(Now),
            ': find says ', Item.Name, ' is a symlink? ',
            (Item.Attr and faSymlink) = faSymLink);
          {$ENDIF HELLFREEZESOVER}
          // Filter out any symlink/reparse point, as well as current and parent directories:
          if (Item.Attr and faDirectory > 0) and (Item.Attr and faSymlink = 0) and (Item.Name <> '.') and
            (Item.Name <> '..')
  {$IFDEF Windows}
            and ((Item.FindData.dwFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT) = 0)
  {$ENDIF}
          then
          begin
            //Subdirectory; no symlink
  {$IFDEF DEBUG}
            writeln(stderr, 'Debug: ', DateTimeToStr(Now), ': into subdir ',
              Item.Name, ': attribs: $', IntToHex(Item.Attr, 4), ';faSymlink: $',
              IntToHex(faSymlink, 4), '; anded: ', Item.Attr and faSymlink);
  {$ENDIF}
            SearchPerFilter(ThisDir + Item.Name + DirectorySeparator, ThisFilter);
          end;
          SearchRes := SysUtils.FindNext(Item);
        end;
      finally
        SysUtils.FindClose(Item);
      end;
    end;
    //done with searching subdirectories
    SearchRes := SysUtils.FindFirst(ThisDir + ThisFilter, ((Att or faSymlink) and not faDirectory), Item);
    //Find non-directory files; don't follow symlinks.
    try
      while SearchRes = 0 do
      begin
        if (Item.Name <> '.') and (Item.Name <> '..') and (Item.Attr and faSymlink = 0)
  {$IFDEF Windows}
          and ((Item.FindData.dwFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT) = 0)
  {$ENDIF}
        then //added .. for Linux compatibility. Not sure if we really need it
        begin
          try
            AddFileToResult(Results, ThisDir, Item);
          except
            on E: EFOpenError do
            begin
              // Probably permission denied
              {$IFDEF DEBUG}
              writeln(stderr, 'Debug: ', DateTimeToStr(Now),
                ': ignoring exception '+E.Message+' while reading file : '+Item.Name);
              {$ENDIF}
            end;
          end;
        end
        else //it is a directory: . or .. or a reparse point/symlink
        begin
          // Just ignore it.
          //todo: maybe later add special class for symlinks??
  {$IFDEF DEBUG}
          writeln(stderr, 'Debug: ', DateTimeToStr(Now),
            ': found ., .., symlink or reparse point for file : ', Item.Name);
  {$ENDIF}
        end;
        //testing for dirs/reparse points
        SearchRes := SysUtils.FindNext(Item);
      end;
    finally
      SysUtils.FindClose(Item);
    end;
    Results.SaveToDatabase(FCatalogDescription,FScanDescription);
  finally
    Results.Free;
  end;
end;

procedure TFileSearch.ClearFilters;
begin
  FFilterList.Clear;
end;

procedure TFileSearch.Search;
// Go through each of the filters and search for them.
var
  Counter: longint;
begin
  if FFilterList.Count = 0 then
    Filters := '*'; // *.* is Windows-only
  for Counter := 0 to FFilterList.Count - 1 do
    SearchPerFilter(FRootDirectory, FFilterList[Counter]);
  // Commit any remaining pending saves
  FDB.SavePendingChanges;
end;

end.



