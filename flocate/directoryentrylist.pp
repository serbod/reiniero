unit directoryentrylist;

{*
Windows/Linux/Mac OSX FreePascal code to hold a list of DirectoryEntry objects
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

interface

uses
  Classes, SysUtils,
  Contnrs { from FCL for TObjectList/THashList },
  flocatecommon { constants etc },
  flocatedb { for saving results to database },
  directoryentry;

type
  TDirectoryEntryList = class(TFPObjectList) {* Inherit from this class *}
  // Can't use TFPHashObjectList because it hashes on a shortstring=>only 255 character filenames supported

  private
    FDB: TFlocateDB;
    FComputerName: string;
    FRoot: string;
    //Initialize data:
    procedure Init;
  protected
    function GetItem(Index: integer): TDirectoryEntry;
  public
    //Save everything to database.
    procedure SaveToDatabase(CatalogDescription,ScanDescription: string);
    constructor Create(TheDB: TFlocateDB);
    destructor Destroy;
      override;
  published
    // Computer with which we are searching.
    property ComputerName: String read FComputerName;
    // Root/starting directory of search
    property Root: String read FRoot write FRoot;
  end;
//class

implementation

uses
{$IFDEF Windows}
  Windows {* For computername *}
{$ENDIF}// Windows
{$IFDEF Unix}
  Unix {* For computername *}
{$ENDIF}//Unix
  ;

{$IFDEF Windows}
function GetWindowsComputerName: string;

var
  buffer: array[0..255] of char;
  size: dword;
begin
  size := 256;
  Result := '\\UNKNOWN//';
  //Fail by default; Invalid computer name in Windows, probably Unix, too.
  try
    if Windows.GetComputerName(buffer, size) then
    begin
      Result := buffer;
    end;
  except
    on E: Exception do
    begin
    {$IFDEF DEBUG}
      writeln(stderr, 'Debug: ', DateTimeToStr(Now),
        ': Error ', E.ClassName,
        '(', E.message, ') getting computer name. Using \\UNKNOWN//');
  {$ENDIF}
    end;
  end;
end;

{$ENDIF}
// Windows

procedure TDirectoryEntryList.Init;
begin
  //Get computer name
  FComputerName := '';
  //Default.
{$IFDEF Unix}
  FComputerName := Unix.GetHostName;
{$ENDIF}
{$IFDEF Windows}
  FComputerName := GetWindowsComputerName;
{$ENDIF}
end;

function TDirectoryEntryList.GetItem(Index: integer): TDirectoryEntry;
begin
  Result := TDirectoryEntry(Items[Index]);
end;

procedure TDirectoryEntryList.SaveToDatabase(CatalogDescription,ScanDescription: string);
// Save all entries to database.
// We could put this in directoryentry so we can lower the load
// for the db: insert on file found instead of all at once in a loop
// only useful if we use at least multiprocess (not embedded)
// or use a separate thread for adding database records
var
  Counter: longword;
  CurrentCount: integer;
  InsertFields: TflocateInsertSPSignature;
begin
  {$IFDEF CRAZYDEBUG}
  writeln(stderr, 'Debug: ', DateTimeToStr(Now),
    ': We found files: #', Count);
  {$ENDIF}
  if not(Assigned(FDB)) then
    raise Exception.Create('Problems saving to database. Technical details: TFDB not assigned');

  {$IFDEF CRAZYDEBUG}
  writeln(stderr, 'Debug: ', DateTimeToStr(Now),
    ': SaveToDatabase: number of files objects to save:', Count);
  {$ENDIF}
  Assert(FRoot<>'','Root may not be empty.');
  CurrentCount:=Self.Count;
  // Strange, FPC trunk seems to need this to avoid range errors
  if CurrentCount<=0 then
    exit;
  for Counter := 0 to CurrentCount - 1 do
  begin
    assert(Assigned(FDB),
      'The database object must be valid in order to save data.');
    //todo: this is not good. Can't we send the directory entry
    // to the db class to have it saved there? we're just copying property
    // values here! or do something with rtti? save all properties kind of thing
    // or tiopf or SOMETHING!
    InsertFields.COMPUTERNAME := FComputerName;
    InsertFields.ROOT := FRoot;
    InsertFields.DateAccessed :=
      TDirectoryEntry(Items[Counter]).TIMEACCESSED;
    InsertFields.DateCreated :=
      TDirectoryEntry(Items[Counter]).TIMECREATED;
    InsertFields.DateModified := TDirectoryEntry(Items[Counter]).TIMEMODIFIED;
    InsertFields.EXECOMPANY := TDirectoryEntry(Items[Counter]).EXECOMPANY;
    InsertFields.EXECOPYRIGHT := TDirectoryEntry(Items[Counter]).EXECOPYRIGHT;
    InsertFields.EXEDESCRIPTION := TDirectoryEntry(Items[Counter]).EXEDESCRIPTION;
    InsertFields.EXEFILEVERSION := TDirectoryEntry(Items[Counter]).EXEFILEVERSION;
    InsertFields.EXEINTERNALNAME :=
      TDirectoryEntry(Items[Counter]).EXEINTERNALNAME;
    InsertFields.EXEORIGINALFILENAME :=
      TDirectoryEntry(Items[Counter]).EXEORIGINALFILENAME;
    InsertFields.EXEPRODUCTNAME := TDirectoryEntry(Items[Counter]).EXEPRODUCTNAME;
    InsertFields.EXEPRODUCTVERSION :=
      TDirectoryEntry(Items[Counter]).EXEPRODUCTVERSION;
    InsertFields.EXIFAPERTURE := TDirectoryEntry(Items[Counter]).EXIFAPERTURE;
    InsertFields.EXIFARTIST := TDirectoryEntry(Items[Counter]).EXIFARTIST;
    InsertFields.EXIFCOMPRESSEDBPP :=
      TDirectoryEntry(Items[Counter]).EXIFCOMPRESSEDBPP;
    InsertFields.EXIFCOPYRIGHT := TDirectoryEntry(Items[Counter]).EXIFCOPYRIGHT;
    InsertFields.EXIFDATETIME := TDirectoryEntry(Items[Counter]).EXIFDATETIME;
    InsertFields.EXIFDATETIMEDIGITIZED :=
      TDirectoryEntry(Items[Counter]).EXIFDATETIMEDIGITIZED;
    InsertFields.EXIFDATETIMEORIGINAL :=
      TDirectoryEntry(Items[Counter]).EXIFDATETIMEORIGINAL;
    InsertFields.EXIFEXPOSURE := TDirectoryEntry(Items[Counter]).EXIFEXPOSURE;
    InsertFields.EXIFEXPOSUREPROGRAM :=
      TDirectoryEntry(Items[Counter]).EXIFEXPOSUREPROGRAM;
    InsertFields.EXIFFLASH := TDirectoryEntry(Items[Counter]).EXIFFLASH;
    InsertFields.EXIFFSTOPS := TDirectoryEntry(Items[Counter]).EXIFFSTOPS;
    InsertFields.EXIFIMAGEDESCRIPTION :=
      TDirectoryEntry(Items[Counter]).EXIFIMAGEDESCRIPTION;
    InsertFields.EXIFISO := TDirectoryEntry(Items[Counter]).EXIFISO;
    InsertFields.EXIFLIGHTSOURCE :=
      TDirectoryEntry(Items[Counter]).EXIFLIGHTSOURCE;
    InsertFields.EXIFMAKE := TDirectoryEntry(Items[Counter]).EXIFMAKE;
    InsertFields.EXIFMAXAPERTURE :=
      TDirectoryEntry(Items[Counter]).EXIFMAXAPERTURE;
    InsertFields.EXIFMETERINGMETHOD :=
      TDirectoryEntry(Items[Counter]).EXIFMETERINGMETHOD;
    InsertFields.EXIFMETERINGMODE :=
      TDirectoryEntry(Items[Counter]).EXIFMETERINGMODE;
    InsertFields.exifmodel := TDirectoryEntry(Items[Counter]).EXIFMODEL;
    InsertFields.EXIFORIENTATION :=
      TDirectoryEntry(Items[Counter]).EXIFORIENTATION;
    InsertFields.EXIFPIXELXDIMENSION :=
      TDirectoryEntry(Items[Counter]).EXIFPIXELXDIMENSION;
    InsertFields.EXIFPIXELYDIMENSION :=
      TDirectoryEntry(Items[Counter]).EXIFPIXELYDIMENSION;
    InsertFields.EXIFSHUTTERSPEED :=
      TDirectoryEntry(Items[Counter]).EXIFSHUTTERSPEED;
    InsertFields.EXIFSOFTWARE := TDirectoryEntry(Items[Counter]).EXIFSOFTWARE;
    InsertFields.EXIFUSERCOMMENTS :=
      TDirectoryEntry(Items[Counter]).EXIFUSERCOMMENTS;
    InsertFields.EXIFXRESOLUTION :=
      TDirectoryEntry(Items[Counter]).EXIFXRESOLUTION;
    InsertFields.EXIFYRESOLUTION :=
      TDirectoryEntry(Items[Counter]).EXIFYRESOLUTION;
    InsertFields.FILEDESCRIPTION :=
      TDirectoryEntry(Items[Counter]).FILEDESCRIPTION;
    InsertFields.FILENAME := TDirectoryEntry(Items[Counter]).FILENAME;
    InsertFields.FILEPATH := TDirectoryEntry(Items[Counter]).Directory;
    InsertFields.FILESIZE := TDirectoryEntry(Items[Counter]).SIZE;
    InsertFields.FILETYPE := TDirectoryEntry(Items[Counter]).FILETYPE;
    InsertFields.MD5VALID := TDirectoryEntry(Items[Counter]).MD5Valid;
    InsertFields.MD5HASH := TDirectoryEntry(Items[Counter]).MD5HASH;
    InsertFields.MP3ALBUM := TDirectoryEntry(Items[Counter]).MP3ALBUM;
    InsertFields.MP3ARTIST := TDirectoryEntry(Items[Counter]).MP3ARTIST;
    InsertFields.MP3COMMENT := TDirectoryEntry(Items[Counter]).MP3COMMENT;
    InsertFields.MP3GENRE := TDirectoryEntry(Items[Counter]).MP3GENRE;
    InsertFields.MP3TITLE := TDirectoryEntry(Items[Counter]).MP3TITLE;
    InsertFields.MP3TRACK := TDirectoryEntry(Items[Counter]).MP3TRACK;
    InsertFields.MP3YEAR := TDirectoryEntry(Items[Counter]).MP3YEAR;
    InsertFields.UNIXVALIDDATA := TDirectoryEntry(Items[Counter]).UnixValidData;
    InsertFields.UNIXUID := TDirectoryEntry(Items[Counter]).UNIXUID;
    InsertFields.UNIXGID := TDirectoryEntry(Items[Counter]).UNIXGID;
    InsertFields.UNIXMODE := TDirectoryEntry(Items[Counter]).UNIXMODE;
    FDB.SaveDirectoryEntry(CatalogDescription,FRoot,ScanDescription,InsertFields);
  end;
end;

function CompareByPath(dir1, dir2: TDirectoryEntry): integer;
  // Compares based on simple alpha sort of paths
  // Could be used by a sort function
begin
  if dir1.Directory < dir2.Directory then
  begin
    Result := -1;
  end
  else // not smaller
  begin
    if dir1.Directory > dir2.Directory then
    begin
      Result := 1;
    end
    else
    begin
      // Dig deeper, less likely this codepath will execute
      if dir1.FullPath < dir2.FullPath then
      begin
        Result := -1;
      end
      else
      if dir1.FullPath > dir2.FullPath then
      begin
        Result := 1;
      end
      else
      begin
        Result := 0;
        //Not smaller, not bigger: they must be the same
      end;
    end;
  end;
end;

constructor TDirectoryEntryList.Create(TheDB: TFlocateDB);
begin
  inherited Create;
  FDB:=TheDB;
  Init;
  //Initialize data
end;

destructor TDirectoryEntryList.Destroy;
begin
  //Clear; //is called by parent destructor anyway.
  {$IFDEF CRAZYDEBUG}
  writeln(stderr, 'Debug: ', DateTimeToStr(Now),
    ': TDirectoryEntrylist.Destroy: object count is now:', Count);
  {$ENDIF}
  FDB:=nil; //remove our reference; let calling class clean up their TheDB
  inherited Destroy;
end;


end.


