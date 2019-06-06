unit directoryentry;

{*
Includes calls to other units/code to:
- give the MD5 hash of a file
- give additional info on jpeg files (exif info)
- give additional info on mp3 files
- give file (resource) info for executables (Windows only)
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
  flocatecommon,
  md5 {*md5 hash for files; use FPC trunk August 2014 for assembler implementation *}, typinfo {for getenumname}
  {$IFDEF WINDOWS}
  , Windows {*Windows specific file info*}
  {$ENDIF}

  {$IF (FPC_FULLVERSION>=20701)}
  // New fileinfo reads exe resources as long as you register the appropriate units
    , fileinfo, winpeimagereader {need this for reading exe info}
  {$ELSE}
  {$IFDEF WINDOWS}
  //*nix: the fpc26 fileinfo is win only
  , fileinfo
  {$ENDIF WINDOWS}
  {$ENDIF FPC_FULLVERSION}

  {$IFDEF UNIX}
  , baseunix { for file time }
  {$ENDIF}
  , dateutils
  ;


type
  // Status of an entry (compared to previous scans).
  deEntryStatus = (deDeleted,deSame,deNew,deModified);
  //Types of files we're interested in and will do additional processing for.
  //All other file types are other. On Linux we could use the file program to get an idea
  //but for now just use the extension to determine file type.
  //maybe there's some authoritative list? mime?
  //We put other first in the enum: if getenumname errors out, we get the first item in the list.
  enumFiletype = (other, exe, dll, ocx, jpg, mp3, doc, ppt, xls, html, Text);

  { TDirectoryentry }

  TDirectoryEntry = class(TObject)
  private
    FComplete: boolean;
    // Directory where result file is.
    FDirectory: String;
    // Executable company info
    FExeCompany: String;
    // Executable copyright
    FExeCopyright: String;
    // Executable description
    FExeDescription: String;
    // Executable internal name
    FExeInternalName: String;
    // Executable original filename
    FExeOriginalFilename: String;
    // Executable file version
    FExeFileVersion: String;
    // Executable productname
    FExeProductName: String;
    // Executable productversion
    FExeProductVersion: String;
    FExifAperture: String;
    FExifArtist: String;
    FExifCompressedBPP: String;
    FExifCopyright: String;
    FExifDateTime: TDateTime;
    FExifDateTimeDigitized: TDateTime;
    FExifDateTimeOriginal: TDateTime;
    FExifExposure: String;
    FExifExposureProgram: String;
    FExifFlash: String;
    FExifFStops: String;
    FExifImageDescription: String;
    FExifISO: word;
    FExifLightSource: String;
    FExifMake: String;
    FExifMaxAperture: String;
    FExifMeteringMethod: String;
    FExifMeteringMode: String;
    FExifModel: String;
    // Exif pictures; orientation described
    FExifOrientation: String;
    FExifPixelXDimension: cardinal;
    FExifPixelYDimension: cardinal;
    FExifShutterSpeed: String;
    FExifSoftware: String;
    FExifUserComments: String;
    FExifXResolution: cardinal;
    FExifYResolution: cardinal;
    FFileBuffer: PFileBuffer; //pointer to existing buffer for e.g. md5 etc can be reused.
    // User-generated description of the file. Could also be program-generated if useful.
    FFileDescription: String;
    // Filename of our file
    FFileName: String;
    // Type of file
    FFileType: enumFileType;
    // Absolute path - directory and filename - of file
    FFullPath: String;
    // MD5 hash of file
    FMD5Hash: TMD5Digest;
    // If we get an error calculating the hash it is set to a hash of an empty file. This flag helps prevent output mistakes
    FMD5Valid: boolean;

    // MP3 Album name
    FMP3Album: String;
    // MP3 Artist name
    FMP3Artist: String;
    // MP3 comment
    FMP3Comment: String;
    // MP3 genre
    FMP3Genre: String;
    // MP3 song title
    FMP3Title: String;
    // MP3 track number
    FMP3Track: byte;
    // MP3 year
    FMP3Year: integer;
    FStatus: deEntryStatus;
    // If no, ignore FUnixUID,FUnixGID,FUnixMode
    FUnixValidData: boolean;
    // Unix file user id (e.g. 0=root)
    FUnixUID: cardinal;
    // Unix file group id
    FUnixGID: cardinal;
    // Unix mode/file permissions (rwx etc)
    FUnixMode: cardinal;
    // Whether or not to read contents and get md5hash etc.
    FReadFileContents: boolean;
    // File size in bytes; size matches RTL/FCL declaration
    FSize: int64;
    // File acccess time - atime in Unix. Note: atime is often disabled in mount options (noatime) to increase speed.
    FTimeAccessed: TDateTime;
    //Time file was created (Windows) - different from normal unix ctime: directory entry last changed (e.g. owner changed).
    FTimeCreated: TDateTime;
    // Time file was last modified - mtime in Unix:
    FTimeModified: TDateTime;
    procedure Init;
    // Reads metadata on relevant file, and data if ReadFileContents is set:
    procedure ReadInfo;
    // Reads in MD5 hash for file.
    procedure ReadMD5Hash;
    {$IF (DEFINED(Windows)) OR (FPC_FULLVERSION>=20701)}
    // Reads windows executable info
    procedure GetExeInfo(var WindowsInfo: TFileVersionInfo);
    {$ENDIF (DEFINED(Windows)) OR (FPC_FULLVERSION>=20701)}
    // Determines whether file is jpeg, has exif and reads it
    procedure GetExifInfo;
    // Reads ID3 tags from mp3 files
    procedure GetMP3Info;
    // Gets file type
    function GetFileType: enumFileType;
    function PrintFormatFromDate(SourceDateTime: TDateTime): string;
    function PrintFormatFromString(SourceString: String): String;
    // Converts file type enum to string
    function StringFromFileType: String;
    // Formats md5 hash as a string
    function ShowMD5Hash: shortstring;
    // Clear out exif data
    procedure EmptyExifData;
    // Clear out Windows exe data
    procedure EmptyExeData;
  public
    constructor Create(FullPath: String; FileBufferPointer: PFileBuffer);
    constructor Create(FullPath: string; FileBufferPointer: PFileBuffer; ReadFileContents: boolean);
    destructor Destroy;
      override;
    //Print output of all property names in CSV format (; separated)
    function PrintPropertyNames: String;
    //Print output of all property values in CSV format (; separated)
    //Output properties to console. Useful for debugging or output. //todo: overload, add column/csv output; output to stderr
    function Print: String;
    //Binary representation of md5 hash
    property MD5HashArray: TMD5Digest read FMD5Hash;
  published
    {* used published instead of public to enable streaming to/from disk, and RTTI for database access *}
    // Indicate whether scan/data collection is complete or not
    property Complete: boolean read FComplete write FComplete;
    //Directory where result file is.
    property Directory: String read FDirectory;
    //Executable company info
    property ExeCompany: String read FExeCompany;
    //Executable copyright
    property ExeCopyright: String read FExeCopyright;
    //Executable description
    property ExeDescription: String read FExeDescription;
    //Executable file version
    property ExeFileVersion: String read FExeFileVersion;
    //Executable internal name
    property ExeInternalName: String read FExeInternalName;
    //Executable original filename
    property ExeOriginalFilename: String read FExeOriginalFilename;
    //Executable productname
    property ExeProductName: String read FExeProductName;
    //Executable productversion
    property ExeProductVersion: String read FExeProductVersion;
    //Exif pictures
    property ExifAperture: String read FExifAperture;
    //Exif pictures
    property ExifArtist: String read FExifArtist;
    //Exif pictures
    property ExifCompressedBPP: String read FExifCompressedBPP;
    //Exif pictures
    property ExifCopyright: String read FExifCopyright;
    //Exif pictures
    property ExifDateTime: TDateTime read FExifDateTime;
    // Exif pictures:
    property ExifDateTimeDigitized: TDateTime read FExifDateTimeDigitized;
    // Exif pictures:
    property ExifDateTimeOriginal: TDateTime read FExifDateTimeOriginal;
    // Exif pictures:
    property ExifExposure: String read FExifExposure;
    //Exif pictures
    property ExifExposureProgram: String read FExifExposureProgram;
    //Exif pictures
    property ExifFlash: String read FExifFlash;
    //Exif pictures
    property ExifFStops: String read FExifFStops;
    //Exif pictures
    property ExifImageDescription: String read FExifImageDescription;
    //Exif pictures
    property ExifIso: word read FExifIso;
    //Exif pictures
    property ExifLightSource: String read FExifLightSource;
    //Exif pictures
    property ExifMake: String read FExifMake;
    //Exif pictures
    property ExifModel: String read FExifModel;
    //Exif pictures
    property ExifMaxAperture: String read FExifMaxAperture;
    //Exif pictures
    property ExifMeteringMethod: String read FExifMeteringMethod;
    //Exif pictures
    property ExifMeteringMode: String read FExifMeteringMode;
    //Exif pictures
    property ExifOrientation: String read FExifOrientation;
    //Exif pictures; orientation described
    property ExifPixelXDimension: cardinal read FExifPixelXDimension;
    //Exif pictures
    property ExifPixelYDimension: cardinal read FExifPixelYDimension;
    //Exif pictures
    property ExifShutterSpeed: String read FExifShutterSpeed;
    //Exif pictures
    property ExifSoftware: String read FExifSoftware;
    //Exif pictures
    property ExifUserComments: String read FExifUserComments;
    //Exif pictures
    property ExifXResolution: cardinal read FExifXResolution;
    //Exif pictures
    property ExifYResolution: cardinal read FExifYResolution;
    property FileDescription: String read FFileDescription;
    //Filename of result file
    property FileName: String read FFileName;
    property FileType: String read StringFromFileType;
    //Directory and filename concatenated
    property FullPath: String read FFullPath;
    // Hex representation of MD5 hash of file
    property MD5Hash: Shortstring read ShowMD5Hash;
    // Indicates whether there is a valid MD5 hash. If not, MD5Hash is empty
    // Needed in database operations.
    property MD5Valid: boolean read FMD5Valid;
    // MP3 tag info
    property MP3Album: String read FMP3Album;
    // MP3 tag info
    property MP3Artist: String read FMP3Artist;
    // MP3 tag info
    property MP3Comment: String read FMP3Comment;
    // MP3 tag info
    property MP3Genre: String read FMP3Genre;
    // MP3 tag info
    property MP3Title: String read FMP3Title;
    // MP3 tag info
    property MP3Track: byte read FMP3Track;
    // MP3 tag info
    property MP3Year: integer read FMP3Year;
    // If yes, UnixUID, UnixGID, UnixMode has been read. If no, these properties should be regarded as NULL
    property UnixValidData: boolean read FUnixValidData;
    // User ID on *nix systems (e.g. 0=root)
    property UnixUID: cardinal read FUnixUID;
    // Group ID on *nix systems
    property UnixGID: cardinal read FUnixGID;
    // Mode/permissions (rwx etc) on *nix systems
    property UnixMode: cardinal read FUnixMode;

    //If set, read file contents, and get md5 hash etc. If not, only get directory metadata. Can save a huge amount in time.
    property ReadFileContents: boolean read FReadFileContents write FReadFileContents;

    //File size in bytes; variable type matches FCL/RTL definition
    //todo: add in unix equivalents
    property Size: int64 read FSize;
    // Time file was last accessed - note: local time? not GMT? todo: look at this
    property TimeAccessed: TDateTime read FTimeAccessed;
    // Time file was created - note: local time? not GMT? todo: look at this
    property TimeCreated: TDateTime read FTimeCreated;
    // Time file was modified - note: local time? not GMT? todo: look at this
    property TimeModified: TDateTime read FTimeModified;

    // Status (compared to previous scan).
    property Status: deEntryStatus read FStatus;
  end;

implementation

uses
  exif { exif info for jpeg files },
  mp3id3tag { i3d info for mp3 files };

//const
  //MaxSizeForMD5: Cardinal = 1073741824; //Don't calculate hash if bigger unless in forensic mode (to be implemented). Should be about 10 megabytes.

// Adapted from MDFile in md5 unit
function CustomMD5File(const Filename: String; BufferPointer: PFileBuffer): TMDDigest;
const
  Version=MD_VERSION_5; //calculate md5 format hash
var
  F: File;
  Context: TMDContext;
  Count: Cardinal;
  OldFileMode: Longint;
begin
  MDInit(Context, Version);

  Assign(F, Filename);
  {$push}{$i-}
  OldFileMode := FileMode;
  FileMode := 0;
  Reset(F, 1);
  {$pop}

  try
    if IOResult = 0 then
    begin
      repeat
        BlockRead(F, BufferPointer^, Bufsize, Count);
        if Count > 0 then
          MDUpdate(Context, BufferPointer^, Count);
      until Count < BufSize;
      Close(F);
    end;

    MDFinal(Context, Result);
  finally
    FileMode := OldFileMode;
  end;
end;

procedure TDirectoryEntry.Init;
// Todo: init is used both in creating and destroying object. check use of finalize.
begin
  Finalize(FDirectory);
  Finalize(FFileName);
  FFileType := other;
  Finalize(FFullPath);
  // MD5:
  FMD5Valid := false;
  Finalize(FMD5Hash);
  // Mp3:
  Finalize(FMP3Album);
  Finalize(FMP3Artist);
  Finalize(FMP3Comment);
  Finalize(FMP3Genre);
  Finalize(FMP3Title);
  FMP3Track := 0;
  FMP3Year := 0;
  FUnixValidData:=false;
  FUnixUID := 0;
  FUnixGID := 0;
  FUnixMode := 0;
  FReadFileContents := true;
  //by default, do read file contents.
  FSize := 0;
  FTimeAccessed := UnknownDateTime;
  FTimeCreated := UnknownDateTime;
  FTimeModified := UnknownDateTime;

  FStatus:=deDeleted;
  EmptyExeData;
  EmptyExifData;
end;

{$IFDEF WINDOWS}
function WindowsFileTimeToUTC(const FileTime: TFileTime): TDateTime;
// Converts date/time stored in windows file to UTC time for uniform storage
var
  LocalTime: TFileTime;
  DOSTime: integer;
begin
  try
    //Make compiler happy by initializing vars:
    DOSTime := 0;
    LocalTime.dwHighDateTime := 0;
    LocalTime.dwLowDateTime := 0;
    FileTimeToLocalFileTime(FileTime, LocalTime);
    FileTimeToDosDateTime(LocalTime, LongRec(DOSTime).Hi, LongRec(DOSTime).Lo);
    Result:=LocalTimeToUniversal(FileDateToDateTime(DOSTime));
  except
{$IFDEF DEBUG}
    writeln(stderr, 'Debug: ', DateTimeToStr(Now), ': error getting file time info.');
{$ENDIF}
    Result := UnknownDateTime;
  end;
end;
{$ENDIF WINDOWS}

function TDirectoryEntry.GetFileType: enumFileType;
// Returns a file type depending on file extension
// This code could be replace by some kind of MIMEtype lookup code.
var
  TheFileType: enumFileType;
  LowerExt: string;
begin
  TheFileType := other;
  LowerExt := LowerCase(ExtractFileExt(string(Filename)));
  //note: includes . eg .exe!!!!
  //note2: we explicitly cast the filename to string to try and avoid memory issues. todo: check if this works
{*
  //This would be much simpler; but is only available in fpc 2.5.1+
  case string LowerExt of
    '.exe': TheFileType:=exe;
    end;
  *}
  if LowerExt = '.exe' then
  begin
    TheFileType := exe;
  end
  else if LowerExt = '.dll' then
  begin
    TheFileType := dll;
  end
  else if LowerExt = '.ocx' then
  begin
    TheFileType := ocx;
  end
  else if (LowerExt = '.jpg') or (LowerExt = '.jpeg') then
  begin
    TheFileType := jpg;
  end
  else if LowerExt = '.mp3' then
  begin
    TheFileType := mp3;
  end
  else if LowerExt = '.doc' then
  begin
    TheFileType := doc;
  end
  else if LowerExt = '.ppt' then
  begin
    TheFileType := ppt;
  end
  else if LowerExt = '.xls' then
  begin
    TheFileType := xls;
  end
  else if (LowerExt = '.htm') or (LowerExt = '.html') then
  begin
    TheFileType := html;
  end
  else if (LowerExt = '.txt') or (LowerExt = '.ini') then
  begin
    TheFileType := Text;
  end
  else
  begin
    TheFileType := other;
  end;
  //if
  //writeln('extension for ', Filename, ' is: ',TheFileType);
  Result := TheFileType;
end;

function TDirectoryEntry.StringFromFileType: String;
begin
  Result := GetEnumName(TypeInfo(enumFiletype), longint(FFileType));
end;

procedure TDirectoryEntry.ReadMD5Hash;
// Reads file and sets MD5 property variable if succesful
const
  // The md5 hash for an empty file
  MD5HashEmptyFile: TMD5Digest = (
    $d4,$1d,$8c,$d9,
    $8f,$00,$b2,$04,
    $e9,$80,$09,$98,
    $ec,$f8,$42,$7e
    );
begin
  try
    // When size = 0 and we don't have read permissions, we can still know the hash:
    if Size>0 then
      FMD5Hash := CustomMD5File(FFullPath,FFileBuffer) //MD5File(FFullPath,BufSize)
    else
      FMD5Hash := MD5HashEmptyFile;
    //use default buffer
    FMD5Valid := true;
  except
    {$IFDEF DEBUG}
    writeln(stderr, 'Debug: ', DateTimeToStr(Now),
      ': error getting MD5 hash for file: ', Filename);
    {$ENDIF}
    FMD5Valid := false;
    //If we display this hash, just check for this flag and.... don't ;)
    FMD5Hash := MD5HashEmptyFile;
    //hash of an empty string. Still not very good, but ok.
  end;
end;

{$IF (DEFINED(Windows)) OR (FPC_FULLVERSION>=20701)}
procedure TDirectoryEntry.GetExeInfo(var WindowsInfo: TFileVersionInfo);
// Gets product version, copyright etc if file is executable (based on file extension).
// Puts result into WindowsInfo object

// Returns true if it has filled data; false if not. If it returns false, contents of WindowsInfo are unaltered.
// Note: only returns info on Windows platforms; will return empty strings on others
// because fileinfo is not implemented on all platforms.
// Parts adapted from freepascal showver example program
{$IFDEF CRAZYDEBUG}
var
  i: integer;
{$ENDIF CRAZYDEBUG}
begin
  if (FSize>0) and
    ((FFileType = exe) or (FFileType = dll) or (FFileType = ocx)) then
  begin
    WindowsInfo.FileName := FFullPath;
    {$IFDEF HELLFREEZESOVER}
    writeln(stderr, 'Debug: ', DateTimeToStr(Now), ': Versioninfo started filename: ',
      WindowsInfo.FileName);
    {$ENDIF HELLFREEZESOVER}

    {$IF FPC_FULLVERSION>=20701}
    WindowsInfo.ReadFileInfo;
    {$IFDEF CRAZYDEBUG}
    for i:=0 to WindowsInfo.VersionStrings.Count-1 do
    begin
      writeln(stderr, 'exe info: '+WindowsInfo.VersionStrings[i]);
    end;
    {$ENDIF CRAZYDEBUG}
    FExeCompany:=WindowsInfo.VersionStrings.Values['CompanyName'];
    FExeDescription := WindowsInfo.VersionStrings.Values['FileDescription'];
    FExeFileVersion :=
      WindowsInfo.VersionStrings.Values['FileVersion'];
    FExeInternalName :=
      WindowsInfo.VersionStrings.Values['InternalName'];
    FExeCopyright :=
      WindowsInfo.VersionStrings.Values['LegalCopyright'];
    FExeOriginalFilename :=
      WindowsInfo.VersionStrings.Values['OriginalFilename'];
    FExeProductName :=
      WindowsInfo.VersionStrings.Values['ProductName'];
    FExeProductVersion :=
      WindowsInfo.VersionStrings.Values['ProductVersion'];
    {$ELSE}
    // Use old FPC file version code, only for Windows
    if WindowsInfo.VersionStrings.Count > 0 then
    begin
      try
        FExeCompany := WindowsInfo.getVersionSetting('CompanyName');
        FExeDescription :=
          WindowsInfo.getVersionSetting('FileDescription');
        FExeFileVersion :=
          WindowsInfo.getVersionSetting('FileVersion');
        FExeInternalName :=
          WindowsInfo.getVersionSetting('InternalName');
        FExeCopyright :=
          WindowsInfo.getVersionSetting('LegalCopyright');
        FExeOriginalFilename :=
          WindowsInfo.getVersionSetting('OriginalFilename');
        FExeProductName :=
          WindowsInfo.getVersionSetting('ProductName');
        FExeProductVersion :=
          WindowsInfo.getVersionSetting('ProductVersion');
      except
        // Something went wrong; swallow it.
        // In this way we leave existing info as is.
{$IFDEF DEBUG}
        writeln(stderr, 'Debug: ', DateTimeToStr(Now),
          ': Exception getting versioninfo for filename: ',
          WindowsInfo.FileName);
{$ENDIF DEBUG}
      end;
    end;
    {$ENDIF FPC_FULLVERSION>=20701}
    //if results present
  end
  //file type
  else
  begin
  {$IFDEF HELLFREEZESOVER}
    writeln(stderr, 'Debug: ', DateTimeToStr(Now),
      ': This is not an exe file: ',
      FFullPath);
  {$ENDIF HELLFREEZESOVER}
  end;
end;
{$ENDIF (DEFINED(Windows)) OR (FPC_FULLVERSION>=20701)}


procedure TDirectoryEntry.EmptyExifData();
//Empty out exif stuff.
//Useful if we have no jpg file or get an error reading data.
begin
  FExifAperture := '';
  FExifArtist := '';
  FExifCompressedBPP := '';
  FExifCopyright := '';
  FExifDateTime := UnknownDateTime;
  FExifDateTimeDigitized := UnknownDateTime;
  FExifDateTimeOriginal := UnknownDateTime;
  FExifExposure := '';
  FExifExposureProgram := '';
  FExifFlash := '';
  FExifFStops := '';
  FExifImageDescription := '';
  FExifISO := 0;
  FExifLightSource := '';
  FExifMake := '';
  FExifMaxAperture := '';
  FExifMeteringMethod := '';
  FExifMeteringMode := '';
  FExifModel := '';
  FExifOrientation := '';
  FExifPixelXDimension := 0;
  FExifPixelYDimension := 0;
  FExifShutterSpeed := '';
  FExifSoftware := '';
  FExifUserComments := '';
  FExifXResolution := 0;
  FExifYResolution := 0;
end;

procedure TDirectoryEntry.EmptyExeData();
// Clear out Windows exe information.
begin
  FExeCompany := '';
  FExeDescription := '';
  FExeFileVersion := '';
  FExeInternalName := '';
  FExeCopyright := '';
  FExeOriginalFilename := '';
  FExeProductName := '';
  FExeProductVersion := '';
end;



procedure TDirectoryEntry.ReadInfo;

var
  item: TSearchRec;
  {$IF (DEFINED(Windows)) OR (FPC_FULLVERSION>=20701)}
  // Executable version information
  WindowsExeInfo: TFileVersionInfo;
  {$ENDIF (DEFINED(Windows)) OR (FPC_FULLVERSION>=20701)}
  {$IFDEF UNIX}
  info : stat;
  {$ENDIF UNIX}
begin
  // Default, and for unsupported platforms:
  FTimeAccessed := UnknownDateTime;
  FTimeCreated := UnknownDateTime;

  assert(FFullPath <> '', 'TDirectory.ReadInfo: FFullPath must have a value.');
  {$IFDEF HELLFREEZESOVER}
  writeln(stderr, 'Debug: ', DateTimeToStr(Now),
    ': Starting Readinfo for: ', FFullPath);
  {$ENDIF}
  if SysUtils.FindFirst(FFullPath, faAnyFile, item) = 0 then
    //if file actually exists
  begin
    try
      FDirectory := '';
      //safe default
      try
        FDirectory := ExtractFilePath(FFullPath);
      except
        {$IFDEF DEBUG}
        writeln(stderr, 'Debug: ', DateTimeToStr(Now),
          ': Error getting path for: ', FFullPath);
        {$ENDIF}
      end;
      assert(FDirectory <> '', 'FDirectory must have a value.');

      FFileName := '';
      //safe default
      try
        FFileName := ExtractFileName(FFullPath);
      except
        {$IFDEF DEBUG}
        writeln(stderr, 'Debug: ', DateTimeToStr(Now),
          ': Error getting file name for: ', FFullPath);
        {$ENDIF}
      end;
      assert(FFileName <> '', 'FFilename must have a value.');
      //Required before exif or exe info code:
      FFileType := GetFileType;
      FTimeModified := FileDateToDateTime(Item.Time);
      FSize := Item.Size; //Needs to be done fairly early; used when reading file contents
      if FReadFileContents then
      begin
        //This will read in entire file so hopefully OS buffering lets following operations go quickly:
        ReadMD5Hash;
      end;

// Get time acccessed/created data, depending on platform:
{$IFDEF Windows}
      try
        FTimeAccessed :=
          WindowsFileTimeToUTC(Item.FindData.ftLastAccessTime);
        {$IFDEF HELLFREEZESOVER}
        writeln(stderr, 'Debug: ', DateTimeToStr(Now),
          ': Dateaccessed as float: ', floattostr(FTimeAccessed),
          '; as date: ', datetostr(FTimeAccessed));
        {$ENDIF HELLFREEZESOVER}
      except
{$IFDEF DEBUG}
        writeln(stderr, 'Debug: ', DateTimeToStr(Now),
          ': Error getting access time for file ', FFullPath);
{$ENDIF DEBUG}
      end;

      try
        FTimeCreated :=
          WindowsFileTimeToUTC(Item.FindData.ftCreationTime);
      except
{$IFDEF DEBUG}
        writeln(stderr, 'Debug: ', DateTimeToStr(Now),
          ': Error getting create time for file ', FFullPath);
{$ENDIF DEBUG}
      end;
{$ENDIF Windows}

      FUnixValidData:=false;
{$IFDEF Unix}
      // including Linux
      if fpstat(FFullPath,info)=0 then
      begin
        //FSize:=info.st_size; not needed, already done
        FTimeAccessed:=UnixToDateTime(info.st_atime);
        FTimeCreated:=UnixToDateTime(info.st_ctime);
        FUnixUID:=info.st_uid;
        FUnixGID:=info.st_gid;
        FUnixMode:=info.st_mode;
        {$IFDEF CRAZYDEBUG}
        //todo: st_mode only returns 16877 all the time!! uid/gid always 0. This works in a demo program!
        writeln(stderr, FFullPath,'st_uid:',FUnixUID);
        writeln(stderr, FFullPath,'st_gid:',FUnixGID);
        writeln(stderr, FFullPath,'st_mode:',FUnixMode);
        {$ENDIF}
        FUnixValidData:=true;
      end
      else
      begin
{$IFDEF CRAZYDEBUG}
        writeln(stderr,FFullPath+' error getting atime/ctime');
{$ENDIF CRAZYDEBUG}
      end;
{$ENDIF Unix}


      // We've reset exe data on start, so we can leave it.
      //EmptyExeData; //Just zero out the fields
      // Only fill it when reading contents and running on Windows:
      if FReadFileContents then
      begin
{$IF (DEFINED(Windows)) OR (FPC_FULLVERSION>=20701)}
        WindowsExeInfo := TFileVersionInfo.Create(nil);
        try
          try
            GetExeInfo(WindowsExeInfo);
          except
            EmptyExeData;
            //just clean up stuff if something went wrong.
          end;
        finally
          WindowsExeInfo.Free;
        end;
{$ENDIF (DEFINED(Windows)) OR (FPC_FULLVERSION>=20701)}

        try
          //todo: exif still gets reading past end of file errors sometimes.
          GetExifInfo;
        except
          on E: Exception do
          begin
{$IFDEF CRAZYDEBUG}
            writeln(stderr, 'Debug: ', DateTimeToStr(Now),
              ': Ignoring GetExifInfo errors while processing file : ',
              FFullPath, '; technical details:',
              E.ClassName, '/', E.Message
              );
{$ENDIF}
            EmptyExifData;
            //Make sure to delete nonsense on errors.
          end;
        end;
        GetMP3Info;
      end;
    finally
{$IFDEF HELLFREEZESOVER}
      writeln(stderr, 'Debug: ', DateTimeToStr(Now),
        ': Going to run FindClose for: ', FFullPath);
{$ENDIF}
      SysUtils.FindClose(item);
    end;
  end
  else
  begin
      {$IFDEF DEBUG}
    SysUtils.FindClose(item);
    writeln(stderr, 'Debug: ', DateTimeToStr(Now),
      ': Error getting file information for file ', FFullPath);
      {$ENDIF}
  end;
end;

procedure TDirectoryEntry.GetMP3Info;

var
  ID3: TID3Tag;
begin
  // Clear values to make sure
  FMP3Album := '';
  FMP3Artist := '';
  FMP3Comment := '';
  FMP3Genre := '';
  FMP3Title := '';
  FMP3Track := 0;
  FMP3Year := 0;
  if (FSize>0) and (FFileType = mp3) then
  begin
    ID3 := TID3Tag.Create(FFullPath);
    try
      if ID3.Valid then
      begin
        FMP3Album := ID3.Album;
        FMP3Artist := ID3.Artist;
        FMP3Comment := ID3.Comment;
        FMP3Genre := ID3.Genre;
        FMP3Title := ID3.Title;
        FMP3Track := ID3.Track;
        FMP3Year := ID3.Year;
      end;
    finally
      ID3.Free
    end;
  end;
end;

procedure TDirectoryEntry.GetExifInfo;

var
  exif: TExif;
begin
  if (FSize>0) and (FFileType = jpg) then
  begin
    exif := TExif.Create;
    try
      exif.ReadFromFile(FFullPath);
      if exif.Valid then
      begin
        FExifImageDescription := exif.ImageDescription;
        FExifMake := exif.Make;
        FExifModel := exif.Model;
        FExifOrientation := exif.OrientationDesc;
        FExifCopyright := exif.Copyright;
        FExifDateTime := exif.DateTime;
        FExifDateTimeOriginal := exif.DateTimeOriginal;
        FExifDateTimeDigitized := exif.DateTimeDigitized;
      end;
      //valid exif
    finally
      exif.Free //avoid memory leak.
    end;
  end;
end;

function TDirectoryEntry.ShowMD5Hash: shortstring;
  // Presents the MD5 hash of the file as a text.
  // Returns an empty text if the hash was invalid (i.e. file could not be read etc)
begin
  if FMD5Valid = true then
  begin
    Result := MD5Print(FMD5Hash);
  end
  else
  begin
    Result := '';
  end;
end;


function TDirectoryEntry.PrintPropertyNames: String;
  // Give print output of property names; useful in CSV output
begin
  Result :=
    PrintFormatFromString('File') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('Directory') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('MD5') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('CreateTime') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ModifyTime') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('AccessTime') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('Size') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('FileVersion') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ProductVersion') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('Description') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('Copyright') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ExifAperture') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ExifArtist') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ExifCompressedBPP') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ExifCopyright') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ExifDateTime') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ExifDateTimeDigitized') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ExifDateTimeOriginal') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ExifExposure') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ExifExposureProgram') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ExifFlash') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ExifFStops') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ExifImageDescription') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ExifISO') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ExifLightSource') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ExifMake') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ExifMaxAperture') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ExifMeteringMethod') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ExifMeteringMode') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ExifModel') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ExifOrientation') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ExifPixelXDimension') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ExifPixelYDimension') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ExifShutterSpeed') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ExifSoftware') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ExifUserComments') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ExifXResolution') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('ExifYResolution') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('MP3Album') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('MP3Artist') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('MP3Comment') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('MP3Genre') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('MP3Title') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('MP3Track') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('MP3Year') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('UnixUID') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('UnixGID') + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString('UnixMode');
end;

function TDirectoryEntry.PrintFormatFromDate(SourceDateTime: TDateTime): string;
  // Formats date/time for printing, e.g. CSV. Converts unknown date magic value to blank string.
  // Currently uses ISO 8601 extended format using - and :
begin
  if SourceDateTime = UnknownDateTime then
  begin
    Result := '';
    //Null date.
  end
  else
  begin
    Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', SourceDateTime);
  end;

end;

function TDirectoryEntry.PrintFormatFromString(SourceString: String): String;
  // Formats string for output as CSV.
begin
  Result := '"' + SourceString + '"';
end;

function TDirectoryEntry.Print: String;
  // Give print output of all properties
  // Note that some properties are not supported depending on the platform.
  // All properties are still included in order to standardize the output format,
  // and for future platform portability improvements.
  // Isn't there some handy shortcut for for each property give property value?? ;)
  // Yes, rttiobj and typinfo; but rttiobj is somewhere in the documentation source code
  // I don't want to deal with that right now.
begin
  Result :=
    PrintFormatFromString(FFileName) + ';' +  //Keep Jedi Code Format happy
    PrintFormatFromString(FDirectory) + ';' +  //Keep Jedi Code Format happy
    ShowMD5Hash + ';' +  //Keep Jedi Code Format happy
    PrintFormatFromdate(FTimeCreated) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromdate(FTimeModified) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromdate(FTimeAccessed) + ';' + //Keep Jedi Code Format happy
    IntToStr(FSize) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString(FExeFileVersion) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString(FExeProductVersion) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString(FExeDescription) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString(FExeCopyright) + ';' +  //Keep Jedi Code Format happy
    PrintFormatFromString(FExifAperture) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString(FExifArtist) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString(FExifCompressedBPP) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString(FExifCopyright) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromdate(FExifDateTime) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromdate(FExifDateTimeDigitized) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromdate(FExifDateTimeOriginal) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString(FExifExposure) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString(FExifExposureProgram) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString(FExifFlash) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString(FExifFStops) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString(FExifImageDescription) + ';' + //Keep Jedi Code Format happy
    IntToStr(FExifISO) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString(FExifLightSource) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString(FExifMake) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString(FExifMaxAperture) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString(FExifMeteringMethod) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString(FExifMeteringMode) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString(FExifModel) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString(FExifOrientation) + ';' + //Keep Jedi Code Format happy
    IntToStr(FExifPixelXDimension) + ';' + //Keep Jedi Code Format happy
    IntToStr(FExifPixelYDimension) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString(FExifShutterSpeed) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString(FExifSoftware) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString(FExifUserComments) + ';' + //Keep Jedi Code Format happy
    IntToStr(FExifXResolution) + ';' + //Keep Jedi Code Format happy
    IntToStr(FExifYResolution) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString(FMP3Album) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString(FMP3Artist) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString(FMP3Comment) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString(FMP3Genre) + ';' + //Keep Jedi Code Format happy
    PrintFormatFromString(FMP3Title) + ';' + //Keep Jedi Code Format happy
    IntToStr(FMP3Track) + ';' + //Keep Jedi Code Format happy
    IntToStr(FMP3Year)+ ';' + //Keep Jedi Code Format happy
    IntToStr(FUnixUID)+ ';' + //Keep Jedi Code Format happy
    IntToStr(FUnixGID)+ ';' + //Keep Jedi Code Format happy
    OctStr(FUnixMode,10) (* permissions; todo: check done properly *);
end;

constructor TDirectoryEntry.Create(FullPath: String; FileBufferPointer: PFileBuffer);
begin
  Create(FullPath, FileBufferPointer, true);
end;

constructor TDirectoryEntry.Create(FullPath: string; FileBufferPointer: PFileBuffer; ReadFileContents: boolean);
begin
  inherited Create;
  //Zero out variables just to be certain:
  Init;
  FFileBuffer:=FileBufferPointer;
  FReadFileContents:=ReadFileContents;
  FFullPath := ExpandFileName(FullPath);
  ReadInfo;
end;

destructor TDirectoryEntry.Destroy;
begin
{$IFDEF HELLFREEZESOVER}
  writeln(stderr, 'Debug: ', DateTimeToStr(Now),
    ': TDirectoryEntry.Destroy started for: ', FFullpath);
{$ENDIF}
  inherited;
end;

end.
