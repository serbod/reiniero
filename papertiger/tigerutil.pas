unit tigerutil;

{ Utility functions such as logging support.

  Copyright (c) 2012-2014 Reinier Olislagers

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}


{$i tigerserver.inc}
// Use imagemagick bindings
{$DEFINE USEMAGICK}
{$IFDEF MSWINDOWS}
{$R fclel.res} //needed for message files to get Windows to display event log contents correctly
// Not needed for *nix
{$ENDIF}

interface

uses
  Classes, SysUtils, eventlog
  {$IFDEF USEMAGICK}
  ,magick_wand, ImageMagick {for image conversion}
  {$ENDIF USEMAGICK}
  ;

type
  // Orientation of scanned image versus the "right side up".
  // E.g. TurnedAntiClock means the scanned image should be rotated 90 degrees
  // clockwise to get the correct orientation
  Orientation=(orNormal,orUpsideDown,orTurnedClock,orTurnedAntiClock,orUnknown);

type
  { TLogger }
  TLogger = class(TObject)
  private
    FLog: TEventLog; //Logging/debug output to syslog/eventlog
  public
    property EventLog: TEventLog read FLog;
    // Write to log and optionally console with seriousness etInfo
    procedure WriteLog(Message: string; ToConsole: boolean = False);
    // Write to log and optionally console with specified seriousness
    procedure WriteLog(EventType: TEventType; Message: string;
      ToConsole: boolean = False);
    constructor Create;
    destructor Destroy; override;
  end;

{todo: need translation array:
- tesseract language code (nld, eng, fra...)
- cuneiform language code (=ISO x letter code):
cuneiform -l
Cuneiform for Linux 1.1.0
...
eng ger fra rus swe spa ita ruseng ukr srp hrv pol
dan por dut cze rum hun bul slv lav lit est tur
- possibly LANG code for Linux environment for Hunspell
and lookup/translation
}
var
  TigerLog: TLogger; //Created by unit initialization so available for every referencing unit

{$IFDEF HELLFREEZESOVER}
  //todo: debug: use if needed later
// Converts bmp image in stream to tiff image in memory stream
// Memory stream must exist before calling this function
function ConvertStreamBMP_TIFF(Source: TStream; Destination: TMemoryStream): boolean;
{$ENDIF HELLFREEZESOVER}

// Converts image file to black and white CCIT Group 4 compressed image
procedure ConvertTIFFCCITT4(InputFile, OutputFile: string);

// Checks if an image is a TIFF black and white CCIT Group 4 compressed image
function IsTIFFCCITT4(InputFile: string): boolean;

// Converts image in memory to black and white CCIT Group 4 compressed image
// Calling function should clean up memory pointed to by OlImageMemoryPtr if needed
// Returns success status
//todo: don't use this  as is it currently crashes imagemagick!!!
function ConvertMemTIFFCCITTGroup4(OldImageMemoryPtr: Pointer; OldImageSize: integer;
  out NewImageMemoryPtr: Pointer; out NewImageSize: integer): boolean;

// Copy file to same or other filesystem, overwriting existing files
function FileCopy(Source, Target: string): boolean;

// Delete length characters from starting position from a stream
procedure DeleteFromStream(Stream: TStream; Start, Length: Int64);

// Searches for SearchFor in Stream starting at Start.
// Returns -1 or position in stream (0-based)
function FindInStream(Stream: TStream; Start: int64; SearchFor: string): int64;

//Shows non-debug messages on screen; also shows debug messages if DEBUG defined
procedure infoln(Message: string; Level: TEventType);



implementation
uses math;

{$IFDEF USEMAGICK}
// Imagemagick command+error handling
procedure MagickCommand(CallingFunction: string;
  wand: PMagickWand;
  const status: MagickBooleanType;
  CommandDescription: string);
var
  description: PChar;
  severity: ExceptionType;
begin
  if (status = MagickFalse) then
  begin
    description := MagickGetException(wand, @severity);
    try
      raise Exception.Create(Format
        ('%s: an error ocurred running %s. Description: %s',
        [CallingFunction, CommandDescription,description]));
    finally
      description := MagickRelinquishMemory(description);
    end;
  end;
end;
{$ENDIF USEMAGICK}

{$IFDEF HELLFREEZESOVER}
//todo: debug: use if needed later
{$IFDEF USEMAGICK}
function ConvertStreamBMP_TIFF(Source: TStream; Destination: TMemoryStream): boolean;
const
  CallF='ConvertStreamBMP_TIFF';
var
  wand: PMagickWand;
begin
  result:=false;
  if not(assigned(Destination)) then
    raise Exception.Create('ConvertStreamBMP_TIFF: Destination memorystream must be assigned before calling. Please fix the code.');
  if not(assigned(Source)) then
    raise Exception.Create('ConvertStreamBMP_TIFF: Source stream must be assigned before calling. Please fix the code.');
  wand := NewMagickWand;
  try
    MagickCommand(CallF,wand,MagickReadImageBlob(wand, OldImageMemoryPtr, OldImageSize),'MagickReadImageBlob');

    // Force TIFF format so this can also be used for converting from e.g. BMP or JPG
    MagickCommand(CallF,wand,MagickSetImageFormat(wand,'TIFF'),'GetImageFormat');

    MagickCommand(CallF,wand,MagickSetImageCompression(wand,Group4Compression),'MagickSetImageCompression');

    // Get result into new memory segment
    NewImageSize:=0;
    NewImageMemoryPtr:=MagickGetImageBlob(wand,Pointer(NewImageSize));
    if NewImageMemoryPtr<>nil then
      result:=true;
    //Calling function should clean up original memory
  finally
    wand := DestroyMagickWand(wand);
  end;

  result:=true;
end;
{$ENDIF USEMAGICK}
{$ENDIF HELLFREEZESOVER}

{$IFDEF USEMAGICK}
procedure ConvertTIFFCCITT4(InputFile, OutputFile: string);
// Let imagemagick convert an image file to TIFF Fax compressed B/W
var
  status: MagickBooleanType;
  wand: PMagickWand;
  description: PChar;
  severity: ExceptionType;
  procedure HandleError;
  begin
    description := MagickGetException(wand, @severity);
    try
      raise Exception.Create(Format('ConvertTIFFCCITT4: an error ocurred. Description: %s', [description]));
    finally
      description := MagickRelinquishMemory(description);
    end;
  end;
begin
  wand := NewMagickWand;
  try
    status := MagickReadImage(wand,PChar(InputFile));
    if (status = MagickFalse) then HandleError;

    status := MagickSetImageFormat(wand,'TIFF');
    if (status = MagickFalse) then HandleError;

    // Perhaps this helps?
    //todo: not supported in pascalmagick?
    {
    status := MagickSetOption(wand,'tiff:rows-per-strip','1');
    if (status = MagickFalse) then HandleError;
    }

    { perhaps needed for some images: remove the alpha channel:
    MagickSetImageMatte(magick_wand,MagickFalse);
    MagickQuantizeImage(magick_wand,2,GRAYColorspace,0,MagickFalse,MagickFalse);
    }
    // convert to black & white/lineart
    status := MagickSetImageType(wand,BilevelType);
    if (status = MagickFalse) then HandleError;

    // Compress with CCIT group 4 compression (fax compression); best for B&W
    {$IF FPC_FULLVERSION<20701}
    //Group4Compression seems defined as 4 which apparently doesn't match imagemagick source
    //http://mantis.freepascal.org/view.php?id=26723
    status := MagickSetImageCompression(wand,CompressionType(7));
    {$ELSE}
    status := MagickSetImageCompression(wand,Group4Compression);
    {$ENDIF}
    if (status = MagickFalse) then HandleError;

    // Apparently set(image)compresionquality and
    // stripimage are necessary to actually compress
    status := MagickSetImageCompressionQuality(wand,0);
    if (status = MagickFalse) then HandleError;
    status := MagickStripImage(wand);
    if (status = MagickFalse) then HandleError;

    status := MagickWriteImage(wand,PChar(OutputFile));
    if (status = MagickFalse) then HandleError;

  finally
    wand := DestroyMagickWand(wand);
  end;
end;
{$ENDIF USEMAGICK}

{$IFDEF USEMAGICK}
function IsTIFFCCITT4(InputFile: string): boolean;
// Check if an image file to TIFF Fax compressed B/W
var
  ResultPChar: PChar;
  Compression: CompressionType;
  status: MagickBooleanType;
  wand: PMagickWand;
  description: PChar;
  severity: ExceptionType;
  procedure HandleError;
  begin
    description := MagickGetException(wand, @severity);
    try
      raise Exception.Create(Format('ConvertTIFFCCITT4: an error ocurred. Description: %s', [description]));
    finally
      description := MagickRelinquishMemory(description);
    end;
  end;

begin
  wand := NewMagickWand;
  try
    status := MagickReadImage(wand,PChar(InputFile));
    if (status = MagickFalse) then HandleError;

    ResultPchar := MagickGetImageFormat(wand);

    Compression := UndefinedCompression;
    Compression := MagickGetImageCompression(wand);
    {$IF FPC_FULLVERSION<20701}
    //Group4Compression enum has the wrong number
    //http://mantis.freepascal.org/view.php?id=26723
    result := (Compression=CompressionType(7));
    {$ELSE}
    result := (Compression=CompressionType(Group4Compression));
    {$ENDIF}
  finally
    wand := DestroyMagickWand(wand);
  end;
end;
{$ENDIF}

{$IFDEF USEMAGICK}
function ConvertMemTIFFCCITTGroup4(OldImageMemoryPtr: Pointer; OldImageSize: integer;
  out NewImageMemoryPtr: Pointer; out NewImageSize: integer): boolean;
// Let imagemagick convert a TIFF image to CCIT Group 4
const
  CallF='ConvertMemTIFFCCITGroup4';
var
  wand: PMagickWand;
  PNewImageSize: PPtrUint; //points to NewImageSize
begin
  result:=false;
  wand := NewMagickWand;
  try
    //todo: debug: remove logging
    MagickCommand(CallF,wand,MagickReadImageBlob(wand, OldImageMemoryPtr, OldImageSize),'MagickReadImageBlob');
    TigerLog.WriteLog('1');

    // Force TIFF format so this can also be used for converting from e.g. BMP or JPG
    MagickCommand(CallF,wand,MagickSetImageFormat(wand,'TIFF'),'GetImageFormat');
    TigerLog.WriteLog('2');
    {$IF FPC_FULLVERSION<20701}
    //Group4Compression enum has the wrong number
    //http://mantis.freepascal.org/view.php?id=26723
    MagickCommand(CallF,wand,MagickSetImageCompression(wand,CompressionType(7)),'MagickSetImageCompression');
    {$ELSE}
    MagickCommand(CallF,wand,MagickSetImageCompression(wand,Group4Compression),'MagickSetImageCompression');
    {$ENDIF}
    TigerLog.WriteLog('3');

    // Get result into new memory segment
    NewImageSize:=0;
    PNewImageSize:=@NewImageSize;
    // or rather use GetImageFromMagickWand?!?! see code in client
    NewImageMemoryPtr:=MagickGetImageBlob(wand,PNewImageSize);
    TigerLog.WriteLog('4'); //Up to now it works
    if NewImageMemoryPtr<>nil then
    begin
      //this doesn't anymore
      result:=true;
      TigerLog.WriteLog('5');
    end;
    //Calling function should clean up original memory
  finally
    wand := DestroyMagickWand(wand); //this does get executed
    TigerLog.WriteLog('6');
  end;
end;
{$ENDIF USEMAGICK}

function FileCopy(Source, Target: string): boolean;
// Copies source to target; overwrites target.
// Caches entire file content in memory.
// Returns true if succeeded; false if failed
var
  MemBuffer: TMemoryStream;
begin
  result:=false;
  if not(FileExists(Source)) then
  begin
    TigerLog.WriteLog(etDebug,'FileCopy: source file '+Source+' does not exist. Arborting');
    exit;
  end;

  MemBuffer:=TMemoryStream.Create;
  try
    try
      MemBuffer.LoadFromFile(Source);
      MemBuffer.Position:=0;
      MemBuffer.SaveToFile(Target);
      result:=true;
    except
      on E: Exception do begin
        TigerLog.WriteLog(etDebug,'FileCopy: error '+E.Message);
        result:=false; //swallow exception; convert to error code
      end;
    end;
  finally
    MemBuffer.Free;
  end;
end;

procedure DeleteFromStream(Stream: TStream; Start, Length: Int64);
// Source:
// http://stackoverflow.com/questions/9598032/is-it-possible-to-delete-bytes-from-the-beginning-of-a-file
var
  Buffer: Pointer;
  BufferSize: Integer;
  BytesToRead: Int64;
  BytesRemaining: Int64;
  SourcePos, DestPos: Int64;
begin
  SourcePos := Start+Length;
  DestPos := Start;
  BytesRemaining := Stream.Size-SourcePos;
  BufferSize := Min(BytesRemaining, 1024*1024*16);//no bigger than 16MB
  GetMem(Buffer, BufferSize);
  try
    while BytesRemaining>0 do begin
      BytesToRead := Min(BufferSize, BytesRemaining);
      Stream.Position := SourcePos;
      Stream.ReadBuffer(Buffer^, BytesToRead);
      Stream.Position := DestPos;
      Stream.WriteBuffer(Buffer^, BytesToRead);
      inc(SourcePos, BytesToRead);
      inc(DestPos, BytesToRead);
      dec(BytesRemaining, BytesToRead);
    end;
    Stream.Size := DestPos;
  finally
    FreeMem(Buffer);
  end;
end;

function FindInStream(Stream: TStream; Start: int64; SearchFor: string): int64;
// Adapted from
// http://wiki.lazarus.freepascal.org/Rosetta_Stone#Finding_all_occurrences_of_some_bytes_in_a_file
var
  a: array of byte;
  BlockArray: array of byte; //Gets a block of bytes from the stream
  BlockSize:integer = 1024*1024;
  ReadSize:integer;
  fPos:Int64;
  FifoBuff:array of byte; //Window into blockarray, used to match SearchFor
  FifoStart,FifoEnd,SearchLen,lpbyte:integer;

  function CheckPos: int64;
  var
    l,p:integer;
  begin
    result:=-1;
    p := FifoStart;
    for l := 0 to pred(SearchLen) do
    begin
      if a[l] <> FifoBuff[p] then exit; //match broken off
      //p := (p+1) mod SearchLen,   the if seems quicker
      inc(p);
      if p >= SearchLen then p := 0;
    end;
    result:=(fpos-SearchLen);
  end;

begin
  SetLength(a,length(SearchFor));
  Move(Searchfor[1], a[0], Length(Searchfor)); //todo check if this shouldn't be a^

  setlength(BlockArray,BlockSize);
  Stream.Position:=Start;
  ReadSize := Stream.Read(BlockArray[0],Length(BlockArray));
  SearchLen := length(a);
  if SearchLen > length(BlockArray) then
    raise Exception.CreateFmt('FindInStream: search term %s larger than blocksize',[SearchFor]);
  if ReadSize < SearchLen then exit; //can't be in there so quit

  setlength(FifoBuff,SearchLen);
  move(BlockArray[0],FifoBuff[0],SearchLen);
  fPos:=0;
  FifoStart:=0;
  FifoEnd:=SearchLen-1;
  result:=CheckPos;
  if result>-1 then
    exit; //found it
  while ReadSize > 0 do
  begin
    for lpByte := 0 to pred(ReadSize) do
    begin
      inc(FifoStart); if FifoStart>=SearchLen then FifoStart := 0;
      inc(FifoEnd); if FifoEnd>=SearchLen then FifoEnd := 0;
      FifoBuff[FifoEnd] := BlockArray[lpByte];
      inc(fPos);
      result:=CheckPos;
      if result>-1 then
        exit; //found it
    end;
    ReadSize := Stream.Read(BlockArray[0],Length(BlockArray));
  end;
end;

procedure infoln(Message: string; Level: TEventType);
var
  Seriousness: string;
begin
  case Level of
    etCustom: Seriousness := 'Custom:';
    etDebug: Seriousness := 'Debug:';
    etInfo: Seriousness := 'Info:';
    etWarning: Seriousness := 'WARNING:';
    etError: Seriousness := 'ERROR:';
    else
      Seriousness := 'UNKNOWN CATEGORY!!:'
  end;
  if (Level <> etDebug) then
  begin
    if AnsiPos(LineEnding, Message) > 0 then
      writeln(''); //Write an empty line before multiline messagse
    writeln(Seriousness + ' ' + Message); //we misuse this for info output
    sleep(200); //hopefully allow output to be written without interfering with other output
  end
  else
  begin
      {$IFDEF DEBUG}
      {DEBUG conditional symbol is defined using e.g.
      Project Options/Other/Custom Options using -dDEBUG}
    if AnsiPos(LineEnding, Message) > 0 then
      writeln(''); //Write an empty line before multiline messagse
    writeln(Seriousness + ' ' + Message); //we misuse this for info output
    sleep(200); //hopefully allow output to be written without interfering with other output
      {$ENDIF DEBUG}
  end;
end;

{ TLogger }

procedure TLogger.WriteLog(Message: string; ToConsole: boolean = False);
begin
  FLog.Log(etInfo, Message);
  if ToConsole then
    infoln(Message, etinfo);
end;

procedure TLogger.WriteLog(EventType: TEventType; Message: string;
  ToConsole: boolean = False);
begin
  // Only log debug level if compiled as a debug build in order to cut down on logging
  {$IFDEF DEBUG}
  if 1 = 1 then
  {$ELSE}
    if EventType <> etDebug then
  {$ENDIF}
    begin
      FLog.Log(EventType, Message);
      if ToConsole then
        infoln(Message, etinfo);
    end;
  {$IFDEF DEBUG}
  // By setting active to false, we try to force a log write. Next log attempt will set active to true again
  FLog.Active := False;
  {$ENDIF}
end;

constructor TLogger.Create;
begin
  FLog := TEventLog.Create(nil);
  FLog.LogType := ltSystem; //eventlog/syslog, not log to file
  FLog.RegisterMessageFile('');
  //specify Windows should use the binary to look up formatting strings
  FLog.RaiseExceptionOnError := False; //Don't throw exceptions on log errors.
  FLog.Active := True;
end;

destructor TLogger.Destroy;
begin
  FLog.Active := False; //save WriteLog text
  FLog.Free;
  inherited Destroy;
end;

initialization
  begin
    TigerLog := TLogger.Create;
    {$IFDEF USEMAGICK}
    MagickWandGenesis;
    {$ENDIF}
  end;

finalization
  begin
    {$IFDEF USEMAGICK}
    MagickWandTerminus;
    {$ENDIF}
    TigerLog.Free;
  end;
end.
