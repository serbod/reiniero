
{==============================================================================
Version 1.5: updated, ran Jedi code format
Reinier Olislagers: March 2014: some more error trapping
Reinier Olislagers: March 2013: cosmetic changes
Version 1.4: Modified from code below for FreePascal
jb: April 2010: changed to include file for debug settings.
jb: March 2010: converted to Lazarus code.
 - Date/time from string to TDateTime data type.
 - Added some debug output for troubleshooting. define DEBUG if you want to use this.
 - Fixed some data types to comply with FreePascal definitions for I/O
==============================================================================
Component simple read Exif section in Jpeg/Jfif Files.
More information about Exif at www.exif.org


Component written by SimBa aka Dimoniusis
You may use this component absolutely free.

You may talk with me via
e-mail: dimonius@mail333.com
ICQ: 11152101
Web: http://dimonius.da.ru


Changes:
Version 1.3
 - some more ifd tags implemented
 - some bugs fixes

Version 1.2 (Some code by Jim Wood,  e-mail: jwood@visithink.com)
 - some more ifd tags implemented
 - corrected work with ReadOnly files

Version 1.1 (By Ive, e-mail: ive@lilysoft.com)
 - works now with Motorola and Intel byte order tags
 - better offset calculation
 - some more ifd tags implemented
 - some format functions for rational values
 - naming convention changed a little


NOTE: far away from being complete but it seems to
      work with all example files from www.exif.org

 - Ive (c) 2003




==============================================================================}

unit exif;

{$MODE Delphi}
{$INCLUDE flocatesettings.inc}
//Project-wide options.


interface

uses
  Classes, SysUtils, DateUtils;

const
  //Some bogus date representing an unknown date/time, well before earliest file, photograph, or sound recording:
  UnknownDateTime: TDateTime = -693593;
//Note we can't do: EncodeDateTime(0,1,1,0,0,0,0);

type
  TIfdTag = packed record
    ID: word;
    //Tag number
    Typ: word;
    //Type tag
    Count: cardinal;
    //tag length
    Offset: cardinal;
    //Offset / Value
  end;

  TExif = class(TObject)
  private
    FImageDesc: string; //Picture description
    FMake: string; //Camera manufacturer
    FModel: string; //Camera model
    FOrientation: byte; //Image orientation - 1 normal
    FOrientationDesc: string; //Image orientation description
    FCopyright: string; //Copyright
    FValid: boolean; //Has valid Exif header
    FDateTime: TDateTime; //Date and Time of Change
    FDateTimeOriginal: TDateTime; //Original Date and Time
    FDateTimeDigitized: TDateTime; //Camshot Date and Time
    FUserComments: string; //User Comments

    FExposure: string; //Exposure
    FFstops: string;
    FShutterSpeed: string;
    FAperture: string;
    FMaxAperture: string;

    FExposureProgram: byte;
    FExposureProgramDesc: string;
    FPixelXDimension: cardinal;
    FPixelYDimension: cardinal;
    FXResolution: cardinal;
    FYResolution: cardinal;
    FMeteringMode: byte;
    FMeteringMethod: string;
    FLightSource: byte;
    FLightSourceDesc: string;
    FFlash: byte;
    FFlashDesc: string;
    FISO: word;
    FSoftware: string;
    FArtist: string;
    FCompressedBPP: string;

    f: file;
    ifdp: cardinal;
    FSwap: boolean;
    function ReadAsci(const Offset: int64; Count: cardinal): string;
    function ReadRatio(const Offset: int64; frac: boolean): string;
      overload;
    function ReadRatio(const Offset: int64): single;
      overload;
    procedure ReadTag(var tag: TIfdTag);
    procedure Init;
    function ReadLongIntValue(const Offset: int64): longint;
  public
    constructor Create;
    procedure ReadFromFile(const FileName: string);

    property Valid: boolean read FValid;
    property ImageDescription: string read FImageDesc;
    //Image description; ascii
    property Make: string read FMake;
    property Model: string read FModel;
    property Orientation: byte read FOrientation;
    property OrientationDesc: string read FOrientationDesc;
    property Copyright: string read FCopyright;
    property DateTime: TDateTime read FDateTime;
    property DateTimeOriginal: TDateTime read FDateTimeOriginal;
    property DateTimeDigitized: TDateTime read FDateTimeDigitized;
    property UserComments: string read FUserComments;
    // Can be unicode, ascii or jis
    property Software: string read FSoftware;
    property Artist: string read FArtist;
    property Exposure: string read FExposure;
    property ExposureProgram: byte read FExposureProgram;
    property ExposureProgramDesc: string read FExposureProgramDesc;
    property FStops: string read FFStops;
    property ShutterSpeed: string read FShutterSpeed;
    property Aperture: string read FAperture;
    property MaxAperture: string read FMaxAperture;
    property CompressedBPP: string read FCompressedBPP;
    property PixelXDimension: cardinal read FPixelXDimension;
    property PixelYDimension: cardinal read FPixelYDimension;
    property XResolution: cardinal read FXResolution;
    property YResolution: cardinal read FYResolution;
    property MeteringMode: byte read FMeteringMode;
    property MeteringMethod: string read FMeteringMethod;
    property LightSource: byte read FLightSource;
    // Value for lightsource lookup (Exif variable type: 2 byte unsigned integer)
    property LightSourceDesc: string read FLightSourceDesc;
    property Flash: byte read FFlash;
    property FlashDesc: string read FFlashDesc;
    property ISO: word read FISO;
  end;

implementation

uses
  Math;

type
  TMarker = packed record
    Marker: word; //Section marker
    Len: word; //Length Section
    Indefin: array [0..4] of char; //Indefiner - "Exif" 00, "JFIF" 00 and etc
    Pad: char; //0x00
  end;

  TIFDHeader = packed record
    pad: byte;
    //00h
    ByteOrder: word;
    //II (4D4D) or MM
    i42: word;
    //2A00 (magic number from the 'Hitchhikers Guide'
    Offset: cardinal;
    //0th offset IFD
    Count: word;
    // number of IFD entries
  end;


function ExifDateToDateTime(const ExifString: string): TDateTime;
  //<- converts a text string that we find in Exif to a date/time value.
  //Could be optimized for speed but I think this is readable.
  //Sample output for a date/time in Exif:
  //2007:12:26 02:28:35
  //yyyy:mm:dd hh:nn:ss
  //could also be 2007:12: 1  1:12: 4
  //Since: 20100319

var
  List: TStringList;
  DateList: TStringList;
  TimeList: TStringList;
begin
  List := TStringList.Create;
  DateList := TStringList.Create;
  TimeList := TStringList.Create;
  try
    //whatever happens, we have to get rid of these lists at the end.
    try
      // First split up between date and time.
      List.Delimiter := ' ';
      List.StrictDelimiter := true;
      List.DelimitedText := ExifString;
      //Now split up between :s
      DateList.Delimiter := ':';
      DateList.StrictDelimiter := true;
      DateList.DelimitedText := List[0];
      TimeList.Delimiter := ':';
      TimeList.StrictDelimiter := true;
      TimeList.DelimitedText := List[1];
      // Try and fill in some missing values if any.
      // Assume years are 2000-2099 unless explicitly specified.
      Result := EncodeDateTime(StrToInt(Rightstr('20' + Trim(DateList[0]), 4)), {*years*}
        StrToInt(Rightstr('0' + Trim(DateList[1]), 2)), {*months*}
        StrToInt(Rightstr('0' + Trim(DateList[2]), 2)), {*days*}
        StrToInt(Rightstr('0' + Trim(TimeList[0]), 2)), {*hours*}
        StrToInt(Rightstr('0' + Trim(TimeList[1]), 2)), {*minutes*}
        StrToInt(Rightstr('0' + Trim(TimeList[2]), 2)), {*seconds*}
        0 {milliseconds}
        );
      {$IFDEF DEBUG}
      //writeln(stderr, 'Debug: ', DateTimeToStr(Now), ': Date/time is:', DateTimeTostr(Result));
      {$ENDIF}
    except
      {$IFDEF DEBUG}
      writeln(stderr, 'Debug: ', DateTimeToStr(Now),
        ': Error getting exif date. Info follows:');
      writeln(stderr, 'Raw exif date/time string: *', ExifString, '* end of string');
      try
        //These variables could be empty so just try.
        writeln(stderr, 'Date item 1: *', DateList[0], '*');
        writeln(stderr, 'Date item 2: *', DateList[1], '*');
        writeln(stderr, 'Date item 3: *', DateList[2], '*');
        writeln(stderr, 'Time item 1: *', TimeList[0], '*');
        writeln(stderr, 'Time item 2: *', TimeList[1], '*');
        writeln(stderr, 'Time item 3: *', TimeList[2], '*');
      except
        writeln(stderr, 'Debug: ', DateTimeToStr(Now),

          ': error getting time/date info from exif DateList or Timelist. Probably really whacky exif string: '
          ,
          ExifString);
      end;
      //try dumping error output
      {$ENDIF}
      Result := UnknownDateTime;
    end;
    //except
  finally
    //make sure we clear memory after usage
    {$IFDEF DEBUG}

    //writeln(stderr, 'Debug: ', DateTimeToStr(Now), ': finalizing exif memory for string: ', Exifstring);
    {$ENDIF}
    List.Free;
    DateList.Free;
    TimeList.Free;
  end;
  //finally
end;

procedure TExif.ReadTag(var tag: TIfdTag);
begin
  BlockRead(f, tag, 12);
  if FSwap then
    with tag do
    begin
      // motorola or intel byte order ?
      ID := Swap(ID);
      Typ := Swap(Typ);
      Count := BEtoN(Count);
      if (Typ = 1) or (Typ = 3) then
        Offset := (Offset shr 8) and $FF
      else
        Offset := BEtoN(Offset);
    end
  else
    with tag do
    begin
      if ID <> $8827 then  //ISO Metering Mode not need conversion
        if (Typ = 1) or (Typ = 3) then
          Offset := Offset and $FF;
      // other bytes are undefined but maybe not zero
    end;
end;


function TExif.ReadAsci(const Offset: int64; Count: cardinal): string;

var
  fp: longword;
  // changed from longint so we can have file sizes of about 2 gb ;)
  i: cardinal;
  // changed i from word to cardinal to better match count.
begin
  SetLength(Result, Count);
  fp := FilePos(f);
  //Save file offset
  Seek(f, Offset);
  try
    i := 1;
    repeat
      BlockRead(f, Result[i], 1);
      Inc(i);
    until (eof(f)) or (i >= Count) or (Result[i - 1] = #0);
    if i <= Count then
      Result := Copy(Result, 1, i - 1);
  except
    Result := '';
  end;
  Result := TrimRight(Result);
  try
    Seek(f, fp);
    //Restore file offset
  except
    on E: Exception do
    begin
      raise Exception.CreateFmt('Error in seek; fp is %d, original exception: %s',[fp,E.Message]);
    end;
  end;
end;

function TExif.ReadLongIntValue(const Offset: int64): longint;

var
  fp: longint;
begin
  fp := FilePos(f);
  //Save file offset
  Seek(f, Offset);
  //typecast to conform to spec
  try
    {$NOTE Compiler hints function result warning not initialized but we're covering it with an except construct}
    BlockRead(f, Result, sizeof(Result));
    if FSwap then
      Result := BEtoN(Result);
  except
    Result := 0;
  end;
  Seek(f, fp);
  //Restore file offset
end;

function TExif.ReadRatio(const Offset: int64; frac: boolean): string;

var
  fp: longint;
  nom, denom: cardinal;
begin
  nom := 0;
  //avoid compiler warning re var initialization
  denom := 0;
  //avoid compiler warning re var initialization
  fp := FilePos(f);
  //Save file offset
  Seek(f, Offset);
  try
    BlockRead(f, nom, 4);
    BlockRead(f, denom, 4);
    if FSwap then
    begin
      // !!!
      nom := BEtoN(nom);
      denom := BEtoN(denom);
    end;
    if frac then
    begin
      str((nom / denom): 1: 2, Result);
      if (length(Result) > 0) and (Result[length(Result)] = '0') then
        Result := copy(Result, 1, length(Result) - 1);
    end
    else
    if denom <> 1000000 then
      Result := IntToStr(nom) + '/' + IntToStr(denom)
    else
      Result := '0';
  except
    Result := '';
  end;
  Seek(f, fp);
  //Restore file offset
end;


function TExif.ReadRatio(const Offset: int64): single;

var
  fp: longint;
  nom, denom: cardinal;
begin
  nom := 0;
  //avoid compiler warning re var initialization
  denom := 0;
  //avoid compiler warning re var initialization
  fp := FilePos(f);
  //Save file offset
  Seek(f, Offset);
  try
    BlockRead(f, nom, 4);
    BlockRead(f, denom, 4);
    if FSwap then
    begin
      // !!!
      nom := BEtoN(nom);
      denom := BEtoN(denom);
    end;
    Result := nom / denom;
  except
    Result := 0.0;
  end;
  Seek(f, fp);
  //Restore file offset
end;


procedure TExif.Init;
begin
  ifdp := 0;
  FImageDesc := '';
  FMake := '';
  FModel := '';
  FOrientation := 0;
  FOrientationDesc := '';
  FDateTime := UnknownDateTime;
  FCopyright := '';
  FValid := false;
  FDateTimeOriginal := UnknownDateTime;
  FDateTimeDigitized := UnknownDateTime;
  FUserComments := '';
  FExposure := '';
  FFstops := '';
  FShutterSpeed := '';
  FAperture := '';
  FExposureProgram := 0;
  FExposureProgramDesc := '';
  FPixelXDimension := 0;
  FPixelYDimension := 0;
  FMeteringMode := 0;
  FMeteringMethod := '';
  FLightSource := 0;
  FLightSourceDesc := '';
  FFlash := 0;
  FFlashDesc := '';
  FISO := 0;
  FCompressedBPP := '';
  FArtist := '';
  FSoftware := '';
  FMaxAperture := '';
  FXResolution := 0;
  FYResolution := 0;
end;


constructor TExif.Create;
begin
  Init;
end;


procedure TExif.ReadFromFile(const FileName: string);

const
  orient: array[1..9] of string =
    ('Normal', 'Mirrored', 'Rotated 180', 'Rotated 180, mirrored',
    'Rotated 90 left, mirrored', 'Rotated 90 right',
    'Rotated 90 right, mirrored',
    'Rotated 90 left', 'Unknown');
  ExplType: array[1..9] of string =
    ('Unknown', 'Manual Control', 'Normal Program',
    'Aperture Priority',
    'Shutter Priority', 'Creative Program', 'Action Program',
    'Portrait Mode',
    'Landscape Mode');
  Meter: array[0..7] of string =
    ('Unknown', 'Average', 'Center Weighted Average', 'Spot',
    'Multi Spot',
    'Pattern', 'Partial', 'Other');

var
  j: TMarker;
  ifd: TIFDHeader;
  NullExifOffset: cardinal;
  OriginalFileMode: byte;
  tag: TIfdTag;
  i: integer;
  n: single;
  SOIMarker: word;
  //2 bytes SOI marker. FF D8 (Start Of Image)
  IfdCnt: word;
  Tmp: string;

begin
  if not FileExists(FileName) then
    exit;
  Init;
  //local init:
  IfdCnt := 0;
  SOIMarker := 0;

  OriginalFileMode := System.FileMode;
  System.FileMode := (fmOpenRead or fmShareDenyWrite); //needed for reset to only read
  AssignFile(f, FileName);

  reset(f, 1); //read using recordsize 1
  //Whatever happens, we need to close the file later to avoid problems:
  try
    BlockRead(f, SOIMarker, 2);
    if SOIMarker = $D8FF then
    begin
      //Is this Jpeg
      FillByte(j, SizeOf(j), 0); //initialize var.
      BlockRead(f, j, 9);

      if j.Marker = $E0FF then
      begin
        //JFIF Marker Found
        Seek(f, 20);
        //Skip JFIF Header
        try
          BlockRead(f, j, 9);
        except
          // Probably read past end of file
          FillByte(j, SizeOf(j), 0);
        end;
      end;

      //Search Exif start marker;
      if j.Marker <> $E1FF then
      begin
        i := 0;
        repeat
          try
            BlockRead(f, SOIMarker, 2);
          except
            // Probably read past end of file
            FillByte(SOIMarker, SizeOf(SOIMarker), 0);
          end;
          Inc(i);
        until (EOF(f) or (i > 1000) or (SOIMarker = $E1FF));
        //If we find maker
        if SOIMarker = $E1FF then
        begin
          Seek(f, FilePos(f) - 2);
          //return Back on 2 bytes
          try
            //read Exif header:
            BlockRead(f, j, 9);
          except
            FillByte(j, SizeOf(j), 0);
          end;
        end;
      end;

      if j.Marker = $E1FF then
      begin
        //If we found Exif Section. j.Indefin='Exif'.
        FValid := true;
        NullExifOffset := FilePos(f) + 1;
        //0'th offset Exif header
        FillByte(ifd, SizeOf(ifd), 0); //initialize var.
        BlockRead(f, ifd, 11);
        //Read IDF Header
        FSwap := ifd.ByteOrder = $4D4D;
        // II or MM  - if MM we have to swap
        if FSwap then
        begin
          ifd.Offset := BEToN(ifd.Offset); //SwapLong(ifd.Offset);
          ifd.Count := BEToN(ifd.Count);//Swap(ifd.Count);
        end;
        if ifd.Offset <> 8 then
        begin
          Seek(f, FilePos(f) + abs(ifd.Offset) - 8);
        end;

        if (ifd.Count = 0) then
          ifd.Count := 100;

        FillByte(tag, SizeOf(tag), 0); //initialize var.
        for i := 1 to ifd.Count do
        begin
          ReadTag(tag);
          case tag.ID of
            0: break;
            // ImageDescription
            $010E: FImageDesc := ReadAsci(tag.Offset + NullExifOffset, tag.Count);
            // Make
            $010F: FMake := ReadAsci(tag.Offset + NullExifOffset, tag.Count);
            // Model
            $0110: FModel := ReadAsci(tag.Offset + NullExifOffset, tag.Count);
            // Orientation
            $0112:
            begin
              FOrientation := tag.Offset;
              if FOrientation in [1..8] then
                FOrientationDesc := orient[FOrientation]
              else
                FOrientationDesc := orient[9];
              //Unknown
            end;
            // DateTime

{*
# EXIF v2.2 tag 0x0132 DateTime
# "of image creation"
# aka. Modify Date (exiftool output)
# aka. ModifyDate (Image::ExifTool::Exif)
# aka. DateTime (Image::Info, Image::TIFF)
              *}
            $0132:
            begin
              FDateTime := ExifDateToDateTime(ReadAsci(tag.Offset + NullExifOffset, tag.Count));
            end;

            // CopyRight
            $8298: FCopyright := ReadAsci(tag.Offset + NullExifOffset, tag.Count);
            // Software
            $0131: FSoftware := ReadAsci(tag.Offset + NullExifOffset, tag.Count);
            // Artist
            $013B: FArtist := ReadAsci(tag.Offset + NullExifOffset, tag.Count);
            // Exif IFD Pointer
            $8769: ifdp := Tag.Offset;
            //Read Exif IFD offset
            //XResolution
            $011A: FXResolution := ReadLongIntValue(Tag.Offset + NullExifOffset);
            //YResolution
            $011B: FYResolution := ReadLongIntValue(Tag.Offset + NullExifOffset);
          end;
        end;

        if ifdp > 0 then
        begin
          Seek(f, int64(ifdp) + int64(NullExifOffset));
          //added typecasting to avoid buffer overflows for freepascal
          BlockRead(f, IfdCnt, 2);
          if FSwap then
            IfdCnt := swap(IfdCnt);
          for i := 1 to IfdCnt do
          begin
            ReadTag(tag);

{
          You may simple realize read this info:

          Tag |Name of Tag

          9000 ExifVersion
          0191 ComponentsConfiguration
          0392 BrightnessValue
          0492 ExposureBiasValue
          0692 SubjectDistance
          0A92 FocalLength
          9092 SubSecTime
          9192 SubSecTimeOriginal
          9292 SubSecTimeDigitized
          A000 FlashPixVersion
          A001 Colorspace
  }
            case tag.ID of
              0: break;
              // ExposureTime
              $829A: FExposure := ReadRatio(tag.Offset + NullExifOffset, false) + ' seconds';
              // Compressed Bits Per Pixel
              $9102: FCompressedBPP := ReadRatio(tag.Offset + NullExifOffset, true);
              // F-Stop
              $829D: FFStops := ReadRatio(tag.Offset + NullExifOffset, true);
              // FDateTimeOriginal
              $9003:
              begin
                FDateTimeOriginal :=
                  ExifDateToDateTime(ReadAsci(tag.OffSet + NullExifOffset, tag.Count));
                //writeln (ReadAsci(tag.OffSet+NullExifOffset,tag.Count));
              end;
              // DateTimeDigitized
              $9004:
              begin
                FDateTimeDigitized :=
                  ExifDateToDateTime(ReadAsci(tag.OffSet + NullExifOffset, tag.Count));

                //writeln ('Debug: datetimedigitized: ', ReadAsci(tag.OffSet+NullExifOffset,tag.Count));
              end;
              // ShutterSpeed
              $9201:
                try
                  n := ReadRatio(tag.Offset + NullExifOffset);
                  if n < 65535 then
                  begin
                    str(power(2, n): 1: 0, tmp);
                    FShutterSpeed := '1/' + tmp + ' seconds';
                  end
                  else
                    FShutterSpeed := '1 seconds';
                except
                  FShutterSpeed := '';
                end;
              // ISO Speed
              { http://u88.n24.queensu.ca/exiftool/forum/index.php?topic=4090.0
              In Exif v2.20 there's only one tag:
              $8827: ISO Speed Rating -16bit=>word
              In Exif v2.30, there's more
              $8827: Photographic sensitivity -16bit (write 65535 if overflow, write real value in 8832)
              $8832: Recommended Exposure Index 32 bit
              $8833: ISO Speed -32bit => apparently nothing to do with 8827?
              }
              //todo: test with sample exifs word(tag.offset) or perhaps only the low word?
              $8827: FISO := Word(Tag.Offset);
              // Aperture
              $9202: FAperture := ReadRatio(int64(tag.Offset) + NullExifOffset, true);
              // Max Aperture
              $9205: FMaxAperture := ReadRatio(int64(tag.Offset) + NullExifOffset, true);
              // UserComments
              $9286: FUserComments := ReadAsci(int64(tag.OffSet) + NullExifOffset, tag.Count);
              // Metering Mode
              $9207:
              begin
                FMeteringMode := Tag.OffSet;
                if Tag.OffSet in [0..6] then
                  FMeteringMethod := Meter[Tag.OffSet]
                else
                if Tag.OffSet = 7 then
                  FMeteringMethod := Meter[7]  //Other
                else
                  FMeteringMethod := Meter[0];
                //Unknown
              end;
              // Light Source
              $9208:
              begin
                FLightSource := Tag.OffSet;
                case Tag.OffSet of
                  0: FLightSourceDesc := 'Unknown';
                  1: FLightSourceDesc := 'Daylight';
                  2: FLightSourceDesc := 'Flourescent';
                  3: FLightSourceDesc := 'Tungsten';
                  10: FLightSourceDesc := 'Flash';
                  17: FLightSourceDesc := 'Standard Light A';
                  18: FLightSourceDesc := 'Standard Light B';
                  19: FLightSourceDesc := 'Standard Light C';
                  20: FLightSourceDesc := 'D55';
                  21: FLightSourceDesc := 'D65';
                  22: FLightSourceDesc := 'D75';
                  255: FLightSourceDesc := 'Other';
                  else
                    FLightSourceDesc := 'Unknown';
                end;
              end;
              //Flash
              $9209:
              begin
                FFlash := Tag.OffSet;
                case Tag.OffSet of
                  0: FFlashDesc := 'No Flash';
                  1: FFlashDesc := 'Flash';
                  5: FFlashDesc := 'Flash No Strobe';
                  7: FFlashDesc := 'Flash Strobe';
                  25: FFlashDesc := 'Flash (Auto)';
                  else
                    FFlashDesc := 'No Flash';
                end;
              end;
              //Exposure
              $8822:
              begin
                FExposureProgram := Tag.OffSet;
                if Tag.OffSet in [1..8] then
                  FExposureProgramDesc := ExplType[Tag.OffSet]
                else
                  FExposureProgramDesc := ExplType[9];
              end;
              //PixelXDimension
              $A002: FPixelXDimension := Tag.Offset;
              //PixelYDimension
              $A003: FPixelYDimension := Tag.Offset;
            end;
          end;
        end;
      end;
    end;
  finally
    //whatever error was generated, we need to close the file...
    CloseFile(f);
    //... and restore the original filemode (default read/write)
    System.FileMode := OriginalFileMode;
  end;

end;

end.



