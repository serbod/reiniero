unit imagecleaner;

{ Image cleaning unit; to be used to straighten up/deskew, despeckle etc images
  so OCR is more accurate.

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

{todo: add preprocess code to CleanUpImage despeckle, deskew etc? ScanTailor?
Scantailor: more for letters/documents; unpaper more for books
scantailor new version: https://sourceforge.net/projects/scantailor/files/scantailor-devel/enhanced/
unpaper input.ppm output.ppm => perhaps more formats than ppm? use eg. exactimage's econvert for format conversion}


{$i tigerserver.inc}
{$DEFINE USE_IMAGEMAGICK}
{.$DEFINE USE_EXACTIMAGE}

interface

uses
  Classes, SysUtils,
  processutils, strutils, tigerutil,
  ocr;

type

  { TImageCleaner }

  TImageCleaner = class(TObject)
  private
    FLanguage: string;
    // Tests page layout by running a scan.
    // Returns OCR recognition score (percentage: correct/total words) as well as the
    // approximate number of correctly-detected words found
    // todo: dead code but could be useful perhaps when autodetecting various languages?
    function CheckRecognition(ImageFile: string): integer;
    // Returns degrees image needs to be turned to end right-side-up
    // Currently based on Tesseract rotation detection functionality (added January 2014)
    function DetectRotation(Source: string): integer;
    // Convert image to black and white TIFF image
    function ToBlackWhiteTIFF(SourceFile,DestinationFile: string): boolean;
  public
    // Cleans up before scanning:
    // Converts image to black/white
    // Reads image, performs OCR tests on it to figure out if it needs to be rotated.
    // Rotates image if needed
    // Returns number of degrees the image has been turned,
    // e.g. 90: image rotated counterclockwise 90 degrees
    // Returns INVALIDID if function failed.
    function Clean(Source, Destination: string; AutoRotate: boolean): integer;
    // Rotates source image to destination image over specified number of degrees clockwise
    // Returns true if succesful
    function Rotate(Degrees: integer; SourceFile, DestinationFile: string): boolean;
    // Language to use for OCR, e.g. eng for English, nld for Dutch
    property Language: string read FLanguage write FLanguage;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

// Common constants etc:
{$i tigercommondefs.inc}

const
  {$IFDEF USE_EXACTIMAGE}
  ConvertCommand='econvert'; //exactimage's econvert utility
  {$ENDIF}
  {$IFDEF USE_IMAGEMAGICK}
  ConvertCommand='convert'; //Imagemagick's version
  {$ENDIF}
  NormalizeCommand='optimize2bw'; //exactimage's => black&white TIFF conversion tool

function TImageCleaner.CheckRecognition(ImageFile: string): integer;
{ Tesseract tries to output valid words in the selected language and
will often falsely detect numbers instead of gibberish when scanning rotated text.
Therefore remove all words containing only numbers before calculating statistics
}
const
  DetectLog = '/tmp/detectlog.txt';
var
  i: integer;
  TempOCR: TOCR;
  ResList: TStringList;
  ResText: string;
begin
  result:=-1; //Negative recognition rate: fail by default
  TempOCR:=TOCR.Create;
  ResList:=TStringList.Create;
  try
    result:=0;
    TigerLog.WriteLog(etDebug,'CheckRecognition: going to call ocr for file '+ImageFile);
    TempOCR.ImageFile:=ImageFile;
    TempOCR.Language:=FLanguage;
    TempOCR.RecognizeText;

    // strip out all numbers - including gibberish misdetected as numbers
    ResText:=TempOCR.Text;
    for i:=length(ResText) downto 1 do
    begin
      if char(ResText[i]) in ['0'..'9'] then
        Delete(ResText,i,1);
    end;
    ResList.Text:=ResText;
    ResText:='';

    result:=WordCount((ResList.Text),StdWordDelims);
  finally
    TempOCR.Free;
    ResList.Free;
  end;
end;

{ TImageCleaner }
function TImageCleaner.DetectRotation(Source: string): integer;
// Requires Tesseract for now
const
  //todo: add support for windows
  BogusFile = '/tmp/deleteme';
  TesseractCommand = 'tesseract';
var
  Command: string;
  CommandOutput: string;
  OutputList: TStringList;
begin
  Result:=0;
  {
  Tesseract since about 3.03 will print out orientation:
  Orientation: 0
  Orientation in degrees: 0
  Orientation confidence: 15.33
  }
  Command:=TesseractCommand+' "'+Source+'" "'+BogusFile+'" -l '+FLanguage + ' -psm 0';
  CommandOutput:='';
  if ExecuteCommand(Command,CommandOutput,false)=0 then
  begin
    OutputList:=TStringList.Create;
    try
      OutputList.Text:=CommandOutput;
      OutputList.NameValueSeparator:=':';
      if OutputList.Values['Orientation in degrees']<>'' then
      begin
        Result:=StrToIntDef(OutputList.Values['Orientation in degrees'],0);
        TigerLog.WriteLog(etDebug,'DetectRotation: found rotation '+inttostr(Result));
      end;
    finally
      OutputList.Free;
    end;
  end;
end;

constructor TImageCleaner.Create;
begin
  FLanguage:='eng'; //default to English; tesseract format
end;

destructor TImageCleaner.Destroy;
begin
  inherited Destroy;
end;

function TImageCleaner.ToBlackWhiteTIFF(SourceFile,DestinationFile: string): boolean;
var
  ErrorCode: integer;
  TempFile: string;
begin
  result:=false;
  if ExpandFileName(SourceFile)=ExpandFileName(DestinationFile) then
    TempFile:=GetTempFileName('','TIF')
  else
    TempFile:=DestinationFile;
  try
    ErrorCode:=ExecuteCommand(NormalizeCommand+
      ' --denoise --dpi 300'+
      ' --input "'+SourceFile+'" --output "tiff:'+TempFile+'" ', false);
  except
    on E: Exception do
    begin
      TigerLog.WriteLog(etWarning,
        'ToBlackWhiteTIFF: got exception '+E.Message+
        ' when calling '+NormalizeCommand+' for image '+SourceFile);
      ErrorCode:=processutils.PROC_INTERNALEXCEPTION;
    end;
  end;
  if ErrorCode=0 then
  begin
    result:=true;
  end
  else
  begin
    TigerLog.WriteLog(etWarning,
      'ToBlackWhiteTIFF: got result code '+inttostr(ErrorCode)+
      ' when calling '+NormalizeCommand+' for image '+SourceFile);
  end;
  if (result) and (ExpandFileName(SourceFile)=ExpandFileName(DestinationFile)) then
  begin
    // Copy over original file as requested
    result:=FileCopy(TempFile,DestinationFile);
  end;
end;

function TImageCleaner.Rotate(Degrees: integer; SourceFile,
  DestinationFile: string): boolean;
// Rotates uses either exactimage tools econvert or imagemagick
var
  AdditionalMessage: string;
  ErrorCode: integer;
  Overwrite: boolean;
  TempFile: string;
begin
  result:=false;
  if Degrees=0 then
    AdditionalMessage:=' - it also selects correct compression';
  TigerLog.WriteLog(etDebug,
    'TImageCleaner.Rotate: going to rotate '+SourceFile+' to '+
    DestinationFile+' over '+inttostr(Degrees)+' degrees'+AdditionalMessage);

  Overwrite:=(ExpandFileName(SourceFile)=ExpandFileName(DestinationFile));
  if Overwrite then
    TempFile:=GetTempFileName('','TIFR')
  else
    TempFile:=DestinationFile;

  // We just let the tool do the 0 degree rotations, too.
  {$IFDEF USE_EXACTIMAGE}
  //todo: this just doesn't seem to rotate. Command line appears correct though.
  // perhaps bug in exactimage 0.8.8 which I ran.

  // Rotate; indicate output should be tiff format
  // Output appears to be CCIT fax T.6, but apparently tesseract 3.02.02
  // can now read that
  try
    ErrorCode:=ExecuteCommand(ConvertCommand+
      ' --rotate "'+inttostr(Degrees)+'" '+
      ' --input "'+SourceFile+'" '+
      ' --output "tiff:'+TempFile+'" ', false);
  except
    on E: Exception do
    begin
      TigerLog.WriteLog(etWarning,
        'TImageCleaner.Rotate: got exception '+E.Message+
        ' when calling '+ConvertCommand+' for rotation '+inttostr(Degrees))
      ErrorCode:=PROC_INTERNALEXCEPTION;
    end;
  end;
  {$ENDIF}
  {$IFDEF USE_IMAGEMAGICK}
  // Rotate; indicate output should be tiff format
  // Output appears to be CCIT fax T.6, but apparently tesseract 3.02.02
  // can now read that
  try
    ErrorCode:=ExecuteCommand(ConvertCommand+
      ' "'+SourceFile+'" '+
      ' -rotate '+inttostr(Degrees)+
      ' "'+TempFile+'" ', false);
  except
    on E: Exception do
    begin
      TigerLog.WriteLog(etWarning,
        'TImageCleaner.Rotate: got exception '+E.Message+
        ' when calling '+ConvertCommand+' for rotation '+inttostr(Degrees));
      ErrorCode:=PROC_INTERNALEXCEPTION;
    end;
  end;
  {$ENDIF}

  result:=(ErrorCode=0);
  if not(result) then
    TigerLog.WriteLog(etWarning,
      'TImageCleaner.Rotate: got result code '+inttostr(ErrorCode)+
      ' when calling '+ConvertCommand+' for rotation '+inttostr(Degrees))
  else
  if Overwrite then
  begin
    if FileExists(TempFile) then
      // Copy over original file as requested
      result:=FileCopy(TempFile,DestinationFile)
    else
      TigerLog.WriteLog(etError,
        'TImageCleaner.Rotate: rotation failed; rotated temp file '+TempFile+
        ' does not exist');
  end;
end;

function TImageCleaner.Clean(Source, Destination: string; AutoRotate: boolean): integer;
var
  TempImage: string;
  Degrees:integer=0;
begin
  Result:=INVALIDID;
  TempImage:=GetTempFileName('','BW');
  // Convert to CCIT group IV fax compression
  ToBlackWhiteTIFF(Source,TempImage);
  if AutoRotate then
  begin
    Degrees:=DetectRotation(TempImage);
    if Rotate(Degrees,TempImage,Destination) then
      result:=Degrees;
  end
  else
  begin
    FileCopy(TempImage,Destination);
    result:=0;
  end;
  {$IFNDEF DEBUG}
  DeleteFile(TempImage);
  {$ENDIF}
end;

initialization
  {$IFDEF USEMAGICK}
  MagickWandGenesis;
  {$ENDIF}

finalization;
  {$IFDEF USEMAGICK}
  MagickWandTerminus;
  {$ENDIF}

end.
