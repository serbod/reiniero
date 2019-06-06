unit pdf;

{ PDF generation functionality

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

interface

uses
  Classes, SysUtils, tigerutil;

type

  { TPDF }

  TPDF = class(TObject)
  private
    FHOCRFile: string;
    FImageFile: string;
    FImageResolution: integer;
    FPDFFile: string;

  public
    // File with hOCR: position and text in image
    property HOCRFile: string write FHOCRFile;
    // Input image
    property ImageFile: string write FImageFile;
    // Output PDF file
    property PDFFile: string read FPDFFile write FPDFFile;
    // Manual override/specification of image resolution. Enter 0 for no override.
    // Used for passing to hocr2pdf
    property ImageResolution: integer write FImageResolution;
    // Takes hOCR file, image and creates a PDF from that. Returns success or failure
    function CreatePDF: boolean;
    constructor Create;
    destructor Destroy; override;
  end;

// Concatenates all pdf files in PDF list into OutputPDF
// If only one pdf, just copies to OutputPDF
// Overwrites OutputPDF if present
// Returns success or failure
function ConcatenatePDF(PDFList: TStrings; var OutputPDF: string): boolean;


implementation

uses processutils;

function ConcatenatePDF(PDFList: TStrings;
  var OutputPDF: string): boolean;
const
  Command = 'pdftk';
var
  ErrorCode: integer;
  i: integer;
  SourcePDFs: string;
begin
  result := false;
  SourcePDFs := '';

  if PDFList.Count=0 then
  begin
    TigerLog.WriteLog(etWarning,'ConcatenatePDF: no input pdfs specified.');
    exit(false);
  end;

  if OutputPDF='' then
  begin
    TigerLog.WriteLog(etError,'ConcatenatePDF: OutputPDF may not be empty.');
    exit(false);
  end;

  if FileExists(OutputPDF) then
  begin
    //todo: debug
    TigerLog.WriteLog(etDebug,'ConcatenatePDF: OutputPDF file '+OutputPDF+' already exists. Going to overwrite.');
    if not(DeleteFile(OutputPDF)) then
    begin
      TigerLog.WriteLog(etError,'ConcatenatePDF: OutputPDF: error trying to delete existing file '+OutputPDF);
      exit;
    end;
  end;

  if PDFList.Count=1 then
  begin
    // Simply copy PDF instead of concatenating
    exit(FileCopy(PDFList[0],OutputPDF));
  end;

  for i:= 0 to PDFList.Count - 1 do
  begin
    if i=0 then
      SourcePDFs := '"' + PDFList[i] + '"'
    else
      SourcePDFs := SourcePDFs + ' "' + PDFList[i] + '"';
  end;


  try
    ErrorCode:=ExecuteCommand(Command + ' ' +
      SourcePDFs + ' ' +
      ' cat output "'+OutputPDF+'" ', false);
  except
    on E: Exception do
    begin
      TigerLog.WriteLog(etWarning,
        'ConcatenatePDF: got exception '+E.Message+
        ' when calling '+Command+' for PDFs '+SourcePDFs+' to target PDF '+OutputPDF);
      ErrorCode:=processutils.PROC_INTERNALEXCEPTION;
    end;
  end;
  if ErrorCode=0 then
  begin
    result:=true;
    TigerLog.WriteLog(etDebug,
      'ConcatenatePDF: success '+
      ' calling '+Command+' for PDFs '+SourcePDFs+' to target PDF '+OutputPDF);
  end
  else
  begin
    TigerLog.WriteLog(etWarning,
      'ConcatenatePDF: got result code '+inttostr(ErrorCode)+
      ' when calling '+Command+' for PDFs '+SourcePDFs+' to target PDF '+OutputPDF);
  end;
end;


{ TPDF }

function TPDF.CreatePDF: boolean;
  // Create searchable PDF using exactimage (using -s for aligning text better)
const
  Command = 'hocrwrap.sh';
var
  Options: string;
  ResolutionOption: string;
begin
  Result := false;
  if FPDFFile = '' then
    FPDFFile := ChangeFileExt(FImageFile, '.pdf');

  // hocrwrap.sh expects hocr file as 1st parameter
  // Specifying --sloppy-text or not doesn't really seem to help; we get a lot of extraneous characters
  if FImageResolution > 0 then
    ResolutionOption := ' --resolution ' + IntToStr(FImageResolution);
  Options := ' "' + FHOCRFile + '" -i "' + FImageFile + '" -o "' + FPDFFile + '"' + ResolutionOption + ' --sloppy-text';
  TigerLog.WriteLog(etDebug, 'CreatePDF: PDF generation: running ' + Command + Options, true);
  try
    if ExecuteCommand(Command + Options, false) = 0 then
    begin
      TigerLog.WriteLog(etDebug, 'CreatePDF: PDF succeeded.', true);
      Result := true;
    end
    else
    begin
      TigerLog.WriteLog(etError, 'CreatePDF: Error running command.');
    end;
  except
    on E: Exception do
    begin
      TigerLog.WriteLog(etError,
        'CreatePDF: got exception '+E.Message+
        ' when calling '+Command + Options);
      Result:=false;
    end;
  end;
  //todo: deal with temp files somewhere. complicated because some are needed by other processes. Best to add them to an overarchiing object with list?
end;

constructor TPDF.Create;
begin
  inherited Create;
  FImageFile := '';
  FHOCRFile := '';
  FPDFFile := '';
end;

destructor TPDF.Destroy;
begin
  inherited Destroy;
end;

end.
