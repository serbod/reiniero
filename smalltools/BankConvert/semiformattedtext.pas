unit semiformattedtext;
{ Reads semi-formatted text records, such as reports written to text files.
  Assumes fixed width record/field setup, but accepts variable length headers,
  footers.
  Gives output in UTF8.

  Copyright (c) 2013 Reinier Olislagers

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
{
Reads until RecordHeaderStart is hit and then RecordHeaderEnd
Todo: process record headers
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lconvencoding, fgl, lazutf8;

type

  { TSemiFieldDef }
  // Field definitions for import
  TSemiFieldDef = class(TObject)
  private
    FFirstPosition: integer;
    FLastPosition: integer;
    FLine1: boolean; //linked to IndicatesFirstLineRecord
    FName: string;
  public
    // Character position where the field begins
    property FirstPosition: integer read FFirstPosition write FFirstPosition;
    // If yes, the presence of text in this field indicates the first line of a
    // record is read. The next lines of the records have no text in this field
    property IndicatesFirstLineRecord: boolean read FLine1 write FLine1;
    // Last character position (inclusive) of the field
    property LastPosition: integer read FLastPosition write FLastPosition;
    // Name of the field
    property Name: string read FName write FName;
  end;

  TFieldDefs = specialize TFPGObjectList<TSemiFieldDef>;

  { TSemiFmtText }
  TSemiFmtText = class(TObject)
  private
    FBody:boolean; //Flags whether our current physical line is part of record data
    FCodePage: integer;
    FCurrentLine: integer; //physical position of "cursor" in text. 0-based to match stringlist
    FFieldData: array of string; //Field data
    FFieldDefs: TFieldDefs;
    FFileName: string;
    FLines: TStringList;
    FOpen: boolean; //indicates whether file has been read
    FRecordHeaderEnd: string;
    FRecordHeaderStart: string;
    FRecordFooterStart: string;
    FRecordNumber: integer; // Starts with 1
    // Gets field data for current record
    function GetFieldData(aIndex: integer): String;
    // Gets part of field data for current physical line
    function GetFieldDataCurrentLine(aIndex: integer): String;
    function GetRawLines: TStringList;
    // Reads past header text until body found.
    // Returns false if EOF is reached first. Advances FCurrentLine
    function ReadNextBodyLine: boolean;
  protected
    // If not already open, reads file and tries to process it.
    // Resets all data structures
    procedure Open;
  public
    // File codepage (e.g. 850 for MS DOS Latin1; 1251 for Windows Cyrillic), 65001 for utf8
    property Encoding: integer read FCodePage write FCodePage;
    property FieldData[aIndex: integer]: String read GetFieldData;
    // Definition (start/stop position, name) of import fields
    property FieldDefs: TFieldDefs read FFieldDefs write FFieldDefs;
    // File that contains the data
    property FileName: string read FFileName write FFileName;
    // Reads following record; returns false if end of record
    function GetNextRecord: boolean;
    // Read access to the content of the file
    property RawLines: TStringList read GetRawLines;
    // If this line is hit, the data content of a page is finished
    // Spaces to the right of the text are ignored.
    property RecordFooterStart: string read FRecordFooterStart write FRecordFooterStart;
    // If there's a record header in a text; this line precedes it
    // Spaces to the right of the text are ignored.
    property RecordHeaderStart: string read FRecordHeaderStart write FRecordHeaderStart;
    // If this line is hit, the record header is finished
    // Spaces to the right of the text are ignored.
    property RecordHeaderEnd: string read FRecordHeaderEnd write FRecordHeaderEnd;
    constructor Create;
    constructor Create(TheFileName: string); overload;
    destructor Destroy; override;
  end;

implementation

{ TSemiFmtText }

function TSemiFmtText.GetRawLines: TStringList;
begin
  result:=nil;
  if not(FOpen) then
    Open;
  result:=FLines;
end;

function TSemiFmtText.ReadNextBodyLine: boolean;
begin
  // Read next body line; skip over headers and footers
  result:=false;
  if FCurrentLine<FLines.Count-1 then
    FCurrentLine:=FCurrentLine+1
  else
    exit; //end of file

  if FBody then
  begin
    if trimright(FLines[FCurrentLine])=FRecordFooterStart then
    begin
      // Move past footer
      FBody:=false;
      FCurrentLine:=FCurrentLine+1;
      if FCurrentLine>FLines.Count-1 then exit;
      // and continue below reading past header
    end;
  end;

  if not(FBody) then
  begin
    if FRecordHeaderStart<>'' then
    begin
      // Read past header
      while TrimRight(FLines[FCurrentLine])<>TrimRight(FRecordHeaderStart) do
      begin
        FCurrentLine:=FCurrentLine+1;
        if FCurrentLine>FLines.Count-1 then exit;
      end;
      FCurrentLine:=FCurrentLine+1; //move past header
      if FCurrentLine>FLines.Count-1 then exit;
    end;

    if FRecordHeaderEnd<>'' then
    begin
      while TrimRight(FLines[FCurrentLine])<>TrimRight(FRecordHeaderEnd) do
      begin
        FCurrentLine:=FCurrentLine+1;
        if FCurrentLine>FLines.Count-1 then exit;
      end;
      FCurrentLine:=FCurrentLine+1; //move past footer
      if FCurrentLine>FLines.Count-1 then exit;
    end;
    FBody:=true;
  end;

  result:=true;
end;

function TSemiFmtText.GetFieldData(aIndex: integer): String;
begin
  result:='';
  if not(FOpen) then
    Open;
  result:=FFieldData[aIndex];
end;

function TSemiFmtText.GetFieldDataCurrentLine(aIndex: integer): String;
begin
  result:='';
  if not(FOpen) then
    Open;
  if FBody then
    result:=UTF8Copy(FLines[FCurrentLine],
      FFieldDefs[aIndex].FirstPosition,
      1+FFieldDefs[aIndex].LastPosition-FFieldDefs[aIndex].FirstPosition);
end;

procedure TSemiFmtText.Open;
var
  i: integer;
  Sentinel:TSemiFieldDef; //If text here, indicates first line of record
begin
  FLines.LoadFromFile(FFileName);
  if FCodePage<>65001 {UTF8} then
  begin
    //todo: there probably is a simpler way of converting from multiple possibilities?
    case FCodePage of
      437: FLines.Text:=CP437ToUTF8(FLines.Text);
      850: FLines.Text:=CP850ToUTF8(FLines.Text);
      1251: FLines.Text:=CP1251ToUTF8(FLines.Text);
      else raise Exception.CreateFmt('Unsupported codepage %d',[FCodePage]);
    end;
  end;
  FOpen:=true;

  FBody:=false; //assume headers first... will be checked later.
  FCurrentLine:=0;
  FRecordNumber:=0;

  SetLength(FFieldData,FFieldDefs.Count);
  // todo: check overlapping fielddefs

  // Find out which field is the sentinel for a new line
  // assign first field if nothing found
  Sentinel:=nil;
  for i:=0 to FFieldDefs.Count-1 do
  begin
    if FFieldDefs[i].IndicatesFirstLineRecord then
      Sentinel:=FFieldDefs[i];
  end;

  if Sentinel=nil then
    if FFieldDefs.Count>=1 then
      FFieldDefs[0].IndicatesFirstLineRecord:=true //some artificial intelligence
    else
      raise Exception.Create('Field definitions are required.');

  //Go to first data line:
  ReadNextBodyLine;
end;

function TSemiFmtText.GetNextRecord: boolean;
var
  Line1, LineLast: integer;
  i,FieldNo,SentinelField:integer;
begin
  result:=false;

  if not(FOpen) then
    Open;
  // Remove any previous field data:
  for FieldNo:=0 to FFieldDefs.Count-1 do
  begin
    FFieldData[FieldNo]:='';
  end;
  if not(FBody) then exit;

  // Find out which field is the sentinel for a new line
  //todo: could be split out to class level for performance gain
  SentinelField:=-1;
  for i:=0 to FFieldDefs.Count-1 do
  begin
    if FFieldDefs[i].IndicatesFirstLineRecord then
      SentinelField:=i;
  end;
  if SentinelField=-1 then
    raise Exception.Create('At least one field definition needs to have IndicatesFirstLineRecord set.');

  // Check for sentinel field filled, then get last empty following field
  // Sentinelfield should always contain data in first record.
  //todo: check and raise exception if found, or skip to first line with non-zero field
  if GetFieldDataCurrentLine(SentinelField)<>'' then
  begin
    Line1:=FCurrentLine;
    LineLast:=FCurrentLine;
    if not(ReadNextBodyLine) then exit;
    while trim(GetFieldDataCurrentLine(SentinelField))='' do
    begin
      LineLast:=FCurrentLine;
      if ((FCurrentLine+1)<(FLines.Count-1)) and
        (trimright(FLines[FCurrentLine+1])=FRecordFooterStart) then
        break; // Next line is a footer, so no filled sentinel field needed
      if not(ReadNextBodyLine) then exit;
    end;
  end;

  //Read records from line1 to linelast, skipping over footers etc
  FCurrentLine:=Line1;
  while (FCurrentLine <= LineLast) do
  begin
    for FieldNo:=0 to FFieldDefs.Count-1 do
    begin
      FFieldData[FieldNo]:=FFieldData[FieldNo]+trim(GetFieldDataCurrentLine(FieldNo));
    end;
    if not(ReadNextBodyLine) then exit;
  end;
  //FCurrentLine is now past the record
  result:=true;
end;

constructor TSemiFmtText.Create;
begin
  inherited Create;
  FBody:=false;
  FLines:=TStringList.Create;
  FFieldDefs:=TFieldDefs.Create;
  FOpen:=false;
end;

constructor TSemiFmtText.Create(TheFileName: string);
begin
  Create;
  FFileName:=TheFilename;
end;

destructor TSemiFmtText.Destroy;
begin
  FLines.Free;
  FFieldDefs.Free;
  inherited Destroy;
end;

end.

