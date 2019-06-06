unit importfile;
{ Copyright (c) 2012 Reinier Olislagers

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
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, transactioninfo;

type

  { TImportFile }

  TImportFile = class(TObject)
  protected
    FFileName: string;
    FFileStream: TFileStream;
    FValidated: boolean;
    // Convenience function that can be used by descendents
    function ReadALine(var TheLine: string): boolean;
  public
    property FileName: string write FFileName;
    // Read the file and see if it matches this format.
    function ValidateFile: boolean; virtual; abstract;
    // Open the file if validated and import.
    // Call FGetTransactonProc for each transaction so calling code can process it.
    procedure Import(Transactions: TTransactionList); virtual; abstract;
    constructor Create;
    constructor Create(ImportFile: string); virtual;
    destructor Destroy; override;
  end;

const
  OldDate=0; //somewhere around 1600? 1800?

// Count the number of occurrences of LookFor in the LookIn string
function CountPos(const LookFor, LookIn: string): integer;


// Tries to convert 8 character date. Accepts yyyyMMdd date strings, optionally padded with spaces
function YMDToDateDef(DateValue: string; DefaultDate: TDateTime=OldDate): TDateTime;

implementation

function YMDToDateDef(DateValue: string; DefaultDate: TDateTime=OldDate): TDateTime;
var
  Stripped: string;
begin
  result:=DefaultDate;
  Stripped:=Trim(DateValue);
  if length(Stripped)=length('yyyyMMdd') then
  begin
    Result:=EncodeDate(
      strtointdef(copy(Stripped,1,4),0),
      strtointdef(copy(Stripped,5,2),0),
      strtointdef(copy(Stripped,7,2),0));
  end;
end;


function CountPos(const LookFor, LookIn: string): Integer;
begin
  if (Length(LookFor) = 0) or (Length(LookIn) = 0) or (Pos(LookFor, LookIn) = 0) then
    Result := 0
  else
    Result := (Length(LookIn) - Length(StringReplace(LookIn, LookFor, '',[rfReplaceAll]))) div Length(LookFor);
end;

{ TImportFile }


function TImportFile.ReadALine(var TheLine: string): boolean;
// Assumes ANSI encoded files; stops on CRLF or LF
var
  ReadChar: Char;
begin
  TheLine:='';
  result:=false;
  if not(assigned(FFileStream)) then
    FFileStream:= TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
  if FFileStream.Size>0 then
  begin
    ReadChar:=' ';
    while FFileStream.Read(ReadChar,1)>0 do
    begin
      result:=true; // at least an (empty) line
      if ReadChar=#10 then
        break
      else
        TheLine:=TheLine+ReadChar;
    end;
    // Strip out CR in CRLF pair
    if (Length(TheLine)>0) and (TheLine[Length(TheLine)]=#13) then
      TheLine:=copy(TheLine,1,length(TheLine)-1);
  end;
end;

constructor TImportFile.Create;
begin
  FValidated:=false;
end;

constructor TImportFile.Create(ImportFile: string);
begin
  Create;
  FFileName:=ImportFile;
end;

destructor TImportFile.Destroy;
begin
  if assigned(FFileStream) then
    FFileStream.Free;
  inherited Destroy;
end;

end.

