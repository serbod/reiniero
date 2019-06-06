program bankconvert;
{

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
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, transactioninfo, bankconverter,
  importfile,importfileabnamro, importfilerabo,importfilesberbank,importfileorov,
  exportfile, exportscreen,
  {other project files:}
  csvdocument,
  fpspreadsheet, bankutils, databaseexporter, dataset2sql,
  semiformattedtext;

type

  { TBankConvert }
  TBankConvert = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TBankConvert }

procedure TBankConvert.DoRun;
var
  Convert: TBankConverter;
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  Convert:=TBankConverter.Create;
  try
    case paramcount of
      0:
      begin
        writeln('Please specify input file (optionally output file).');
        writehelp;
        halt(2);
      end;
      1:
      begin
        Convert.InputFile:=paramstr(1);
        Convert.OutputFile:='';
        Convert.OutputFormat:=bdtScreen;
      end
      else
      begin
        Convert.InputFile:=paramstr(1);
        Convert.OutputFile:=paramstr(2);
      end;
    end;
    Convert.Convert;
  finally
    Convert.Free;
  end;

  // stop program loop
  Terminate;
end;

constructor TBankConvert.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TBankConvert.Destroy;
begin
  inherited Destroy;
end;

procedure TBankConvert.WriteHelp;
begin
  writeln('Usage: ',ExeName,' -h');
  writeln('');
  writeln('Please specify input file.');
  writeln('Specifying an output file is optional; defaults to screen output.');
end;

var
  Application: TBankConvert;
begin
  Application:=TBankConvert.Create(nil);
  Application.Title:='BankConvert';
  Application.Run;
  Application.Free;
end.

