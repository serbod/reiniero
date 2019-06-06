{*
This source code is provided under the MIT license:
Copyright (C) 2011-2013 by Reinier Olislagers

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*}

unit iptableslogguiform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SdfData, DB, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, EditBtn, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ImportButton: TButton;
    ExportButton: TButton;
    LogFile: TFileNameEdit;
    OutputFile: TFileNameEdit;
    LogGrid: TDBGrid;
    LogLabel: TLabel;
    LogLabel1: TLabel;
    LogTableSource: TDatasource;
    SDFDataset: TSdfDataSet;
    procedure ExportButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImportButtonClick(Sender: TObject);
  private
    { private declarations }
    // Read log file and give output for csv
    function GetLogCSV(SourceFile: string): string;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses iptableslog;

{$R *.lfm}

{ TForm1 }

function TForm1.GetLogCSV(SourceFile: string): string;
var
  CSVFile: string;
  Importer: TIPTablesLog;
begin
  Result := '';
  CSVFile := SysUtils.GetTempFileName('', 'FW');
  Importer := TIPTablesLog.Create;
  try
    Importer.InputFile := SourceFile;
    Importer.OutputFile := CSVFile;
    Importer.Parse;
    Result := CSVFile;
  finally
    Importer.Free;
  end;
end;

procedure TForm1.ImportButtonClick(Sender: TObject);
begin
  if FileExistsUTF8(LogFile.Filename) = False then
  begin
    ShowMessage('No valid input log file given. Stopping.');
    exit; //procedure
  end;
  Screen.Cursor := crHourglass;
  SDFDataset.Active := False;
  try
    SDFDataset.FileName := GetLogCSV(LogFile.FileName);
  except
    on E: Exception do
    begin
      Screen.Cursor := crDefault;
      ShowMessage('Error importing firewall logs. Exception:' + E.ClassName + '/' + E.Message);
      exit;
    end;
  end;
  Screen.Cursor := crDefault;

  if SDFDataset.FileName = '' then
  begin
    ShowMessage('Error importing ' + LogFile.FileName);
  end
  else
  begin
    LogGrid.BeginUpdate;
    SDFDataset.Active := True;
    LogGrid.AutoSizeColumns; //Requires dbgrid.Options=dgAutoSizeColumn
    LogGrid.EndUpdate;
    if OutputFile.FileName = '' then
      OutputFile.Text := LogFile.Filename + '.csv';
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$IFDEF Unix}
  LogFile.FileName := '/var/log/messages';
  {$ELSE}
  LogFile.FileName := 'messages';
  {$ENDIF Unix}
end;

procedure TForm1.ExportButtonClick(Sender: TObject);
begin
  if OutputFile.FileName = '' then
  begin
    ShowMessage('Please specify an output file name before exporting.');
    exit;
  end;

  SDFDataset.SaveFileAs(OutputFile.FileName);
  ShowMessage('Export complete.');
end;

end.
