unit bancomain;
{ Main unit for banco bank account info/management application

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

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, StdCtrls, EditBtn,
  bankconverter, transactioninfo, db,bufdataset,
  databaseexporter, Grids {$ifdef mswindows},shlobj{$endif};

type

  { TForm1 }

  TForm1 = class(TForm)
    ExportDirectory: TDirectoryEdit;
    ExportFormatChoice: TListBox;
    DeleteButton: TButton;
    InputFile: TFileNameEdit;
    Label1: TLabel;
    ExportButton: TButton;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    ImportButton: TButton;
    procedure DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure DeleteButtonClick(Sender: TObject);
    procedure ExportFormatChoiceSelectionChange(Sender: TObject; User: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImportButtonClick(Sender: TObject);
    procedure ExportButtonClick(Sender: TObject);
  private
    { private declarations }
    FChosenFormat: TExportFormats; //User-selected export format
    FData: TBufDataset;
    procedure SetChosenFormat(FormatText: string);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  {$ifdef mswindows}
  InitialDir: Array[0..MaxPathLen] of Char; //Allocate memory for api call
  {$else}
  InitialDir: string;
  {$endif}
begin
  ExportFormatChoice.Items.Add('ADO.Net dataset XML');
  ExportFormatChoice.Items.Add('CSV');
  ExportFormatChoice.Items.Add('DBF/DBase IV');
  ExportFormatChoice.Items.Add('DBF/DBase7');
  ExportFormatChoice.Items.Add('Firebird embedded');
  ExportFormatChoice.Items.Add('Latex');
  ExportFormatChoice.Items.Add('LibreOffice Calc');
  {$IFDEF MSWINDOWS}
  ExportFormatChoice.Items.Add('Microsoft Access');
  {$ENDIF}
  ExportFormatChoice.Items.Add('Microsoft Access XML');
  ExportFormatChoice.Items.Add('Microsoft Excel (xls)');
  ExportFormatChoice.Items.Add('Microsoft Excel (xlsx)');
  ExportFormatChoice.Items.Add('Sqlite');
  // Preselect CSV format, which should handle arbitrary length data well:
  FChosenFormat:=efXLS;
  ExportFormatChoice.ItemIndex := ExportFormatChoice.Items.IndexOf('Microsoft Excel (xls)');

  FData:=TBufDataset.Create(nil);
  // Let's forget about hash; it's not useful for end users
  //FData.FieldDefs.Add('hashid',ftString,32);
  FData.FieldDefs.Add('account',ftMemo);
  FData.FieldDefs.Add('currency',ftString,3); //ISO currency 3 character
  FData.FieldDefs.Add('amount',ftFloat); //not using currency as that inserts currency signs in csv export etc
  FData.FieldDefs.Add('bookdate',ftDateTime);
  FData.FieldDefs.Add('contraaccount',ftMemo);
  FData.FieldDefs.Add('memo',ftMemo);
  FData.CreateDataset;
  FData.Active:=true;
  DataSource1.DataSet:=FData;

  DBGrid1.DataSource:=DataSource1;

  InitialDir:='';
  {$ifdef mswindows}
  SHGetSpecialFolderPath(0,InitialDir,CSIDL_DESKTOPDIRECTORY,false);
  {$endif}
  {$ifdef darwin} //osx
  if InitialDir='' then
    InitialDir:=ExpandUNCFileNameUTF8('~/Desktop');
  {$endif}
  {$ifdef unix} //Linux, bsd
  if InitialDir='' then
    InitialDir:=ExpandUNCFileNameUTF8('~');
  {$endif}
  ExportDirectory.Directory:=InitialDir;

  Memo1.Visible:={$ifdef debug}true{$else}false{$endif};
end;

procedure TForm1.DeleteButtonClick(Sender: TObject);
begin
  if MessageDlg('Delete data','Do you want to delete all imported transactions?',mtWarning,mbOkCancel,'')<>mrOk then
    exit;

  // Naive approach:
  while not(FData.EOF) do
  begin
    FData.Delete;
  end;
end;

procedure TForm1.DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
// Draw memo text instead of (Memo), draw other text as usual.
// Requires grid DefaultDrawing to be false

{
Basically copied the existing DefaultDrawColumnCell procedure
but tested for memo first. If no memo, pass on to default procedure.
Maybe slower, more complicated, but it allows for changes in the
core Lazarus DefaultDrawColumnCell procedure.
Thanks to User137 & ludob on the Lazarus forum
}
var
  //determine if we're going to override normal Lazarus draw routines
  OverrideDraw: boolean;
  OurDisplayString: string;
  CurrentField: TField;
begin
  OverrideDraw := false;

  // Make sure selected cells are highlighted
  if (gdSelected in State) then
  begin
    (Sender as TDBGrid).Canvas.Brush.Color := clHighlight;
  end
  else
  begin
    (Sender as TDBGrid).Canvas.Brush.Color := (Sender as TDBGrid).Color;
  end;

  // Draw background in any case - thanks to ludob on the forum:
  (Sender as TDBGrid).Canvas.FillRect(Rect);

  //Foreground
  try
    CurrentField := Column.Field;
    if CurrentField.DataType = ftMemo then
    begin
      OverrideDraw := true;
    end;
  except
    on E: Exception do
    begin
      // We might have an inactive datalink or whatever,
      // in that case, pass on our problems to the LCL
      OverrideDraw := false;
    end;
  end;

  if OverrideDraw = false then
  begin
    // Call normal procedure to handle drawing for us.
    (Sender as TDBGrid).DefaultDrawColumnCell(Rect, DataCol, Column, State);
  end
  else
  begin
    // Get to work displaying our memo contents
    // Basically shamelessly ripped from
    // DefaultDrawColumnCell
    OurDisplayString := '';
    if CurrentField <> nil then
    begin
      //DO display memo ;) OurDisplayString is string to be displayed
      try
        OurDisplayString := CurrentField.AsString; //DisplayText will only show (Memo)
      except
        // Ignore errors; use empty string as specified above
      end;
    end;
    //Actual foreground drawing, taken from Grids.DrawCellText coding:
    (Sender as TDBGrid).Canvas.TextRect(Rect, Rect.Left, Rect.Top, OurDisplayString);
  end;
end;

procedure TForm1.ExportFormatChoiceSelectionChange(Sender: TObject;
  User: boolean);
begin
  SetChosenFormat(ExportFormatChoice.Items[ExportFormatChoice.ItemIndex]);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  //todo: save
  if FData.Active then
    FData.Close;
  FData.Free;
end;

procedure TForm1.ImportButtonClick(Sender: TObject);
var
  i:integer;
  Convert:TBankConverter;
begin
  if (FileExistsUTF8(InputFile.Text) = false) then
  begin
    ShowMessage('No valid input file. Aborting.');
    exit;
  end;

  Convert:=TBankConverter.Create;
  try
    Convert.InputFile:=InputFile.Text;
    Convert.ImportTransactions;
    for i:=0 to Convert.Transactions.Count-1 do
    begin
      FData.Append;
      //FData.FieldByName('hashid').AsString:=Convert.Transactions[i].HashID;
      FData.FieldByName('account').AsString:=Convert.Transactions[i].Account;
      FData.FieldByName('currency').AsString:=Convert.Transactions[i].Currency;
      FData.FieldByName('amount').AsFloat:=Convert.Transactions[i].Amount;
      FData.FieldByName('bookdate').AsDateTime:=Convert.Transactions[i].BookDate;
      FData.FieldByName('contraaccount').AsString:=Convert.Transactions[i].ContraAccount;
      FData.FieldByName('memo').AsString:=Convert.Transactions[i].Memo;
      FData.Post;
    end;
  finally
    Convert.Free;
  end;

end;

procedure TForm1.ExportButtonClick(Sender: TObject);
const
  ExportName='transactions';
var
  Exporter: TDatabaseExporter;
  {$ifdef debug}
  i: integer;
  Fields: integer;
  {$endif}
begin
  Memo1.Clear;
  {$IFDEF DEBUG}
  Memo1.Lines.Add('Dataset recordcount: '+inttostr(FData.RecordCount));
  FData.First;
  for i:=0 to FData.RecordCount-1 do
  begin
    Memo1.Lines.Add('Record no '+inttostr(i+1));
    for Fields:=0 to FData.Fields.Count-1 do
    begin
      Memo1.Lines.Add('Field '+FData.Fields[Fields].FieldName+'=*'+FData.Fields[Fields].AsString+'*');
    end;
    Memo1.Lines.Add('');
    FData.Next
  end;
  {$ENDIF}
  Exporter := TDatabaseExporter.Create;
  try
    Exporter.BaseFileName := IncludeTrailingPathDelimiter(ExportDirectory.Directory)+ExportName;
    Exporter.AddDataset(FData, ExportName);
    case FChosenFormat of
      {$IFDEF MSWINDOWS}
      efAccess:
      begin
        Exporter.ExportFormat := efAccess;
      end;
      {$ENDIF}
      efAccessXML:
      begin
        Exporter.ExportFormat := efAccessXML;
      end;
      efADONetXML:
      begin
        Exporter.ExportFormat := efADONetXML;
      end;
      efCalc:
      begin
        Exporter.ExportFormat := efCalc;
      end;
      efCSV:
      begin
        Exporter.ExportFormat := efCSV;
      end;
      efDBF4:
      begin
        Exporter.ExportFormat := efDBF4;
      end;
      efDBF7:
      begin
        Exporter.ExportFormat := efDBF7;
      end;
      efXLS:
      begin
        Exporter.ExportFormat := efXLS;
      end;
      efXLSX:
      begin
        Exporter.ExportFormat := efXLSX;
      end;
      efFirebird:
      begin
        Exporter.ExportFormat := efFirebird;
      end;
      efLatex:
      begin
        Exporter.ExportFormat := efLatex;
      end;
      efSQLite:
      begin
        Exporter.ExportFormat := efSQLite;
      end;
      else
        raise Exception.CreateFmt('Unknown export format code %d. Please fix the '
          +'program code.',
          [FChosenFormat]);
    end;

    try
      Exporter.ExportData;
      ShowMessage('Exported transactions to file starting with '+Exporter.BaseFileName);
    except
      on E: Exception do
      begin
        ShowMessage('Error exporting data. Technical details:'+E.Message);
      end;
    end;
  finally
    Exporter.Free;
  end;
end;

procedure TForm1.SetChosenFormat(FormatText: string);
begin
  case UpperCase(FormatText) of
    {$IFDEF MSWINDOWS}
    'MICROSOFT ACCESS', 'ACCESS', 'JET':
      FChosenFormat:=efAccess;
    {$ENDIF}
    'MICROSOFT ACCESS XML', 'ACCESS XML', 'ACCESSXML':
      FChosenFormat:=efAccessXML;
    'ADO.NET DATASET XML', 'ADO.NET XML', 'ADO.NET', 'ADONET':
      FChosenFormat:=efADONetXML;
    'CSV', 'DELIMITED':
      FChosenFormat:=efCSV;
    'DBF/DBASE IV', 'DB4', 'DBIV', 'DBF4', 'DBFIV', 'DBASEIV', 'DBASE4':
      FChosenFormat:=efDBF4;
    'DBF/DBASE7', 'DB7', 'DBF7', 'DBASE7':
      FChosenFormat:=efDBF7;
    'LIBREOFFICE CALC', 'CALC', 'ODS':
      FChosenFormat:=efCalc;
    'MICROSOFT EXCEL (XLS)', 'XLS':
      FChosenFormat:=efXLS;
    // Use the Microsoft Excel alias for this format due to
    // support for >32767 characters per cell
    'MICROSOFT EXCEL (XLSX)', 'MICROSOFT EXCEL', 'EXCEL', 'XLSX':
      FChosenFormat:=efXLSX;
    'FIREBIRD EMBEDDED', 'FIREBIRD':
      FChosenFormat:=efFirebird;
    'LATEX':
      FChosenFormat:=efLatex;
    'SQLITE', 'SQLITE3':
      FChosenFormat:=efSQLite;
    else
      raise Exception.CreateFmt('Unknown export format %s. Please fix the '
        +'program code.',
        [FormatText]);
  end;
end;

end.

