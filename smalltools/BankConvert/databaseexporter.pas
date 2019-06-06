unit DatabaseExporter;
{ Non-GUI database export functionality for 1..n recordsets

  This source code is provided under the MIT license:
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
  Classes, SysUtils, fileutil, fpstdexports, {fpdataexporter,}
  fpspreadsheet, fpsallformats {needed to export to all possible XLS formats},
  fpcsvexport, fpDBExport {for csv quotestring support},
  fpdbfexport, fptexexport, fpXMLXSDExport,
  {$IFDEF MSWINDOWS}
  // Only Windows supports writing to Access databases
  //todo: investigate using ODBC+mdbtools ODBC driver
  CreateAccessDb,
  {$ENDIF}
  DB, sqldb, odbcconn,
  IBConnection,
  sqlite3conn, dataset2sql, dateutils;

type
  TExportFormats = (efAccess, efFirebird, efSQLite,
    efDBF4, efDBF7,
    efAccessXML, efADONetXML, efCalc, efCSV,
    efXLS, efXLSX, efLatex);

  { TDatabaseExporter }

  TDatabaseExporter = class(TObject)
  private
    FBaseFileName: string;
    FDatasets: TFPList;
    FExportFormat: TExportFormats;
    FExportNames: TStringList; //export names for FDatasets
    procedure SetBaseFileName(AValue: string);
  public
    property ExportFormat: TExportFormats read FExportFormat write FExportFormat;
    // Filename for export without extension
    // (any extensions given will be replaced by the relevant output format extension)
    property BaseFileName: string read FBaseFileName write SetBaseFileName;
    // Indicate this dataset should be exported
    procedure AddDataset(ADataset: TDataset; ExportName: string);
    function ExportData: boolean;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TDatabaseExporter }

procedure TDatabaseExporter.SetBaseFileName(AValue: string);
begin
  if FBaseFileName = AValue then
    Exit;
  FBaseFileName := ChangeFileExt(ExpandFileName(AValue), '');
end;


procedure TDatabaseExporter.AddDataset(ADataset: TDataset; ExportName: string);
begin
  FDatasets.Add(ADataset);
  if ExportName='' then
    FExportNames.Add(ADataset.Name)
  else
    FExportNames.Add(ExportName);
end;

function TDatabaseExporter.ExportData: boolean;
const
  XLS_OUTPUT_FORMAT = sfExcel8; //Excel 97, 2000, XP and 2003;
var
  i: integer;
  AccessConn: TODBCConnection;
  DataCorruption: boolean;
  DbTran: TSQLTransaction;
  OutputFile: string;
  Conversion: TTargetDB;
  CSVExport: TCSVExporter;
  CSVSettings: TCSVFormatSettings;
  CurrentPos: array of TBookMark;
  DBFExport: TFPDBFExport;
  DBFSettings: TDBFExportFormatSettings;
  ExcelWorkBook: TsWorkBook;
  ExcelWorkSheet: TsWorkSheet;
  FBConn: TIBConnection;
  FieldValue: string;
  LatexExport: TTeXExporter;
  SQLiteConn: TSQLite3Connection;
  TheColumn: integer;
  TheRow: integer;
  XMLExport: TXMLXSDExporter;
  XMLSettings: TXMLXSDFormatSettings;
begin
  Result := false;

  // Reserve memory for the bookmarks:
  SetLength(CurrentPos,FDatasets.Count);

  // Move to beginning of datasets
  for i := 0 to FDataSets.Count - 1 do
  begin
    TDataset(FDataSets[i]).Active:=true;
    CurrentPos[i]:=TDataset(FDataSets[i]).GetBookMark;
    TDataset(FDataSets[i]).First;
  end;

  case FExportFormat of
    {$IFDEF MSWINDOWS}
    efAccess: // Access mdb
    begin
      OutputFile := FBaseFileName + '.mdb';
      if not (FileExistsUTF8(OutputFile)) then
      begin
        if not (CreateAccessDatabase(OutputFile)) then
          raise Exception.CreateFmt('Could not create database %s.', [OutputFile]);
      end;

      AccessConn := TODBCConnection.Create(nil);
      DbTran := TSQLTransaction.Create(nil);
      // Override regular ODBC driver so Access-specific SQL is used:
      Conversion := TTargetDB.Create(AccessConn, 'ODBC_MSACCESS');
      try
        AccessConn.Transaction := DbTran;
        AccessConn.Params.Add('DBQ=' + OutputFile);
        // New MS Access driver
        try
          AccessConn.Driver := 'Microsoft Access Driver (*.mdb, *.accdb)';
          AccessConn.Open;
        except
          //swallow problems; try next driver
        end;

        if not (AccessConn.Connected) then
        begin
          try
            AccessConn.Driver := 'Microsoft Access Driver (*.mdb)';
            AccessConn.Open;
          except
            //swallow
          end;
        end;

        if not (AccessConn.Connected) then
          raise Exception.Create(
            'Could not connect to Access database with either .mdb or .accdb./.mdb ODBC driver. Please make sure the Microsoft Access Database Engine 2010 Redistributable package is installed.');
        // Create table without overwriting existing tables,
        // and insert data
        for i := 0 to FDatasets.Count - 1 do
        begin
          Conversion.SourceDataSet:=TDataSet(FDataSets[i]);
          Conversion.DestinationTable:=FExportNames[i];
          Conversion.CloneDataset;
        end;
        AccessConn.Connected := false;
      finally
        AccessConn.Free;
        DBTran.Free;
        Conversion.Free;
      end;
    end;
    {$ENDIF}
    efAccessXML: //Access XML
    begin
      for i := 0 to FDatasets.Count - 1 do
      begin
        XMLExport := TXMLXSDExporter.Create(nil);
        XMLSettings := TXMLXSDFormatSettings.Create(true);
        try
          XMLSettings.DecimalSeparator := char(''); //Don't override decimalseparator;
          XMLSettings.ExportFormat := AccessCompatible;
          XMLSettings.CreateXSD := true; //create header info, Access likes this.
          XMLExport.FormatSettings := XMLSettings;
          XMLExport.Dataset := TDataset(FDatasets[i]);
          if FDataSets.Count = 1 then
            XMLExport.FileName := FBaseFileName + '.access.xml'
          else
            XMLExport.FileName := FBaseFileName + FExportNames[i] + '.access.xml';
          XMLExport.Execute;
        finally
          XMLSettings.Free;
          XMLExport.Free;
        end;
      end;
    end;
    efADONetXML: //ADO.Net XML
    begin
      for i := 0 to FDatasets.Count - 1 do
      begin
        XMLExport := TXMLXSDExporter.Create(nil);
        XMLSettings := TXMLXSDFormatSettings.Create(true);
        try
          XMLSettings.ExportFormat := ADONETCompatible;
          XMLSettings.CreateXSD := true; //create header info, ADO.Net likes this.
          XMLExport.FormatSettings := XMLSettings;
          XMLExport.Dataset := TDataset(FDatasets[i]);
          if FDatasets.Count=1 then
            XMLExport.FileName := FBaseFileName + '.Net.xml'
          else
            XMLExport.FileName := FBaseFileName + FExportNames[i] + '.Net.xml';
          XMLExport.Execute;
        finally
          XMLSettings.Free;
          XMLExport.Free;
        end;
      end;
    end;
    efCSV:
    begin
      for i := 0 to FDatasets.Count - 1 do
      begin
        CSVExport := TCSVExporter.Create(nil);
        CSVSettings := TCSVFormatSettings.Create(true);
        try
          CSVSettings.HeaderRow := true;
          CSVSettings.FieldDelimiter := ','; //we could use ; also
          {$IF FPC_FULLVERSION<20701}
          CSVSettings.QuoteStrings := [qsAlways, qsSpace, qsDelimiter];
          {$ENDIF}
          CSVSettings.StringQuoteChar := '"';
          CSVExport.FormatSettings := CSVSettings;
          CSVExport.Dataset := TDataset(FDatasets[i]);
          if FDatasets.Count=1 then
            CSVExport.FileName := FBaseFileName + '.csv'
          else
            CSVExport.FileName := FBaseFileName + FExportNames[i] + '.csv';
          CSVExport.Execute;
        finally
          CSVSettings.Free;
          CSVExport.Free;
        end;
      end;
    end;
    efDBF4, efDBF7: //DBase IV, 7
    {$IF FPC_FULLVERSION<20701}
      // In r24490, fixed DBF export field name truncation issue
      // Applicable to all export formats except DBaseVII
    {$WARNING Please use a newer compiler (r24490 or later). This compiler does not correctly export max 10 character DBF fields.}
    {$ENDIF}
    begin
      for i := 0 to FDatasets.Count - 1 do
      begin
        DBFExport := TFPDBFExport.Create(nil);
        DBFSettings := TDBFExportFormatSettings.Create(true);
        try
          DBFSettings.AutoRenameFields := true;
          // Support most common format, even though it has 10 character field names
          case FExportFormat of
            efDBF4: DBFSettings.TableFormat := tfDBaseIV;
            efDBF7: DBFSettings.TableFormat := tfDBaseVII;
            else
              raise Exception.Create('Unknown DBF export format');
          end;
          DBFExport.FormatSettings := DBFSettings;
          DBFExport.Dataset := TDataset(FDatasets[i]);
          if FDatasets.Count = 1 then
            DBFExport.FileName := FBaseFileName + '.dbf'
          else
            DBFExport.FileName := FBaseFileName + FExportNames[i] + '.dbf';
          DBFExport.Execute;
        finally
          DBFSettings.Free;
          DBFExport.Free;
        end;
      end;
    end;
    efXLS, efXLSX, efCalc:
    //Excel/Calc spreadsheet. We use fpSpreadsheet for this.
    { While we could have exported using fpXMLXSDExport,
    the generated XML does not support multiline cells,
    which is essential in this case }
    begin
      DataCorruption := false;
      ExcelWorkBook := TsWorkbook.Create;
      try
        for i := 0 to FDatasets.Count - 1 do
        begin
          ExcelWorkSheet := ExcelWorkBook.AddWorksheet(FExportNames[i]);
          TheColumn := 0;
          TheRow := 0;
          { Write out field names on first row }
          for TheColumn := 0 to TDataset(FDatasets[i]).Fields.Count - 1 do
            ExcelWorkSheet.WriteUTF8Text(0, TheColumn,
              TDataset(FDatasets[i]).Fields[TheColumn].FieldName);

          { Data on second and further rows }
          TheRow := 1; //Start after line of field names
          while not TDataset(FDatasets[i]).EOF do
          begin
            for TheColumn := 0 to TDataset(FDatasets[i]).Fields.Count - 1 do
            begin
              case TDataset(FDatasets[i]).Fields[TheColumn].DataType of
                ftDate,ftDateTime,ftTime,ftTimeStamp:
                begin
                  ExcelWorkSheet.WriteDateTime(TheRow, TheColumn,
                      TDataset(FDatasets[i]).Fields[TheColumn].AsDateTime);
                end;
                ftCurrency,ftFloat,ftFMTBcd:
                begin
                  ExcelWorkSheet.WriteNumber(TheRow, TheColumn,
                      TDataset(FDatasets[i]).Fields[TheColumn].AsCurrency);
                end;
                ftAutoInc,ftInteger,ftLargeInt,ftSmallint:
                begin
                  ExcelWorkSheet.WriteNumber(TheRow, TheColumn,
                      TDataset(FDatasets[i]).Fields[TheColumn].AsLargeInt);
                end;
                else
                  // Treat as string (ftString,ftMemo,ftBlob,ftGUID etc)
                  begin
                    FieldValue:=TDataset(FDatasets[i]).Fields[TheColumn].AsString;
                    // requires a new fpspreadsheet for limit checking... older versions
                    // silently corrupt output
                    try
                      ExcelWorkSheet.WriteUTF8Text(TheRow, TheColumn,
                        FieldValue);
                    except
                      // silently discard size limit exceptions.
                      // todo: get fpspreadsheet to use exception error numbers to more easily check
                      DataCorruption:=true;
                    end;
                  end;
                end;
            end;
            TDataset(FDatasets[i]).Next;
            Inc(TheRow);
          end;
        end;
        // Save the spreadsheet to a file, overwriting any existing files
        case FExportFormat of
          efXLS:
          begin
            OutputFile:=FBaseFileName + STR_EXCEL_EXTENSION;
            ExcelWorkbook.WriteToFile(OutputFile,XLS_OUTPUT_FORMAT, true)
          end;
          efXLSX:
          begin
            OutputFile:=FBaseFileName + STR_OOXML_EXCEL_EXTENSION;
            ExcelWorkbook.WriteToFile(OutputFile,sfOOXML, true)
          end;
          efCalc:
          begin
            OutputFile:=FBaseFileName + STR_OPENDOCUMENT_CALC_EXTENSION;
            ExcelWorkbook.WriteToFile(OutputFile,sfOpenDocument, true)
          end;
          else
            raise Exception.CreateFmt('Unknown export format code %d',[FExportFormat]);
          end;
      finally
        //ExcelWorkSheet.Free; //Don't do this, the workbook parent will free it.
        ExcelWorkBook.Free;
      end;
      if DataCorruption then
        raise Exception.CreateFmt('Wrote file %s but data is corrupted (e.g. text field too long) ',[OutputFile]);
    end;
    efFirebird: //Firebird
    begin
      OutputFile := FBaseFileName + '.fdb';
      FBConn := TIBConnection.Create(nil);
      DbTran := TSQLTransaction.Create(nil);
      Conversion := TTargetDB.Create(FBConn);
      try
        FBConn.Transaction := DbTran;
        FBConn.HostName := '';
        FBConn.Username := 'SYSDBA';
        FBConn.Password := 'masterkey'; //default password for SYSDBA
        FBConn.Charset := 'UTF8'; //Send and receive string data in UTF8 encoding
        FBConn.Params.Add('PAGE_SIZE=16384'); // Useful for larger indexes=>larger possible column sizes
        FBConn.DatabaseName := OutputFile;
        // Create the database if it doesn't exist
        if not (FileExistsUTF8(OutputFile)) then
          FBConn.CreateDB; //Create the database file.
        FBConn.Open;

        if not (FBConn.Connected) then
          raise Exception.CreateFmt('Could not connect to database %s', [OutputFile]);

        // Create table without overwriting existing tables,
        // and insert data
        for i := 0 to FDatasets.Count - 1 do
        begin
          Conversion.SourceDataSet:=TDataSet(FDataSets[i]);
          Conversion.DestinationTable:=FExportNames[i];
          Conversion.CloneDataset;
        end;

        FBConn.Connected := false;
      finally
        FBConn.Free;
        DBTran.Free;
        Conversion.Free;
      end;
    end;
    efLatex: //Latex
    begin
      for i := 0 to FDatasets.Count - 1 do
      begin
        LatexExport := TTexExporter.Create(nil);
        try
          LatexExport.Dataset := TDataset(FDatasets[i]);
          if FDatasets.Count = 1 then
            LatexExport.FileName := FBaseFileName + '.tex'
          else
            LatexExport.FileName := FBaseFileName + FExportNames[i] + '.tex';
          LatexExport.Execute;
        finally
          LatexExport.Free;
        end;
      end;
    end;
    efSQLite:
    begin
      OutputFile := FBaseFileName + '.sqlite';
      SQLiteConn := TSQLite3Connection.Create(nil);
      DbTran := TSQLTransaction.Create(nil);
      Conversion := TTargetDB.Create(SQLiteConn);
      try
        SQLiteConn.Transaction := DbTran;
        SQLiteConn.Charset := 'UTF8';
        SQLiteConn.DatabaseName := OutputFile;
        SQLiteConn.Open; //creates file if not present

        if not (SQLiteConn.Connected) then
          raise Exception.CreateFmt('Could not connect to database %s', [OutputFile]);

        for i := 0 to FDataSets.Count - 1 do
        begin
          Conversion.SourceDataSet:=TDataSet(FDataSets[i]);
          Conversion.DestinationTable:=FExportNames[i];
          Conversion.CloneDataset;
        end;
        SQLiteConn.Connected := false;
      finally
        SQLiteConn.Free;
        DBTran.Free;
        Conversion.Free;
      end;
    end
    else
      raise Exception.CreateFmt('Unknown export format code: %d.', [FExportFormat]);
  end;

  // Restore original position
  for i:=0 to FDatasets.Count-1 do
  begin
    TDataset(FDatasets[i]).GotoBookMark(CurrentPos[i]);
  end;
  Result := true;
end;

constructor TDatabaseExporter.Create;
begin
  inherited Create;
  FDatasets:=TFPList.Create;
  FExportNames:=TStringList.Create;
end;

destructor TDatabaseExporter.Destroy;
begin
  FDatasets.Free;
  FExportNames.Free;
  inherited Destroy;
end;

end.
