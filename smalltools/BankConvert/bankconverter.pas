unit bankconverter;
{ Copyright (c) 2012-2013 Reinier Olislagers

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
  Classes, SysUtils, transactioninfo,
  importfile, importfileabnamro, importfileing, importfilesberbank, importfileorov,importfilerabo,
  exportfile, exportscreen;

type

  // Form in which data is stored; when adding, add corresponding importers/exporters
  TBankDataType = (bdtAutoDetect, bdtINGCSV, bdtABNTAB, bdtOROVDatabase,
  bdtRabobank, bdtSberBank,
  bdtOFX, bdtScreen);

const
  TBankDtDescription: array [TBankDataType] of string=('Autodetect','ING comma separated','ABN-Amro comma separated',
  'Offline Rekeningoverzicht database',
  'Rabobank text file','Sberbank text file','OFX (MS Money etc) file', 'Screen (output only)');

type

  { TBankConverter }
  TBankConverter = class(TObject)
  private
    FInputFormat: TBankDataType;
    FOutputFormat: TBankDataType;
    FTransactions: TTransactionList;
    FInputFile: string;
    FOutputFile: string;
    function InputFormatDetect: TBankDataType;
    function OutputFormatDetect: TBankDataType;
  public
    property InputFile: string read FInputFile write FInputFile;
    // Input format; will be autodetected if not set
    property InputFileFormat: TBankDataType read FInputFormat write FInputFOrmat;
    property OutputFile: string read FOutputFile write FOutputFile;
    // Output format; will be autodetected from output file extension if not set
    property OutputFormat: TBankDataType read FOutputFormat write FOutputFormat;
    // Transactions that were imported/converted
    property Transactions: TTransactionList read FTransactions;
    // Perform import/export conversion; requires at least input file (default output is screen)
    procedure Convert;
    // Performs export only; default output is screen
    procedure ExportTransactions;
    // Performs import only; requires at least input file
    procedure ImportTransactions;
    constructor Create;
    destructor Destroy; override;
  end;
implementation

{ TBankConverter }

function TBankConverter.InputFormatDetect: TBankDataType;
const
  DefaultResult = bdtScreen;  //There's no bdtInvalid so use this
var
  Importer: TImportFile;
begin
  // Try to validate each input format
  // from the most specific to the most general
  result:=DefaultResult;

  // We could also generate an exception here, but let's leave it to the import
  // code.
  if not(FileExists(FInputFile)) then
    exit(result);

  // Once the amount of import formats starts to grow, rewrite using register
  // procedure etc.
  if result=DefaultResult then
  begin
    Importer:=TImportFileABNAmro.Create(FInputFile);
    try
      try
        if Importer.ValidateFile then
          exit(bdtABNTAB);
      except
        // Ignore validation error here; try next validator
      end;
    finally
      Importer.Free;
    end;
  end;

  if result=DefaultResult then
  begin
    Importer:=TImportFileING.Create(FInputFile);
    try
      try
        if Importer.ValidateFile then
          exit(bdtINGCSV);
      except
        // Ignore validation error here; try next validator
      end;
    finally
      Importer.Free;
    end;
  end;

  if result=DefaultResult then
  begin
    Importer:=TImportFileSberBank.Create(FInputFile);
    try
      try
        if Importer.ValidateFile then
          exit(bdtSberBank);
      except
        // Ignore validation error here; try next validator
      end;
    finally
      Importer.Free;
    end;
  end;

  if result=DefaultResult then
  begin
    Importer:=TImportFileRabo.Create(FInputFile);
    try
      try
        if Importer.ValidateFile then
          exit(bdtRabobank);
      except
        // Ignore validation error here; try next validator
      end;
    finally
      Importer.Free;
    end;
  end;

  {$IFDEF MSWINDOWS}
  if result=DefaultResult then
  begin
    Importer:=TImportFileOROV.Create(FInputFile);
    try
      try
        if Importer.ValidateFile then
          exit(bdtOROVDatabase);
      except
        // Ignore validation error here; try next validator
      end;
    finally
      Importer.Free;
    end;
  end;
  {$ENDIF}
end;

function TBankConverter.OutputFormatDetect: TBankDataType;
begin
  // read output file using input format, detect any, then return value
  // useful for appending transactions to existing file
  //todo: write me, for now hardcoded to screen
  result:=bdtScreen;
end;

procedure TBankConverter.Convert;
begin
  ImportTransactions;
  ExportTransactions;
end;

procedure TBankConverter.ExportTransactions;
var
  Exporter:TExportFile;
begin
  // Default to screen if no output file given
  if FOutputFormat=bdtAutoDetect then
    if FOutputFile='' then
      FOutputFormat:=bdtScreen
    else
      FOutputFormat:=OutputFormatDetect;
  case FOutputFormat of
    bdtScreen: Exporter:=TExportScreen.Create;
    else
      raise Exception.Create('Unsupported output format!');
  end;
  try
    Exporter.FileName:=FOutputFile;
    Exporter.ExportFile(FTransactions);
  finally
    Exporter.Free;
  end;
end;

procedure TBankConverter.ImportTransactions;
var
  Importer: TImportFile;
begin
  if FInputFormat=bdtAutoDetect
    then FInputFormat:=InputFormatDetect;
  case FInputFormat of
    bdtABNTAB: Importer:=TImportFileABNAmro.Create;
    bdtINGCSV: Importer:=TImportFileING.Create;
    bdtOROVDatabase: Importer:=TImportFileOROV.Create;
    bdtRabobank: Importer:=TImportFileRabo.Create;
    bdtSberBank:
    begin
      Importer:=TImportFileSberBank.Create;
    end
    else
      raise Exception.Create('Unsupported input format.');
  end;
  try
    Importer.FileName:=FInputFile;
    Importer.Import(FTransactions);
  finally
    Importer.Free;
  end;
end;

constructor TBankConverter.Create;
begin
  FInputFormat:=bdtAutoDetect;
  FOutputFormat:=bdtAutoDetect;
  FTransactions:=TTransactionList.Create;
end;

destructor TBankConverter.Destroy;
begin
  FTransactions.Free;
  inherited Destroy;
end;

end.

