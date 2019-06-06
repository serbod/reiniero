unit importfileabnamro;
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
  {$ifdef unix}
  cwstring,
  {$endif}
  Classes, SysUtils, importfile, transactioninfo, csvdocument, dateutils, bankutils;

type

  { TImportFileABNAmro }

  TImportFileABNAmro = class(TImportFile)
  private
    FParser: TCSVParser;
    FTempAccount: string; //temporarily store (contra) accuont info
    FTempMemo: string; //temporarily store memo info

    // Shared initialization routine called by all Create variants
    procedure Initialize;
    // Parses a line and fills transaction info (or returns an error)
    function ParseLine(Line: string; var TheTransaction: TTransaction): boolean;
    // Takes possible account+memo line and tries to split it up, formatting
    // any found Dutch account numbers as well.
    // Results go to FTempAccount and FTempMemo
    procedure SplitAccountFromMemo(const Total: string);
  public
    function ValidateFile: boolean; override;
    procedure Import(Transactions: TTransactionList); override;
    constructor Create;
    constructor Create(ImportFile: string); override;
    destructor Destroy; override;
  end;

implementation

{ TImportFileABNAmro }

procedure TImportFileABNAmro.SplitAccountFromMemo(const Total: string);
// Account is anything that has only numbers and periods.
// Does not require Dutch account numbers as counterparties may use foreign accounts
// Does format Dutch account numbers if positively identified.
// Stores results in FTempAccount and FTempMemo
var
  i: integer;
begin
  FTempAccount:='';
  FTempMemo:='';
  if Total='' then
  begin
    FTempMemo:='';
  end
  else
  begin
    FTempMemo:=trim(Total);
    i:=1;
    while (i<Length(FTempMemo)) and (FTempMemo[i] in ['0'..'9','.']) do
    begin
      inc(i);
    end;
    if (i<Length(FTempMemo)) and (FTempMemo[i] in [' ',#9]) then
    begin
      FTempAccount:=copy(FTempMemo,1,i-1);
      FTempAccount:=NormalizeNLAccountDef(FTempAccount,FTempAccount);
      FTempMemo:=copy(FTempMemo,i+1,Length(FTempMemo));
    end
    else
    begin
      // No valid account
      FTempAccount:='';
    end;
  end;
end;

procedure TImportFileABNAmro.Initialize;
begin
  FParser:=TCSVParser.Create;
  FParser.Delimiter:=#9; //tab-separated
end;

function TImportFileABNAmro.ParseLine(Line: string; var TheTransaction: TTransaction): boolean;
var
  i: integer;
begin
  result:=false;
  {
  (note: tab separators. 7 per line)
  380935772	EUR	20010108	0,00	1,77	null	0,00	UW SALDO PER 31-12-2007 BEDRAAGTEUR                0,00 CREDIT.
  or
  380995772	EUR	20061127	0,00	1,77	20061125	50,00	 38.09.95.515 A NAAM          WESTERVLIERWEG 18                4 192 QX  WORTEKSAP
  }
  FParser.SetSource(Line); //resets parser at the same time

  // Copy over data; make sure it's UTF8.
  for i := 1 to 8 do
  begin
    if FParser.ParseNextCell then
      case FParser.CurrentCol+1 of
        1:
        begin
          //account. Try Dutch account first
          TheTransaction.Account:=AnsiToUTF8(NormalizeNLAccountDef(FParser.CurrentCellText,FParser.CurrentCellText));
        end;
        2: TheTransaction.Currency:=AnsiToUTF8(FParser.CurrentCellText); //currency/valuta
        3: TheTransaction.BookDate:=YMDToDateDef(FParser.CurrentCellText); //transaction date yyyyMMdd
        4: ; //beginning balance: ignore
        5: ; //ending balance: ignore
        6: ; //effective date/valuteringsdatum?: ignore
        // We're assuming a Dutch locale here for this format as the bank is Dutch.
        // Therefore expect a decimal , not a decimal .
        // However for casting to currency we need a .
        7: TheTransaction.Amount:=StrToCurrDef(StringReplace(Trim(FParser.CurrentCellText),',','.',[rfReplaceAll]),0);
        // last field: description + contra account
        8:
        begin
          SplitAccountFromMemo(Trim(FParser.CurrentCellText));
          TheTransaction.ContraAccount:=AnsiToUTF8(FTempAccount);
          TheTransaction.Memo:=AnsiToUTF8(FTempMemo);
        end
        else
          Exception.CreateFmt('Too many fields found; field content: %s',[FParser.CurrentCellText])
      end
    else
      Exception.CreateFmt('Error parsing line %s',[Line]);
  end;
  result:=true;
end;

function TImportFileABNAmro.ValidateFile: boolean;
// Basically reads first line to check if fields are in the right position
// This file format has no header lines
var
  Line: string='';
  Trans: TTransaction;
begin
  FValidated:=false;
  // Read first line and rewind to beginning:
  ReadALine(Line);
  FFileStream.Position:=0;

  // Count tabs as a rough first start
  if CountPos(#9, Line)<>7 then
    exit(FValidated);

  Trans:=TTransaction.Create;
  try
    try
      FValidated:=ParseLine(Line, Trans);
    except
      FValidated:=false; //it doesn't exactly matter yet what the error was.
      // This would be different if actually reading the data.
    end;
  finally
    Trans.Free;
  end;
  result:=FValidated;
end;

procedure TImportFileABNAmro.Import(Transactions: TTransactionList   );
var
  i: integer;
  Line: string='';
  Trans: TTransaction;
begin
  if not(FValidated) then
    ValidateFile;
  if not(FValidated) then
    raise Exception.CreateFmt('Could not validate file %s on import.', [FFilename]);

  i:=1; //line counter
  while ReadALine(Line) do
  begin
    Trans:=TTransaction.Create;
    if ParseLine(Line, Trans) then
      Transactions.Add(Trans)
    else
      raise Exception.CreateFmt('Invalid import line %d in file %s.',[i,FFileName]);
    inc(i);
  end;
end;

constructor TImportFileABNAmro.Create;
begin
  inherited Create;
  Initialize;
end;

constructor TImportFileABNAmro.Create(ImportFile: string);
begin
  inherited Create(ImportFile);
  Initialize;
end;

destructor TImportFileABNAmro.Destroy;
begin
  FParser.Free;
  inherited Destroy;
end;

end.

