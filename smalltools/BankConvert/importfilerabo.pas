unit importfilerabo;
{ Import module for Rabobank (a Dutch bank) transactions in their new
  IBAN .txt format }
  
{ Copyright (c) 2013 Reinier Olislagers

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

  { TImportFileRabo }

  TImportFileRabo = class(TImportFile)
  private
    FParser: TCSVParser;
    FTempAccount: string; //temporarily store (contra) accuont info
    FTempMemo: string; //temporarily store memo info

    // Shared initialization routine called by all Create variants
    procedure Initialize;
    // Parses a line and fills transaction info (or returns an error)
    function ParseLine(Line: string; var TheTransaction: TTransaction): boolean;
  public
    function ValidateFile: boolean; override;
    procedure Import(Transactions: TTransactionList); override;
    constructor Create;
    constructor Create(ImportFile: string); override;
    destructor Destroy; override;
  end;

implementation

{ TImportFileRabo }

procedure TImportFileRabo.Initialize;
begin
  FParser:=TCSVParser.Create;
  FParser.Delimiter:=','; //comma-separated
  FParser.QuoteChar:='"'; //using double quotes
end;

function TImportFileRabo.ParseLine(Line: string; var TheTransaction: TTransaction): boolean;
var
  i: integer;
  PlusFactor: Currency;
  Description: array[1..6] of string; //ANSI; collect description lines in field data
begin
  result:=false;
  {
  "NL51RABO0101629141","EUR","20130227","D","25.00","NL42INGB0000000286","boekenwinkel.nl","20130228","id","","91380376857119616000000000000000000","0090613007938900 Boeking naar de I","NG test De Boekenwinkel ? thuis","","","","27-02-2013 10:59 0090613007938900","",""
  1 Account
  2 Currency
  3 "Rentedatum"; date; ignore
  4 (D)ebit -/(c)redit +
  5 Amount, with decimal .
  6 Contra account
  7 Contra name
  8 Book date
  9 Boekcode/transaction type
  10 Filler
  11 Description1
  12 Description2
  13 Description3
  14 Description4
  15 Description5
  16 Description6
  17 End to end ID (SEPA credit transfer: code given by initiator)
  18 ID contra account holder (SEPA credit transfer)
  19 Mandate ID: (SEPA Direct Debit: description mandate)
  }
  FParser.SetSource(Line); //resets parser at the same time

  // Copy over data; file format seems to be ANSI so convert
  for i := 1 to 6 do
  begin
    Description[i]:='';
  end;

  for i := 1 to 19 do
  begin
    if FParser.ParseNextCell then
      case FParser.CurrentCol+1 of
        1:
        begin
          //account. Try Dutch account first
          TheTransaction.Account:=AnsiToUTF8(NormalizeNLAccountDef(FParser.CurrentCellText,FParser.CurrentCellText));
        end;
        2: TheTransaction.Currency:=AnsiToUTF8(FParser.CurrentCellText); //currency/valuta
        3: ; //valuteringsdatum; ignore
        4: if uppercase(trim(FParser.CurrentCellText)) = 'C' then
            PlusFactor := 1.0
           else
            PlusFactor := -1.0; //sign for the amount (i.e. debit or credit) //(D)ebit/- or (C)redit/+
        5: TheTransaction.Amount:=StrToCurrDef(Trim(FParser.CurrentCellText),0); //amount, decimal .
        6: TheTransaction.ContraAccount:=AnsiToUTF8(NormalizeNLAccountDef(FParser.CurrentCellText,FParser.CurrentCellText)); //contra account
        7: TheTransaction.ContraName:=AnsiToUTF8(FParser.CurrentCellText);//contra account name
        8: TheTransaction.BookDate:=YMDToDateDef(FParser.CurrentCellText); //transaction date yyyyMMdd
        9: TheTransaction.TransType:=FParser.CurrentCellText;//transaction code
        10: ;//filler, ignore
        11..16: //Description1 through 6
        begin
          Description[FParser.CurrentCol+1-10]:=trim(FParser.CurrentCellText);
        end;
        17: ; //End to end ID, ignore
        18: ; //ID contra account holder, ignore
        19: ; //Mandate ID, ignore
        else
          Exception.CreateFmt('Too many fields found; field content: %s',[FParser.CurrentCellText])
      end
    else
      Exception.CreateFmt('Error parsing line %s',[Line]);
  end;
  TheTransaction.Memo:=AnsiToUTF8(trim(Description[1]+' '+Description[2]+' '+Description[3]+' '+
    Description[4]+' '+Description[5]+' '+Description[6]));
  result:=true;
end;

function TImportFileRabo.ValidateFile: boolean;
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

  // Count commas as a rough first start
  if CountPos(',', Line)<18 then
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

procedure TImportFileRabo.Import(Transactions: TTransactionList   );
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

constructor TImportFileRabo.Create;
begin
  inherited Create;
  Initialize;
end;

constructor TImportFileRabo.Create(ImportFile: string);
begin
  inherited Create(ImportFile);
  Initialize;
end;

destructor TImportFileRabo.Destroy;
begin
  FParser.Free;
  inherited Destroy;
end;

end.

