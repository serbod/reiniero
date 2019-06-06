unit importfileing;

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
  {$ifdef unix}
  cwstring,
  {$endif}
  Classes, SysUtils, importfile, transactioninfo, csvdocument, bankutils;

type

  { TImportFileING }

  TImportFileING = class(TImportFile)
  private
    FParser: TCSVParser;
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

{ TImportFileING }

procedure TImportFileING.Initialize;
begin
  FParser := TCSVParser.Create;
  FParser.Delimiter := ','; //comma-separated
end;

function TImportFileING.ParseLine(Line: string; var TheTransaction: TTransaction): boolean;
var
  i: integer;
  PlusFactor: Currency;
begin
  Result := false;
  FParser.SetSource(Line); //resets parser at the same time
  {
  Header:
  "Datum","Naam / Omschrijving","Rekening","Tegenrekening","Code","Af Bij","Bedrag (EUR)","MutatieSoort","Mededelingen"
  Data:
  "20110528","ABN AMRO BANK>AMSTERDAM   S2E474","685411886","","GM","Af","100,00","Geldautomaat","28-05-2011 16:12 001     0011316        "
  }

  // Copy over data to transaction, making sure it's converted to UTF8
  for i := 1 to 9 do
  begin
    if FParser.ParseNextCell then
      case FParser.CurrentCol + 1 of
        1: TheTransaction.BookDate:=YMDToDateDef(FParser.CurrentCellText); //transaction date yyyyMMdd
        2: TheTransaction.Memo := AnsiToUTF8(trim(FParser.CurrentCellText)); //description
        3:
        begin
          // Account. Try Dutch bank account first
          TheTransaction.Account:=AnsiToUTF8(NormalizeNLAccountDef(FParser.CurrentCellText,FParser.CurrentCellText));
        end;
        4: // counter account. Try Dutch account first
        begin
          TheTransaction.ContraAccount := AnsiToUTF8(NormalizeNLAccountDef(FParser.CurrentCellText,FParser.CurrentCellText));
        end;
        5: ;
        //todo: code: perhaps add to transactiontype
        6: if uppercase(trim(FParser.CurrentCellText)) = 'BIJ' then
            PlusFactor := 1.0
          else
            PlusFactor := -1.0; //sign for the amount (i.e. debit or credit)
        // We're assuming a Dutch locale here for this format as the bank is Dutch.
        // Therefore expect a decimal , not a decimal .
        // However, it's easiest to just use .
        7:
        begin
           //amount, in euros?!?! (to do: find out how dollar account etc looks like)
           TheTransaction.Currency:='EUR';
           TheTransaction.Amount := PlusFactor * (StrToCurrDef(StringReplace(Trim(FParser.CurrentCellText), ',', '.', [rfReplaceAll]), 0));
        end;
        8: ; // MutatieSoort; ignore
        9: TheTransaction.Memo := AnsiToUTF8(TheTransaction.Memo + ' ' + trim(FParser.CurrentCellText)); //messages
        else
          Exception.CreateFmt('Too many fields found; field content: %s', [FParser.CurrentCellText])
      end
    else
      Exception.CreateFmt('Error parsing line %s', [Line]);
  end;
  Result := true;
end;

function TImportFileING.ValidateFile: boolean;
var
  Line: string = '';
  LookFor: string;
  i: integer = 0;
begin
  FValidated := true;
  // Read first line - which should be a header:
  {"Datum","Naam / Omschrijving","Rekening","Tegenrekening","Code","Af Bij","Bedrag (EUR)","MutatieSoort","Mededelingen"}
  try
    // Read first line and rewind to beginning:
    ReadALine(Line);
    FFileStream.Position:=0;
    FParser.SetSource(Line); //resets parser at the same time
    while FParser.ParseNextCell do
    begin
      Inc(i); // Start counting at field 1
      case FParser.CurrentCol + 1 of
        1: LookFor := 'Datum';
        2: LookFor := 'Naam / Omschrijving';
        3: LookFor := 'Rekening';
        4: LookFor := 'Tegenrekening';
        5: LookFor := 'Code';
        6: LookFor := 'Af Bij';
        7: LookFor := 'Bedrag (EUR)'; // Ignore pre 2002 data - or we need to add conversion routines
        8: LookFor := 'MutatieSoort';
        9: LookFor := 'Mededelingen';
        else
          FValidated := false; //too many fields
      end;
      if FValidated then
        if trim(FParser.CurrentCellText) <> LookFor then
        begin
          FValidated := false;
          break;
        end;
    end;
  except
    FValidated := false;
  end;
  Result := FValidated;
end;

procedure TImportFileING.Import(Transactions: TTransactionList);
var
  i: integer;
  Line: string = '';
  Trans: TTransaction;
begin
  if not (FValidated) then
    ValidateFile;
  if not (FValidated) then
    raise Exception.CreateFmt('Could not validate file %s on import.', [FFilename]);

  i := 1; //line counter
  if ReadAline(Line) then
  begin
    inc(i); //skip over first, header line.
    while ReadALine(Line) do
    begin
      Trans := TTransaction.Create;
      if ParseLine(Line, Trans) then
        Transactions.Add(Trans)
      else
        raise Exception.CreateFmt('Invalid import line %d in file %s.', [i, FFileName]);
      Inc(i);
    end;
  end;
end;

constructor TImportFileING.Create;
begin
  inherited Create;
  Initialize;
end;

constructor TImportFileING.Create(ImportFile: string);
begin
  inherited Create(ImportFile);
  Initialize;
end;

destructor TImportFileING.Destroy;
begin
  FParser.Free;
  inherited Destroy;
end;

end.
