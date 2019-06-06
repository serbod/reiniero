unit importfilesberbank;

{ Reads bank transactions from Sberbank statements.
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
Statements are expected to be in text format, fixed width, Windows CP1251

to do: add balance support, e.g. opening balance at
ОСТАТОК НА НАЧАЛО ПЕРИОДА:
or closing balance after period:
ОСТАТОК НА КОНЕЦ ПЕРИОДА:
}

{$mode objfpc}{$H+}

interface

uses
  {$ifdef unix}
  cwstring,
  {$endif}
  Classes, SysUtils, importfile, transactioninfo, semiformattedtext,
  lconvencoding, lazutf8 {for utf8copy};

type

  { TImportFileSberBank }

  TImportFileSberBank = class(TImportFile)
  private
    FOurAccount: string; //our account number
    FOurCurrency: string; //currency in which our account is denominated
    FParser: TSemiFmtText;
    // Read account number, which is mentioned once a page
    procedure GetAccountNumber;
    // Read currency of our account, which is mentioned at the top
    procedure GetCurrency;
    // Shared initialization routine called by all Create variants
    procedure Initialize;
    // Parses a logical line (can span physical lines) and fills transaction info (or returns an error)
    // Returns false if end of file reached
    function ReadNextRecord(var TheTransaction: TTransaction): boolean;
    // Converts codes used by Sberbank to ISO codes
    function TranslateCurrency(SberCode: string): string;
  public
    function ValidateFile: boolean; override;
    procedure Import(Transactions: TTransactionList); override;
    constructor Create;
    constructor Create(ImportFile: string); override;
    destructor Destroy; override;
  end;

implementation


function RusToDateDef(DateValue: string; DefaultDate: TDateTime=OldDate): TDateTime;
//                         1234567
// Converts something like 26АВГ13 to a date
// Note: year is optional; if so, we don't know which year to uss as bookdates
// around end of December become tricky. So keep generating invalid dates as we do
var
  Stripped: string;
  MonthPart: string;
  Month: integer;
begin
  result:=DefaultDate;
  Month:=0;
  Stripped:=Trim(DateValue);

  // Fix slightly deviating formats:
  if length(Stripped)=length('26АВГ2013') then
    Stripped:=UTF8Copy(Stripped,1,5)+UTF8Copy(Stripped,8,2)
  else if length(Stripped)=length('6АВГ13') then
    Stripped:='0'+Stripped;

  if length(Stripped)=length('26АВГ13') then
  begin
    MonthPart:=UTF8UpperCase(UTF8Copy(Stripped,3,3));
    case MonthPart of
      'ЯНВ': Month:=1;
      'ФЕВ': Month:=2;
      'МАР': Month:=3;
      'АПР': Month:=4;
      'МАЙ': Month:=5;
      'ИЮН': Month:=6;
      'ИЮЛ': Month:=7;
      'АВГ': Month:=8;
      'СЕН': Month:=9;
      'ОКТ': Month:=10;
      'НОЯ': Month:=11;
      'ДЕК': Month:=12;
    end;

    if Month>0 then
      Result:=EncodeDate(
        2000+strtointdef(UTF8Copy(Stripped,6,2),0),
        Month,
        strtointdef(UTF8Copy(Stripped,1,2),0)
        );
  end;
end;


{ TImportFileSberBank }

procedure TImportFileSberBank.GetAccountNumber;
// Using a semiformatted text parser based on the one used for accounts
var
  AccountLine: string;
  PrivateParser: TSemiFmtText;
  NewDef: TSemiFieldDef;
  i, LineCounter: integer;
begin
  // Use the general parser settings as much as possible
  if FFileName='' then
    raise Exception.Create('No source filename specified.');
  PrivateParser := TSemiFmtText.Create(FFileName);
  try
    PrivateParser.Encoding:=FParser.Encoding;
    PrivateParser.RecordHeaderStart:=FParser.RecordHeaderStart;
    PrivateParser.RecordHeaderEnd:=FParser.RecordHeaderEnd;
    PrivateParser.RecordFooterStart:=FParser.RecordFooterStart;

    NewDef:=TSemiFieldDef.Create;
    NewDef.Name:='Account'; //actually ТИП КАРТЫ, № КАРТЫ=card type, card number.
    // Multiline, obfuscated but has some digits left at the end
    NewDef.FirstPosition:=1;
    NewDef.LastPosition:=20;
    NewDef.IndicatesFirstLineRecord:=true; //Indicates our "sentinel" field:
    PrivateParser.FieldDefs.Add(NewDef);

    // Get the data - first value only, ASSUMING only reports for one card in one file
    //todo: verify, otherwise split out per page processing... :(
    FOurAccount:='';
    // We read each line as a separate record.
    // If we keep reading, we read the same data on each page, so hack it
    // and stop after 3 lines
    LineCounter:=0;
    while (LineCounter<3) and (PrivateParser.GetNextRecord) do
    begin
      for i := 0 to PrivateParser.FieldDefs.Count-1 do
      begin
        // These names must match the definition above
        if PrivateParser.FieldDefs[i].Name='Account' then
        begin
          AccountLine:=trim(PrivateParser.FieldData[i]);
          if AccountLine='' then //last line done
            exit;
          FOurAccount:=trim(FOurAccount+' '+AccountLine);
        end;
      end;
      LineCounter:=LineCounter+1;
    end;
    if FOurAccount='' then FOurAccount:='ERROR';

  finally
    PrivateParser.Free;
  end;
end;

procedure TImportFileSberBank.GetCurrency;
// Parsing the stuff ourselves
const
  LookFor='ВАЛЮТА СЧЕТА'; //on column 86..97 inclusive; "account currency"
var
  Lines: TStringList;
  i:integer;
begin
  // Use some general parser settings
  FOurCurrency:='RUB'; //sensible default for a Russian bank...
  if FFileName='' then
    raise Exception.Create('No source filename specified.');

  Lines:=TStringList.Create;
  try
    Lines.LoadFromFile(FFileName);
    if FParser.Encoding<>65001 {UTF8} then
    begin
      //todo: there probably is a simpler way of converting from multiple possibilities?
      case FParser.Encoding of
        437: Lines.Text:=CP437ToUTF8(Lines.Text);
        850: Lines.Text:=CP850ToUTF8(Lines.Text);
        1251: Lines.Text:=CP1251ToUTF8(Lines.Text);
        else raise Exception.CreateFmt('Unsupported codepage %d',[FParser.Encoding]);
      end;
    end;
    for i:=0 to Lines.Count-1 do
    begin
      if UTF8Copy(Lines[i],86,1+97-86)=LookFor then
      try
        FOurCurrency:=TranslateCurrency((UTF8Copy(Lines[i+1],86,1+97-86)));
      except
        // Ignore end of file
      end;
    end;
  finally
    Lines.Free;
  end;
end;

procedure TImportFileSberBank.Initialize;
const
  HeaderSeparator='--------------------+-----+-----+-------+--------------------------+---------------+--------------';
var
  NewDef: TSemiFieldDef;
begin
  if FFileName<>'' then
    FParser := TSemiFmtText.Create(FFileName)
  else
    FParser := TSemiFmtText.Create;
  FParser.Encoding:=1251; //Windows 1251 Cyrillic
  // All these separators happen to be the same in this layout:
  FParser.RecordHeaderStart:=HeaderSeparator;
  FParser.RecordHeaderEnd:=HeaderSeparator;
  FParser.RecordFooterStart:=HeaderSeparator;

  {
  Not adding this as it conflicts with the sentinel field below:
  found 3 lines of text in this field, only 2 lines (or 1) in a sample record with sentinel
  NewDef:=TSemiFieldDef.Create;
  NewDef.Name:='Account'; //actually ТИП КАРТЫ, № КАРТЫ=card type, card number.
  // Multiline, obfuscated but has some digits left at the end
  NewDef.FirstPosition:=1;
  NewDef.LastPosition:=20;
  FParser.FieldDefs.Add(NewDef);

  Instead use a private parser in a separate function
  }

  NewDef:=TSemiFieldDef.Create;
  NewDef.Name:='Date'; //date of transaction. Note: fairly useless: no year info
  NewDef.FirstPosition:=21;
  NewDef.LastPosition:=26;
  NewDef.IndicatesFirstLineRecord:=true; //Indicates our "sentinel" field:
  // when there's data in it, a new record has begun.
  FParser.FieldDefs.Add(NewDef);

  NewDef:=TSemiFieldDef.Create;
  NewDef.Name:='DateProcessed'; //date processed into account??=>bookdate
  NewDef.FirstPosition:=27;
  NewDef.LastPosition:=33;
  FParser.FieldDefs.Add(NewDef);

  NewDef:=TSemiFieldDef.Create;
  NewDef.Name:='OperationNumber';
  NewDef.FirstPosition:=34;
  NewDef.LastPosition:=41;
  FParser.FieldDefs.Add(NewDef);

  NewDef:=TSemiFieldDef.Create;
  NewDef.Name:='Description'; //Heading: transaction kind, place, currency
  // Note that we split out the currency in the field below
  NewDef.FirstPosition:=42;
  NewDef.LastPosition:=63;
  FParser.FieldDefs.Add(NewDef);

  NewDef:=TSemiFieldDef.Create;
  NewDef.Name:='Currency';
  // This is the original transaction currency, not the currency of the account
  // so we don't use this
  NewDef.FirstPosition:=65;
  NewDef.LastPosition:=68;
  FParser.FieldDefs.Add(NewDef);

  NewDef:=TSemiFieldDef.Create;
  NewDef.Name:='AmountOriginalCurrency';
  NewDef.FirstPosition:=69;
  NewDef.LastPosition:=84;
  FParser.FieldDefs.Add(NewDef);

  NewDef:=TSemiFieldDef.Create;
  NewDef.Name:='Amount';
  NewDef.FirstPosition:=85;
  NewDef.LastPosition:=95;
  FParser.FieldDefs.Add(NewDef);

  NewDef:=TSemiFieldDef.Create;
  NewDef.Name:='DebitCredit'; //if credit, CR is used. Debit is empty
  NewDef.FirstPosition:=96;
  NewDef.LastPosition:=97;
  FParser.FieldDefs.Add(NewDef);
end;

function TImportFileSberBank.ReadNextRecord(var TheTransaction: TTransaction): boolean;
var
  i: integer;
  Amount: Currency;
  Credit: boolean;
begin
  Result := false;
  if FParser.FileName='' then
    FParser.FileName:=FFileName;
  if FParser.GetNextRecord then
  begin
    Amount:=0;
    Credit:=false; //deduct from account by default
    for i := 0 to FParser.FieldDefs.Count-1 do
    begin
      // These names must match the definition above
      case FParser.FieldDefs[i].Name of
        'DateProcessed':
        begin
          TheTransaction.BookDate:=RusToDateDef(FParser.FieldData[i]); //transaction date yyyyMMdd
          // This has to go somewhere; let's do it at the start:
          TheTransaction.Account:=FOurAccount;
        end;
        'Description': TheTransaction.Memo := trim(FParser.FieldData[i]);
        'Currency': TheTransaction.Currency := FOurCurrency;
        'Amount': Amount := StrToFloatDef(trim(FParser.FieldData[i]),0);
        'DebitCredit':
        begin
          if uppercase(trim(FParser.FieldData[i]))='CR' then
            Credit:=true
          else
            Credit:=false;
        end;
        else ; //case else: do nothing
      end;
    end;
    if Credit then
      TheTransaction.Amount:=Amount
    else
      TheTransaction.Amount:=-Amount;
    Result := true;
  end
  else
    result:=false;
end;

function TImportFileSberBank.TranslateCurrency(SberCode: string): string;
var
  Stripped: string;
begin
  Stripped:=UTF8Copy(trim(SberCode),1,3);
  case Stripped of
    'RUR': result:='RUB'; //Russian ruble/rouble
    else //ASSUME other codes are regular ISO
      result:=Stripped;
  end;
end;

function TImportFileSberBank.ValidateFile: boolean;
const
  //Both are UTF8
  LookFor1='                                   С Б Е Р Б А Н К  Р О С С И И';
  LookFor2='                                       ОТЧЕТ ПО СЧЕТУ КАРТЫ';
begin
  FValidated := false;
  if FParser.FileName='' then
    FParser.FileName:=FFileName;
  if (FParser.RawLines[0]=LookFor1) and
    (FParser.RawLines[1]=LookFor2) and
    (Pos(FParser.RecordHeaderStart, FParser.RawLines.Text)>0) and
    (Pos(FParser.RecordHeaderEnd, FParser.RawLines.Text)>0) then
      FValidated:=true;

  Result := FValidated;
end;

procedure TImportFileSberBank.Import(Transactions: TTransactionList);
var
  Trans: TTransaction;
begin
  if not (FValidated) then
    ValidateFile;
  if not (FValidated) then
    raise Exception.CreateFmt('Could not validate file %s on import.', [FFilename]);

  // Get currency our account is denominated in
  // Separate step because the record layout is different from regular records
  GetCurrency;

  // Get card number/account number, which is obfuscated but better than nothing I suppose.
  // Separate step because the record layout is different from regular records
  GetAccountNumber;

  // Read the actual transaction info:
  while true do
  begin
    Trans := TTransaction.Create;
    try
      if ReadNextRecord(Trans) then
        Transactions.Add(Trans)
      else
      begin
        Trans.Free;
        break;
      end;
    except
      raise Exception.CreateFmt('Invalid import record in file %s.', [FFileName]);
    end;
  end;
end;

constructor TImportFileSberBank.Create;
begin
  inherited Create;
  Initialize;
end;

constructor TImportFileSberBank.Create(ImportFile: string);
begin
  inherited Create(ImportFile);
  Initialize;
end;

destructor TImportFileSberBank.Destroy;
begin
  FParser.Free;
  inherited Destroy;
end;

end.
