unit transactioninfo;
{ Represents a bank transaction (payment, cash withdrawal, etc)
  Copyright (c) 2012-2013 Reinier Olislagers

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
//todo: add FPC 2.7.1 $H specific code that indicates what strings are used

interface

uses
  Classes, SysUtils, fgl, md5;

type

  { TTransaction }

  TTransaction = class
  private
    FAccount: string;
    FAmount: Currency;
    FCurrency: string;
    FBookDate: TDateTime;
    FContraAccount: string;
    FHashID: string; //cached result of GetHashID
    function GetBookDate: TDateTime;
    function GetHashID: string;
    procedure SetAccount(AValue: string);
    procedure SetAmount(AValue: Currency);
    procedure SetBookDate(AValue: TDateTime);
    procedure SetContraAccount(AValue: string);
  public
    // Account number, in UTF8
    property Account: string read FAccount write SetAccount;
    property Amount: Currency read FAmount write SetAmount;
    // Currency in which transaction was performed (ISO 4217 3 characters, e.g. USD, EUR, RUB)
    property Currency: string read FCurrency write FCurrency;
    property BookDate: TDateTime read GetBookDate write SetBookDate;
    // Account number of counterparty, in UTF8
    property ContraAccount: string read FContraAccount write SetContraAccount; //counter account
    // Identifier for record
    // Does not completely guarantee uniqueness: 2 payments same day may have different Memo.
    // However memo import differs between mechanisms so needs human verification
    property HashID: string read GetHashID; //Hash over bookdate,account,contraacount,amount
  public
    // Name associated with counter account, in UTF8
    ContraName: string;
    TransType: string; //e.g. cash withdrawal, standing order etc...
    //to do: implement enum for this?
    // Remarks/memo, in UTF8
    Memo: string;
    // Shows transaction in display/print format
    function Display: string;
    // Tests for equality on all fields.
    function IsExactlyEqual(AnotherTransaction: TTransaction): boolean;
    constructor Create;
    // Create transaction while giving details (in UTF8 strings)
    constructor Create(TransBookDate: TDateTime; TransAccount: string;
      TransContraAccount: string; TransContraName: string;
      TransCurrency: string; TransAmount: Currency;
      TransTransType: string; TransMemo: string);
  end;

  TTransactionList = specialize TFPGObjectList<TTransaction>;

implementation

{ TTransaction }

function TTransaction.GetHashID: string;
var
  CanonicalDate: string;
begin
  if FHashID='' then
  begin
    // For now, just append.
    DateTimeToString(CanonicalDate, 'yyyymmddhhnnss',BookDate);
    // todo: replace with faster hash function that returns smaller results. Collisions are not very important
    FHashID:=md5print(md5string(CanonicalDate+Account+ContraAccount+currtostr(Amount)));
    // no memo; same transaction may be imported via different mechanisms with different memo
  end;
  result:=FHashID
end;

function TTransaction.GetBookDate: TDateTime;
begin
  //for debugging
  result:=FBookDate;
end;

procedure TTransaction.SetAccount(AValue: string);
begin
  if FAccount=AValue then Exit;
  FAccount:=AValue;
  FHashID:=''; //invalidate cache
end;

procedure TTransaction.SetAmount(AValue: Currency);
begin
  if FAmount=AValue then Exit;
  FAmount:=AValue;
  FHashID:=''; //invalidate cache
end;

procedure TTransaction.SetBookDate(AValue: TDateTime);
begin
  if FBookDate=AValue then Exit;
  FBookDate:=AValue;
  FHashID:=''; //invalidate cache
end;

procedure TTransaction.SetContraAccount(AValue: string);
begin
  if FContraAccount=AValue then Exit;
  FContraAccount:=AValue;
  FHashID:=''; //invalidate cache
end;

function TTransaction.Display: string;
begin
  result:='****'+HashID+'****'+LineEnding+
    'On: '+DateToStr(FBookDate)+LineEnding;
  if FAmount>=0 then
    result:=result+FAccount+'<=='+FContraAccount+' - '+ContraName+LineEnding
  else
    result:=result+FAccount+'==>'+FContraAccount+' - '+ContraName+LineEnding;
  result:=result+FCurrency+' '+CurrToStrF(FAmount,ffNumber,2)+LineEnding+
    '('+TransType+')'+LineEnding+
    Memo;
end;

function TTransaction.IsExactlyEqual(AnotherTransaction: TTransaction): boolean;
begin
  if Self.GetHashID<>AnotherTransaction.GetHashID then
    result:=false
  else
  begin
    // If hashes are the same, a collision may have occurred
    // Also, the memo may still be different
    // attempt comparison in speed order... more for own vanity than actual benefit
    if (Self.Amount=AnotherTransaction.Amount) and
      (Self.Currency=AnotherTransaction.Currency) and
      (Self.BookDate=AnotherTransaction.Bookdate) and
      (Self.Account=AnotherTransaction.Account) and
      (Self.ContraAccount=AnotherTransaction.ContraAccount) and
      (Self.ContraName=AnotherTransaction.ContraName) and
      (Self.Memo=AnotherTransaction.Memo) then
      result:=true;
  end;
end;

constructor TTransaction.Create;
begin
  inherited Create;
  FHashID:=''; //reset so it gets calculated when requested
end;

constructor TTransaction.Create(TransBookDate: TDateTime; TransAccount: string;
  TransContraAccount: string; TransContraName: string; TransCurrency: string; TransAmount: Currency;
  TransTransType: string; TransMemo: string);
begin
  Create;
  FBookDate:=TransBookDate;
  FAccount:=TransAccount;
  FContraAccount:=TransContraAccount;
  ContraName:=TransContraName;
  FAmount:=TransAmount;
  FCurrency:=TransCurrency;
  TransType:=TransTransType;
  Memo:=TransMemo;
  GetHashID; //calculate FHashID
end;

end.

