unit importfileorov;
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
//todo: add reference balance functionality - though probably not in a transaction but an account class
interface

uses
  {$ifdef unix}
  cwstring,
  {$endif}
  Classes, SysUtils, importfile, transactioninfo, odbcconn, sqldb;

type

  { TImportFileOROV }

  TImportFileOROV = class(TImportFile)
  private
    FODBCDriver: string;
  public
    function ValidateFile: boolean; override;
    procedure Import(Transactions: TTransactionList); override;
    constructor Create;
    constructor Create(ImportFile: string); override;
    destructor Destroy; override;
  end;

implementation

{ TImportFileOROV }
const
  {$ifdef mswindows}
  NewDriver = 'Microsoft Access Driver (*.mdb, *.accdb)'; //Office components etc, also 64 bit driver
  {$else}
  // Linux, Unix, OSX, FreeBSD... can use mdbtools and libmdbodbc.so.1 etc
  NewDriver = 'MDBTools'; //driver name as installed at least on Debian
  {$endif}
  OldDriver = 'Microsoft Access Driver (*.mdb)'; //MDAC/Jet driver; supplied with Win2K+
  TableName = 'mutaties';

function TImportFileOROV.ValidateFile: boolean;
// Open MS Access mdb, try to open mutaties table
var
  DBConn: TODBCConnection;
  DBTran: TSQLTransaction;
  FieldList: TStringList;

  procedure ValidateFieldList;
  begin
    DBConn.Driver:=FODBCDriver;
    DBConn.Params.Add('DBQ='+FFileName);
    DBConn.Open;
    DBConn.GetFieldNames(TableName,FieldList);
    if FieldList.Count>0 then
      FValidated:=true;
    DbConn.Close;
  end;

begin
  // Fail by default, even if already validated before:
  FValidated:=false;
  if not(FileExists(FFileName)) then
    exit;
  if FODBCDriver='' then
    FODBCDriver:=NewDriver;
  DBConn:=TODBCConnection.Create(nil);
  DBTran:=TSQLTransaction.Create(nil);
  FieldList:=TStringList.Create;
  try
    DBConn.Transaction:=DBTran;
    ValidateFieldList;

    // Try again with older driver
    if not(FValidated) and (FODBCDriver=NewDriver) then
    begin
      FODBCDriver:=OldDriver;
      ValidateFieldList;
    end;
  finally
    DBTran.Free;
    DBConn.Free;
    FieldList.Free;
  end;
  result:=FValidated;
end;

procedure TImportFileOROV.Import(Transactions: TTransactionList);
var
  DBConn: TODBCConnection;
  DBTran: TSQLTransaction;
  PlusFactor: integer; //todo: perhaps currency to avoid conversion?
  Query: TSQLQuery;
begin
  Transactions.Clear;
  if not(FValidated) then
    ValidateFile;
  if not(FValidated) then
    raise Exception.CreateFmt('Could not validate file %s on import.', [FFilename]);

  DBConn:=TODBCConnection.Create(nil);
  DBTran:=TSQLTransaction.Create(nil);
  Query:=TSQLQuery.Create(nil);
  try
    DBConn.Transaction:=DBTran;
    Query.Database:=DBConn;
    //To avoid
    //"could not return primary key metadata" errors:
    Query.UsePrimaryKeyAsKey:=false;
    DBConn.Driver:=FODBCDriver;
    DBConn.Params.Add('DBQ='+FFileName);
    DBConn.Open;
    Query.SQL.Text:='select datum,naam_oms,rekening,tegenrekening,code,ba,bedrag,mededelingen from '
      +tablename+' order by id,datum ';
    Query.Open;
    while not(Query.EOF) do
    begin
      //todo: split out name from description in naam_oms
      if UpperCase(Query.FieldByName('ba').AsString)='BIJ' then
        PlusFactor:=1 {payment into our account}
      else
        PlusFactor:=-1; {payment out of our account}
      // The ODBC driver should be supplying ANSI characters here; convert to UTF8 if needed
      Transactions.Add(TTransaction.Create(
        Query.FieldByName('datum').AsDateTime,
        AnsiToUTF8(Query.FieldByName('rekening').AsString),
        AnsiToUTF8(Query.FieldByName('tegenrekening').AsString),
        AnsiToUTF8(Query.FieldByName('naam_oms').AsString),
        'EUR' {assume transactions are all in euros},
        Query.FieldByName('bedrag').AsCurrency*PlusFactor,
        Query.FieldByName('code').AsString {or mutatiesoort, the long version},
        AnsiToUTF8(Query.FieldByName('mededelingen').AsString)));
      Query.Next;
    end;
  finally
    Query.Free;
    DBTran.Free;
    DBConn.Free;
  end;
end;

constructor TImportFileOROV.Create;
begin
  inherited Create;
  FODBCDriver:=''; //signify we haven't tested driver
end;

constructor TImportFileOROV.Create(ImportFile: string);
begin
  Inherited Create(ImportFile);
  FODBCDriver:=''; //signify we haven't tested driver
end;

destructor TImportFileOROV.Destroy;
begin
  inherited Destroy;
end;

end.

