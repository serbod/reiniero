unit dataset2sql;

{ Conversion from dataset to SQL create table/insert table statements

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
  Classes, SysUtils, sqldb, DB;

type
  {First 12 entries are copies of those in the FPC test suite code;
  the other ones (odbc_msaccess,...) are overrides for specific databases behind ODBC}
  TSQLConnType = (mysql40, mysql41, mysql50, mysql51, mysql55, postgresql, interbase, odbc, oracle, sqlite3, mssql, sybase, odbc_msaccess);

const
  MySQLConnTypes = [mysql40, mysql41, mysql50, mysql51, mysql55];
  // Content matches TSQLConnection.GetConnectionInfo(citServerType);
  // order matches TSQLConnType
  SQLConnTypesNames: array [TSQLConnType] of string[19] =
    ('MYSQL40', 'MYSQL41', 'MYSQL50', 'MYSQL51', 'MYSQL55', 'POSTGRESQL',
    'INTERBASE' {actually meant for Firebird}, 'ODBC', 'ORACLE', 'SQLITE3', 'MSSQL', 'SYBASE',
    'ODBC_MSACCESS');

type
  TFieldMapping = record
    FieldClass: TFieldClass;
    SQL: array[TSQLConnType] of string;
  end;

  { TTargetDB }

  TTargetDB = class(TObject)
  private
    FConnection: TSQLConnection;
    FServerType: string;     //Type of server the connection is connected to
    FConnType: TSQLConnType; //TSQLConnType that matches FServerType
    FObjDelimStart, FObjDelimEnd: string; //Delimiters used around objects (e.g. to enable creating fields with reserved names)
    FSourceDataset: TDataSet; //Source of data
    FTableName: string; //Destination table
  public
    // Copies over data from dataset to target database/table
    function CopyData: boolean;
    // Creates table if needed and copies dataset content
    function CloneDataset: boolean;
    // Creates table matching source dataset, if it doesn't already exist
    function CreateTable: boolean;
    // Connection to the target database. Should be open before running conversion
    property Connection: TSQLConnection read FConnection;
    // Table that should be created/to which data should be copied. Default table1
    property DestinationTable: string read FTableName write FTableName;
    // Dataset that is the source of the data. Required.
    property SourceDataSet: TDataSet read FSourceDataset write FSourceDataset;
    // Specify a connection suitable for the target db.
    // Target db type will be determined by this connection unless overridden by
    // overridedbtype
    constructor Create(TargetConnection: TSQLConnection; OverrideDBType: string = '');
    destructor Destroy; override;
  end;

implementation

const
  Fields = 14;
  // Todo: this certainly needs verification/updates
  FieldMappings: array[0..Fields - 1] of TFieldMapping =
    ((FieldClass: TStringField; SQL: (' VARCHAR(', ' VARCHAR(', ' VARCHAR(', ' VARCHAR(', ' VARCHAR(',
    ' VARCHAR(', ' VARCHAR(', ' VARCHAR(', ' VARCHAR(', ' VARCHAR(', ' VARCHAR(', ' VARCHAR(', ' VARCHAR(')), // + size
    (FieldClass: TIntegerField; SQL: (' INTEGER ', ' INTEGER ', ' INTEGER ', ' INTEGER ', ' INTEGER ',
    ' INTEGER ', ' INTEGER ', ' INTEGER ', ' INTEGER ', ' INTEGER ', ' INTEGER ', ' INTEGER ', ' INTEGER ')),
    (FieldClass: TAutoIncField; SQL: (' INTEGER ', ' INTEGER ', ' INTEGER ', ' INTEGER ', ' INTEGER ',
    ' INTEGER ', ' INTEGER ', ' INTEGER ', ' INTEGER ', ' INTEGER ', ' INTEGER ', ' INTEGER ', ' INTEGER ')),
    (FieldClass: TLongIntField; SQL: (' INTEGER ', ' INTEGER ', ' INTEGER ', ' INTEGER ', ' INTEGER ',
    ' INTEGER ', ' INTEGER ', ' INTEGER ', ' INTEGER ', ' INTEGER ', ' INTEGER ', ' INTEGER ', ' INTEGER ')),
    (FieldClass: TSmallIntField; SQL: (' SMALLINT ', ' SMALLINT ', ' SMALLINT ', ' SMALLINT ', ' SMALLINT ',
    ' SMALLINT ', ' SMALLINT ', ' SMALLINT ', ' SMALLINT ', ' SMALLINT ', ' SMALLINT ', ' SMALLINT ', ' SMALLINT ')),
    (FieldClass: TFloatField; SQL: (' FLOAT ', ' DOUBLE ', ' DOUBLE ', ' DOUBLE ', ' DOUBLE ', ' DOUBLE ',
    ' DOUBLE PRECISION ', ' DOUBLE ', ' DOUBLE ', ' DOUBLE ', ' DOUBLE ', ' DOUBLE ', ' DOUBLE ')),
    //Firebird float: 32 bit,single precision, approx 7 decimal digits
    //Firebird double precision: 64 bit,double precision, approx 15 decimal digits
    (FieldClass: TDateTimeField; SQL: (' TIMESTAMP ', ' TIMESTAMP ', ' TIMESTAMP ', ' TIMESTAMP ',
    ' TIMESTAMP ', ' TIMESTAMP ', ' TIMESTAMP ', ' TIMESTAMP ', ' TIMESTAMP ', ' TIMESTAMP ', ' DATETIME ', ' DATETIME ', ' DATETIME ')),
    (FieldClass: TDateField; SQL: (' DATE ', ' DATE ', ' DATE ', ' DATE ', ' DATE ', ' DATE ', ' DATE ',
    ' DATE ', ' DATE ', ' DATE ', ' DATE ', ' DATE ', ' DATE ')),
    (FieldClass: TTimeField; SQL: (' TIME ', ' TIME ', ' TIME ', ' TIME ', ' TIME ', ' TIME ', ' TIME ',
    ' TIME ', ' TIME ', ' TIME ', ' TIME ', ' TIME ', ' TIME ')),
    (FieldClass: TCurrencyField; SQL: (' DECIMAL ', ' DECIMAL ', ' DECIMAL ', ' DECIMAL ', ' DECIMAL ',
    ' DECIMAL ', ' DECIMAL ', ' DECIMAL ', ' DECIMAL ', ' DECIMAL ', ' DECIMAL ', ' DECIMAL ', ' DECIMAL ')),
    (FieldClass: TBooleanField; SQL: (' BOOLEAN ', ' BOOLEAN ', ' BOOLEAN ', ' BOOLEAN ', ' BOOLEAN ', ' BOOLEAN ',
    ' INTEGER ', ' BOOLEAN ', ' BOOLEAN ', ' BOOLEAN ', ' BIT ', ' BIT ', ' BOOLEAN ')),
    (FieldClass: TMemoField; SQL: (' VARCHAR ', ' VARCHAR ', ' VARCHAR ', ' VARCHAR ', ' VARCHAR ', ' VARCHAR ',
    ' BLOB SUB_TYPE TEXT ', ' VARCHAR ', ' VARCHAR ', ' VARCHAR ', ' VARCHAR ', ' VARCHAR ', ' LONGTEXT ')),
    (FieldClass: TGraphicField; SQL: (' BLOB ', ' BLOB ', ' BLOB ', ' BLOB ', ' BLOB ', ' BLOB ', ' BLOB ',
    ' BLOB ', ' BLOB ', ' BLOB ', ' BLOB ', ' BLOB ', ' BLOB ')),
    (FieldClass: TBlobField; SQL: (' BLOB ', ' BLOB ', ' BLOB ', ' BLOB ', ' BLOB ', ' BLOB ', ' BLOB ',
    ' BLOB ', ' BLOB ', ' BLOB ', ' BLOB ', ' BLOB ', ' BLOB '))
    );

{ TTargetDB }

function TTargetDB.CopyData: boolean;
var
  i: integer;
  InsertQuery: TSQLQuery;
  InsertSQL: string;
  OriginalPosition: TBookMark;
  WasOpen: boolean;
  WasInTransaction: boolean;
begin
  if (Assigned(FConnection) = false) or (FConnection.Connected = false) then
    raise Exception.Create('Connection is nil or not active.');
  WasOpen := FSourceDataset.Active;
  FSourceDataset.Open;
  InsertQuery := TSQLQuery.Create(nil);
  try
    try
      InsertQuery.DataBase := FConnection;
      // Remember but rollback any previous transaction:
      WasInTransaction := FConnection.Transaction.Active;
      if WasInTransaction then
        FConnection.Transaction.Rollback;
      FConnection.Transaction.StartTransaction;
      InsertSQL := 'INSERT INTO ' + FObjDelimStart + FTableName + FObjDelimEnd + ' (';
      // Field names; assumes target field name is same as source displayname
      for i := 0 to FSourceDataset.Fields.Count - 1 do
      begin
        InsertSQL := InsertSQL + FObjDelimStart + FSourceDataset.Fields[i].DisplayName + FObjDelimEnd;
        if (i < FSourceDataset.Fields.Count - 1) then
          InsertSQL := InsertSQL + ', ';
      end;
      InsertSQL := InsertSQL + ' ) VALUES (';
      //Parameters
      for i := 0 to FSourceDataset.Fields.Count - 1 do
      begin
        InsertSQL := InsertSQL + ':' + FSourceDataset.Fields[i].DisplayName;
        if (i < FSourceDataset.Fields.Count - 1) then
          InsertSQL := InsertSQL + ', ';
      end;
      InsertSQL := InsertSQL + ') ';
      InsertQuery.SQL.Text := InsertSQL;

      OriginalPosition := FSourceDataset.GetBookMark;
      FSourceDataset.First;
    except
      // Errors should be reverted as a batch:
      on D: EDatabaseError do
      begin
        Result := false;
        FConnection.Transaction.Rollback;
        raise; //pass on to caller
      end;
    end;
    try
      while not (FSourceDataset.EOF) do
      begin
        // Note: currently ODBC units for FPC do not support unicode, so
        // e.g. odbc_msaccess/MS Access export will have faulty data if unicode is written
        InsertQuery.Params.CopyParamValuesFromDataset(FSourceDataset, true);
        InsertQuery.ExecSQL;
        FSourceDataset.Next;
      end;
      FSourceDataset.GotoBookmark(OriginalPosition);
    except
      on D: EDatabaseError do
      begin
        Result := false;
        // The entire insert operation should be reverted as a batch:
        FConnection.Transaction.Rollback;
        raise; //pass on to caller
      end;
    end;
    FConnection.Transaction.Commit;
    if WasInTransaction then
      FConnection.Transaction.StartTransaction;
    Result := true;
  finally
    InsertQuery.Free;
  end;
  if not (WasOpen) then
    FSourceDataset.Close;
end;

function TTargetDB.CloneDataset: boolean;
begin
  Result := CreateTable;
  if Result then
    Result := CopyData;
end;

function TTargetDB.CreateTable: boolean;
  // Adapted from http://www.drbob42.com/examine/examin98.htm
var
  i: integer;
  CreateSQL: string;
  FieldMapping: TFieldMapping;
  Found: boolean;
  WasOpen: boolean;
  WasInTransaction: boolean;
begin
  if (Assigned(FConnection) = false) or (FConnection.Connected = false) then
    raise Exception.Create('Connection to target database is nil or not active.');
  WasOpen := FSourceDataset.Active;
  FSourceDataset.Open;
  try
    CreateSQL := 'CREATE TABLE ' + FObjDelimStart + FTableName + FObjDelimEnd + ' (';
    for i := 0 to FSourceDataset.Fields.Count - 1 do
    begin
      CreateSQL := CreateSQL + ' ' + FObjDelimStart + FSourceDataset.Fields[i].DisplayName + FObjDelimEnd + ' ';
      if FSourceDataset.FieldDefs[i].FieldClass = TStringField then
        CreateSQL := CreateSQL + ' VARCHAR(' + IntToStr(FSourceDataset.FieldDefs[i].Size) + ')'
      else
      begin
        found := false;
        for FieldMapping in FieldMappings do
          if not found then
          begin
            if FSourceDataset.FieldDefs[i].FieldClass = FieldMapping.FieldClass then
            begin
              CreateSQL := CreateSQL + FieldMapping.SQL[FConnType];
              found := true;
            end;
          end;

        if not found then
          raise Exception.Create('Unsupported field type ' + FSourceDataset.FieldDefs[i].FieldClass.ClassName);
      end;
      if FSourceDataset.FieldDefs[i].Required then
        CreateSQL := CreateSQL + ' NOT NULL';
      if (i < FSourceDataset.Fields.Count - 1) then
        CreateSQL := CreateSQL + ', ';
    end;

    CreateSQL := CreateSQL + ')';
    try
      //ignore any previously open transactions
      WasInTransaction := FConnection.Transaction.Active;
      if WasInTransaction then
        FConnection.Transaction.Rollback;
      FConnection.Transaction.StartTransaction;
      FConnection.ExecuteDirect(CreateSQL);
      FConnection.Transaction.Commit;
      if WasInTransaction then
        FConnection.Transaction.StartTransaction;
    except
      // ignore errors; table may already exist etc
      on D: EDatabaseError do
      begin
        FConnection.Transaction.Rollback;
      end;
    end;
    Result := true;
  finally
    if not (WasOpen) then
      FSourceDataset.Close;
  end;
end;

constructor TTargetDB.Create(TargetConnection: TSQLConnection; OverrideDBType: string = '');
var
  i: TSQLConnType;
begin
  inherited Create;
  FConnection := TargetConnection;
  {$IF FPC_FULLVERSION>=20701}
  FServerType := Connection.GetConnectionInfo(citServerType);
  {$ELSE}
  {$WARNING No support for detecting server type in this version of FPC. Assuming Firebird.}
  FServerType := 'Firebird';
  {$ENDIF}

  if uppercase(FServerType) = 'FIREBIRD' then
    FServerType := 'Interbase';
  if OverrideDBType <> '' then
    FServerType := OverrideDBType;
  FConnType := TSQLConnType.odbc; // Generic SQL, good as a fallback
  for i := low(SQLConnTypesNames) to high(SQLConnTypesNames) do
    if UpperCase(FServerType) = SQLConnTypesNames[i] then
      FConnType := i;

  if FConnType = odbc_msaccess then
  begin
    FObjDelimStart := '[';
    FObjDelimEnd := ']';
    //todo: perhaps do this for other engines, too. Probably easier to get delimiter from tsqlconnection
  end;
  FTableName := 'table1'; //default
end;

destructor TTargetDB.Destroy;
begin
  inherited Destroy;
end;

end.
