unit dbcreate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dbconfig, sqldb,
  IBConnection, mssqlconn {includes sybase support},
  mysql50conn,mysql51conn,mysql55conn,
  {$IF (FPC_FULLVERSION >= 20604)}
  mysql56conn,
  {$ENDIF}
  odbcconn,
  {$IF (FPC_FULLVERSION >= 20604) OR NOT(DEFINED(MSWIN64))}
  oracleconnection,
  {$ENDIF}
  pqconnection,sqlite3conn;

type

  { TDBCreate }

  TDBCreate = class(TObject)
  private
    FConnectionConfig: TDBConnectionConfig;
    FConnection: TSQLConnection;
    FErrors: TStringList;
  public
    // Creates database (if Config property is set)
    function CreateDB: boolean;
    property Config: TDBConnectionConfig read FConnectionConfig write FConnectionConfig;
    // Text of any errors during database creation
    property Errors: TStringList read FErrors;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TDBCreate }

function TDBCreate.CreateDB: boolean;
var
  Tran: TSQLTransaction;
begin
  result:=false;
  if not(assigned(FConnectionConfig)) then
  begin
    Errors.Add('ConnectionConfig property needs to be set first.');
    exit(false);
  end;

  case FConnectionConfig.DBType of
    'Firebird':
    begin
      FConnection:=TIBConnection.Create(nil);
      (FConnection as TIBConnection).Params.Add('PAGE_SIZE=16384'); // Useful for larger indexes=>larger possible column sizes;
    end;
    'MSSQLServer':
    begin
      FConnection:=TMSSQLConnection.Create(nil);
      (FConnection as TMSSQLConnection).Params.Add('AutoCommit=true'); // Required for creating db
    end;
    'MySQL5.0': FConnection:=TMySQL50Connection.Create(nil);
    'MySQL5.1': FConnection:=TMySQL51Connection.Create(nil);
    'MySQL5.5': FConnection:=TMySQL55Connection.Create(nil);
    {$IF (FPC_FULLVERSION >= 20604)}
    'MySQL5.6': FConnection:=TMySQL56Connection.Create(nil);
    {$ENDIF}
    'ODBC': FConnection:=TODBCConnection.Create(nil);
    {$IF (FPC_FULLVERSION >= 20604) OR NOT(DEFINED(MSWIN64))}
    'Oracle': FConnection:=TOracleConnection.Create(nil);
    {$ENDIF}
    'PostGreSQL': FConnection:=TPQConnection.Create(nil);
    'SQLite3':
      FConnection:=TSQLite3Connection.Create(nil);
    'Sybase':
    begin
      FConnection:=TSybaseConnection.Create(nil);
      (FConnection as TSybaseConnection).Params.Add('AutoCommit=true'); // Required for creating db
    end;
  else
    FErrors.Add('Unknown connector type '+FConnectionConfig.DBType+'. Please correct program code.');
    exit(false);
  end;

  FConnection.DatabaseName:=FConnectionConfig.DBPath;
  FConnection.CharSet:=FConnectionConfig.DBCharset;
  FConnection.HostName:=FConnectionConfig.DBHost;
  FConnection.Password:=FConnectionConfig.DBPassword;
  FConnection.UserName:=FConnectionConfig.DBUser;

  Tran:=TSQLTransaction.Create(nil);
  try
    FConnection.Transaction:=Tran;
    try
      case FConnectionConfig.DBType of
        'MSSQLServer':
        begin
          // Should work on FPC2.6.x+
          FConnection.ExecuteDirect('CREATE DATABASE '+FConnectionConfig.DBPath);
          result:=true;
        end;
        'Sybase':
        begin
          // Almost works at least on FPC trunk with an error about not enough space for copying master
          FConnection.CreateDB;
          result:=true;
        end
      else
        begin
          FConnection.CreateDB;
          result:=true;
        end;
      end;
    except
      on E: Exception do
      begin
        FErrors.Add(E.Message);
      end;
    end;
  finally
    Tran.Free;
  end;
end;

constructor TDBCreate.Create;
begin
  FErrors:=TStringList.Create;
end;

destructor TDBCreate.Destroy;
begin
  FErrors.Free;
  if assigned(FConnection) then FConnection.Free;
  inherited Destroy;
end;

end.

