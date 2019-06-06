unit fbscriptmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, Forms, Controls, Graphics, Dialogs,
  StdCtrls, EditBtn,
  fbscript,
  sqldb, db, IBConnection, dbconfiggui,dbconfig;

type

  { TForm1 }

  TForm1 = class(TForm)
    ErrorMemo: TSynMemo;
    LoadScriptButton: TButton;
    SaveScriptButton: TButton;
    RunScriptButton: TButton;
    ScriptFileEdit: TFileNameEdit;
    ScriptMemo: TSynMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LoadScriptButtonClick(Sender: TObject);
    procedure RunScriptButtonClick(Sender: TObject);
    procedure SaveScriptButtonClick(Sender: TObject);
  private
    FConn: TSQLConnector;
    FQuery: TSQLQuery;
    FTran: TSQLTransaction;
    function ConnectionTest(ChosenConfig: TDBConnectionConfig; var ErrorMessages: string): boolean;
    procedure LoadScript;
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.LoadScriptButtonClick(Sender: TObject);
begin
  LoadScript;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  CurrentDir: string;
  LoginForm: TDBConfigForm;
begin
  FConn:=TSQLConnector.Create(nil);
  FQuery:=TSQLQuery.Create(nil);
  FTran:=TSQLTransaction.Create(nil);
  FConn.Transaction:=FTran;
  FQuery.DataBase:=FConn;

  LoginForm:=TDBConfigForm.Create(self);
  try
    // The test button on dbconfiggui will link to this procedure:
    LoginForm.ConnectionTestCallback:=@ConnectionTest;
    LoginForm.ShowCreateDBButton:=true;
    LoginForm.ConnectorType.Clear; //remove any default connectors
    // Now add the dbs that you support - use the name of the *ConnectionDef.TypeName property
    LoginForm.ConnectorType.AddItem('Firebird', nil);
    case LoginForm.ShowModal of
    mrOK:
      begin
        //user wants to connect, so copy over db info
        FConn.ConnectorType:=LoginForm.Config.DBType;
        FConn.HostName:=LoginForm.Config.DBHost;
        FConn.DatabaseName:=LoginForm.Config.DBPath;
        FConn.UserName:=LoginForm.Config.DBUser;
        FConn.Password:=LoginForm.Config.DBPassword;
        FConn.Transaction:=FTran;
      end;
    mrCancel:
      begin
        ShowMessage('You canceled the database login. Application will terminate.');
        Close;
      end;
    end;
  finally
    LoginForm.Free;
  end;

  CurrentDir:= IncludeTrailingPathDelimiter(ExtractFilePath(Paramstr(0)));
  ScriptFileEdit.FileName:=CurrentDir+'test.sql';
  if fileexists(ScriptFileEdit.FileName) then
    LoadScript;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FConn.Free;
  FQuery.Free;
  FTran.Free;
end;

procedure TForm1.RunScriptButtonClick(Sender: TObject);
var
  Script: TFBScript;
begin
  Script:=TFBScript.Create(nil);
  try
    Script.Script.AddStrings(ScriptMemo.Lines);
    Script.Database:=FConn;
    try
      Script.ExecuteScript;
      ErrorMemo.Lines.Assign(Script.Statements);
    except
      on E: Exception do
      begin
        ErrorMemo.Lines.Text:=E.Message;
        Beep;
      end;
    end;
  finally
    Script.Free;
  end;
end;

procedure TForm1.SaveScriptButtonClick(Sender: TObject);
begin
  if ScriptFileEdit.FileName<>'' then
    ScriptMemo.Lines.SaveToFile(ScriptFileEdit.FileName);
end;

function TForm1.ConnectionTest(ChosenConfig: TDBConnectionConfig; var ErrorMessages: string): boolean;
// Callback function that uses the info in dbconfiggui to test a connection
// and return the result of the test to dbconfiggui
var
  // Generic database connector...
  Conn: TSQLConnector;
begin
  result:=false;
  Conn:=TSQLConnector.Create(nil);
  Screen.Cursor:=crHourglass;
  try
    // ...actual connector type is determined by this property.
    // Make sure the ChosenConfig.DBType string matches
    // the connectortype (e.g. see the string in the
    // T*ConnectionDef.TypeName for that connector .
    Conn.ConnectorType:=ChosenConfig.DBType;
    Conn.HostName:=ChosenConfig.DBHost;
    Conn.DatabaseName:=ChosenConfig.DBPath;
    Conn.UserName:=ChosenConfig.DBUser;
    Conn.Password:=ChosenConfig.DBPassword;
    try
      Conn.Open;
      result:=Conn.Connected;
    except
      // Result is already false
      on D: EDatabaseError do
      begin
        ErrorMessages:=D.Message;
      end;
      on E: Exception do
      begin
        ErrorMessages:=E.Message;
      end;
    end;
    Conn.Close;
  finally
    Screen.Cursor:=crDefault;
    Conn.Free;
  end;
end;

procedure TForm1.LoadScript;
begin
  ScriptMemo.Lines.Clear;
  ErrorMemo.Lines.Clear;
  ScriptMemo.Lines.LoadFromFile(ScriptFileEdit.FileName);
end;

end.

