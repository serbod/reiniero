unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, ExtCtrls, Menus, dbconfiggui, dbconfig,
  {General db unit}sqldb,
  {For EDataBaseError}db,
  {Now we add all databases we want to support, otherwise their drivers won't be loaded}
  {$IFNDEF Solaris}IBConnection,{$ENDIF}pqconnection,sqlite3conn,
  mssqlconn,mysql50conn,mysql51conn,mysql55conn,odbcconn,
  {$IF (FPC_FULLVERSION >= 20604) OR NOT(DEFINED(MSWIN64))}
  // Oracle connector not available in FPC 2.6.2- on Win64
  oracleconnection,
  {$ENDIF}
  sqlscript {the unit that contains tsqlscript};

type

  { TForm1 }

  TForm1 = class(TForm)
    ErrorMemo: TMemo;
    MainMenu1: TMainMenu;
    mnuSaveAs: TMenuItem;
    mnuFile: TMenuItem;
    mnuFileOpen: TMenuItem;
    mnuFileQuit: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    UseTermCtl: TCheckBox;
    UseCommitCtl: TCheckBox;
    UseCommentCtl: TCheckBox;
    CmdCopyDDL: TButton;
    CmdCopyDML: TButton;
    CmdRunScript: TButton;
    Panel1: TPanel;
    ScriptMemo: TMemo;
    procedure CmdCopyDDLClick(Sender: TObject);
    procedure CmdCopyDMLClick(Sender: TObject);
    procedure CmdRunScriptClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuFileOpenClick(Sender: TObject);
    procedure mnuFileQuitClick(Sender: TObject);
    procedure mnuSaveAsClick(Sender: TObject);
  private
    { private declarations }
    FConn: TSQLConnector;
    FErrors: integer; //Number of errors hit during script execution
    FQuery: TSQLQuery;
    FTran: TSQLTransaction;
    // Callback function for db connection configuration. This function
    // tests if the specified credentials/details are correct by trying
    // to connect.
    function ConnectionTest(ChosenConfig: TDBConnectionConfig; var ErrorMessages: string): boolean;
    // Set TSQLScript's OnException handler to this procedure
    // so any exceptions hit running the scripts can be handled by this
    // procedure which shows a warning message in a memo:
    procedure SQLScriptException(Sender: TObject; Statement: TStrings;
      TheException: Exception; var Continue: boolean);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
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
    LoginForm.ConnectorType.Clear; //remove any default connectors
    // Now add the dbs that you support - use the name of the *ConnectionDef.TypeName property
    {$IFNDEF Solaris}
    // Not available on Solaris
    LoginForm.ConnectorType.AddItem('Firebird', nil);
    {$ENDIF}
    LoginForm.ConnectorType.AddItem('MSSQLServer', nil);
    LoginForm.ConnectorType.AddItem('MySQL5.0', nil);
    LoginForm.ConnectorType.AddItem('MySQL5.1', nil);
    LoginForm.ConnectorType.AddItem('MySQL5.5', nil);
    LoginForm.ConnectorType.AddItem('ODBC', nil);
    {$IF (FPC_FULLVERSION >= 20604) OR NOT(DEFINED(MSWIN64))}
    LoginForm.ConnectorType.AddItem('Oracle', nil);
    {$ENDIF}
    LoginForm.ConnectorType.AddItem('PostGreSQL', nil);
    LoginForm.ConnectorType.AddItem('SQLite3', nil);
    LoginForm.ConnectorType.AddItem('Sybase', nil);
    case LoginForm.ShowModal of
    mrOK:
      begin
        //user wants to connect, so copy over db info
        FConn.ConnectorType:=LoginForm.Config.DBType;
        // Required for DDL:
        if FConn.ConnectorType='Sybase' then
          FConn.Params.Add('AutoCommit=true');
        FConn.HostName:=LoginForm.Config.DBHost;
        FConn.DatabaseName:=LoginForm.Config.DBPath;
        FConn.UserName:=LoginForm.Config.DBUser;
        FConn.Password:=LoginForm.Config.DBPassword;
        FConn.Transaction:=FTran;
      end;
    mrCancel:
      begin
        ShowMessage('You canceled the database login. Application will terminate.');
        Application.Terminate; //close won't work in FormCreate event.
      end;
    end;
  finally
    LoginForm.Free;
  end;

  // Get a script before the user's eyes:
  CmdCopyDDLClick(nil);
end;

procedure TForm1.CmdCopyDDLClick(Sender: TObject);
// Script that sets up tables as used in SQLdb_Tutorial1..3
// Notice we include 2 SQL statements, each terminated with ;
const ScriptText=
  'CREATE TABLE CUSTOMER '+LineEnding+
  '( '+LineEnding+
  '  CUST_NO INTEGER NOT NULL, '+LineEnding+
  '  CUSTOMER VARCHAR(25) NOT NULL, '+LineEnding+
  '  CITY VARCHAR(25), '+LineEnding+
  '  COUNTRY VARCHAR(15), '+LineEnding+
  '  CONSTRAINT CT_CUSTOMER_PK PRIMARY KEY (CUST_NO) '+LineEnding+
  '); '+LineEnding+
  'CREATE TABLE EMPLOYEE '+LineEnding+
  '( '+LineEnding+
  '  EMP_NO INTEGER NOT NULL, '+LineEnding+
  '  FIRST_NAME VARCHAR(15) NOT NULL, '+LineEnding+
  '  LAST_NAME VARCHAR(20) NOT NULL, '+LineEnding+
  '  PHONE_EXT VARCHAR(4), '+LineEnding+
  '  JOB_CODE VARCHAR(5) NOT NULL, '+LineEnding+
  '  JOB_GRADE INTEGER NOT NULL, '+LineEnding+
  '  JOB_COUNTRY VARCHAR(15) NOT NULL, '+LineEnding+
  '  SALARY NUMERIC(10,2) NOT NULL, '+LineEnding+
  '  CONSTRAINT CT_EMPLOYEE_PK PRIMARY KEY (EMP_NO) '+LineEnding+
  ');';
begin
  Scriptmemo.Lines.Text:=ScriptText;
end;

procedure TForm1.CmdCopyDMLClick(Sender: TObject);
// Script that fills the table with sample data as used in SQLdb_Tutorial1..3
// The double quotes inside the statements will be parsed by the Pascal compiler and
// end up as single quotes in the actual ScriptText string, like SQL expects it.
const ScriptText=
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (1, ''Michael Design'', ''San Diego'', ''USA''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (2, ''VC Technologies'', ''Dallas'', ''USA''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (3, ''KlÃ¤mpfl, Van Canneyt'', ''Boston'', ''USA''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (4, ''Felipe Bank'', ''Manchester'', ''England''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (5, ''Joost Systems, LTD.'', ''Central Hong Kong'', ''Hong Kong''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (6, ''Van der Voort Int.'', ''Ottawa'', ''Canada''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (7, ''Mrs. Mauvais'', ''Pebble Beach'', ''USA''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (8, ''Asinine Vacation Rentals'', ''Lihue'', ''USA''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (9, ''Fax'', ''Turtle Island'', ''Fiji''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (10, ''FPC Corporation'', ''Tokyo'', ''Japan''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (11, ''Dynamic Intelligence Corp'', ''Zurich'', ''Switzerland''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (12, ''3D-Pad Corp.'', ''Paris'', ''France''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (13, ''Swen Export, Ltd.'', ''Milan'', ''Italy''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (14, ''Graeme Consulting'', ''Brussels'', ''Belgium''); '+LineEnding+
  'INSERT INTO CUSTOMER (CUST_NO, CUSTOMER, CITY, COUNTRY) VALUES (15, ''Klenin Inc.'', ''Den Haag'', ''Netherlands''); '+LineEnding+
  'INSERT INTO employee(emp_no, first_name, last_name, phone_ext, job_code, job_grade,  '+LineEnding+
  '  job_country, salary) '+LineEnding+
  '  VALUES (1,''William'',''Shatner'',''1702'',''CEO'',1,''USA'',48000); '+LineEnding+
  'INSERT INTO employee(emp_no, first_name, last_name, phone_ext, job_code, job_grade,  '+LineEnding+
  '  job_country, salary) '+LineEnding+
  '  VALUES (2,''Ivan'',''Rzeszow'',''9802'',''Eng'',2,''Russia'',38000); '+LineEnding+
  'INSERT INTO employee(emp_no, first_name, last_name, phone_ext, job_code, job_grade,  '+LineEnding+
  '  job_country, salary) '+LineEnding+
  '  VALUES (3,''Erin'',''Powell'',''1703'',''Admin'',2,''USA'',45368); ';
begin
  Scriptmemo.Lines.Text:=ScriptText;
end;

procedure TForm1.CmdRunScriptClick(Sender: TObject);
// The heart of the program: runs the script in the memo
var
  // Note: of course you can also simply drop a TSQLScript component from the
  // Components palette on your form and set properties using Object Inspector...
  OurScript: TSQLScript;
begin
  OurScript:=TSQLScript.Create(nil);
  try
    OurScript.Database:=FConn; //Indicate what db & ...
    OurScript.Transaction:=FTran; // ... transaction we actually want to run the script against
    OurScript.Script.Assign(ScriptMemo.Lines); //Copy over the script itself
    //Now set some options:
    OurScript.UseCommit:=UseCommitCtl.Checked; //try process any COMMITs inside the script, instead of batching everything together. See readme.txt though
    OurScript.UseSetTerm:=UseTermCtl.Checked; //SET TERM is Firebird specific, used when creating stored procedures etc. It's not needed here
    OurScript.CommentsInSQL:=UseCommentCtl.Checked; //Send commits to db server as well; could be useful to troubleshoot by monitoring all SQL statements at the server
    OurScript.OnException:=@SQLScriptException;
    FErrors:=0;
    ErrorMemo.Clear;
    try
      if not(FTran.Active) then
        FTran.StartTransaction; //better safe than sorry
      OurScript.Execute;
      if FErrors=0 then
      begin
        ShowMessage('Script was succesfully run.');
        FTran.Commit; //Make sure entire script is committed to the db
        // of course, we cannot influence COMMIT statements in the script
        // if UseCommit is true...
      end
      else
      begin
        ShowMessage('Script ran with '+inttostr(FErrors)+' errors.');
        FTran.Rollback;
      end;
    except
      // This shouldn't happen because we catch TSQLScript errors in our
      // separate procedure
      on E: EDataBaseError do
      begin
        ShowMessage('Error running script: '+E.Message);
        FTran.Rollback;
      end;
    end;
  finally
    OurScript.Free;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FQuery.Free;
  FTran.Free;
  FConn.Free;
end;

procedure TForm1.mnuFileOpenClick(Sender: TObject);
begin
  // We've already set up a filter for *.sql in the Object Inspector for OpenDialog1...
  if OpenDialog1.Execute then
  begin
    // Load file contents into our memo, overwriting existing text.
    ScriptMemo.Clear;
    ScriptMemo.Lines.LoadFromFile(OpenDialog1.FileName);
  end;
end;

procedure TForm1.mnuFileQuitClick(Sender: TObject);
begin
  Close; //ask the form to close. As this is the only form, the application should close.
end;

procedure TForm1.mnuSaveAsClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    ScriptMemo.Lines.SaveToFile(SaveDialog1.FileName);
  end;
end;


procedure TForm1.SQLScriptException(Sender: TObject; Statement: TStrings;
  TheException: Exception; var Continue: boolean);
begin
  // Handle errors coming from TSQLScript: show them in the errors memo.
  FErrors:=FErrors+1;
  ErrorMemo.Append('Error running statement');
  ErrorMemo.Lines.AddStrings(Statement);
  ErrorMemo.Append('Error message:');
  ErrorMemo.Append(TheException.Message);
  ErrorMemo.Append('******************************************');
  ErrorMemo.Append('');
  Continue:=false; //If a serious error occurred, let's not make things worse
end;


function TForm1.ConnectionTest(ChosenConfig: TDBConnectionConfig; var ErrorMessages: string): boolean;
// Callback function that uses the info in dbconfiggui to test a connection
// and return the result of the test to dbconfiggui
var
  // Generic database connector...
  Conn: TSQLConnector;
begin
  ErrorMessages:='';
  result:=false; //fail by default
  Conn:=TSQLConnector.Create(nil);
  Screen.Cursor:=crHourglass;
  // Now try to connect using info the user supplied:
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

end.

