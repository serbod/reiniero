unit sqldbtutorial2unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, IBConnection, DB, FileUtil, Forms,
  Controls, Graphics, Dialogs, DBGrids, StdCtrls, DbCtrls, LCLType;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    DatabaseName: TEdit;
    Datasource1: TDatasource;
    DBGrid1: TDBGrid;
    Dbnavigator1: Tdbnavigator;
    Edit1: TEdit;
    Label2: Tlabel;
    Label3: Tlabel;
    Label4: Tlabel;
    Label5: Tlabel;
    Password: TEdit;
    DBConnection: TIBConnection;
    UserName: TEdit;
    ServerName: TEdit;
    Label1: TLabel;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure DBGrid1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SaveChanges;
    procedure Button1click(Sender: TObject);
    procedure Formclose(Sender: TObject; var Closeaction: Tcloseaction);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure Tform1.Savechanges;
// Saves edits done by user, if any.
begin
  try
    if SQLTransaction1.Active=true then
    // Only if we are within a started transaction
    // otherwise you get "Operation cannot be performed on an inactive dataset"
    begin
      SQLQuery1.ApplyUpdates; //Pass user-generated changes back to database...
      SQLTransaction1.Commit; //... and commit them using the transaction.
      //SQLTransaction1.Active now is false
    end;
  except
  on E: EDatabaseError do
    begin
      MessageDlg('Error', 'A database error has occurred. Technical error message: ' +
        E.Message, mtError, [mbOK], 0);
      Edit1.Text := '';
    end;
  end;
end;

procedure TForm1.DBGrid1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  // Check for del key being hit and delete the current record in response
  // as long as we're not editing data
  if (key=VK_DELETE) and (not(DBGrid1.EditorMode)) then
  begin
    //... delete current record and apply updates to db:
    SQLQuery1.Delete;
    SQLQuery1.ApplyUpdates;
  end;
end;


procedure Tform1.Button1click(Sender: TObject);
begin
  SaveChanges; //Saves changes and commits transaction
  try
    SQLQuery1.Close;
    //Connection settings for Firebird/Interbase database
    //only needed when we have not yet connected:
    if DBConnection.Connected = false then
    begin
      DBConnection.HostName := ServerName.Text;
      DBConnection.DatabaseName := DatabaseName.Text;
      DBConnection.Username := UserName.Text;
      DBConnection.Password := Password.Text;
      // Now we've set up our connection, visually show that
      // changes are not possibly any more
      ServerName.ReadOnly:=true;
      DatabaseName.ReadOnly:=true;
      UserName.ReadOnly:=true;
      Password.ReadOnly:=true;
    end;
    // Show all records, or filter if user specified a filter criterium
    if Edit1.Text='' then
      SQLQuery1.SQL.Text := 'select * from CUSTOMER'
    else
    begin
      SQLQuery1.SQL.Text := 'select * from CUSTOMER where COUNTRY = :COUNTRY';
      SQLQuery1.Params.ParamByName('COUNTRY').AsString := Edit1.Text;
    end;
    DBConnection.Connected := True;
    SQLTransaction1.Active := True; //Starts a new transaction
    SQLQuery1.Open;
    {
    Make sure we don't get problems with inserting blank (=NULL) CUST_NO values, i.e. error message:
    "Field CUST_NO is required, but not supplied"
    We need to tell Lazarus that, while CUST_NO is a primary key, it is not required
    when inserting new records.
    }
    SQLQuery1.FieldByName('CUST_NO').Required:=false;
    {
    Hide the primary key column which is the first column in our queries.
    We can only do this once the DBGrid has created the columns
    }
    DBGrid1.Columns[0].Visible:=false;
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Error', 'A database error has occurred. Technical error message: ' +
        E.Message, mtError, [mbOK], 0);
      Edit1.Text := '';
    end;
  end;
end;

procedure Tform1.Formclose(Sender: TObject; var Closeaction: Tcloseaction);
begin
  SaveChanges; //Saves changes and commits transaction
  SQLQuery1.Close;
  SQLTransaction1.Active := False;
  DBConnection.Connected := False;
end;

end.

