unit dbconfiggui;

{ GUI part needed to let the user know/edit settings. Use together with dbconfig

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

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  dbconfig, dbcreate;

type
  // Implement this in your callback connection test function.
  // Get the ChosenConfig, try to connect and pass on the result (true/false).
  // Any error/warning messages can be added as lines to ErrorMessages.
  TConnectionTestFunction = function(ChosenConfig: TDBConnectionConfig; var ErrorMessages: string): boolean of object;
  { TDBConfigForm }

  TDBConfigForm = class(TForm)
  private
    FShowCreateDBButton: boolean;
  published
    CreateButton: TButton;
    OKButton: TButton;
    CancelButton: TButton;
    TestButton: TButton;
    ConnectorType: TComboBox;
    Host: TEdit;
    Database: TEdit;
    lblDatabaseType: TLabel;
    lblHost: TLabel;
    lblDatabase: TLabel;
    lblUser: TLabel;
    lblPassword: TLabel;
    Password: TEdit;
    User: TEdit;
    procedure ConnectorTypeEditingDone(Sender: TObject);
    procedure CreateButtonClick(Sender: TObject);
    procedure DatabaseEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HostEditingDone(Sender: TObject);
    procedure PasswordEditingDone(Sender: TObject);
    procedure TestButtonClick(Sender: TObject);
    procedure UserEditingDone(Sender: TObject);
    // Controls whether or not to show the Create database button on the form.
    // Default: enabled
    property ShowCreateDBButton: boolean read FShowCreateDBButton write FShowCreateDBButton;
  private
    // Internally keeps track of the config chosen so far:
    FConnectionConfig: TDBConnectionConfig;
    // Internally keeps track of which actual callback function to call:
    FConnectionTestFunction: TConnectionTestFunction;
    FSetupComplete: boolean;
    { private declarations }
  public
    property Config: TDBConnectionConfig read FConnectionConfig;
    property ConnectionTestCallback: TConnectionTestFunction write FConnectionTestFunction;
    { public declarations }
  end;

var
  DBConfigForm: TDBConfigForm;

implementation

{$R *.lfm}

{ TDBConfigForm }

procedure TDBConfigForm.TestButtonClick(Sender: TObject);
var
  Errors: string;
begin
  // Call callback with settings, let it figure out if connection succeeded and
  // get test result back
  if assigned(FConnectionTestFunction) and assigned(FConnectionConfig) then
  begin
    if FConnectionTestFunction(FConnectionConfig,Errors) then
    begin
      if Errors='' then
        showmessage('Connection test succeeded.')
      else
        showmessage('Connection test succeeded.'+LineEnding+
         'Warnings:'+LineEnding+
         Errors);
    end
    else
    begin
      showmessage('Connection test failed.'+LineEnding+
       'Errors:'+LineEnding+
       Errors);
    end;
  end
  else
    showmessage('Error: connection test code has not been implemented.');
end;

procedure TDBConfigForm.UserEditingDone(Sender: TObject);
begin
  FConnectionConfig.DBUser:=User.Text;
end;

procedure TDBConfigForm.FormCreate(Sender: TObject);
begin
  FConnectionConfig:=TDBConnectionConfig.Create;
  FSetupComplete:=false;
  FShowCreateDBButton:=true; //by default, let users create new dbs
end;

procedure TDBConfigForm.ConnectorTypeEditingDone(Sender: TObject);
begin
  FConnectionConfig.DBType:=ConnectorType.Text;
end;

procedure TDBConfigForm.CreateButtonClick(Sender: TObject);
var
  DBCreate: TDBCreate;
begin
  // Create database if possible
  DBCreate:=TDBCreate.Create;
  try
    DBCreate.Config:=FConnectionConfig;
    if DBCreate.CreateDB then
      ShowMessage('Succesfully created database '+FConnectionConfig.DBPath)
    else
      ShowMessage('Error trying to create database '+FConnectionConfig.DBPath+LineEnding+
        DBCreate.Errors.Text);
  finally
    DBCreate.Free;
  end;
end;

procedure TDBConfigForm.DatabaseEditingDone(Sender: TObject);
begin
  FConnectionConfig.DBPath:=Database.Text;
end;

procedure TDBConfigForm.FormDestroy(Sender: TObject);
begin
  FConnectionConfig.Free;
end;

procedure TDBConfigForm.FormShow(Sender: TObject);
begin
  // Copy over values specified by config/calling code.
  // Only do this once in form's lifetime
  if not FSetupComplete then
  begin
    FSetupComplete:=true;
    CreateButton.Visible:=FShowCreateDBButton;
    ConnectorType.Text:=FConnectionConfig.DBType;
    Host.Text:=FConnectionConfig.DBHost;
    Database.Text:=FConnectionConfig.DBPath;
    User.Text:=FConnectionConfig.DBUser;
    Password.Text:=FConnectionConfig.DBPassword;
  end;
end;

procedure TDBConfigForm.HostEditingDone(Sender: TObject);
const
  DefaultSybasePort='5000'; //default port; different from default mssqlconn port 1433 (for SQL Server).
begin
  if (ConnectorType.Text='Sybase') and (pos(':',Host.Text)=0) then
  begin
    ShowMessage('Sybase database without port specification. Adding default port '+DefaultSybasePort);
    Host.Text:=Host.Text+':'+DefaultSybasePort;
  end;
  FConnectionConfig.DBHost:=Host.Text;
end;

procedure TDBConfigForm.PasswordEditingDone(Sender: TObject);
begin
  FConnectionConfig.DBPassword:=Password.Text;
end;

end.

