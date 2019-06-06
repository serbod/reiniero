unit MainForm;
//todo: add port forwarding if possible to wrappers see
//http://www.libssh2.org/examples/tcpip-forward.html
{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  ClientSSH2, EditBtn;

type

  { TMainFrm }

  TMainFrm = class(TForm)
    EServerPort: TEdit;
    Label6: TLabel;
    Panel1: TPanel;
    Label1: TLabel;
    EServerIP: TEdit;
    Label2: TLabel;
    ELogin: TEdit;
    EPassword: TEdit;
    BtnConnect: TButton;
    BtnDisconnect: TButton;
    BtnClose: TButton;
    Label3: TLabel;
    Label4: TLabel;
    ECommand: TMemo;
    Memo1: TMemo;
    BtnExecute: TButton;
    Label5: TLabel;
    EPrivateKey: TFileNameEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnConnectClick(Sender: TObject);
    procedure BtnDisconnectClick(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure BtnExecuteClick(Sender: TObject);
  private
    { private declarations }
    FClientSSH: TTelnetSSHClient;

    function DoConnect: Boolean;
    function DoDisconnect: Boolean;
  public
    { public declarations }
  end;

var
  MainFrm: TMainFrm;

implementation

{$R *.lfm}

{ TMainFrm }

procedure TMainFrm.FormCreate(Sender: TObject);
begin
  FClientSSH := TTelnetSSHClient.Create;
end;

procedure TMainFrm.FormDestroy(Sender: TObject);
begin
  FClientSSH.Free
end;

procedure TMainFrm.FormShow(Sender: TObject);
begin
  DoDisconnect
end;

procedure TMainFrm.BtnConnectClick(Sender: TObject);
begin
  DoConnect
end;

procedure TMainFrm.BtnDisconnectClick(Sender: TObject);
begin
  DoDisconnect
end;

function TMainFrm.DoConnect: Boolean;
var
  wLastErrorText: String;
begin
  result:=false;
  FClientSSH.HostName := EServerIP.Text;
  if trim(EServerPort.Text)='' then
    FClientSSH.Port := '22'
  else
    FClientSSH.Port:=EServerPort.Text;
  FClientSSH.Servertype := stUnix;
  FClientSSH.ProtocolType := ptSSH;
  FClientSSH.UserName := ELogin.Text;
  FClientSSH.Password := EPassword.Text;
  FClientSSH.PrivateKeyFile := EPrivateKey.FileName;
  wLastErrorText := FClientSSH.Connect;
  if not FClientSSH.Connected then begin
    MessageDlg('Connect',Format('Connect error "%s"', [wLastErrorText]), mtInformation, [mbOK], 0, mbOK);
    Exit
  end;
  Memo1.Clear;
  Memo1.Append('*** Connected');
  result:=true;
end;

function TMainFrm.DoDisconnect: Boolean;
begin
  result:=false;
  FClientSSH.LogOut;
  Memo1.Clear;
  Memo1.Append('*** Disconnected');
  result:=true;
end;

procedure TMainFrm.BtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainFrm.BtnExecuteClick(Sender: TObject);
begin
  Memo1.Append('*** executing: '+ECommand.Text);
  Memo1.Append(FClientSSH.CommandResult(ECommand.Text));
end;

end.

