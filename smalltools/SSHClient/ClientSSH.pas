unit ClientSSH;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  tlntsend, ssl_openssl, ssl_openssl_lib, ssl_cryptlib;

type

  { TSSHClient }

  TSSHClient = class
  private
    FTelnetSend: TTelnetSend;

    function GetServer: String;
    procedure SetServer(AValue: String);
    function GetPort: Integer;
    procedure SetPort(AValue: Integer);
    function GetUserLogin: String;
    procedure SetUserLogin(AValue: String);
    function GetUserPassword: String;
    procedure SetUserPassword(AValue: String);
    function GetLastErrorCode: Integer;
    function GetLastErrorText: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SendCommand(ACommand: string);
    procedure LogOut;
    function ReceiveData: string;
    function LogIn: Boolean;

    property Server: String read GetServer write SetServer;
    property Port: Integer read GetPort write SetPort;
    property UserLogin: String read GetUserLogin write SetUserLogin;
    property UserPassword: String read GetUserPassword write SetUserPassword;
    property LastErrorCode: Integer read GetLastErrorCode;
    property LastErrorText: String read GetLastErrorText;
  end;

implementation

{ TSSHClient }

constructor TSSHClient.Create;
begin
  FTelnetSend := TTelnetSend.Create;

end;

destructor TSSHClient.Destroy;
begin
  FTelnetSend.Free;
  inherited;
end;

function TSSHClient.LogIn: Boolean;
begin
  Result := FTelnetSend.SSHLogin;
end;

procedure TSSHClient.LogOut;
begin
  FTelnetSend.Logout;
end;

function TSSHClient.ReceiveData: string;
var
  lPos: Integer;
begin
  Result := '';
  lPos := 1;
  while FTelnetSend.Sock.CanRead(1000) or (FTelnetSend.Sock.WaitingData>0) do
  begin
    FTelnetSend.Sock.RecvPacket(1000);
    Result := Result + Copy(FTelnetSend.SessionLog, lPos, Length(FTelnetSend.SessionLog));
    lPos := Length(FTelnetSend.SessionLog)+1;
  end;
end;

procedure TSSHClient.SendCommand(ACommand: string);
begin
  FTelnetSend.Send(ACommand + #13);
end;

function TSSHClient.GetServer: String;
begin
  Result := FTelnetSend.TargetHost
end;

procedure TSSHClient.SetServer(AValue: String);
begin
  FTelnetSend.TargetHost := AValue
end;

function TSSHClient.GetPort: Integer;
begin
  Result := StrToInt(FTelnetSend.TargetPort)
end;

procedure TSSHClient.SetPort(AValue: Integer);
begin
  FTelnetSend.TargetPort := IntToStr(AValue)
end;

function TSSHClient.GetUserLogin: String;
begin
  Result := FTelnetSend.UserName
end;

procedure TSSHClient.SetUserLogin(AValue: String);
begin
  FTelnetSend.UserName := AValue
end;

function TSSHClient.GetUserPassword: String;
begin
  Result := FTelnetSend.Password
end;

procedure TSSHClient.SetUserPassword(AValue: String);
begin
  FTelnetSend.Password := AValue;
end;

function TSSHClient.GetLastErrorCode: Integer;
begin
  Result := FTelnetSend.Sock.LastError
end;

function TSSHClient.GetLastErrorText: String;
begin
  Result := FTelnetSend.Sock.LastErrorDesc
end;

end.

