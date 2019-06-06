unit ClientSSH2;

{ Wrapper around Synapse libraries and SSL library (libssh2+libssl
is used right now)
Download compiled Windows dll from e.g.
http://alxdm.dyndns-at-work.com:808/files/windll_libssh2.zip
Download FreePascal interface files:
http://www.lazarus.freepascal.org/index.php/topic,15935.msg86465.html#msg86465

This unit allows the user to send Telnet or SSH commands and get the output
Thanks to Leonardo Rame
http://leonardorame.blogspot.com/2010/01/synapse-based-ssh-client.html
and Ludo Brands.

Written by Reinier Olislagers 2011.
Modified for libssh2 by Alexey Suhinin 2012.
Modified: no exceptions but error messages in connect 2013
Modified: slight renaming of enums 2013

License of code:
* MIT
* LGPLv2 or later (with FreePascal static linking exception)
* GPLv2 or later
according to your choice.
Free use allowed but please don't sue or blame me.

Uses other libraries/components; different licenses may apply that also can influence the combined/compiled work.
}

{$mode objfpc}{$H+}
{$DEFINE HAS_SSH_SUPPORT} //comment out if only telnet support required
{$DEFINE LIBSSH2} //Specify encryption library to use

interface

uses
  Classes, SysUtils,
  tlntsend,
  {$IFDEF HAS_SSH_SUPPORT}
  {ssl - or actually ssh - libs required by tlntsend}
    {$IFDEF LIBSSH2}
      ssl_libssh2
    {$ELSE}
      ssl_cryptlib
  {$ENDIF}
  {$ENDIF HAS_SSH_SUPPORT}  ;

type
  TProtocolType = (ptTelnet, ptSSH); //Different means of connecting
  TServerType = (stUnix, stWindows); //line endings, mostly

  { TTelnetSSHClient }

  TTelnetSSHClient = class(TTelnetSend)
  protected
    FConnected: boolean;
    FOutputPosition: integer; //Keeps track of position in output stream
    FProtocolType: TProtocolType;
    FServerLineEnding: string; //depends on FServerType
    FServerType: TServerType;
    FWelcomeMessage, FTelnetLoginPrompt, FTelnetPasswordPrompt: string;
    procedure SetPrivateKeyFile(Value: string);
    function GetPrivateKeyFile: string;
    { Based on protocol and servertype, set expected serverside line ending}
    procedure DetermineLineEnding;
    { Sets port if no explicit port set. Uses protocol type: SSH or telnet}
    procedure DeterminePort;
    function GetSessionLog: string;
    procedure ProtocolTypeChange(Value: TProtocolType);
    function ReceiveData: string; //Can be used to get welcome message etc.
    procedure SendData(Data: string);
    procedure ServerTypeChange(Value: TServerType);
  public
    {All output generated during the entire session up to now}
    property AllOutput: string read GetSessionLog;
    {True if connected to server}
    property Connected: boolean read FConnected;
    {Name or IP address of host to connect to}
    property HostName: string read FTargetHost write FTargetHost;
    {Port on host used for connection. If left as 0, it will be determined by protocol type (22 for SSH, 23 for Telnet}
    property Port: String read FTargetPort write FTargetPort;
    {Location of private key file.}
    property PrivateKeyFile: string read GetPrivateKeyFile write SetPrivateKeyFile;
    {Telnet login prompt, default 'login:'}
    property TelnetLoginPrompt: string read FTelnetLoginPrompt write FTelnetLoginPrompt;
    {Telnet password prompt, default 'password:'}
    property TelnetPasswordPrompt: string read FTelnetPasswordPrompt write FTelnetPasswordPrompt;
    {Username used when connecting}
    property UserName: string read FUserName write FUserName;
    {Password used when connecting. Used as passphrase if PrivateKey is used}
    property Password: string read FPassword write FPassword;
    {Should we talk Telnet or SSH to the server? Defaults to SSH.}
    property ProtocolType: TProtocolType read FProtocolType write ProtocolTypeChange;
    {Windows or Unix/Linux server? Has effect on line endings. Defaults to Unix. NOTE: untested}
    property Servertype: TServerType read FServerType write ServerTypeChange;
    {Initial message displayed on logon}
    property WelcomeMessage: string read FWelcomeMessage;
    {Connect/logon to server. Requires that all authentication, protocol and hostname/port options are correct
    Returns descriptive result. You can then use the Connected property.}
    function Connect: string;
    {If connected, logoff from server}
    procedure Disconnect;
    {Send command to server; do not wait for response}
    procedure Command(CommandText: string);
    {Send command to server and receive resulting output}
    function CommandResult(CommandText: string): string;
    constructor Create;
    destructor Destroy; override;
  end;

implementation


{ TelnetSSHClient }
procedure TTelnetSSHClient.SetPrivateKeyFile(Value: string);
begin
  Sock.SSL.PrivateKeyFile := value;
end;

function TTelnetSSHClient.GetPrivateKeyFile: string;
begin
  Result := Sock.SSL.PrivateKeyFile;
end;

procedure TTelnetSSHClient.DetermineLineEnding;
begin
  case FProtocolType of
    ptSSH:
    begin
      if FServerType = stUnix then
        FServerLineEnding := #10 //Unix
      else
        FServerLineEnding := #13 + #10; //windows
    end;
    ptTelnet:
    begin
      if FServerType = stUnix then
        FServerLineEnding := #10 //Unix
      else
        FServerLineEnding := #13 + #10; //windows
    end;
    else
      raise Exception.Create('Unknown protocol type');
  end;
end;

procedure TTelnetSSHClient.DeterminePort;
begin
  if FTargetPort = '' then
    //Set default port for protocol
  begin
    case FProtocolType of
      ptTelnet: FTargetPort := '23';
      ptSSH: FTargetPort := '22';
      else
        raise Exception.Create('Unknown protocol type.');
    end;
  end;
end;

procedure TTelnetSSHClient.ServerTypeChange(Value: TServerType);
begin
  FServerType := Value;
  DetermineLineEnding;
end;

function TTelnetSSHClient.Connect: string;
var
  Received: string;
begin
  result:='Unknown error while connecting';
  FOutputPosition := 1; //First character in output stream
  FWelcomeMessage := '';
  //Just to make sure:
  DetermineLineEnding;
  DeterminePort;
  if FTargetPort='0' then
  begin
    result:='Port may not be 0.';
    exit; //jump out of function
  end;
  case FProtocolType of
    ptTelnet:
    begin
      try
        if Login then
        begin
          FConnected := True;
          result:='Connected to telnet server.';
        end
        else
          if Sock.LastError<>0 then raise Exception.Create(Sock.LastErrorDesc);
      except
        on E: Exception do
        begin
          FConnected:=false;
          result:='Error connecting to telnet server '+FTargetHost+':'+
          FTargetPort+' as user ' + FUserName +
          '. Technical details: '+E.Message;
        end;
      end;
    end;
    ptSSH:
    begin
      {$IFNDEF HAS_SSH_SUPPORT}
      raise Exception.Create(
        'SSH support has not been compiled into the telnetsshclient library.');
      {$ENDIF HAS_SSH_SUPPORT}
      try
        // Repurpose password property as passphrase if keyfile is used
        if (PrivateKeyFile <> '') and (FPassword <> '') then
          Sock.SSL.KeyPassword:=FPassword;
        if SSHLogin then
        begin
          FConnected := True;
          result:='Connected to SSH server.';
        end
        else
        begin
          if Sock.LastError<>0 then
          begin
            FConnected:=false;
            result:='Error connecting to SSH server '+FTargetHost+':'+
            FTargetPort+' as user ' + FUserName +
            '. Technical details: socket error: '+Sock.LastErrorDesc;
          end;
          if Sock.SSL.LastError<0 then
          begin
            FConnected:=false;
            result:='Error connecting to SSH server '+FTargetHost+':'+
            FTargetPort+' as user ' + FUserName +
            '. Technical details: socket encryption error: '+Sock.SSL.LastErrorDesc;
          end;
        end;
      except
        on E: Exception do
        begin
          FConnected:=false;
          result:='Error connecting to SSH server '+FTargetHost+':'+
          FTargetPort+' as user ' + FUserName +
          '. Technical details: '+E.Message;
        end;
      end;
    end;
    else
    begin
      FConnected:=false;
      result:='Error connecting to SSH server '+FTargetHost+':'+
      FTargetPort+' as user ' + FUserName +
      '. Technical details: unknown protocol type.';
    end;
  end;
  if FConnected = True then
  begin
    Received := ReceiveData;
    FWelcomeMessage:=Received; //Copy over for archival
    if FProtocolType=ptTelnet then
    begin
      //Unfortunately, we'll have to extract login ourselves
      //Hope it applies to all server types.
      if (AnsiPos(AnsiLowerCase(FTelnetLoginPrompt),AnsiLowerCase(Received))>0) then
      begin
        SendData(UserName);
        Received:=ReceiveData;
      end;
      if (AnsiPos(AnsiLowerCase(FTelnetPasswordPrompt),AnsiLowerCase(Received))>0) then
      begin
        SendData(Password);
      end;
      //Receive additional welcome message/message of the day
      FWelcomeMessage:=FWelcomeMessage+LineEnding+ReceiveData;
    end;
  end;
end;

procedure TTelnetSSHClient.Disconnect;
begin
  Logout;
  FConnected := False;
end;

procedure TTelnetSSHClient.Command(CommandText: string);
begin
  if FConnected then
  begin
    SendData(CommandText);
  end
  else
  begin
    raise Exception.Create('Can only run command when connected');
  end;
end;

function TTelnetSSHClient.ReceiveData: string;
begin
  Result := '';
  while Sock.CanRead(1000) or (Sock.WaitingData > 0) do
  begin
    Sock.RecvPacket(1000);
    Result := Result + Copy(SessionLog, FOutputPosition,
      Length(SessionLog));
    FOutputPosition := Length(SessionLog) + 1;
  end;
end;

procedure TTelnetSSHClient.SendData(Data: string);
begin
  Data := Data + FServerLineEnding; //Could be linux, could be Windows
  Send(Data);
end;

function TTelnetSSHClient.GetSessionLog: string;
begin
  // Gets complete output up to now
  Result := SessionLog;
end;

procedure TTelnetSSHClient.ProtocolTypeChange(Value: TProtocolType);
begin
  FProtocolType := Value;
  //Auto-determine port and line ending, if necessary
  DeterminePort;
  DetermineLineEnding;
end;

function TTelnetSSHClient.CommandResult(CommandText: string): string;
begin
  Result := '';
  if FConnected then
  begin
    SendData(CommandText);
    Result := ReceiveData; //gets too much
  end
  else
  begin
    Result := '';
    raise Exception.Create('Can only run command when connected');
  end;
end;

constructor TTelnetSSHClient.Create;
begin
  inherited;
  FConnected := False;
  FProtocolType := ptSSH; //Could be telnet, too
  FServerType := stUnix; //Probably a safe default.
  FTelnetLoginPrompt := 'login:';
  FTelnetPasswordPrompt := 'password:';
  DetermineLineEnding;
  DeterminePort;
end;

destructor TTelnetSSHClient.Destroy;
begin
  if FConnected then
    Disconnect;
  inherited Destroy;
end;
end.

