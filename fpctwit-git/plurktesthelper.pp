unit plurktesthelper;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  IniFiles,
  fgl,
  plurklib;

type

  TCommandHandler = procedure of object;

  TCommand = class
    Description: string;
    OnExecute: TCommandHandler;
  end;

  TCommands = specialize TFPGMap<String,TCommand>;

  { TPlurkTestHelper }

  TPlurkTestHelper = class
  private
    FPlurk: TPlurk;
    FConsumerKey: string;
    FConsumerSecret: string;
    FAuthToken: string;
    FAuthSecret: string;
    FCommands: TCommands;
    FQuit: boolean;
    procedure LoadSettings;
    procedure SaveSettings;
    // Command handlers
    procedure ChangePicture;
    procedure GetKarmaStatistics;
    procedure Echo;
    procedure CheckToken;
    procedure CheckTime;
    procedure ShowHelp;
    procedure ExitApp;
  public
    constructor Create;
    destructor Destroy; override;
    procedure MainLoop;
  end;

implementation

function GetPIN(URL: string): string;
  // Callback function: called when a PIN is requested by the authentication library
  // We get a URL the user needs to go to, and we should return the PIN the user
  // got from that URL.
var
  PIN: string;
begin
  writeln('Please go to: (note: URL should be all on one line!!!)');
  //see e.g.
  //https://dev.twitter.com/docs/api/1/get/oauth/authorize
  writeln(URL);
  writeln('... and copy the PIN you receive.');
  repeat
    begin
      writeln('Please enter the PIN:');
      readln(PIN);
    end;
  until PIN <> '';
  Result := PIN;
end;

{ TPlurkTestHelper }

procedure TPlurkTestHelper.ExitApp;
begin
  FQuit := True;
end;

constructor TPlurkTestHelper.Create;
begin
  LoadSettings;

  FPlurk := TPlurk.Create;
  with FPlurk do begin
    GetPINFunction := @GetPIN; //Register PIN callback with object
    ConsumerKey := FConsumerKey;
    ConsumerSecret := FConsumerSecret;
    // Next two will be empty if not preauthorized and using PIN auth
    AuthToken := FAuthToken;
    AuthSecret := FAuthSecret;
  end;

  FCommands := TCommands.Create;

  FCommands['karma'] := TCommand.Create;
  with FCommands['karma'] do begin
    Description := 'get karma statistics';
    OnExecute := @GetKarmaStatistics;
  end;

  FCommands['echo'] := TCommand.Create;
  with FCommands['echo'] do begin
    Description := 'test data send and receive';
    OnExecute := @Echo;
  end;

  FCommands['checktoken'] := TCommand.Create;
  with FCommands['checktoken'] do begin
    Description := 'check current token validity';
    OnExecute := @CheckToken;
  end;

  FCommands['checktime'] := TCommand.Create;
  with FCommands['checktime'] do begin
    Description := 'check current time from Plurk servers';
    OnExecute := @CheckTime;
  end;

  FCommands['chpic'] := TCommand.Create;
  with FCommands['chpic'] do begin
    Description := 'change profile picture';
    OnExecute := @ChangePicture;
  end;

  FCommands['help'] := TCommand.Create;
  with FCommands['help'] do begin
    Description := 'show this text';
    OnExecute := @ShowHelp;
  end;
  FCommands['quit'] := TCommand.Create;
  with FCommands['quit'] do begin
    Description := 'exit application';
    OnExecute := @ExitApp;
  end;
end;

destructor TPlurkTestHelper.Destroy;
var
  i: integer;
begin
  for i := 0 to FCommands.Count - 1 do begin
    FCommands.Data[i].Free;
  end;
  FCommands.Free;

  FPlurk.Free;

  SaveSettings;

  inherited Destroy;
end;

procedure TPlurkTestHelper.LoadSettings;
begin
  with TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini')) do begin
    try
      FConsumerKey := ReadString('Settings', 'ConsumerKey', '');
      FConsumerSecret := ReadString('Settings', 'ConsumerSecret', '');
      FAuthToken := ReadString('Settings', 'AuthToken', '');
      FAuthSecret := ReadString('Settings', 'AuthSecret', '');
    finally
      Free;
    end;
  end;
end;

procedure TPlurkTestHelper.SaveSettings;
begin
  with TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini')) do begin
    try
      WriteString('Settings', 'ConsumerKey', FConsumerKey);
      WriteString('Settings', 'ConsumerSecret', FConsumerSecret);
      WriteString('Settings', 'AuthToken', FAuthToken);
      WriteString('Settings', 'AuthSecret', FAuthSecret);
    finally
      Free;
    end;
  end;
end;

procedure TPlurkTestHelper.ChangePicture;
type
  TByteArray = array of byte;
var
  FileName: string;
  FStream: TFileStream;
  PictureBytes: TByteArray;
begin
  Write('Filename: ');
  ReadLn(FileName);
  try
    try
      FStream := TFileStream.Create(FileName, fmOpenRead);
      SetLength(PictureBytes, FStream.Size);
      FStream.ReadBuffer(PictureBytes[0], FStream.Size);
      with FPlurk.UsersUpdatePicture(PictureBytes) do begin
        try
          WriteLn('HTTP Result      : ', HTTPResult);
          if HTTPResult <> 200 then begin
            WriteLn('Error Text       : ' + ErrorText);
          end;
        finally
          Free;
        end;
      end;
    except
      on e: Exception do begin
        WriteLn(StdErr, 'Exception: ' + E.ClassName + '/' + E.Message);
      end;
    end;
  finally
    if Assigned(FStream) then begin
      FStream.Free;
    end;
  end;
end;

procedure TPlurkTestHelper.ShowHelp;
var
  i: integer;
begin
  for i := 0 to FCommands.Count - 1 do begin
    WriteLn(FCommands.Keys[i] + ': ' + FCommands.Data[i].Description);
  end;
end;

procedure TPlurkTestHelper.GetKarmaStatistics;
var
  Karma: TKarmaTrend;
begin
  with FPlurk.UsersGetKarmaStatus do begin
    try
      WriteLn('HTTP Result      : ', HTTPResult);
      if HTTPResult <> 200 then begin
        WriteLn('Error Text       : ' + ErrorText);
      end;
      WriteLn('Current karma    : ', CurrentKarma: 1: 2);
      WriteLn('Karma trend      :');
      for Karma in KarmaTrend do begin
        WriteLn('- ' + Karma.Timestamp + ': ', Karma.Value: 1: 2);
      end;
      WriteLn('Karma fall reason: ' + KarmaFallReason);
    finally
      Free;
    end;
  end;
end;

procedure TPlurkTestHelper.Echo;
var
  Data: string;
begin
  Write('echo data: ');
  ReadLn(Data);
  with FPlurk.OAuthEcho(Data) do begin
    try
      WriteLn('HTTP Result: ', HTTPResult);
      if HTTPResult <> 200 then begin
        WriteLn('Error Text : ' + ErrorText);
      end;
      WriteLn('Data       : ' + Data);
      WriteLn('Length     : ', Length);
    finally
      Free;
    end;
  end;
end;

procedure TPlurkTestHelper.CheckToken;
begin
  with FPlurk.OAuthCheckToken do begin
    try
      WriteLn('HTTP Result: ', HTTPResult);
      if HTTPResult <> 200 then begin
        WriteLn('Error Text : ' + ErrorText);
      end;
      WriteLn('App ID     : ', AppID);
      WriteLn('User ID    : ', UserID);
      WriteLn('Issued On  : ', IssuedOn);
      WriteLn('Device ID  : ', DeviceID);
    finally
      Free;
    end;
  end;
end;

procedure TPlurkTestHelper.CheckTime;
begin
  with FPlurk.OAuthCheckTime do begin
    try
      WriteLn('HTTP Result: ', HTTPResult);
      if HTTPResult <> 200 then begin
        WriteLn('Error Text : ' + ErrorText);
      end;
      WriteLn('Now        : ', Now);
      WriteLn('Timestamp  : ', Timestamp);
      WriteLn('App ID     : ', AppID);
      WriteLn('User ID    : ', UserID);
    finally
      Free;
    end;
  end;
end;

procedure TPlurkTestHelper.MainLoop;
var
  Line: string;
begin
  WriteLn('Plurk console test, type help for available commands');
  FQuit := False;
  repeat
    try
      Write('> ');
      ReadLn(Line);
      if Line <> '' then begin
        FCommands.KeyData[LowerCase(Line)].OnExecute;
      end;
    except
      on E: EListError do begin
        WriteLn(StdErr, 'Exception: ' + E.ClassName + '/' + E.Message);
      end;
      on E: Exception do begin
        WriteLn(StdErr, 'Exception: ' + E.ClassName + '/' + E.Message);
      end;
    end;
  until FQuit;
end;

end.
