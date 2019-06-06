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
  EFirstRun = class(Exception) end;

  TCommandHandler = procedure of object;

  TCommand = class
    Description: string;
    OnExecute: TCommandHandler;
  end;

  TCommands = specialize TFPGMap<String,TCommand>;

  { TPlurkTestHelper }

  TPlurkTestHelper = class
  private
    FSettingsFileName: String;
    FPlurk: TPlurkLib;
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
    procedure GetCurrentUserInfo;
    procedure UpdateUserInfo;
    procedure GetPlurks;
    procedure DeletePlurks;
    procedure ShowHelp;
    procedure ExitApp;
  public
    constructor Create;
    destructor Destroy; override;
    procedure MainLoop;
  end;

implementation

uses
  Types;

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
var
  SettingsFileExists: Boolean;
begin
  FSettingsFileName := ChangeFileExt(ParamStr(0), '.ini');
  SettingsFileExists := FileExists(FSettingsFileName);

  FPlurk := TPlurkLib.Create;
  LoadSettings;

  if not SettingsFileExists then begin
    raise EFirstRun.Create(FSettingsFileName + ' created, please fill the entries then re-run this app');
  end;

  FPlurk.GetPINFunction := @GetPIN; //Register PIN callback with object

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

  FCommands['user'] := TCommand.Create;
  with FCommands['user'] do begin
    Description := 'get current user information';
    OnExecute := @GetCurrentUserInfo;
  end;

  FCommands['update'] := TCommand.Create;
  with FCommands['update'] do begin
    Description := 'update current user information';
    OnExecute := @UpdateUserInfo;
  end;

  FCommands['plurks'] := TCommand.Create;
  with FCommands['plurks'] do begin
    Description := 'get plurks';
    OnExecute := @GetPlurks;
  end;

  FCommands['delete'] := TCommand.Create;
  with FCommands['delete'] do begin
    Description := 'delete plurk(s) based on ID';
    OnExecute := @DeletePlurks;
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
  if Assigned(FCommands) then begin
    for i := 0 to FCommands.Count - 1 do begin
      FCommands.Data[i].Free;
    end;
    FCommands.Free;
  end;

  SaveSettings;

  FPlurk.Free;

  inherited Destroy;
end;

procedure TPlurkTestHelper.LoadSettings;
begin
  with TIniFile.Create(FSettingsFileName) do begin
    try
      FPlurk.ConsumerKey := ReadString('Settings', 'ConsumerKey', '');
      FPlurk.ConsumerSecret := ReadString('Settings', 'ConsumerSecret', '');
      FPlurk.AuthToken := ReadString('Settings', 'AuthToken', '');
      FPlurk.AuthSecret := ReadString('Settings', 'AuthSecret', '');
    finally
      Free;
    end;
  end;
end;

procedure TPlurkTestHelper.SaveSettings;
begin
  with TIniFile.Create(FSettingsFileName) do begin
    try
      WriteString('Settings', 'ConsumerKey', FPlurk.ConsumerKey);
      WriteString('Settings', 'ConsumerSecret', FPlurk.ConsumerSecret);
      WriteString('Settings', 'AuthToken', FPlurk.AuthToken);
      WriteString('Settings', 'AuthSecret', FPlurk.AuthSecret);
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

procedure TPlurkTestHelper.GetCurrentUserInfo;
const
  BaseAvaLink    = 'http://avatars.plurk.com';
  DefaultAvaLink = 'http://www.plurk.com/static';
var
  AvaLink: String;
begin
  with FPlurk.UsersCurrUser do
    try
      WriteLn('HTTP Result: ', HTTPResult);
      if HTTPResult <> 200 then begin
        WriteLn('Error Text : ' + ErrorText);
      end;
      WriteLn('ID               : ', ID);
      WriteLn('Karma            : ', FloatToStrF(Karma,ffGeneral,8,2));
      WriteLn('Display name     : ', DisplayName);
      WriteLn('Nickname         : ', NickName);
      WriteLn('Full name        : ', FullName);
      WriteLn('About            : ', About);
      WriteLn('Gender           : ', Gender);
      WriteLn('Date of birth    : ', DateOfBirth);
      WriteLn('Timezone         : ', Timezone);
      WriteLn('Has profile image: ', HasProfileImage);
      WriteLn('Verified account : ', VerifiedAccount);
      WriteLn('Email confirmed  : ', EmailConfirmed);
      WriteLn('Birthday privacy : ', BirthdayPrivacy);
      WriteLn('Name color       : $', HexStr(NameColor,6));
      WriteLn('Default language : ', DefaultLang);
      WriteLn('Date format      : ', DateFormat);
      WriteLn('Location         : ', Location);
      if HasProfileImage then
        if Avatar >= 0 then
          AvaLink := Format('%s/%d-big%d.jpg',[BaseAvaLink,ID,Avatar])
        else
          AvaLink := Format('%s/%d-big.jpg',[BaseAvaLink,ID])
      else
        AvaLink := Format('%s/default_big.gif',[DefaultAvaLink]);
      WriteLn('Avatar           : ', AvaLink);
      WriteLn('Is premium       : ', IsPremium);
    finally
      Free;
    end;
end;

procedure TPlurkTestHelper.UpdateUserInfo;

  function StrToPrivacy(const s: String): TPrivacy; inline;
  begin
    Result := ppNoChange;
    if Length(s) > 0 then
      case LowerCase(s[1]) of
        'o': Result := ppOnlyFriends;
        'w': Result := ppWorld;
      end;
  end;

var
  FullName,Email,DisplayName,Privacy,DateOfBirth: String;
  DateValue: TDate;
begin
  WriteLn('Fill in data to update (put empty / just press enter to skip):');
  Write('Full name    : ');ReadLn(FullName);
  Write('Email        : ');ReadLn(Email);
  Write('Display name : ');ReadLn(DisplayName);
  WriteLn('Privacy ([N]o change,[O]nly friends,[W]orld)');
  Write(': ');ReadLn(Privacy);
  Write('Date of birth (YYYY-MM-DD): ');ReadLn(DateOfBirth);
  try
    DateValue := StrToDate(DateOfBirth,'YYYY-MM-DD');
  except
    DateValue := 0;
  end;
  with FPlurk.UsersUpdate(FullName,Email,DisplayName,StrToPrivacy(Privacy),DateValue) do
    try
      WriteLn('HTTP Result: ', HTTPResult);
      if HTTPResult <> 200 then begin
        WriteLn('Error Text : ' + ErrorText);
      end;
    finally
      Free;
    end;
end;

procedure TPlurkTestHelper.GetPlurks;
var
  Offset: String;
  OffsetValue: TDateTime;
  Fmt: TFormatSettings;
  i,Limit: Integer;
  P: TPlurk;
begin
  Write('Offset (y-m-d h:n:s): ');ReadLn(Offset);
  Write('Limit               : ');ReadLn(Limit);
  try
    Fmt := DefaultFormatSettings;
    Fmt.ShortDateFormat := 'y-m-d';
    OffsetValue := StrToDateTime(Offset,Fmt);
  except
    OffsetValue := 0;
  end;
  with FPlurk.PlurksGetPlurks(OffsetValue,Limit,pfOnlyUser) do
    try
      WriteLn('HTTP Result: ', HTTPResult);
      if HTTPResult <> 200 then begin
        WriteLn('Error Text : ' + ErrorText);
      end;
      for i := 0 to Plurks.Size - 1 do begin
        P := Plurks[i];
        WriteLn('ID: ',P.ID);
        WriteLn('Posted on: ',P.Timestamp);
        WriteLn('Content:');
        WriteLn(P.Content);
        WriteLn;
      end;
    finally
      Free;
    end;
end;

procedure TPlurkTestHelper.DeletePlurks;
var
  IDs: TIntegerDynArray;
  i: Integer;
begin
  SetLength(IDs,0);
  Write('Enter plurk id to delete, separated by space: ');
  while not EOLn do begin
    Read(i);
    SetLength(IDs,Length(IDs) + 1);
    IDs[Length(IDs) - 1] := i;
  end;
  ReadLn;
  with FPlurk.PlurkDelete(IDs) do
    try
      for i := 0 to Size - 1 do
        if Items[i].HTTPResult = 200 then
          WriteLn('Plurk ID ' + IntToStr(Items[i].ID) + ' successfully deleted')
        else
          WriteLn('Failed to delete plurk ID ' + IntToStr(Items[i].ID));
    finally
      Free;
    end;
end;

procedure TPlurkTestHelper.MainLoop;
var
  Line: string;
  i: Integer;
begin
  WriteLn('Plurk console test, type help for available commands');
  FQuit := False;
  repeat
    try
      Write('> ');
      ReadLn(Line);
      if Line <> '' then begin
        i := FCommands.IndexOf(Line);
        if i >= 0 then
          FCommands.Data[i].OnExecute
        else
          WriteLn(StdErr,'Unknown command: ' + Line);
      end;
    except
      on E: Exception do begin
        WriteLn(StdErr, 'Exception: ' + E.ClassName + '/' + E.Message);
      end;
    end;
  until FQuit;
end;

end.
