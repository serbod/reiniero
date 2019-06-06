unit fpctwitguiform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, Menus, twitter, authcodesformunit, IniFiles, BlowFish, base64, lclintf;

type

  { TForm1 }

  TForm1 = class(TForm)
    ConnectButton: TButton;
    ConnectMenu: TMenuItem;
    UserLabel: TLabel;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    HelpMenu: TMenuItem;
    AboutMenu: TMenuItem;
    RegisterMenu: TMenuItem;
    QuitMenu: TMenuItem;
    TweetGrid: TStringGrid;
    PrivateBox: TToggleBox;
    TweetButton: TButton;
    TweetEdit: TEdit;
    procedure ConnectButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure LoadGrid; //Load grid with tweets
    procedure AboutMenuClick(Sender: TObject);
    procedure RegisterMenuClick(Sender: TObject);
    procedure QuitMenuClick(Sender: TObject);
    procedure ConnectMenuClick(Sender: TObject);
    procedure PrivateBoxChange(Sender: TObject);
    procedure TweetButtonClick(Sender: TObject);
  private
    { private declarations }
    FConfigFileName: string;
    FOriginalAuthToken: string; //Used to check if config has changed and needs to be saved
    FOriginalAuthSecret: string;
    FOriginalConsumerKey: string;
    FOriginalConsumerSecret: string;
    FOriginalProxyHost: string;
    FOriginalProxyPort: string;
    FOriginalProxyUser: string;
    FOriginalProxyPass: string;
    FTwitter: TTwitter; //The Twitter object we use to send/receive tweets
    procedure Connect; //Connect to Twitter
    function Obfuscate(Plaintext: string): string; //Used for config obfuscation.
    function Deobfuscate(Ciphertext: string): string;
    procedure LoadConfig; //Load secret codes
    procedure SaveConfig; //Saves config, only if values have changed
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}
function PINIfSomebodyReallyWantsIt(URL: string): string;
begin
  openurl(URL);
  Application.ProcessMessages;
  Result := InputBox('Authorize this appliction', 'Please authorize the application through the link in your browser - which should be' +
    LineEnding + URL + LineEnding + '... and enter the PIN below:', '');
end;

{ TForm1 }

const
  ConfigKey = 'hihosilver,thisisonlyobfuscationnotrealcrypto_*(#$%*(&#(@#$@)_#$*(54890574r5424@E@349uijasdkgjk1934ujvgjasddst4';

procedure TForm1.LoadGrid;
var
  i: integer;
  Tweets: TTweetsArray;
begin
  Screen.Cursor := crHourglass;
  TweetGrid.BeginUpdate;
  try
    TweetGrid.Clear;
    if PrivateBox.Checked then
      Tweets := FTwitter.GetTweets(TwitterAPIURL + '/1/statuses/home_timeline.json')
    else
      Tweets := FTwitter.GetTweets(TwitterAPIURL + '/1/statuses/public_timeline.json');
    TweetGrid.RowCount := Length(Tweets)+1;

    for i := 0 to Length(Tweets) - 1 do
    begin
      TweetGrid.Cells[1, i+1] := Tweets[i].User;
      TweetGrid.Cells[2, i+1] := DateTimeTostr(Tweets[i].Timestamp);
      TweetGrid.Cells[3, i+1] := Tweets[i].Message;
    end;
    TweetGrid.AutoSizeColumns;
  finally
    TweetGrid.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FTwitter := TTwitter.Create;
  FTwitter.GetPINFunction := @PINIfSomebodyReallyWantsIt; //Inform user he should register in a different way.
  {revision<37671
  // Make sure the directory exists. Let's hope
  // GetappConfigDirUTF8 and the file version give the same directory.
  ForceDirectoriesUTF8(GetAppConfigDirUTF8(false));
  FConfigFileName := GetAppConfigFileUTF8(false,false);
  }
  // In revision 37671 and higher, replace the 2 previous lines with:
  FConfigFileName := GetAppConfigFileUTF8(false,false,true);
  LoadConfig;
end;

procedure TForm1.ConnectButtonClick(Sender: TObject);
begin
  Connect;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // If they have changed since loading, save our codes
  Screen.Cursor := crHourglass;
  try
    SaveConfig;
    if FTwitter.Connected then
      FTwitter.Disconnect;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FTwitter.Free;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: char);
begin
  // Reload
  if (Key = 'r') or (Key = 'R') then
    LoadGrid;
  //todo: use keydown for F5, perhaps Ctrl-R
end;

procedure TForm1.AboutMenuClick(Sender: TObject);
begin
  ShowMessage('Demo application for TTwitter/TOAuth1 library.' + LineEnding + 'Configuration file: ' + FConfigFileName + LineEnding +
    'Web site: https://bitbucket.org/reiniero/fpctwit'+LineEnding+
    'Programmed in Lazarus/FreePascal using Synapse network components.');
end;

procedure TForm1.RegisterMenuClick(Sender: TObject);
var
  AuthCodesForm: TAuthcodesForm; //We have to do this otherwise we get in trouble with resourceless forms
begin
  AuthCodesForm := TAuthcodesForm.Create(Application);
  try
    AuthCodesForm.AccessToken.Text := FTwitter.AuthToken;
    AuthCodesForm.AccessTokenSecret.Text := FTwitter.AuthSecret;
    AuthCodesForm.ConsumerKey.Text := FTwitter.ConsumerKey;
    AuthCodesForm.ConsumerSecret.Text := FTwitter.ConsumerSecret;
    AuthCodesForm.ProxyHost.Text := FTwitter.ProxyHost;
    AuthCodesForm.ProxyPort.Text := FTwitter.ProxyPort;
    AuthCodesForm.ProxyUser.Text := FTwitter.ProxyUser;
    AuthCodesForm.ProxyPass.Text := FTwitter.ProxyPass;
    AuthcodesForm.ShowModal; //Should change twitter properties for us.
    if AuthCodesForm.FCancelled = false then
    begin
      FTwitter.AuthToken := AuthCodesForm.AccessToken.Text;
      FTwitter.AuthSecret := AuthCodesForm.AccessTokenSecret.Text;
      FTwitter.ConsumerKey := AuthCodesForm.ConsumerKey.Text;
      FTwitter.ConsumerSecret := AuthCodesForm.ConsumerSecret.Text;
      FTwitter.ProxyHost := AuthCodesForm.ProxyHost.Text;
      FTwitter.ProxyPort := AuthCodesForm.ProxyPort.Text;
      FTwitter.ProxyUser := AuthCodesForm.ProxyUser.Text;
      FTwitter.ProxyPass := AuthCodesForm.ProxyPass.Text;
    end;
  finally
    AuthCodesForm.Release;
  end;
end;

procedure TForm1.QuitMenuClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TForm1.ConnectMenuClick(Sender: TObject);
begin
  if FTwitter.ConsumerKey='' then
  begin
    ShowMessage('Consumer key is empty. Please register first via File/Register.');
  end
  else
  begin
    Connect;
  end;
end;

procedure TForm1.PrivateBoxChange(Sender: TObject);
begin
  if PrivateBox.Checked then
    PrivateBox.Caption := 'Private timeline'
  else
    PrivateBox.Caption := 'Public timeline';
  LoadGrid;
end;

procedure TForm1.TweetButtonClick(Sender: TObject);
var
  Tweet: string;
begin
  Tweet := TweetEdit.Text;
  if FTwitter.Tweet(Tweet) then
    LoadGrid
  else
  begin
    LoadGrid;
    ShowMessage('Sorry, tweet failed.');
  end;
end;

procedure TForm1.LoadConfig;
// Loads configuration (basically keys) from .ini file.
// The ini file is obfuscated through encryption with a key in this source code.
// This is meant more to keep casual inspectors out of the config file.
// Additional protection should come from keeping the config file safe, physically
// and through logical access security.
// For this, OS specific/lower level mechanisms must be used (e.g. TrueCrypt containers).
// More protection at the application level can't be given.
const
  INISectionName = 'Secrets';
var
  ConsumerKey, ConsumerSecret, AuthToken, AuthSecret: string;
  ProxyHost, ProxyPort, ProxyUser, ProxyPass: string;
  TwitterINI: TIniFile;
begin
  // Load saved file content and overwrite any existing values
  if FileExistsUTF8(FConfigFileName) then
  begin
    // The INI file contains obfuscated values
    TwitterINI := TINIFile.Create(FConfigFileName);
    try
      // We need to explicitly check for empty data due to the encryption
      ConsumerKey := TwitterINI.ReadString(INISectionName, 'ConsumerKey', '');
      ConsumerSecret := TwitterINI.ReadString(INISectionName, 'ConsumerSecret', '');
      AuthToken := TwitterINI.ReadString(INISectionName, 'AccessToken', '');
      AuthSecret := TwitterINI.ReadString(INISectionName, 'AccessTokenSecret', '');

      //Proxy settings if any are unencrypted for now
      {todo: encrypt username, password, create GUI editor for settings.
      It's mainly for testing now so no urgent need for this.}
      ProxyHost := TwitterINI.ReadString('Proxy', 'Host', '');
      ProxyPort := TwitterINI.ReadString('Proxy', 'Port', '');
      ProxyUser := TwitterINI.ReadString('Proxy', 'User', '');
      ProxyPass := TwitterINI.ReadString('Proxy', 'Password', '');
      try
        if ConsumerKey <> '' then
          FTwitter.ConsumerKey := Deobfuscate(ConsumerKey);
        if ConsumerSecret <> '' then
          FTwitter.ConsumerSecret := Deobfuscate(ConsumerSecret);
        if AuthToken <> '' then
          FTwitter.AuthToken := Deobfuscate(AuthToken);
        if AuthSecret <> '' then
          FTwitter.AuthSecret := Deobfuscate(AuthSecret);
        if ProxyHost <> '' then
        begin
          // If proxy specified, assume the user wants to use a proxy
          FTwitter.ProxyHost := ProxyHost;
          FTwitter.ProxyPort := ProxyPort;
          FTwitter.ProxyUser := ProxyUser;
          FTwitter.ProxyPass := ProxyPass;
        end;
      except
        // If any of these steps failed, it makes sense to suspect the integrity of the whole.
        // Deobfuscation may fail due to manually edited values in the file, etc.
        FTwitter.ConsumerKey := '';
        FTwitter.ConsumerSecret := '';
        FTwitter.AuthToken := '';
        FTwitter.AuthSecret := '';
        ShowMessage('Error occurred during configuration loading. Please use File/Register.');
      end;

      FOriginalConsumerKey := FTwitter.ConsumerKey;
      FOriginalConsumerSecret := FTwitter.ConsumerSecret;
      FOriginalAuthToken := FTwitter.AuthToken;
      FOriginalAuthSecret := FTwitter.AuthSecret;
      FOriginalProxyHost := FTwitter.ProxyHost;
      FOriginalProxyPort := FTwitter.ProxyPort;
      FOriginalProxyUser := FTwitter.ProxyUser;
      FOriginalProxyPass := FTwitter.ProxyPass;
    finally
      TwitterINI.Free;
    end;
  end;
end;

procedure TForm1.SaveConfig;
// Uses encryption to obfuscate the values
const
  INISectionName = 'Secrets';
var
  TwitterINI: TIniFile;
begin
  // Save to file... only if needed
  if (FOriginalConsumerKey <> FTwitter.ConsumerKey) or (FOriginalConsumerSecret <> FTwitter.ConsumerSecret) or
    (FOriginalAuthToken <> FTwitter.AuthToken) or (FOriginalAuthSecret <> FTwitter.AuthSecret) or
    (FOriginalProxyHost <> FTwitter.ProxyHost) or (FOriginalProxyPort <> FTwitter.ProxyPort) or
    (FOriginalProxyUser <> FTwitter.ProxyUser) or (FOriginalProxyPass <> FTwitter.ProxyPass) then
  begin
    TwitterINI := TINIFile.Create(FConfigFileName);
    try
      TwitterINI.WriteString(INISectionName, 'ConsumerKey', Obfuscate(FTwitter.ConsumerKey));
      TwitterINI.WriteString(INISectionName, 'ConsumerSecret', Obfuscate(FTwitter.ConsumerSecret));
      TwitterINI.WriteString(INISectionName, 'AccessToken', Obfuscate(FTwitter.AuthToken));
      TwitterINI.WriteString(INISectionName, 'AccessTokenSecret', Obfuscate(FTwitter.AuthSecret));

      TwitterINI.WriteString('Proxy', 'Host', FTwitter.ProxyHost);
      TwitterINI.WriteString('Proxy', 'Port', FTwitter.ProxyPort);
      TwitterINI.WriteString('Proxy', 'User', FTwitter.ProxyUser);
      TwitterINI.WriteString('Proxy', 'Password', FTwitter.ProxyPass);
    finally
      TwitterINI.Free;
    end;
  end;
end;

procedure TForm1.Connect;
begin
  if FTwitter.Connect then
  begin
    UserLabel.Caption := 'User: ' + FTwitter.ScreenName;
    LoadGrid;
  end
  else
  begin
    UserLabel.Caption := 'User: not connected.';
    ShowMessage('Sorry, connection attempt failed.');
  end;
end;

function TForm1.Obfuscate(Plaintext: string): string;
  // With thanks to Leledumbo from the FPC list ;)
var
  en: TBlowFishEncryptStream;
  ResultStream: TStringStream;
begin
  Result := '';
  ResultStream := TStringStream.Create('');
  try
    en := TBlowFishEncryptStream.Create(ConfigKey, ResultStream);
    try
      en.WriteAnsiString(Plaintext);
    finally
      en.Free;
    end;
    Result := EncodeStringBase64(ResultStream.DataString);
  finally
    ResultStream.Free;
  end;
end;

function TForm1.Deobfuscate(Ciphertext: string): string;
var
  de: TBlowFishDecryptStream;
  InputStream: TStringStream;
begin
  Result := '';
  InputStream := TStringStream.Create(DecodeStringBase64(CipherText));
  try
    de := TBlowFishDecryptStream.Create(ConfigKey, InputStream);
    try
      Result := de.ReadAnsiString;
    finally
      de.Free;
    end;
  finally
    InputStream.Free;
  end;
end;

end.


