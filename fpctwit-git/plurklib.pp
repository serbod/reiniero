unit plurklib;

{ Plurk library for FreePascal

  Copyright (c) 2012 Mario Ray Mahardhika

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
  Classes,
  SysUtils,
  oauth1,
  fpjsonutf8,
  jsonparserutf8,
  dateutils,
  synautil {for readstrfromstream};

const
  PlurkBaseURL = 'http://www.plurk.com';

type
  TBirthdayPrivacy = (bpHide, bpBirthdateOnly, bpShowAll);

  TGender = (gFemale, gMale, gNotStatingOrOther);

  TRelationship = (
    rsNotSaying, rsSingle, rsMarried, rsDivorced, rsEngaged,
    rsInRelationship, rsComplicated, rsWidowed,
    rsUnstableRelationship, rsOpenRelationship
    );

  THTTPMethodResult = class
    HTTPResult: integer;
    ErrorText: string;
  end;

  TOAuthTokenResult = class(THTTPMethodResult)
    AppID: integer;
    UserID: integer;
    IssuedOn: string;
    DeviceID: string;
  end;

  TOAuthTimeResult = class(THTTPMethodResult)
    Now: string;
    Timestamp: string;
    AppID: integer;
    UserID: integer;
  end;

  TOAuthEchoResult = class(THTTPMethodResult)
    Data: string;
    Length: integer;
  end;

  TKarmaTrend = record
    Timestamp: string;
    Value: double;
  end;

  TUsersGetKarmaStatsResult = class(THTTPMethodResult)
    CurrentKarma: double;
    KarmaTrend: array of TKarmaTrend;
    KarmaFallReason: string;
    KarmaGraphURL: string;
  end;

  TPlurkProfile = class(THTTPMethodResult)
    ID: integer;
    NickName: string;
    DisplayName: string;
    VerifiedAccount: boolean;
    EmailConfirmed: boolean;
    IsPremium: Boolean;
    HasProfileImage: boolean;
    Avatar: integer;
    Location: string;
    DefaultLang: string;
    DateFormat: integer;
    DateOfBirth: String;
    BirthdayPrivacy: TBirthdayPrivacy;
    FullName: string;
    Gender: TGender;
    PageTitle: string;
    Karma: double;
    Recruited: integer;
    Relationship: TRelationship;
    NameColor: integer;
    Timezone: string;
  end;

  TPrivacy = (ppNoChange, ppWorld, ppOnlyFriends);

  { TPlurk }

  TPlurk = class(TObject)
  private
    FOauth: TOAuth1;
    // Similar to synapse HTTPMethod with built in Plurk authentication,
    // automatically parses resulting JSON and also add HTTP result code
    // to it
    // Returns parsed JSON + HTTP result code embedded
    function JSONHTTPMethod(const Method, URL: string;
      const SendBody: string): TJSONObject;
  protected
    function GetAuthSecret: string;
    function GetAuthToken: string;
    function GetConsumerKey: string;
    function GetConsumerSecret: string;
    function GetThePINFunction: TGetPINFunction;
    procedure SetAuthSecret(AValue: string);
    procedure SetAuthToken(AValue: string);
    procedure SetConsumerKey(AValue: string);
    procedure SetConsumerSecret(AValue: string);
    procedure SetThePINFunction(AValue: TGetPINFunction);
  public
    // Authentication token that identifies session
    property AuthToken: string read GetAuthToken write SetAuthToken;
    // Accompanies authentication token; keep secret
    property AuthSecret: string read GetAuthSecret write SetAuthSecret;
    // Key that identifies the application
    property ConsumerKey: string read GetConsumerKey write SetConsumerKey;
    // Accompanies consumer key; keep secret
    property ConsumerSecret: string read GetConsumerSecret
      write SetConsumerSecret;
    // Assign this if you are using OOB authentication in your application.
    // You will get a URL; direct the user to this URL and have him enter the PIN that is shown.
    // Return this PIN
    property GetPINFunction: TGetPINFunction
      read GetThePINFunction write SetThePINFunction;
    // Current UTC/GMT time. Useful for comparing UTC timestamps with current date
    function CurrentUTCTime: TDateTime;
    constructor Create;
    destructor Destroy; override;
    // OAuth utilities
    function OAuthCheckToken: TOAuthTokenResult;
    function OAuthExpireToken: TOAuthTokenResult;
    function OAuthCheckTime: TOAuthTimeResult;
    function OAuthEcho(const EchoData: string): TOAuthEchoResult;
    // Users
    function UsersCurrUser: TPlurkProfile;
    function UsersUpdate(const FullName: string = '';
      const Email: string = ''; const DisplayName: string = '';
      const Privacy: TPrivacy = ppNoChange;
      const DateOfBirth: TDate = 0): TPlurkProfile;
    function UsersUpdatePicture(const ProfileImage: array of byte):
      TPlurkProfile;
    function UsersGetKarmaStatus: TUsersGetKarmaStatsResult;
    // Profile
    function ProfileGetOwnProfile: TPlurkProfile;
    function ProfileGetPublicProfile(const UserID: string): TPlurkProfile;
  end;

implementation

{ TPlurk }

function TPlurk.GetAuthSecret: string;
begin
  Result := FOAuth.AuthSecret;
end;

function TPlurk.GetAuthToken: string;
begin
  Result := FOauth.AuthToken;
end;

function TPlurk.GetConsumerKey: string;
begin
  Result := FOauth.ConsumerKey;
end;

function TPlurk.GetConsumerSecret: string;
begin
  Result := FOauth.ConsumerSecret;
end;

function TPlurk.GetThePINFunction: TGetPINFunction;
begin
  Result := FOAuth.GetPINFunction;
end;

procedure TPlurk.SetAuthSecret(AValue: string);
begin
  FOauth.AuthSecret := AValue;
end;

procedure TPlurk.SetAuthToken(AValue: string);
begin
  FOauth.AuthToken := AValue;
end;

procedure TPlurk.SetConsumerKey(AValue: string);
begin
  FOAuth.ConsumerKey := AValue;
end;

procedure TPlurk.SetConsumerSecret(AValue: string);
begin
  FOAuth.ConsumerSecret := AValue;
end;

procedure TPlurk.SetThePINFunction(AValue: TGetPINFunction);
begin
  FOAuth.GetPINFunction := AValue;
end;

function TPlurk.CurrentUTCTime: TDateTime;
begin
  Result := CurrentUTCTime;
end;

function TPlurk.JSONHTTPMethod(const Method, URL: string;
  const SendBody: string): TJSONObject;
var
  HTTPResult: integer;
  ResultString: string;
begin
  FOauth.SendBody := SendBody;
  HTTPResult := FOauth.OAuthHTTPMethod(Method, PlurkBaseURL + URL);
  ResultString := FOauth.ReceivedBody;
  {$ifdef plurk_json_debug}
  WriteLn('DEBUG: RAW JSON: ' + ResultString);
  {$endif}
  with TJSONParser.Create(ResultString) do begin
    try
      Result := TJSONObject(Parse);
      if not Assigned(Result) then begin
        Result := TJSONObject.Create;
      end;
      Result['http_result'] := TJSONIntegerNumber.Create(HTTPResult);
    finally
      Free;
    end;
  end;
end;

constructor TPlurk.Create;
begin
  inherited Create;
  FOauth := TOAuth1.Create;
  FOAuth.BaseURL := PlurkBaseURL;
  FOAuth.RequestTokenResource := 'OAuth/request_token';
  FOAuth.AccessTokenResource := 'OAuth/access_token';
  FOAuth.AcquirePINPath := '/OAuth/authorize?oauth_token=';
  //For now, only support PIN auth and preauthenticated credentials:
  FOauth.CallBackURL := 'oob';
end;

destructor TPlurk.Destroy;
begin
  FOauth.Destroy;
  inherited Destroy;
end;

function TPlurk.OAuthCheckToken: TOAuthTokenResult;
var
  JSON: TJSONObject;
  Data: TJSONData;
begin
  JSON := JSONHTTPMethod('GET', '/APP/checkToken', '');
  with JSON do begin
    try
      Result := TOAuthTokenResult.Create;
      Data := JSON.Find('http_result');
      if Assigned(Data) then begin
        Result.HTTPResult := Data.AsInteger;
      end;
      Data := JSON.Find('error_text');
      if Assigned(Data) then begin
        Result.ErrorText := Data.AsString;
      end;
      Data := JSON.Find('app_id');
      if Assigned(Data) then begin
        Result.AppID := Data.AsInteger;
      end;
      Data := JSON.Find('user_id');
      if Assigned(Data) then begin
        Result.UserID := Data.AsInteger;
      end;
      Data := JSON.Find('issued');
      if Assigned(Data) then begin
        Result.IssuedOn := Data.AsString;
      end;
      Data := JSON.Find('device_id');
      if Assigned(Data) then begin
        Result.DeviceID := Data.AsString;
      end;
    finally
      Free;
    end;
  end;
end;

function TPlurk.OAuthExpireToken: TOAuthTokenResult;
var
  JSON: TJSONObject;
  Data: TJSONData;
begin
  JSON := JSONHTTPMethod('GET', '/APP/expireToken', '');
  with JSON do begin
    try
      Result := TOAuthTokenResult.Create;
      Data := JSON.Find('http_result');
      if Assigned(Data) then begin
        Result.HTTPResult := Data.AsInteger;
      end;
      Data := JSON.Find('error_text');
      if Assigned(Data) then begin
        Result.ErrorText := Data.AsString;
      end;
      Data := JSON.Find('app_id');
      if Assigned(Data) then begin
        Result.AppID := Data.AsInteger;
      end;
      Data := JSON.Find('user_id');
      if Assigned(Data) then begin
        Result.UserID := Data.AsInteger;
      end;
      Data := JSON.Find('issued');
      if Assigned(Data) then begin
        Result.IssuedOn := Data.AsString;
      end;
      Data := JSON.Find('device_id');
      if Assigned(Data) then begin
        Result.DeviceID := Data.AsString;
      end;
    finally
      Free;
    end;
  end;
end;

function TPlurk.OAuthCheckTime: TOAuthTimeResult;
var
  JSON: TJSONObject;
  Data: TJSONData;
begin
  JSON := JSONHTTPMethod('GET', '/APP/checkTime', '');
  with JSON do begin
    try
      Result := TOAuthTimeResult.Create;
      Data := JSON.Find('http_result');
      if Assigned(Data) then begin
        Result.HTTPResult := Data.AsInteger;
      end;
      Data := JSON.Find('error_text');
      if Assigned(Data) then begin
        Result.ErrorText := Data.AsString;
      end;
      Data := JSON.Find('now');
      if Assigned(Data) then begin
        Result.Now := Data.AsString;
      end;
      Data := JSON.Find('timestamp');
      if Assigned(Data) then begin
        Result.Timestamp := Data.AsString;
      end;
      Data := JSON.Find('app_id');
      if Assigned(Data) then begin
        Result.AppID := Data.AsInteger;
      end;
      Data := JSON.Find('user_id');
      if Assigned(Data) then begin
        Result.UserID := Data.AsInteger;
      end;
    finally
      Free;
    end;
  end;
end;

function TPlurk.OAuthEcho(const EchoData: string): TOAuthEchoResult;
var
  JSON: TJSONObject;
  Data: TJSONData;
begin
  JSON := JSONHTTPMethod('POST', '/APP/echo', 'data=' +
    EncodeURLElementRFC3896(EchoData));
  with JSON do begin
    try
      Result := TOAuthEchoResult.Create;
      Data := JSON.Find('http_result');
      if Assigned(Data) then begin
        Result.HTTPResult := Data.AsInteger;
      end;
      Data := JSON.Find('error_text');
      if Assigned(Data) then begin
        Result.ErrorText := Data.AsString;
      end;
      Data := JSON.Find('data');
      if Assigned(Data) then begin
        Result.Data := Data.AsString;
      end;
      Data := JSON.Find('length');
      if Assigned(Data) then begin
        Result.Length := Data.AsInteger;
      end;
    finally
      Free;
    end;
  end;
end;

function TPlurk.UsersCurrUser: TPlurkProfile;
var
  JSON: TJSONObject;
begin
  JSON := JSONHTTPMethod('GET', '/APP/Users/currUser', '');
end;

function TPlurk.UsersUpdate(const FullName: string; const Email: string;
  const DisplayName: string; const Privacy: TPrivacy;
  const DateOfBirth: TDate): TPlurkProfile;

  function PrivacyStr: string;
  var
    PStr: string;
  begin
    if Privacy <> ppNoChange then begin
      Result := 'privacy=';
      case Privacy of
        ppWorld: begin
          PStr := 'world';
        end;
        ppOnlyFriends: begin
          PStr := 'only_friends';
        end;
      end;
      Result := Result + EncodeURLElementRFC3896(PStr) + '&';
    end else begin
      Result := '';
    end;
  end;

  function DateStr: string;
  begin
    if DateOfBirth <> 0 then begin
      Result := 'date_of_birth=' + EncodeURLElementRFC3896(
        FormatDateTime('YYYY-MM-DD', DateOfBirth));
    end else begin
      Result := '';
    end;
  end;

var
  JSON: TJSONObject;
begin
  JSON := JSONHTTPMethod('GET', '/APP/Users/update',
    Format('full_name=%s&email=%s&display_name=%s&%s%s',
    [EncodeURLElementRFC3896(FullName),
    EncodeURLElementRFC3896(Email),
    EncodeURLElementRFC3896(DisplayName), PrivacyStr, DateStr]));
end;

function TPlurk.UsersUpdatePicture(
  const ProfileImage: array of byte): TPlurkProfile;

  function ArrayHexStr: string;
  var
    i: integer;
  begin
    Result := '';
    for i := Low(ProfileImage) to High(ProfileImage) do begin
      Result := Result + HexStr(ProfileImage[i], 2);
    end;
  end;

var
  JSON: TJSONObject;
  Data: TJSONData;
  img:string;
begin
  //Documentation is a bit vague:
  //http://www.plurk.com/API/2/#user_data
  // how to render the avatar
  //combined with
  // /APP/Users/updatePicture
  setlength(img,length(ProfileImage));
  System.Move(ProfileImage[0],img[1],length(img));
  FOauth.MimeType:='multipart/form-data; boundary=plurkboundary1234';
  JSON := JSONHTTPMethod('POST', '/APP/Users/updatePicture',
    '--plurkboundary1234'#13#10+
    'content-disposition: form-data; name="profile_image"; filename="avatar.gif"'#13#10+
    'Content-Type: image/gif'#13#10+
    'Content-Transfer-Encoding: binary'#13#10#13#10+img+#13#10+
    '--plurkboundary1234--'#13#10);
  with JSON do begin
    try
      Result := TPlurkProfile.Create;
      Data := JSON.Find('http_result');
      if Assigned(Data) then begin
        Result.HTTPResult := Data.AsInteger;
      end;
      Data := JSON.Find('error_text');
      if Assigned(Data) then begin
        Result.ErrorText := Data.AsString;
      end;
      Data := JSON.Find('verified_account');
      if Assigned(Data) then begin
        Result.VerifiedAccount := Data.AsBoolean;
      end;
      Data := JSON.Find('bday_privacy');
      if Assigned(Data) then begin
        Result.BirthdayPrivacy := TBirthdayPrivacy(Data.AsInteger);
      end;
      Data := JSON.Find('default_lang');
      if Assigned(Data) then begin
        Result.DefaultLang := Data.AsString;
      end;
      Data := JSON.Find('display_name');
      if Assigned(Data) then begin
        Result.DisplayName := Data.AsString;
      end;
      Data := JSON.Find('dateformat');
      if Assigned(Data) then begin
        Result.DateFormat := Data.AsInteger;
      end;
      Data := JSON.Find('nick_name');
      if Assigned(Data) then begin
        Result.NickName := Data.AsString;
      end;
      Data := JSON.Find('has_profile_image');
      if Assigned(Data) then begin
        Result.HasProfileImage := Data.AsBoolean;
      end;
      Data := JSON.Find('location');
      if Assigned(Data) then begin
        Result.Location := Data.AsString;
      end;
      Data := JSON.Find('avatar');
      if Assigned(Data) then begin
        Result.Avatar := Data.AsInteger;
      end;
      Data := JSON.Find('is_premium');
      if Assigned(Data) then begin
        Result.IsPremium := Data.AsBoolean;
      end;
      Data := JSON.Find('date_of_birth');
      if Assigned(Data) then begin
        if Data.IsNull then
          Result.DateOfBirth := ''
        else
          Result.DateOfBirth := Data.AsString;
      end;
      Data := JSON.Find('email_confirmed');
      if Assigned(Data) then begin
        Result.EmailConfirmed := Data.AsBoolean;
      end;
      Data := JSON.Find('full_name');
      if Assigned(Data) then begin
        Result.FullName := Data.AsString;
      end;
      Data := JSON.Find('gender');
      if Assigned(Data) then begin
        Result.Gender := TGender(Data.AsInteger);
      end;
      Data := JSON.Find('name_color');
      if Assigned(Data) then begin
        Result.NameColor := StrToInt('$' + Data.AsString);
      end;
      Data := JSON.Find('timezone');
      if Assigned(Data) then begin
        if Data.IsNull then
          Result.Timezone := ''
        else
          Result.Timezone := Data.AsString;
      end;
      Data := JSON.Find('id');
      if Assigned(Data) then begin
        Result.ID := Data.AsInteger;
      end;
      Data := JSON.Find('karma');
      if Assigned(Data) then begin
        Result.Karma := Data.AsFloat;
      end;
    finally
      Free;
    end;
  end;
end;

function TPlurk.UsersGetKarmaStatus: TUsersGetKarmaStatsResult;
var
  JSON: TJSONObject;
  Data: TJSONData;
  i: integer;
  KarmaTrend: string;
  StripPos: SizeInt;
begin
  JSON := JSONHTTPMethod('GET', '/APP/Users/getKarmaStats', '');
  with JSON do begin
    try
      Result := TUsersGetKarmaStatsResult.Create;
      Data := JSON.Find('http_result');
      if Assigned(Data) then begin
        Result.HTTPResult := Data.AsInteger;
      end;
      Data := JSON.Find('error_text');
      if Assigned(Data) then begin
        Result.ErrorText := Data.AsString;
      end;
      Data := JSON.Find('current_karma');
      if Assigned(Data) then begin
        Result.CurrentKarma := Data.AsFloat;
      end;
      Data := JSON.Find('karma_trend');
      if Assigned(Data) then begin
        with Data as TJSONArray do begin
          SetLength(Result.KarmaTrend, Count);
          for i := 0 to Count - 1 do begin
            KarmaTrend := Items[i].AsString;
            StripPos := Pos('-', KarmaTrend);
            Result.KarmaTrend[i].Timestamp :=
              Copy(KarmaTrend, 1, StripPos - 1);
            Val(
              Copy(KarmaTrend, StripPos + 1, Length(KarmaTrend) -
              StripPos + 1),
              Result.KarmaTrend[i].Value
              );
          end;
        end;
      end;
      Data := JSON.Find('karma_fall_reason');
      if Assigned(Data) then begin
        Result.KarmaFallReason := Data.AsString;
      end;
      Data := JSON.Find('karma_graph');
      if Assigned(Data) then begin
        Result.KarmaGraphURL := Data.AsString;
      end;
    finally
      Free;
    end;
  end;
end;

function TPlurk.ProfileGetOwnProfile: TPlurkProfile;
begin

end;

function TPlurk.ProfileGetPublicProfile(const UserID: string): TPlurkProfile;
begin

end;

end.
