unit plurklib;

{ Plurk library for FreePascal

  Copyright (c) 2013 Mario Ray Mahardhika

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
  gvector,
  oauth1,
  {$IF FPC_FULLVERSION>=20602}
  // Newer FPC versions support UTF8
  fpjson,
  jsonparser,
  {$ELSE}
  // Use our own version that supports UTF8
  fpjsonutf8,
  jsonparserutf8,
  {$ENDIF FPCFULLVERSION}
  dateutils,
  synautil {for readstrfromstream};

resourcestring
  SConnectionFailed = 'Cannot connect, please check your connection settings';

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
    About: string;
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

  TPlurk = class
    ID: integer;
    Content: String;
    Timestamp: String;
  end;

  { TPlurks }

  TPlurks = class(THTTPMethodResult)
  private
    type
      TPlurkVector = specialize TVector<TPlurk>;
    var
      FPlurkVector: TPlurkVector;
  public
    constructor Create;
    destructor Destroy; override;
    property Plurks: TPlurkVector read FPlurkVector;
  end;

  TPlurkDeleteResult = class(THTTPMethodResult)
    ID: Integer;
  end;

  { TPlurkDeleteResults }

  TPlurkDeleteResults = class(specialize TVector<TPlurkDeleteResult>)
  public
    destructor Destroy; override;
  end;

  TPrivacy = (ppNoChange, ppWorld, ppOnlyFriends);

  TPlurkFilter = (pfAll, pfOnlyUser,pfOnlyResponded, pfOnlyPrivate, pfOnlyFavorite);

  { TPlurkLib }

  TPlurkLib = class(TObject)
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
    function ProfileGetPublicProfile(const UserID: Integer): TPlurkProfile;
    // Timeline
    function PlurksGetPlurks(const Offset: TDateTime; const Limit: Integer = 20;
      const Filter: TPlurkFilter = pfAll; const FavorerDetail: Boolean = false;
      const LimitedDetail: Boolean = false; const ReplurkersDetail: Boolean = false): TPlurks;
    function PlurkDelete(const IDs: array of Integer): TPlurkDeleteResults;
  end;

implementation

{ TPlurkDeleteResults }

destructor TPlurkDeleteResults.Destroy;
var
  i: Integer;
begin
  for i := 0 to Size - 1 do Items[i].Free;
  inherited Destroy;
end;

{ TPlurks }

constructor TPlurks.Create;
begin
  FPlurkVector := TPlurkVector.Create;
end;

destructor TPlurks.Destroy;
var
  i: Integer;
begin
  for i := 0 to FPlurkVector.Size - 1 do
    FPlurkVector[i].Free;
  FPlurkVector.Free;
  inherited Destroy;
end;

{ TPlurkLib }

function TPlurkLib.GetAuthSecret: string;
begin
  Result := FOAuth.AuthSecret;
end;

function TPlurkLib.GetAuthToken: string;
begin
  Result := FOauth.AuthToken;
end;

function TPlurkLib.GetConsumerKey: string;
begin
  Result := FOauth.ConsumerKey;
end;

function TPlurkLib.GetConsumerSecret: string;
begin
  Result := FOauth.ConsumerSecret;
end;

function TPlurkLib.GetThePINFunction: TGetPINFunction;
begin
  Result := FOAuth.GetPINFunction;
end;

procedure TPlurkLib.SetAuthSecret(AValue: string);
begin
  FOauth.AuthSecret := AValue;
end;

procedure TPlurkLib.SetAuthToken(AValue: string);
begin
  FOauth.AuthToken := AValue;
end;

procedure TPlurkLib.SetConsumerKey(AValue: string);
begin
  FOAuth.ConsumerKey := AValue;
end;

procedure TPlurkLib.SetConsumerSecret(AValue: string);
begin
  FOAuth.ConsumerSecret := AValue;
end;

procedure TPlurkLib.SetThePINFunction(AValue: TGetPINFunction);
begin
  FOAuth.GetPINFunction := AValue;
end;

function TPlurkLib.CurrentUTCTime: TDateTime;
begin
  Result := Now;
end;

function TPlurkLib.JSONHTTPMethod(const Method, URL: string;
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
      if HTTPResult = 0 then Result['error_text'] := TJSONString.Create(SConnectionFailed);
    finally
      Free;
    end;
  end;
end;

constructor TPlurkLib.Create;
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

destructor TPlurkLib.Destroy;
begin
  FOauth.Destroy;
  inherited Destroy;
end;

function TPlurkLib.OAuthCheckToken: TOAuthTokenResult;
var
  JSON: TJSONObject;
  Data: TJSONData;
begin
  JSON := JSONHTTPMethod('GET', '/APP/checkToken', '');
  with JSON do begin
    try
      Result := TOAuthTokenResult.Create;
      Data := Find('http_result');
      if Assigned(Data) then begin
        Result.HTTPResult := Data.AsInteger;
      end;
      Data := Find('error_text');
      if Assigned(Data) then begin
        Result.ErrorText := Data.AsString;
      end;
      Data := Find('app_id');
      if Assigned(Data) then begin
        Result.AppID := Data.AsInteger;
      end;
      Data := Find('user_id');
      if Assigned(Data) then begin
        Result.UserID := Data.AsInteger;
      end;
      Data := Find('issued');
      if Assigned(Data) then begin
        Result.IssuedOn := Data.AsString;
      end;
      Data := Find('device_id');
      if Assigned(Data) then begin
        Result.DeviceID := Data.AsString;
      end;
    finally
      Free;
    end;
  end;
end;

function TPlurkLib.OAuthExpireToken: TOAuthTokenResult;
var
  JSON: TJSONObject;
  Data: TJSONData;
begin
  JSON := JSONHTTPMethod('GET', '/APP/expireToken', '');
  with JSON do begin
    try
      Result := TOAuthTokenResult.Create;
      Data := Find('http_result');
      if Assigned(Data) then begin
        Result.HTTPResult := Data.AsInteger;
      end;
      Data := Find('error_text');
      if Assigned(Data) then begin
        Result.ErrorText := Data.AsString;
      end;
      Data := Find('app_id');
      if Assigned(Data) then begin
        Result.AppID := Data.AsInteger;
      end;
      Data := Find('user_id');
      if Assigned(Data) then begin
        Result.UserID := Data.AsInteger;
      end;
      Data := Find('issued');
      if Assigned(Data) then begin
        Result.IssuedOn := Data.AsString;
      end;
      Data := Find('device_id');
      if Assigned(Data) then begin
        Result.DeviceID := Data.AsString;
      end;
    finally
      Free;
    end;
  end;
end;

function TPlurkLib.OAuthCheckTime: TOAuthTimeResult;
var
  JSON: TJSONObject;
  Data: TJSONData;
begin
  JSON := JSONHTTPMethod('GET', '/APP/checkTime', '');
  with JSON do begin
    try
      Result := TOAuthTimeResult.Create;
      Data := Find('http_result');
      if Assigned(Data) then begin
        Result.HTTPResult := Data.AsInteger;
      end;
      Data := Find('error_text');
      if Assigned(Data) then begin
        Result.ErrorText := Data.AsString;
      end;
      Data := Find('now');
      if Assigned(Data) then begin
        Result.Now := Data.AsString;
      end;
      Data := Find('timestamp');
      if Assigned(Data) then begin
        Result.Timestamp := Data.AsString;
      end;
      Data := Find('app_id');
      if Assigned(Data) then begin
        Result.AppID := Data.AsInteger;
      end;
      Data := Find('user_id');
      if Assigned(Data) then begin
        Result.UserID := Data.AsInteger;
      end;
    finally
      Free;
    end;
  end;
end;

function TPlurkLib.OAuthEcho(const EchoData: string): TOAuthEchoResult;
var
  JSON: TJSONObject;
  Data: TJSONData;
begin
  JSON := JSONHTTPMethod('POST', '/APP/echo', 'data=' +
    EncodeURLElementRFC3896(EchoData));
  with JSON do begin
    try
      Result := TOAuthEchoResult.Create;
      Data := Find('http_result');
      if Assigned(Data) then begin
        Result.HTTPResult := Data.AsInteger;
      end;
      Data := Find('error_text');
      if Assigned(Data) then begin
        Result.ErrorText := Data.AsString;
      end;
      Data := Find('data');
      if Assigned(Data) then begin
        Result.Data := Data.AsString;
      end;
      Data := Find('length');
      if Assigned(Data) then begin
        Result.Length := Data.AsInteger;
      end;
    finally
      Free;
    end;
  end;
end;

function TPlurkLib.UsersCurrUser: TPlurkProfile;
var
  JSON: TJSONObject;
  Data: TJSONData;
  RelInt: Integer;
begin
  JSON := JSONHTTPMethod('GET', '/APP/Users/currUser', '');
  with JSON do begin
    try
      Result := TPlurkProfile.Create;
      Data := Find('http_result');
      if Assigned(Data) then begin
        Result.HTTPResult := Data.AsInteger;
      end;
      Data := Find('error_text');
      if Assigned(Data) then begin
        Result.ErrorText := Data.AsString;
      end;
      Data := Find('page_title');
      if Assigned(Data) then begin
        Result.PageTitle := Data.AsString;
      end;
      Data := Find('verified_account');
      if Assigned(Data) then begin
        Result.VerifiedAccount := Data.AsBoolean;
      end;
      Data := Find('bday_privacy');
      if Assigned(Data) then begin
        Result.BirthdayPrivacy := TBirthdayPrivacy(Data.AsInteger);
      end;
      Data := Find('default_lang');
      if Assigned(Data) then begin
        Result.DefaultLang := Data.AsString;
      end;
      Data := Find('display_name');
      if Assigned(Data) then begin
        Result.DisplayName := Data.AsString;
      end;
      Data := Find('dateformat');
      if Assigned(Data) then begin
        Result.DateFormat := Data.AsInteger;
      end;
      Data := Find('nick_name');
      if Assigned(Data) then begin
        Result.NickName := Data.AsString;
      end;
      Data := Find('has_profile_image');
      if Assigned(Data) then begin
        Result.HasProfileImage := Data.AsBoolean;
      end;
      Data := Find('location');
      if Assigned(Data) then begin
        Result.Location := Data.AsString;
      end;
      Data := Find('avatar');
      if Assigned(Data) then begin
        if Data.IsNull then
          Result.Avatar := -1
        else
          Result.Avatar := Data.AsInteger;
      end;
      Data := Find('is_premium');
      if Assigned(Data) then begin
        Result.IsPremium := Data.AsBoolean;
      end;
      Data := Find('date_of_birth');
      if Assigned(Data) then begin
        if Data.IsNull then
          Result.DateOfBirth := ''
        else
          Result.DateOfBirth := Data.AsString;
      end;
      Data := Find('email_confirmed');
      if Assigned(Data) then begin
        Result.EmailConfirmed := Data.AsBoolean;
      end;
      Data := Find('full_name');
      if Assigned(Data) then begin
        Result.FullName := Data.AsString;
      end;
      Data := Find('about');
      if Assigned(Data) then begin
        Result.About := Data.AsString;
      end;
      Data := Find('gender');
      if Assigned(Data) then begin
        Result.Gender := TGender(Data.AsInteger);
      end;
      Data := Find('relationship');
      if Assigned(Data) then begin
        case Data.AsString of
          'not_saying'           : RelInt := 0;
          'single'               : RelInt := 1;
          'married'              : RelInt := 2;
          'divorced'             : RelInt := 3;
          'engaged'              : RelInt := 4;
          'in_relationship'      : RelInt := 5;
          'complicated'          : RelInt := 6;
          'widowed'              : RelInt := 7;
          'unstable_relationship': RelInt := 8;
          'open_relationship'    : RelInt := 9;
        end;
        Result.Relationship := TRelationship(RelInt);
      end;
      Data := Find('name_color');
      if Assigned(Data) then begin
        Result.NameColor := StrToInt('$' + Data.AsString);
      end;
      Data := Find('recruited');
      if Assigned(Data) then begin
        Result.Recruited := StrToInt('$' + Data.AsString);
      end;
      Data := Find('timezone');
      if Assigned(Data) then begin
        if Data.IsNull then
          Result.Timezone := ''
        else
          Result.Timezone := Data.AsString;
      end;
      Data := Find('id');
      if Assigned(Data) then begin
        Result.ID := Data.AsInteger;
      end;
      Data := Find('karma');
      if Assigned(Data) then begin
        Result.Karma := Data.AsFloat;
      end;
    finally
      Free;
    end;
  end;
end;

function TPlurkLib.UsersUpdate(const FullName: string; const Email: string;
  const DisplayName: string; const Privacy: TPrivacy;
  const DateOfBirth: TDate): TPlurkProfile;

  function PrivacyStr: string;
  var
    PStr: string;
  begin
    if Privacy <> ppNoChange then begin
      case Privacy of
        ppWorld: begin
          PStr := 'world';
        end;
        ppOnlyFriends: begin
          PStr := 'only_friends';
        end;
      end;
      Result := EncodeURLElementRFC3896(PStr);
    end else begin
      Result := '';
    end;
  end;

  function DateStr: string;
  begin
    if DateOfBirth <> 0 then begin
      Result := EncodeURLElementRFC3896(
        FormatDateTime('YYYY-MM-DD', DateOfBirth));
    end else begin
      Result := '';
    end;
  end;

var
  JSON: TJSONObject;
  Data: TJSONData;
begin
  with TStringList.Create do
    try
      Delimiter := '&';
      StrictDelimiter := true;
      if FullName <> EmptyStr then Add('full_name=' + EncodeURLElementRFC3896(FullName));
      if Email <> EmptyStr then Add('email=' + EncodeURLElementRFC3896(Email));
      if DisplayName <> EmptyStr then Add('display_name=' + EncodeURLElementRFC3896(DisplayName));
      if Privacy <> ppNoChange then Add('privacy=' + PrivacyStr);
      if Date <> 0 then Add('date_of_birth=' + DateStr);
      JSON := JSONHTTPMethod('POST', '/APP/Users/update',DelimitedText);
    finally
      Free;
    end;
  with JSON do begin
    try
      Result := TPlurkProfile.Create;
      Data := Find('http_result');
      if Assigned(Data) then begin
        Result.HTTPResult := Data.AsInteger;
      end;
      Data := Find('error_text');
      if Assigned(Data) then begin
        Result.ErrorText := Data.AsString;
      end;
    finally
      Free;
    end;
  end;
end;

function TPlurkLib.UsersUpdatePicture(
  const ProfileImage: array of byte): TPlurkProfile;
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
      Data := Find('http_result');
      if Assigned(Data) then begin
        Result.HTTPResult := Data.AsInteger;
      end;
      Data := Find('error_text');
      if Assigned(Data) then begin
        Result.ErrorText := Data.AsString;
      end;
    finally
      Free;
    end;
  end;
end;

function TPlurkLib.UsersGetKarmaStatus: TUsersGetKarmaStatsResult;
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
      Data := Find('http_result');
      if Assigned(Data) then begin
        Result.HTTPResult := Data.AsInteger;
      end;
      Data := Find('error_text');
      if Assigned(Data) then begin
        Result.ErrorText := Data.AsString;
      end;
      Data := Find('current_karma');
      if Assigned(Data) then begin
        Result.CurrentKarma := Data.AsFloat;
      end;
      Data := Find('karma_trend');
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
      Data := Find('karma_fall_reason');
      if Assigned(Data) then begin
        Result.KarmaFallReason := Data.AsString;
      end;
      Data := Find('karma_graph');
      if Assigned(Data) then begin
        Result.KarmaGraphURL := Data.AsString;
      end;
    finally
      Free;
    end;
  end;
end;

function TPlurkLib.ProfileGetOwnProfile: TPlurkProfile;
begin

end;

function TPlurkLib.ProfileGetPublicProfile(const UserID: Integer): TPlurkProfile;
begin

end;

function TPlurkLib.PlurksGetPlurks(const Offset: TDateTime;
  const Limit: Integer; const Filter: TPlurkFilter;
  const FavorerDetail: Boolean; const LimitedDetail: Boolean;
  const ReplurkersDetail: Boolean): TPlurks;
var
  JSON: TJSONObject;
  Data: TJSONData;
  DateStr,FilterStr: String;
  PlurkData: TJSONArray;
  i: Integer;
  PD: TJSONObject;
  P: TPlurk;
begin
  with TStringList.Create do
    try
      Delimiter := '&';
      StrictDelimiter := true;
      if Offset <> 0 then begin
        DateStr := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss',Offset);
        Add('offset=' + EncodeURLElementRFC3896(DateStr));
      end;
      Add('limit=' + EncodeURLElementRFC3896(IntToStr(Limit)));
      if Filter <> pfAll then begin
        case Filter of
          pfOnlyUser     : FilterStr := 'only_user';
          pfOnlyResponded: FilterStr := 'only_responded';
          pfOnlyPrivate  : FilterStr := 'only_private';
          pfOnlyFavorite : FilterStr := 'only_favorite';
        end;
        Add('filter=' + EncodeURLElementRFC3896(FilterStr));
      end;
      if FavorerDetail then Add('favorer_detail=true');
      if LimitedDetail then Add('limited_detail=true');
      if ReplurkersDetail then Add('replurkers_detail=true');
      JSON := JSONHTTPMethod('POST', '/APP/Timeline/getPlurks',DelimitedText);
    finally
      Free;
    end;
  with JSON do begin
    try
      Result := TPlurks.Create;
      Data := Find('http_result');
      if Assigned(Data) then begin
        Result.HTTPResult := Data.AsInteger;
      end;
      Data := Find('error_text');
      if Assigned(Data) then begin
        Result.ErrorText := Data.AsString;
      end;
      Data := Find('plurks');
      if Assigned(Data) then begin
        PlurkData := Data as TJSONArray;
        for i := 0 to PlurkData.Count - 1 do begin
          PD := PlurkData[i] as TJSONObject;
          P := TPlurk.Create;
          with P do begin
            ID := PD['plurk_id'].AsInteger;
            Content := PD['content'].AsString;
            Timestamp := PD['posted'].AsString;
          end;
          Result.Plurks.PushBack(P);
        end;
      end;
    finally
      Free;
    end;
  end;
end;

function TPlurkLib.PlurkDelete(const IDs: array of Integer
  ): TPlurkDeleteResults;
var
  ID: Integer;
  JSON: TJSONObject;
  Data: TJSONData;
  R: TPlurkDeleteResult;
begin
  Result := TPlurkDeleteResults.Create;
  for ID in IDs do begin
    JSON := JSONHTTPMethod('POST', '/APP/Timeline/plurkDelete','plurk_id=' + EncodeURLElementRFC3896(IntToStr(ID)));
    with JSON do begin
      try
        R := TPlurkDeleteResult.Create;
        R.ID := ID;
        Data := Find('http_result');
        if Assigned(Data) then begin
          R.HTTPResult := Data.AsInteger;
        end;
        Data := Find('error_text');
        if Assigned(Data) then begin
          R.ErrorText := Data.AsString;
        end;
        Result.PushBack(R);
      finally
        Free;
      end;
    end;
  end;
end;

end.
