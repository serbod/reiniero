unit twitter;

{ Twitter library for FreePascal

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

{
Usage/development notes:
Twitter sends data as UTF8. Todo: we could check this by inspecting the HTTP headers.
All strings received from Twitter contain UTF8 strings; it is the responsibility of the programmer to convert these if necessary.

Streaming API: the streaming API will send normal tweets, deletion statuses and more.
The code converts a deletion status message to a "tweet" with the Deleted field set to true.
Your application should use this to remove the tweet with corresponding id from storage etc.

}

// Follow oauth includes, including setting test compiler define:
{$i oauth.inc}

interface
// Used for debug output/logging etc.

uses
  Classes, SysUtils, oauth1,
  {$IF FPC_FULLVERSION>=20602}
  // Newer FPC versions support UTF8
  fpjson,
  jsonparser,
  {$ELSE}
  // Use our own version that supports UTF8
  fpjsonutf8,
  jsonparserutf8,
  {$ENDIF FPCFULLVERSION}
  dateutils, synautil {for readstrfromstream};

const
  TwitterAPIURL = 'https://api.twitter.com';
//Replace with http for sniffing traffic when testing

type
  TTweet = record
    ID: qword; //Or perhaps longword (4 bytes) will suffice
    Deleted: boolean; //If yes, the tweet has been marked for deletion by Twitter.
    // In a stream, you will see the original message with Deleted=false, and one with Deleted=true
    // Note: the delete message could be received before the original message.

    //http://support.twitter.com/entries/14609-how-to-change-your-username#
    //User name: max 15 characters (probably the "screen name" in the API docs);
    //real name: max 20 characters (probably the "name" in the API docs)
    User: string; //Name/real name
    UserID: qword; //Unique user ID
    Message: string; //Max 140 characters
    Timestamp: TDateTime;
  end;
  TTweetsArray = array of TTweet;

  TGetPINFunction = function(URL: string): string;

  { TTwitter }
  TTwitter = class(TObject)
  private
    FConnected: boolean;
    FOauth: TOAuth1; //Used for authorization & HTTP/HTTPS communication
    FRateLimit: integer;
    FRateLimitRemaining: integer;
    FRateLimitTimeRemaining: TTime;
    FScreenName: string; //Twitter screen name can be different from username
    function GetProxyHost: string;
    function GetProxyPass: string;
    function GetProxyPort: string;
    function GetProxyUser: string;
    procedure SetProxyHost(AValue: string);
    procedure SetProxyPass(AValue: string);
    procedure SetProxyPort(AValue: string);
    procedure SetProxyUser(AValue: string);
  protected
    function GetAuthSecret: string;
    function GetAuthToken: string;
    function GetConsumerKey: string;
    function GetConsumerSecret: string;
    function GetThePINFunction: TGetPINFunction;
    // Processes responses and filters out Twitter-specific headers (rate limit etc)
    procedure ProcessHeaders;
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
    // Connect and authenticate to Twitter. Use this explicitly or it will be done for you implicitly
    function Connect: boolean;
    // Whether you have connected and authenticated succesfully to Twitter
    property Connected: boolean read FConnected;
    // Key that identifies the application
    property ConsumerKey: string read GetConsumerKey write SetConsumerKey;
    // Accompanies consumer key; keep secret
    property ConsumerSecret: string read GetConsumerSecret write SetConsumerSecret;
    // If authenticated, close session
    procedure Disconnect;
    // Assign this if you are using OOB authentication in your application.
    // You will get a URL; direct the user to this URL and have him enter the PIN that is shown.
    // Return this PIN
    property GetPINFunction: TGetPINFunction read GetThePINFunction write SetThePINFunction;
    // Returns all tweets generated by call to specified URL.
    // If URL is empty, show home timeline/user's tweets.
    function GetTweets(const URL: string): TTweetsArray;
    // Gets tweets from streaming API (the "Spritzer" - a sample of all tweets)
    // If it works, it doesn't return until the application stops the stream.
    // Let your handler return false if you want to stop processing.
    // Returns http result code.
    function GetTweetStream(Handler: TReceiveHandler): integer;
    // Current UTC/GMT time. Useful for comparing UTC timestamps with current date
    function GetUTCTime: TDateTime;
    // Similar to synapse HTTPMethod with built in Twitter authentication
    // Returns HTTP result code (e.g. 200 OK) or 0 on failure
    function HTTPMethod(const Method, URL: string; var DocumentString: string): integer;
    // Converts JSON tweets data into tweets.
    // Can also be used for e.g. offline processing of data
    function ProcessTweets(JSONTweets: string): TTweetsArray;
    // Address of proxy server (IP address or domain name), if any, that
    // you want to connect through
    property ProxyHost: string read GetProxyHost write SetProxyHost;
    // Port number of proxy server, if any, that you want to connect through
    property ProxyPort: string read GetProxyPort write SetProxyPort;
    // Username for proxy server, if any, that you want to connect through
    property ProxyUser: string read GetProxyUser write SetProxyUser;
    // Password for proxy server, if any, that you want to connect through
    property ProxyPass: string read GetProxyPass write SetProxyPass;

    // Total number of calls twitter allows in time period (indicated in RateLimitTimeRemaining)
    property RateLimit: integer read FRateLimit;
    // Remaining number of calls twitter allows in time period (indicated in RateLimitTimeRemaining)
    property RateLimitRemaining: integer read FRateLimitRemaining;
    // Time in seconds before rate limit counter is reset
    property RateLimitTimeRemaing: TTime read FRateLimitTimeRemaining;
    // Twitter screen name
    property ScreenName: string read FScreenName write FScreenName;
    // Tweet a message; returns result
    function Tweet(Message: string): boolean;
    // Tweet a message; returns tweet if succes or empty array on failure
    function TweetAndSee(Message: string): TTweetsArray;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

function ParseTwitterTimestamp(InputDate: string): TDateTime;
  // Converts something like
  // Thu Jun 21 11:26:42 +0000 2012
  // 123456789012345678901234567890
  //          1         2         3
  // to a valid TDateTime
  // Inspired by code by Simon J Stuart aka LaKraven
const
  MonthNames: array[1..12] of string =
    ('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec');
var
  MonthName, DayInMonth, TZOffset, Year, Hour, Minute, Second: string;
  Month, i: integer;
begin
  MonthName := Trim(Copy(InputDate, 5, 3));
  DayInMonth := Trim(Copy(InputDate, 9, 2));
  Hour := Trim(Copy(InputDate, 12, 2));
  Minute := Trim(Copy(InputDate, 15, 2));
  Second := Trim(Copy(InputDate, 18, 2));
  TZOffset := Trim(Copy(InputDate, 20, 6));
  Year := Trim(Copy(InputDate, 27, 4));

  Month := -1;
  for i := 1 to 12 do
  begin
    if MonthNames[i] = LowerCase(MonthName) then
      Month := i;
  end;

  try
    Result := EncodeDateTime(StrToInt(Year), Month, StrToInt(DayInMonth), StrToInt(Hour), StrToInt(Minute), StrToInt(Second), 0);
  except
    Result := EncodeDateTime(1900, 1, 1, 0, 0, 0, 0);
  end;

  //todo: deal with timezone if any!!!
end;


{ TTwitter }

procedure TTwitter.SetProxyHost(AValue: string);
begin
  FOauth.ProxyHost := AValue;
end;

function TTwitter.GetProxyHost: string;
begin
  Result := FOAuth.ProxyHost;
end;

function TTwitter.GetProxyPass: string;
begin
  Result := FOAuth.ProxyPass;
end;

function TTwitter.GetProxyPort: string;
begin
  Result := FOAuth.ProxyPort;
end;

function TTwitter.GetProxyUser: string;
begin
  Result := FOAuth.ProxyUser;
end;

procedure TTwitter.SetProxyPass(AValue: string);
begin
  FOAuth.ProxyPass := AValue;
end;

procedure TTwitter.SetProxyPort(AValue: string);
begin
  FOAuth.ProxyPort := AValue;
end;

procedure TTwitter.SetProxyUser(AValue: string);
begin
  FOAuth.ProxyUser := AValue;
end;

function TTwitter.GetAuthSecret: string;
begin
  Result := FOAuth.AuthSecret;
end;

function TTwitter.GetAuthToken: string;
begin
  Result := FOauth.AuthToken;
end;

function TTwitter.GetConsumerKey: string;
begin
  Result := FOauth.ConsumerKey;
end;

function TTwitter.GetConsumerSecret: string;
begin
  Result := FOauth.ConsumerSecret;
end;

function TTwitter.GetThePINFunction: TGetPINFunction;
begin
  Result := FOAuth.GetPINFunction;
end;

procedure TTwitter.ProcessHeaders;
const
  RateLimitHeader = 'X-RateLimit-Limit';
  RateLimitRemainingHeader = 'X-RateLimit-Remaining';
  RateLimitResetHeader = 'X-RateLimit-Reset';
var
  i: integer;
begin
  try
    // Rate limits: see e.g.
    //http://dev.twitter.com/docs/rate-limiting/faq
    i := FOauth.ReceivedHeaders.IndexOfName(RateLimitHeader);
    if i >= 0 then
      FRateLimit := StrToInt(Trim(FOAuth.ReceivedHeaders.ValueFromIndex[i]));
    i := FOauth.ReceivedHeaders.IndexOfName(RateLimitRemainingHeader);
    if i >= 0 then
      FRateLimitRemaining := StrToInt(Trim(FOAuth.ReceivedHeaders.ValueFromIndex[i]));
    i := FOauth.ReceivedHeaders.IndexOfName(RateLimitResetHeader);
    if i >= 0 then
      // Convert from seconds to TTime
      FRateLimitTimeRemaining :=
        StrToInt(Trim(FOAuth.ReceivedHeaders.ValueFromIndex[i])) * OneSecond;
  except
    on E: Exception do
    begin
      // Ignore errors here; auxilliary functions
      {$IFDEF OAUTHTEST}
      FOAuth.Log('TTwitter.ProcessHeaders: exception: ' + E.ClassName + '/' + E.Message);
      {$ENDIF}
    end;
  end;
end;

function TTwitter.ProcessTweets(JSONTweets: string): TTweetsArray;
type
  DataTypes = (DeletedTweet, ArrayWithTweets, ArrayWithUnknownStuff,
    SingleTweet, Unknown, Uninterested);
var
  i: integer;
  Parser: TJSONParser;
  TweetCounter: integer;
  TweetData: TJSONObject;
  TweetsData: TJSONData;
  TweetsType: DataTypes;

  procedure ParseSingleTweet(TheTweet: TJSONObject);
  var
    TweetUser: TJSONObject;
  begin
    // Deletion messages are not processed by ParseSingleTweet, so this is safe:
    Result[TweetCounter].Deleted := false;
    try
      //https://dev.twitter.com/docs/twitter-ids-json-and-snowflake
      //64bit unsigned integer in 'id'
      //Use the string representation in 'id_str' if experiencing problems
      Result[TweetCounter].ID := TheTweet.Int64s['id'];
    except
      // Apparently does not exist
      Result[TweetCounter].ID := 0;
    end;

    try
      Result[TweetCounter].Message := TheTweet.Strings['text'];
    except
      Result[TweetCounter].Message := '';
    end;

    //User fields:
    //http://dev.twitter.com/docs/platform-objects/users
    // Note: the contents of the user field inside a tweet are not reliable.
    // More elegant to cache user ids and do a parallel REST API lookup for details
    TweetUser := TheTweet.Objects['user'];
    if assigned(TweetUser) then
    begin
      try
        Result[TweetCounter].User := TweetUser.Strings['name']
      except
        Result[TweetCounter].User := '';
      end;
      try
        Result[TweetCounter].UserID := TweetUser.Int64s['id'];
      except
        Result[TweetCounter].UserID := 0;
      end;
    end
    else
    begin
      Result[TweetCounter].User := '';
      Result[TweetCounter].UserID := 0;
    end;

    try
      Result[TweetCounter].Timestamp :=
        ParseTwitterTimeStamp(TheTweet.Strings['created_at']);
    except
      Result[TweetCounter].Timestamp := EncodeDateTime(1900, 1, 1, 0, 0, 0, 0);
    end;
  end;

  procedure ParseDeletion(StatusMessage: TJSONObject);
  // Parses a deletion JSON snippet and returns a "tweet" with the Deleted flag set.
  begin
    Result[TweetCounter].Deleted := true;
    try
      //https://dev.twitter.com/docs/twitter-ids-json-and-snowflake
      //64bit unsigned integer in 'id'
      //Use the string representation in 'id_str' if experiencing problems
      Result[TweetCounter].ID := StatusMessage.Int64s['id'];
    except
      // Apparently does not exist => then it is pointless
      Result[TweetCounter].ID := 0;
    end;

    Result[TweetCounter].Message := '';
    Result[TweetCounter].User := '';

    //User fields:
    //http://dev.twitter.com/docs/platform-objects/users
    try
      Result[TweetCounter].UserID := StatusMessage.Int64s['user_id'];
    except
      Result[TweetCounter].UserID := 0;
    end;

    // Let's put in the UTC time we received it. There's no timestamp info available in the content.
    Result[TweetCounter].Timestamp := GetUTCTime;
  end;

begin
  SetLength(Result, 0);
  try
    Parser := TJSONParser.Create(JSONTweets);
    try
      TweetsData := Parser.Parse;
      // Tweets:
      //http://dev.twitter.com/docs/platform-objects/tweets
      // Types of other data in streaming API:
      //http://dev.twitter.com/docs/streaming-apis/messages
      if Assigned(TweetsData) then
        case TweetsData.JSONType of
          jtArray: //probably array of ArrayWithTweets
            TweetsType := ArrayWithUnknownStuff;
          jtObject: //probably deleted tweet, or single tweet
          begin
            if (TJSONObject(TweetsData).IndexOfName('id', false) >= 0) and
              (TJSONObject(TweetsData).IndexOfName('text', false) >= 0) then
              TweetsType := SingleTweet
            else if TJSonObject(TweetsData).IndexOfName('delete', true) >= 0 then
              TweetsType := DeletedTweet
            else if TJSONObject(TweetsData).IndexOfName('status_withheld') >= 0 then
            begin
              {$IFDEF OAUTHTEST}
              FOAuth.Log('ProcessTweets: got limit notice:' + LineEnding + TweetsData.AsString);
              {$ENDIF OAUTHTEST}
              TweetsType := Uninterested;
            end
            else if TJSONObject(TweetsData).IndexOfName('scrub_geo') >= 0 then
              TweetsType := Uninterested
            {note: may be interested later if we implement geo functionality}
            else if TJSONObject(TweetsData).IndexOfName('status_withheld') >= 0 then
              TweetsType := Uninterested
            else if TJSONObject(TweetsData).IndexOfName('user_withheld') >= 0 then
              TweetsType := Uninterested;
          end;
          jtString: //Ignore; print out
          begin
            TweetsType := Unknown;
            {$IFDEF OAUTHTEST}
            FOAuth.Log('ProcessTweets: ignoring JSON string:' + LineEnding + TweetsData.AsString);
            {$ENDIF OAUTHTEST}
          end;
          else {jtBoolean,jtNumber,jtNull,jtUnknown}
          begin
            TweetsType := Unknown;
            {$IFDEF OAUTHTEST}
            FOAuth.Log('ProcessTweets: ignoring unknown JSON:' + LineEnding + JSONTweets);
            {$ENDIF OAUTHTEST}
          end;
        end;

      case TweetsType of
        ArrayWithUnknownStuff, ArrayWithTweets:
        begin
          SetLength(Result, TweetsData.Count);
          for TweetCounter := 0 to TweetsData.Count - 1 do
          begin
            TweetData := TJSONObject(TweetsData.Items[TweetCounter]);
            if TweetData.IndexOfName('id', false) >= 0 then
              if TweetData.IndexOfName('text', false) >= 0 then
                TweetsType := ArrayWithTweets;
            //Ignore ArrayWithUnknownStuff for now.
            case TweetsType of
              ArrayWithTweets:
                ParseSingleTweet(TweetData);
              ArrayWithUnknownStuff:
              begin
                {$IFDEF OAUTHTEST}
                FOAuth.Log('ProcessTweets: ignoring unknown JSON array:' + LineEnding + JSONTweets);
                {$ENDIF OAUTHTEST}
              end;
              else
                raise Exception.Create('Unknown tweet type, please fix your code.');
            end;
          end;
        end;
        SingleTweet:
        begin
          SetLength(Result, 1); //Single tweet.
          TweetCounter := 0;
          TweetData := TJSONObject(TweetsData);
          ParseSingleTweet(TweetData);
        end;
        DeletedTweet:
        begin
          try
            // Object contains delete object
            i := TJSONObject(TweetsData).IndexOfName('delete', false);
            TweetData := TJSONObject(TweetsData.Items[i]); //Contains status object
            TweetData := TweetData.Objects['status'];
            SetLength(Result, 1); //Single, deleted tweet
            TweetCounter := 0;
            ParseDeletion(TweetData); //Contains id,user_id...
          except
            {$IFDEF OAUTHTEST}
            FOAuth.Log(
              'ProcessTweets: something went wrong reporting about a deleted tweet. Check JSON code. JSON data:' +
              LineEnding + JSONTweets);
            {$ENDIF OAUTHTEST}
          end;
        end;
        Uninterested:
        begin
          {$IFDEF OAUTHTEST}
          FOAuth.Log('ProcessTweets: processing uninterested: JSON data:' + LineEnding + JSONTweets);
          {$ENDIF OAUTHTEST}
          exit;
        end;
        Unknown:
        begin
          {$IFDEF OAUTHTEST}
          FOAuth.Log('ProcessTweets: processing unknown: ' + LineEnding + JSONTweets);
          {$ENDIF OAUTHTEST}
          exit;
        end;
        else
        begin
          {$IFDEF OAUTHTEST}
          FOAuth.Log('ProcessTweets: processing other: ' + LineEnding + JSONTweets);
          {$ENDIF OAUTHTEST}
          exit;
        end;
      end;
    finally
      TweetsData.Free;
      FreeAndNil(Parser);
    end;
  except
    on E: Exception do
    begin
      // Unfortunately huge measures to deal with FPJSON generating exceptions
      SetLength(Result, 0);
      {$IFDEF OAUTHTEST}
      FOAuth.Log('ProcessTweets: parsing exception: ' + E.ClassName + '/' + E.Message);
      {$ENDIF OAUTHTEST}
    end;
  end;
end;

procedure TTwitter.SetAuthSecret(AValue: string);
begin
  FOauth.AuthSecret := AValue;
end;

procedure TTwitter.SetAuthToken(AValue: string);
begin
  FOauth.AuthToken := AValue;
end;

procedure TTwitter.SetConsumerKey(AValue: string);
begin
  FOAuth.ConsumerKey := AValue;
end;

procedure TTwitter.SetConsumerSecret(AValue: string);
begin
  FOAuth.ConsumerSecret := AValue;
end;

procedure TTwitter.SetThePINFunction(AValue: TGetPINFunction);
begin
  FOAuth.GetPINFunction := AValue;
end;

function TTwitter.GetTweets(const URL: string): TTweetsArray;
var
  RealURL: string;
begin
  SetLength(Result, 0);
  if not (FConnected) then
    if not (Connect) then
      exit;
  // Use user's timeline by default:
  if URL = '' then
    RealURL := TwitterAPIURL + '/1/statuses/home_timeline.json'
  else
    RealURL := URL;
  if FOAuth.OAuthHTTPMethod('GET', RealURL) = 200 then
  begin
    {$IFDEF OAUTHTEST}
    FOAuth.Log('Tweets body:');
    FOauth.Log(FOAuth.ReceivedBody);
    {$ENDIF OAUTHTEST}
    Result := ProcessTweets(FOAuth.ReceivedBody);
  end;
  ProcessHeaders;
end;

function TTwitter.GetTweetStream(Handler: TReceiveHandler): integer;
var
  ResultCode: integer;
  URL: string;
begin
  Result := 0;
  if not (FConnected) then
    if not (Connect) then
      exit;
  // todo: check into getting gzip compressed stream (requires HTTP 1.1):
  // set header Accept-Encoding: deflate, gzip
  // Do NOT set connection: close=>keepalive to yes
  // check the Content-Encoding header when you get stuff back
  // http://dev.twitter.com/docs/streaming-apis/processing#gzip-compression

  // http://dev.twitter.com/docs/api/1/get/statuses/sample
  // Note: not based on api.twitter.com!

  // We break on #13#10; only #10s will occur in Tweet JSON data.
  // Alternatively, use the length returned by delimited to load and display blocks of tweets
  // http://dev.twitter.com/docs/streaming-apis/processing

  // You can check if (your UTC timestamp-latest tweet timestamp) is increasing to see if you're falling behind
  // Alternatively use stall warnings to warn us if falling behind - good for diagnostics anyway:
  // http://dev.twitter.com/docs/streaming-apis/parameters#stall_warnings
  URL := 'https://stream.twitter.com/1/statuses/sample.json?stall_warnings=true';
  // Set up redirection of our stream to callback function:
  FOAuth.ReceiveHandler := Handler;
  ResultCode := FOAuth.OAuthHTTPMethod('GET', URL);
  Result := ResultCode;
  // 200: writeln('Succes.');
  // 401: writeln('Unauthorized');
  // 420: writeln('Throttled.');
  FOAuth.ReceiveHandlerClear;
end;

function TTwitter.GetUTCTime: TDateTime;
begin
  Result := CurrentUTCTime;
end;

function TTwitter.HTTPMethod(const Method, URL: string; var DocumentString: string): integer;
begin
  Result := 0;
  if not (FConnected) then
    if not (Connect) then
      exit;
  FOauth.SendBody := DocumentString;
  Result := FOauth.OAuthHTTPMethod(Method, URL);
  DocumentString := FOauth.ReceivedBody;
  ProcessHeaders;
end;


function TTwitter.Tweet(Message: string): boolean;
  // Tweet a message/update status. See:
  // http://dev.twitter.com/docs/api/1/post/statuses/update
var
  URL: string;
begin
  if not (FConnected) then
    if not (Connect) then
      exit;

  URL := TwitterAPIURL + '/1/statuses/update.json';
  FOAuth.SendBody := ('status=' + EncodeURLElementRFC3896(Message));
  // A 403 can indicate a duplicate tweet... or tweet limit reached. So return false to indicate status not updated
  Result := (FOauth.OAuthHTTPMethod('POST', URL) = 200);

  ProcessHeaders;
end;

function TTwitter.TweetAndSee(Message: string): TTweetsArray;
// Tweet a message/update status. See:
// http://dev.twitter.com/docs/api/1/post/statuses/update
var
  URL: string;
begin
  SetLength(Result, 0);
  if not (FConnected) then
    if not (Connect) then
      exit;

  URL := TwitterAPIURL + '/1/statuses/update.json';
  FOAuth.SendBody := ('status=' + EncodeURLElementRFC3896(Message));
  // A 403 can indicate a duplicate tweet... or tweet limit reached. So return false to indicate status not updated
  if FOauth.OAuthHTTPMethod('POST', URL) = 200 then
    Result := ProcessTweets(FOAuth.ReceivedBody);

  ProcessHeaders;
end;

function TTwitter.Connect: boolean;
var
  Parser: TJSONParser;
  ReturnObject: TJSONObject;
  URL: string;
begin
  FConnected := false; //Try to connect regardless of current status
  URL := TwitterAPIURL + '/1/account/verify_credentials.json' + '?skip_status=1';
  // Returns JSON object with e.g. name (string), id (numeric), screen_name (string)
  FOAuth.FallBackOOB := true; //If any tokens don't work, fallback to PIN auth
  if (FOauth.OAuthHTTPMethod('GET', URL) = 200) then
  begin
    Parser := TJSONParser.Create(FOAuth.ReceivedBody);
    try
      try
        ReturnObject := TJSONObject(Parser.Parse);
        FScreenName := ReturnObject.Strings['screen_name'];
        FOAuth.UserID := IntToStr(ReturnObject.Int64s['id']);
        FConnected := true;
      except
        FScreenName := '';
        FOAuth.UserID := '';
        FConnected := false;
      end;
    finally
      ReturnObject.Free;
      FreeAndNil(Parser);
    end;
    ProcessHeaders;
  end;
  FOAuth.FallBackOOB := false;
  Result := FConnected;
end;

procedure TTwitter.Disconnect;
var
  URL: string;
begin
  FConnected := false; //Regardless of actual outcome
  // See:
  // https://dev.twitter.com/docs/api/1/post/account/end_session
  URL := TwitterAPIURL + '/1/account/end_session.json';
  // This will give a 401 on read only applications, but no matter:
  FOauth.OAuthHTTPMethod('POST', URL);
end;

constructor TTwitter.Create;
begin
  inherited Create;
  FOauth := TOAuth1.Create;
  FOAuth.AccessTokenResource := '/oauth/access_token';
  FOAuth.AcquirePINPath:='/oauth/authorize?oauth_token=';
  FOAuth.BaseURL := TwitterAPIURL;
  FOAuth.RequestTokenResource := '/oauth/request_token';
  //For now, only support PIN auth and preauthenticated credentials:
  FOauth.CallBackURL := 'oob';
  FConnected := false;
end;

destructor TTwitter.Destroy;
begin
  FOauth.Destroy;
  inherited Destroy;
end;

end.
