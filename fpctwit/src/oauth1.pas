unit oauth1;

{ Authorization via OAuth 1.0a protocol, e.g. for Twitter, Plurk, LinkedIn, DropBox, Google. Yahoo
  Written for FreePascal; can be adapted for different network libraries; Synapse adapter included.
  Should be easily modified for Delphi use; patches welcome.

  Transparantly authenticates against OAuth1 server.

  Natively, only supports PIN based/OOB authentication (so it can be used on the command line,
  not requiring a browser.
  Can be inherited by a GUI version that does browser authentication.

  Logic flow (assuming a Twitter application):
  Note: FAuthToken/FAuthSecret content can vary over time; containing..., a request token, an access token etc.
  All requests are signed using the various tokens and should use HTTPS (TLS).
  Some providers may accept regular HTTP traffic instead of HTTPS.

  Using this unit:
  -  Set up your network transport type define, and relevant libraries (see those units).
     Call the chosen unit in the implementation section below.
  -  Register your application as an OAuth consumer, e.g. via http://dev.twitter.com/apps
     You get consumer key and consumer secret; if you want also a oauth_token and oauth_secret.
  -  If using PIN/OOB authentication, register your TGetPINFunction with the GetPINFunction property,
     set up the AcquirePINPath property to the correct URL path path+query part.
  -  If not using PIN/OOB authentication, or if the implementation in this unit does not work for your provider,
     override the Verifier function in your own unit.
  -  Set ConsumerKey, ConsumerSecret. You should have these (or your provider should be fine with empty values,
     which seems unlikely).
  -  Set AuthToken, AuthSecret if you have them.
  -  Set BaseURL to the authentication host

  Authentication flow within this unit (using PIN/OOB authentication):
  -  If consumer key/secret and auth token/secret are available, try to authenticate with these existing credentials first
  -  Using the oauth_consumer_key and oauth_consumer_secret, get a request token via http://twitter.com/oauth,
     including oauth_callback=oob as one of the parameters
  -  This returns a link in the HTML body; present to user and let him get a PIN from there.
  -  Get the PIN from the user.
  -  Passing PIN and request token, request an access token
  -  This returns oauth_token and oauth_token_secret; we're now authorized.
  -  Generated oauth_header using oauth_token oauth_token_secret, consumer_key and consumer_secret when asking for resources

  All these requests are signed using HMAC-SHA1 signature (OAuth also supports plaintext protocol)

}
{
  Copyright (c) 2012 Reinier Olislagers

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

{$i oauth.inc}

interface


uses
  Classes, SysUtils,dateutils;

const
  MinSecondsClockCorrect=5; // Only apply correction to local clock if apparent difference between local and server clock larger than this value
  // We only support OAuth 1.0a in this unit.
  // 1.0 is deprecated; 2.0 is a completely different protocol
  oauth_version = '1.0';
  oauth_signature_method = 'HMAC-SHA1';

type
  EOauth1Error = class(Exception);


  // Callback function for getting a PIN based on user going to URL
  TGetPINFunction = function(URL: string): string;

  // Callback function that handles streaming results.
  // Return false if you want to stop processing; true if you want to continue
  TReceiveHandler = function(ReceiveText: string): boolean of object;

  TOAuth1 = class; //forward so we can refer to it in TFiddeHTTPSend

  { TOAuthTransport }
  // General, network library agnostic transport for OAuth-enabled traffic
  // Some utility functions have been added as class functions. These are used
  // both within the OAuth unit and can be used by library users (see e.g. the
  // twitter library).
  // They are exported to external users in the interface section as functions
  // so internal implementation in this unit can be kept independent and consumers
  // will need to work only with TOAuth1 objects.
  TOAuthTransport = class(TObject)
  protected
    FMimeType: string;
    FReceivedBody: string;
    FReceivedHeaders: TStringList; //Headers of received document
    FReceiveHandler: TReceiveHandler;
    FResultCode: integer;
    FSendBody: string;
    procedure SetReceiveHandler(AValue: TReceiveHandler);virtual;
  public
    // UTC/GMT time at this moment; can be used as a class function.
    class function CurrentUTCTime: TDateTime;virtual;abstract;
    // Convert RFC822/RFC1123 date/time string into TDateTime
    class function DecodeRFC1123DateTime(DateTimeString: string): TDateTime;virtual;abstract;
    // Decode % encoded URL or data
    class function DecodeURL(URL: string): string;virtual;abstract;
    // Encode string with base64 //todo: replace with native FPC/Delphi code?
    class function EncodeBase64(Source: string): string;virtual;abstract;
    // Encode HTTP URL parameter names or values using % encoding
    // See http://dev.twitter.com/docs/auth/percent-encoding-parameters
    class function EncodeURLElementRFC3896(Source: string): string;virtual;abstract;
    // Calculate a HMAC_SHA1 hash/signature with source and key
    class function HMAC_SHA1(Source, key: string): string;virtual;abstract;
    // Split URL into URL and parameter part
    class procedure ParseURL(var URL, Parameters: string);virtual;abstract;
    // Insert or replace header with HeaderContent
    procedure SetHeader(HeaderName, HeaderContent: string);virtual;abstract;
    // Perform a GET http request
    function Get(RequestURL: string):boolean;virtual;abstract;
    // Perform a POST http request
    function Post(RequestURL: string):boolean;virtual;abstract;
    property MimeType: string write FMimeType;
    // HTTP body/document returned in HTTP response
    property ReceivedBody: string read FReceivedBody;
    // Headers in received HTTP document
    property ReceivedHeaders: TStringList read FReceivedHeaders;
    // If assigned, the incoming response data will be sent to the handler.
    // Useful for providing support for reading streams in a HTTP response body
    property ReceiveHandler: TReceiveHandler read FReceiveHandler write SetReceiveHandler;
    // HTTP result code that is returned after e.g. a get or post request
    property ResultCode: integer read FResultCode;
    // HTTP body/document to be sent in HTTP request.
    property SendBody: string read FSendBody write FSendBody;
    constructor Create(Parent: TOAuth1);
    destructor Destroy; override;
  end;

  { TOAuth1 }
  TOAuth1 = class(TObject)
  private
    FAccessTokenResource: string;
    FAcquirePINPath: string;
    // URL to where we send authentication requests
    FBaseURL: string;
    // Whether or not the application/consumer has been authenticated
    FAuthenticated: boolean;
    // Parameters that the service provider returns during the authentication process:
    FAuthReturnParameters: TStringList;
    FCallBackURL: string;

    FFallBackOOB: boolean;
    FGetPINFunction: TGetPINFunction;
    FMimeType: string;
    {$IFDEF OAUTHTEST}
    FLogFile: Text; //Log output to file
    {$ENDIF OAUTHTEST}
    Foauth_token_secret: string;
    Foauth_token: string;
    Foauth_consumer_key: string;
    Foauth_consumer_secret: string;
    // Contains additional oauth-specific parameters in name=value format (apart from the minimal 7 used in signature calculation)
    FMoreOauthParameters: TStringList;
    FProxyHost: string;
    FProxyPass: string;
    FProxyPort: string;
    FProxyUser: string;
    FReceiveHandler: TReceiveHandler;
    FRequestTokenResource: string;
    FUserID: string;
    // Complete authorization header, used for any request to the server:
    function GetAuthorizationHeader(NormalizedURL, Body: string; RequireAuthToken: boolean; Method: string): string;
    procedure SetAccessTokenResource(AValue: string);
    procedure SetAcquirePINPath(AValue: string);
    procedure SetClockCorrection(const RequestTimestamp: TDateTime;const DateHeader: string);
    procedure SetRequestTokenResource(AValue: string);
  protected
    FClockCorrection: TDateTime; //Correction against local clock, determined from remote server.
    FReceivedHeaders: TStringList; //Headers of received document
    FReceivedBody: string; //Body of received document
    FSendBody: string; //Body of document to be sent to server
    // Gets auth token etc. Calls GetAccessToken
    function Authenticate: boolean;
    // Sorts and encodes parameters (in name=value pairs), appends them, ready for use in an URL or POST form data field
    // Opposite of SplitDecodeAmpersandConcatString
    // Note: if UseDoubleQuoteComma: quote values with double quotes and use comma and space instead of &
    function JoinEncodeConcatString(ParameterList: TStringList; UseDoubleQuoteComma: boolean): string;
    // Splits & concatenated string (e.g. parameters in a HTML body or URL parameter) into a stringlist
    procedure SplitAmpersandConcatString(const InputText: string; var OutputText: TStringList);
    // Does what SplitAmpersandConcatString does but also decodes name=value pairs so you get the plain text parameters.
    procedure SplitDecodeAmpersandConcatString(const InputText: string; var OutputText: TStringList);
    // Get auth token/secret. This implementation uses Out Of Band/OOB
    // PIN as that is the only feasible approach when using a command line
    // client. Note children could override this and implement e.g. 3 legged auth
    function Verifier: string; virtual;
    // Use PIN/OOB to get access token
    // Set the AcquirePinPath property in advance.
    function VerifierViaOOB: string;
    // A unique value used to identify requests.
    // Override in descendents to test (e.g. generate the same value every time)
    function Nonce: string; virtual;
    // Get timestamp based on current UTC time
    // Override in descendents to test (e.g. generate the same value every time)
    function TimeStamp: string; virtual;
  public
    // Part of the URL used to ask for an access token
    // Can be specified with or without a starting /; the code will insert one if not present.
    property AccessTokenResource: string read FAccessTokenResource write SetAccessTokenResource;
    // Part of the path in the URL needed to get a PIN.
    // Used in VerifierViaOOB.
    // Can be specified with or without a starting /; the code will insert one if not present.
    property AcquirePINPath: string write SetAcquirePINPath;
    // Parameters returned by provider during authorization
    property AuthReturnedParameters: TStringList read FAuthReturnParameters;
    // Authentication token that identifies session
    property AuthToken: string read Foauth_token write Foauth_token;
    // Accompanies authentication token; keep secret
    property AuthSecret: string read Foauth_token_secret write Foauth_token_secret;
    // URL to which request is directed without query string, parameters.
    // For twitter, please use https:// (which requires SSL/TLS support)
    // Don't specify a trailing / at the end.
    // Example: https://api.twitter.com
    property BaseURL: string read FBaseURL write FBaseURL;
    // URL to which Oauth supplier redirects user's web browser.
    // In case of OOB/PIN authentication, set to 'oob'
    property CallBackURL: string read FCallBackURL write FCallBackURL;
    // Correction to local clock given remote server. Approximate; only set if > 5 seconds difference.
    property ClockCorrection: TDateTime read FClockCorrection;
    // Key that identifies the application
    property ConsumerKey: string read Foauth_consumer_key write Foauth_consumer_key;
    // Accompanies consumer key; keep secret
    property ConsumerSecret: string read Foauth_consumer_secret write Foauth_consumer_secret;
    // If set, OAuthHTTPMethod will try OOB after authorisation failure.
    // Default false: assume caller knows what he is doing. Error messages are returned in body.
    property FallBackOOB: boolean read FFallBackOOB write FFallBackOOB;
    // Assign this if you are using OOB authentication in your application.
    // You will get a URL; direct the user to this URL and have him enter the PIN that is shown.
    // Return this PIN
    property GetPINFunction: TGetPINFunction read FGetPINFunction write FGetPINFunction;
    // Convenience procedure that adds messages to our log; useful when debugging an entire stack
    // Note: only does anything if OAUTHTEST is defined.
    procedure Log(Message: string);

    // HTTP GET or POST with built in TOAuth1 authentication
    // Returns 0 on error; HTTP result code otherwise (e.g. 200 for OK)
    // This is the function that does the actual information retrieval for the user
    // POST method only: use SendDocumentString before calling this if needed.
    // Use ReceiveDocumentString after calling this if needed.
    function OAuthHTTPMethod(const Method, URL: string): integer;

    // Mimetype. Reset after every call to OAuthHTTPMethod
    property MimeType: string read FMimeType write FMimeType;
    // Address of proxy server (IP address or domain name), if any, that
    // you want to connect through
    property ProxyHost: string read FProxyHost write FProxyHost;
    // Port number of proxy server, if any, that you want to connect through
    property ProxyPort: string read FProxyPort write FProxyPort;
    // Username for proxy server, if any, that you want to connect through
    property ProxyUser: string read FProxyUser write FProxyUser;
    // Password for proxy server, if any, that you want to connect through
    property ProxyPass: string read FProxyPass write FProxyPass;
    // Stringlist with HTTP document/HTTP body received from server.
    property ReceivedBody: string read FReceivedBody;
    // Headers in received HTTP document
    property ReceivedHeaders: TStringList read FReceivedHeaders;

    // Set this to divert return traffic to your own handler instead of ReceivedDocument etc.
    property ReceiveHandler: TReceiveHandler read FReceiveHandler write FReceiveHandler;
    // Remove receive handler so result traffic is no longer diverted
    procedure ReceiveHandlerClear;
    // Part of the URL used to ask for a request token
    // Can be specified with or without a starting /; the code will insert one if not present.
    property RequestTokenResource: string read FRequestTokenResource write SetRequestTokenResource;
    // String with HTTP document/HTTP body to be sent to server.
    // Only useful/supported in POST request; not supported in GET requests.
    property SendBody: string read FSendBody write FSendBody;
    // User ID as known by provider
    property UserID: string read FUserID write FUSerID;

    constructor Create;
    destructor Destroy; override;
  published
  end;

  {$IFDEF OAUTHTEST}
  { OAuth1Test }
  // This tests OAuth1 calculations with some predefined values
  OAuth1Test = class(TOAuth1)
  private
    FTestNonce: string;
    FTestTimeStamp: string;
    procedure TestSignatureCalc;
  protected
    function Nonce: string; override;
    function TimeStamp: string; override;
  public

    constructor Create;
    destructor Destroy; override;
  end;
  {$ENDIF OAUTHTEST}

  // Functions provided by network transport layers or FPC/Delphi libraries.
  // Can be used by/useful for consumers.

  // Get current UTC/GMT time
  function CurrentUTCTime: TDateTime;
  // Decode an RFC822/RFC1123 string like Sun, 06 Nov 1994 08:49:37 GMT
  // Useful for getting date out of HTTP headers etc.
  function DecodeRFC1123DateTime(DateTimeString: string): TDateTime;
  // Decode percent-encoded URL string data
  function DecodeURL(URL: string): string;
  // Encode HTTP URL parameter names or values using % encoding
  // See http://dev.twitter.com/docs/auth/percent-encoding-parameters
  function EncodeURLElementRFC3896(Source: string): string;
  // Encode string with base64 //todo: replace with native FPC/Delphi code?
  function EncodeBase64(Source: string): string;
  // Calculate a HMAC_SHA1 hash/signature with source and key
  function HMAC_SHA1(Source, key: string): string;
  // Split URL into URL and parameter part
  procedure ParseURL(var URL, Parameters: string);


implementation

{$IFDEF TRANSPORT_SYNAPSE}
// Needed for reference to TOauth class, as well as utility functions below
// If you want to change transport layer, add your own reference to TOAuthTransportCustom
// and change the defines in oauth.inc
uses oauth1synapse;
{$ELSE}
// Generate an error to let the developer know that he hasn't chosen a correct transport layer
{$ERROR No valid network transport layer chosen. Please add your network layer reference to implementation uses section with compiler constant!}
{$ENDIF TRANSPORT_SYNAPSE}

function EncodeURLElementRFC3896(Source: string): string;
begin
  result:=TOAuthTransportCustom.EncodeURLElementRFC3896(Source);
end;

function EncodeBase64(Source: string): string;
begin
  result:=TOAuthTransportCustom.EncodeBase64(Source);
end;

function HMAC_SHA1(Source, key: string): string;
begin
  result:=TOAuthTransportCustom.HMAC_SHA1(Source, key);
end;

procedure ParseURL(var URL, Parameters: string);
begin
  TOAuthTransportCustom.ParseURL(URL, Parameters);
end;

function CurrentUTCTime: TDateTime;
begin
  result:=TOAuthTransportCustom.CurrentUTCTime;
end;

function DecodeRFC1123DateTime(DateTimeString: string): TDateTime;
begin
  result:=TOAuthTransportCustom.DecodeRFC1123DateTime(DateTimeString);
end;

function DecodeURL(URL: string): string;
// Decode %encoded URL etc
begin
  result:=TOAuthTransportCustom.DecodeURL(URL);
end;

function DictionarySort(List: TStringList; Index1, Index2: integer): integer;
  // Sort on Name part (of a Name=Value pair), instead of the entire string.
  // Useful in dictionary type/lookup TStringLists
var
  First: string;
  Second: string;
begin
  // Sort on name part
  First := List.Names[Index1];
  Second := List.Names[Index2];
  if First = Second then
  begin
    // Subsort on value part
    if List.CaseSensitive then
      Result := CompareStr(List.ValueFromIndex[Index1], List.ValueFromIndex[Index2])
    else
      Result := CompareText(List.ValueFromIndex[Index1], List.ValueFromIndex[Index2]);
  end
  else
  begin
    if List.CaseSensitive then
      Result := CompareStr(First, Second)
    else
      Result := CompareText(First, Second);
  end;
end;


{ TOAuthTransport }

procedure TOAuthTransport.SetReceiveHandler(AValue: TReceiveHandler);
begin
  if FReceiveHandler=AValue then Exit;
  FReceiveHandler:=AValue;
end;

constructor TOAuthTransport.Create(Parent: TOAuth1);
begin
  inherited Create;
  FReceivedHeaders:=TStringList.Create;
  FReceivedHeaders.NameValueSeparator := ':'; // Instead of =; useful for finding headers
end;

destructor TOAuthTransport.Destroy;
begin
  FReceivedHeaders.Free;
  inherited Destroy;
end;



{ TOAuth1 }

function TOAuth1.JoinEncodeConcatString(ParameterList: TStringList; UseDoubleQuoteComma: boolean): string;
var
  i: integer;
begin
  Result := '';
  // % encode every key and value (but not the =).
  // (if UseDoubleQuoteComma specified) surround value with "s
  // normally add & to every one but the last.
  for i := 0 to ParameterList.Count - 1 do
  begin
    // Encode the value parts, and concatenate them with &
    if UseDoubleQuoteComma then
      Result := Result + TOAuthTransportCustom.EncodeURLElementRFC3896(ParameterList.Names[i]) + '="' + TOAuthTransportCustom.EncodeURLElementRFC3896(
        ParameterList.ValueFromIndex[i]) + '"'
    else
      Result := Result + TOAuthTransportCustom.EncodeURLElementRFC3896(ParameterList.Names[i]) + '=' + TOAuthTransportCustom.EncodeURLElementRFC3896(ParameterList.ValueFromIndex[i]);
    if i < ParameterList.Count - 1 then
      if UseDoubleQuoteComma then
        Result := Result + ', '
      else
        Result := Result + '&';
  end;
end;

procedure TOAuth1.SplitAmpersandConcatString(const InputText: string; var OutputText: TStringList);
{ Splits something like:
oauth_token=NPcudxy0yU5T3tBzho7iCotZ3cnetKwcTIRlX0iwRl0&oauth_token_secret=veNRnAWe6inFuo8o2u8SLLZLjolYDmDP7SzL0YfYI&
oauth_callback_confirmed=true
into stringlist, splitting on the & signs and discarding them:
oauth_token=NPcudxy0yU5T3tBzho7iCotZ3cnetKwcTIRlX0iwRl0
oauth_token_secret=veNRnAWe6inFuo8o2u8SLLZLjolYDmDP7SzL0YfYI
oath_callback_confirmed=true

It only parses the &s, doesn't DecodeURL the contents; use SplitDecodeAmpersandConcatString for that.
}
var
  InputLines: TStringList;
  SourceLine, SourceElement: integer;
  TempList: TStringList;
begin
  OutputText.Clear;
  InputLines:=TStringList.Create;
  TempList := TStringList.Create;
  try
    InputLines.Text:=InputText;
    TempList.Delimiter := '&';
    TempList.StrictDelimiter := true; //only break on &
    for SourceLine := 0 to InputLines.Count - 1 do
    begin
      TempList.DelimitedText := InputLines[SourceLine];
      for SourceElement := 0 to TempList.Count - 1 do
      begin
        if TempList[SourceElement] <> '' then
          OutputText.Add(TempList[SourceElement]);
      end;
    end;
  finally
    InputLines.Free;
    TempList.Free;
  end;
end;

procedure TOAuth1.SplitDecodeAmpersandConcatString(const InputText: string; var OutputText: TStringList);
begin
  OutputText.Clear;
  // Shouldn't matter whether we decode everything at once or piece by pieces
  SplitAmpersandConcatString(TOAuthTransportCustom.DecodeURL(InputText), OutputText);
end;

function TOAuth1.GetAuthorizationHeader(NormalizedURL, Body: string; RequireAuthToken: boolean; Method: string): string;
var
  AllParameters: TStringList;
  OurNonce: string;
  OurSignature: string;
  OurTimeStamp: string;
  Para: string;

  function SignatureBaseString: string;
    // URLParams: any parameters specified in URL (after the ?)

    //See e.g.
    //http://dev.twitter.com/docs/auth/creating-signature
    //http://tools.ietf.org/html/rfc5849#section-3.4.1
    //on how this basestring is to be constructed
    //Test result e.g. on http://quonos.nl/oauthTester/
  var
    DecodedParams: TStringList; // The unencoded/plain text version of the EncodedParams
    SignatureParameters: TStringList;
    ParameterString: string; // Naming follows twitter example
  begin
    // Completeness check/initialization:
    if Foauth_consumer_key = '' then
      raise EOauth1Error.Create('Consumer key may not be empty.');
    // If we're in the process of getting an auth token, it's better to not require it to
    // avoid Catch 22 situations ;)
    if RequireAuthToken then
    begin
      if Foauth_token = '' then
        raise EOauth1Error.Create('Oauth token/Authentication token may not be empty.');
    end;

    SignatureParameters := TStringList.Create;
    try
      // List of name=value strings. Sort (later on) on name/key only.
      // OAuth 1.0a doesn't allow duplicates:
      // http://tools.ietf.org/html/rfc5849#section-3.1
      // We use the regular sorted as a way to detect this.
      SignatureParameters.Duplicates := dupError;
      SignatureParameters.Sorted := true;

      // Add any extra oauth parameters:
      SignatureParameters.AddStrings(FMoreOauthParameters);
      // ... as well as the minimum required ones - if present:
      SignatureParameters.Add('oauth_consumer_key=' + Foauth_consumer_key);
      SignatureParameters.Add('oauth_nonce=' + OurNonce);
      SignatureParameters.Add('oauth_signature_method=' + oauth_signature_method);
      SignatureParameters.Add('oauth_timestamp=' + OurTimeStamp);
      SignatureParameters.Add('oauth_token=' + Foauth_token);
      SignatureParameters.Add('oauth_version=' + oauth_version);

      // Add URL parameters
      DecodedParams := TStringList.Create;
      try
        TOAuthTransportCustom.ParseURL(NormalizedURL, Para);
        SplitDecodeAmpersandConcatString(Para, DecodedParams);
        {$IFDEF OAUTHTEST}
        Log('Debug: test: URL params decodedparams: ' + LineEnding + DecodedParams.Text + LineEnding +
          'Count: ' + IntToStr(DecodedParams.Count));
        {$ENDIF OAUTHTEST}
        SignatureParameters.AddStrings(DecodedParams);

        if (Body <> '') and (Uppercase(Method)='POST') and (FMimeType='') then
        begin
          // Add parameters specified in body (e.g. POST request)
          DecodedParams.Clear;
          {$IFDEF OAUTHTEST}
          Log('Debug: test: body params encoded: ' + LineEnding + Body);
          {$ENDIF OAUTHTEST}
          SplitDecodeAmpersandConcatString(Body, DecodedParams);
          {$IFDEF OAUTHTEST}
          Log('Debug: test: body params decoded: ' + LineEnding + 'Count: ' + IntToStr(DecodedParams.Count) +
            LineEnding + DecodedParams.Text);
          {$ENDIF OAUTHTEST}
          SignatureParameters.AddStrings(DecodedParams);
        end;
      finally
        DecodedParams.Free;
      end;
      // Sort, encode and concatenate all these parameters
      SignatureParameters.Sorted := false;
      SignatureParameters.CustomSort(@DictionarySort); //Sort by name part only
      ParameterString := JoinEncodeConcatString(SignatureParameters, false);
    finally
      SignatureParameters.Free;
    end;
    {$IFDEF OAUTHTEST}
    Log('Debug: Parameter string: ' + LineEnding + ParameterString);
    {$ENDIF OAUTHTEST}

    // Go from parameterstring to the signature base string
    // We should end up with two & signs in the result.
    Result := uppercase(Method) + '&' + TOAuthTransportCustom.EncodeURLElementRFC3896(NormalizedURL) + '&' + TOAuthTransportCustom.EncodeURLElementRFC3896(ParameterString);
    {$IFDEF OAUTHTEST}
    Log('Debug: Signature base string: ' + LineEnding + Result);
    {$ENDIF OAUTHTEST}
  end;

  function Signature: string;
  var
    SigningKey: string;
  begin
    //See e.g.
    //http://dev.twitter.com/docs/auth/creating-signature

    // In some flows, we don't yet have an oauth token secret; only add it if we do.
    if Foauth_token_secret = '' then
      SigningKey := TOAuthTransportCustom.EncodeURLElementRFC3896(Foauth_consumer_secret) + '&'
    else
      SigningKey := TOAuthTransportCustom.EncodeURLElementRFC3896(Foauth_consumer_secret) + '&' + TOAuthTransportCustom.EncodeURLElementRFC3896(Foauth_token_secret);
    {$IFDEF OAUTHTEST}
    Log('Signing key: ' + LineEnding + SigningKey);
    {$ENDIF OAUTHTEST}
    Result := TOAuthTransportCustom.EncodeBase64(TOAuthTransportCustom.HMAC_SHA1(SignatureBaseString, SigningKey));
    {$IFDEF OAUTHTEST}
    Log('Signature: ' + LineEnding + Result);
    {$ENDIF OAUTHTEST}
  end;

begin
  // See e.g.
  // http://dev.twitter.com/docs/auth/authorizing-request
  // but note it incorrectly states the authorization_header is always made up
  // of 7 oauth param=name values.
  // See http://dev.twitter.com/docs/auth/implementing-sign-twitter
  // all oauth param=name values should be included

  // Completeness check/initialization:
  if Foauth_consumer_key = '' then
    raise EOauth1Error.Create('Consumer key may not be empty.');

  // If we're in the process of getting an auth token, it's better to not require it to
  // avoid Catch 22 situations ;)
  if RequireAuthToken then
  begin
    if Foauth_token = '' then
      raise EOauth1Error.Create('Oauth token/Authentication token may not be empty.');
  end;

  // Get one-time/time-dependent values: timestamp and nonce
  OurTimeStamp := TimeStamp;
  {$IFDEF OAUTHTEST}
  Log('Current UTC time:       ' + TimeToStr(TOAuthTransportCustom.CurrentUTCTime) + LineEnding + 'Request timestamp:      ' +
    OurTimeStamp + LineEnding + 'Converted back:         ' + DateTimeToStr(UnixToDateTime(StrToInt(OurTimeStamp))) +
    LineEnding + 'Approximate local time: ' + TimeToStr(Now));
  {$ENDIF OAUTHTEST}
  OurNonce := Nonce;
  // Get signature based on oath header values (same as below) and parameters.
  OurSignature := Signature;

  // Signature depends on all oauth_ params that are present
  // and may include empty oauth_token values, see
  // http://tools.ietf.org/html/rfc5849#section-2.1
  // Encode parameter values according to RFC 3986, Section 2.1/2.4s.
  AllParameters := TStringList.Create;
  try
    // Enforce no duplicate parameters by using a custom sort... below.
    AllParameters.Sorted := true;
    AllParameters.Duplicates := dupError;
    AllParameters.Add('oauth_consumer_key' + '=' + Foauth_consumer_key);
    AllParameters.Add('oauth_nonce' + '=' + OurNonce);
    AllParameters.Add('oauth_signature' + '=' + OurSignature);
    AllParameters.Add('oauth_signature_method' + '=' + oauth_signature_method);
    AllParameters.Add('oauth_timestamp' + '=' + OurTimeStamp);
    AllParameters.Add('oauth_token' + '=' + Foauth_token);
    AllParameters.Add('oauth_version' + '=' + OAuth_Version);
    AllParameters.AddStrings(FMoreOauthParameters);
    AllParameters.Sorted := false;
    AllParameters.CustomSort(@DictionarySort); //Sort on Name part instead of entire string
    Result := 'OAuth ' + JoinEncodeConcatString(AllParameters, true);
  finally
    AllParameters.Free;
  end;
  {$IFDEF OAUTHTEST}
  Log('Debug: Authorization Header: ' + LineEnding + Result);
  {$ENDIF OAUTHTEST}
end;

procedure TOAuth1.SetAccessTokenResource(AValue: string);
begin
  if (AValue<>'') and (Copy(AValue,1,1)<>'/') then
    FAccessTokenResource:='/'+AValue
  else
    FAccessTokenResource:=AValue;
end;

procedure TOAuth1.SetAcquirePINPath(AValue: string);
begin
  if (AValue<>'') and (Copy(AValue,1,1)<>'/') then
    FAcquirePINPath:='/'+AValue
  else
    FAcquirePINPath:=AValue;
end;

procedure TOAuth1.SetClockCorrection(const RequestTimestamp: TDateTime;
  const DateHeader: string);
var
  ServerTimestamp: TDateTime;
begin
  // Only change clock correction if we have valid time data.
  if DateHeader<>'' then
  begin
    ServerTimestamp:=TOAuthTransportCustom.DecodeRFC1123DateTime(DateHeader);
    if Abs(SecondsBetween(RequestTimestamp, ServerTimestamp))>
      MinSecondsClockCorrect then
    begin
      // Calculate correction. Of course, we're ignoring request roundtrip time, but that
      // should be covered by MinSecondsClockCorrect
      FClockCorrection:=ServerTimeStamp-RequestTimestamp;
      {$IFDEF OAUTHTEST}
      Log('Debug: applied clock correction: '+TimeToStr(FClockCorrection)+ ' - as fraction: '+FloatToStr(FClockCorrection));
      {$ENDIF OAUTHTEST}
    end
    else
    begin
      FClockCorrection:=0;
    end;
  end;
end;

procedure TOAuth1.SetRequestTokenResource(AValue: string);
begin
  if (AValue<>'') and (Copy(AValue,1,1)<>'/') then
    FRequestTokenResource:='/'+AValue
  else
    FRequestTokenResource:=AValue;
end;

function TOAuth1.Authenticate: boolean;
var
  AuthHTTP: TOAuthTransport;
  RequestTimestamp: TDateTime;
  RequestURL: string; //URL used in each request below.
  Success: boolean;
  oauth_verifier: string;
begin
  // Fail by default:
  Result := false;
  FAuthenticated := false;
  Success := false;
  // see e.g.
  // http://dev.twitter.com/docs/auth/implementing-sign-twitter
  // Step 1: obtain request token:
  { OAuth 1.0a 6.1.1 parameters:
      oauth_consumer_key:
          The Consumer Key.
      oauth_signature_method:
          The signature method the Consumer used to sign the request.
      oauth_signature:
          The signature as defined in Signing Requests.
      oauth_timestamp:
          As defined in Nonce and Timestamp.
      oauth_nonce:
          As defined in Nonce and Timestamp.
      oauth_version:
          OPTIONAL. If present, value MUST be 1.0 . Service Providers MUST assume the protocol version to be 1.0 if this parameter is not present. Service Providersâ response to non-1.0 value is left undefined.
      Additional parameters:
          Any additional parameters, as defined by the Service Provider.
  }
  // Send regular signed message to POST /oauth/request_token
  // This is similar for all authentication methods
  FMoreOauthParameters.Clear;
  if (FCallBackURL = '') or (FCallBackURL = 'oob') then
  begin
    //http://dev.twitter.com/docs/auth/pin-based-authorization
    FMoreOauthParameters.Add('oauth_callback=oob');
    // We're trying to get an auth token/secret, so ignore any present:
    Foauth_token := '';
    Foauth_token_secret := '';
  end
  else
    FMoreOauthParameters.Add('oauth_callback=' + FCallBackURL);

  AuthHTTP := TOAuthTransportCustom.Create(Self);
  try
    if FBaseURL = '' then
      raise EOauth1Error.Create('Base URL may not be empty.');
    //todo: deal with non-default ports etc; we'll need to check/fix normalizedurl
    if FRequestTokenResource = '' then
      RequestURL := FBaseURL
    else
      RequestURL := FBaseURL + FRequestTokenResource;

    // Empty post, payload sits in Authorization header.
    AuthHTTP.MimeType := 'application/x-www-form-urlencoded';
    AuthHTTP.SetHeader('Authorization', GetAuthorizationHeader(RequestURL, '', false, 'POST'));
    {$IFDEF OAUTHTEST}
    Log('Going to call: ' + RequestURL);
    {$ENDIF OAUTHTEST}
    RequestTimestamp:=Now();
    Success := AuthHTTP.Post(RequestURL);
    SetClockCorrection(RequestTimeStamp, AuthHTTP.ReceivedHeaders.Values['Date']);

    if FClockCorrection<>0 then
    begin
      if (Success = false) or (AuthHTTP.ResultCode <> 200) then
      begin
        // Retry if clock is skewed
        AuthHTTP.MimeType := 'application/x-www-form-urlencoded';
        // Use corrected timestamp in auth header:
        AuthHTTP.SetHeader('Authorization', GetAuthorizationHeader(RequestURL, '', false, 'POST'));
        {$IFDEF OAUTHTEST}
        Log('Clock was wrong. Again going to call: ' + RequestURL);
        {$ENDIF OAUTHTEST}
        Success := AuthHTTP.Post(RequestURL);
      end;
    end;

    if (Success = false) or (AuthHTTP.ResultCode <> 200) then
    begin
      {$IFDEF OAUTHTEST}
      Log('Debug: auth failure; returned document body:' + LineEnding + AuthHTTP.ReceivedBody + LineEnding +
        'Debug: headers:' + LineEnding + AuthHTTP.ReceivedHeaders.Text);
      {$ENDIF OAUTHTEST}
      raise EOauth1Error.Create('Authentication failed in step 1: result code:' + IntToStr(AuthHTTP.ResultCode) +
        '; Provider response: ' + AuthHTTP.ReceivedBody);
      exit;
    end;
    // Clear up for the next call:
    FMoreOauthParameters.Clear;
    // If we got here, we have a 200 OK
    // Split along the & marks
    SplitAmpersandConcatString(AuthHTTP.ReceivedBody, FAuthReturnParameters);

    if FAuthReturnParameters.IndexOf('oauth_callback_confirmed=true') = -1 then
    begin
      raise EOauth1Error.Create('Authentication failed in step 1: no ouath_callback_confirmed=true received.');
      exit;
    end;
    // Overwrite any existing auth token
    Foauth_token := FAuthReturnParameters.Values['oauth_token'];
    if Foauth_token = '' then
    begin
      raise EOauth1Error.Create('Authentication failed in step 1: no oauth_token received.');
      exit;
    end;
    // Overwrite any existing auth token secret
    Foauth_token_secret := FAuthReturnParameters.Values['oauth_token_secret'];
    if Foauth_token_secret = '' then
    begin
      raise EOauth1Error.Create('Authentication failed in step 1: no oauth_token_secret received.');
      exit;
    end;
  finally
    AuthHTTP.Free;
  end;

  // Step 2: get oauth_verifier which we'll need later
  // to convert the request token into an access token
  oauth_verifier := Verifier;
  if oauth_verifier = '' then
  begin
    raise EOauth1Error.Create('Authentication failed in step 2: no oauth_verifier found.');
    exit;
  end;

  // Step 3: convert the request to an access token
  AuthHTTP := TOAuthTransportCustom.Create(Self);
  try
    if FBaseURL = '' then
      raise EOauth1Error.Create('Base URL may not be empty.');
    if FAccessTokenResource = '' then
      RequestURL := FBaseURL
    else
      RequestURL := FBaseURL + FAccessTokenResource;

    FMoreOauthParameters.Clear;
    FMoreOauthParameters.Add('oauth_verifier=' + oauth_verifier);
    // Use empty body - all parameters are OAuth parameters
    AuthHTTP.SetHeader('Authorization',GetAuthorizationHeader(RequestURL, '', false, 'POST'));
    {$IFDEF OAUTHTEST}
    Log('Going to call: ' + RequestURL);
    {$ENDIF OAUTHTEST}

    Success := AuthHTTP.Post(RequestURL);

    if (Success = false) or (AuthHTTP.ResultCode <> 200) then
    begin
      {$IFDEF OAUTHTEST}
      Log('Debug: auth failure; returned document body:' + LineEnding + AuthHTTP.ReceivedBody + LineEnding +
        'Debug: headers:' + LineEnding + AuthHTTP.ReceivedHeaders.Text);
      {$ENDIF OAUTHTEST}
      raise EOauth1Error.Create('Authentication failed in step 3: result code:' + IntToStr(AuthHTTP.ResultCode) +
        '; Provider response: ' + AuthHTTP.ReceivedBody);
      exit;
    end;

    // If we got here, we have a 200 OK
    {$IFDEF OAUTHTEST}
    Log('Debug: step 3: returned body:' + LineEnding + AuthHTTP.ReceivedBody + LineEnding + 'Debug: step 3: returned headers:' +
      LineEnding + AuthHTTP.ReceivedHeaders.Text);
    {$ENDIF OAUTHTEST}
    // Split along the & marks
    SplitAmpersandConcatString(AuthHTTP.ReceivedBody, FAuthReturnParameters);

    Foauth_token := FAuthReturnParameters.Values['oauth_token'];
    if Foauth_token = '' then
    begin
      raise EOauth1Error.Create('Authentication failed in step 3: no oauth_token received.');
      exit;
    end;
    // Overwrite any existing auth token secret with our validated auth secret
    Foauth_token_secret := FAuthReturnParameters.Values['oauth_token_secret'];
    if Foauth_token_secret = '' then
    begin
      raise EOauth1Error.Create('Authentication failed in step 3: no oauth_token_secret received.');
      exit;
    end;
    FUserID := FAuthReturnParameters.Values['user_id'];
  finally
    AuthHTTP.Free;
  end;
  FAuthenticated := true;
  Result := true;
end;

function TOAuth1.Nonce: string;
  // Number used once for use in requests
  // Override this in child class if you want to test with the same nonce every time
const
  NonceLength = 41;
  ValidChars = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
var
  i: integer;
begin
  // We could do what twitter does: get 32 random bytes and base64 encode them
  // However, less dependencies to just pick 41 random valid characters
  Result := StringOfChar('q', NonceLength);
  for i := 1 to NonceLength do
  begin
    Result[i] := ValidChars[Trunc((Length(ValidChars) * Random) + 1)];
  end;
end;

function TOAuth1.TimeStamp: string;
begin
  // Timestamp in Unix epoch, so we need UTC time to calculate.
  // We apply a correction factor if we have determined local and server clock are too far apart
  Result := IntToStr(DateTimeToUnix(TOAuthTransportCustom.CurrentUTCTime+FClockCorrection));
end;


procedure TOAuth1.Log(Message: string);
begin
  {$IFDEF OAUTHTEST}
  writeln(FLogFile, FormatDateTime('yyyymmddhhnnss', Now));
  writeln(FLogFile, Message);
  Flush(FLogFile);
  {$ENDIF OAUTHTEST}
end;

function TOAuth1.Verifier: string;
begin
  // In this implementation, get verifier using PIN/OOB.
  // Children can override this if they include e.g. a web browser
  Result := VerifierViaOOB;
end;

function TOAuth1.VerifierViaOOB: string;
var
  PIN: string;
begin
  // See e.g.
  // http://dev.twitter.com/docs/auth/pin-based-authorization
  // Fail by default:
  Result := '';
  if assigned(FGetPINFunction) = false then
    raise EOAuth1Error.Create('PIN callback must be assigned if using OOB authentication. Check your code.');
  if FAcquirePINPath='' then
    raise EOAuth1Error.Create('AcquirePINPath must not be empty. Check your code.');
  PIN := FGetPINFunction(FBaseURL + FAcquirePINPath + TOAuthTransportCustom.EncodeURLElementRFC3896(Foauth_token));
  if PIN = '' then
    raise EOauth1Error.Create('PIN may not be empty.');

  //The PIN code must be passed as the value for oauth_verifier
  // for a later POST oauth/access_token request.
  Result := PIN;
end;

function TOAuth1.OAuthHTTPMethod(const Method, URL: string): integer;
var
  HTTP: TOAuthTransport;
  RequestTimestamp: TDateTime;
begin
  {$IFDEF OAUTHTEST}
  // Indicate start of new request.
  Log('========================================================================');
  {$ENDIF OAUTHTEST}
  Result := 0;
  HTTP := TOAuthTransportCustom.Create(Self);
  FReceivedBody := ''; //Clear out in advance
  try
    // Put user's HTTP body into our object.
    HTTP.FSendBody:=FSendBody;
    HTTP.MimeType:=FMimeType;
    // Set receive handler if appropriate
    if Assigned(FReceiveHandler) then
      HTTP.ReceiveHandler := FReceiveHandler;
    // Try to use existing credentials
    if (Foauth_consumer_key <> '') and (Foauth_consumer_secret <> '') and (Foauth_token <> '') and (Foauth_token_secret <> '') then
    begin
      {$IFDEF OAUTHTEST}
      Log('Trying to authenticate with existing oauth_token and oauth_token_secret.' + LineEnding + Method +
        ' ' + URL + LineEnding + 'Body before sending:' + LineEnding + FSendBody);
      {$ENDIF OAUTHTEST}
      HTTP.SetHeader('Authorization',GetAuthorizationHeader(URL, FSendBody, false, Method));
      Result:=0;
      RequestTimestamp:=Now;
      case Method of
        'GET': if HTTP.Get(URL) then Result:=HTTP.ResultCode;
        'POST': if HTTP.Post(URL) then Result:=HTTP.ResultCode;
        else raise Exception.Create('Unsupported HTTP method '+Method);
      end;
      // Detect large differences in local and remote time, which will cause authentication to fail.
      // We set up a clock correction if necessary and use it in our subsequent calls.
      SetClockCorrection(RequestTimestamp, HTTP.ReceivedHeaders.Values['Date']);

      case Result of
        401:
        begin
          {$IFDEF OAUTHTEST}
          Log('Got a 401 when using current credentials.' + LineEnding +
            'Credentials may be invalid or you may have no permission to page.');
          {$ENDIF OAUTHTEST}
          if FFallBackOOB then
          begin
            Foauth_token := '';
            Foauth_token_secret := '';
            FAuthenticated := false;
          end;
        end
        else
        begin
          // Including 200, 404, server errors: just pass it on back to the user
          {$IFDEF OAUTHTEST}
          Log('Got result code: ' + IntToStr(HTTP.ResultCode));
          {$ENDIF OAUTHTEST}
          FAuthenticated := true;
          // Copy over return data if not handled otherwise
          if Assigned(FReceiveHandler) = false then
          begin
            FReceivedBody:=HTTP.ReceivedBody;
            FReceivedHeaders.Assign(HTTP.ReceivedHeaders);
          end;
        end;
      end;
    end;
  finally
    // reset MimeType for next call
    FMimeType:='';
    HTTP.Free;
  end;

  // First time, or previous attempt failed:
  // Note: we could add clock correction code here by doing a HEAD for the
  if FAuthenticated = false then
  begin
    // Make sure we're authenticated before continuing
    if Authenticate = false then
    begin
      raise EOauth1Error.Create('Could not authenticate.');
    end
    else
    begin
      HTTP := TOAuthTransportCustom.Create(Self);
      FReceivedBody := ''; //Clear out in advance
      try
        // Put user's HTTP body into our object. Could have used Synapse writestrtobuffer as well..
        HTTP.SendBody:=FSendBody;
        // Set receive handler if appropriate
        if Assigned(FReceiveHandler) then
          HTTP.ReceiveHandler := FReceiveHandler;
        // Replace any existing authorization header
        HTTP.SetHeader('Authorization',GetAuthorizationHeader(URL, FSendBody, false, Method));
        // Copy over return data if not handled otherwise
        Result:=0;
        case Method of
          'GET': if HTTP.Get(URL) then Result:=HTTP.ResultCode;
          'POST': if HTTP.Post(URL) then Result:=HTTP.ResultCode;
          else raise Exception.Create('Unsupported HTTP method '+Method);
        end;
        if Assigned(FReceiveHandler) = false then
        begin
          FReceivedBody:=HTTP.ReceivedBody;
          FReceivedHeaders.Assign(HTTP.ReceivedHeaders);
        end;
      finally
        HTTP.Free;
      end;
    end;
  end;
  FSendBody := ''; //Clean up for next call
end;

procedure TOAuth1.ReceiveHandlerClear;
begin
  FReceiveHandler := nil;
end;


constructor TOAuth1.Create;
begin
  inherited Create;
  FReceivedHeaders := TStringList.Create;
  FReceivedHeaders.NameValueSeparator := ':'; // Instead of =; useful for finding headers
  FAuthReturnParameters := TStringList.Create;
  FMoreOauthParameters := TStringList.Create;
  //Useful for i.e. Twitter; programmer's can set AccessTokenResource though if needed:
  FAccessTokenResource := '/oauth/access_token';
  FAcquirePINPath := '/oauth/authorize?oauth_token=';
  FAuthenticated := false;
  FCallBackURL := 'oob'; //Default to out of band authentication; no auth_token or auth_secret needed
  FFallBackOOB := false;
  //Useful for i.e. Twitter; programmer's can set AccessTokenResource though if needed:
  FRequestTokenResource := '/oauth/request_token';
  {$IFDEF OAUTHTEST}
  Assign(FLogFile, 'log.txt');
  Rewrite(FLogFile); //use this for output of debug messages etc.
  {$ENDIF OAUTHTEST}
  Randomize;
end;

destructor TOAuth1.Destroy;
begin
  {$IFDEF OAUTHTEST}
  try
    Close(FLogFile);
  except
    // Closing log file failed. Too bad, ignore it.
  end;
  {$ENDIF OAUTHTEST}
  FReceivedHeaders.Free;
  FAuthReturnParameters.Free;
  FMoreOauthParameters.Free;
  inherited Destroy;
end;

{$IFDEF OAUTHTEST}
{ OAuth1Test }

procedure OAuth1Test.TestSignatureCalc;
// We're following testing against the values on
// http://dev.twitter.com/docs/auth/creating-signature
const
  // We use this in our call (appended by parameters):
  TestBaseURL = 'https://api.twitter.com/1/statuses/update.json';
  // We expect these results:
  ExpectedBaseString = 'POST&https%3A%2F%2Fapi.twitter.com%2F1%2Fstatuses%2Fupdate.json&include_entities%3Dtrue%26oauth_consumer_key%3Dxvz1evFS4wEEPTGEFPHBog%26oauth_nonce%3DkYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg%26oauth_signature_method%3DHMAC-SHA1%26oauth_timestamp%3D1318622958%26oauth_token%3D370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb%26oauth_version%3D1.0%26status%3DHello%2520Ladies%2520%252B%2520Gentlemen%252C%2520a%2520signed%2520OAuth%2520request%2521';
  ExpectedSigningKey = 'kAcSOqF21Fu85e7zjz7ZN2U4ZRhfV3WpwPAoE3Z7kBw&LswwdoUaIvS8ltyTt5jkRh4J50vUPVVHtR2YPi5kE';
  ExpectedSignature = 'tnnArxj06cWHq44gCs1OSKk/jLY=';
begin
  // Trick parent in not doing anything:
  FAuthenticated := true;
  // Set up test variables for the signature base string:
  BaseURL := TestBaseURL;
  // POST form parameters in body:
  FSendBody := (TOAuthTransportCustom.EncodeURLElementRFC3896('status') + '=' + TOAuthTransportCustom.EncodeURLElementRFC3896(
    'Hello Ladies + Gentlemen, a signed OAuth request!'));
  writeln('Test: document body to be sent: ');
  writeln(FSendBody);
  Foauth_consumer_key := 'xvz1evFS4wEEPTGEFPHBog';
  // Force parent to use this nonce (should be written again before each call):
  FTestNonce := 'kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg';
  Foauth_token := '370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb';
  // Force parent to use this timestamp (should be written again before each call):
  FTestTimeStamp := '1318622958';
  //=>should result in TestBaseString

  // Test variables for the signature:
  Foauth_consumer_secret := 'kAcSOqF21Fu85e7zjz7ZN2U4ZRhfV3WpwPAoE3Z7kBw';
  Foauth_token_secret := 'LswwdoUaIvS8ltyTt5jkRh4J50vUPVVHtR2YPi5kE';
  //=> should result in TestSigningKey

  // Getting auth header should calculate everything and run debug output.
  GetAuthorizationHeader(BaseURL + '?include_entities=true', FSendBody, false, 'POST');

  writeln('Expected values: ');
  writeln('Expected base string:');
  writeln(ExpectedBaseString);
  writeln('Expected signing key:');
  writeln(ExpectedSigningKey);
  writeln('Expected signature:');
  writeln(ExpectedSignature);
end;

function OAuth1Test.Nonce: string;
begin
  if FTestNonce = '' then
    Result := inherited Nonce
  else
    Result := FTestNonce;
end;

function OAuth1Test.TimeStamp: string;
begin
  if FTestTimeStamp = '' then
    Result := inherited TimeStamp
  else
    Result := FTestTimeStamp;
end;


constructor OAuth1Test.Create;

begin
  inherited Create;
  TestSignatureCalc;
end;

destructor OAuth1Test.Destroy;
begin
  inherited Destroy;
end;

{$ENDIF OAUTHTEST}

end.
