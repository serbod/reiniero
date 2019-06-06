unit oauth1synapse;

{ Synapse-specific transport layer units for the oauth1 OAuth 1.0a classes.
  -  Make sure you have the Synapse sources (should be included with this unit),
     the synapse SSL plugin sources (e.g. ssl_openssl.pas & ssl_openssl_lib.pas) and
     the required SSL libraries (e.g. openssl) present/installed
  -  If you want to use another SSL library than openssl, change the define below

  Note: this code uses Synapse units from the Synapse trunk version. For your convenicence, these have been included with the source.
  Regular Synapse code will not work (you'll need to set THTTPSend.ReadUnknown to Virtual, and more).

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
// With synapse, use OpenSSL (default) or cryptlib?
{not $DEFINE TRANSPORT_SYNAPSE_CRYPTLIB}
{$DEFINE TRANSPORT_SYNAPSE_OPENSSL}

interface

// Make sure you have the required SSL libraries installed/in your (search) path!
// todo: implement error handling in case libs not found? Can we test for this?
uses
  Classes, SysUtils,
  synacode, synautil,
  {$IFDEF TRANSPORT_SYNAPSE_CRYPTLIB}ssl_cryptlib,{$ENDIF}
  {$IFDEF TRANSPORT_SYNAPSE_OPENSSL}ssl_openssl,{$ENDIF}
  httpsend, synsock {for constants used in TFiddleHTTPSend},
  oauth1,
  dateutils;

const
  RFC3896PercentEncodeChars: TSpecials =
    [
    //RFC3986 2.3 unreserved characters are always permitted:
    //unreserved  = [A..Z], [a..z], [0-9], '-', '.', '_', 'Ëœ'
    //RFC3986, 2.2 Reserved characters: Gen-delims
    ':', '#', '[', ']', '@', '/', '?',
    //RFC3986, 2.2 Reserved characters: sub-delims
    '!', '$', '&', '''', '(', ')', '*', '+', ',', ';', '=',
    //Everything not in gen-delims, sub-delims and unreserved characters...
    //... must be encoded:
    #$00..#$20, '<', '>', '"', '%', '{', '}', '|', '\', '^', '`', #$7F..#$FF];

type
  // Callback function for getting a PIN based on user going to URL
  TGetPINFunction = function(URL: string): string;

  // Callback function that handles streaming results.
  // Return false if you want to stop processing; true if you want to continue
  TReceiveHandler = function(ReceiveText: string): boolean of object;

  { TFiddleHTTPSend}
  // A Synapse THTTPSend descendent that allows us to manipulate a bit more
  // Used by the TOAuthTransportCustom class
  TFiddleHTTPSend = class(THTTPSend)
  private
    FReceiveHandler: TReceiveHandler;
  protected
    // Intended to override receiving data. Won't output to stream
    function ReadUnknown: boolean; override;
  public
    // If assigned, the incoming response data will be sent to the handler.
    // Useful for providing support for reading streams in a HTTP response body
    property ReceiveHandler: TReceiveHandler read FReceiveHandler write FReceiveHandler;
    // Pass the OAuth instance in order to assign proxy info etc
    constructor Create(OAuthInstance: TOAuth1);
    destructor Destroy; override;
  end;

  { TOAuthTransportCustom }
  // Synapse implementation of TOAuthTransport
  TOAuthTransportCustom = class(TOAuthTransport)
  protected
    FFiddle: TFiddleHTTPSend;
    procedure SetMimeType(AValue: string);
    procedure SetReceiveHandler(AValue: TReceiveHandler);override;
  public
    class function CurrentUTCTime: TDateTime;override;
    class function DecodeRFC1123DateTime(DateTimeString: string): TDateTime;override;
    class function DecodeURL(URL: string): string;override;
    class function EncodeBase64(Source: string): string;override;
    class function EncodeURLElementRFC3896(Source: string): string;override;
    class function HMAC_SHA1(Source, key: string): string;override;
    class procedure ParseURL(var URL, Parameters: string);override;
    property MimeType: string write SetMimeType;
    function Get(RequestURL: string):boolean;override;
    function Post(RequestURL: string):boolean;override;
    procedure SetHeader(HeaderName, HeaderContent: string);override;
    constructor Create(Parent: TOAuth1);
    destructor Destroy; override;
  end;

implementation


{ TOAuthTransportCustom }

procedure TOAuthTransportCustom.SetMimeType(AValue: string);
begin
  FFiddle.MimeType:=AValue;
end;

procedure TOAuthTransportCustom.SetReceiveHandler(AValue: TReceiveHandler);
begin
  inherited SetReceiveHandler(AValue);
  FFiddle.ReceiveHandler:=AValue;
end;

class function TOAuthTransportCustom.CurrentUTCTime: TDateTime;
begin
  // todo: we can use FPC trunk's UTC time function (LocalTimeToUniversal) here; perhaps Delphi has a comparable function
  result:=GetUTTime;
end;

class function TOAuthTransportCustom.DecodeRFC1123DateTime(
  DateTimeString: string): TDateTime;
begin
  result:=Synautil.DecodeRfcDateTime(DateTimeString);
end;

class function TOAuthTransportCustom.DecodeURL(URL: string): string;
begin
  result:=Synacode.DecodeURL(URL);
end;

class function TOAuthTransportCustom.EncodeBase64(Source: string): string;
begin
  // todo: perhaps FPC and/or Delphi has a comparable function
  Result := synacode.EncodeBase64(Source);
end;

class function TOAuthTransportCustom.EncodeURLElementRFC3896(Source: string
  ): string;
begin
  // We encode much more than the Synapse provided function.
  // todo: might be replacable by FPC's httpdefs unit; perhaps something similar in Delphi
  Result := EncodeTriplet(Source, '%', RFC3896PercentEncodeChars);
end;

class function TOAuthTransportCustom.HMAC_SHA1(Source, key: string): string;
begin
  Result := synacode.HMAC_SHA1(Source, key);
end;

class procedure TOAuthTransportCustom.ParseURL(var URL, Parameters: string);
var
  Path, Port, Host, Pass, User, Prot: string;
begin
  // We're only interested in the normalized URL and parameter part of the URL, not the rest:
  synautil.ParseURL(URL,Prot,User,Pass,Host,Port,Path,Parameters);
  if Parameters <> '' then //chop parameter part from url
    URL := copy(URL, 1, pos('?', URL) - 1);
end;

function TOAuthTransportCustom.Get(RequestURL: string):boolean;
begin
  FReceivedBody:='';
  FReceivedHeaders.Clear;
  // Could have used Synapse writestrtobuffer as well..
  if FSendBody<>'' then
    FFiddle.Document.WriteBuffer(Pointer(FSendBody)^, Length(FSendBody));
  result:=FFiddle.HTTPMethod('GET', RequestURL);
  FResultCode:=FFiddle.ResultCode;
  FFiddle.Document.Position := 0;
  FReceivedBody:=ReadStrFromStream(FFiddle.Document, FFiddle.Document.Size);
  FReceivedHeaders.Assign(FFiddle.Headers);
end;

function TOAuthTransportCustom.Post(RequestURL: string):boolean;
begin
  FReceivedBody:='';
  FReceivedHeaders.Clear;
  if FSendBody<>'' then
    FFiddle.Document.WriteBuffer(Pointer(FSendBody)^, Length(FSendBody));
  if (FSendBody <> '') then
  begin
    // Override existing mime type
    if (FMimeType = '') then
      FMimeType:='application/x-www-form-urlencoded';
    FFiddle.MimeType:=FMimeType;
  end
  else
    FFiddle.Headers.Add('Content-Length: 0'); //synapse skips this!!
  result:=FFiddle.HTTPMethod('POST', RequestURL);
  FResultCode:=FFiddle.ResultCode;
  FFiddle.Document.Position := 0;
  FReceivedBody:=ReadStrFromStream(FFiddle.Document, FFiddle.Document.Size);
  FReceivedHeaders.Assign(FFiddle.Headers);
end;

procedure TOAuthTransportCustom.SetHeader(HeaderName, HeaderContent: string);
// Replace existing header or insert new header
var
  HeaderIndex: integer;
begin
  HeaderIndex:=FFiddle.Headers.IndexOfName(HeaderName);
  if HeaderIndex > -1 then
  begin
    FFiddle.Headers.Delete(HeaderIndex);
  end;
  FFiddle.Headers.Insert(0, HeaderName+': '+HeaderContent);
end;

constructor TOAuthTransportCustom.Create(Parent: TOAuth1);
begin
  inherited Create(Parent);
  FFiddle:=TFiddleHTTPSend.Create(Parent);
end;

destructor TOAuthTransportCustom.Destroy;
begin
  FFiddle.Free;
  inherited Destroy;
end;

{ TFiddleHTTPSend }


function TFiddleHTTPSend.ReadUnknown: boolean;
  // Adapted from Synapse HTTPSend.ReadUnknown to omit stream handling
  // Breaks on #13#10 and sends out string to handler (if assigned)
var
  s: ansistring;
begin
  Result := false;
  repeat
    s := FSock.RecvPacket(FTimeout);
    if FSock.LastError = 0 then
    begin
      if Assigned(FReceiveHandler) then
        if FReceiveHandler(s) = false then
          break;
    end;
  until FSock.LastError <> 0;
  if FSock.LastError = WSAECONNRESET then
  begin
    Result := true;
    FSock.ResetLastError;
  end;
end;

constructor TFiddleHTTPSend.Create(OAuthInstance: TOAuth1);
begin
  inherited Create;
  FProtocol := '1.1'; //Assume HTTP 1.1 works if OAuth is implemented as well.
  FKeepAlive := false; //If set to true, current twitter streaming won't work
  //todo: look into setting to true and dealing with gzip compressed body (zstream unit)?
  FUserAgent := 'Mozilla/4.0 (compatible; oauth1synapse 20120622)'; // http://dev.twitter.com/docs/streaming-apis/connecting#User_Agent
  FHeaders.NameValueSeparator := ':'; // Instead of =; useful for finding headers

  // Set up proxy details if given
  if Assigned(OAuthInstance) then
  begin
    if (OAuthInstance.ProxyHost <> '') then
    begin
      FProxyHost := OAuthInstance.ProxyHost;
      // By default, Synapse sets up the proxy port as 8080 as well:
      if OAuthInstance.ProxyPort = '' then
        FProxyPort := '8080'
      else
        FProxyPort := OAuthInstance.ProxyPort;
      FProxyUser := OAuthInstance.ProxyUser;
      FProxyPass := OAuthInstance.ProxyPass;
    end;
  end;
end;

destructor TFiddleHTTPSend.Destroy;
begin
  inherited Destroy;
end;

end.

