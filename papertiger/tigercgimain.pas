unit tigercgimain;

{ CGI server part of papertiger.

  Copyright (c) 2013 Reinier Olislagers

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
If we send responses as JSON, always send an object or an array, not simply a string, float or integer.
Dates/times in JSON should be represented as ISO 8601 UTC (no timezone) formatted strings
}
{$i tigerserver.inc}

interface

uses
  SysUtils, Classes, httpdefs, fpjson, jsonparser, fpHTTP, fpWeb,
  tigerutil, tigerservercore;

type

  { TFPWebobsolete }

  TFPWebobsolete = class(TFPWebModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure unsupportedRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean); //handler for invalid requests
  private
    { private declarations }
    FTigerCore: TTigerServerCore;
  public
    { public declarations }
  end;

var
  FPWebobsolete: TFPWebobsolete;

implementation

{$R *.lfm}

{ TFPWebobsolete }




procedure TFPWebobsolete.DataModuleCreate(Sender: TObject);
begin
  FTigerCore := TTigerServerCore.Create;
end;

procedure TFPWebobsolete.DataModuleDestroy(Sender: TObject);
begin
  FTigerCore.Free;
end;

procedure TFPWebobsolete.unsupportedRequest(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: boolean);
begin
  //todo add hyperlinks to all supported docs etc
  TigerLog.WriteLog(etDebug, 'unsupportedRequest: got request: ' +
    ARequest.PathInfo + ' with method ' + ARequest.Method);
  TigerLog.WriteLog(etDebug, 'unsupportedRequest: got query: ' + ARequest.QueryString);

  AResponse.Code := 404;
  AResponse.CodeText := 'Unsupported method';
  AResponse.Contents.Add('<p>Unsupported method.</p>');
  //Tried with http://<server>/cgi-bin/tigercgi/unsupported?q=5
  AResponse.Contents.Add('<p>Command was: ' + ARequest.Command + '</p>'); //gives nothing
  AResponse.Contents.Add('<p>Commandline was: ' + ARequest.CommandLine + '</p>');
  AResponse.Contents.Add('<p>GetNextPathinfo: ' + ARequest.GetNextPathInfo + '</p>'); //gives
  AResponse.Contents.Add('<p>Pathinfo: ' + ARequest.PathInfo + '</p>'); //gives /unsupported
  AResponse.Contents.Add('<p>LocalPathPrefix: ' + ARequest.LocalPathPrefix + '</p>');
  AResponse.Contents.Add('<p>ReturnedPathInfo: ' + ARequest.ReturnedPathInfo + '</p>');
  AResponse.Contents.Add('<p>URI: ' + ARequest.URI + '</p>'); //gives nothing
  AResponse.Contents.Add('<p>URL: ' + ARequest.URL + '</p>');
  //gives eg /cgi-bin/tigercgi/unsupported?q=5
  Handled := True;
end;

initialization
  // This registration will handle http://server/cgi-bin/tigercgi/obsolete/*
  RegisterHTTPModule('obsolete', TFPWebobsolete);
end.
