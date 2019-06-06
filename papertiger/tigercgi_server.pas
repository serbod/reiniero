unit tigercgi_server;

{ Papertiger CGI handling for server/administration-related functionality.

  Copyright (c) 2013-2014 Reinier Olislagers

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
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, tigerutil, tigerservercore,
  strutils, fpjson, jsonparser;

type

  { TFPWebserver }

  TFPWebserver = class(TFPWebModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: boolean);
  private
    { private declarations }
    FTigerCore: TTigerServerCore;
  public
    { public declarations }
  end;

var
  FPWebimage: TFPWebserver;

implementation

{$R *.lfm}

{ TFPWebserver }

procedure TFPWebserver.DataModuleCreate(Sender: TObject);
begin
  FTigerCore := TTigerServerCore.Create;
end;

procedure TFPWebserver.DataModuleDestroy(Sender: TObject);
begin
  FTigerCore.Free;
end;

procedure TFPWebserver.DataModuleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
// We don't define any actions but handle the request at the module level before any actions would be evaluated.
{
Handled URLs/methods:
GET    http://server/cgi-bin/tigercgi/server/              // get server info
}
var
  IsValidRequest: boolean;
  OutputJSON: TJSONObject;
  StrippedPath: string;
begin
  IsValidRequest := False;
  {
  pathinfo apparently returns something like
  /image/304
  StrippedPath will remove trailing and leading /
  }
  StrippedPath := copy(ARequest.PathInfo, 2, Length(ARequest.PathInfo));
  if RightStr(StrippedPath, 1) = '/' then
    StrippedPath := Copy(StrippedPath, 1, Length(StrippedPath) - 1);
  TigerLog.WriteLog(etDebug, 'Server module: got stripped path: ' +
    StrippedPath + ' with method ' + ARequest.Method);
  if ARequest.QueryString <> '' then
    TigerLog.WriteLog(etDebug, 'Server module: got query: ' + ARequest.QueryString);
  TigerLog.WriteLog(etDebug, 'Wordcount: ' + IntToStr(WordCount(StrippedPath, ['/'])));

  // Make sure the user didn't specify levels in the URI we don't support:
  case ARequest.Method of
    'DELETE':
    begin
      case WordCount(StrippedPath, ['/']) of
        1: //http://server/cgi-bin/tigercgi/server/
        IsValidRequest := false; //deletion not allowed
      end;
    end;
    'GET':
    begin
      case WordCount(StrippedPath, ['/']) of
        0, { http://server/cgi-bin/tigercgi/server }
        1: { http://server/cgi-bin/tigercgi/server/ }
        begin
          IsValidRequest := True;
          AResponse.ContentType := 'application/json';
          OutputJSON := TJSONObject.Create();
          try
            OutputJSON.Add('serverinfo', FTigerCore.ServerInfo);
            AResponse.Contents.Add(OutputJSON.AsJSON);
          finally
            OutputJSON.Free;
          end;
        end;
      end;
    end;
    'POST':
    begin
      {
      POST   http://server/cgi-bin/tigercgi/server
      }
      // Note we don't allow empty images to be created: either scan or upload image
      IsValidRequest := false; //don't allow post for now
    end;
    'PUT':
    begin
      //http://server/cgi-bin/tigercgi/server
      IsValidRequest := false; // don't allow put for now
    end;
  end;
  if not (IsValidRequest) then
  begin
    TigerLog.WriteLog(etWarning, 'Server module: invalid request; got stripped path: ' +
      StrippedPath + ' with method ' + ARequest.Method);
    if ARequest.QueryString <> '' then
      TigerLog.WriteLog(etWarning, 'Server module: invalid request; got query: ' +
        ARequest.QueryString);
    TigerLog.WriteLog(etDebug,
      'Server module: invalid request; got URL interesting wordcount: ' +
      IntToStr(WordCount(StrippedPath, ['/'])));
    AResponse.Code := 404;
    AResponse.CodeText := 'File not found.';
    AResponse.Contents.Add('<p>Server module: file not found/invalid request</p>');
  end;
  Handled := True;
end;

initialization
  // This registration will handle http://server/cgi-bin/tigercgi/server/*
  RegisterHTTPModule('server', TFPWebserver);
end.
