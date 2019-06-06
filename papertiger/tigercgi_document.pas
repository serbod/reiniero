unit tigercgi_document;

{ Papertiger CGI handling for document-related functionality.

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

  { TFPWebdocument }

  TFPWebdocument = class(TFPWebModule)
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
  FPWebdocument: TFPWebdocument;

implementation

{$R *.lfm}

{ TFPWebdocument }

procedure TFPWebdocument.DataModuleCreate(Sender: TObject);
begin
  FTigerCore := TTigerServerCore.Create;
end;

procedure TFPWebdocument.DataModuleDestroy(Sender: TObject);
begin
  FTigerCore.Free;
end;

procedure TFPWebdocument.DataModuleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: boolean);
// We don't define any actions but handle the request at the module level before any actions would be evaluated.
{
Handled URLs/methods:
DELETE http://server/cgi-bin/tigercgi/document/        //delete all docs?!?!
GET    http://server/cgi-bin/tigercgi/document/        //list of docs
POST   http://server/cgi-bin/tigercgi/document/        //let server create new doc, return documentid
DELETE http://server/cgi-bin/tigercgi/document/304     //remove document with id 304
GET    http://server/cgi-bin/tigercgi/document/304     //get details about document with id 304
GET    http://server/cgi-bin/tigercgi/document/304/pdf //get pdf of document 304
POST   http://server/cgi-bin/tigercgi/document/304?processdocument=true  //do OCR for all images etc
PUT    http://server/cgi-bin/tigercgi/document/304     //edit doc with id 304
}
var
  DocumentArray: TJSONArray;
  DocumentID: integer;
  IsValidRequest: boolean;
  OutputJSON: TJSONObject;
  StrippedPath: string;
begin
  IsValidRequest := False;
  {
  pathinfo apparently returns something like
  /document/304
  StrippedPath will remove trailing and leading /
  }
  StrippedPath := copy(ARequest.PathInfo, 2, Length(ARequest.PathInfo));
  if RightStr(StrippedPath, 1) = '/' then
    StrippedPath := Copy(StrippedPath, 1, Length(StrippedPath) - 1);
  TigerLog.WriteLog(etDebug, 'Document module: got stripped path: ' +
    StrippedPath + ' with method ' + ARequest.Method);
  if ARequest.QueryString <> '' then
    TigerLog.WriteLog(etDebug, 'Document module: got query: ' + ARequest.QueryString);
  TigerLog.WriteLog(etDebug, 'Wordcount: ' + IntToStr(WordCount(StrippedPath, ['/'])));

  // Make sure the user didn't specify levels in the URI we don't support:
  case ARequest.Method of
    'DELETE':
    begin
      case WordCount(StrippedPath, ['/']) of
        1: //http://server/cgi-bin/tigercgi/document/
        begin
          IsValidRequest:=FTigerCore.DeleteDocuments(true);
        end;
        2: //http://server/cgi-bin/tigercgi/document/304
        begin
          DocumentID := StrToIntDef(ExtractWord(2, StrippedPath, ['/']), INVALIDID);
          if DocumentID <> INVALIDID then
          begin
            IsValidRequest:= FTigerCore.DeleteDocument(DocumentID,true);
          end
          else
          begin
            TigerLog.WriteLog('Document module: delete method: got invalid document ID');
          end;
        end;
      end;
    end;
    'GET':
    begin
      case WordCount(StrippedPath, ['/']) of
        1: //http://server/cgi-bin/tigercgi/document/ get list of documents
        begin
          IsValidRequest := True;
          DocumentArray := TJSONArray.Create();
          try
            FTigerCore.ListDocuments(INVALIDID, DocumentArray);
            AResponse.ContentType := 'application/json';
            AResponse.Contents.Add(DocumentArray.AsJSON);
          except
            on E: Exception do
            begin
              DocumentArray.Clear;
              DocumentArray.Add(TJSONSTring.Create('listRequest: exception ' +
                E.Message));
              AResponse.Contents.Insert(0, DocumentArray.AsJSON);
            end;
          end;
        end;
        2: //http://server/cgi-bin/tigercgi/document/304 get document details
        begin
          IsValidRequest := True;
          DocumentArray := TJSONArray.Create();
          OutputJSON := TJSONObject.Create();
          try
            try
              // document name, pdf path, scandate, document hash
              OutputJSON.Add('documentid', DocumentID);
              //todo: add doc details
              // list of all images for document: image order, path, imagehash
              FTigerCore.ListImages(DocumentID, InvalidID, DocumentArray);
              OutputJSON.Add('imagedetails', DocumentArray);
              AResponse.ContentType := 'application/json';
              AResponse.Contents.Add(OutputJSON.AsJSON);
            except
              on E: Exception do
              begin
                OutputJSON.Add('error', 'documentDetails request: exception ' + E.Message);
                AResponse.Contents.Insert(0, OutputJSON.AsJSON);
              end;
            end;
          finally
            DocumentArray.Free;
            OutputJSON.Free;
          end;
        end;
        3: //http://server/cgi-bin/tigercgi/document/304/pdf get document as PDF
        begin
          if lowercase(ExtractWord(3, StrippedPath, ['/'])) = 'pdf' then
          begin
            DocumentID := StrToIntDef(ExtractWord(2, StrippedPath, ['/']), INVALIDID);
            if DocumentID <> INVALIDID then
            begin
              //retrieve pdf and put in output stream
              AResponse.ContentStream := TMemoryStream.Create;
              try
                // Load PDF into content stream:
                if FTigerCore.GetPDF(DocumentID, AResponse.ContentStream) then
                begin
                  // Indicate papertiger should be able to deal with this data:
                  IsValidRequest := True;
                  AResponse.ContentType := 'application/pdf';
                  AResponse.ContentLength := AResponse.ContentStream.Size;
                  //apparently doesn't happen automatically:
                  AResponse.SendContent;
                end
                else
                begin
                  TigerLog.WriteLog(etDebug,'Document module: could not get valid PDF.');
                  IsValidRequest := False; //spell it out; follow up code will return 404 error
                end;
              finally
                AResponse.ContentStream.Free;
              end;
            end
            else
            begin
              TigerLog.WriteLog(etDebug,'Document module: pdf request: invalid document ID was passed: '+ExtractWord(2, StrippedPath, ['/']));
            end;
          end
          else
          begin
            TigerLog.WriteLog(etDebug,'Document module: failure: received '+StrippedPath+' expecting PDF request.');
          end;
        end;
      end;
    end;
    'POST':
    begin
      //http://server/cgi-bin/tigercgi/document/
      if WordCount(StrippedPath, ['/']) = 1 then
      begin
        IsValidRequest := True;
        DocumentID := FTigerCore.AddDocument('Document ' +
          FormatDateTime('yyyymmddhhnnss', Now));
        if DocumentID = INVALIDID then
        begin
          IsValidRequest := False;
        end
        else
        begin
          AResponse.ContentType := 'application/json';
          OutputJSON := TJSONObject.Create();
          try
            OutputJSON.Add('documentid', DocumentID);
            AResponse.Contents.Add(OutputJSON.AsJSON);
          finally
            OutputJSON.Free;
          end;
        end;
      end;
      //POST   http://server/cgi-bin/tigercgi/document/304?processdocument=true  //do OCR for all images etc
      if (WordCount(StrippedPath, ['/']) = 2) and
        (ARequest.QueryFields.Values['processdocument'] = 'true') then
      begin
        DocumentID := StrToIntDef(ExtractWord(2, StrippedPath, ['/']), INVALIDID);
        if DocumentID <> INVALIDID then
        begin
          if FTigerCore.ProcessImages(DocumentID, 0, true) <> '' then
          begin
            IsValidRequest := True;
            // we could return the pdf name etc but it doesn't make much sense
          end;
        end
        else
        begin
          TigerLog.WriteLog(etDebug, 'Document module: got invalid document ID.');
        end;
      end;
    end;
    'PUT':
    begin
      //todo: actually implement editing doc here via json properties or something?
      //edit doc with id 304
      //http://server/cgi-bin/tigercgi/document/304
      if WordCount(StrippedPath, ['/']) = 2 then
      begin
        DocumentID := StrToIntDef(ExtractWord(2, StrippedPath, ['/']), INVALIDID);
        if DocumentID <> INVALIDID then
        begin
          IsValidRequest := True;
          //todo: modify given document, not add it
          //FTigerCore.UpdateDocument(....)
          DocumentID := INVALIDID; //replace with actual code
          if DocumentID = INVALIDID then
          begin
            AResponse.Code := 404;
            AResponse.CodeText := 'Error inserting new document.';
            AResponse.Contents.Add('<p>Error inserting new document.</p>');
          end
          else
          begin
            AResponse.ContentType := 'application/json';
            OutputJSON := TJSONObject.Create();
            try
              OutputJSON.Add('documentid', DocumentID);
              AResponse.Contents.Add(OutputJSON.AsJSON);
            finally
              OutputJSON.Free;
            end;
          end;
        end;
      end;
    end;
  end;
  if not (IsValidRequest) then
  begin
    TigerLog.WriteLog(etWarning, 'Document module: invalid request; got stripped path: ' +
      StrippedPath + ' with method ' + ARequest.Method);
    if ARequest.QueryString <> '' then
      TigerLog.WriteLog(etWarning, 'Document module: invalid request; got query: ' +
        ARequest.QueryString);
    TigerLog.WriteLog(etDebug,
      'Document module: invalid request; got URL interesting wordcount: ' +
      IntToStr(WordCount(StrippedPath, ['/'])));
    AResponse.Code := 404;
    AResponse.CodeText := 'Document not found.';
    AResponse.Contents.Add('<p>Document not found/invalid request</p>');
  end;
  Handled := True;
end;

initialization
  // This registration will handle http://server/cgi-bin/tigercgi/document/*
  RegisterHTTPModule('document', TFPWebdocument);
end.
