program tigerserver;

{ Paper Tiger paper scanning/OCR/archiving solution

  Copyright (c) 2012-2014 Reinier Olislagers

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


{$i tigerserver.inc}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes, SysUtils, CustApp, fpjson, jsonparser, tigerservercore, ocr,
  imagecleaner, tigerutil, scan, pdf, tigerdb, tigersettings;

type

  { TTigerServer }

  TTigerServer = class(TCustomApplication)
  private
    FTigerCore: TTigerServerCore;
    procedure ListDocuments;
  protected
    procedure DoRun; override;
    // Main entry point into the program; processes command line options etc
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TTigerServer }

  procedure TTigerServer.ListDocuments;
  var
    Cell: string;
    DateCell: TDateTime;
    Document: TJSONObject;
    DocumentID: integer;
    DocumentsArray: TJSONArray;
    ImagesArray: TJSONArray;
    Image: TJSONObject;
    DocCol,DocNo: integer;
    ImCol,ImNo: integer;
  begin
    writeln('Existing documents on server:');
    DocumentsArray := TJSONArray.Create;
    FTigerCore.ListDocuments(INVALIDID, DocumentsArray);
  {$IFDEF DEBUG}
    // Extra troubleshooting; useful in client/server environment
    if DocumentsArray.JSONType <> jtArray then
      Exception.CreateFmt('ListDocuments error: Got "%s", expected "TJSONArray".',
        [DocumentsArray.ClassName]);
  {$ENDIF DEBUG}

    // Check for empty array
    if DocumentsArray.Count < 1 then
    begin
      writeln('*** no documents available ***');
      exit;
    end;

    // Check for empty object=>empty recordset
    Document := TJSONObject(DocumentsArray.Items[0]);
    if Document.JSONType <> jtObject then
    begin
      writeln('*** no documents available (empty document object) ***');
      exit;
    end;

    for DocNo := 0 to DocumentsArray.Count - 1 do
    begin
      Document := (DocumentsArray[DocNo] as TJSONObject);
      // Write column headers:
      if DocNo = 0 then
      begin
        for DocCol := 0 to Document.Count - 1 do
        begin
          Write(Document.Names[DocCol]);
          if DocCol<Document.Count -1 then
            Write(';');
        end;
        writeln();
      end;

      // Write column data for each record:
      for DocCol := 0 to Document.Count - 1 do
      begin
        try
          DocumentID := Document.Items[0].AsInteger; //document ID is first returned item
        except
          writeln('Error getting document ID. Aborting.');
          exit;
        end;

        try
          Cell := Document.Items[DocCol].AsString;
        except
          Cell := '[INVALID]';
        end;
        case Document.Items[DocCol].JSONType of
          jtUnknown: Write('[UNKNOWN]');
          jtNumber: Write(Cell);
          jtString:
          begin
            if FTigerCore.TryParseDate(Cell, DateCell) then
              Write(DateTimeToStr(DateCell))
            else
              Write(Cell);
          end;
          jtBoolean: Write(Cell);
          jtNull: Write(Cell);
          jtArray: Write('[ARRAY]');
          jtObject: Write('[OBJECT]');
        end;
        if DocCol<Document.Count - 1 then
          Write(';');
      end;
      writeln;

      // Now enumerate all images in that document
      ImagesArray := TJSONArray.Create;
      FTigerCore.ListImages(DocumentID, INVALIDID, ImagesArray);
      // Check for empty array
      if ImagesArray.Count < 1 then
      begin
        writeln('- no images');
        writeln();
        continue; //skip to next document
      end;

      // Check for empty object=>empty recordset
      Image := TJSONObject(ImagesArray.Items[0]);
      if Image.JSONType <> jtObject then
      begin
        writeln('- no images (technical note: invalid object)');
        writeln();
        continue; //skip to next document
      end;

      for ImNo := 0 to ImagesArray.Count - 1 do
      begin
        Image := (ImagesArray[ImNo] as TJSONObject);
        // Write column headers:
        if ImNo = 0 then
        begin
          Write('- '); //indent
          for ImCol := 0 to Image.Count - 1 do
          begin
            Write(Image.Names[ImCol]);
            if ImCol<Image.Count -1 then
              Write(';');
          end;
          writeln();
        end;

        // Write column data for each record:
        Write('- '); //indent
        for ImCol := 0 to Image.Count - 1 do
        begin
          //todo: for date, we get a number instead of a date. fix this
          try
            Cell := Image.Items[ImCol].AsString;
          except
            Cell := '[INVALID]';
          end;
          case Image.Items[ImCol].JSONType of
            jtUnknown: Write('[UNKNOWN]');
            jtNumber: Write(Cell);
            jtString:
            begin
              if FTigerCore.TryParseDate(Cell, DateCell) then
                Write(DateTimeToStr(DateCell))
              else
                Write(Cell);
            end;
            jtBoolean: Write(Cell);
            jtNull: Write(Cell);
            jtArray: Write('[ARRAY]');
            jtObject: Write('[OBJECT]');
          end;
          if ImCol<Image.Count - 1 then
            Write(';');
        end;
        writeln();
        writeln();
      end;
    end;
    writeln();
  end;

  procedure TTigerServer.DoRun;
  var
    DocumentID: integer;
    i: integer;
    ErrorMsg: string;
    PDF: string;
  begin
    // quick check parameters
    ErrorMsg := CheckOptions('d:hi:l:op:r:sv',
      'blackwhite color colour deletedocument: device: gray grayscale help image: language: lineart list ocr pages: purge rotate: scan scanonly version');
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters; show help if no params given
    if (ParamCount = 0) or (HasOption('h', 'help')) then
    begin
      writeln(FTigerCore.ServerInfo);
      writeln('');
      WriteHelp;
      Terminate;
      Exit;
    end;

    if HasOption('d','device') then
    begin
      FTigerCore.ScanDevice:=GetOptionValue('d','device');
    end
    else
    begin
      FTigerCore.ScanDevice:='';
    end;

    if HasOption('color') or HasOption('colour') then
    begin
      FTigerCore.ColorType:=ScanType.stColor;
    end;

    if HasOption('gray') or HasOption('grayscale') then
    begin
      FTigerCore.ColorType:=ScanType.stGray;
    end;

    // Put black and white last so it trumps the other settings
    if HasOption('blackwhite') or HasOption('lineart') then
    begin
      FTigerCore.ColorType:=ScanType.stLineArt;
    end;

    if HasOption('list') then
    begin
      ListDocuments;
      Terminate;
      exit;
    end;

    if HasOption('v', 'version') then
    begin
      writeln(FTigerCore.ServerInfo);
      Terminate;
      Exit;
    end;

    if HasOption('l', 'language') then
    begin
      FTigerCore.CurrentOCRLanguage := GetOptionValue('l', 'language');
    end;

    if HasOption('p', 'pages') then
    begin
      FTigerCore.Pages := StrToIntDef(GetOptionValue('p', 'pages'),1);
    end;

    if HasOption('r', 'rotate') then
    begin
      FTigerCore.DesiredRotation:=StrToIntDef(GetOptionValue('r','rotate'),0);
    end;

    // Branching off into processing starts here
    if HasOption('deletedocument') then
    begin
      DocumentID:=strtointdef(GetOptionValue('deletedocument'),INVALIDID);
      if DocumentID<>INVALIDID then
      begin
        if not(FTigerCore.DeleteDocument(DocumentID,true)) then
          writeln('Error trying to delete document '+inttostr(DocumentID));
      end
      else
      begin
        writeln('Invalid document ID specified. Stopping.');
      end;
    end;

    if HasOption('o', 'ocr') then
    begin
      if not FTigerCore.ProcessAllDocuments then
        writeln('Error trying to OCR all needed documents.');
    end;

    if HasOption('purge') then
    begin
      if not FTigerCore.PurgeDB then
        writeln('Error trying to purge database.');
    end;

    if HasOption('i', 'image') then
    begin
      //todo: add support for ; or , separated ? image names when pages>1
      DocumentID := FTigerCore.AddDocument('Document ' + FormatDateTime('yyyymmddhhnnss', Now));
      if DocumentID <> INVALIDID then
      begin
        if (FTigerCore.AddImage(ExpandFileName(GetOptionValue('i', 'image')), DocumentID, 0, (FTigerCore.ColorType=ScanType.stLineArt)) <> INVALIDID) then
        begin
          PDF := FTigerCore.ProcessImages(DocumentID, 0, true);
          if PDF = '' then
            writeln('Error creating PDF. Stopping.')
          else
            writeln('Finished adding image; created PDF '+PDF);
        end
        else
        begin
          writeln('Error adding image. Stopping.');
        end;
      end
      else
      begin
        writeln('Error getting document ID. Stopping.');
      end;
    end;

    if HasOption('s', 'scan') then
    begin
      DocumentID := INVALIDID;
      PDF := '';
      try
        DocumentID := FTigerCore.AddDocument('Document ' + FormatDateTime('yyyymmddhhnnss', Now));
        if DocumentID <> INVALIDID then
        begin
          for i := 1 to FTigerCore.Pages do
          begin
            if FTigerCore.ScanSinglePage(DocumentID)=INVALIDID then
            begin
              writeln('Error scanning page '+inttostr(i)+'. Aborting.');
              Terminate;
              exit;
            end;
            if (FTigerCore.Pages > 1) and (i < FTigerCore.Pages) then
            begin
              writeln('Please put page ' + IntToStr(i + 1) + ' in the scanner and press enter to continue.');
              readln;
            end;
          end;
          PDF := FTigerCore.ProcessImages(DocumentID, 0, true);
        end;
        if PDF = '' then
          writeln('Error while scanning')
        else
          writeln('Scanning complete; created PDF '+PDF);
      except
        on E: Exception do
        begin
          writeln('Exception: ' + E.Message);
        end;
      end;
    end;

    if HasOption('scanonly') then
    begin
      DocumentID := INVALIDID;
      try
        DocumentID := FTigerCore.AddDocument('Document ' + FormatDateTime('yyyymmddhhnnss', Now));
        if DocumentID <> INVALIDID then
        begin
          for i := 1 to FTigerCore.Pages do
          begin
            if FTigerCore.ScanSinglePage(DocumentID)=INVALIDID then
            begin
              writeln('Error scanning page '+inttostr(i)+'. Aborting.');
              Terminate;
              exit;
            end;
            if (FTigerCore.Pages > 1) and (i < FTigerCore.Pages) then
            begin
              writeln('Please put page ' + IntToStr(i + 1) + ' in the scanner and press enter to continue.');
              readln;
            end;
          end;
          writeln('Scanning complete.');
        end;
      except
        on E: Exception do
        begin
          writeln('Exception: ' + E.Message);
        end;
      end;
    end;


    // stop program loop
    Terminate;
  end;

  constructor TTigerServer.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := true;
    FTigerCore := TTigerServerCore.Create;
  end;

  destructor TTigerServer.Destroy;
  begin
    FTigerCore.Free;
    inherited Destroy;
  end;

  procedure TTigerServer.WriteHelp;
  begin
    writeln('Usage: ', ExeName, ' -h');
    writeln('--blackwhite, --lineart');
    writeln(' Scan/PDF in black & white. Useful for text only documents such as');
    writeln(' letters.');
    writeln('--color, --colour');
    writeln(' Scan/PDF in color (text detection internally still in black/white');
    writeln(' for better performance). Useful for photos, graphics etc.');
    writeln('-d <device> --device=<device>');
    writeln(' Scanning device (use sane notation) - empty to select default.');
    writeln('--deletedocument=<documentid>');
    writeln(' Deletes scanned document and associated images from database and ');
    writeln(' filesystem.');
    writeln('--gray, --grayscale');
    writeln(' Scan/PDF in grayscale (text detection internally still in black/white');
    writeln(' for better performance)');
    writeln('-i <image> --image=<image>');
    writeln(' Process image.');
    writeln('-l <lang> --language=<language>');
    writeln(' Language to be used for OCR, e.g. eng, nld');
    writeln(' eng (English) by default. See the OCR documentation for ');
    writeln(' language codes (e.g. man tesseract)');
    writeln('--list');
    writeln(' List already scanned documents, including document IDs and images');
    writeln('-o --ocr');
    writeln(' OCR/process all images that still need to be processed to generate');
    writeln(' pdfs.');
    writeln('-p <n> --pages=<n>');
    writeln(' Specify number of pages for processing/scanning multi page docs.');
    writeln('--purge');
    writeln(' Purge database of empty and invalid document/image records.');
    writeln('-r <d> --rotate=<d>');
    writeln(' Rotate image or scan d degrees clockwise before processing');
    writeln('-s --scan');
    writeln(' Scan document, process (perform OCR, create PDF).');
    writeln('--scanonly');
    writeln(' Scan document but do not process further.');
    writeln('-v --version');
    writeln(' Show version information and exit.');
  end;

var
  Application: TTigerServer;
begin
  Application := TTigerServer.Create(nil);
  Application.Run;
  Application.Free;
end.
