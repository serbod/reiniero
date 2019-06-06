unit tigersettings;

{ Settings management for paper tiger.

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

interface

uses
  Classes, SysUtils, IniFiles;

const
  SettingsFile = 'tigerserver.ini'; //Default settings filename

type

  { TTigerSettings }

  TTigerSettings = class(TObject)
  private
    FCGIURL: string;
    FClientCertificate: string;
    FImageDirectory: string;
    FLanguage: string;
    FPDFDirectory: string;
    FScanDevice: string; //Scanner device name
    FScanProtocol: string; //SANE,WIA,TWAIN planned
    FSettings: TINIFile;
    FSettingsFileName: string; //name part only of the required settings file
  public
    // Client config: the URL that points to the tiger server
    property CGIURL: string read FCGIURL write FCGIURL;
    // Use this certificate if using client side certificates with TLS/SSL,
    // Note: client side certificates are not required for TLS/SSL, but your server may require them for security reasons.
    property ClientCertificate: string read FClientCertificate write FClientCertificate;
    // Directory where scanned images must be/are stored; Has trailing path delimiter.
    property ImageDirectory: string read FImageDirectory write FImageDirectory;
    // Language used for text recognition. Use Tesseract notation. Default English.
    property Language: string read FLanguage write FLanguage;
    // Directory where resulting PDFs must be stored; Has trailing path delimiter.
    property PDFDirectory: string read FPDFDirectory write FPDFDirectory;
    // Device ID for scanner (e.g. in SANE notation if SANE used)
    property ScanDevice: string read FScanDevice write FScanDevice;
    // Protocol used for scanner
    property ScanProtocol: string read FScanProtocol write FScanProtocol;
    constructor Create;
    // In case your settings file is not the default SettingsFile
    constructor Create(SettingsFileName: string);
    destructor Destroy; override;
  end;

implementation

{ TTigerSettings }

constructor TTigerSettings.Create;
begin
  //todo: handle config storage directory /etc on linux etc
  // default settings file unless called with overridden constructor
  if FSettingsFileName = '' then
    FSettingsFileName := SettingsFile;
  FSettings := TINIFile.Create(FSettingsFileName);
  // Default for Apache localhost:
  FCGIURL := 'http://127.0.0.1/cgi-bin/tigercgi/';
  FClientCertificate:='';
  FImageDirectory := '';
  FLanguage := 'eng'; //Default to English
  FPDFDirectory := '';
  FScanDevice := ''; //todo: find if there is some SANE default device name
  try
    FCGIURL := FSettings.ReadString('General', 'CGIURL', FCGIURL);
    FClientCertificate := FSettings.ReadString('General', 'ClientCertificate', '');;

    // When reading the settings, expand ~ to home directory etc
    // Default to current directory
    {$IFDEF UNIX}
    FImageDirectory := IncludeTrailingPathDelimiter(ExpandFileName(FSettings.ReadString('General', 'ImageDirectory', '~/scans')));
    {$ENDIF}
    {$IFDEF WINDOWS}
    FImageDirectory := IncludeTrailingPathDelimiter(ExpandFileName(FSettings.ReadString('General', 'ImageDirectory', IncludeTrailingPathDelimiter(GetAppConfigDir(false))+'scans')));
    {$ENDIF}
    FLanguage := FSettings.ReadString('General', 'Language', FLanguage);
    //Default to current directory
    FPDFDirectory := IncludeTrailingPathDelimiter(ExpandFileName(FSettings.ReadString('General', 'PDFDirectory', '~/pdfs')));
    // Default to sane, then wia then twain
    FScanProtocol :=''; //not defined, e.g. for client: use server
    FScanDevice := FSettings.ReadString('Sane', 'DeviceName', '');
    if FScanDevice<>'' then
      FScanProtocol := 'SANE'
    else
    begin
      FScanDevice := FSettings.ReadString('WIA', 'DeviceName', '');
      if FScanDevice <> '' then
        FScanProtocol := 'WIA'
      else
      begin
        FScanDevice := FSettings.ReadString('TWAIN', 'DeviceName', '');
        if FScanDevice <> '' then
          FScanProtocol := 'TWAIN';
      end;
    end;
  except
    // ignore errors
  end;
  // Fallback to directory where .ini file is stored
  if FImageDirectory = '' then
    FImageDirectory := IncludeTrailingPathDelimiter(ExtractFilePath(FSettingsFileName));
  if FLanguage = '' then
    FLanguage := 'eng';
  if FPDFDirectory = '' then
    FPDFDirectory := FImageDirectory;
end;

constructor TTigerSettings.Create(SettingsFileName: string);
begin
  FSettingsFileName := SettingsFileName;
  Create;
end;

destructor TTigerSettings.Destroy;
begin
  FSettings.Free;
  inherited Destroy;
end;

end.
