unit flocateshared;

{*
Windows/Linux/Mac OSX FreePascal code for keeping information regarding flocate that needs to be shared
between programs such as GUI, command line, web flocate projects
======================================================================================
MIT X11 License: no warranties, express or implied, but all other use permitted:
Copyright (c) 2010-2014 copyright holders

 Permission is hereby granted, free of charge, to any person
 obtaining a copy of this software and associated documentation
 files (the "Software"), to deal in the Software without
 restriction, including without limitation the rights to use,
 copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the
 Software is furnished to do so, subject to the following
 conditions:

 The above copyright notice and this permission notice shall be
 included in all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 OTHER DEALINGS IN THE SOFTWARE.
======================================================================================
*}

{$mode objfpc}
{$INCLUDE flocatesettings.inc}

interface

uses
  Classes, SysUtils;

type
  { Tflocateshared }

  Tflocateshared = class
  public
  private
    FDBHost: string;
    FDBPort: integer;
    FDBDatabase: string;
    FDBUser: string;
    FDBPassword: string;
    FUseEmbedded: boolean;
  public
    // If use, use local embedded database. If no, use client/server
    property UseEmbedded: boolean read FUseEmbedded write FUseEmbedded;
    property Host: string read FDBHost write FDBHost;
    // Port number for database, e.g. 3050
    property Port: integer read FDBPort write FDBPort;
    // Name/path for database
    property Database: string read FDBDatabase write FDBDatabase;
    property User: string read FDBUser write FDBUser;
    property Password: string read FDBPassword write FDBPassword;
    constructor Create;
    // Looks for ini file with settings. Returns empty string if not found, otherwise full path/filename
    function GetValidConfig(const Directory: string; IniFileName: string = ''): string;
  end;

implementation

uses
  IniFiles {for support of .ini file ;) };

const
  USERINIFILE: string = 'flocate.ini'; //Name of file that contains our configuration. No indication of directory
  USERINIFILETEMPLATE: string = 'flocate.template.ini'; //fallback general template
  DEFAULTDBNAME: string = 'flocate.fdb';

function Tflocateshared.GetValidConfig(const Directory: string; IniFileName: string = ''): string;
var
  IniFile: string;
begin
  if IniFileName = '' then
    IniFileName := USERINIFILE;
  IniFile := Directory + IniFileName;
  if FileExists(IniFile) then
    Result := Inifile
  else
    Result := '';
end;

constructor Tflocateshared.Create;
var
  IniFile: string;
  UserIni: TIniFile;
begin
  // We first use system-dependent global app config files:
  // This will avoid leaving ini files all over working dirs
  IniFile := '';
  // Try user level config dir, then system config dir, then
  // application dir, then template in application dir
  if IniFile = '' then
    IniFile := GetValidConfig(GetAppConfigDir(false), '');
  if IniFile = '' then
    IniFile := GetValidConfig(GetAppConfigDir(true), '');
  if IniFile = '' then
    IniFile := GetValidConfig(ExtractFilePath(ParamStr(0)), '');
  if IniFile = '' then
    IniFile := GetValidConfig(ExtractFilePath(ParamStr(0)), USERINIFILETEMPLATE);

  // Now use config info if present:
  if IniFile = '' then
  begin
    // No valid ini, so use defaults
    FUseEmbedded := true;
    FDBHost := '';
    FDBPort := 3050;
    if FUseEmbedded = true then
    begin
      // We use the code of the ExePath function to avoid a link to the LCL.
      // Note the app dir will only work under Windows.
      //todo: for OSX/linux, search e.g. in ~/.flocate/flocate.fdb
      FDBDatabase := ExtractFilePath(ParamStr(0)) + DEFAULTDBNAME;
    end
    else
    begin
      // We're not using embedded, so just give a default database name
      // and let the server figure it out. Let's hope there's an alias defined
      // for it, so it works.
      FDBDatabase := 'flocate.fdb';
    end;
    FDBUser := 'SYSDBA';
    FDBPassword := 'masterkey';
  end
  else // valid ini file, so we can read it
  begin
    UserIni := TIniFile.Create(IniFile);
    try
      FUseEmbedded := UserIni.ReadBool('Database', 'Embedded', true);
      //Host empty for embedded; db hostname or ip for client/server:
      FDBHost := UserIni.ReadString('Database', 'Host', '');
      //irrelevant for embedded:
      FDBPort := UserIni.ReadInteger('Database', 'Port', 3050);
      FDBDatabase := UserIni.ReadString('Database', 'DBName', 'flocate.fdb');
      //name (and full path for db if no alias or embedded db in current dir is used)
      // Todo: Investigate if full path required for embedded db in linux. Also investigate use
      // of qualified db name or alias in ini file or aliases definition
      if FUseEmbedded then
      begin
        FDBDatabase := ExtractFilePath(ParamStr(0)) + UserIni.ReadString('Database', 'DBName', 'flocate.fdb');
        //todo: for linux, search e.g. in ~/.flocate/flocate.fdb
      end;
      FDBUser := UserIni.ReadString('Database', 'Username', 'SYSDBA');
      FDBPassword := UserIni.ReadString('Database', 'Password', 'masterkey');
    finally
      UserIni.Free;
    end;
  end;

end;


end.
