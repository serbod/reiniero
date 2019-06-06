unit CreateAccessDb;

{ Creates Microsoft Access format .mdb (or .accdb) database files
  Requires MS Access ODBC driver (either the old mdb or newer mdb/accdb ones);
  the Microsoft Access program itself is not required.
  Microsoft Windows specific unit, but surely Linux/Unix ODBC has similar functions.

  This source code is provided under the MIT license:
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

{$mode objfpc}{$H+}
interface

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  Windows;

// Fills List with ODBC driver names
procedure ODBCDriverList(List: TStrings);
// Uses Microsoft Access ODBC driver to create DatabaseFile
function CreateAccessDatabase(DatabaseFile: string): boolean;

implementation

type
  PSQLCHAR = ^SQLCHAR;
  SQLCHAR = char;
  SQLSCHAR = char;
  SQLINTEGER = longint;
  SQLPOINTER = Pointer;
  SQLSMALLINT = smallint;
  SQLUSMALLINT = word;
{ function return type }
type
  SQLRETURN = SQLSMALLINT;

{ generic data structures }
type
  SQLHANDLE = Pointer;
  SQLHENV = SQLHANDLE;

const
  ODBC_ADD_DSN = 1;
  SQL_FETCH_NEXT = 1;
  SQL_FETCH_FIRST = 2;

{ Functions for listing drivers}
function SQLAllocEnv(var EnvironmentHandle: SQLHENV): SQLRETURN; stdcall; external 'odbc32.dll';
function SQLFreeEnv(var EnvironmentHandle: SQLHENV): SQLRETURN; stdcall; external 'odbc32.dll';

function SQLDrivers(henv: SQLHENV; fDirection: SQLUSMALLINT; szDriverDesc: PSQLCHAR;
  cbDriverDescMax: SQLSMALLINT; var pcbDriverDesc: SQLSMALLINT;
  szDriverAttributes: PSQLCHAR; cbDrvrAttrMax: SQLSMALLINT;
  var pcbDrvrAttr: SQLSMALLINT): SQLRETURN; stdcall; external 'odbc32.dll';

{ Functions for creating a db}
function SQLConfigDataSource(hwndParent: integer; fRequest: integer; lpszDriverString: PChar; lpszAttributes: PChar): integer;
  stdcall; external 'odbccp32.dll';
function SQLInstallerError(iError: integer; pfErrorCode: PInteger; lpszErrorMsg: string; cbErrorMsgMax: integer;
  pcbErrorMsg: PInteger): integer; stdcall; external 'odbccp32.dll';

procedure ODBCDriverList(List: TStrings);
//http://www.delphigroups.info/2/3f/244321.html
var
  CurrentHEnv: SQLHENV;
  DriverName: array[0..255] of SQLCHAR;
  NameLen: SQLSMALLINT;
  Attribs: array[0..255] of SQLCHAR;
  AttrLen: SQLSMALLINT;
  Status: SQLRETURN;
begin
  CurrentHEnv := nil;
  if SQLAllocEnv(CurrentHEnv) <> 0 then
    Exit;
  FillChar(DriverName[0], 256, #0);
  FillChar(Attribs[0], 256, #0);
  Status := SQLDrivers(CurrentHEnv, SQL_FETCH_FIRST, @DriverName[0], 255, NameLen, @Attribs[0], 255, AttrLen);
  while (Status = 0) do
  begin
    List.Add(string(DriverName));
    Status := SQLDrivers(CurrentHEnv, SQL_FETCH_NEXT, @DriverName[0], 255, NameLen, @Attribs[0], 255, AttrLen);
  end;
end;

function CreateAccessDatabase(DatabaseFile: string): boolean;
const
  SQL_MAX_MESSAGE_LENGTH=4096;
var
  DBPChar: PChar;
  Driver: string;
  Drivers: TStringList;
  ErrorCode, ResizeErrorMessage: integer;
  ErrorMessage: PChar;
  ExistingError: string;
  retCode: integer;
begin
  // SQLConfigDataSource seems to return 0 error code even if db creation failed or
  // the driver does not even exist.
  // Therefore check first which driver the system has
  ExistingError:=''; //use this to check progress
  Drivers := TStringList.Create;
  try
    ODBCDriverList(Drivers);
    result := false; //fail by default
    Driver := 'Microsoft Access Driver (*.mdb, *.accdb)';
    if Drivers.IndexOf(Driver) > -1 then
    begin
      { With this driver,
      CREATE_DB/CREATE_DBV12 will create an .accdb format database;
      CREATE_DBV4 will create an mdb
      http://stackoverflow.com/questions/9205633/how-do-i-specify-the-odbc-access-driver-format-when-creating-the-database
      }
      DBPChar := PChar('CREATE_DBV4="' + DatabaseFile + '"');
      retCode := SQLConfigDataSource(Hwnd(nil), ODBC_ADD_DSN, PChar(Driver), DBPChar);
      if retCode = 1 then //note SQLConfigDataSource returns a boolean
      begin
        Result := true;
      end
      else
      begin
        Result := false;
        ErrorCode := 0;
        ResizeErrorMessage := 0;
        GetMem(ErrorMessage, SQL_MAX_MESSAGE_LENGTH);

        try
          SQLInstallerError(1, @ErrorCode, ErrorMessage, SizeOf(ErrorMessage), @ResizeErrorMessage);
          if ErrorMessage<>'' then
            ExistingError:=trim(ExistingError+' '+ErrorMessage);
        finally
          FreeMem(ErrorMessage);
        end;
        // Save up errors for later
      end;
    end;
    if result=false then
    begin
      // Call failed or first driver not present, so try old driver:
      Driver := 'Microsoft Access Driver (*.mdb)';
      if Drivers.IndexOf(Driver) > -1 then
      begin
        DBPChar := PChar('CREATE_DB="' + DatabaseFile + '"');
        retCode := SQLConfigDataSource(Hwnd(nil), ODBC_ADD_DSN, PChar(Driver), DBPChar);
        if retCode = 1 then //note function returns a boolean
        begin
          Result := true;
        end
        else
        begin
          Result := false;
          ErrorCode := 0;
          ResizeErrorMessage := 0;
          GetMem(ErrorMessage, SQL_MAX_MESSAGE_LENGTH);

          try
            SQLInstallerError(1, @ErrorCode, ErrorMessage, SizeOf(ErrorMessage), @ResizeErrorMessage);
            if ErrorMessage<>'' then
              ExistingError:=trim(ExistingError+' '+ErrorMessage);
          finally
            FreeMem(ErrorMessage);
          end;
        end;
      end
      else
      begin
        raise Exception.Create('Error creating Access database: no Access ODBC driver present.');
      end;
    end;
  finally
    Drivers.Free;
  end;
  if ExistingError<>'' then
    raise Exception.CreateFmt('Error creating Access database: %s', [ExistingError]);
end;

end.
