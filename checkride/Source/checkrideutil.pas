unit CheckRideUtil;

{*
This source code is provided under the MIT license:
Copyright (C) 2011 by Reinier Olislagers

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  CheckRideConfigFileName = 'CheckRide.conf'; //Change this when filename changes
  CheckRideExe = 'checkride.exe'; //Change this if you change the executable file name
  CheckRideHelperExe = 'checkridehelper.exe';
  //Change this if you change the executable file name
  HelperHostExample = 'changethisline.example.com';
  //Edit this when changing the config file
  HelperPortExample = '33334';//Edit this when changing the config file
  HelperNameExample = 'Wonko the Sane';//Edit this when changing the config file

type
  TStunnelMode = (Helped, Helper);

var
  FConnectHost: string; //Host to connect to (CheckRide)
  FConnectPort: integer; //Port to connect to (CheckRide) or listen on (CheckRideHelper)
  FConnectHelper: string; //Show this to CheckRide as a connection identifier
  FResourceDir: string = '';
//Directory where configs etc are temporarily placed, no trailing directory separator.

procedure CheckDebug(const Message: string);
//Our own version of debugln that doesn't run if DEBUG not defined
procedure CreateUniqueTempDir;
procedure CleanTempDir; //Cleans up the temp dir, if created.
procedure CleanSystemTray;
function ServiceExists(ServiceName: string): boolean;
function IsServiceRunning(ServiceName: string): boolean;
procedure StartService(ServiceName: string);
procedure StopService(ServiceName: string);
function ProcessExists(ExeFileName: string): boolean;
function WaitForProcessToDie(ProcessName: string): boolean;
function CustomStunnelconfig(TypeOfTunnel: TStunnelMode): string;
{ Extract required executables, config files out of resource so we can use them. Returns directory, with trailing \ or /, where extracted.}
function ResourceExtract(ExeFile: string): string;


implementation


uses
  FileUtil,
  ServiceManager,
  JwaTlHelp32 {for running processes},
  JwaWinType {for processes declarations},
  JwaWinBase {just a guess: for closing process handles},
  JwaWinSvc {for services declarations, always required},
  jwawinuser {for clearing tray icon/notification area},
  Zipper {extraction of resources},
  IniFiles, poormansresource,
  lclproc {for debugging};

function WaitForProcessToDie(ProcessName: string): boolean;
  {description Waits until process is dead, but respects timeout}
const
  SleepTimeOut = 4;
var
  i: integer;
begin
  i := 0;
  while ProcessExists(ProcessName) = False do
  begin
    sleep(500);
    //Application.ProcessMessages; //Handle GUI events
    i := i + 1;
    if i >= SleepTimeOut then
    begin
      Result := False;
      break;
    end;
  end; //while
  Result := True;
end;

procedure CleanSystemTray;
{description Clean dead icons from system tray/notification area}
var
  hNotificationArea: HWND;
  r: RECT;
  x: integer;
  y: integer;
begin
  hNotificationArea := FindWindowEx(
    FindWindowEx(FindWindowEx(FindWindowEx(0, 0, 'Shell_TrayWnd', ''),
    0, 'TrayNotifyWnd', ''), 0, 'SysPager', ''), 0, 'ToolbarWindow32',
    'Notification Area');
  GetClientRect(hNotificationArea, r);

  //Now we've got the area, force it to update
  //by sending mouse messages to it.
  x := 0;
  y := 0;
  while x < r.Right do
  begin
    while y < r.Bottom do
    begin
      SendMessage(hNotificationArea, WM_MOUSEMOVE, 0, (y shl 16) + x);
      y := y + 5;
    end;
    x := x + 5;
  end;
end;

function ServiceExists(ServiceName: string): boolean;
  {description Checks if a Windows service exists}
var
  Services: TServiceManager;
  ServiceStatus: TServiceStatus;
begin
  //Check for existing services
  Services := TServiceManager.Create(nil);
  try
    try
      Services.Acces := SC_MANAGER_CONNECT; //Note typo in property.
      //We don't need more access permissions than this; by default
      //the servicemanager is trying to get all access
      Services.Connect;
      try
        Services.GetServiceStatus(ServiceName, ServiceStatus);
        Result := True;
      except
        Result := False; //assume service does not exist
      end;
      Services.Disconnect;
    except
      on E: Exception do
      begin
        Result := False;
      end;
    end;
  finally
    Services.Free;
  end;
end;

function IsServiceRunning(ServiceName: string): boolean;
  {description Checks if a Windows service is running}
var
  Services: TServiceManager;
  ServiceStatus: TServiceStatus;
begin
  //Check for existing services
  //equivalent to sc query <servicename>
  Services := TServiceManager.Create(nil);
  try
    try
      Services.Acces := SC_MANAGER_CONNECT; //Note typo in .Access property.
      Services.Connect; //Connect with requested access permissions.
      Services.GetServiceStatus(ServiceName, ServiceStatus);
      if ServiceStatus.dwCurrentState = SERVICE_RUNNING then
      begin
        Result := True;
      end
      else
      begin
        Result := False;
      end;
      Services.Disconnect;
    except
      on E: EServiceManager do
      begin
        // A missing service might throw a missing handle exception? No?
        {LogOutput('Error getting service information for ' + ServiceName +
          '. Technical details: ' + E.ClassName + '/' + E.Message);}
        Result := False;
        raise; //rethrow original exception
      end;
      on E: Exception do
      begin
        {LogOutput('Error getting service information for ' + ServiceName +
          '. Technical details: ' + E.ClassName + '/' + E.Message);
          }
        Result := False;
        raise; //rethrow original exception
      end;
    end;
  finally
    Services.Free;
  end;
end;

procedure StartService(ServiceName: string);
{description Gives a service the start command}
var
  Services: TServiceManager;
  ServiceStatus: TServiceStatus;
begin
  Services := TServiceManager.Create(nil);
  try
    try
      Services.Acces := SC_MANAGER_CONNECT; //Note typo in .Access property.
      Services.Connect; //Connect with requested access permissions.
      Services.GetServiceStatus(ServiceName, ServiceStatus);
      if ServiceStatus.dwCurrentState <> SERVICE_RUNNING then
      begin
        Services.StartService(ServiceName, nil);
      end;
      Services.Disconnect;
    except
      on E: EServiceManager do
      begin
        // A missing service might throw a missing handle exception? No?
        {LogOutput('Error getting service information for ' + ServiceName +
          '. Technical details: ' + E.ClassName + '/' + E.Message);}
        raise; //rethrow original exception
      end;
      on E: Exception do
      begin
        {LogOutput('Error getting service information for ' + ServiceName +
          '. Technical details: ' + E.ClassName + '/' + E.Message);
          }
        raise; //rethrow original exception
      end;
    end;
  finally
    Services.Free;
  end;
end;

procedure StopService(ServiceName: string);
{description Gives a service the stop command}
var
  Services: TServiceManager;
  ServiceStatus: TServiceStatus;
begin
  Services := TServiceManager.Create(nil);
  try
    try
      Services.Acces := SC_MANAGER_CONNECT; //Note typo in .Access property.
      Services.Connect; //Connect with requested access permissions.
      Services.GetServiceStatus(ServiceName, ServiceStatus);
      if ServiceStatus.dwCurrentState = SERVICE_RUNNING then
      begin
        Services.StopService(ServiceName, True);
      end;
      Services.Disconnect;
    except
      on E: EServiceManager do
      begin
        // A missing service might throw a missing handle exception? No?
        {LogOutput('Error getting service information for ' + ServiceName +
          '. Technical details: ' + E.ClassName + '/' + E.Message);}
        raise; //rethrow original exception
      end;
      on E: Exception do
      begin
        {LogOutput('Error getting service information for ' + ServiceName +
          '. Technical details: ' + E.ClassName + '/' + E.Message);
          }
        raise; //rethrow original exception
      end;
    end;
  finally
    Services.Free;
  end;
end;

function ProcessExists(ExeFileName: string): boolean;
{description checks if the process is running. Adapted for freepascal from:
URL: http://www.swissdelphicenter.ch/torry/showcode.php?id=2554}
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  Result := False;

  while integer(ContinueLoop) <> 0 do
  begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) =
      UpperCase(ExeFileName)) or (UpperCase(FProcessEntry32.szExeFile) =
      UpperCase(ExeFileName))) then
    begin
      Result := True;
    end;
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

procedure CheckDebug(const Message: string);
begin
  {$IFDEF DEBUG}
  DebugLn(DateTimeTostr(Now) + ': ' + Message);
  sleep(500);//hopefully give time to write
  {$ENDIF DEBUG}
end;

procedure CreateUniqueTempDir;
{description Create a uniquely named directory under the user's temporary directory
for this application - one temp dir for all files. Resulting dir in FTempDir
Adapted from http://www.delphifaq.net/how-to-get-a-unique-file-name/}
const
  InvalidDir = '++&INVALID_TEMP_DIR!!';
var
  chTemp: char;
  DirectoryName: string = '';
  TempDir: string; //Our user's temporary directory
begin
  // Only create dir if it doesn't already exist:
  if (FResourceDir = '') or (FResourceDir = InvalidDir) then
  begin
    TempDir := SysUtils.GetTempDir; //Apparently contains trailing path separator

    // Get subdirectory name that doesn't already exist:
    repeat
      Randomize;
      repeat
        chTemp := Chr(Random(43) + 47);
        if Length(DirectoryName) = 8 then
          DirectoryName := DirectoryName + '.'
        else if chTemp in ['0'..'9', 'A'..'Z'] then
          DirectoryName := DirectoryName + chTemp;
      until Length(DirectoryName) = 12;
    until not DirectoryExists(TempDir + DirectoryName);

    //Once we're sure we have a new directory:
    try
      mkdir(TempDir + DirectoryName);
      FResourceDir := TempDir + DirectoryName; //Store the result
    except
      //This is bad.
      FResourceDir := InvalidDir;
      raise Exception.Create('Cannot create temporary directory.');
    end;
  end;
end;

procedure CleanTempDir;
begin
  if FResourceDir <> '' then
  begin
    DeleteDirectory(FResourceDir, False); //get rid of directory and children
  end;
end;


function ResourceExtract(ExeFile: string): string;
  {description Extracts required executables and other files from our poor man's resource to
temporary directory - stored in FResourceDir}
{NOTE: if you change files in the resources, please adjust
CheckRideResourceZipper.lpr accordingly and recompile.
CheckRideResourceZipper.exe is called before build of CheckRide and
CheckRideHelper, so it needs to know about changes.}
const
  ResourceName = 'ALL';
var
  CheckRideConfigFile: string = ''; //Source for config adjustments
  CheckRideConfigIni: TMemIniFile;
  // Contains CheckRideConfig data for modifying templates
  CheckRideConfigStrings: TStringList;// Contains data from ConfigResource
  PoorMansResource: TPayload;
  UnzipFile: TUnZipper;
  ZipToExtract: string;
begin
  // Init, just to make sure
  Result := '';
  if FResourceDir = '' then
  begin
    CreateUniqueTempDir;
  end;

  // Point resource stream to the resource
  PoorMansResource := TPayload.Create(ExeFile); //point to this executable
  {Note: Paramstr(0) not guaranteed portable to Unix etc...}
  UnzipFile := TUnZipper.Create();
  try
    //todo: we might streamline this, reading into memory and unzipping
    //relevant files, directly reading others. Not worth the effort for
    //minimum speed gains.
    ZipToExtract := FResourceDir + DirectorySeparator + ResourceName + '.zip';
    CheckDebug('ZipToExtract: ' + ZipToExtract); //is this actually the right file?
    PoorMansResource.PayloadIntoFile(ZipToExtract);
    UnzipFile.FileName := ZipToExtract;
    UnzipFile.OutputPath := FResourceDir;
    UnzipFile.UnZipAllFiles; //into temp dir, we hope
  finally
    UnzipFile.Free;
    PoorMansResource.Free;
  end;
  try
    DeleteFile(PChar(ZipToExtract));
  except
    //ignore errors deleting temp file; let's hope the file system will do this in time.
  end;

  //Now read in config
  CheckRideConfigIni := TMemIniFile.Create(CheckRideConfigFile, False);
  CheckRideConfigStrings := TStringList.Create;
  try
    try
      // If ini file exists in exe directory, use that.
      CheckRideConfigFile := ExtractFilePath(ParamStr(0)) + CheckRideConfigFileName;
      CheckRideConfigStrings.LoadFromFile(CheckRideConfigFile);
    except
      // Fallback: use resource embedded in program
      CheckRideConfigStrings.LoadFromFile(FResourceDir + DirectorySeparator +
        CheckRideConfigFileName);
    end;
    // Load strings into ini object for further processing:
    CheckRideConfigIni.SetStrings(CheckRideConfigStrings);
    FConnectHost := Trim(CheckRideConfigIni.ReadString('Default',
      'HelperHost', 'thiswillnotwork.example.com'));
    FConnectPort := CheckRideConfigIni.ReadInteger('Default', 'HelperPort', 33334);
    FConnectHelper := CheckRideConfigIni.ReadString('Default',
      'HelperName', 'FallbackHelperInProgram');
    Result := FResourceDir + DirectorySeparator; //Show valid result
  finally
    CheckRideConfigIni.Free;
    CheckRideConfigStrings.Free;
  end;
end;

function CustomStunnelconfig(TypeOfTunnel: TStunnelMode): string;
  {description Applies CheckRide.conf to relevant template file, writes
result to temporary file and returns the config file name.
If CheckRide.conf doesn't exist in the executable directory,
if necessary extract from resource and use.}
const
  HostReplace = '$(HELPERSERVERNAME)';
  PortReplace = '$(HELPERSERVERPORT)';
  HelpedTemplateName = 'stunnelhelped.conf.template';
  HelpedConfigName = 'stunnelhelped.conf';
  HelperTemplateName = 'stunnelhelper.conf.template';
  HelperConfigName = 'stunnelhelper.conf';
var
  HelpedResultFile: string; //Final config file
  HelperResultFile: string; //Final config file
  ResultingConfig: TStringList; // Template with checkrideconfig replacements run
begin
  // Init, just to make sure
  if FResourceDir = '' then
  begin
    CreateUniqueTempDir;
  end;

  HelpedResultFile := FResourceDir + DirectorySeparator + HelpedConfigName;
  HelperResultFile := FResourceDir + DirectorySeparator + HelperConfigName;
  ResultingConfig := TStringList.Create;
  try
    // Replace strings in helped template and save
    case TypeOfTunnel of
      Helped:
      begin
        ResultingConfig.LoadFromFile(FResourceDir + DirectorySeparator +
          HelpedTemplateName);
      end;
      Helper:
      begin
        ResultingConfig.LoadFromFile(FResourceDir + DirectorySeparator +
          HelperTemplateName);
      end;
      else
        raise Exception.Create('Invalid TypeOfTunnel selected.');
    end;
    ResultingConfig.Text := StringReplace(ResultingConfig.Text,
      HostReplace, FConnectHost, [rfReplaceAll]);
    ResultingConfig.Text := StringReplace(ResultingConfig.Text,
      PortReplace, IntToStr(FConnectPort), [rfReplaceAll]);
    case TypeOfTunnel of
      Helped:
      begin
        ResultingConfig.SaveToFile(HelpedResultFile);
        Result := HelpedResultFile;
      end;
      Helper:
      begin
        ResultingConfig.SaveToFile(HelperResultFile);
        Result := HelperResultFile;
      end;
      else
        raise Exception.Create('Invalid TypeOfTunnel selected.');
    end;
  finally
    ResultingConfig.Free;
  end;
end;

end.
