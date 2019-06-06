unit CheckRideHelperUnit;

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Grids, AsyncProcess, Menus;

type

  { TCheckRideHelperMain }

  TCheckRideHelperMain = class(TForm)
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    HelpMenu: TMenuItem;
    About: TMenuItem;
    ToolsMenu: Tmenuitem;
    CustomizeMenu: Tmenuitem;
    MenuLicenses: TMenuItem;
    QuitMenu: TMenuItem;
    PortScanButton: TButton;
    WhatIsMyIPButton: TButton;
    Memo1: TMemo;
    TunnelProcess: TAsyncProcess;
    ConnectButton: TButton;
    DisconnectButton: TButton;
    ServerPort: TLabeledEdit;
    VNCViewerProcess: TAsyncProcess;
    procedure AboutClick(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure CustomizeMenuClick(Sender: TObject);
    procedure MenuLicensesClick(Sender: TObject);
    procedure PortScanButtonClick(Sender: TObject);
    procedure QuitMenuClick(Sender: TObject);
    procedure SetupConfigAndExes;
    procedure ListenForHelp;
    procedure DisconnectHelp;
    procedure ShowCommandLineHelp;
    procedure ServerPortEditingDone(Sender: TObject);
    procedure WhatIsMyIPButtonClick(Sender: TObject);
  private
    FExePath: string;
    FPortInCommandLine: boolean; //Does user want to override port using command line?
    FListenOneTime: boolean; //Should we start listening in the OnActivate event?
    FSetupComplete: boolean;
    FVNCViewerFullPath: string;
    FStunnelFullPath: string;
    FListening: boolean; //Is the tunnel/vnc combo running and listening?
    procedure Updatebuttons;
    { private declarations }
  public
    { public declarations }
  end;

var
  CheckRideHelperMain: TCheckRideHelperMain;

implementation

uses
  Windows, CheckRideUtil, LCLIntf, aboutform, clientcustomizer;

{$R *.lfm}

{ TCheckRideHelperMain }
const
  VNCViewerExe = 'vncviewer.exe';
  StunnelExe = 'stunnel.exe';
  VNCViewerListenPort = 65001;


procedure TCheckRideHelperMain.FormCreate(Sender: TObject);
var
  ErrorMessage: string;
begin
  FSetupComplete := False;
  FListening := False;//We're not listening yet
  FExePath := ExtractFilePath(Application.ExeName); //or ExtractFilePath(ParamStr(0))
  FVNCViewerFullPath := FExePath + VNCViewerExe;  //FExePath already has trailing \
  FStunnelFullPath := FExePath + StunnelExe;
  //Find out if we need to start listening in the OnActivate event:
  FListenOneTime := True;
  // Check parameters.
  // Host, port and name require parameters.
  ErrorMessage := Application.CheckOptions('hxp:', 'noautoconnect help helperport:');
  if Length(ErrorMessage) > 0 then
  begin
    Memo1.Append('Error: wrong command line options given:');
    Memo1.Append(ErrorMessage);
    ShowCommandLineHelp;
  end;

  if Application.HasOption('x', 'noautoconnect') then
  begin
    FListenOneTime := False;
  end;

  if Application.HasOption('h', 'help') then
  begin
    ShowCommandLineHelp;
  end;

  if Application.HasOption('p', 'helperport') then
  begin
    FPortInCommandLine := True;
  end
  else
  begin
    FPortInCommandLine := False;
  end;
end;

procedure Tcheckridehelpermain.CustomizeMenuClick(Sender: TObject);
var
  TheForm: TForm;
begin
  TheForm := clientcustomizer.TfrmClientCustomizer.Create(Application);
  try
    TheForm.ShowModal;
  finally
    TheForm.Release; //free will be done by LCL code
  end;
end;

procedure TCheckRideHelperMain.MenuLicensesClick(Sender: TObject);
var
  TheForm: TInfoAboutForm; //We need more precision than just TForm
begin
  TheForm := aboutform.TInfoAboutForm.Create(Application);
  try
    TheForm.Filename := 'License.txt';
    TheForm.ShowModal;
  finally
    TheForm.Release;
  end;
end;

procedure TCheckRideHelperMain.PortScanButtonClick(Sender: TObject);
{description Open web browser to let user portscan own machine}
const
  URL = 'http://nmap-online.com/';
begin
  OpenURL(URL);
end;

procedure TCheckRideHelperMain.QuitMenuClick(Sender: TObject);
begin
  Close;
end;

procedure TCheckRideHelperMain.Updatebuttons;
begin
  ConnectButton.Enabled := not (FListening);
  DisconnectButton.Enabled := FListening;
end;

procedure TCheckRideHelperMain.ConnectButtonClick(Sender: TObject);
begin
  if FListening = True then
  begin
    ShowMessage('Already connected. Please disconnect first.');
  end
  else
  begin
    ListenForHelp;
  end;
end;

procedure TCheckRideHelperMain.AboutClick(Sender: TObject);
var
  TheForm: TInfoAboutForm; //We need more precision than just TForm
begin
  TheForm := aboutform.TInfoAboutForm.Create(Application);
  try
    TheForm.Filename := 'readme.txt';
    TheForm.ShowModal;
  finally
    TheForm.Release;
  end;
end;

procedure TCheckRideHelperMain.DisconnectButtonClick(Sender: TObject);
begin
  if FListening = False then
  begin
    ShowMessage('Connection not started, so I can''t disconnect.');
  end
  else
  begin
    DisconnectHelp;
  end;
end;

procedure TCheckRideHelperMain.SetupConfigAndExes;
{description Unpacks resources and sets up configuration. Needs to be called once in application.}
begin
  // Get resources from this executable.
  // Note: won't be guaranteed portable to Linux/OSX
  ResourceExtract(ParamStr(0));

  if Trim(ServerPort.Text) = '' then
  begin
    if FPortInCommandLine = True then
    begin
      // Command line options override config file.
      FConnectPort := StrToInt(Trim(Application.GetOptionValue('p', 'helperport')));
      //Then we also override the rest
      FConnectHelper := 'command line options';
      FConnectHost := 'without name';
    end;
    ServerPort.Text := IntToStr(checkrideutil.FConnectPort);
  end;
  FSetupComplete := True;
end;


procedure TCheckRideHelperMain.FormActivate(Sender: TObject);
begin
  // Set up, one time only, hopefully
  if FSetupComplete = False then
  begin
    SetupConfigAndExes; //Updates FSetupComplete.
  end;
  if FListenOneTime = True then
  begin
    FListenOneTime := False; //only do it once
    ConnectButton.Enabled := False; //Don't let user click this
    DisconnectButton.Enabled := False; //This just doesn't make sense now, either
    Application.ProcessMessages; //Give Lazarus a chance to draw the screen.
    ListenForHelp; //Actual connect/listen action
  end;
end;

procedure TCheckRideHelperMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // Clean up existing connections
  DisconnectHelp;
  // Clean up temp dir
  CleanTempDir;
end;

procedure TCheckRideHelperMain.DisconnectHelp;
var
  ExitCode: integer = 0;
begin
  Screen.Cursor := crHourglass;
  DisconnectButton.Enabled := False; //Don't let user click any more
  try
    // Stop/kill viewer
    if VNCViewerProcess.Running = True then
    begin
      VNCViewerProcess.Terminate(ExitCode);
    end;

    //Stop/kill tunnel
    if TunnelProcess.Running = True then
    begin
      TunnelProcess.Terminate(ExitCode);
    end;

    //Check again after spending some time closing stunnel
    if VNCViewerProcess.Running = True then
    begin
      //Wait a bit for viewer to close
      Application.ProcessMessages;
      Sleep(200);
      if VNCViewerProcess.Running = True then
      begin
        //If it won't go nicely, then force it...
        PostMessage(VNCViewerProcess.Handle, WM_QUIT, 0, 0);
        Application.ProcessMessages;
        Sleep(500);
        if VNCViewerProcess.Running = True then
        begin
          if TerminateProcess(VNCViewerProcess.Handle, 255) then
          begin
            Memo1.Append('Finished killing ' + VNCViewerExe + ' process.');
          end
          else
          begin
            Memo1.Append('Error killing ' + VNCViewerExe +
              ' process. Please stop the program yourself.');
          end;
        end;
      end
      else
      begin
        Memo1.Append('Finished stopping ' + VNCViewerExe);
      end;
    end
    else
    begin
      Memo1.Append('Finished stopping ' + VNCViewerExe);
    end;

    //Check after possibly spending some time closing
    //VNCViewer
    if TunnelProcess.Running = True then
    begin
      //Wait a bit for tunnel to close
      Application.ProcessMessages;
      Sleep(200);
      if TunnelProcess.Running = True then
      begin
        // Stop stunnel
        if SysUtils.ExecuteProcess(FindDefaultExecutablePath('taskkill.exe'),
          ' /f /t /im ' + StunnelExe + '') <> 0 then
        begin
          Memo1.Append('Error running taskkill.exe /f /t /im ' + StunnelExe);
        end
        else
        begin
          Memo1.Append('Finished killing stunnel');
        end;
      end
      else
      begin
        Memo1.Append('Finished stopping ' + StunnelExe);
      end;
    end
    else
    begin
      //Tunnel had already stopped
      Memo1.Append('Finished stopping ' + StunnelExe);
    end;

    try
      CleanSystemTray; //Get rid of zombie icons
    except
      on E: Exception do
      begin
        Memo1.Append('Small problem cleaning up icons. Details:' +
          E.ClassName + '/' + E.Message);
      end;
    end;

    FListening := False;
    Memo1.Append('Remote support session ended.');
    UpdateButtons;
    Screen.Cursor := crDefault;
  except
    on E: Exception do
    begin
      FListening := False; //Let's assume things were closed
      UpdateButtons;
      Memo1.Append('Error running commands; error was ' + E.ClassName + '/' + E.Message);
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TCheckRideHelperMain.ShowCommandLineHelp;
begin
  Memo1.Append('Command line options:');
  Memo1.Append('-h or --help: show this information.');
  Memo1.Append('-x or --noautoconnect: don''t connect/listen automatically.');
  Memo1.Append(
    '-p or --helperport: helper port number that we listen on. Overrides CheckRide.conf');
end;

procedure TCheckRideHelperMain.ServerPortEditingDone(Sender: TObject);
begin
  CheckRideUtil.FConnectPort := StrToIntDef(ServerPort.Text, 3334);
end;

procedure TCheckRideHelperMain.WhatIsMyIPButtonClick(Sender: TObject);
{description Open web browser to show external IP}
const
  URL = 'http://automation.whatismyip.com/n09230945.asp';
begin
  OpenURL(URL);
end;

procedure TCheckRideHelperMain.ListenForHelp;
{description This does the actual work }
var
  VNCViewerParameters: string;
  LogFile: string;
begin
  // note: include space in front of option
  // interesting options gleaned from vncviewer -help
  // /8bit or /64colors: for improved connection speed.
  // Don't know what enablecache does, but it sounds good!
  // /listen 33334 listens on specified port, so you shouldn't have anything listening on that port
  // /proxy proxyhost [portnum]
  // /encoding zrle|zywrle|tight|zlib|zlibhex|ultra => also found raw rre corre hextile ultra2 in source (VNCOptions.cpp, near line 672)
  // ultra2 might be useful; doesn't seem to work well though, see below
  // /autoacceptincoming automatically accept incoming connections
  // /socketkeepalivetimeout n
  // /enablecache
  // /autoacceptnodsm: useful for ignoring encryption if you have specified a DSM. Not useful to us ;)
  // Finally, we have to specify /quickoption 8 to force manual parameters instead of auto.
  // /quickoption
  // 1: auto mode (zrle, full colors, cache)
  // 2: LAN (hextile, full colors, no cache)
  // 3: medium (zrle, 256 colors, no cache)
  // 4: modem (zrle, 64 colors, cache)
  // 5: slow (zrle, 8 colors, cache)
  // 7: ULTRA_LAN (ultra enc, full color)
  // 8: apparently manual
  // apparently you can also disable auto mode by specifying noauto
  // NOTE: at least in 1.0.9.6.1 and earlier:
  // vncviewer -listen 65001 -quickoption 8 -8bit -encoding ultra2 -enablecache -disablesponsor -autoacceptincoming -autoacceptnodsm -loglevel 10 -logfile C:\<somewhere>TMP00015.tmp
  // seems to give a crash in WinVNC: an unhandled Win32 exception occurred in WinVNC [724]
  //
  // Previous versions of this code used quickoption 3, which caused text entries/fonts in command windows to not appear on the screen
  // and other update anomalies.
  LogFile := SysUtils.GetTempFileName();
  VNCViewerParameters :=
    ' ' + '-listen ' + IntToStr(VNCViewerListenPort) + ' -quickoption 8 ' +
    ' -encoding zywrle ' +
    ' -disablesponsor -autoacceptincoming -autoacceptnodsm -loglevel 10 -logfile '
    + LogFile;
  if TunnelProcess.Running = True then
  begin
    ShowMessage('Tunnel has already been started. Stop tunnel before connecting.');
    Exit;
  end;

  ConnectButton.Enabled := False; //Don't let user click twice
  Memo1.Append('Reading profile ' + FConnectHelper + ' for server ' +
    FConnectHost + ' listening on port ' + IntToStr(FConnectPort));
  // Start up stunnel asynchronously: let it run in parallel with the rest of the program
  try
    TunnelProcess.CommandLine :=
      FStunnelFullPath + ' ' + CustomStunnelconfig(Helper) + '';
    TunnelProcess.Execute;
    Memo1.Append('Finished setting up SSL/TLS tunnel listening on port ' +
      IntToStr(CheckRideUtil.FConnectPort));
  except
    Screen.Cursor := crDefault;
    Memo1.Append('Error running ' + FStunnelFullPath + ' stunnelhelper.conf');
    exit; //exit procedure, useless to continue
  end;

  try
    if TunnelProcess.Running = False then;
    begin
      Memo1.Append('Waiting 5 seconds for stunnel to come up.');
      sleep(5000);
    end;

    // Start listening viewer. Don't use service parameter for this
    try
      VNCViewerProcess.CommandLine := FVNCViewerFullPath + VNCViewerParameters;
      VNCViewerProcess.Execute;
      FListening := True;
      Memo1.Append('Finished setting up VNCViewer listening to tunnel.');
      Memo1.Append('Information: parameters used: ' + VNCViewerParameters);
      Memo1.Append('VNCViewer log file: ' + logfile);
    except
      FListening := False;
      Screen.Cursor := crDefault;
      Memo1.Append('Error running ' + FVNCViewerFullPath + ' ' + VNCViewerParameters);
      exit; //exit procedure, useless to continue
    end;
    UpdateButtons;
    Memo1.Append('Listening VNCViewer started...');
    Screen.Cursor := crDefault;
  except
    on E: Exception do
    begin
      //Assume listening so we can clean up
      FListening := True;
      Screen.Cursor := crDefault;
      Memo1.Append('Error running commands; error was ' + E.ClassName + '/' + E.Message);
      Memo1.Append('Cleaning up.');
      DisconnectHelp;
    end;
  end;
end;

end.

