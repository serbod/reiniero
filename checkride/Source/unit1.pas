unit Unit1;

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
  { TCheckRideMain }
  TCheckRideMain = class(TForm)
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    FileMenu: TMenuItem;
    HelpMenu: TMenuItem;
    AboutMenu: TMenuItem;
    MenuLicense: TMenuItem;
    QuitMenu: TMenuItem;
    TunnelProcess: TAsyncProcess;
    VNCGUIProcess: TAsyncProcess;
    ConnectButton: TButton;
    DisconnectButton: TButton;
    ServerPort: TLabeledEdit;
    ServerName: TLabeledEdit;
    procedure AboutMenuClick(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ConnectForHelp;
    procedure DisconnectHelp;
    procedure ShowCommandLineHelp;
    procedure MenuLicenseClick(Sender: TObject);
    procedure QuitMenuClick(Sender: TObject);
    procedure ServerNameEditingDone(Sender: TObject);
    procedure ServerPortEditingDone(Sender: TObject);
  private
    FSetupComplete: boolean;
    FHostInCommandLine: boolean; //Does user want to override host using command line?
    FPortInCommandLine: boolean; //Does user want to override port using command line?
    FNameInCommandLine: boolean;
    //Does user want to override helper name using command line?
    FConnected: boolean; //whether or not the connection is started
    FConnectOneTime: boolean; //Should we connect in the OnActivate event?
    FVNCFullPath: string;
    FStunnelFullPath: string;
    FVNCServiceAlreadyExisted: boolean;
    FWeInstalledVNCService: boolean; //Used for cleanup
    FVNCServiceWasRunning: boolean; //Shows if VNC service was already running.
    procedure SetupConfigAndExes;
    procedure Updatebuttons;
    { private declarations }
  public
    { public declarations }
  end;

var
  CheckRideMain: TCheckRideMain;

implementation

uses
  Windows, CheckRideUtil, aboutform;

{$R *.lfm}
// Use explicit manifest file created so we have elevated (admin) privileges on
// Windows systems with UAC on.
// Note: disable in Project Options, uncheck the "Use manifest file to enable themes (windows only)" checkbox
{$R manifest.rc}


{ TCheckRideMain }
const
  VNCExe = 'winvnc.exe';
  UltraVNCServiceName = 'uvnc_service';
  StunnelExe = 'stunnel.exe';

procedure TCheckRideMain.ShowCommandLineHelp;
begin
  Memo1.Append('Command line options:');
  Memo1.Append('-h or --help: show this information.');
  Memo1.Append('-x or --noautoconnect: don''t connect automatically.');
  Memo1.Append('-i or --helperhost: host name or IP address of helper. Overrides CheckRide.conf');
  Memo1.Append('-p or --helperport: helper port number. Overrides CheckRide.conf');
  Memo1.Append('-n or --helpername: helper name to be displayed. Overrides CheckRide.conf');
end;

procedure TCheckRideMain.FormCreate(Sender: TObject);
var
  ErrorMessage: string = '';
begin
  FSetupComplete := False; //Run setup later, in activate
  FConnected := False;
  FWeInstalledVNCService := False; //We haven't installed the VNC service... yet
  FVNCServiceAlreadyExisted := False; //We haven't detected any existing VNC service...
  FVNCServiceWasRunning := False; // .. so it isn't running either, as far as we know.
  FVNCServiceAlreadyExisted := False;
  FConnectOneTime := True;

  // Check parameters.
  // Host, port and name require parameters.
  ErrorMessage := Application.CheckOptions(
    'hxi:p:n:', 'noautoconnect help helperhost: helperport: helpername:');
  if Length(ErrorMessage) > 0 then
  begin
    Memo1.Append('Error: wrong command line options given.');
    Memo1.Append(ErrorMessage);
    ShowCommandLineHelp;
  end;

  if Application.HasOption('x', 'noautoconnect') then
  begin
    FConnectOneTime := False;
  end;

  if Application.HasOption('h', 'help') then
  begin
    ShowCommandLineHelp;
  end;

  // We start with name as we might need to overrule
  // FNameInCommandLine:=false later on.
  if Application.HasOption('n', 'helpername') then
  begin
    FNameInCommandLine := True;
  end
  else
  begin
    FNameInCommandLine := False;
  end;

  if Application.HasOption('i', 'helperhost') then
  begin
    FHostInCommandLine := True;
    FNameInCommandLine := True; //Seems logical, it's a different host now.
  end
  else
  begin
    FHostInCommandLine := False;
  end;

  if Application.HasOption('p', 'helperport') then
  begin
    FPortInCommandLine := True;
    FNameInCommandLine := True; //Seems logical, it's a different host now.
  end
  else
  begin
    FPortInCommandLine := False;
  end;
end;

procedure TCheckRideMain.SetupConfigAndExes;
{description Unpacks resources and sets up configuration. Needs to be called once in application.}
begin
  ResourceExtract(ParamStr(0)); //Extract resources, read config etc.
  FVNCFullPath := FResourceDir + DirectorySeparator + VNCExe;
  FStunnelFullPath := FResourceDir + DirectorySeparator + StunnelExe;
  if Trim(ServerName.Text) = '' then
  begin
    if FHostInCommandLine = True then
    begin
      // Command line options override config file.
      FConnectHost := Trim(Application.GetOptionValue('i', 'helperhost'));
      FConnectHelper := 'helper'; //default value
    end;
    ServerName.Text := FConnectHost;
  end;
  if Trim(ServerPort.Text) = '' then
  begin
    if FPortInCommandLine = True then
    begin
      // Command line options override config file.
      FConnectPort := StrToInt(Trim(Application.GetOptionValue('p', 'helperport')));
      FConnectHelper := 'helper'; //default value
    end;
    ServerPort.Text := IntToStr(FConnectPort);
  end;
  if FNameInCommandLine = True then
  begin
    FConnectHelper := Application.GetOptionValue('n', 'helpername');
  end;
  FSetupComplete := True;
end;

procedure TCheckRideMain.ConnectButtonClick(Sender: TObject);
begin
  if FConnected = True then
  begin
    ShowMessage('Already connected. Please disconnect first.');
  end
  else
  begin
    ConnectForHelp;
  end;
end;

procedure TCheckRideMain.AboutMenuClick(Sender: TObject);
var
  TheForm: TInfoAboutForm;
begin
  TheForm := aboutform.TInfoAboutForm.Create(Application);
  try
    TheForm.FileName := 'Readme.txt';
    TheForm.ShowModal;
  finally
    TheForm.Release;
  end;
end;

procedure TCheckRideMain.DisconnectButtonClick(Sender: TObject);
begin
  if FConnected = False then
  begin
    ShowMessage('Connection not started, so I can''t disconnect.');
  end
  else
  begin
    DisconnectHelp;
  end;
end;

procedure TCheckRideMain.FormActivate(Sender: TObject);
begin
  // Set up, one time only, hopefully
  if FSetupComplete = False then
  begin
    SetupConfigAndExes; //Updates FSetupComplete.
  end;
  // Implement autoconnect after form is shown for the first time
  if FConnectOneTime = True then
  begin
    FConnectOneTime := False; //Don't start this again
    ConnectButton.Enabled := False; //Don't let user click this
    DisconnectButton.Enabled := False; //This just doesn't make sense now, either
    Application.ProcessMessages; //Give Lazarus a chance to draw the screen.
    ConnectForHelp;
  end;
end;

procedure TCheckRideMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // Clean up existing connections
  DisconnectHelp;
  // Clean up temp dir
  CleanTempDir;
end;

procedure TCheckRideMain.DisconnectHelp;
{description Disconnect existing sessions, clean up}
const
  SleepTimeOut = 4; //Wait this number of times for service to stop
var
  i: integer;
begin
  Screen.Cursor := crHourglass;
  try
    Memo1.Append('Stopping connections to helper.');
    // Stop connection
    if SysUtils.ExecuteProcess(FVNCFullPath, ' -stopreconnect') <> 0 then
    begin
      Memo1.Append('Step 1 of 7: Error running winvnc -stopreconnect');
    end
    else
    begin
      Memo1.Append('Step 1 of 7: VNC connection attempts stopped.');
    end;

    if FWeInstalledVNCService = True then
    begin
      // Stop vnc service, if we have installed it
      if SysUtils.ExecuteProcess(FVNCFullPath, ' -stopservice') <> 0 then
      begin
        Memo1.Append('Step 2 of 7: Error running winvnc -stopservice');
      end
      else
      begin
        Memo1.Append('Step 2 of 7: Temporary VNC service stopped.');
      end;

      // Uninstall service, if we have installed it.
      if SysUtils.ExecuteProcess(FVNCFullPath, ' -uninstall') <> 0 then
      begin
        Memo1.Append('Step 3 of 7: Error running winvnc -uninstall');
      end
      else
      begin
        Memo1.Append('Step 3 of 7: Temporary VNC service uninstalled.');
        FWeInstalledVNCService := False; //Now it's not installed anymore
      end;
    end
    else
    begin
      if FVNCServiceAlreadyExisted = True then
      begin
        if FVNCServiceWasRunning = False then
        begin
          // Leave it as we found it:
          StopService(UltraVNCServiceName);
          Memo1.Append(
            'Step 3 of 7: Stopped existing VNC service, to leave it as we found it.');
        end;
      end;
    end;

    if VNCGUIProcess.Running = True then
      // Kill VNC trayicon GUI
    begin
      VNCGUIProcess.Terminate(ExitCode);
      //We'll wait for it to quit below, so no need to wait here.
      Memo1.Append('Step 4 of 7: VNC tray icon closed.');
    end;

    if TunnelProcess.Running = True then
    begin
      // Stop our stunnel process
      TunnelProcess.Terminate(ExitCode);
      // Wait some, apparently needed for terminate to filter through.
      Sleep(100); //Should be long enough to get some work done
      Application.ProcessMessages;//for good measure
      //DON'T MESS WITH FWeStartedStunnel; we'll need it below
      Memo1.Append('Step 5 of 7: Asked SSL/TLS tunnel to stop.');
    end;

    // Make sure that the VNC service is gone after uninstalling
    if FWeInstalledVNCService = True then
    begin
      if IsServiceRunning(UltraVNCServiceName) = True then
      begin
        // Wait for service to stop
        Memo1.Append('Step 6 of 7: Temporary VNC service is still running.');
        i := 0;
        repeat
          sleep(500);
          Application.ProcessMessages; //Handle GUI events
          i := i + 1;
          Memo1.Append('Step 6 of 7: Waiting for temporary VNC service to stop (' +
            IntToStr(i) + ')');
          if i >= SleepTimeOut then
          begin
            Memo1.Append(
              'Step 6 of 7: Giving up waiting for temporary VNC service to stop.');
            break;
          end;
        until IsServiceRunning(UltraVNCServiceName) = False;
      end;
    end;

    // Now check that stunnel is down
    // note we can check for process names, but what if there are 2 stunnels?
    if TunnelProcess.Running = True then
    begin
      Memo1.Append(
        'Step 7 of 7: Error stopping SSL/TLS tunnel; trying to kill all stunnel.exes with taskkill.');
      // Stop stunnel process forcefully
      PostMessage(TunnelProcess.Handle, WM_QUIT, 0, 0);
      Application.ProcessMessages;
      Sleep(500);
      if TunnelProcess.Running = True then
      begin
        if TerminateProcess(TunnelProcess.Handle, 255) then
        begin
          Memo1.Append('Step 7 of 7: Finished stopping SSL/TLS tunnel.');
        end
        else
        begin
          Memo1.Append('Step 7 of 7: Error stopping SSL/TLS tunnel: ' + StunnelExe);
        end;
      end;
    end
    else
    begin
      Memo1.Append('Step 7 of 7: SSL/TLS tunnel is stopped.');
    end;

    try
      CleanSystemTray; //Get rid of zombie icons
    except
      on E: Exception do
      begin
        Memo1.Append('Step 7 of 7: small problem cleaning up icons. Details:' +
          E.ClassName + '/' + E.Message);
      end;
    end;

    FConnected := False;
    //might not be totally true if disconnect attempts above failed...

    Memo1.Append('Disconnected.');
    Updatebuttons;
    Screen.Cursor := crDefault;
  except
    on E: Exception do
    begin
      Memo1.Append('Error running commands; error was ' + E.ClassName + '/' + E.Message);
      Updatebuttons;
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TCheckRideMain.MenuLicenseClick(Sender: TObject);
var
  TheForm: TInfoAboutForm; //TForm is not specialised enough
begin
  TheForm := aboutform.TInfoAboutForm.Create(Application);
  try
    TheForm.FileName := 'License.txt';
    TheForm.ShowModal;
  finally
    TheForm.Release;
  end;
end;

procedure TCheckRideMain.QuitMenuClick(Sender: TObject);
begin
  Close;
end;

procedure TCheckRideMain.ServerNameEditingDone(Sender: TObject);
begin
  CheckRideUtil.FConnectHost := Trim(ServerName.Text);
end;

procedure TCheckRideMain.ServerPortEditingDone(Sender: TObject);
begin
  CheckRideUtil.FConnectPort := StrToIntDef(ServerPort.Text, 3334);
end;

procedure TCheckRideMain.Updatebuttons;
begin
  ConnectButton.Enabled := not (FConnected);
  DisconnectButton.Enabled := FConnected;
end;

procedure TCheckRideMain.ConnectForHelp;
{description Connect to helper}
{ TODO 6 -oAnyone -cNice to have: refactor vnc into separate unit/class, also stunnel }
const
  // Note the leading space to separate command from options/parameters
  // Note that this "autoreconnect" param MUST be BEFORE the "connect" on the command line
  // (see UltraVNC winvnc.cpp)
  // Officially, I think you should run -servicehelper, but that doesn't seem
  // to do anything.
  VNCParameters = ' -autoreconnect -connect 127.0.0.1::65000';
  SleepTimeOut = 4;
var
  i: integer;

  procedure PrepareToExit(Message: string);
  {description Revert GUI level changes so we can exit the procedure cleanly}
  begin
    Screen.Cursor := crDefault;
    UpdateButtons;
    Memo1.Append(Message);
  end;

begin
  ConnectButton.Enabled := False; //Don't let user click twice.
  Screen.Cursor := crHourglass;
  Memo1.Append('Setting up connection to helper ' + FConnectHelper +
    ' at ' + FConnectHost + ':' + IntToStr(FConnectPort) + '.');
  // Start up stunnel asynchronously: let it run in parallel with the rest of the program
  try
    TunnelProcess.CommandLine :=
      FStunnelFullPath + ' ' + CustomSTunnelconfig(Helped) + '';
    TunnelProcess.Execute;
    Memo1.Append('Step 1 of 5: Started SSL/TLS tunnel.');
  except
    PrepareToExit('Step 1 of 5: Error running ' + FStunnelFullPath +
      ' ' + CustomSTunnelConfig(Helped) + '');
    exit; //exit procedure, useless to continue
  end;

  try
    if ServiceExists(UltraVNCServiceName) then
    begin
      FVNCServiceAlreadyExisted := True;
      if IsServiceRunning(UltraVNCServiceName) then
      begin
        FVNCServiceWasRunning := True;
        Memo1.Append('Step 2 of 5: Existing running VNC service detected.');
      end
      else
      begin
        FVNCServiceWasRunning := False;
        Memo1.Append('Step 2 of 5: Existing VNC service detected.');
      end;
    end
    else
    begin
      FVNCServiceAlreadyExisted := False;
    end;
  except
    on E: Exception do
    begin
      //do nothing, just report the error
      Memo1.Append('Error checking existing VNC service. Details: ' +
        E.ClassName + '/' + E.Message);
    end;
  end;

  try
    // Install vnc service, required for ctr-alt-del support on at least Vista+
    FWeInstalledVNCService := False; //default
    if FVNCServiceAlreadyExisted = False then
    begin
      // Only do this if there isn't a vnc service already present
      Memo1.Append('Step 2 of 5: Installing temporary VNC service.');
      if SysUtils.ExecuteProcess(FVNCFullPath, ' -install') <> 0 then
      begin
        Memo1.Append('Error running ' + FVNCFullPath + ' -install');
      end
      else
      begin
        FWeInstalledVNCService := True;
        Memo1.Append('Step 2 of 5: Finished installing temporary VNC service.');
      end;
    end
    else
    begin
      // Service already existed.
      if FVNCServiceWasRunning = False then
      begin
        // Start up VNC service
        CheckRideUtil.StartService(UltraVNCServiceName);
        Memo1.Append(
          'Step 2 of 5: Found and started existing VNC service. This might not work, though.');
      end;
    end;

    if TunnelProcess.Running = True then
    begin
      //Let's not get the user confused.
      //Memo1.Append('Tunnel program running.');
    end
    else
    begin
      Memo1.Append('Step 3 of 5: Tunnel not running yet. We might have problems later on.');
    end;

    //Wait for vnc service to come up
    if IsServiceRunning(UltraVNCServiceName) = False then
    begin
      // Wait for service to come up
      i := 0;
      repeat
        sleep(500);
        Application.ProcessMessages; //Handle
        i := i + 1;
        Memo1.Append('Step 3 of 5: Waiting for temporary VNC service to start up (' +
          IntToStr(i) + ')');
        if i >= SleepTimeOut then
          break;
      until IsServiceRunning(UltraVNCServiceName) = True;
    end
    else
    begin
      Memo1.Append('Step 3 of 5: Temporary VNC service is running.');
    end;

    // Now, it seems we need some extra wait time otherwise
    // vnc command won't work
    Memo1.Append('Step 4 of 5: Waiting 10 seconds for temporary VNC service to get ready.');
    Application.ProcessMessages;
    sleep(10000);
    Application.ProcessMessages;

    // Actual connect. Don't use service parameter for this
    try
      VNCGUIProcess.CommandLine :=
        FVNCFullPath + ' ' + VNCParameters + '';
      VNCGUIProcess.Execute;
      FConnected := True;
      Memo1.Append('Step 5 of 5: Started VNC connection attempt.');
      Memo1.Append('Done.');
      Memo1.Append('Waiting for helper to take over...');
      PrepareToExit('If helper takes over, the VNC eye symbol close to the clock will change color.');
    except
      PrepareToExit('Step 1 of 5: Error running ' + FVNCFullPath +
        ' ' + VNCParameters + '');
      exit; //exit procedure, useless to continue
    end;

  except
    on E: Exception do
    begin
      PrepareToExit('Error running commands; error was ' + E.ClassName +
        '/' + E.Message);
      Memo1.Append('Cleaning up.');
      DisconnectHelp;
    end;
  end;
end;

end.

