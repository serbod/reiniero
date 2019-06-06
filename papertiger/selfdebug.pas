unit selfdebug;

//see
//http://www.hu.freepascal.org/lists/fpc-pascal/2011-October/030752.html

{
Instructions:
-Include unit anyware in the program.
-Change DEBUGGER constant to match your debugger
-For Windows, if the program to debug is running as a service (fe. CGI application
 from apache running as a service), make sure the service is configured with
 "Interact with desktop" checked. Failing to do so, the debugger will be started
 without a console, break the program and hang waiting for input. Killing the debugger
 is your only option.
-For Linux, if the program to debug is running as a different user (fe. CGI application),
 run "xhost +" in a terminal to enable all users to connect to xserver.
 If needed, change DISPLAY to match your DISPLAY environment variable
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process;

implementation

const
  {$ifdef windows}
  DEBUGGER = 'C:\lazarus\mingw\bin\7.3\gdb.exe';
  {$endif}
  {$ifdef linux}
  // Debuggers: e.g. gdb, gdbtui or ddd
  DEBUGGER = 'ddd';
  DISPLAY = ':0.0'; //modify according to your setup
  {$endif}
  MSWAIT = 2000;

var
  AProcess: TProcess;

initialization
  AProcess := TProcess.Create(nil);
{$ifdef windows}
  AProcess.CommandLine := format('cmd /C START "Debugging %s" /WAIT "%s" "%s" %d"', [ParamStr(0), debugger, ParamStr(0), GetProcessID]);
{$endif}
{$ifdef linux}
  AProcess.CommandLine := format('xterm -display %s -T "Debugging %s" -e "%s" "%s" %d', [DISPLAY, ParamStr(0),
    DEBUGGER, ParamStr(0), GetProcessID]);
{$endif}
  AProcess.Execute;
  sleep(MSWAIT);

finalization
  AProcess.Free;
end.
