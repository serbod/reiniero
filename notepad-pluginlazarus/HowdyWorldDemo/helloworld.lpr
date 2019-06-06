library helloworld;

{$MODE Delphi}

{
  Please pass string information to NPP
  using PChar, PWideChar or ShortString parameters.

  If you want to use this for ANSI Notepad++:
  * remove "-dNPPUnicode" in the project settings/other section.
  * remove "-dNPPUnicode" from the the NotepadPPPlugin package.

  Compiles with Lazarus 1.x (SVN or stable), FPC 2.6.x or higher (SVN or stable) for x86.
  Initial version     : August 2012
  Rework and packaging: November 2012
}

{$R 'helloworldres.rc'}

uses
  SysUtils,
  Classes,
  Types,
  Interfaces, LCLIntf, LCLType, LMessages, Forms,
  Messages,
  nppplugin,
  helloworldplugin in 'helloworldplugin.pas',
  AboutForms in 'AboutForms.pas' {AboutForm},
  windows;

// Notepad++ DLL interface functions that need exporting.
// Copy of the text in NppPluginExport.inc.
exports
  setInfo, getName, getFuncsArray, beNotified, messageProc
  {$IFDEF NPPUNICODE}
  , isUnicode
  {$ENDIF}
  ;

begin
  // Register our own class as the plugin class.
  NPPInitialize(THelloWorldPlugin);

  { Continue initialization }
  Application.Initialize;
end.

