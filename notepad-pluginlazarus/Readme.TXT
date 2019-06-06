Welcome, future developer of Notepad++ plugins (in Lazarus)!

We present a port of the Delphi demo plugin/template plugin for Notepad++ to Lazarus.

Started with Delphi Unicode plugin from 
http://sourceforge.net/projects/npp-plugins/files/DelphiPluginTemplate/DelphiPluginTemplate%201.0/

Just as the Delphi plugin, it's GPL licensed.

If you want to use this for ANSI Notepad++, please remove the -dNPPUnicode in the project settings/other sectino.

Compiles with Lazarus 1.1 (SVN), FPC 2.6.2+ and FPC 2.7.1 x86.
August 2012
Ludo Brands & Reinier Olislagers

Updated by eny
November 2012

Readme from the original Delphi version:

Welcome, future developer of Notepad++ plugins (in Delphi)!

This is a base framework that will help you create Notepad++ plugins.
The framework itself it stored in the 'lib' subdirectory, you mostly have to
worry about the stuff that is in the root directory. Most of the DLL
initialization has been moved to framework files, so you don't even have to
bother with this. The thing you need to do is:
 - Create a new project
 - Add all framework files to the project
 - In your project file add {$Include 'lib\NppPluginInclude.pas'} and inside begin end. add DllProc := @DLLEntryPoint; DLLEntryPoint(DLL_PROCESS_ATTACH);
 - Create a new unit that will extend NppPlugin class
 - In the interface of your plugin unit create a variable Npp of your plugin class (ex: var Npp: THelloWorldPlugin;).
 - At the end of that unit add initialization Npp := TYourExcelentPlugin.Create;
 - You can use TNppForm and TNppDockingForm as base for your forms. Remember to use the correct constructor!
 - Create your windows when the are requested by the user via one of the func calls. Notepad++ will also use this when it starts up.
 - Never create windows in the costructor or before setInfo is called.
Other stuff that might be interesting to extend in your plugin:
 - BeNotified (NPPN_TB_MODIFICATION - extend DoNppnToolbarModification, NPPN_SHUTDOWN - extend DoNppnShutdown ...)
 - MessageProc (WM_CREATE...)
Notes:
 - I'd like to make this plugin more in a way that real Delphi stuff is made (usage of ObjectInpector, not creating new constructors...) but that would require me to make an installable component. I hate this. If anyone wants to make this for me...
 
Any suggestions or questions: zobo@users.sf.net