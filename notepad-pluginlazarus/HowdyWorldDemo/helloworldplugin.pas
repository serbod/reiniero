unit helloworldplugin;

{$MODE Delphi}

interface

uses
  NppPlugin, NPPTypes,
  Classes,
  Dialogs,
  SysUtils, LCLIntf, LCLType, SciSupport,
  AboutForms, HelloWorldDockingForms, windows;

type
  { THelloWorldPlugin }

  THelloWorldPlugin = class(TNppPlugin)
  private
    // A demo form
    HelloWorldDockingForm : THelloWorldDockingForm;

  protected
    procedure SetToolbarIcon(out pToolbarIcon: TToolbarIcons); override;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure HelloWorld;
    procedure HelloWorldDocking;
    procedure About;
  end;


implementation

{ NPP interface functions }
procedure _HelloWorld; cdecl;
begin
  (GetNPPPluginInstance as THelloWorldPlugin).HelloWorld;
end;

procedure _About; cdecl;
begin
  (GetNPPPluginInstance as THelloWorldPlugin).About;
end;

procedure _HelloWorldDocking; cdecl;
begin
  (GetNPPPluginInstance as THelloWorldPlugin).HelloWorldDocking;
end;

procedure _GetWord; cdecl;
begin
  (GetNPPPluginInstance as THelloWorldPlugin).GetWord;
end;

{ THelloWorldPlugin }

constructor THelloWorldPlugin.Create;
begin
  inherited;
  PluginName := 'Howdy &World NPP Plugin Demo';

  AddFunction('Replace Text',         _HelloWorld, #118, [ssCtrl, ssAlt]);
  AddFunction('Docking window test',  _HelloWorldDocking);
  AddFunction('-');
  AddFunction('About',                _About);
end;

destructor THelloWorldPlugin.Destroy;
begin
  if assigned(HelloWorldDockingForm) then
    HelloWorldDockingForm.Free;
  inherited Destroy;
end;

procedure THelloWorldPlugin.HelloWorld;
var
  s: string;
begin
  s := 'Howdy World';
  GetNPPPluginInstance.SendToNppScintilla(SCI_REPLACESEL, 0, LPARAM(PChar(s)));
end;


{ Note there are potentially serious problems with multiple instances and
memory leaks if you try to make TNppForm modal.

see http://forum.lazarus.freepascal.org/index.php/topic,26149.0.html
see also TAboutForm}
procedure THelloWorldPlugin.About;
begin
  // Make sure there is only one instance
  if (not Assigned(AboutForm)) then
  begin
    AboutForm := TAboutForm.Create;
    AboutForm.Show;
  end
  else
    AboutForm.Show;
end;


procedure THelloWorldPlugin.HelloWorldDocking;
begin
  if (not Assigned(HelloWorldDockingForm)) then
    HelloWorldDockingForm := THelloWorldDockingForm.Create(1);

  // Populate the docked form with some information about the file
  with HelloWorldDockingForm do
  begin
    Memo1.Clear;
    Memo1.Append('** Information current open file **');
    Memo1.Append('Plugins path = ' + GetPluginsConfigDir + LineEnding +
      'Filename     = ' + GetSourceFilenameNoPath + LineEnding +
      'Full path    = ' + GetSourceFilename
      );
  end;
  HelloWorldDockingForm.Show;
end;

procedure THelloWorldPlugin.SetToolbarIcon(out pToolbarIcon: TToolbarIcons);
begin
  pToolbarIcon.ToolbarBmp := LoadImage(Hinstance, 'IDB_TB_TEST', IMAGE_BITMAP,
    0, 0, (LR_DEFAULTSIZE or LR_LOADMAP3DCOLORS));
end;

end.
