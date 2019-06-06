unit inieditexampleform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, inieditor;

type

  { TForm1 }

  TForm1 = class(TForm)
    Inieditrunbutton: TButton;
    FileNameEdit1: TFileNameEdit;
    procedure InieditrunbuttonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.InieditrunbuttonClick(Sender: TObject);
var
  Ini: TFormIniEditor;
begin
  Ini:=TFormIniEditor.Create(self);
  try
    Ini.ProfileSelectSection:='Database';
    Ini.ProfileSelectKey:='type';
    Ini.INIFile:=FileNameEdit1.FileName;
    if Ini.ShowModal=mrOK then
      ShowMessage('The user selected database type '+Ini.ProfileSelectValue)
    else
      ShowMessage('The user cancelled.');
  finally
    Ini.Release;
  end;
end;

end.

