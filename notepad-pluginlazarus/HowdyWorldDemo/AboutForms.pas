unit AboutForms;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, NppForms, StdCtrls;

type

  { TAboutForm }

  TAboutForm = class(TNppForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure Label4MouseEnter(Sender: TObject);
    procedure Label4MouseLeave(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.Label4Click(Sender: TObject);
begin
  //
end;

{Note there are potential problems with multiple instances and memory leaks if
you try to make a TNppForm modal. This is because the launching of a new process
is controlled by Notepad++, not the plugin.
Since Np++ is not subject to the plugin's modal mode, the only effect of
declaring an object modal is to make it impossible to free the object from
memory if Np++ is closed before the object.

see http://forum.lazarus.freepascal.org/index.php/topic,26149.0.html
}


procedure TAboutForm.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TAboutForm.Label4MouseEnter(Sender: TObject);
begin
  Label4.Font.Style := Font.Style + [fsUnderline];
end;

procedure TAboutForm.Label4MouseLeave(Sender: TObject);
begin
  Label4.Font.Style := Font.Style - [fsUnderline];
end;

end.
