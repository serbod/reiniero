unit imageformunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TImageForm }

  TImageForm = class(TForm)
    btnPrevious: TButton;
    btnNext: TButton;
    ScanImage: TImage;
    procedure btnNextClick(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ImageForm: TImageForm;

implementation

{$R *.lfm}

{ TImageForm }

procedure TImageForm.btnPreviousClick(Sender: TObject);
begin
  showmessage('todo: implement me');
end;

procedure TImageForm.btnNextClick(Sender: TObject);
begin
  showmessage('todo: implement me');
end;

end.

