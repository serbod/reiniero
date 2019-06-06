unit aboutform;

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TInfoAboutForm }

  TInfoAboutForm = class(TForm)
    CloseButton: TButton;
    InfoMemo: TMemo;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    FFileName: string;
    FDoOnce: boolean;
  public
    { public declarations }
    property Filename: string read FFileName write FFileName;
  end;

var
  InfoAboutForm: TInfoAboutForm;

implementation

uses
  CheckRideUtil;

{$R *.lfm}

{ TInfoAboutForm }

procedure TInfoAboutForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TInfoAboutForm.FormActivate(Sender: TObject);
begin
  if FDoOnce then
  begin
    try
      InfoMemo.Lines.LoadFromFile(FFileName); //assumes file in same dir
    except
      try
        InfoMemo.Lines.LoadFromFile(FResourceDir + DirectorySeparator + FFileName);
        //Load from resource extract dir.
      except
        InfoMemo.Lines.Text := 'Sorry, could not load file ' + FFileName;
      end;
    end;
    FDoOnce := False;
  end;
end;

procedure TInfoAboutForm.FormCreate(Sender: TObject);
begin
  if FFileName = '' then
    FFileName := 'readme.txt'; //Default
  FDoOnce := True;
end;

end.
