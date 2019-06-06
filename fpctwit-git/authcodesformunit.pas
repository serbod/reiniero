unit authcodesformunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, lclintf;

type

  { TAuthcodesForm }

  TAuthcodesForm = class(TForm)
    ProxyUser: TEdit;
    ProxyPass: TEdit;
    ProxyHost: TEdit;
    ProxyPort: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    OKButton: TButton;
    CancelButton: TButton;
    ConsumerKey: TEdit;
    ConsumerSecret: TEdit;
    AccessToken: TEdit;
    AccessTokenSecret: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    TwitterHyperlinkLabel: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TwitterHyperlinkLabelClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    FCancelled: boolean;
  end;

var
  AuthcodesForm: TAuthcodesForm;

implementation

{$R *.lfm}

{ TAuthcodesForm }

procedure TAuthcodesForm.TwitterHyperlinkLabelClick(Sender: TObject);
begin
  openurl('https://dev.twitter.com/apps/new');
end;


procedure TAuthcodesForm.CancelButtonClick(Sender: TObject);
begin
  FCancelled := true;
  Close;
end;

procedure TAuthcodesForm.FormCreate(Sender: TObject);
begin
  FCancelled := false;
end;


procedure TAuthcodesForm.OKButtonClick(Sender: TObject);
begin
  FCancelled := false;
  Close;
end;



end.

