unit clientcustomizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Fileutil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  EditBtn, StdCtrls;

type

  { TfrmClientCustomizer }

  TfrmClientCustomizer = class(TForm)
    Btncancel: TButton;
    Btnsave: TButton;
    Checkridefile: Tfilenameedit;
    Lblcheckrideexe: Tlabel;
    SaveCheckride: Tsavedialog;
    Servername: Tlabelededit;
    HelperName: TLabeledEdit;
    Serverport: Tlabelededit;
    procedure Btncancelclick(Sender: TObject);
    procedure Btnsaveclick(Sender: TObject);
    procedure CheckRideFileAcceptFileName(Sender: TObject; var Value: string);
    procedure Checkridefileeditingdone(Sender: TObject);
  private
    procedure FindAndReplace(TextFile, LookFor, ReplaceWith: string);
    procedure UpdateEditControls(SwitchOn: boolean);
  public
    { public declarations }
  end;

var
  frmClientCustomizer: TfrmClientCustomizer;

implementation

uses resourcezipper, checkrideutil;


{$R *.lfm}

{ TfrmClientCustomizer }

procedure Tfrmclientcustomizer.Checkridefileeditingdone(Sender: TObject);
begin
  UpdateEditControls(FileExists(CheckRideFile.Text));
end;

procedure TfrmClientCustomizer.FindAndReplace(TextFile, LookFor, ReplaceWith: string);
var
  slFile: TStringList;
begin
  slFile := TStringList.Create;
  try
    slFile.LoadFromFile(TextFile);
    slFile.Text := StringReplace(slFile.Text, LookFor, ReplaceWith, [rfReplaceAll]);
    slFile.SaveToFile(TextFile);
  finally
    slFile.Free;
  end;
end;

procedure Tfrmclientcustomizer.Btncancelclick(Sender: TObject);
begin
  UpdateEditControls(False);
  CheckRideFile.Text := '';
end;

procedure Tfrmclientcustomizer.Btnsaveclick(Sender: TObject);
var
  TargetFileName: string;
  Resource: TResourceZipper;
  ResourceDir: string;
begin
  //Suggestion:
  SaveCheckride.FileName := CheckRideFile.FileName + '_' + Trim(HelperName.Text) + '.exe';
  if SaveCheckride.Execute then
  begin
    Resource := TResourceZipper.Create;
    try
      TargetFileName := SaveCheckride.FileName;
      CopyFile(Checkridefile.FileName, TargetFileName);

      // Extract CheckRide.exe resource into temp dir...
      ResourceDir := ResourceExtract(TargetFileName);
      if ResourceDir = EmptyStr then
        raise Exception.Create('Resource extraction failed.');

      // ...Change contents...
      FindAndReplace(ResourceDir + CheckRideConfigFileName, HelperHostExample,
        ServerName.Text);
      FindAndReplace(ResourceDir + CheckRideConfigFileName, HelperPortExample,
        ServerPort.Text);
      FindAndReplace(ResourceDir + CheckRideConfigFileName, HelperNameExample,
        HelperName.Text);

      //...and overwreite resource in target file
      Resource.Executable := TargetFileName;
      Resource.ZipSourceDir := ResourceDir; //Point to changed resource files
      Resource.WriteCheckRideResource; //Replace existing resource
      //todo: clear out temp files
      ShowMessage('Custom checkride executable has been saved as: ' + TargetFileName);
    finally
      Resource.Free;
      try
        UpdateEditControls(false);
      finally
        //ignore errors
      end;
    end;
  end;
end;

procedure TfrmClientCustomizer.CheckRideFileAcceptFileName(Sender: TObject;
  var Value: string);
begin
  UpdateEditControls(True);
end;

procedure Tfrmclientcustomizer.UpdateEditControls(SwitchOn: boolean);
begin
  ServerPort.Enabled := SwitchOn;
  ServerName.Enabled := SwitchOn;
  HelperName.Enabled := SwitchOn;
  btnSave.Enabled := SwitchOn;
end;

end.
