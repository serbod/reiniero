program tigerclient;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, clientmain, httpclient, LJGridUtils,
  imageformunit, tigersettings
  {$IFDEF WINDOWS}
  ,DelphiTwain
  ,DelphiTwain_VCL
  ,wia
  ,WIA_1_0_TLB
  {$ENDIF}
  ;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TImageForm, imageform);
  Application.Run;
end.

