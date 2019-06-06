program TestSynapse;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces,
  Forms,
  MainForm,
  ClientSSH2;

{$R *.res}

begin
  RequireDerivedFormResource := true;
  Application.Initialize;
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.



