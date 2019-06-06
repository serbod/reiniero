program CheckRideHelper;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  CheckRideHelperUnit,
  checkrideutil,
  clientcustomizer, poormansresource, resourcezipper, aboutform;

{$R CheckRideHelper.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TCheckRideHelperMain, CheckRideHelperMain);
  Application.Createform(Tfrmclientcustomizer, Frmclientcustomizer);
  Application.Run;
end.
