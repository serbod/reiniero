program banco;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, bancomain, bancodb, transactioninfo, fpsallformats,
  fpspreadsheet;

{$R *.res}

begin
  {$IFDEF DEBUG}
  // Leak view integration: open heap.trc with leak view
  // By default information is written to standard output,
  // this function allows you to redirect the information to a file
  SetHeapTraceOutput('heap.trc');
  {$ENDIF}

  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

