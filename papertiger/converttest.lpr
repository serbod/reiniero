program converttest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Sysutils,Classes,tigerutil;

const
  InputFile='scan_24bpp.bmp';
  OutputFile='scan_result.tiff';
begin
  if FileExists(outputfile) then
    DeleteFile(outputfile);
  tigerutil.ConvertTIFFCCITT4(InputFile,OutputFile);
  if tigerutil.IsTIFFCCITT4(OutputFile) then
    writeln('Success: Group 4 compression')
  else
    writeln('Failure');
end.

