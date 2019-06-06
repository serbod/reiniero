program CheckRideResourceZipper;

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}
//Handy for error messages in resourcezipper
{$DEFINE CONSOLE}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  Interfaces, // this includes the LCL widgetset
  SysUtils,
  Forms {for application support},
  checkrideutil {for future work with unzipping from resources},
  resourcezipper;

var
  OurResourceZipper: TResourceZipper;
  FMustWriteCheckRideResource: boolean;
  FMustWriteCheckRideHelpResource: boolean;
  FMustWriteAllResources: boolean;
  FExeDirectory: string;

  procedure ShowCommandLineHelp;
  begin
    writeln(ExtractFileName(Application.ExeName) + ' [options]');
    writeln('-h or --help                         : help');
    writeln('-o <dir> or --outputdirectory=<dir>  : output directory');
    writeln('--writeresourcecheck                 : add zip as "poor man''s resource"');
    writeln('                                       to the ' +
      CheckRideExe + ' executable.');
    writeln('--writeresourcehelp                  : add zip as "poor man''s resource"');
    writeln('                                       to the ' +
      CheckRideHelperExe + ' executable.');
    writeln('                                       to the both executable.');
  end;

  procedure OptionsAndInit;
  var
    ErrorMessage: string;
  begin
    Application.CaseSensitiveOptions := False; //accept upper and lowercase
    ErrorMessage := Application.CheckOptions('ho:',
      'help outputdirectory: writeallresources writeresourcecheck writeresourcehelp');
    if Length(ErrorMessage) > 0 then
    begin
      writeln(ErrorMessage);
      ShowCommandLineHelp;
      halt(1); //invalid options
    end;


    if (Application.HasOption('h', 'help')) or (Application.HasOption('?')) then
    begin
      ShowCommandLineHelp;
      halt(0);
    end;

    FExeDirectory := '';
    if Application.HasOption('o', 'outputdirectory') then
    begin
      //Get absolute path
      //Trailed separator apparently not attached by ExpandFileName
      // We write zips to output, this helps in troubleshooting.
      FExeDirectory := ExpandFileName(
        Trim(Application.GetOptionValue('o', 'outputdirectory')));
      OurResourceZipper.ZipDirectory := FExeDirectory;
    end;

    if Application.HasOption('writeresourcecheck') then
    begin
      FMustWriteCheckRideResource := True;
    end
    else
    begin
      FMustWriteCheckRideResource := False;
    end;

    if Application.HasOption('writeallresources') then
    begin
      FMustWriteAllResources := True;
    end
    else
    begin
      FMustWriteAllResources := False;
    end;

    if Application.HasOption('writeresourcehelp') then
    begin
      FMustWriteCheckRideHelpResource := True;
    end
    else
    begin
      FMustWriteCheckRideHelpResource := False;
    end;
  end;

begin
  OurResourceZipper := TResourceZipper.Create;
  try
    writeln(ExtractFileName(Application.ExeName) +
      ': zip CheckRide files for use into resource; optionally write resource.');
    writeln('***NOTE: please update and recompile CheckRideResourceZipper.lpr');
    writeln('         whenever you need files added to the resource.');
    //Setup
    OptionsAndInit; //Check what users asked us to do.
    //Create zip(s), append to executable(s):
    if (FMustWriteCheckRideResource or FMustWriteAllResources) then
    begin
      OurResourceZipper.Executable := FExeDirectory + CheckRideExe;
      OurResourceZipper.WriteCheckRideResource;
    end;
    if (FMustWriteCheckRideHelpResource or FMustWriteAllResources) then
    begin
      OurResourceZipper.Executable := FExeDirectory + CheckRideHelperExe;
      OurResourceZipper.WriteCheckRideHelperResource;
    end;
  finally
    OurResourceZipper.Free;
  end;
end.
