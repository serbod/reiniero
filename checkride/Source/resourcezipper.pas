unit resourcezipper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, zipper;

type

  { TResourceZipper }

  TResourceZipper = class(TObject)
  private
    FZipDir: string; {Includes trailing dir separator}
    FTargetExecutable: string;
    FZipSourceDir: string;
    { Indicates whether the resource zip has been written yet }
    FCheckRideZipWritten: boolean;
    FCheckRideHelperZipWritten: boolean;
    { Add file to zip; strip out path part }
    procedure AddToZip(var Zip: TZipper; const FileName: string);
    procedure SetOutputDirectory(Value: string);
    procedure SetOutputFiles;
    procedure SetZipSourceDir(AValue: string);
  public
    { Directory where zip will be written }
    property ZipDirectory: string read FZipDir write Setoutputdirectory;
    { Executable where resource will be written/read }
    property Executable: string read FTargetExecutable write FTargetExecutable;
    { Write out helped resource to Executable, overwriting any resources present }
    procedure WriteCheckRideResource;
    { Write out helper resource to Executable, overwriting any resources present }
    procedure WriteCheckRideHelperResource;
    { Write out CheckRide zip for inclusion in resources etc. }
    procedure WriteCheckRideZip;
    { Write out CheckRide helper zip for inclusion in resources etc. }
    procedure WriteCheckRideHelperZip;
    { Directory where source files of resource zip are located. Can be empty for current dir.}
    property ZipSourceDir: string read FZipSourceDir write SetZipSourceDir;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses poormansresource, checkrideutil;

const
  HelpedResourceName = 'helped.zip';
  HelperResourceName = 'helper.zip';

var
  FHelpedResourceZip: string; //Zip file with resource for helped program (CheckRide)
  FHelperResourceZip: string;
//Zip file with resource for helper program (CheckRideHelper)

procedure Tresourcezipper.Setoutputdirectory(Value: string);
begin
  FZipDir := Value;
  if (RightStr(FZipDir, 1) <> DirectorySeparator) and (Value <> EmptyStr) then
  begin
    FZipDir := FZipDir + DirectorySeparator;
  end;
  Setoutputfiles;
end;

procedure TResourceZipper.AddToZip(var Zip: TZipper;
  const FileName: string);
begin
  if FileExists(FileName)=false then
  begin
    {$IFDEF CONSOLE}
    writeln('Error adding '+FileName+' to zip file. Could not find file.');
    {$ENDIF CONSOLE}
    raise Exception.Create('Cannot find file');
  end;
  Zip.Entries.AddFileEntry(FileName, ExtractFileName(FileName));
end;

procedure Tresourcezipper.Setoutputfiles;
begin
  FHelpedResourceZip := FZipDir + HelpedResourceName;
  FHelperResourceZip := FZipDir + HelperResourceName;
end;

procedure TResourceZipper.SetZipSourceDir(AValue: string);
begin
  // Make sure trailing / or \
  FZipSourceDir := AValue;
  if (RightStr(AValue, 1) <> DirectorySeparator) and (AValue <> EmptyStr) then
  begin
    FZipSourceDir := AValue + DirectorySeparator;
  end;
end;

procedure TResourceZipper.WriteCheckRideHelperResource;
var
  Resource: TPayload;
begin
  if Executable = EmptyStr then
    raise Exception.Create('Executable property is required.');
  ;
  if FCheckRideHelperZipWritten = False then
    WriteCheckRideHelperZip;
  Resource := TPayload.Create(Executable);
  try
    try
      Resource.FileIntoPayload(FZipDir + HelperResourceName);
      writeln('Wrote resource ' + FZipDir + HelperResourceName + ' to ' + Executable);
    except
      on E: Exception do
      begin
        {$IFDEF CONSOLE}
        writeln('An error occurred writing CheckRideHelper resource. Technical details: ',
          E.ClassName, '/', E.message);
        writeln('Resource ' + FZipDir + HelpedResourceName);
        writeln('Output exe: ' + Executable);
        {$ENDIF CONSOLE}
        raise Exception.Create(
          'Error writing CheckRideHelper resource. Technical details: ' +
          E.ClassName + '/' + E.message);
        halt(1); //stop with error
      end;
    end;
  finally
    Resource.Free;
  end;
end;

procedure TResourceZipper.WriteCheckRideResource;
var
  Resource: TPayload;
begin
  if Executable = EmptyStr then
    raise Exception.Create('Executable property is required.');
  ;
  if FCheckRideZipWritten = False then
    WriteCheckRideZip;
  Resource := TPayload.Create(Executable);
  try
    try
      Resource.FileIntoPayload(FZipDir + HelpedResourceName);
      {$IFDEF CONSOLE}
      writeln('Wrote resource ' + FZipDir + HelpedResourceName + ' to ' + Executable);
      {$ENDIF CONSOLE}
    except
      on E: Exception do
      begin
        {$IFDEF CONSOLE}
        writeln('An error occurred writing CheckRide resource. Technical details: ',
          E.ClassName, '/', E.message);
        writeln('Resource ' + FZipDir + HelpedResourceName);
        writeln('Output exe: ' + Executable);
        {$ENDIF CONSOLE}
        raise Exception.Create('Error writing CheckRide resource. Technical details: ' +
          E.ClassName + '/' + E.message);
        halt(1); //stop with error
      end;
    end;
  finally
    Resource.Free;
  end;
end;

{ TResourceZipper }

procedure Tresourcezipper.WriteCheckRideZip;
var
  ExternalDirPrefix: string;
  RootDirTrail: string; {Includes trailing directoryseparator}
  Zip: TZipper;
begin
  RootDirTrail := ZipSourceDir; //May be empty for current path

  if DirectoryExists(RootDirTrail + 'external') then
  begin
    ExternalDirPrefix := RootDirTrail + 'external' + DirectorySeparator;
  end
  else
  begin
    ExternalDirPrefix := RootDirTrail;
  end;

  // Preparation: create output directory, delete existing file.
  if trim(FZipDir) <> '' then
    try
      ForceDirectories(FZipDir);
    except
      on E: Exception do
      begin
        {
        writeln('Warning: could not create output directory ' + FZipDir);
        writeln('Technical details: ',
          E.ClassName, '/', E.message);
        }
        raise; //just pass it on
      end;//E: Exception
    end;
  try
    DeleteFile(FHelpedResourceZip);
  except
    on E: Exception do
    begin
      writeln('Warning: Could not delete file ' + FHelpedResourceZip);
      Writeln('Technical details: ',
        E.ClassName, '/', E.message);
    end;//E: Exception
  end;

  // Actual work
  Zip := TZipper.Create;
  try
    try
      Zip.FileName := FHelpedResourceZip;
      // My build environment has an external directory with dlls/exes. Directory layout in
      // CheckRideHelper install dir is flat though
      AddToZip(Zip,ExternalDirPrefix+'libeay32.dll');
      AddToZip(Zip,ExternalDirPrefix+'sas.dll');
      AddToZip(Zip,ExternalDirPrefix+'schook.dll');
      AddToZip(Zip,ExternalDirPrefix+'ssleay32.dll');
      AddToZip(Zip,ExternalDirPrefix+'stunnel.exe');
      AddToZip(Zip,ExternalDirPrefix+'stunnel.pem');
      AddToZip(Zip,ExternalDirPrefix+'ultravnc.ini');
      //See https://forum.ultravnc.net/viewtopic.php?f=9&t=15864
      //we already have schook.dll so presumably that is used, no need for vnchooks.
      AddToZip(Zip,ExternalDirPrefix+'winvnc.exe');
      AddToZip(Zip,ExternalDirPrefix+'zlib1.dll');
      AddToZip(Zip,RootDirTrail+'stunnelhelped.conf.template');
      AddToZip(Zip,RootDirTrail+'CheckRide.conf');
      if FileExists(RootDirTrail+'..'+DirectorySeparator+'Readme.txt') then
      begin
        AddToZip(Zip,RootDirTrail+'..'+DirectorySeparator+'Readme.txt');
      end
      else
      begin
        // Otherwise assume in current directory.
        AddToZip(Zip,RootDirTrail+'Readme.txt');
      end;
      if FileExists(RootDirTrail+'..'+DirectorySeparator+'License.txt') then
      begin
        AddToZip(Zip,RootDirTrail+'..'+DirectorySeparator+'License.txt');
      end
      else
      begin
        // Otherwise assume in current directory.
        AddToZip(Zip,RootDirTrail+'License.txt');
      end;
      Zip.ZipAllFiles;
      {$IFDEF CONSOLE}
      writeln('Done writing ' + HelpedResourceName);
      {$ENDIF CONSOLE}
    except
      on E: Exception do
      begin
        {$IFDEF CONSOLE}
        Writeln('Error creating ' + Zip.Filename + '. Details: ',
          E.ClassName, '/', E.message);
        {$ENDIF CONSOLE}
        //Writeln('Fatal error: aborting.');
        Halt(1); //Exit status: error.
      end;//E: Exception
    end;
  finally
    Zip.Free;
  end;
  FCheckRideZipWritten := True;
end;

procedure Tresourcezipper.WriteCheckRideHelperZip;
var
  RootDirTrail: string; {Includes trailing directoryseparator}
  Zip: TZipper;
begin
  RootDirTrail := ZipSourceDir; //May be empty for current path

  // Preparation: create output directory, delete existing file.
  if trim(FZipDir) <> '' then
    try
      ForceDirectories(FZipDir);
    except
      on E: Exception do
      begin
        {$IFDEF CONSOLE}
        writeln('Warning: could not create output directory ' + FZipDir);
        writeln('Technical details: ',
          E.ClassName, '/', E.message);
        {$ENDIF CONSOLE}
        raise; //just pass it on
      end;//E: Exception
    end;

  try
    DeleteFile(FHelperResourceZip);
  except
    on E: Exception do
    begin
      {$IFDEF CONSOLE}
      writeln('Warning: Could not delete file ' + FHelperResourceZip);
      Writeln('Technical details: ',
        E.ClassName, '/', E.message);
      {$ENDIF CONSOLE}
    end;//E: Exception
  end;

  // Actual work
  Zip := TZipper.Create;
  try
    try
      Zip.FileName := FHelperResourceZip;
      AddToZip(Zip,RootDirTrail+'stunnelhelper.conf.template');
      AddToZip(Zip,RootDirTrail+'CheckRide.conf');
      Zip.ZipAllFiles;
      writeln('Done writing ' + HelperResourceName);
    except
      on E: Exception do
      begin
        Writeln('Error creating ' + Zip.Filename + '. Details: ',
          E.ClassName, '/', E.message);
        {$IFDEF CONSOLE}
        Writeln('Fatal error: aborting.');
        {$ENDIF CONSOLE}
        Halt(1); //Exit status: error.
      end;//E: Exception
    end;
  finally
    Zip.Free;
  end;
  FCheckRideHelperZipWritten := True;
end;


constructor Tresourcezipper.Create;
begin
  FCheckRideHelperZipWritten := False;
  FCheckRideZipWritten := False;
  FZipDir := EmptyStr;
  FTargetExecutable := CheckRideExe; //no path
  FZipSourceDir := EmptyStr;
  SetOutputFiles;
end;

destructor Tresourcezipper.Destroy;
begin
  inherited Destroy;
end;

end.
