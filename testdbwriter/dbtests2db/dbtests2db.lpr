program dbtests2db;
{ dbtests2db: run database tests and store results in (another) database

  Copyright (C) 2012-2014 Reinier Olislagers

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{ Runs same tests as dbtestframework.pas, but stores them in a database. Useful
  for Continuous Integration (CI) environments and for regression testing.
  For ease of use, the FPC 2.6.1 r22717 tests are included.
  Please edit testdbwriter.ini to set up the connection to the database where
  the results will be stored. You will probably also need an existing database,
  look in testdbwriter.pas for instructions.

  Assumes subversion/svn is used (to get revision number) but revision number can be
  specified on the command line, too (via --revision)

  Minimum required FPC version: 2.6.2.

}


{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  fpcunit,
  // Needed to write the test output to db:
  testdbwriter,
  testregistry,
  toolsunit,
  // List of supported database connectors
  sqldbtoolsunit,
  dbftoolsunit,
  bufdatasettoolsunit,
  memdstoolsunit,
  SdfDSToolsUnit,
  tcsdfdata,
  // Units wich contain the tests
  TestBasics,
  TestFieldTypes,
  TestDatasources,
  TestDBBasics,
  TestBufDatasetStreams,
  TestSpecificTBufDataset,
  CustApp,
  regexpr,
  process;

type

  { TDBTestsToDB }

  TDBTestsToDB = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure StoreTestOutput;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

const
  INIFile='testdbwriter.ini';

function GetSVNRevision: string;
const
  BranchRevLength = Length('Last Changed Rev:');
  RevLength = Length('Revision:');
  RevExpression = '\:\s+(\d+)\s'; //regex to match revision in svn info
var
  LRevision: string = ''; // Revision of repository as a whole
  Output: string = '';
  RevExtr: TRegExpr;
  FReturnCode: integer;
begin
  result:='';
  // Could have used svnversion but that would have meant calling yet another command...
  // Get the part after "Revision:"...
  // unless we're in a branch/tag where we need "Last Changed Rev: "
  if RunCommand('svn',['info'], Output) then
  begin
    // Use regex to try and extract from localized SVNs:
    // match exactly 2 occurences of the revision regex.
    RevExtr := TRegExpr.Create;
    try
      RevExtr.Expression := RevExpression;
      if RevExtr.Exec(Output) then
      begin
        // Whole repository revision - preferable
        result := RevExtr.Match[1];
        if (result='') and (RevExtr.ExecNext) then
        begin
          result := RevExtr.Match[1];
        end;
      end;
    finally
      RevExtr.Free;
    end;
    if result='' then
    begin
      // Regex failed; trying for English revision message (though this may be
      // superfluous with the regex)
      result := trim(copy(Output, (pos('Last Changed Rev: ', Output) + BranchRevLength), 6));
      //local revision (in this particular directory)
      //revision := trim(copy(Output, (pos('Revision: ', Output) + RevLength), 6));
    end;
  end;
end;

{ TDBTestsToDB }

Procedure TDBTestsToDB.StoreTestOutput;

var
  SVNBranch: string;
  DBResultsWriter: TDBResultsWriter;
  testResult: TTestResult;
  TestSuiteRootNames: TStringList;
  RevisionID: string;
begin
  testResult := TTestResult.Create;
  DBResultsWriter:=TDBResultsWriter.Create;
  try
    testResult.AddListener(DBResultsWriter);
    // Use the TestSuiteRoot property to prepend results with test suite names
    // that the user specified
    SVNBranch:=GetOptionValue('s','svnbranch');
    if SVNBranch<>'' then
    begin
      // Strip starting, trailing / in order to normalize
      if SVNBranch[1]='/' then
        SVNBranch:=copy(SVNBranch,2,length(SVNBranch));
      if RightStr(SVNBranch,1)='/' then
        SVNBranch:=copy(SVNBranch,1,length(SVNBranch)-1);
    end
    else
    begin
      // No option given - assume trunk
      SVNBranch:='2.7.1';
    end;

    // Add in the connector type (e.g. sql) and database type (e.g. interbase)
    SVNBranch:=SVNBranch+'/'+dbconnectorname+'/'+dbtype;
    TestSuiteRootNames:=TStringList.Create;
    try
      TestSuiteRootNames.Delimiter:='/';
      TestSuiteRootNames.DelimitedText:=SVNBranch;
      DBResultsWriter.TestSuiteRoot.AddStrings(TestSuiteRootNames);
    finally
      TestSuiteRootNames.Free;
    end;
    DBResultsWriter.ApplicationName:='dbtestframework';

    RevisionID:=GetOptionValue('r','revisionid');
    if RevisionID='' then
      RevisionID:=GetOptionValue('revision');
    if RevisionID='' then // fall back to svn revision if retrievable
      RevisionID:=GetSVNRevision;
    if RevisionID<>'' then
      DBResultsWriter.RevisionID:=RevisionID;

    DBResultsWriter.Comment:=GetOptionValue('c','comment');
    GetTestRegistry.Run(testResult);
  finally
    testResult.Free;
    DBResultsWriter.Free;
  end;
end;

procedure TDBTestsToDB.DoRun;
var
  ErrorMsg: String;
begin
  ErrorMsg:=CheckOptions('c:hr:s:','comment: help revision: revisionid: svnbranch:');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse some parameters; others are parsed later in StoreTestOutput
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if not FileExists(ExtractFilePath(ParamStr(0))+INIFile) then
  begin
    writeln('Warning: could not find settings file '+INIFile);
    writeln('Defaulting to Firebird embedded test storage.');
    writeln('To avoid this, make sure '+INIFile+' is in the current directory.');
    writeln('Note: if database.ini cannot be read either, the tests will error.');
  end;

  writeln('Test run started at');
  writeln(DateTimeToStr(Now()));
  StoreTestOutput;
  writeln('Test run complete at');
  writeln(DateTimeToStr(Now()));
  // stop program loop
  Terminate;
end;

constructor TDBTestsToDB.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TDBTestsToDB.Destroy;
begin
  inherited Destroy;
end;

procedure TDBTestsToDB.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
  writeln(ExeName + '<dbtype> [options]');
  writeln('<dbtype>: Specify what database connector shall be ');
  writeln('   used in testing (see database.ini, [Database], type=)');
  writeln('   So this sets the tested db, not the test output db.');
  writeln('   *NOTE*: must be first argument on command line.');
  writeln('');
  writeln('Possible options: ');
  writeln('-c <comment>, --comment=<comment>');
  writeln('   add comment to test run info.');
  writeln('-r <id> --revision=<id>, --revisionid=<id>');
  writeln('   add revision id/application version ID to test run info.');
  writeln('   If this option is not specified, svn info will be used to try');
  writeln('   and get the subversion revision (if in use).');
  writeln('-s <names>, --svnbranch=<names>');
  writeln('   use names as part the testsuite hierarchy for the test run');
  writeln('   (corresponds to the TDBResultsWriter.TestSuiteRoot property)');
  writeln('   example: 2.6.1/sql/interbase');
  writeln('   Useful to differentiate between e.g. trunk and a stable branch.');
  writeln('   Write one or more testsuite names, separated by /');
  writeln('   Please do not use leading or trailing / ');
  writeln('   If this option is not specified, 2.7.1/sql/<dbconnector> is assumed.');
  writeln('');
  writeln('Specify details for the database where you want to store results in ');
  writeln(INIFile);
  writeln('Specify the database you want to test in database.ini; override ');
  writeln('type with <dbtype> if wanted.');
end;

var
  Application: TDBTestsToDB;
begin
  Application:=TDBTestsToDB.Create(nil);
  Application.Run;
  Application.Free;
end.

