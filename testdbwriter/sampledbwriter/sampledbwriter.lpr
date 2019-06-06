{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2004-2012 by Dean Zobec, Michael Van Canneyt,
    Reinier Olislagers

    An example of using a TestDBWriter listener in a console test runner of FPCUnit tests.
    Also demonstrates using the TXMLResultsWriter (that generates the newer XML output)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

program sampledbwriter;

{$mode objfpc}
{$h+}
// Enable to get logging/writelns in order to troubleshoot
// database problems/program correctness. Note: uses writeln; only for console applications.
{.$DEFINE DEBUGCONSOLE}

uses
  custapp, Classes, SysUtils, fpcunit,
  xmltestreport {used to get results into XML format},
  testregistry,
  testdbwriter {used to get results into db},
  dbwritertestset {sample tests};


const
  ShortOpts = 'ac:hlx';
  Longopts: Array[1..6] of String = (
    'all','count:','list','suite:','help','xml');
  Version = 'Version 0.4';


type

  { TTestRunner }
  TTestOutputFormat = (tDB, tXMLAdvanced);

  TTestRunner = Class(TCustomApplication)
  private
    FFormat: TTestOutputFormat;
    FDBResultsWriter: TDBResultsWriter;
    FXMLResultsWriter: TXMLResultsWriter;
    procedure WriteHelp;
  protected
    procedure DoRun ; Override;
    procedure doTestRun(aTest: TTest); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


constructor TTestRunner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //FDBResultsWriter := TDBResultsWriter.Create; //done in procedures
  FXMLResultsWriter:=TXMLResultsWriter.Create(nil);
end;


destructor TTestRunner.Destroy;
begin
  //FDBResultsWriter.Free; //done in procedures
  FXMLResultsWriter.Free;
end;


procedure TTestRunner.doTestRun(aTest: TTest);
var
  testResult: TTestResult;
begin
  testResult := TTestResult.Create;
  try
    case FFormat of
      tDB:
      begin
        {$IF FPC_FULLVERSION<20701}
        {$Warning Compiler < 2.7.1 has TSQLScript bugs that probably prevent on the fly db creation. You may need to set up your db manually with the SQL scripts provided.}
        writeln('Compiled with FPC < 2.7.1. Bug in TSQLScript prevents proper creation of Firebird embedded.');
        writeln('You may need to set up your database manually with the Firebird SQL script.');
        {$ENDIF}
        testResult.AddListener(FDBResultsWriter);
        FDBResultsWriter.Comment:='Sample test program';

        // Fake a version that at least changes on each test run.
        // Because of possibility of repeating, we need sub-second variation
        FDBResultsWriter.RevisionID:=FormatDateTime('ssz',Now());

        // Depending on the application, you may want to add some fake test suite hierarchy
        // at the top of the test project.
        // Why? This makes it easier to avoid comparing apples to oranges when you have
        // various platforms, editions, configurations, database connectors etc of your program/test set.
        // Here, we demonstrate this with a Latin language edition of the code in its Enterprise edition:
        FDBResultsWriter.TestSuiteRoot.Add('Enterprise');
        FDBResultsWriter.TestSuiteRoot.Add('Latin');

        {
        // Normally, we would edit the testdbwriter.ini file and select our db
        // where the tests are stored that way.... or omit any ini file and let it
        // fallback to a Firebird embedded database.
        // However, if needed, that can be overridden here:
        FDBResultsWriter.DatabaseType:=TDBW_POSTGRESQLCONN_NAME;
        FDBResultsWriter.DatabaseHostname:='dbserver';
        FDBResultsWriter.DatabaseName:='dbtests';
        FDBResultsWriter.DatabaseUser:='postgres';
        FDBResultsWriter.DatabasePassword:='password';
        FDBResultsWriter.DatabaseCharset:='UTF8';
        }
      end;
      tXMLAdvanced:
      begin
        testResult.AddListener(FXMLResultsWriter);
        // if filename='null', no console output is generated...
        //FXMLResultsWriter.FileName:='';
      end;
    end;
    aTest.Run(testResult);
    case FFormat of
      tDB: testResult.RemoveListener(FDBResultsWriter);
      tXMLAdvanced:
      begin
        // This actually generates the XML output:
        FXMLResultsWriter.WriteResult(TestResult);
        // You can use fcl-xml's xmlwrite.WriteXMLFile to write the results
        // to a stream or file...
        testResult.RemoveListener(FXMLResultsWriter);
      end;
    end;
  finally
    testResult.Free;
  end;
end;

procedure TTestRunner.WriteHelp;
begin
  writeln(Title);
  writeln(Version);
  writeln(ExeName+': testdbwriter sample application');
  writeln('Runs a set of sample testa and stores the results in a database');
  writeln('using the testdbwriter unit.');
  writeln('');
  writeln('Usage: ');
  writeln('-c<n> or --count=<n> to run the tests n times');
  writeln('-l or --list to show a list of registered tests');
  writeln('-x or --xml to run all the tests and show the output in XML (new '
    +'DUnit style), no db output');
  writeln('');
  writeln('--suite=MyTestSuiteName to run only the tests in a single test '
    +'suite class');
end;

procedure TTestRunner.DoRun;
const
  RepeatInterval=10;
var
  I : Integer;
  RepeatCount: integer;
  S : String;
  TestCounter: integer;
begin
  S:=CheckOptions(ShortOpts,LongOpts);
  If (S<>'') then
  begin
    Writeln(StdErr,S);
    WriteHelp;
    halt(1);
  end;

  // Default to db output:
  FFormat:=tDB;

  if HasOption('h', 'help') then
  begin
    WriteHelp;
    halt(0);
  end;

  RepeatCount:=StrToIntDef(GetOptionValue('c','count'),1);
  {$IFDEF DEBUGCONSOLE}
  writeln('debug: repeatcount option detected: '+inttostr(RepeatCount));
  {$ENDIF}

  if HasOption('l', 'list') then
  begin
    writeln(GetSuiteAsXML(GetTestRegistry));
    halt(0);
  end;

  if HasOption('x', 'xml') then
    FFormat:=tXMLAdvanced;

  if HasOption('suite') then
  begin
    S := '';
    S:=GetOptionValue('suite');

    for TestCounter:=1 to RepeatCount do
    begin
      // For the db writer: recreate test objects so we get new runs each time
      FDBResultsWriter:=TDBResultsWriter.Create;
      try
        if S = '' then
          for I := 0 to GetTestRegistry.Tests.count - 1 do
            writeln(GetTestRegistry[i].TestName)
        else
        for I := 0 to GetTestRegistry.Tests.count - 1 do
          if GetTestRegistry[i].TestName = S then
          begin
            doTestRun(GetTestRegistry[i]);
          end;
        if TestCounter mod RepeatInterval=0 then
          writeln(StdErr,'Run repeated '+inttostr(TestCounter)+' times');
      finally
        FDBResultsWriter.Free;
      end;
    end;
  end
  else
  begin
    // No suite
    for TestCounter:=1 to RepeatCount do
    begin
      // For the db writer: recreate test objects so we get new runs each time
      FDBResultsWriter:=TDBResultsWriter.Create;
      try
        doTestRun(GetTestRegistry);
        if (TestCounter mod RepeatInterval)=0 then
          writeln(StdErr,'Run repeated '+inttostr(TestCounter)+' times');
      finally
        FDBResultsWriter.Free;
      end;
    end;
  end;
  Terminate;
end;


var
  App: TTestRunner;
begin
  App := TTestRunner.Create(nil);
  App.Initialize;
  App.Title := 'dbtestwriter test';
  App.Run;
  App.Free;
end.

