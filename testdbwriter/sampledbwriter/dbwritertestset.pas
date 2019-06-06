unit dbwritertestset;

{
fpcunit tests that can be used to exercise the testdbwriter code.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type

  { TTestCase1 }

  TTestCase1= class(TTestCase)
  published
    procedure ThisErrors;
    procedure ThisFails;
    procedure ThisWorks;
    procedure ThisTestIsIgnored;
    procedure WorksToo;
    procedure WorksSometimes;
  end;

implementation

procedure TTestCase1.ThisErrors;
begin
  raise Exception.Create('Message: some awful exception generated in the test code.');
  // for completeness' sake: add a test
  assertTrue('true is true. That''s a fact.',true=true);
end;

procedure TTestCase1.ThisFails;
begin
  assertTrue('This piece of code thinks true is false.',true=false);
end;

procedure TTestCase1.ThisWorks;
begin
  assertTrue('This piece of code should work.',true=true);
end;

procedure TTestCase1.ThisTestIsIgnored;
begin
  ignore('This test is not relevant for this platform.');
end;

procedure TTestCase1.WorksToo;
begin
  assertTrue('This piece of code should work, too.',true=true);
end;

procedure TTestCase1.WorksSometimes;
// Handy for showing regression tests because failure and success alternate
var
  Coin: integer;
begin
  Coin:=Random(2); //Flip a coin
  CheckEquals(1,Coin,'Random test: chance worked against us.');
end;



initialization
  // This tests hierarchy
  RegisterTest(TTestCase1); // no parent
  // We're testing the top layer...
  RegisterTest('Simulation',TTestCase1); //TopLevel testsuite is parent
  // ... and also adding some test case down under in a hierarchy
  RegisterTest('Simulation.Brain.Simplified',TTestCase1);
  // and yet another hierarchy:
  RegisterTest('World.Domination.FailedAttempts',TTestCase1);
end.

