unit fbscript;

{ A script processor for Firebird databases. }
{ Written using Firebird 2.5 databases }
//todo: rework to tsqlscript descendent

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, fpsqlparser, fpsqltree, sqldb;

type

{ TFBScript }

TFBScript = class(TSQLScript)
private
  FDatabase: string;
  FScript: TStringList;
  FStatements: TStringList;
public
  // Parsed statements that can be executed one by one
  property Statements: TStringList read FStatements;
  procedure Execute; override;
  procedure ExecuteScript;
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
end;

implementation

{ TFBScript }

procedure TFBScript.Execute;
var
  i: integer;
  Parser: TSQLParser;
  ResultList: TSQLElementList;
  ScriptStream: TMemoryStream;
  SQL: TStringList;
  StopExecution: boolean;
begin
  inherited Execute;
  // load if not loaded
  // parse
  ScriptStream:=TMemoryStream.Create;
  SQL:=TStringList.Create;
  try
    FScript.SaveToStream(ScriptStream);
    ScriptStream.Position:=0;
    try
      Parser:=TSQLParser.Create(ScriptStream);
      ResultList:=Parser.ParseScript();
      for i:=0 to ResultList.Count-1 do
      begin
        SQL.Text:=ResultList[i].GetAsSQL([sfoDoubleQuoteIdentifier]);
        ExecuteStatement(SQL,StopExecution);
      end;
    finally
      Parser.Free;
    end;
  finally
    ScriptStream.Free;
    ResultList.Free;
    SQL.Free;
  end;
end;

procedure TFBScript.ExecuteScript;
begin
  Execute;
end;

constructor TFBScript.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScript:=TStringList.Create;
  FStatements:=TStringList.Create;
end;

destructor TFBScript.Destroy;
begin
  FScript.Free;
  FStatements.Free;
  inherited Destroy;
end;

end.

