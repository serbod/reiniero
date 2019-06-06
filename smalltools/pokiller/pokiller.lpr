program pokiller;

{ Svn reverts modified .po files in svn repo
Useful to remove generated .po files that Lazarus developers forgot to commit
}

{ MIT license - suitable for use in LGPL/modified LGPL programs

  Copyright (c) 2014 Reinier Olislagers

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils,Classes,
  process;

const
  SvnCommand = 'svn';

function SvnInfo: boolean;
var
  DumpThis: string;
begin
  result:=false;
  result:=RunCommand(svncommand,['info'],DumpThis);
end;

var
  POFiles: TStringList;
  CmdOutput: string;
  i:integer;
begin
  if not(SvnInfo) then
  begin
    writeln('Not an svn repository. Aborting.');
    exit;
  end;

  // Gives output like
  //M       components\chmhelp\lhelp\lhelp.lpr
  if not(RunCommand(svncommand,['-q','status'],CmdOutput)) then
  begin
    writeln('svn -q status gave error. Aborting. Output was:');
    writeln(CmdOutput);
  end;

  POFiles:=TStringList.Create;
  try
    POFiles.Text:=CmdOutput;
    for i:=POFiles.Count-1 downto 0 do
    begin
      if RightStr(POFiles[i],length('.po'))<>'.po' then
        POFiles.Delete(i)
      else // get rid of M    stuff at the beginning
        if POFiles[i][1]='M' then
          POFiles[i]:=Trim(Copy(POFiles[i],2,MaxInt))
        else
          POFiles.Delete(i); //not modified but updated etc
    end;
    if POFiles.Count=0 then
    begin
      writeln('No .po files found.');
      exit;
    end
    else
    begin
      writeln('Found '+inttostr(POFiles.Count)+' po files');
      for i:=0 to POFiles.Count-1 do
      begin
        writeln(POFiles[i]);
        if not(RunCommand(SvnCommand,['revert',POFiles[i]],CmdOutput)) then
        begin
          writeln('Error running svn revert. Output was: ');
          writeln(CmdOutput);
          writeln('');
        end;
      end;
    end;
  finally
    POFiles.free;
  end;
end.

