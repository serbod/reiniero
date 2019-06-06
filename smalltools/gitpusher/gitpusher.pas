unit gitpusher;
{ Pushes chunks to remote git repo

  Copyright (c) 2013 Reinier Olislagers

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

interface

uses
  Classes, SysUtils, gitclient, processutils, fileutil;

const
  DefaultMaxChunkSize=40*1024*1024; //max amount of bytes to send in one commit
  MaxWaitSecs=43; //Wait up to this amount of seconds between chunks
  MinWaitSecs=18; //Wait at least this amount of secons between chunks

type

  { TGitPusher }

  TGitPusher = class(TGitClient)
  private
    FMaxChunkSize: integer;//in bytes
  public
    // Pushes chunks to remote repo until all committed or until errors occur.
    // Returns success or failure
    function PushChunks: boolean;
    // Size of the largest chunk to send (in bytes)
    // Actually the size can be larger depending on the last file added, but
    // no new files are added once this limit is reached
    property MaxChunkSize: integer read FMaxChunkSize write FMaxChunksize;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TGitPusher }

function TGitPusher.PushChunks: boolean;
var
  Adder: integer;
  i:integer;
  Files: TStringList;
  SingleFile: string='';
  Output: string = '';
  SingleSize: integer;
  TotalSize: integer;
  WaitSecs:integer;
begin
  result:=false;
  Randomize;
  Files:=TStringList.Create;
  try
    Files.Sorted:=true;
    Files.Duplicates:=dupIgnore;
    // Get list of modified and untracked (others) files
    FReturnCode:=ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' ls-files --exclude-standard --modified --others', FLocalRepository, Output, Verbose);
    Files.Text:=Output;
    if Files.Count<100 then
    begin
      //todo: debug
      writeln('Added/modified files:');
      writeln(Files.Text);
    end;
    TotalSize:=0;
    for i:=Files.Count-1 downto 0 do
    begin
      // check until chunksize>=Maxchunksize
      // git outputs directory separators as /, also on windows.
      SingleFile:=IncludeTrailingPathDelimiter(FLocalRepository)+
        StringReplace(Files[i],'/',DirectorySeparator,
        [rfReplaceAll,rfIgnoreCase]);
      SingleSize:=FileUtil.FileSize(SingleFile);;
      if SingleSize>0 then
        TotalSize:=TotalSize+SingleSize;
      if (i=0) or (TotalSize>=FMaxChunkSize) then
      begin
        writeln('');
        writeln('Adding ',TotalSize,' bytes of files');
        writeln('Diagnostic: i=',i,', files.count now ',files.count);
        for Adder:=Files.Count-1 downto i do
        begin
          // Add file to local repo if not already done
          // Do mark deleted files for removal
          FReturnCode:=ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' add --all '+DoubleQuoteIfNeeded(Files[Adder]), FLocalRepository, Output, Verbose);
          if FReturnCode<>0 then exit;
        end;
        //todo: update??!
        // Push chunks to remote, indicate progress in commit message
        FReturnCode:=ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' commit -m "Adding everything '+inttostr(i+1)+':'+inttostr(Files.Count)+'" ', FLocalRepository, Output, Verbose);
        if FReturnCode<>0 then exit;
        FReturnCode:=ExecuteCommandInDir(DoubleQuoteIfNeeded(FRepoExecutable) + ' push origin master', FLocalRepository, Output, Verbose);
        if FReturnCode<>0 then exit;
        TotalSize:=0; //reset
        //todo: check off by one
        for Adder:=Files.Count-1 downto i do
        begin
          Files.Delete(Adder);
        end;
        // Wait loop if not final push
        if (i>0) then
        begin
          WaitSecs:=MinWaitSecs+random(MaxWaitSecs-MinWaitSecs);
          writeln('Waiting ',WaitSecs,' seconds');
          sleep(WaitSecs*1000);
        end;
      end;
    end;
  finally
    Files.Free;
  end;
  result:=(FReturnCode=0);
end;

constructor TGitPusher.Create;
begin
  inherited Create;
  FMaxChunksize:=DefaultMaxChunkSize;
end;

destructor TGitPusher.Destroy;
begin
  inherited Destroy;
end;

end.

