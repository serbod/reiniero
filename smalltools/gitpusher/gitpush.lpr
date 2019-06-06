program gitpush;
{ Commits and pushes directory with subdirectories to git, including remote
  repository, in chunks of a certain maximum size. Useful for huge repositories
  that you can't seem to commit at once.
  Note: suggest first trying to increase git config http.postBuffer before using
  this as it may fix the problem.

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

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, repoclient, gitclient, gitpusher;

type

  { TGitPusher }

  TGitPush = class(TCustomApplication)
  private
    FGitPusher:TGitPusher;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TGitPush }

procedure TGitPush.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help local: maxchunksize: remote:');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('maxchunksize') then
  begin
    // TGitPusher expects bytes, not megabytes
    FGitPusher.MaxChunkSize:=StrToIntDef(GetOptionValue('maxchunksize'),DefaultMaxChunkSize)*1024*1024;
  end;

  if HasOption('local') then
  begin
    FGitPusher.LocalRepository:=GetOptionValue('local');
  end
  else
  begin
    writeln('No --local argument specified. This is required.');
    writeln('');
    WriteHelp;
    Terminate;
    exit;
  end;

  if HasOption('remote') then
  begin
    FGitPusher.Repository:=GetOptionValue('remote');
  end
  else
  begin
    writeln('No --remote argument specified. This is required.');
    writeln('');
    WriteHelp;
    Terminate;
    exit;
  end;

  if FGitPusher.PushChunks then
    writeln('Pushing complete.')
  else
    writeln('Errors pushing.');

  // stop program loop
  Terminate;
end;

constructor TGitPush.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FGitPusher:=TGitPusher.Create;
  FGitPusher.Verbose:=true;
end;

destructor TGitPush.Destroy;
begin
  FGitPusher.Free;
  inherited Destroy;
end;

procedure TGitPush.WriteHelp;
begin
  writeln('Usage: ',ExeName,' -h');
  writeln('--local=<dir>      Local directory you want to push');
  writeln('--maxchunksize=<s> Approximate maximum size of a chunk, in megabyte');
  writeln('                   Effective sizes can be larger depending on size of last file added by the program.');
  writeln('--remote=<URL>     Remote repo you want to push to');
  writeln('');
  writeln('Some git hints:');
  writeln('git config http.postBuffer 209715200');
  writeln('                   Increase buffer for pushes');
  writeln('git gc --aggressive garbage collect metadata');
end;

var
  Application: TGitPush;
begin
  Application:=TGitPush.Create(nil);
  Application.Title:='GitPusher';
  Application.Run;
  Application.Free;
end.

