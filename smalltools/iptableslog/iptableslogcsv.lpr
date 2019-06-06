{*
This source code is provided under the MIT license:
Copyright (C) 2011 by Reinier Olislagers

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*}
program iptableslogcsv;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  iptableslog,
  SysUtils;

  procedure WriteHelp;
  begin
    writeln('iptableslogcsv v1.3');
    writeln('An IPTables firewall logfile CSV exporter');
    writeln('Open source freeware, see:');
    writeln('https://bitbucket.org/reiniero/smalltools');
    writeln('');
    writeln('iptableslogcsv [inputfile] [outputfile]');
    writeln(' Inputfile : use this filename as input');
    writeln(' none given: use messages (Windows), /var/log/messages (Unix/Linux)');
    writeln(' Outputfile: use this filename as output');
    writeln(' none given: use iptableslog.csv');
    writeln('iptableslogcsv -?|-h|--help|/?|/h|/help');
    writeln(' Show this help');
    writeln('iptableslogcsv -s|--showfields');
    writeln(' Show CSV output fields');

    writeln('');
  end;

var
  LogFile: string;
  IPTables: TIPTablesLog;
  Argument: string;
  ShowFields: boolean = False;
begin
  {$IFDEF WINDOWS}
  LogFile := 'messages';
  {$ENDIF WINDOWS}
  {$IFDEF UNIX}
  LogFile := '/var/log/messages';
  {$ENDIF UNIX}
  Argument := AnsiUpperCase(ParamStr(1));
  if Length(ParamStr(1)) > 0 then
  begin
    case Argument of
      '-H', '-HELP', '--HELP', '-?', '/H', '/HELP', '/?':
      begin
        WriteHelp;
        Halt(0);
      end;
      '-S', '-SHOWFIELDS', '--SHOWFIELDS':
      begin
        ShowFields := True;
      end;
      else
        LogFile := ParamStr(1);
    end;
  end;

  IPTables := TIPTablesLog.Create;
  try
    if Length(ParamStr(2)) > 0 then
      IPTables.OutputFile := ParamStr(2)
    else
      IPTables.OutputFile := 'iptableslog.csv';

    if ShowFields then
    begin
      writeln(IPTables.Showfields);
    end
    else
    begin
      //Parse
      IPTables.InputFile := LogFile;
      writeln('Opening '+IPTables.InputFile+' for processing.');
      IPTables.Parse;
      writeln('Wrote '+IntToStr(IPTables.LinesProcessed) + ' lines to '+IPTables.OutputFile);
      writeln('Finished.');
    end;
  finally
    IPTables.Free;
  end;
end.
