program dbimportdemo;

{ Demonstration/test program for fileimport unit

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
  SysUtils, Classes,
  fileimport, CsvDocument;

const
  DefaultInputFile='test.tab';
var
  i: integer;
  DestField: integer;
  Imp: TFileImport;
  ImportFile: string;
  procedure PrintMapping;
  begin
    for i:=0 to Imp.MappingCount-1 do
    begin
      writeln(Imp.Mapping[i].SourceField,'=>',Imp.Mapping[i].DestinationField);
    end;
  end;
begin
  Imp:=TFileImport.Create;
  try
    if ParamCount<1 then
    begin
      writeln('No input file specified. Switching to default '+DefaultInputFile);
      ImportFile:=DefaultInputFile;
    end
    else
    begin
      writeln('Input file: '+ParamStr(1));
      ImportFile:=ParamStr(1);
    end;
    if not(fileexists(ImportFile)) then
      raise Exception.CreateFmt('Import file %s does not exist.',[ImportFile]);
    Imp.FileName:=ImportFile;

    writeln('Setting destination fields:');
    Imp.DestinationFields.Add('postcode');
    Imp.DestinationFields.Add('bogus');
    Imp.DestinationFields.Add('city');
    Imp.DestinationFields.Add('street');
    Imp.DestinationFields.Add('latitude');
    Imp.DestinationFields.Add('this/is\tricky');
    for i:=0 to Imp.DestinationFields.Count-1 do
    begin
      writeln(Imp.DestinationFields[i]);
    end;
    writeln('');

    if Imp.Delimiter=#9 then
      writeln('Delimiter: <TAB>')
    else
      writeln('Delimiter: ',Imp.Delimiter);

    writeln('Show auto-generated mapping:');
    PrintMapping;

    writeln('');
    writeln('Now add our own mapping:');
    Imp.AddMapping('id','this/is\tricky');
    PrintMapping;

    writeln('');
    writeln('Total lines:');
    writeln(Imp.GetRowCount);
    writeln('Press any key.');
    readln();

    writeln('');
    writeln('Simulating import run');

    while Imp.ReadRow do
    begin
      writeln('');
      writeln('Begin of record '+inttostr(Imp.Row));
      for DestField:=0 to Imp.DestinationFields.Count-1 do
      begin
        writeln(
          'DB field '+Imp.DestinationFields[DestField]+
          ' gets value: '+Imp.GetData(Imp.DestinationFields[DestField])
          );
      end;
      writeln('Saving record '+inttostr(Imp.Row));
      writeln('');
    end;
  finally
    Imp.Free;
  end;
end.

