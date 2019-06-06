
Program tsort2;

{ Unix tsort clone for multiple platforms (Windows, OS X, Linux)
  Topological sorter to parse e.g. dependencies.

  Copyright (C) 2010-2012 Reinier Olislagers

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
{$IFDEF FPC}{$mode objfpc}{$ENDIF}

Uses
  {$IFDEF UNIX}
cwstring, {* widestring support for unix *}
  {$IFDEF UseCThreads}
cthreads,
  {$ENDIF UseCThreads}
  {$ENDIF UNIX}
Classes,
SysUtils,
CustApp,
topologicalsort {*for actual sorting *};

Type
  { Tsort2 }
  TTsort2 = Class(TCustomApplication)
    Protected
    Procedure DoRun;
    override;
    
	Public
    constructor Create(TheOwner: TComponent);
    override;
    destructor Destroy;
    override;
    Procedure WriteHelp;
	Procedure WriteVersion;
	
  End;

  { Tsort2 }

Procedure TTsort2.DoRun;

Var
  ErrorMsg: string;
  InputFile: string;
  InputList: TStringList;
  TopSort: TTopologicalSort;
  OutputList: TStringList;
  Counter: Integer;
  
  LongOptions: TStringList;
  //Long format possible options
  OptionValues: TStringList;
  //Command line options extracted by CheckOptions
  OtherArguments: TStringList;
  //Other command line arguments (that are no options) extracted by CheckOptions
Begin
  ErrorMsg := '';
  InputFile := '';
  LongOptions := TStringList.Create;
  LongOptions.Add('help');
  LongOptions.Add('version');
  OptionValues := TStringList.Create;
  OtherArguments := TStringList.Create;
  // Check parameters; parse into results
  ErrorMsg := CheckOptions('hv', LongOptions, OptionValues, OtherArguments);
  If ErrorMsg <> '' Then
    Begin
      //ShowException(Exception.Create(ErrorMsg)); // a bit too harsh
      writeln('Wrong command or option specified: ');
      writeln(ErrorMsg);
      writeln();
      WriteHelp;
      LongOptions.Free;
      OptionValues.Free;
      OtherArguments.Free;
      Terminate;
      Exit;
    End;

  // parse parameters
  If HasOption('h', 'help') Then
    Begin
      WriteHelp;
      LongOptions.Free;
      OptionValues.Free;
      OtherArguments.Free;
      Terminate;
      Exit;
    End;

  If HasOption('v', 'version') Then
    Begin
      WriteVersion;
      LongOptions.Free;
      OptionValues.Free;
      OtherArguments.Free;
      Terminate;
      Exit;
    End;
    // Get input file
    Begin
      // User could have just specified a InputFile
      If OtherArguments.Count > 0 Then
        Begin
          InputFile := OtherArguments[0];
        End
      Else // input from terminal
        Begin
          InputFile := '';
          //default value we rely on later
        End;
    End;
		
  Begin
    // Free up command line option-related varliabes
    LongOptions.Free;
    OptionValues.Free;
    OtherArguments.Free;
	
	//Actual sort
	InputList := TStringList.Create;
	TopSort := TTopologicalSort.Create;
	OutputList := TStringList.Create; 
	try
       if InputFile='' then
       begin
         writeln(' **** standard input not implemented yet *** todo***');
         halt(1);
       end;
	   InputList.LoadFromFile(InputFile);
	   TopSort.Sort(InputList, OutputList);
	   for Counter := 0 to OutputList.Count -1 do
	   begin
	     writeln(OutputList[Counter]);
	   end;	   	   
	finally
	   OutputList.Free;
	   TopSort.Free;
	   InputList.Free;
	end;
    // stop program loop
  End;
  Terminate;
End;

constructor TTsort2.Create(TheOwner: TComponent);
Begin
  inherited Create(TheOwner);
  StopOnException := True;
End;

destructor TTsort2.Destroy;
Begin
  inherited Destroy;
End;

Procedure TTsort2.WriteHelp;
Begin
  writeln(Title, ' usage: tsort2 [OPTION] [FILE]');
  writeln(' Write totally ordered list consistent with the partial ordering in FILE.');
  writeln(' With no FILE, or when FILE is -, read standard input.');
  writeln(' **** standard input not implemented yet *** todo***');
  writeln(' ');
  writeln(' --help     display this help and exit');
  writeln(' --version: output version information and exit');
  writeln('Freeware but no warranties, express or implied.');
  writeln('For full copyright and license, please see the source code.');
End;

Procedure TTsort2.WriteVersion;
Begin
  writeln('Version: we don''t really have a version number.');
End;

Var
  Application: TTsort2;

Begin
  Application := TTsort2.Create(Nil);
  Application.Title := 'sort2';
  {$IFDEF CRAZYDEBUG}
  writeln(stderr, 'Debug: ', DateTimeToStr(Now), ': Application started.');
  {$ENDIF}
  Application.Run;
  {$IFDEF CRAZYDEBUG}
  writeln(stderr, 'Debug: ', DateTimeToStr(Now), ': Application finished.');
  {$ENDIF}

  Application.Free;
End.
