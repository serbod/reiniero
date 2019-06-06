program fixscheduledtasks;
{
Fixes corrupt scheduled tasks
Basically automates the steps mentioned in
http://support.microsoft.com/kb/2305420

requires setacl.exe to be present in path

For all scheduled tasks, the program
- exports the task XML definition file
- deletes the task
- recreates the task from the exported file
thereby fixing corrupted tasks errors

Code refers to the steps in that KB article
NOTE:
- MUST be compiled with x64 compiler if you are running x64/64 bit Windows
- MUST be compiled with x86 compiler if you are running x86/32 bit Windows

Code works on x64 Windows 7; could be modularized.
}

{
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
{$R *.res}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils,
  Classes, registry,
  processutils, shlobj;

type
  TSchedTask=record
    TaskName: string; //Name of registry item HKLM\Software\Microsoft\Windows NT\CurrentVersion\Schedule\TaskCache\Tree\...
    GUID: string; //Registry HKLM\Software\Microsoft\Windows NT\CurrentVersion\Schedule\TaskCache\Tree\...\id
    TreeLocation: string; //KB step 1.3 location of scheduled task info in registry (somewhere under HKLM\Software\Microsoft\Windows NT\CurrentVersion\Schedule\TaskCache\tree)
    PlainLogonBootLocation: string; {KB step 1.4: location of task in
    HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Schedule\TaskCache\Plain
    Or:
    HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Schedule\TaskCache\Logon
    Or:
    HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Schedule\TaskCache\Boot
    }
    DefinitionFile: string; //XML file defining task
  end;
  TSchedTaskArray=array of TSchedTask;

const
  BootPart='SOFTWARE\Microsoft\Windows NT\CurrentVersion\Schedule\TaskCache\Boot';
  CompatPart='SOFTWARE\Microsoft\Windows NT\CurrentVersion\Schedule\TaskCache\CompatibilityAdapter\Signatures'; //compatibility adapters
  DefinitionPart='SOFTWARE\Microsoft\Windows NT\CurrentVersion\Schedule\TaskCache\Tasks'; //XML definition files are below this somewhere
  LogonPart='SOFTWARE\Microsoft\Windows NT\CurrentVersion\Schedule\TaskCache\Logon';
  PlainPart='SOFTWARE\Microsoft\Windows NT\CurrentVersion\Schedule\TaskCache\Plain';
  TasksPart='SOFTWARE\Microsoft\Windows NT\CurrentVersion\Schedule\TaskCache\Tasks';
  TreePart='SOFTWARE\Microsoft\Windows NT\CurrentVersion\Schedule\TaskCache\Tree'; //contains all scheduled tasks

var
  DefinitionPrefix: string; //beginning of path to tasks folder on system drive

function FileCopy(Source, Target: string): boolean;
// Copies source to target; overwrites target.
// Caches entire file content in memory.
// Returns true if succeeded; false if failed
var
  MemBuffer: TMemoryStream;
begin
  result:=false;
  MemBuffer:=TMemoryStream.Create;
  try
    try
      MemBuffer.LoadFromFile(Source);
      MemBuffer.Position:=0;
      MemBuffer.SaveToFile(Target); //may be same as source
      result:=true;
    except
      result:=false; //swallow exception; convert to error code
    end;
  finally
    MemBuffer.Free;
  end;
end;

function GetPlainLogonBootLocation(RootKey: HKEY; const TaskID: string): string;
// Look up task ID in plain logon or boot locations
var
  Name: string;
  Registry: TRegistry;
  SubKeyNames: TStringList;
begin
  Result:='';
  Registry:=TRegistry.Create;
  try
    Registry.RootKey:=RootKey;
    if not(Registry.OpenKeyReadOnly(PlainPart)) then
    begin
      writeln('GetPlainLogonBootLocation: Error opening key'+PlainPart+'. Aborting');
      halt(21);
    end;
    if Registry.HasSubKeys then
    begin
      SubKeyNames := TStringList.Create;
      try
        Registry.GetKeyNames(SubKeyNames); //get "subdirectory" names
        Registry.CloseKey;
        for Name in SubKeyNames do
        begin
          if Uppercase(Name)=uppercase(TaskID) then
          begin
            exit(PlainPart+'\'+TaskID);
          end;
        end;
      finally
        SubKeyNames.Free;
      end;
    end
    else
    begin
      //No subkeys
      Registry.CloseKey;
    end;

    if not(Registry.OpenKeyReadOnly(LogonPart)) then
    begin
      writeln('GetPlainLogonBootLocation: Error opening key'+LogonPart+'. Aborting');
      halt(22);
    end;
    if Registry.HasSubKeys then
    begin
      SubKeyNames := TStringList.Create;
      try
        Registry.GetKeyNames(SubKeyNames); //get "subdirectory" names
        Registry.CloseKey;
        for Name in SubKeyNames do
        begin
          if Uppercase(Name)=uppercase(TaskID) then
          begin
            exit(LogonPart+'\'+TaskID);
          end;
        end;
      finally
        SubKeyNames.Free;
      end;
    end
    else
    begin
      //No subkeys
      Registry.CloseKey;
    end;

    if not(Registry.OpenKeyReadOnly(BootPart)) then
    begin
      writeln('GetPlainLogonBootLocation: Error opening key'+BootPart+'. Aborting');
      halt(23);
    end;
    if Registry.HasSubKeys then
    begin
      SubKeyNames := TStringList.Create;
      try
        Registry.GetKeyNames(SubKeyNames); //get "subdirectory" names
        Registry.CloseKey;
        for Name in SubKeyNames do
        begin
          if Uppercase(Name)=uppercase(TaskID) then
          begin
            exit(BootPart+'\'+TaskID);
          end;
        end;
      finally
        SubKeyNames.Free;
      end;
    end
    else
    begin
      //No subkeys
      Registry.CloseKey;
    end;
  finally
    Registry.Free;
  end;
end;

function GetDefinitionFile(RootKey: HKEY; const Key: string; const TaskID: string): string;
// Look up XML definition file for specified task in registry
var
  Name: string;
  Registry: TRegistry;
  SubKeyNames: TStringList;
  SysDrivePath: Array[0..MaxPathLen] of char;
begin
  result:='';
  // One-time setup
  if DefinitionPrefix='' then
  begin
    SysDrivePath:='';
    SHGetSpecialFolderPath(0,SysDrivePath,CSIDL_SYSTEM,false); //gets e.g. C:\Windows\System32
    DefinitionPrefix:=IncludeTrailingPathDelimiter(SysDrivePath)+'Tasks'; //e.g. c:\windows\system32\tasks
  end;

  Registry:=TRegistry.Create;
  try
    Registry.RootKey:=RootKey;
    if not(Registry.OpenKeyReadOnly(Key)) then
    begin
      writeln('GetDefinitionFile: Error opening key'+Key+'. Aborting');
      halt(20);
    end;
    if Registry.HasSubKeys then
    begin
      SubKeyNames := TStringList.Create;
      try
        Registry.GetKeyNames(SubKeyNames); //get "subdirectory" names
        Registry.CloseKey;
        for Name in SubKeyNames do
        begin
          if Uppercase(Name)=uppercase(TaskID) then
          begin
            try
              Registry.OpenKeyReadOnly(Key+'\'+Name);
              result:=DefinitionPrefix+Registry.ReadString('Path');
              Registry.CloseKey;
              // e.g. read this in registry
              //\Microsoft\Windows\Offline Files\Background Synchronization
              //add e.g. prefix
              //C:\Windows\System32\Tasks
            except
              on E: Exception do
              begin
                writeln('GetDefinitionFile: error getting task definition file for task '+TaskID+': '+E.Message);
                exit('');
              end;
            end;
          end
          else
          begin
            // Perhaps subdirectory, not a task definition
            GetDefinitionFile(RootKey,Key+'\'+Name,TaskID);
          end;
        end;
      finally
        SubKeyNames.Free;
      end;
    end
    else
    begin
      //No subkeys
      Registry.CloseKey;
    end;
  finally
    Registry.Free;
  end;
end;

function IsTask(RootKey: HKEY; const ParentKey: string; const TaskName: string; out IsATask: boolean): TSchedTask;
// Finds out if the entry is a scheduled task (not the definition but the entry in tree)
// Returns IsTask=true if it is a task as well as task info
var
  Key: string;
  Registry: TRegistry;
begin
  IsATask:=false;
  Registry:=TRegistry.Create;
  try
    Registry.RootKey:=RootKey;
    Key:=ParentKey+'\'+TaskName;
    if not(Registry.OpenKeyReadOnly(Key)) then
    begin
      writeln('IsTask: Error opening key'+Key+'. Aborting');
      exit;
    end;
    try
      //Expect names Id as Reg_SZ (Guid task ID) and Index as Reg_DW
      if (Registry.ReadString('Id')<>'') and
        (Registry.ReadInteger('Index')>0) {todo: is this correct? does it start with 1?} then
      begin
        IsATask:=true;
        Result.TaskName:=TaskName;
        Result.TreeLocation:=Key;
        Result.GUID:=Registry.ReadString('Id');
        Result.DefinitionFile:=GetDefinitionFile(RootKey,DefinitionPart,Result.GUID);
        Result.PlainLogonBootLocation:=GetPlainLogonBootLocation(RootKey,Result.GUID);
      end;
    except
      IsATask:=false;
    end;
    Registry.CloseKey;
  finally
    Registry.Free;
  end;
end;

function DeleteKey(RootKey: HKEY; const Key: string; const Verbose: boolean): boolean;
var
  Registry: TRegistry;
begin
  result:=false;
  if (Key=BootPart) or
    (Key=CompatPart) or
    (Key=DefinitionPart) or
    (Key=LogonPart) or
    (Key=PlainPart) or
    (Key=TasksPart) or
    (Key=TreePart) then
  begin
    writeln('DeleteKey: sorry, not going to delete key '+Key+'. That will mess up your system. Please fix the code. Aborting.');
    halt(17);
  end;

  Registry:=TRegistry.Create;
  try
    Registry.RootKey:=RootKey;
    try
      result:=Registry.DeleteKey(Key);
    except
      result:=false;
    end;

    if (result=false) and Verbose then
      writeln('Error deleting registry key '+Key);
  finally
    Registry.Free;
  end;
end;

function DeleteValue(RootKey: HKEY; const Key: string; const Name: string; const Verbose: boolean): boolean;
var
  Registry: TRegistry;
begin
  result:=false;
  Registry:=TRegistry.Create;
  try
    Registry.RootKey:=RootKey;
    if Registry.OpenKey(Key,False) then
    begin
      try
        result:=Registry.DeleteValue(Name);
      except
        result:=false;
      end;
      Registry.CloseKey;
    end;
    if (result=false) and Verbose then
      writeln('Error deleting registry name entry '+Key+'\'+Name);
  finally
    Registry.Free;
  end;
end;


function ListTasks(RootKey: HKEY; const Key: string; var Tasks: TSchedTaskArray): boolean;
var
  FoundTask: TSchedTask;
  IsATask: boolean;
  Name: string;
  Registry: TRegistry;
  SubKeyNames: TStringList;
begin
  result:=false;
  Registry:=TRegistry.Create;
  try
    Registry.RootKey:=RootKey;
    if not(Registry.OpenKeyReadOnly(Key)) then
    begin
      writeln('Error opening key'+Key+'. Aborting');
      halt(18);
    end;
    if Registry.HasSubKeys then
    begin
      SubKeyNames := TStringList.Create;
      try
        Registry.GetKeyNames(SubKeyNames); //get "subdirectory" names
        for Name in SubKeyNames do
        begin
          FoundTask:=IsTask(RootKey,Key,Name,IsATask);
          if IsATask then
          begin
            SetLength(Tasks, Length(Tasks)+1);
            Tasks[High(Tasks)]:=FoundTask;
          end
          else
          begin
            // Subdirectory, not a task
            ListTasks(RootKey,Key+'\'+Name,Tasks)
          end;
        end;
        Registry.CloseKey;
      finally
        SubKeyNames.Free;
      end;
    end
    else
    begin
      //No subkeys, which is certainly a possibility if there are no tasks there
    end;
  finally
    Registry.Free;
  end;
  result:=true;
end;

function LocationWithinTree(Key: string): string;
// Gives relative location below treepart folder of registry
const
  TreePlusDel=Treepart+'\';
begin
  result:='';
  if copy(Key,1,length(TreePlusDel))=TreePlusDel then
  begin
    result:=copy(Key,length(TreePlusDel)+1,maxint);
  end;
end;

function CheckSetACL: boolean;
// Check that we have setacl.exe
begin
  if ExecuteCommand('setacl -help',false)<>1 then //apparently returns result code 1
    result:=false;
end;

function SetRegPermissions(RootKey: HKEY; Key: string): boolean;
// Gives administrators group full control to registry key.
// Expects key without starting or trailing \
var
  Command: string;
  ReturnCode:integer;
begin
  result:=false;
  if copy(key,1,1)='\' then
    raise exception.create('do not give me starting \. todo fix this clunky code.');
  if RootKey=HKEY_LOCAL_MACHINE then
    command:='setacl -on "HKLM\'+Key+'" -ot reg -actn setowner -ownr "n:Administrators;s:N"'
  else
    raise exception.create('fix this code for other parts of the registry.');
  ReturnCode:=executecommand(Command,false);
  result:=(Returncode=0);
  if returncode<>0 then
    writeln('SetRegPermissions: error: '+Command+' gave result code ',returncode);
end;

var
  Command: string;
  i:integer;
  Tasks: TSchedTaskArray;
  ATask: TSchedTask;
  TempFile: string;
begin
  if not(CheckSetACL) then
  begin
    writeln('No setacl.exe found in path. Please make sure it is in your path. Aborting.');
    halt(10);
  end;

  SetLength(Tasks,0);
  if ListTasks(HKEY_LOCAL_MACHINE,TreePart,Tasks) then
  begin
    writeln('Tasks:');
    writeln('Found ',Length(tasks),' tasks.');
    for i:=Low(Tasks) to high(Tasks) do
    begin
      ATask:=Tasks[i];
      writeln(''); //especially if you have many tasks, some vertical space is nice
      writeln('Step 1: getting info for: '+ATask.TaskName);
      if ATask.TaskName='' then
      begin
        writeln('Empty task name found. This should not happen. Aborting.');
        halt(18);
      end;

      TempFile:=''; //fail by default; check for empty string is used by step 4 below
      if fileexists(ATask.DefinitionFile) then
      begin
        TempFile:=Sysutils.GetTempFileName;
        writeln('Step 2: copying XML definition file from '+ATask.DefinitionFile+' to '+TempFile);
        if not(filecopy(ATask.DefinitionFile,TempFile)) then
        begin
          writeln('Error copying file. Aborting.');
          halt(30);
        end;
        sleep(100); //wait for filesystem to settle
        writeln('Step 2: deleting old XML definition file.');
        if not(DeleteFile(ATask.DefinitionFile)) then
        begin
          writeln('Error deleting old definition file. Aborting.');
          halt(31);
        end;
        sleep(100); //wait for filesystem to settle
      end
      else
      begin
        writeln('Step 2: there is no XML definition file in '+ATask.DefinitionFile+' so not going to copy this. Continuing.');
      end;

      writeln('Step 3: deleting registry entries for task.');
      Command:=ATask.TreeLocation;
      writeln('3.1: from '+Command);
      if (SetRegPermissions(HKEY_LOCAL_MACHINE, Command)=false) or
        (DeleteKey(HKEY_LOCAL_MACHINE,Command,true)=false) then
      begin
        writeln('Aborting.');
        halt(32);
      end;

      Command:=TasksPart+'\'+ATask.GUID;
      writeln('3.2: from '+Command);
      // This may be missing; if so it's not important
      try
        SetRegPermissions(HKEY_LOCAL_MACHINE,Command);
        DeleteKey(HKEY_LOCAL_MACHINE,Command,false);
      except
        on E: Exception do
        begin
          writeln('Informational: exception: '+E.Message+'. Continuing.');
        end;
      end;

      Command:=ATask.PlainLogonBootLocation;
      writeln('3.3 from Plain/Boot/Logon registry sections: '+Command);
      // This may be missing; if so it's not important
      try
        SetRegPermissions(HKEY_LOCAL_MACHINE,Command);
        DeleteKey(HKEY_LOCAL_MACHINE,Command,false);
      except
        on E: Exception do
        begin
          writeln('Informational: exception: '+E.Message+'. Continuing.');
        end;
      end;

      writeln('3.x not mentioned in KB: delete from CompatibilityAdapter');
      // CompatibilityAdapter probably not used for all entries so ignore errors
      try
        DeleteValue(HKEY_LOCAL_MACHINE,CompatPart,ATask.TaskName+'.job',false);
        DeleteValue(HKEY_LOCAL_MACHINE,CompatPart,ATask.TaskName+'.job.fp',false);
      except
        on E: Exception do
        begin
          writeln('Informational: exception: '+E.Message+'. Continuing.');
        end;
      end;

      writeln('Step 4: recreate task from temporary definition file');
      //Schtasks.exe /CREATE /TN %TASKNAME% /XML "%TEMP%\Tasks\%TASKNAME:~1,-1%"
      //for folders, use \ in taskname...
      if TempFile='' then
      begin
        writeln('No definition file was present so cannot recreate task. Continuing with next task.');
      end
      else
      begin
        Command:='schtasks /CREATE /TN "'+LocationWithinTree(ATask.TreeLocation)+'" /XML "'+TempFile+'" ';
        if ExecuteCommand(Command,false)<>0 then
        begin
          writeln('Error running schtasks to import scheduled task. Aborting.');
          writeln('Command was: ');
          writeln(Command);
          halt(35);
        end;
        TempFile:='';
      end;
    end;
  end
  else
  begin
    writeln('Error getting tasks');
  end;
end.
