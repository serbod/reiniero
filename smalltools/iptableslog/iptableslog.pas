{*
This source code is provided under the MIT license:
Copyright (C) 2011-2013 by Reinier Olislagers

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
unit iptableslog;
// Import IPTables firewall output from an rsyslog file and dump it as csv
// Todo: check for compatiblity with syslog, maybe syslog-ng

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr;

type

  { TIPTablesLog }
  //todo: convert to e.g. bufdataset so we can do more with it
  //include check for assigned so we don't get nasty errors
  TIPTablesLogRecord = record
  {See e.g.
  https://secure.wikimedia.org/wikipedia/en/wiki/Transmission_Control_Protocol
  }
    TimeStamp: TDateTime; //Syslog event timestamp
    PrecisionTimestamp: string;
    //There apparently is some kind of precision timestamp in rsyslog (but not in BSD syslog)
    Host: string; //Host where syslog event originated
    Chain: string; //Name of the chain generating the event
    InterfaceIn: string; //Incoming interface (IN= field)
    InterfaceOut: string; //Outgoing interface (OUT=)
    Source: string; //Source IP address (SRC=)
    MAC: string; //Source? MAC address (MAC=)
    SourcePort: integer; //Source port (SPT=)
    Destination: string; //Destination (DEST=)
    DestinationPort: integer; //Destination port (DPT=)
    PacketLength: integer; //Length of packet (LEN=)
    TOS: string; //TOS flags (TOS=), a hex value
    PREC: string; //(PREC=) what does this mean?; a hex value
    ID: integer; //ID of packet??
    TTL: integer; //Time-to-live count (TTL=)
    Protocol: string; //Protocol (e.g. TCP, UDP) (PROTO=)
    Window: integer; //Receive window size (WINDOW=)
    RES: string; //Reserved field (RES=)
  {
  //IP flags not found in my output
  NS: boolean; //ECN-nonce concealment protection flag
  CWR: boolean; //Congestion Window Reduced flag
  ECE: boolean; //ECE flag
  }
    URG: boolean; //Urgent flag (URGP=) (for OOB transmission, not widely used)
    ACK: boolean; //Acknowledgement field (ACK) (all data packets after handshake need this set)
    PSH: boolean; //Push
    RST: boolean; //Reset (RST) (break connection)
    SYN: boolean; //SYN flag (handshake: SYN=>SYN+ACK=>ACK)
    FIN: boolean;
    //FIN: disconnect half of the connection (full disconnect handshake e.g. FIN=>FIN+ACK=>ACK)
    DF: boolean; //Don't fragment
  end;

  TIPTablesLog = class(TObject)
  private
    //Log lines
    FLines: TStringList;
    FLinesProcessed: integer;
    //Input file
    FLogFile: string;
    //File where parsed output will be written
    FOutputFile: string;
    // Converts various fields 'Jan',' ','1','08:34:55' into date/time
    function GetTimeStamp(const MonthName, Param2, Param3, Param4: string): TDateTime;
    function ValueAfter(const DataList: TStringList; const SearchFor: string;
      FirstField: integer): string;
    function Quote(const ABoolean: boolean): string;
    function Quote(const PlainString: string): string;
    function Quote(const ATimeStamp: TDateTime): string;
  public
    //Source file with messages
    property InputFile: string read FLogFile write FLogFile;
    property LinesProcessed: integer read FLinesProcessed;
    //Destination file
    property OutputFile: string read FOutputFile write FOutputFile;
    //Process input and write output
    procedure Parse;
    //Show output fields
    function ShowFields(): string;
    constructor Create;
    destructor Destroy; override;
  end;

const
  Delim = ',';

implementation

uses dateutils;

{ TIPTablesLog }

function TIPTablesLog.Quote(const ABoolean: boolean): string;
begin
  if ABoolean then
    Result := '1'
  else
    Result := '0';
end;

function TIPTablesLog.Quote(const PlainString: string): string;
  // FPC has bugs in delimited text export so rolling my own... sigh
const
  QuoteChar = '"';
begin
  Result := StringReplace(PlainString, QuoteChar, QuoteChar + QuoteChar,
    [rfReplaceAll, rfIgnoreCase]);
  Result := QuoteChar + PlainString + QuoteChar;
end;

function TIPTablesLog.Quote(const ATimeStamp: TDateTime): string;
const
  QuoteChar = '"';
begin
  Result := QuoteChar + FormatDateTime('yyyy-mm-dd hh:nn:ss', ATimeStamp) + QuoteChar;
end;

function TIPTablesLog.GetTimeStamp(
  const MonthName, Param2, Param3, Param4: string): TDateTime;
  // Parse month name, day, time as present in split out strings.
  // Can deal with extra space between month and day but no other formatting changes.
var
  DayInt: integer;
  HourInt: integer;
  MinuteInt: integer;
  LogYear: word;
  MonthInt: integer;
  MonthSelect: string;
  ParamShiftRight: boolean;
  SecondInt: integer;
  ThisMonth: word;
  ToDay: word;
begin
  MonthSelect := AnsiUpperCase(Copy(MonthName, 1, 3));
  case MonthSelect of
    'JAN': MonthInt := 1;
    'FEB': MonthInt := 2;
    'MAR': MonthInt := 3;
    'APR': MonthInt := 4;
    'MAY': MonthInt := 5;
    'JUN': MonthInt := 6;
    'JUL': MonthInt := 7;
    'AUG': MonthInt := 8;
    'SEP': MonthInt := 9;
    'OCT': MonthInt := 10;
    'NOV': MonthInt := 11;
    'DEC': MonthInt := 12;
    else
      MonthInt := -1;
  end;

  // We could have Mar  1 or Jan 22 (note extra space). If so, we need to shift our parameters
  if Trim(Param2) = '' then
    ParamShiftRight := True
  else
    ParamShiftRight := False;
  if ParamShiftRight then
  begin
    DayInt := StrToIntDef(Param3, -1);
    //12345678
    //23:59:59
    HourInt := StrToIntDef(Copy(Param4, 1, 2), -1);
    MinuteInt := StrToIntDef(Copy(Param4, 4, 2), -1);
    SecondInt := StrToIntDef(Copy(Param4, 7, 2), -1);
  end
  else
  begin
    DayInt := StrToIntDef(Param2, -1);
    ;
    HourInt := StrToIntDef(Copy(Param3, 1, 2), -1);
    MinuteInt := StrToIntDef(Copy(Param3, 4, 2), -1);
    SecondInt := StrToIntDef(Copy(Param3, 7, 2), -1);
  end;

  // Assume within last 12 months... so set up year
  DecodeDate(Date, LogYear, ThisMonth, ToDay);
  if MonthInt > ThisMonth then
    LogYear := LogYear - 1;
  try
    Result := EncodeDateTime(LogYear, MonthInt, DayInt, HourInt, MinuteInt, SecondInt, 0);
  except
    Result := MinDateTime; //default date that is so far out it can't be plausible
  end;
end;

function TIPTablesLog.ValueAfter(const DataList: TStringList;
  const SearchFor: string; FirstField: integer): string;
  // Searches a stringlist and gets the value after SearchFor
  // Handy for key=value lists
  // note: SearchFor may appear anywhere in the field.
  // todo: check if it can simply be replaced by stringlist .Names and .Values?
var
  Counter: integer;
  KeyPos: integer;
begin
  Result := EmptyStr;
  for Counter := FirstField to DataList.Count - 1 do
  begin
    KeyPos := AnsiPos(SearchFor, DataList[Counter]);
    if KeyPos > 0 then
    begin
      Result := Copy(DataList[Counter], KeyPos + Length(SearchFor),
        Length(DataList[Counter]));
      break; //Get out of fields loop
    end;
  end;
end;

procedure TIPTablesLog.Parse;
var
  FirstIPTablesDataField: integer; //First interesting field in output (usually IN=ethx)
  LineNum: integer;
  IPTables: TRegExpr;
  OutputTextFile: TextFile;
  TheRecord: TIPTablesLogRecord; //Single log record
  RecordList: TStringList;

  procedure DetermineFields;
  // Determines position of fields
  const
    LookFor = 'OUT=';
  var
    Counter: integer;
  begin
    //determine fields based on OUT= (IN= is problematic as the FW chain can run up to there without spaces)
    FirstIPTablesDataField := -1;
    for Counter := 0 to RecordList.Count - 1 do
    begin
      if Pos(LookFor, RecordList[Counter]) = 1 then
      begin
        FirstIPTablesDataField := Counter - 1; //Looking for the field before this field
        exit;
      end;
    end;
  end;

begin
  //If performance is bad, perhaps go back to old school readln for line by line reading
  FLines.LoadFromFile(FLogFile);
  IPTables := TRegexpr.Create;
  RecordList := TStringList.Create;
  AssignFile(OutputTextFile, OutputFile);
  try
    try
      Rewrite(OutputTextFile);
    {
    Mar  1 10:49:51 MyServer kernel: [  547.563456] [UFW ALLOW] IN=eth0 OUT= MAC=33:11:2f:90:c3:7b:00:21:27:f7:5d:41:08:00 SRC=215.14.80.193 DST=192.168.5.110 LEN=60 TOS=0x00 PREC=0x00 TTL=42 ID=16050 DF PROTO=TCP SPT=38985 DPT=22 WINDOW=5840 RES=0x00 SYN URGP=0
    Jan 21 13:25:01 router kernel: [517703.569726] [DMZ-OUTBOUND-default-D]IN=eth2 OUT=eth1 SRC=12.13.14.15 DST=172.145.153.201 LEN=40 TOS=0x00 PREC=0x00 TTL=63 ID=0 DF PROTO=TCP SPT=80 DPT=13298 WINDOW=0 RES=0x00 RST URGP=0
    }
      // Should match (only) on IPTables chains. Capture group with firewall chain name
      // Capture groups: () in the regex
      // Capture group 1 matches host name
      // Capture group 2 matches rsyslog kernel timestamp(?) that may exist, e.g.:
      // kernel: [ 77880.665120]
      // Capture group 3 matches [chainname], e.g.:
      // [DMZ-OUTBOUND-default-D]
      // Between OUT and SRC, allow for optional presence of MAC field
      // like MAC=00:11:22:33:d3:7a:00:41:24:55:66:77:18:00
      // Capture group 4,5 match MAC address including MAC
      // Capture group 6 match MAC digits only
      // The regex used may be a bit loose, but I'm more concerned about leaving out valid lines than
      // trying to include invalid ones - the NAME=VALUE handling below will deal with it.
      // OUT may have an empty value
      // We use lazy matching
      // .*?
      // before PROTO to limit our matches
      // It matches ip address etc by just looking at anything except space (\S). This should make it IPv6 ready.
      // Note: this regex implementation does not seem to support [ ] in all situations (or I don't understand it ;)
      // worked around it with ()
      IPTables.Expression := RegExprString(
        '(\w+)\skernel: (\[\s*\d+\.\d+]) (\[.+])\s*IN=\w+ OUT=\w*((\s+MAC=([a-fA-F\d\-:]*)\s)|\s+)SRC=(\S)* DST=(\S)* LEN=\d+ TOS=\w+ PREC=\w+ TTL=\d+ ID=\w+');

      RecordList.StrictDelimiter := True;
      //RecordList.QuoteChar:=''; //NULL or something?
      RecordList.Delimiter := ' ';

      //Header row
      writeln(OutputTextFile,
        Quote('TimeStamp') + Delim + Quote('PrecisionTimestamp') + Delim +
        Quote('Host') + Delim + Quote('Chain') + Delim + Quote('InterfaceIn') +
        Delim + Quote('MAC') + Delim + Quote('InterfaceOut') + Delim +
        Quote('Source') + Delim + Quote('SourcePort') + Delim + Quote('Destination') + Delim +
        Quote('DestinationPort') + Delim + Quote('Protocol') + Delim +
        Quote('ID') + Delim + Quote('PacketLength') + Delim + Quote('TOS') + Delim +
        Quote('PREC') + Delim + Quote('TTL') + Delim + Quote('SYN') + Delim +
        Quote('ACK') + Delim + Quote('FIN') + Delim + Quote('RST') + Delim +
        Quote('PSH') + Delim + Quote('URG') + Delim + Quote('DF') + Delim +
        Quote('Window') + Delim + Quote('Reserved')
        );
      LineNum := 0; //Let's follow stringlist conventions and start at 0
      while (LineNum < FLines.Count) do
      begin
        // Check if we have a match for the regex: a valid firewall log entry
        if IPTables.Exec(RegExprString(FLines[LineNum])) then
        begin
        {$IFDEF DEBUG}
        {
        // Will only work with console, of course..
        writeln('cap group 1:'+IPTables.Match[1]);
        writeln('cap group 2:'+IPTables.Match[2]);
        writeln('cap group 3:'+IPTables.Match[3]);
        writeln('cap group 4:'+IPTables.Match[4]);
        writeln('cap group 5:'+IPTables.Match[5]);
        writeln('cap group 6:'+IPTables.Match[6]);
        }
        {$ENDIF DEBUG}
          RecordList.DelimitedText := FLines[LineNum];
          //Fixed fields
          TheRecord.TimeStamp := GetTimeStamp(RecordList[0], RecordList[1],
            RecordList[2], RecordList[3]);
          //We're getting rid of enclosing [ and ]
          TheRecord.PrecisionTimestamp :=
            Trim(Copy(string(IPTables.Match[2]), 2, Length(string(IPTables.Match[2])) - 2));
          //We're getting rid of enclosing [ and ]
          TheRecord.Chain := Trim(
            Copy(string(IPTables.Match[3]), 2, Length(string(IPTables.Match[3])) - 2));
          TheRecord.Host := string(IPTables.Match[1]);
          TheRecord.MAC := string(IPTables.Match[6]);

          // Variable fields. We could (and did) use fixed field positions.
          // While quicker, it's more tedious to maintain and breaks whenever
          // the logging format changes.

          // We have to check for fields each time, as the number of spaces may change
          DetermineFields;

          TheRecord.InterfaceIn := ValueAfter(RecordList, 'IN=', FirstIPTablesDataField);
          TheRecord.InterfaceOut := ValueAfter(RecordList, 'OUT=', FirstIPTablesDataField);
          TheRecord.Source := ValueAfter(RecordList, 'SRC=', FirstIPTablesDataField);
          TheRecord.Destination := ValueAfter(RecordList, 'DST=', FirstIPTablesDataField);
          TheRecord.ID := StrToIntDef(ValueAfter(RecordList, 'ID=',
            FirstIPTablesDataField), -1);
          TheRecord.PacketLength :=
            StrToIntDef(ValueAfter(RecordList, 'LEN=', FirstIPTablesDataField), -1);
          TheRecord.TOS := ValueAfter(RecordList, 'TOS=', FirstIPTablesDataField);
          TheRecord.PREC := ValueAfter(RecordList, 'PREC=', FirstIPTablesDataField);
          TheRecord.TTL := StrToIntDef(ValueAfter(RecordList, 'TTL=',
            FirstIPTablesDataField), -1);
          TheRecord.DF := (RecordList.IndexOf('DF') > 0);
          TheRecord.Protocol := ValueAfter(RecordList, 'PROTO=', FirstIPTablesDataField);
          TheRecord.SourcePort :=
            StrToIntDef(ValueAfter(RecordList, 'SPT=', FirstIPTablesDataField), -1);
          TheRecord.DestinationPort :=
            StrToIntDef(ValueAfter(RecordList, 'DPT=', FirstIPTablesDataField), -1);
          TheRecord.Window := StrToIntDef(ValueAfter(RecordList, 'WINDOW=',
            FirstIPTablesDataField), -1);
          TheRecord.RES := ValueAfter(RecordList, 'RES=', FirstIPTablesDataField);
          TheRecord.RST := (RecordList.IndexOf('RST') > 0);
          TheRecord.URG := (ValueAfter(RecordList, 'URGP=', FirstIPTablesDataField) = '1');
          //TheRecord.NS:= false; //IP flag not found in my output
          //TheRecord.CWR:=false; //IP flag not found in my output
          //TheRecord.ECE:=false; //IP flag not found in my output
          TheRecord.ACK := (RecordList.IndexOf('ACK') > 0);
          TheRecord.PSH := (RecordList.IndexOf('PSH') > 0);
          TheRecord.SYN := (RecordList.IndexOf('SYN') > 0);
          TheRecord.FIN := (RecordList.IndexOf('FIN') > 0);

          //write out
          writeln(OutputTextFile,
            Quote(TheRecord.TimeStamp) + Delim +
            Quote(TheRecord.PrecisionTimestamp) + Delim +
            Quote(TheRecord.Host) + Delim + Quote(TheRecord.Chain) + Delim +
            Quote(TheRecord.InterfaceIn) + Delim + Quote(TheRecord.MAC) + Delim +
            Quote(TheRecord.InterfaceOut) + Delim + Quote(TheRecord.Source) + Delim +
            IntToStr(TheRecord.SourcePort) + Delim +
            Quote(TheRecord.Destination) + Delim +
            IntToStr(TheRecord.DestinationPort) + Delim +
            Quote(TheRecord.Protocol) + Delim + IntToStr(TheRecord.ID) + Delim +
            IntToStr(TheRecord.PacketLength) + Delim + Quote(TheRecord.TOS) + Delim +
            Quote(TheRecord.PREC) + Delim + IntToStr(TheRecord.TTL) + Delim +
          {
          Quote(TheRecord.NS)+Delim+
          Quote(TheRecord.CWR)+Delim+
          Quote(TheRecord.ECE)+Delim+
          }
            Quote(TheRecord.SYN) + Delim + Quote(TheRecord.ACK) + Delim +
            Quote(TheRecord.FIN) + Delim + Quote(TheRecord.RST) + Delim +
            Quote(TheRecord.PSH) + Delim + Quote(TheRecord.URG) + Delim +
            Quote(TheRecord.DF) + Delim + IntToStr(TheRecord.Window) + Delim +
            Quote(TheRecord.RES));
        end
        else
        begin
          //optionally output something
        {$IFDEF DEBUG}
          //run with e.g. -dDEBUG (e.g. in project options/custom/other)
          writeln('No match for line: (enclosed by *)');
          writeln('*' + FLines[LineNum] + '*');
        {$ENDIF}
        end;
        LineNum := LineNum + 1;
      end;
    except
      on E: Exception do
      begin
        writeln('Error occurred       : ' + E.ClassName + '/' + E.Message);
        writeln('Specified input file : ' + InputFile);
        writeln('Specified output file: ' + OutputFile);
        halt(13);
      end;
    end;
  finally
    FLinesProcessed := LineNum; //Should work as linenum has been increased in last loop
    IPTables.Free;
    RecordList.Free;
    Close(OutputTextFile);
  end;
end;

function TIPTablesLog.ShowFields(): string;

begin
  //todo: use an array with field names etc, or better yet:
  //rebuild record into some kind of class/list that has metadata (field name)
  //this will avoid problems with changing field names/positions
  Result :=
    '## Field name         Format           Description:' + LineEnding +
    '== ================== ================ ========================================' + LineEnding
    +
    '01 TimeStamp          Date/Time        Syslog event timestamp' +
    LineEnding + '02 PrecisionTimestamp Floating point   Precision timestamp? (not in BSD syslog)'
    +
    LineEnding + '03 Host               Text             Hostname where syslog event originated'
    +
    LineEnding + '04 Chain              Text             Name of the iptables chain' +
    LineEnding + '05 InterfaceIn        Text             Incoming interface (IN= field)' +
    LineEnding + '06 MAC                Text             Concatenated MAC addresses of' +
    LineEnding + '                                       firewall and source (MAC=)' +
    LineEnding + '07 InterfaceOut       Text             Outgoing interface (OUT=)' +
    LineEnding + '08 Source             Text             Source IP address (SRC=)' +
    LineEnding + '09 SourcePort         Integer          Source port (SPT=)' +
    LineEnding + '10 Destination        Text             Destination (DEST=)' +
    LineEnding + '11 DestinationPort    Integer          Destination port (DPT=)' +
    LineEnding + '12 Protocol           Text             Protocol (e.g. TCP, UDP) (PROTO=)' +
    LineEnding + '13 ID                 Integer          ID of packet?? (ID=)' +
    LineEnding + '14 PacketLength       Integer          Length of packet (LEN=)' +
    LineEnding + '15 TOS                Text (hex)       TOS (Type of Service) flags (TOS=)' +
    LineEnding + '16 PREC               Text (hex)       Unknown (PREC=)' +
    LineEnding + '17 TTL                Integer          Time-to-live count (TTL=)' +
    LineEnding + '18 SYN                Boolean          SYN flag (SYN)' +
    LineEnding + '19 ACK                Boolean          Acknowledgement flag (ACK)' +
    LineEnding + '20 FIN                Boolean          FIN flag: no more data from sender (FIN)'
    +
    LineEnding + '21 RST                Boolean          Reset flag (RST)' +
    LineEnding + '22 PSH                Boolean          Push flag (PSH)' +
    LineEnding + '23 URG                Boolean          Urgent flag (URGP=)' +
    LineEnding + '24 DF                 Boolean          Don''t fragment flag (DF)' +
    LineEnding + '25 Window             Integer          Receive window size (WINDOW=)' +
    LineEnding + '26 Reserved           Text (hex)       Reserved field (RES=)' +
    LineEnding + LineEnding;
end;

constructor TIPTablesLog.Create;
begin
  inherited Create;
  FLines := TStringList.Create;
  FLogFile := '/var/log/messages';
  FOutputFile := 'iptableslog.csv';
  FLinesProcessed := 0;
end;

destructor TIPTablesLog.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

end.
