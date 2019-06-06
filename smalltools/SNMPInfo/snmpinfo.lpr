program snmpinfo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, Sysutils, LCL {required for synapse}, snmpsend, asn1util, BufDataset;

//{$R *.res} //no resources required

function SNMPSend(Host, Community, OID: string): string;
var
  aSNMPSend: TSNMPSend;
  i: Integer;
  CurMIB: TSNMPMib;
begin
  aSNMPSend := TSNMPSend.Create;
  aSNMPSend.Query.Clear;
  aSNMPSend.Query.Community := Community;
  aSNMPSend.Query.PDUType := PDUGetRequest;
  aSNMPSend.TargetHost := Host;
  writeln('Sending SNMP request.');
  aSNMPSend.Query.MIBAdd(OID, '', ASN1_NULL);
  if aSNMPSend.SendRequest then begin
    for i:=0 to aSNMPSend.Reply.MIBCount-1 do begin
      CurMIB:=aSNMPSend.Reply.MIBByIndex(i);
      if CurMIB.OID=OID then
        result:=CurMIB.Value;
    end;
  end else begin
    writeln('SNMP failed');
    result:='';
  end;
  aSNMPSend.Free;
end;

procedure SNMPTable(Host, Community, OID: string);
// Get SNMP table results into dataset
var
  Table    : TstringList;
  Rule     : TStringList;
  NumRows  : Integer;
  SNMPResult : Boolean;
begin
  Table := TStringList.Create;
  Table.Clear;
  Rule  := TStringList.Create;
  SNMPResult := SNMPGetTable(OID,Community,Host,Table);
  If (SNMPResult = True) then
  begin
    writeln('SNMP table results:');
    If Table.Count > 0 then
    begin
      writeln('****debug begin: ');
      writeln(Table.Text);
      writeln('****debug end: ');
      for NumRows := 1 to Table.Count-1 do
      begin
        rule.Delimiter :=',';
        rule.QuoteChar :='"';
        rule.Clear;
        rule.DelimitedText := Table[NumRows];
        writeln(rule.Text); //write one row
      end;  // of parse the results ...
    end    // of deal with each of the result lines ...
    else
    begin
      writeln('Valid device & string, but no data returned');
    end;
  end
  else writeln('Some sort of SNMP error');
  Rule.Free;
  Table.Free;
end;

procedure PrintSNMPTable(Host, Community, OID: string);
// Wrapper to print out SNMPTable procedure results
begin
 //todo: implement
end;

var
  Host: string;
  Community: string;
begin
writeln('begin');
if Paramcount<2 then
begin
  writeln('Wrong number of arguments.');
  writeln(ExtractFileName(ParamStr(0))+' <hostname> <community>');
  halt(1); // signal error in exitcode
end;
Host:=Paramstr(1);
Community:=Paramstr(2);
//to do: look at snmpwalk and preferably snmpbulk
writeln('Host name: ' + SNMPSend(Host, Community, '1.3.6.1.2.1.1.5.0')); //system.sysName.0
SNMPTable(Host, Community, '1.3.6.1.2.1.4.21'); //MIB2.ip.ipRouteTable
writeln('end');
end.

