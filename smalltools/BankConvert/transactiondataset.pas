unit transactiondataset;

{ Encapsulates TTransactionList into a dataset: allows using transactions as a
  dataset, binding to data-aware controls, etc.

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

{ Thanks to Jos Visser for his article
http://nldelphi.com/cgi-bin/articles.exe/ShowArticle?ID=10370
the Gexperts article:
http://www.gexperts.com/articles/customds.php
and FPC's
dataset.txt

Inspiration taken as well from sdfdata.pp
}
 //todo: blob stream write support
 //todo: check ftransactionlist assigned before modification/query
 //todo: process incoming changes from ftransactionlist
 //add calculated fields support
 //todo: verify all code with sdfdataset to see if nothing forgotten
 //todo: verify all code with bufdataset to see if nothing forgotten
 //todo: verify dataset.txt to see if it matches everything in this code (e.g. calculated fields!!!!, filterbuffer )
{$mode objfpc}{$H+}

interface

uses
  DB, Classes, SysUtils, transactioninfo;

type
  { TRecInfo }
  // Information about a record that the dataset needs
  // Used as the record buffer. The actual data is stored
  // in TTransactionDataset.FTransactionList already; no need to
  // copy that around.
  PRecInfo = ^TRecInfo;
  TRecInfo = packed record
    RecordNumber: PtrUint; //Index of the record in the TTransactionList so we can find it
    BookmarkFlag: TBookmarkFlag; //Stores BOF,EOF,etc??? todo check
    AccountBlob: TMemoryStream; //Stores data in blob info, if present
    ContraAccountBlob: TMemoryStream;
    ContraNameBlob: TMemoryStream;
    TransTypeBlob: TMemoryStream;
    MemoBlob: TMemoryStream;
  end;

  { TTransactionDataset }

  TTransactionDataset = class(TDataSet)
  private
    FBufSize: integer; // Size of record buffer (TRecInfo in this case)
    FCurrentRecord: integer; //current record number (return in GetRecNo); 0..transaction count-1
    //todo: change to 1..transaction count? Look at what other datasets do
    FTransactionList: TTransactionList; //actual data
  protected
    function AllocRecordBuffer: TRecordBuffer; override; //Get memory for record buffer
    procedure CloseBlob(Field: TField); override; // Destroy blobs used for variable width fields
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override; //Free record buffer
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override; //Get kind of record out of record buffer
    function GetCanModify: boolean; override; //Indicate if dataset is read/write
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: boolean): TGetResult; override;
    //Puts record information into buffer
    function GetRecordCount: longint; override; //Get number of records
    function GetRecNo: longint; override;
    procedure InternalClose; override;         //Clean up
    procedure InternalFirst; override;         //Go to first record
    procedure InternalGotoBookmark(ABookmark: Pointer); override; //Move current record to given bookmark
    procedure InternalLast; override;          //Go to last record
    procedure InternalInitFieldDefs; override; //Set up fields
    procedure InternalInitRecord(Buffer: TRecordBuffer); override; //Set up record buffer
    procedure InternalOpen; override;          //Initialization code
    procedure InternalRefresh; override;       //Reload data from underlying store
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override; //Go to record: set current record to record in buffer
    function IsCursorOpen: boolean; override;  //Is there any data/is the dataset active?
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override; //Set record type in record buffer
    procedure SetRecNo(Value: longint); override; //Set current record number
  public
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    function GetFieldData(Field: TField; Buffer: Pointer): boolean;
      overload; override; //Gets field data out of dataset field buffer ready for use
    procedure SetFieldData(Field: TField; Buffer: Pointer); overload; override; //Stores (edited) field data
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // The transactions that make up the actual data
    property TransactionList: TTransactionList read FTransactionList write FTransactionList;
  end;

implementation

const
  // Error messages:
  TDInvalidField = 'Invalid field definition for field name %s';
  TDNoRecords = 'No records'; //no records present
  TDNoTransList = 'TransactionList is not assigned.';

const
  // Field name mappings
  HashIDFldName='HashID';
  AccountFldName='Account';
  BookDateFldName='BookDate';
  AmountFldName='Amount';
  ContraAccountFldName='ContraAccount';
  ContraNameFldName='ContraName';
  TransTypeFldName='TransType';
  MemoFldName='Memo';


{ TTransactionDataset }

function TTransactionDataset.AllocRecordBuffer: TRecordBuffer;
begin
  GetMem(Result, FBufSize);
end;

procedure TTransactionDataset.CloseBlob(Field: TField);
begin
  //todo: add write support by modifying tmemorystream with a dirty property
  // and catching write method. if dirty, then save back to
  case Field.FieldName of
    AccountFldName:
      if assigned(PRecInfo(ActiveBuffer)^.AccountBlob) then
        PRecInfo(ActiveBuffer)^.AccountBlob.Free;
    ContraAccountFldName:
      if assigned(PRecInfo(ActiveBuffer)^.ContraAccountBlob) then
        PRecInfo(ActiveBuffer)^.ContraAccountBlob.Free;
    ContraNameFldName:
      if assigned(PRecInfo(ActiveBuffer)^.ContraNameBlob) then
        PRecInfo(ActiveBuffer)^.ContraNameBlob.Free;
    TransTypeFldName:
      if assigned(PRecInfo(ActiveBuffer)^.TransTypeBlob) then
        PRecInfo(ActiveBuffer)^.TransTypeBlob.Free;
    MemoFldName:
      if assigned(PRecInfo(ActiveBuffer)^.MemoBlob) then
        PRecInfo(ActiveBuffer)^.MemoBlob.Free;
  else
    // E.g. when new fields haven't been added above...
    DatabaseErrorFmt(TDInvalidField, [Field.FieldName]);
  end;
end;

procedure TTransactionDataset.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  FreeMem(Buffer, FBufsize);
end;

procedure TTransactionDataset.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  Assert(Assigned(Buffer));
  PtrUint(Data^) := PRecInfo(Buffer)^.RecordNumber;
end;

function TTransactionDataset.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  Result := PRecInfo(Buffer)^.BookMarkFlag;
end;

function TTransactionDataset.GetCanModify: boolean;
begin
  Result := true; //always allow editing
end;

function TTransactionDataset.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: boolean): TGetResult;
begin
  Result := grOK;
  case GetMode of
    gmPrior:
    begin
      if FCurrentRecord <= 0 then
      begin
        Result := grBOF;
        FCurrentRecord := -1;
      end
      else
        Dec(FCurrentRecord); //previous record is requested
    end;
    gmCurrent:
    begin
      // Check bounds of underlying data structure, which is 0 based:
      if (FCurrentRecord < 0) or (FCurrentRecord > FTransactionList.Count - 1) then
        Result := grError;
    end;
    gmNext:
    begin
      // Also return EOF if currently on last record as next record is requested
      if FCurrentRecord >= FTransactionList.Count - 1 then
        Result := grEOF
      else
        Inc(FCurrentRecord); //next record is requested
    end;
  end;

  // Get correct record info into buffer
  if Result = grOK then
  begin
    with PRecInfo(Buffer)^ do
    begin
      // Note we don't copy over record data; we reference
      // the underlying FTransactionList for the data.
      RecordNumber := FCurrentRecord;
      BookmarkFlag := bfCurrent;
    end;
  end
  else
  begin
    if (Result = grError) and DoCheck then
      DatabaseError(TDNoRecords);
  end;
end;

function TTransactionDataset.GetRecordCount: longint;
begin
  if Assigned(FTransactionList) then
    Result := FTransactionList.Count
  else
    Result := 0;
end;

function TTransactionDataset.GetRecNo: longint;
begin
  UpdateCursorPos; //ensure cursor points to correct physical position
  if (FCurrentRecord = -1) and (FTransactionList.Count > 0) then
    Result := 0 //first record
  else
    Result := FCurrentRecord;
end;

procedure TTransactionDataset.InternalClose;
begin
  //do nothing with FTransactionList as the calling code created it
  BindFields(false);
  if DefaultFields then
    DestroyFields;
end;

procedure TTransactionDataset.InternalFirst;
begin
  FCurrentRecord := -1; //go to first record.
end;

procedure TTransactionDataset.InternalGotoBookmark(ABookmark: Pointer);
begin
  FCurrentRecord := PtrUint(ABookmark^);
end;

procedure TTransactionDataset.InternalLast;
begin
  FCurrentRecord := FTransactionList.Count - 1; //go to last record
end;

procedure TTransactionDataset.InternalInitFieldDefs;
// Fairly simple as we support just the properties TTransaction itself has
begin
  if not (Assigned(FTransactionList)) then
    DatabaseError(TDNoTransList);
  FieldDefs.Clear;

  // We would like to offer ftStrings but these are length-limited.
  // A solution could be to scan existing FTransactionList for the longest strings
  // and use those. That would however give arbitrary/inconsistent behaviour.
  // Instead, use ftMemo.
  //todo: investigate using ftWideMemo instead?
  FieldDefs.Add(HashIDFldName, ftString, 32); //read-only, currently md5 is used; modiffy if changed
  FieldDefs.Add(AccountFldName, ftMemo);
  FieldDefs.Add(BookDateFldName, ftDateTime);
  FieldDefs.Add(AmountFldName, ftCurrency);
  FieldDefs.Add(ContraAccountFldName, ftMemo);
  FieldDefs.Add(ContraNameFldName, ftMemo);
  FieldDefs.Add(TransTypeFldName, ftMemo);
  FieldDefs.Add(MemoFldName, ftMemo);
end;

procedure TTransactionDataset.InternalInitRecord(Buffer: TRecordBuffer);
begin
  FillChar(Buffer[0], FBufSize, 0);  //Clear buffer for use
end;

procedure TTransactionDataset.InternalOpen;
begin
  if not (Assigned(FTransactionList)) then
    DatabaseError(TDNoTransList);
  try
    InternalInitFieldDefs;
    if DefaultFields then
      CreateFields;
    BindFields(true);
    InternalRefresh;
  except
    raise; //pass on our problems
  end;
end;

procedure TTransactionDataset.InternalRefresh;
begin
  inherited InternalRefresh;
  // Not much to do really:
  FCurrentRecord := -1;
end;

procedure TTransactionDataset.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  FCurrentRecord := PRecInfo(Buffer)^.RecordNumber; //set current record to record number in buffer
end;

function TTransactionDataset.IsCursorOpen: boolean;
begin
  Result := Assigned(FTransactionList);
end;

procedure TTransactionDataset.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  PRecInfo(Buffer)^.RecordNumber := PtrUint(Data);
end;

procedure TTransactionDataset.SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
begin
  PRecInfo(Buffer)^.BookmarkFlag := Value;
end;

procedure TTransactionDataset.SetRecNo(Value: longint);
begin
  if (Value >= 0) and (Value <= FTransactionList.Count - 1) and (Value <> FCurrentRecord) then
  begin
    DoBeforeScroll;
    FCurrentRecord := Value;
    Resync([]); //refresh the buffers around the new position
    DoAfterScroll;
  end;
end;

function TTransactionDataset.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  result:=nil;
  // Only cover fields that have variable length here
  // Only copy over data if going to read or read/write - leave empty if writing
  case Field.FieldName of
    'Account':
      if FTransactionList[FCurrentRecord].Account <> '' then
      begin
        PRecInfo(ActiveBuffer)^.AccountBlob:=TMemoryStream.Create;
        result:=PRecInfo(ActiveBuffer)^.AccountBlob;
        if Mode<>bmWrite then
        begin
          result.Size:=Length(FTransactionList[FCurrentRecord].Account);
          result.Write(FTransactionList[FCurrentRecord].Account[1], result.Size);
        end;
      end;
    'ContraAccount':
      if FTransactionList[FCurrentRecord].ContraAccount <> '' then
      begin
        PRecInfo(ActiveBuffer)^.ContraAccountBlob:=TMemoryStream.Create;
        result:=PRecInfo(ActiveBuffer)^.ContraAccountBlob;
        if Mode<>bmWrite then
        begin
          result.Size:=Length(FTransactionList[FCurrentRecord].ContraAccount);
          result.Write(FTransactionList[FCurrentRecord].ContraAccount[1], result.Size);
        end;
      end;
    'ContraName':
      if FTransactionList[FCurrentRecord].ContraName <> '' then
      begin
        PRecInfo(ActiveBuffer)^.ContraNameBlob:=TMemoryStream.Create;
        result:=PRecInfo(ActiveBuffer)^.ContraNameBlob;
        if Mode<>bmWrite then
        begin
          result.Size:=Length(FTransactionList[FCurrentRecord].ContraName);
          result.Write(FTransactionList[FCurrentRecord].ContraName[1], result.Size);
        end;
      end;
    'TransType':
      if FTransactionList[FCurrentRecord].TransType <> '' then
      begin
        PRecInfo(ActiveBuffer)^.TransTypeBlob:=TMemoryStream.Create;
        result:=PRecInfo(ActiveBuffer)^.TransTypeBlob;
        if Mode<>bmWrite then
        begin
          result.Size:=Length(FTransactionList[FCurrentRecord].TransType);
          result.Write(FTransactionList[FCurrentRecord].TransType[1], result.Size);
        end;
      end;
    'Memo':
      if FTransactionList[FCurrentRecord].Memo <> '' then
      begin
        PRecInfo(ActiveBuffer)^.MemoBlob:=TMemoryStream.Create;
        result:=PRecInfo(ActiveBuffer)^.MemoBlob;
        if Mode<>bmWrite then
        begin
          result.Size:=Length(FTransactionList[FCurrentRecord].Memo);
          result.Write(FTransactionList[FCurrentRecord].Memo[1], result.Size);
        end;
      end
    else
    begin
      // E.g. when new fields haven't been added above...
      DatabaseErrorFmt(TDInvalidField, [Field.FieldName]);
    end;
  end;
  if assigned(result) then
    result.Position:=0; //rewind for reading
  // At least for reading: instead of copying
  // over the strings why not assign the memory directly to the memory stream??
end;

function TTransactionDataset.GetFieldData(Field: TField; Buffer: Pointer): boolean;
var
  DateTimeField: TDateTimeRec;
begin
  Result := true;
  // Only deals with NULL fields as well as data for non-variable length fields
  case Field.FieldName of
    HashIDFldName: StrPLCopy(Buffer, FTransactionList[FCurrentRecord].HashID, Field.Size);
    AccountFldName:
      if FTransactionList[FCurrentRecord].Account = '' then
        Result := false; //NULL
      // Leave handling of memo up to createblobstream
    BookDateFldName:
    begin
      DateTimeField.DateTime:=FTransactionList[FCurrentRecord].BookDate;
      PDateTimeRec(Buffer)^:=DateTimeField;
    end;
    AmountFldName: PCurrency(Buffer)^ := FTransactionList[FCurrentRecord].Amount;
    ContraAccountFldName:
      if FTransactionList[FCurrentRecord].ContraAccount = '' then
        Result := false; //NULL
      // Leave handling of memo up to createblobstream
    ContraNameFldName:
      if FTransactionList[FCurrentRecord].ContraName = '' then
        Result := false; //NULL
      // Leave handling of memo up to createblobstream
    TransTypeFldName:
      if FTransactionList[FCurrentRecord].TransType = '' then
        Result := false; //NULL
        // Leave handling of memo up to createblobstream
    MemoFldName:
      if FTransactionList[FCurrentRecord].Memo = '' then
        Result := false; //NULL
      // Leave handling of memo up to createblobstream
    else
    begin
      // E.g. when new fields haven't been added above...
      Result := false;
      DatabaseErrorFmt(TDInvalidField, [Field.FieldName]);
    end;
  end;
end;

procedure TTransactionDataset.SetFieldData(Field: TField; Buffer: Pointer);
begin
  case Field.FieldName of
    HashIDFldName: ; //Read only field, don't change things
    AccountFldName: ; //Do nothing; should be done in blob part
    BookDateFldName: FTransactionList[FCurrentRecord].BookDate := TDateTimeRec(Buffer^).DateTime;
    AmountFldName: FTransactionList[FCurrentRecord].Amount := Currency(Buffer^);
    ContraAccountFldName: ; //Do nothing; should be done in blob part
    ContraNameFldName: ; //Do nothing; should be done in blob part
    TransTypeFldName: ; //Do nothing; should be done in blob part
    MemoFldName: ; //Do nothing; should be done in blob part
    else
      // Implement support for this field
      DatabaseErrorFmt(TDInvalidField, [Field.FieldName]);
  end;
end;

constructor TTransactionDataset.Create(AOwner: TComponent);
begin
  inherited;
  FBufSize := SizeOf(TRecInfo); //FBufSize used for buffer memory allocation
  //could also be done in internalopen?
end;

destructor TTransactionDataset.Destroy;
begin
  inherited Destroy;
end;

end.


{
test code
FDataUnderlying.Add(TTransaction.Create(
  Now,
  '485860231',
  '495931242',
  'Slagerij van Kampen',
  -42.42,
  '',
  'Steak order'));


Memo1.Lines.Add('Dataset recordcount: '+inttostr(FData.RecordCount));
Memo1.Lines.Add('Underlying count: '+inttostr(FdataUnderlying.Count));
FData.First;
for i:=0 to FData.RecordCount-1 do
begin
  Memo1.Lines.Add('Record no '+inttostr(i+1));
  for Fields:=0 to FData.Fields.Count-1 do
  begin
    Memo1.Lines.Add('Field '+FData.Fields[Fields].FieldName+'=*'+FData.Fields[Fields].AsString+'*');
  end;
  Memo1.Lines.Add('Underlying: '+FDataUnderlying[i].Display);
  Memo1.Lines.Add('');
  FData.Next
end;
	}


