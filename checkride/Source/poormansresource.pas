unit poormansresource;

{$mode objfpc}{$H+}
// Alternative way to store data in an .exe file
// doesn't use resources, but just adds stuff behind exe proper
// Adapted from UPayload at http://www.delphidabbler.com/articles?article=7
// This is the base class; there's apparently also a class that implements
// stream-based access to the payload data
interface

type

  { TPayload }

  TPayload = class(TObject)
  private
    {the name of the executable file we are manipulating.}
    fFileName: string;
    {Preserves the current Pascal file mode}
    fOldFileMode: integer;
    {Pascal file descriptor that records the details of an open file.}
    fFile: file;
    {Open payload for read or write}
    procedure Open(Mode: integer);
    procedure Close;
  public
    {Creates payload object; if the Filename executable already has a payload, it reads it in.}
    constructor Create(const ExecutableName: string);
    {Whether exe has payload}
    function HasPayload: boolean;
    {Payload size in bytes}
    function PayloadSize: integer;
    {Writes payload to exe, overwrites any existing payload}
    procedure SetPayload(const Data; const DataSize: integer);
    {Saves file contents into payload, overwrites any existing payload}
    procedure FileIntoPayload(const FileName: string);
    {Retrieves payload from exe into buffer Data.
     Buffer must be big enough, see PayloadSize}
    procedure GetPayload(var Data);
    {Retrieves payload from exe, saves it to file.}
    procedure PayloadIntoFile(const FileName: string);
    {Removes payload from exe}
    procedure RemovePayload;
  end;

implementation

uses
  Classes, SysUtils;

type
  TPayloadFooter = packed record
    WaterMark: TGUID; //magic number that identifies there is a payload attached
    ExeSize: longint; //size of original executable before payload added
    DataSize: longint; //size of payload data (excluding footer)
  end;

const
  cWaterMarkGUID: TGUID =
    '{9FABA105-EDA8-45C3-89F4-369315A947EB}';
  cReadOnlyMode = 0;
  cReadWriteMode = 2;

procedure InitFooter(out Footer: TPayloadFooter);
begin
  FillChar(Footer, SizeOf(Footer), 0);
  Footer.WaterMark := cWaterMarkGUID;
end;

function ReadFooter(var F: file; out Footer: TPayloadFooter): boolean;
var
  FileLen: integer;
begin
  // Check that file is large enough for a footer!
  FileLen := FileSize(F);
  if FileLen > SizeOf(Footer) then
  begin
    // Big enough: move to start of footer and read it
    Seek(F, FileLen - SizeOf(Footer));
    BlockRead(F, Footer, SizeOf(Footer));
  end
  else
    // File not large enough for footer: zero it
    // .. this ensures watermark is invalid
    FillChar(Footer, SizeOf(Footer), 0);
  // Return if watermark is valid
  Result := IsEqualGUID(Footer.WaterMark, cWaterMarkGUID);
end;

procedure TPayload.Close;
begin
  // close file and restores previous file mode
  CloseFile(fFile);
  FileMode := fOldFileMode;
end;

constructor TPayload.Create(const ExecutableName: string);
begin
  inherited Create;
  fFileName := ExecutableName;
end;

procedure TPayload.GetPayload(var Data);
var
  Footer: TPayloadFooter;
begin
  // open file as read only
  Open(cReadOnlyMode);
  try
    // read footer
    if ReadFooter(fFile, Footer) and (Footer.DataSize > 0) then
    begin
      // move to end of exe code and read data
      Seek(fFile, Footer.ExeSize);
      BlockRead(fFile, Data, Footer.DataSize);
    end;
  finally
    // close file
    Close;
  end;
end;

procedure Tpayload.PayloadIntoFile(const Filename: string);
var
  Buffer: string;
begin
  // Fail silently if no payload
  if HasPayload then
  begin
    Setlength(Buffer, PayloadSize);
    GetPayload(Buffer[1]);
    //Get payload into buffer. Pass memory location, not pointer on stack
    if FileExists(FileName) then
      raise Exception.Create('Resource output file already exists.');
    with TFileStream.Create(FileName, fmCreate or fmOpenWrite or fmShareDenyWrite) do
    begin
      try
        Write(Pointer(Buffer)^, Length(Buffer));
      except
        Free;
        raise;
      end;
      Free;
    end;
  end;
end;

function TPayload.HasPayload: boolean;
begin
  // we have a payload if size is greater than 0
  Result := PayloadSize > 0;
end;

procedure TPayload.Open(Mode: integer);
begin
  // open file with given mode, recording current one
  fOldFileMode := FileMode;
  AssignFile(fFile, fFileName);
  FileMode := Mode;
  Reset(fFile, 1); //Open with record size 1
end;

function TPayload.PayloadSize: integer;
var
  Footer: TPayloadFooter;
begin
  // open file and assume no data
  Result := 0;
  Open(cReadOnlyMode);
  try
    // read footer and if valid return data size
    if ReadFooter(fFile, Footer) then
      Result := Footer.DataSize;
  finally
    Close;
  end;
end;

procedure TPayload.RemovePayload;
var
  PLSize: integer;
  FileLen: integer;
begin
  // get size of payload
  PLSize := PayloadSize;
  if PLSize > 0 then
  begin
    // we have payload: open file and get size
    Open(cReadWriteMode);
    FileLen := FileSize(fFile);
    try
      // seek to end of exec code and truncate file there
      Seek(fFile, FileLen - PLSize - SizeOf(TPayloadFooter));
      Truncate(fFile);
    finally
      Close;
    end;
  end;
end;

procedure TPayload.SetPayload(const Data; const DataSize: integer);
var
  Footer: TPayloadFooter;
begin
  // remove any existing payload
  RemovePayload;
  if DataSize > 0 then
  begin
    // we have some data: open file for writing
    Open(cReadWriteMode);
    try
      // create a new footer with required data
      InitFooter(Footer);
      Footer.ExeSize := FileSize(fFile);
      Footer.DataSize := DataSize;
      // write data and footer at end of exe code
      Seek(fFile, Footer.ExeSize);
      BlockWrite(fFile, Data, DataSize);
      BlockWrite(fFile, Footer, SizeOf(Footer));
    finally
      Close;
    end;
  end;
end;

procedure TPayload.FileIntoPayload(const FileName: string);
var
  Filesize: integer;
  Buffer: string;
begin
  if FileExists(FileName) = False then
  begin
    raise Exception.Create('File not found trying to write file to resource.');
  end;
  with TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite) do
  begin
    try
      FileSize := Size;
      SetLength(Buffer, FileSize);
      Read(Pointer(Buffer)^, Size);
      // Write/overwrite resource:
      SetPayload(Buffer[1], Length(Buffer));
    except
      Free;
      Buffer := '';  // Deallocates memory
      raise;
    end;
    Free;
  end;
end;

end.
