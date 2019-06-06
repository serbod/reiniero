unit flocatedb;


{*
Windows/Linux/Mac OSX FreePascal code to connect to an flocate database, perform updates and read results.
Currently only supports Firebird 2.x+ databases. Please see
flocate_database_schema.sql for the database definition.
todo: use flocate_write role instead of sysdba only

windows, Win, embedded, 3 runs, loaded system, debug build:
Default transaction isolation level
and
isc_tpb_consistency
and
isc_tpb_read_committed+isc_tpb_no_rec_version
all give about the same result
Of course, more interesting on client/server...
======================================================================================
MIT X11 License: no warranties, express or implied, but all other use permitted:
Copyright (c) 2010-2014 copyright holders

 Permission is hereby granted, free of charge, to any person
 obtaining a copy of this software and associated documentation
 files (the "Software"), to deal in the Software without
 restriction, including without limitation the rights to use,
 copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the
 Software is furnished to do so, subject to the following
 conditions:

 The above copyright notice and this permission notice shall be
 included in all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 OTHER DEALINGS IN THE SOFTWARE.
======================================================================================
*}
{$IFDEF FPC}{$mode objfpc}{$ENDIF}
{$INCLUDE flocatesettings.inc}
//Project-wide options.

interface

uses
  Classes, SysUtils, sqldb {queries, db access}, ibconnection {Firebird access}
  {$IFDEF Windows}
  , Windows {for resource constants}
  {$ENDIF}
  , fgl
  , flocatecommon;

type
  // The record is used for inserting new file information into the database.
  // Change this whenever the flocate database stored procedure signature changes:
  // Also has some metadata (IsValidUnixData) that determines what should get inserted
  TflocateInsertSPSignature = record
    //CATALOGID: Integer; => covered separately
    COMPUTERNAME: String;
    ROOT: String;
    DATEACCESSED: TDateTime;
    DATECREATED: TDateTime;
    DATEMODIFIED: TDateTime;
    EXECOMPANY: String;
    EXECOPYRIGHT: String;
    EXEDESCRIPTION: String;
    EXEFILEVERSION: String;
    EXEINTERNALNAME: String;
    EXEORIGINALFILENAME: String;
    EXEPRODUCTNAME: String;
    EXEPRODUCTVERSION: String;
    EXIFAPERTURE: String;
    EXIFARTIST: String;
    EXIFCOMPRESSEDBPP: String;
    EXIFCOPYRIGHT: String;
    EXIFDATETIME: TDateTime;
    EXIFDATETIMEDIGITIZED: TDateTime;
    EXIFDATETIMEORIGINAL: TDateTime;
    EXIFEXPOSURE: String;
    EXIFEXPOSUREPROGRAM: String;
    EXIFFLASH: String;
    EXIFFSTOPS: String;
    EXIFIMAGEDESCRIPTION: String;
    EXIFISO: integer;
    EXIFLIGHTSOURCE: String;
    EXIFMAKE: String;
    EXIFMAXAPERTURE: String;
    EXIFMETERINGMETHOD: String;
    EXIFMETERINGMODE: String;
    EXIFMODEL: String;
    EXIFORIENTATION: String;
    EXIFPIXELXDIMENSION: integer;
    EXIFPIXELYDIMENSION: integer;
    EXIFSHUTTERSPEED: String;
    EXIFSOFTWARE: String;
    EXIFUSERCOMMENTS: String;
    EXIFXRESOLUTION: integer;
    EXIFYRESOLUTION: integer;
    FILEDESCRIPTION: String;
    FILENAME: String;
    FILEPATH: String;
    FILESIZE: longint; //Firebird bigint signed 8 bytes
    FILETYPE: String;
    MD5VALID: Boolean; //indicates if hash below is valid. If not, use NULL
    MD5HASH: String;
    MP3ALBUM: String;
    MP3ARTIST: String;
    MP3COMMENT: String;
    MP3GENRE: String;
    MP3TITLE: String;
    MP3TRACK: integer;
    MP3YEAR: integer;
    UNIXVALIDDATA: boolean; //Consider unix* data as NULL or valid?
    UNIXUID: integer; //Firebird: signed integer, 4 bytes; FPC: cardinal unsigned 4 bytes
    UNIXGID: integer; //Firebird: signed integer, 4 bytes; FPC: cardinal unsigned 4 bytes
    UNIXMODE: integer; //Firebird: signed integer, 4 bytes; FPC: cardinal unsigned 4 bytes
  end;

  TIDValueList = specialize TFPGMap<string,integer>;

  { TFlocateDB }

  TFlocateDB = class
  private
    FCatalogIDCache: TIDValueList; //ID, catalog name
    FCatalogScans: TSQLQuery;
    //Connection to database
    FConnection: TIBConnection;
    FInsertQuery: TSQLQuery;
    FScanIDCache: TIDValueList; //ID, scan description cache
    FRTrans: TSQLTransaction; //read-only transaction
    FRWTrans: TSQLTransaction; //read/write transaction
    FReadQuery: TSQLQuery; //read-only query; can be opened and closed at will
    FTransCount: integer; //keeps track of write transactions; used for batch commit
    // Create a new db, including require tables/views
    procedure CreateDatabase;
    // This procedure will receive the events that are logged by the connection:
    // Will only writeln data if CRAZYDEBUG is defined
    procedure GetLogEvent(Sender: TSQLConnection; EventType: TDBEventType; Const Msg : String);
    // Set up database connection, components
    procedure Setup;
    // Free all components that were set up
    procedure FreeAll;
    // Sets up query for inserting search results
    procedure SetupInsertQuery;
  public
    constructor Create;
    destructor Destroy;
      override;
    // List of catalogs and scans; used in GUI applications
    property CatalogScans: TSQLQuery read FCatalogScans;
    // Clears all data
    procedure ClearData;
    // Clears existing scan data. Uses Default catalog if no description given
    procedure ClearExistingScan(CatalogDescription, ScanDescription: string);
    // Finds CatalogID for catalog with Description; returns success status.
    // If AddNew: adds catalog if not present in database
    // Uses caching to avoid database hits
    function FindCatalogID(Description: string; AddNew: boolean; var TheCatalogID: integer): boolean;
    // Find ScanID for scan in catalog CatalogID with Description (not necessarily root); returns success status.
    // If AddNew: adds scan if not present in database
    // Uses caching to avoid database hits.
    function FindScanID(TheCatalogID: integer; Description,Root: string; AddNew: boolean; var TheScanID: integer): boolean;
    // Get a query connected to the db.
    function GetQuery(ReadOnly: boolean): TSQLQuery;
    // Return files in indicated scan into ResultsQuery
    procedure GetResults(CatalogDescription,ScanDescription: string; ResultsQuery: TSQLQuery);
    // Saves a single directoryentry (=file etc) to database
    procedure SaveDirectoryEntry(CatalogDescription,Root,ScanDescription: string; InsertFields: TflocateInsertSPSignature);
    // Commits any unfinished transactions
    procedure SavePendingChanges;
  end;

implementation

{$R flocate_database_schema.rc} //the database rebuild script flocate_database_schema.sql as a resource.
uses DB {for parameter types},
  flocateshared {for ini etc};

const
  CommitBatch = 1000; //commit bulk operations every CommitBatch transactions
  DefaultCatalogDescription='Default catalog';
  DefaultScanDescription='Default scan';

procedure TFlocateDB.CreateDatabase;
// Creates database using existing FConnection.
// Uses embedded SQL to create database
var
  {$IFDEF CRAZYDEBUG}
  i:integer;
  {$ENDIF}
  ScriptText: TStringList;
  SQLResource: TResourceStream;
  SQLScript: TSQLScript;
  TranWasStarted: boolean=false;
begin
  try
    // First create database...
    FConnection.CreateDB;

    // ... then load in our SQL creation script.
    {$IF FPC_FULLVERSION<=20604}
    {$Warning Old FPC version has bugs in SQLScript; you may want to remove comments and autocommit etc}
    // hope this was fixed in 2.6.4; it's definitely in trunk
    if IsConsole then
      writeln('Warning: this program was compiled with an old version of FPC. Database creation may not work');
    {$ENDIF}
    SQLScript := TSQLScript.Create(nil);
    SQLResource := TResourceStream.Create(HInstance,     // your app or DLL instance handle
      'DBRECREATESQL',  // string containing resource name
      RT_RCDATA);    // identifies RCDATA resource type
    ScriptText:=TStringList.Create;
    try
      SQLScript.UseCommit:=false;
      SQLScript.UseSetTerm:=true; //we're using set term in our script
      SQLScript.CommentsInSQL:=false; //let's try hopefully this fixes things
      SQLScript.Database:=(FConnection as TIBConnection);
      SQLScript.Transaction:=FRWTrans;
      ScriptText.LoadFromStream(SQLResource);
      SQLScript.Script:=ScriptText;

      TranWasStarted:=FRWTrans.Active;
      FRWTrans.Commit;
      {$IFDEF CRAZYDEBUG}
      writeln(stderr, 'Debug: ', DateTimeToStr(Now),'SQL script:');
      for i:=0 to SQLScript.Script.Count-1 do
      begin
        writeln(i,SQLScript.Script[i]);
      end;
      {$ENDIF}
      try
        SQLScript.Execute;
      except
        on E: EDatabaseError do
        begin
          raise Exception.Create('DB error: '+E.Message+LineEnding+
            ' Script was: '+SQLScript.Script.Text);
        end;
        on F: Exception do
        begin
          raise; //pass on to handler
        end;
      end;

      FRWTrans.Commit;
      if TranWasStarted then
        FRWTrans.StartTransaction
    finally
      ScriptText.Free;
      SQLScript.Free;
      SQLResource.Free;
    end;
  except
    on E: Exception do
    begin
    {$IFDEF CRAZYDEBUG}
      Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
        ': Error creating database. Technical details: ',
        E.ClassName, '/', E.Message);
    {$ENDIF DEBUG}
    //Escalate exception
      raise Exception.Create('Could not create database. Technical details:'+E.Message);
    end;
  end;
end;

procedure TFlocateDB.GetLogEvent(Sender: TSQLConnection;
  EventType: TDBEventType; const Msg: String);
// Log event data from connection. Only prints if debugging
var
  Source: string;
begin
  case EventType of
    detCustom:   Source:='Custom:  ';
    detPrepare:  Source:='Prepare: ';
    detExecute:  Source:='Execute: ';
    detFetch:    Source:='Fetch:   ';
    detCommit:   Source:='Commit:  ';
    detRollBack: Source:='Rollback:';
    else Source:='Unknown event. Please fix program code.';
  end;
  {$IFDEF CRAZYDEBUG}
  writeln(stderr, 'Debug: ', DateTimeToStr(Now),Source+Msg);
  {$ENDIF}
end;

procedure TFlocateDB.Setup;

var
  Config: Tflocateshared;
begin
  FTransCount:=0;
  // We first use system-dependent global app config files:
  // This will avoid leaving ini files all over working dirs
  Config := TFlocateshared.Create;

  FConnection := TIBConnection.Create(nil);
  try
    {$IFDEF CRAZYDEBUG}
    Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
      ': SetUp: Connecting to database: ');
    FConnection.LogEvents:=LogAllEvents;
    FConnection.OnLog:=@GetLogEvent;
    {$ENDIF}
    FConnection.DatabaseName := Config.Database;
    FConnection.Username := Config.User;
    FConnection.Password := Config.Password;
    FConnection.Charset := 'UTF8';
    FConnection.Dialect := 3;
    FConnection.Params.Add('PAGE_SIZE=16384'); //Handy for db creation.

    FRTrans := TSQLTransaction.Create(nil);
    FRWTrans := TSQLTransaction.Create(nil);
    // Read/write transaction:
    FRWTrans.Params.Add('isc_tpb_read_committed'); //batch-oriented
    FRWTrans.Params.Add('isc_tpb_no_rec_version'); //less overhead than rec version
    //default transaction is read/write:
    FConnection.Transaction := FRWTrans;

    // Read only transaction:
    // http://tech.groups.yahoo.com/group/firebird-support/message/118748
    FRTrans.Database := FConnection;
    FRTrans.Params.Add('isc_tpb_read');
    FRTrans.Params.Add('isc_tpb_read_committed');
    FRTrans.Params.Add('isc_tpb_rec_version');
    FRTrans.Params.Add('isc_tpb_nowait');

    if Config.UseEmbedded = false then
    begin
      FConnection.HostName := Config.Host;
      // Only use port if not default
      if Config.Port <> 3050 then
        FConnection.Params.Add('port=' + IntToStr(Config.Port));
    end
    else
    begin
      FConnection.HostName := '';
      // We're using embedded
      //If the database file doesn't exist, let's create it
      if (FileExists(Config.Database) = false) then
      begin
        {$IFDEF CRAZYDEBUG} //Won't work in GUI applications
        Writeln(stdErr, 'Debug: '+DateTimeToStr(Now),
          ': SetUp: embedded database ', Config.Database, ' does not exist. Creating it.');
        {$ENDIF}
        CreateDatabase;
      end;
    end;

    try
      // Explicitly open here to manage exceptions
      FConnection.Open;
    except
      on E: Exception do
      begin
        {$IFDEF CRAZYDEBUG}
        Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
          ': Error connecting to database. Technical details: ',
          E.ClassName, '/', E.Message);
        {$ENDIF DEBUG}
        raise Exception.Create('Could not connect to the scan results database.');
        // Escalate exception; we should gracefully kill the program
      end;
    end;
  finally
    Config.Free;
  end;

  // See whether our connection attempt worked.
  if FConnection.Connected = false then
  begin
    raise Exception.Create('Connection to database failed.');
  end
  else
  begin
{$IFDEF CRAZYDEBUG}
    Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
      ': DB/Setup/Going to start transaction.');
{$ENDIF}
    FRWTrans.StartTransaction;
    //Let's start it up.
    FInsertQuery := GetQuery(false);
    FInsertQuery.Database := FConnection;
{$IFDEF CRAZYDEBUG}
    Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
      ': Getting query FInsertQuery');
{$ENDIF}
    SetupInsertQuery;
    FReadQuery := GetQuery(true);
    FReadQuery.Database := FConnection;
    {$IFDEF CRAZYDEBUG}
    Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
      ': Getting query FReadQuery');
    {$ENDIF}
  end;

  // Set up some queries needed for GUIs
  FCatalogScans:=GetQuery(false);
  FCatalogScans.SQL.Text:='SELECT * FROM CATALOGSCANS; ';
end;

function TFlocateDB.GetQuery(ReadOnly: boolean): TSQLQuery;

var
  AQuery: TSQLQuery;
begin
  AQuery := TSQLQuery.Create(nil);
  //Use existing connection & transaction
  AQuery.Database := FConnection;
  if ReadOnly then
    AQuery.Transaction := FRTrans
  else
    AQuery.Transaction := FRWTrans;
  Result := AQuery;
end;

procedure TFlocateDB.GetResults(CatalogDescription,ScanDescription: string; ResultsQuery: TSQLQuery);
var
  CatalogID: integer;
  Counter: longword;
  ReadQuery: TSQLQuery;
  ScanID: integer;
  SQL: string;
begin
  // Belts and braces: commit any unfinished insert/edit operations:
  SavePendingChanges;
  ReadQuery := GetQuery(true);
  SQL := 'SELECT CATALOGID FROM TBLCATALOGS WHERE DESCRIPTION=:CATALOGDESCRIPTION;';
  try
		ReadQuery.SQL.Text := SQL;
		ReadQuery.Params.ParamByName('CATALOGDESCRIPTION').AsString:=CatalogDescription;
    ReadQuery.Open;
    // scalar query
    if not(ReadQuery.EOF) then
      CatalogID:=ReadQuery.Fields[0].AsInteger;
    ReadQuery.Close;
  finally
    ReadQuery.Free;
  end;

  ReadQuery := GetQuery(true);
  SQL := 'SELECT SCANID FROM TBLSCANS WHERE DESCRIPTION=:SCANDESCRIPTION AND CATALOGID=:CATALOGID;';
  try
		ReadQuery.SQL.Text := SQL;
		ReadQuery.Params.ParamByName('SCANDESCRIPTION').AsString:=ScanDescription;
    ReadQuery.Params.ParamByName('CATALOGID').AsInteger:=CatalogID;
    ReadQuery.Open;
    // scalar query
    if not(ReadQuery.EOF) then
      ScanID:=ReadQuery.Fields[0].AsInteger;
    ReadQuery.Close;
  finally
    ReadQuery.Free;
  end;

  // Set up resultsquery
  ResultsQuery.Close;
  ResultsQuery.Database := FConnection;
  ResultsQuery.Transaction := FRTrans;
  SQL := 'SELECT * FROM FILESALLSCANS WHERE CATALOGID=:CATALOGIDPARAM AND SCANID=:SCANIDPARAM;';
  ResultsQuery.SQL.Text := SQL;
  ResultsQuery.Params.ParamByName('CATALOGIDPARAM').AsInteger:=CatalogID;
  ResultsQuery.Params.ParamByName('SCANIDPARAM').AsInteger:=ScanID;
  ResultsQuery.Open;
  ResultsQuery.Last;
  ResultsQuery.First;
  {$IFDEF CRAZYDEBUG}
  writeln(stderr, 'Debug: ', DateTimeToStr(Now),
    ': Opened result query.');
  {$ENDIF}
end;

function TFlocateDB.FindScanID(TheCatalogID: integer; Description, Root: string;
  AddNew: boolean; var TheScanID: integer): boolean;
var
  ActionQuery: TSQLQuery;
  TheIndex: integer;
begin
  // First try to find in cache:
  if FScanIDCache.Find(inttostr(TheCatalogID)+Description,TheIndex) then
  begin
    TheScanID:=FScanIDCache.Data[TheIndex];
    exit(true);
  end;

  // fail by default
  TheScanID:=0;
  result:=false;

  FReadQuery.SQL.Text:='SELECT SCANID, ROOT FROM TBLSCANS WHERE DESCRIPTION=:DESCRIPTIONPAR AND CATALOGID='+inttostr(TheCatalogID);
  FReadQuery.Params.ParamByName('DESCRIPTIONPAR').DataType:=ftString;
  FReadQuery.Params.ParamByName('DESCRIPTIONPAR').AsString:=AnsiToUTF8(Description);
  FReadQuery.Open;
  try
    if not(FReadQuery.EOF) then
    begin
      TheScanID:=FReadQuery.FieldByName('SCANID').AsInteger;
      FScanIDCache[inttostr(TheCatalogID)+Description]:=TheScanID; //add to cache
      // If root differs in db version, update it
      if FReadQuery.FieldByName('ROOT').AsString<>Root then
      begin
        ActionQuery:=GetQuery(false);
        try
          FRWTrans.Commit;
          FRWTrans.StartTransaction;
          ActionQuery.SQL.Text:='update tblscans set root=:root where scanid=:scanid';
          ActionQuery.Params.ParamByName('root').AsString:=Root;
          ActionQuery.Params.ParamByName('scanid').AsInteger:=TheScanID;
          ActionQuery.ExecSQL;
          FRWTrans.Commit;
          FRWTrans.StartTransaction;
        finally
          ActionQuery.Free;
        end;
      end;
      result:=true;
    end
    else
    begin
      // If desired, add new scan with description
      if AddNew then
      begin
        ActionQuery:=GetQuery(false);
        try
          FRWTrans.Commit;
          FRWTrans.StartTransaction;
          ActionQuery.SQL.Text:='INSERT INTO TBLSCANS (CATALOGID,DESCRIPTION,ROOT) VALUES (:CATPARAM,:DESCPARAM,:ROOTPARAM) '+
            'RETURNING SCANID ';
          if Description='' then
            ActionQuery.Params.ParamByName('DESCPARAM').AsString:=AnsiToUtf8(DefaultScanDescription)
          else
            ActionQuery.Params.ParamByName('DESCPARAM').AsString:=AnsiToUtf8(Description);
          ActionQuery.ParamByName('CATPARAM').AsInteger:=TheCatalogID;
          ActionQuery.ParamByName('ROOTPARAM').AsString:=Root;
          ActionQuery.Open;
          if not(ActionQuery.EOF) then
          begin
            TheScanID:=ActionQuery.FieldByName('SCANID').AsInteger;
            FScanIDCache[inttostr(TheCatalogID)+Description]:=TheScanID; //add to cache
            result:=true;
          end;
          FRWTrans.Commit;
          FRWTrans.StartTransaction;
          ActionQuery.Close;
        finally
          ActionQuery.Free;
        end;
      end;
    end;
  finally
    FReadQuery.Close;
  end;
end;

procedure TFlocateDB.FreeAll;
// Close and free all opened resources before object is destroyed
// Note: we might be running this procedure when a connect error happened
// or another exception occurred. In this case, we cannot assume
// all objects are assigned, so just swallow any exceptions that
// we can't avoid.
begin
{$IFDEF CRAZYDEBUG}
  Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
    ': FreeAll: Disconnecting from database.');
{$ENDIF}
  try
    if Assigned(FInsertQuery) then
      FInsertQuery.Close;
  except
    {$IFDEF CRAZYDEBUG}
    Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
      ': FreeAll: error closing FInsertQuery. Ignoring.');
    {$ENDIF}
  end;

  try
    FreeAndNil(FinsertQuery);//Let's use freeandnil in class level object instead of free
  except
{$IFDEF CRAZYDEBUG}
    Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
      ': FreeAll: error freeing FInsertQuery. Ignoring.');
{$ENDIF}
  end;

  try
    FreeAndNil(FCatalogScans);//Let's use freeandnil in class level object instead of free
  except
{$IFDEF CRAZYDEBUG}
    Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
      ': FreeAll: error freeing FCatalogScans. Ignoring.');
{$ENDIF}
  end;

  try
    if Assigned(FReadQuery) then
      FReadQuery.Close;
  except
    {$IFDEF CRAZYDEBUG}
    Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
      ': FreeAll: error closing FReadQuery. Ignoring.');
    {$ENDIF}
  end;

  try
    FreeAndNil(FReadQuery);//Let's use freeandnil in class level object instead of free
  except
{$IFDEF CRAZYDEBUG}
    Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
      ': FreeAll: error freeing FReadQuery. Ignoring.');
{$ENDIF}
  end;

  try
    if assigned(FRWTrans) then
      FRWTrans.Active := false;
  except
    {$IFDEF CRAZYDEBUG}
    Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
      ': FreeAll: error deactivating FRWTransaction. Ignoring.');
    {$ENDIF}
  end;

  try
    FreeAndNil(FRWTrans); //Let's use freeandnil in class level object instead of free
    //Free up transaction object
  except
    // We swallow the exception.
{$IFDEF CRAZYDEBUG}
    Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
      ': FreeAll: error freeing FWTransaction. Ignoring.');
{$ENDIF}
  end;

  try
    FreeAndNil(FRTrans); //Let's use freeandnil in class level object instead of free
    //Free up transaction object
  except
    // We swallow the exception.
{$IFDEF CRAZYDEBUG}
    Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
      ': FreeAll: error freeing FRTransaction. Ignoring.');
{$ENDIF}
  end;


  try
    if assigned(FConnection) then
    begin
      //FConnection.Connected := False;
      { Ignore this, .Close calls Connect := False internally
      If we left it in, we get an error running the .Close command below. }
      FConnection.Close;
      //Close the connection if it was opened
    end;
  except
    // We swallow the exception.
{$IFDEF CRAZYDEBUG}
    Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
      ': FreeAll: error closing FConnection. Ignoring.');
{$ENDIF}
  end;

  try
    FreeAndNil(FConnection); //Let's use freeandnil in class level object instead of free
  except
    // We swallow the exception.
{$IFDEF CRAZYDEBUG}
    Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
      ': FreeAll: error freeing FConnection. Ignoring.');
{$ENDIF}
  end;
end;

procedure TFlocateDB.SetupInsertQuery;
var
  ParamCounter: integer;
begin
  FInsertQuery.SQL.Clear;
  FInsertQuery.SQL.Add(
    'EXECUTE PROCEDURE SPADDALLFILEDETAILS (:CATALOGID, :COMPUTERNAME, :DATEACCESSED,'
      +' :DATECREATED, ' +
    ':DATEMODIFIED, :EXECOMPANY, :EXECOPYRIGHT, :EXEDESCRIPTION, :'
      +'EXEFILEVERSION, ' +
    ':EXEINTERNALNAME, :EXEORIGINALFILENAME, :EXEPRODUCTNAME, :'
      +'EXEPRODUCTVERSION, ' +
    ':EXIFAPERTURE, :EXIFARTIST, :EXIFCOMPRESSEDBPP, :EXIFCOPYRIGHT, :'
      +'EXIFDATETIME, ' +
    ':EXIFDATETIMEDIGITIZED, :EXIFDATETIMEORIGINAL, :EXIFEXPOSURE, :'
      +'EXIFEXPOSUREPROGRAM, ' +
    ':EXIFFLASH, :EXIFFSTOPS, :EXIFIMAGEDESCRIPTION, :EXIFISO, :'+
    'EXIFLIGHTSOURCE, ' +
    ':EXIFMAKE, :EXIFMAXAPERTURE, :EXIFMETERINGMETHOD, :EXIFMETERINGMODE, ' +
    ':EXIFMODEL, :EXIFORIENTATION, :EXIFPIXELXDIMENSION, :EXIFPIXELYDIMENSION, ' +
    ':EXIFSHUTTERSPEED, :EXIFSOFTWARE, :EXIFUSERCOMMENTS, :EXIFXRESOLUTION, ' +
    ':EXIFYRESOLUTION, :FILEDESCRIPTION, :FILENAME, :FILEPATH, :FILESIZE, ' +
    ':FILETYPE, :MD5HASH, :MP3ALBUM, :MP3ARTIST, :MP3COMMENT, :MP3GENRE, ' +
    ':MP3TITLE, :MP3TRACK, :MP3YEAR, :UNIXUID, :UNIXGID, :UNIXMODE, :SCANID )');
  // Let's set all parameters as unicode strings, input and deal with the exceptions below:
  for ParamCounter := 0 to FInsertQuery.Params.Count - 1 do
  begin
    FInsertQuery.Params[ParamCounter].DataType := ftString;
    FInsertQuery.Params[ParamCounter].ParamType := ptInput;
  end;
  FInsertQuery.Params.ParamByName('DATEACCESSED').DataType := ftDate;
  //Firebird TIMESTAMP
  FInsertQuery.Params.ParamByName('DATECREATED').DataType := ftDate;
  // Firebird TIMESTAMP
  FInsertQuery.Params.ParamByName('DATEMODIFIED').DataType := ftDate;
  // Firebird TIMESTAMP
  FInsertQuery.Params.ParamByName('EXIFDATETIME').DataType := ftDate;
  // Firebird TIMESTAMP
  FInsertQuery.Params.ParamByName('EXIFDATETIMEDIGITIZED').DataType := ftDate;
  // Firebird TIMESTAMP
  FInsertQuery.Params.ParamByName('EXIFISO').DataType := ftInteger;
  // Firebird Integer
  FInsertQuery.Params.ParamByName('EXIFPIXELXDIMENSION').DataType := ftInteger;
  // Firebird Integer
  FInsertQuery.Params.ParamByName('EXIFPIXELYDIMENSION').DataType := ftInteger;
  // Firebird Integer
  FInsertQuery.Params.ParamByName('EXIFXRESOLUTION').DataType := ftInteger;
  // Firebird Integer
  FInsertQuery.Params.ParamByName('EXIFYRESOLUTION').DataType := ftInteger;
  // Firebird Integer
  FInsertQuery.Params.ParamByName('MP3YEAR').DataType := ftInteger;
  // Firebird Integer
  FInsertQuery.Params.ParamByName('FILESIZE').DataType := ftLargeInt;
  FInsertQuery.Params.ParamByName('UNIXUID').DataType := ftInteger;
  FInsertQuery.Params.ParamByName('UNIXGID').DataType := ftInteger;
  FInsertQuery.Params.ParamByName('UNIXMODE').DataType := ftInteger;
  // Firebird Integer
  FInsertQuery.Params.ParamByName('MD5HASH').DataType := ftString;
  // Firebird CHAR32; ASCII only
  FInsertQuery.Params.ParamByName('MD5HASH').Size := 32;
  // see whether that helps
  FInsertQuery.Params.ParamByName('MP3TRACK').DataType := ftSmallint;
  // Firebird Smallint
  FInsertQuery.ParseSQL := false;

  // don't try to automatically generate update/delete statements based on this query. Could help with access violations?
  FinsertQuery.UsePrimaryKeyAsKey := false;
  // Prepare statement at DB end:
  FInsertQuery.Prepare;
end;

constructor TFlocateDB.Create;
begin
  inherited Create;
  FCatalogIDCache:=TIDValueList.Create;
  FScanIDCache:=TIDValueList.Create;
  {$IFDEF CRAZYDEBUG}
  Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
    ': Starting database setup: ');
  {$ENDIF}
  SetUp;
  //Set up connection, transaction, query.
end;

destructor TFlocateDB.Destroy;
begin
  FCatalogIDCache.Free;
  FScanIDCache.Free;
  // Commit any pending data
  if Assigned(FRWTrans) and (FRWTrans.Active) then
  begin
    {$IFDEF CRAZYDEBUG}
    Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
      ': DB/Destroy/Going to commit active transaction.');
    {$ENDIF}
    FRWTrans.Commit;
  end;
  // Then free all db objects (transaction, connection, etc):
  FreeAll;
  inherited Destroy;
end;

procedure TFlocateDB.ClearData;
// Clear out ALL existing data from database.
// This includes all filenames etc. Useful for testing.
var
  DeleteQuery: TSQLQuery;
begin
  // Make sure cache is cleared:
  FCatalogIDCache.Clear;
  FScanIDCache.Clear;
  DeleteQuery := GetQuery(false);
  try
    DeleteQuery.SQL.Text := 'EXECUTE PROCEDURE CLEARALLDATA;';
    {$IFDEF CRAZYDEBUG}
    Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
      ': ClearData: going to run query ', DeleteQuery.SQL.Text);
    {$ENDIF CRAZYDEBUG}
    DeleteQuery.ExecSQL;
    // Ensure db clears up old transaction data
    // Use this instead of CommitRetaining to give garbage collection a chance
    FRWTrans.Commit;
    FRWTrans.StartTransaction;
    FTransCount:=0;
  finally
    DeleteQuery.Close;
    DeleteQuery.Free;
  end;
end;

procedure TFlocateDB.ClearExistingScan(CatalogDescription, ScanDescription: string);
// Clear out any existing scan data (well, maybe not file names but who cares)
// from database.
// Only for given catalog (or the default catalog is empty string)/scan description (or Default scan
var
  CatalogID: integer=0;
  DeleteQuery: TSQLQuery;
begin
  DeleteQuery := GetQuery(false);
  try
    // If invalid catalog/scan descriptions are provided, an exception will be
    // raised, which seems better than silently creating new empty
    // scans/catalogs
    if (CatalogDescription='') then
    begin
      if (not(FindCatalogID(DefaultCatalogDescription,false,CatalogID))) then
        exit;
    end
    else
    begin
      if (not(FindCatalogID(CatalogDescription,false,CatalogID))) then
        exit;
    end;

    // Leave FCustomCatalogDescription and FCustomScanDescription alone;
    // otherwise insert code thinks the insert query has been updated to the
    // proper values.

    DeleteQuery.SQL.Text := 'DELETE FROM TBLSCANS WHERE TBLSCANS.DESCRIPTION = :DESCRIPTPARAM '+
      'AND CATALOGID='+inttostr(CatalogID);
    DeleteQuery.Params.ParamByName('DESCRIPTPARAM').DataType:=ftString;

    if ScanDescription='' then
    begin
      DeleteQuery.Params.ParamByName('DESCRIPTPARAM').AsString:=AnsiToUTF8(DefaultScanDescription);
    end
    else
    begin
      DeleteQuery.Params.ParamByName('DESCRIPTPARAM').AsString:=AnsiToUTF8(ScanDescription);
    end;
    {$IFDEF CRAZYDEBUG}
    Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
      ': ClearExistingScan: going to run query ', DeleteQuery.SQL.Text, 'for catalog '+CatalogDescription);
    {$ENDIF CRAZYDEBUG}
    DeleteQuery.ExecSQL;
    // Ensure db clears up old transaction data
    // Use this instead of CommitRetaining to give garbage collection a chance
    FRWTrans.Commit;
    FRWTrans.StartTransaction;
    FTransCount:=0;
  finally
    DeleteQuery.Close;
    DeleteQuery.Free;
  end;
  //try  end;
end;

function TFlocateDB.FindCatalogID(Description: string; AddNew: boolean; var TheCatalogID: integer
  ): boolean;
var
  TheIndex: integer;
  NewCat: TSQLQuery;
begin
  // First try to find in cache:
  if FCatalogIDCache.Find(Description,TheIndex) then
  begin
    TheCatalogID:=FCatalogIDCache.Data[TheIndex];
    exit(true);
  end;

  //Default to failure
  TheCatalogID:=0;
  result:=false;

  FReadQuery.SQL.Text:='SELECT CATALOGID FROM TBLCATALOGS WHERE DESCRIPTION=:DESCRIPTIONPAR';
  FReadQuery.Params.ParamByName('DESCRIPTIONPAR').DataType:=ftString;
  FReadQuery.Params.ParamByName('DESCRIPTIONPAR').AsString:=AnsiToUTF8(Description);
  FReadQuery.Open;
  try
    if not(FReadQuery.EOF) then
    begin
      TheCatalogID:=FReadQuery.FieldByName('CATALOGID').AsInteger;
      FCatalogIDCache[Description]:=TheCatalogID; //add to cache
      result:=true;
    end
    else
    begin
      // If desired, add catalog with description
      if AddNew then
      begin
        NewCat:=GetQuery(false);
        try
          NewCat.SQL.Text:='INSERT INTO TBLCATALOGS (DESCRIPTION) VALUES (:DESCPARAM) '+
            'RETURNING CATALOGID ';
          if Description='' then
            NewCat.Params.ParamByName('DESCPARAM').AsString:=AnsiToUtf8(DefaultCatalogDescription)
          else
            NewCat.Params.ParamByName('DESCPARAM').AsString:=AnsiToUtf8(Description);
          NewCat.Open;
          if not(NewCat.EOF) then
          begin
            TheCatalogID:=NewCat.FieldByName('CATALOGID').AsInteger;
            FCatalogIDCache[Description]:=TheCatalogID; //add to cache
            result:=true;
          end;
          NewCat.Close;
        finally
          NewCat.Free;
        end;
      end;
    end;
  finally
    FReadQuery.Close;
  end;
end;

procedure TFlocateDB.SaveDirectoryEntry(CatalogDescription,Root,ScanDescription: string; InsertFields: TflocateInsertSPSignature);
// This procedure will insert a new record into the database.

// Note we're converting system encoding to utf 8 here so we can keep the database
// in UTF8 and support all kinds of clients.

// Note: we're passing old dates and empty strings to the database. The db
// stored procedures will convert these to NULL when appropriate. Might be
// more elegant to already pass null here.
var
  {$IFDEF CRAZYDEBUG}
  DebugCounter: integer=0;
  {$ENDIF}
  CatalogID: integer;
  FieldCounter: integer;
  ScanID: integer;
begin
  assert(Root<>'','DB SaveDirectoryEntry: Root may not be empty');
  // Need catalog id before scan id
  if not(FindCatalogID(CatalogDescription,true,CatalogID)) then
    raise Exception.Create('Could not get valid catalog ID for description '+CatalogDescription);
  if not(FindScanID(CatalogID,ScanDescription,Root,true,ScanID)) then
    raise Exception.Create('Could not get valid scan ID for description '+ScanDescription);

  try
    // Sets all parameters to null. Data types were set up in advance.
    for FieldCounter := 0 to FInsertQuery.Params.Count - 1 do
    begin
      FInsertQuery.Params[FieldCounter].Clear;
    end;
    assert(Assigned(FInsertQuery) = true,
      'The insert query object must exist for save to work.');
    assert(FInsertQuery.Transaction.Active = true,
      'The insert query must have an active transaction for save to work.');

    FInsertQuery.Params.ParamByName('CATALOGID').AsInteger :=
      CatalogID;
    FInsertQuery.Params.ParamByName('COMPUTERNAME').AsString :=
      AnsiToUTF8(InsertFields.COMPUTERNAME);
    FInsertQuery.Params.ParamByName('DATEACCESSED').AsDateTime :=
      InsertFields.DATEACCESSED;
    FInsertQuery.Params.ParamByName('DATECREATED').AsDateTime :=
      InsertFields.DATECREATED;
    FInsertQuery.Params.ParamByName('DATEMODIFIED').AsDateTime :=
      InsertFields.DATEMODIFIED;
    FInsertQuery.Params.ParamByName('EXECOMPANY').AsString := InsertFields.EXECOMPANY;
    FInsertQuery.Params.ParamByName('EXECOPYRIGHT').AsString :=
      AnsiToUTF8(InsertFields.EXECOPYRIGHT);
    FInsertQuery.Params.ParamByName('EXEDESCRIPTION').AsString :=
      AnsiToUTF8(InsertFields.EXEDESCRIPTION);
    FInsertQuery.Params.ParamByName('EXEFILEVERSION').AsString :=
      AnsiToUTF8(InsertFields.EXEFILEVERSION);
    FInsertQuery.Params.ParamByName('EXEINTERNALNAME').AsString :=
      AnsiToUTF8(InsertFields.EXEINTERNALNAME);
    FInsertQuery.Params.ParamByName('EXEORIGINALFILENAME').AsString :=
      AnsiToUTF8(InsertFields.EXEORIGINALFILENAME);
    FInsertQuery.Params.ParamByName('EXEPRODUCTNAME').AsString :=
      AnsiToUTF8(InsertFields.EXEPRODUCTNAME);
    FInsertQuery.Params.ParamByName('EXEPRODUCTVERSION').AsString :=
      AnsiToUTF8(InsertFields.EXEPRODUCTVERSION);
    FInsertQuery.Params.ParamByName('EXIFAPERTURE').AsString :=
      AnsiToUTF8(InsertFields.EXIFAPERTURE);
    FInsertQuery.Params.ParamByName('EXIFARTIST').AsString :=
      AnsiToUTF8(InsertFields.EXIFARTIST);
    FInsertQuery.Params.ParamByName('EXIFCOMPRESSEDBPP').AsString :=
      AnsiToUTF8(InsertFields.EXIFCOMPRESSEDBPP);
    FInsertQuery.Params.ParamByName('EXIFCOPYRIGHT').AsString :=
      AnsiToUTF8(InsertFields.EXIFCOPYRIGHT);
    FInsertQuery.Params.ParamByName('EXIFDATETIME').AsDateTime :=
      InsertFields.EXIFDATETIME;
    FInsertQuery.Params.ParamByName('EXIFDATETIMEDIGITIZED').AsDateTime :=
      InsertFields.EXIFDATETIMEDIGITIZED;
    FInsertQuery.Params.ParamByName('EXIFDATETIMEORIGINAL').AsDateTime :=
      InsertFields.EXIFDATETIMEORIGINAL;
    FInsertQuery.Params.ParamByName('EXIFEXPOSURE').AsString :=
      AnsiToUTF8(InsertFields.EXIFEXPOSURE);
    FInsertQuery.Params.ParamByName('EXIFEXPOSUREPROGRAM').AsString :=
      AnsiToUTF8(InsertFields.EXIFEXPOSUREPROGRAM);
    FInsertQuery.Params.ParamByName('EXIFFLASH').AsString :=
      AnsiToUTF8(InsertFields.EXIFFLASH);
    FInsertQuery.Params.ParamByName('EXIFFSTOPS').AsString :=
      AnsiToUTF8(InsertFields.EXIFFSTOPS);
    FInsertQuery.Params.ParamByName('EXIFIMAGEDESCRIPTION').AsString :=
      AnsiToUTF8(InsertFields.EXIFIMAGEDESCRIPTION);
    FInsertQuery.Params.ParamByName('EXIFISO').AsInteger := InsertFields.EXIFISO;
    FInsertQuery.Params.ParamByName('EXIFLIGHTSOURCE').AsString :=
      AnsiToUTF8(InsertFields.EXIFLIGHTSOURCE);
    FInsertQuery.Params.ParamByName('EXIFMAKE').AsString :=
      AnsiToUTF8(InsertFields.EXIFMAKE);
    FInsertQuery.Params.ParamByName('EXIFMAXAPERTURE').AsString :=
      AnsiToUTF8(InsertFields.EXIFMAXAPERTURE);
    FInsertQuery.Params.ParamByName('EXIFMETERINGMETHOD').AsString :=
      AnsiToUTF8(InsertFields.EXIFMETERINGMETHOD);
    FInsertQuery.Params.ParamByName('EXIFMETERINGMODE').AsString :=
      AnsiToUTF8(InsertFields.EXIFMETERINGMODE);
    FInsertQuery.Params.ParamByName('EXIFMODEL').AsString :=
      AnsiToUTF8(InsertFields.EXIFMODEL);
    FInsertQuery.Params.ParamByName('EXIFORIENTATION').AsString :=
      AnsiToUTF8(InsertFields.EXIFORIENTATION);
    FInsertQuery.Params.ParamByName('EXIFPIXELXDIMENSION').AsInteger :=
      InsertFields.EXIFPIXELXDIMENSION;
    FInsertQuery.Params.ParamByName('EXIFPIXELYDIMENSION').AsInteger :=
      InsertFields.EXIFPIXELYDIMENSION;
    FInsertQuery.Params.ParamByName('EXIFSHUTTERSPEED').AsString :=
      AnsiToUTF8(InsertFields.EXIFSHUTTERSPEED);
    FInsertQuery.Params.ParamByName('EXIFSOFTWARE').AsString :=
      AnsiToUTF8(InsertFields.EXIFSOFTWARE);
    FInsertQuery.Params.ParamByName('EXIFUSERCOMMENTS').AsString :=
      AnsiToUTF8(InsertFields.EXIFUSERCOMMENTS);
    FInsertQuery.Params.ParamByName('EXIFXRESOLUTION').AsInteger :=
      InsertFields.EXIFXRESOLUTION;
    FInsertQuery.Params.ParamByName('EXIFYRESOLUTION').AsInteger :=
      InsertFields.EXIFYRESOLUTION;
    FInsertQuery.Params.ParamByName('FILEDESCRIPTION').AsString :=
      AnsiToUTF8(InsertFields.FILEDESCRIPTION);
    FInsertQuery.Params.ParamByName('FILENAME').AsString :=
      AnsiToUTF8(InsertFields.FILENAME);
    FInsertQuery.Params.ParamByName('FILEPATH').AsString :=
      AnsiToUTF8(InsertFields.FILEPATH);
    FInsertQuery.Params.ParamByName('FILESIZE').AsLargeInt := InsertFields.FILESIZE;
    FInsertQuery.Params.ParamByName('FILETYPE').AsString :=
      AnsiToUTF8(InsertFields.FILETYPE);
    if InsertFields.MD5VALID then
      FInsertQuery.Params.ParamByName('MD5HASH').AsString :=
        AnsiToUTF8(InsertFields.MD5HASH)
    else
      FInsertQuery.Params.ParamByName('MD5HASH').Clear; //NULL

    FInsertQuery.Params.ParamByName('MP3ALBUM').AsString :=
      AnsiToUTF8(InsertFields.MP3ALBUM);
    FInsertQuery.Params.ParamByName('MP3ARTIST').AsString :=
      AnsiToUTF8(InsertFields.MP3ARTIST);
    FInsertQuery.Params.ParamByName('MP3COMMENT').AsString :=
      AnsiToUTF8(InsertFields.MP3COMMENT);
    FInsertQuery.Params.ParamByName('MP3GENRE').AsString :=
      AnsiToUTF8(InsertFields.MP3GENRE);
    FInsertQuery.Params.ParamByName('MP3TITLE').AsString :=
      AnsiToUTF8(InsertFields.MP3TITLE);
    FInsertQuery.Params.ParamByName('MP3TRACK').AsInteger := InsertFields.MP3TRACK;
    FInsertQuery.Params.ParamByName('MP3YEAR').AsInteger := InsertFields.MP3YEAR;
    if InsertFields.UNIXVALIDDATA then
    begin
      // Insert only meaningful data, leave rest null
      FInsertQuery.Params.ParamByName('UNIXUID').AsInteger := InsertFields.UNIXUID;
      FInsertQuery.Params.ParamByName('UNIXGID').AsInteger := InsertFields.UNIXGID;
      FInsertQuery.Params.ParamByName('UNIXMODE').AsInteger := InsertFields.UNIXMODE;
    end
    else
    begin
      FInsertQuery.Params.ParamByName('UNIXUID').Clear; //null
      FInsertQuery.Params.ParamByName('UNIXGID').Clear; //null
      FInsertQuery.Params.ParamByName('UNIXMODE').Clear; //null
    end;
    FInsertQuery.Params.ParamByName('SCANID').AsInteger := ScanID;

    try
      FInsertQuery.ExecSQL;
      FTransCount:=FTransCount+1;
      // Batch commit so db can keep up without a huge open transaction
      if (FTransCount mod CommitBatch)=0 then
      begin
        // Use this instead of CommitRetaining to give garbage collection a chance
        FRWTrans.Commit;
        FRWTrans.StartTransaction;
        FTransCount:=0;
      end;
    except
      on E: EIBDatabaseError do
      begin
        // If we commit above, we should roll back in this error handler.
        {$IFDEF CRAZYDEBUG}
        Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
          ': Database error running insert query (filename: ', InsertFields.FILENAME,
          '). Database error message: ',
          E.Message, '; Database error code (GDS code): ', E.GDSErrorCode
          );
        Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
          ': Parameters were: ');
        for DebugCounter := 0 to FInsertQuery.Params.Count - 1 do
        begin
          Writeln(stderr, FInsertQuery.Params[DebugCounter].Name, ': *',
            FInsertQuery.Params[DebugCounter].Value, '*');
        end;
        Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
          ': Rolling back transaction.');
        {$ENDIF}
        FRWTrans.RollBack;
      end;
      on E: Exception do
      begin
        {$IFDEF CRAZYDEBUG}
        Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
          ': Error running insert query (filename: ',
          InsertFields.FILENAME, '). Technical details: ',
          E.Message
          );
        Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
          ': Parameters were: ');
        for DebugCounter := 0 to FInsertQuery.Params.Count - 1 do
        begin
          Writeln(stderr, FInsertQuery.Params[DebugCounter].Name, ': *',
            FInsertQuery.Params[DebugCounter].Value, '*');
        end;
        {$ENDIF}
      end;
    end;

    {$IFDEF HELLFREEZESOVER}
    Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
      ': SaveDirectoryEntry: insert query code done. (', InsertFields.FILENAME, ')');
    {$ENDIF}
  except
    on E: EIBDatabaseError do
    begin
      // We would have committed, so we should be rolling back.
      {$IFDEF CRAZYDEBUG}
      Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
        ': Database error setting parameters (or possibly running insert query) (filename: ',
        InsertFields.FILENAME,
        '). Database error message: ',
        E.Message, '; Database error code (GDS code): ', E.GDSErrorCode
        );
      Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
        ': Parameters were: ');
      for DebugCounter := 0 to FInsertQuery.Params.Count - 1 do
      begin
        Writeln(stderr, FInsertQuery.Params[DebugCounter].Name, ': *',
          FInsertQuery.Params[DebugCounter].Value, '*');
      end;
      Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
        ': Rolling back transaction.');
      {$ENDIF}
      FRWTrans.RollBack;
    end;
    on E: Exception do
    begin
      {$IFDEF CRAZYDEBUG}
      Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
        ': Error setting parameters (or possibly running insert query) (filename: ',
        InsertFields.FILENAME, '). Technical details: ',
        E.Message
        );
      Writeln(stderr, 'Debug: ', DateTimeToStr(Now),
        ': Parameters were: ');
      for DebugCounter := 0 to FInsertQuery.Params.Count - 1 do
      begin
        Writeln(stderr, FInsertQuery.Params[DebugCounter].Name, ':',
          FInsertQuery.Params[DebugCounter].Value);
      end;
      {$ENDIF}
    end;
  end;
end;

procedure TFlocateDB.SavePendingChanges;
begin
  if FRWTrans.Active then
  begin
    FRWTrans.Commit;
    FRWTrans.StartTransaction;
  end;
end;

end.


