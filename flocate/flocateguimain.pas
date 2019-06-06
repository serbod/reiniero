unit flocateguimain; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, db, FileUtil, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Menus, ComCtrls, DBGrids, EditBtn, search, flocatedb;

type

  { TMainForm }

  TMainForm = class(TForm)
    DatasourceResult: TDatasource;
    DatasourceCatalogs: TDatasource;
    ImageList: TImageList;
    RootDirectoryControl: TDirectoryEdit;
    NewCatalogButton: TButton;
    ResultsGrid: TDBGrid;
    CatalogSelection: TDBGrid;
    IBConnection1: TIBConnection;
    MainMenu1: TMainMenu;
    CatalogDefaultSelect: TRadioButton;
    CatalogSpecificSelect: TRadioButton;
    ScanButton: TButton;
    CommandGroup: TGroupBox;
    ResultButton: TButton;
    SQLQueryResults: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure CatalogDefaultSelectChange(Sender: TObject);
    procedure CatalogSpecificSelectChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NewCatalogButtonClick(Sender: TObject);
    procedure ResultButtonClick(Sender: TObject);
    procedure ResultsGridTitleClick(Column: TColumn);
    procedure ScanButtonClick(Sender: TObject);
    procedure UpdateData;
  private
    { private declarations }
    FFLocateDB: TFlocateDB;
    FLastColumn: TColumn; //column we last sorted on
    // Clears indexes for SQL query. Needed after sorting before reuse to avoid
    // exception (verify if this still is required in FPC trunk)
    procedure ClearIndexes(Query: TSQLQuery);
    // Updates grid UI based on given parameters
    procedure UpdateResultsGrid(CatalogDescription,ScanDescription:string);
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation
uses flocateshared {config};

{$R *.lfm}

{ TMainForm }

procedure TMainForm.UpdateData;
begin
  if CatalogDefaultSelect.Checked = true then
  begin
    CatalogSelection.Enabled:=false;
    CatalogSelection.Visible:=false;
    NewCatalogButton.Enabled:=false;
  end
  else
  begin
    DatasourceCatalogs.Enabled:=true;
    CatalogSelection.Enabled:=true;
    CatalogSelection.Visible:=true;
    NewCatalogButton.Enabled:=true;
  end;
end;

procedure TMainForm.UpdateResultsGrid(CatalogDescription,ScanDescription:string);
begin
  SQLQueryResults.Close;
  ClearIndexes(SQLQueryResults);
  FFLocateDB.GetResults(CatalogDescription, ScanDescription, SQLQueryResults);
  SQLQueryResults.Open;
  ResultsGrid.Columns[0].Visible:=false; //Hide scanid & catalogid columns
  ResultsGrid.Columns[1].Visible:=false;
  ResultsGrid.AutoSizeColumns; //resize to widest text found instead of full column width
end;

procedure TMainForm.CatalogDefaultSelectChange(Sender: TObject);
begin
  UpdateData;
end;

procedure TMainForm.CatalogSpecificSelectChange(Sender: TObject);
begin
  UpdateData;
end;

procedure TMainForm.ClearIndexes(Query: TSQLQuery);
var
  Status:boolean;
begin
  // No direct way to do this
  Status:=Query.UniDirectional;
  Query.UniDirectional:=true;
  Query.UniDirectional:=false;
  if Status<>false then
    Query.UniDirectional:=status;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  Config: Tflocateshared;
  DBParams: TStringList;
begin
  FLastColumn:=nil;
  // Set up database
  Config := Tflocateshared.Create;
  DBParams := TStringList.Create;
  Try
    if Config.UseEmbedded = false then
    begin
      DBParams.Add('port=' + IntToStr(Config.Port));
      IBConnection1.HostName := Config.Host;
    end
    else
    begin
      IBConnection1.HostName := '';
    end;
    IBConnection1.Params := DBParams;
    IBConnection1.DatabaseName := Config.Database;
    IBConnection1.Username := Config.User;
    IBConnection1.Password := Config.Password;
    IBConnection1.Charset := 'UTF8';
  finally
    Config.free;
  end;
  // Try to set scan directory to directory where application resides.
  RootDirectoryControl.Directory:=ExtractFilePath(paramstr(0));
  UpdateData;
  FFLocateDB:=TFlocateDB.Create;
  FFLocateDB.CatalogScans.Open;
  DatasourceCatalogs.DataSet:=FFlocateDB.CatalogScans;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FFLocateDB.Free;
end;

procedure TMainForm.NewCatalogButtonClick(Sender: TObject);
begin
  ShowMessage('//todo: fix me');
end;

procedure TMainForm.ResultButtonClick(Sender: TObject);
var
  CatalogDescription, ScanDescription: string;
begin
  Screen.Cursor := crHourglass;
  try
    CatalogDescription:=DatasourceCatalogs.DataSet.FieldByName('CATALOGDESCRIPTION').AsString;
    ScanDescription:=DatasourceCatalogs.DataSet.FieldByName('SCANDESCRIPTION').AsString;
    UpdateResultsGrid(CatalogDescription,ScanDescription);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.ResultsGridTitleClick(Column: TColumn);
const
  ImageArrowUp=0;
  ImageArrowDown=1;
var
  ASC_IndexName, DESC_IndexName:string;
begin
  Screen.Cursor:=crHourGlass;
  try
    // Requires query maxindexescount to be sufficient for all indexes
    ASC_IndexName:='ASC_'+Column.FieldName;
    DESC_IndexName:='DESC_'+Column.FieldName;
    // indexes can't sort binary types such as ftMemo, ftBLOB
    if (Column.Field.DataType in [ftBLOB,ftMemo,ftWideMemo]) then
      exit;
    // check if an ascending index already exists for this column.
    // if not, create one
    if SQLQueryResults.IndexDefs.IndexOf(ASC_IndexName) = -1 then
      SQLQueryResults.AddIndex(ASC_IndexName,column.FieldName,[]);
    // Check if a descending index already exists for this column
    // if not, create one
    if SQLQueryResults.IndexDefs.IndexOf(DESC_IndexName) = -1 then
      SQLQueryResults.AddIndex(DESC_IndexName,column.FieldName,[ixDescending]);

    // Ensure index defs are up to date
    SQLQueryResults.IndexDefs.Updated:=false;
    SQLQueryResults.IndexDefs.Update;
    // Use the column tag to toggle ASC/DESC
    column.tag := not column.tag;
    if boolean(column.tag) then
    begin
      Column.Title.ImageIndex:=ImageArrowUp;
      SQLQueryResults.IndexName:=ASC_IndexName;
    end
    else
    begin
      Column.Title.ImageIndex:=ImageArrowDown;
      SQLQueryResults.IndexName:=DESC_IndexName;
    end;
    // Remove the sort arrow from the previous column we sorted
    if (FLastColumn <> nil) and (FlastColumn <> Column) then
      FLastColumn.Title.ImageIndex:=-1;
    FLastColumn:=column;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TMainForm.ScanButtonClick(Sender: TObject);
var
  CatalogDescr,ScanDescr:string;
  Search: TFileSearch;
  RootDir: string;
begin
  RootDir:=RootDirectoryControl.Text;
  if RootDir='' then
  begin
    ShowMessage('Please select a starting directory for the search first.');
    exit;
  end;

  CatalogDescr:='flocategui default';
  ScanDescr:=DateTimeToStr(Now) + ' scan of '+RootDir;

  Search:=TFileSearch.Create(CatalogDescr,ScanDescr);
  Screen.Cursor:=crHourglass;
  try
    try
      Search.RootDirectory := RootDir;
      Search.Directory := false; //mmm. is this necessary?
      Search.SearchSubdirectories := true;
      Search.Hidden := true;
      Search.ReadOnly := true;
      Search.System := true;
      Search.Search;
    except
      on E: Exception do
      begin
        ShowMessage('Flocate: Error searching for files. Technical details: '+
          E.ClassName+'/'+E.Message);
      end;
    end;
    UpdateData;
    UpdateResultsGrid(CatalogDescr,ScanDescr);
  finally
    Screen.Cursor := crDefault;
    Search.Free;
  end;
end;

end.

