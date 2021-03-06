unit xlscommon;

{ Comments often have links to sections in the
OpenOffice Microsoft Excel File Format document }

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils, DateUtils,
  fpspreadsheet,
  fpsutils, lconvencoding;

const
  { RECORD IDs which didn't change across versions 2-8 }
  INT_EXCEL_ID_CODEPAGE   = $0042;
  INT_EXCEL_ID_DATEMODE   = $0022;

  { Formula constants TokenID values }

  { Binary Operator Tokens 3.6}
  INT_EXCEL_TOKEN_TADD    = $03;
  INT_EXCEL_TOKEN_TSUB    = $04;
  INT_EXCEL_TOKEN_TMUL    = $05;
  INT_EXCEL_TOKEN_TDIV    = $06;
  INT_EXCEL_TOKEN_TPOWER  = $07; // Power Exponentiation
  INT_EXCEL_TOKEN_TCONCAT = $08; // Concatenation
  INT_EXCEL_TOKEN_TLT     = $09; // Less than
  INT_EXCEL_TOKEN_TLE     = $0A; // Less than or equal
  INT_EXCEL_TOKEN_TEQ     = $0B; // Equal
  INT_EXCEL_TOKEN_TGE     = $0C; // Greater than or equal
  INT_EXCEL_TOKEN_TGT     = $0D; // Greater than
  INT_EXCEL_TOKEN_TNE     = $0E; // Not equal
  INT_EXCEL_TOKEN_TISECT  = $0F; // Cell range intersection
  INT_EXCEL_TOKEN_TLIST   = $10; // Cell range list
  INT_EXCEL_TOKEN_TRANGE  = $11; // Cell range

  { Constant Operand Tokens, 3.8}
  INT_EXCEL_TOKEN_TSTR    = $17; //string
  INT_EXCEL_TOKEN_TBOOL   = $1D; //boolean
  INT_EXCEL_TOKEN_TINT    = $1E; //integer
  INT_EXCEL_TOKEN_TNUM    = $1F; //floating-point

  { Operand Tokens }
  // _R: reference; _V: value; _A: array
  INT_EXCEL_TOKEN_TREFR   = $24;
  INT_EXCEL_TOKEN_TREFV   = $44;
  INT_EXCEL_TOKEN_TREFA   = $64;

  { Function Tokens }
  // _R: reference; _V: value; _A: array
  // Offset 0: token; offset 1: index to a built-in sheet function ( ➜ 3.111)
  INT_EXCEL_TOKEN_FUNC_R = $21;
  INT_EXCEL_TOKEN_FUNC_V = $41;
  INT_EXCEL_TOKEN_FUNC_A = $61;

  //VAR: variable number of arguments:
  INT_EXCEL_TOKEN_FUNCVAR_R = $22;
  INT_EXCEL_TOKEN_FUNCVAR_V = $42;
  INT_EXCEL_TOKEN_FUNCVAR_A = $62;
  INT_EXCEL_TOKEN_TAREA_R = $25;

  { Built-in/worksheet functions }
  INT_EXCEL_SHEET_FUNC_ABS = 24; // $18
  INT_EXCEL_SHEET_FUNC_ROUND = 27; // $1B
  INT_EXCEL_SHEET_FUNC_DATE = 65; // $41
  INT_EXCEL_SHEET_FUNC_TIME = 66; // $42

  { Control Tokens, Special Tokens }
//  01H tExp Matrix formula or shared formula
//  02H tTbl Multiple operation table
//  15H tParen Parentheses
//  18H tNlr Natural language reference (BIFF8)
  INT_EXCEL_TOKEN_TATTR = $19; // tAttr Special attribute
//  1AH tSheet Start of external sheet reference (BIFF2-BIFF4)
//  1BH tEndSheet End of external sheet reference (BIFF2-BIFF4)

  { Built In Color Palette Indexes }
  // Proper spelling
  BUILT_IN_COLOR_PALETTE_BLACK     = $08; // 000000H
  BUILT_IN_COLOR_PALETTE_WHITE     = $09; // FFFFFFH
  BUILT_IN_COLOR_PALETTE_RED       = $0A; // FF0000H
  BUILT_IN_COLOR_PALETTE_GREEN     = $0B; // 00FF00H
  BUILT_IN_COLOR_PALETTE_BLUE      = $0C; // 0000FFH
  BUILT_IN_COLOR_PALETTE_YELLOW    = $0D; // FFFF00H
  BUILT_IN_COLOR_PALETTE_MAGENTA   = $0E; // FF00FFH
  BUILT_IN_COLOR_PALETTE_CYAN      = $0F; // 00FFFFH
  BUILT_IN_COLOR_PALETTE_DARK_RED  = $10; // 800000H
  BUILT_IN_COLOR_PALETTE_DARK_GREEN= $11; // 008000H
  BUILT_IN_COLOR_PALETTE_DARK_BLUE = $12; // 000080H
  BUILT_IN_COLOR_PALETTE_OLIVE     = $13; // 808000H
  BUILT_IN_COLOR_PALETTE_PURPLE    = $14; // 800080H
  BUILT_IN_COLOR_PALETTE_TEAL      = $15; // 008080H
  BUILT_IN_COLOR_PALETTE_SILVER    = $16; // C0C0C0H
  BUILT_IN_COLOR_PALETTE_GREY      = $17; // 808080H

  // Spelling mistake; kept for compatibility
  BUILT_IN_COLOR_PALLETE_BLACK     = $08 deprecated 'Please use the *_PALETTE version';
  BUILT_IN_COLOR_PALLETE_WHITE     = $09 deprecated 'Please use the *_PALETTE version';
  BUILT_IN_COLOR_PALLETE_RED       = $0A deprecated 'Please use the *_PALETTE version';
  BUILT_IN_COLOR_PALLETE_GREEN     = $0B deprecated 'Please use the *_PALETTE version';
  BUILT_IN_COLOR_PALLETE_BLUE      = $0C deprecated 'Please use the *_PALETTE version';
  BUILT_IN_COLOR_PALLETE_YELLOW    = $0D deprecated 'Please use the *_PALETTE version';
  BUILT_IN_COLOR_PALLETE_MAGENTA   = $0E deprecated 'Please use the *_PALETTE version';
  BUILT_IN_COLOR_PALLETE_CYAN      = $0F deprecated 'Please use the *_PALETTE version';
  BUILT_IN_COLOR_PALLETE_DARK_RED  = $10 deprecated 'Please use the *_PALETTE version';
  BUILT_IN_COLOR_PALLETE_DARK_GREEN= $11 deprecated 'Please use the *_PALETTE version';
  BUILT_IN_COLOR_PALLETE_DARK_BLUE = $12 deprecated 'Please use the *_PALETTE version';
  BUILT_IN_COLOR_PALLETE_OLIVE     = $13 deprecated 'Please use the *_PALETTE version';
  BUILT_IN_COLOR_PALLETE_PURPLE    = $14 deprecated 'Please use the *_PALETTE version';
  BUILT_IN_COLOR_PALLETE_TEAL      = $15 deprecated 'Please use the *_PALETTE version';
  BUILT_IN_COLOR_PALLETE_SILVER    = $16 deprecated 'Please use the *_PALETTE version';
  BUILT_IN_COLOR_PALLETE_GREY      = $17 deprecated 'Please use the *_PALETTE version';

  EXTRA_COLOR_PALETTE_GREY10PCT    = $18; // E6E6E6H //todo: is $18 correct? see 5.74.3 Built-In Default Colour Tables
  EXTRA_COLOR_PALETTE_GREY20PCT    = $19; // CCCCCCH //todo: is $19 correct? see 5.74.3 Built-In Default Colour Tables

  { CODEPAGE record constants }
  WORD_ASCII = 367;
  WORD_UTF_16 = 1200; // BIFF 8
  WORD_CP_1250_Latin2 = 1250;
  WORD_CP_1251_Cyrillic = 1251;
  WORD_CP_1252_Latin1 = 1252; // BIFF4-BIFF5
  WORD_CP_1253_Greek = 1253;
  WORD_CP_1254_Turkish = 1254;
  WORD_CP_1255_Hebrew = 1255;
  WORD_CP_1256_Arabic = 1256;
  WORD_CP_1257_Baltic = 1257;
  WORD_CP_1258_Vietnamese = 1258;
  WORD_CP_1258_Latin1_BIFF2_3 = 32769; // BIFF2-BIFF3

  { DATEMODE record, 5.28 }
  DATEMODE_1900_BASE=1; //1/1/1900 minus 1 day in FPC TDateTime
  DATEMODE_1904_BASE=1462; //1/1/1904 in FPC TDateTime

  { FORMAT record constants }
  // Just a subset; needed for output to date/time records
  FORMAT_GENERAL = 0; //general/default format
  FORMAT_SHORT_DATE = 14; //short date
  FORMAT_SHORT_DATETIME = 22; //short date+time


type
  TDateMode=(dm1900,dm1904); //DATEMODE values, 5.28

  // Adjusts Excel float (date, date/time, time) with the file's base date to get a TDateTime
  function ConvertExcelDateTimeToDateTime
    (const AExcelDateNum: Double; ADateMode: TDateMode): TDateTime;
  // Adjusts TDateTime with the file's base date to get
  // an Excel float value representing a time/date/datetime
  function ConvertDateTimeToExcelDateTime
    (const ADateTime: TDateTime; ADateMode: TDateMode): Double;

type
  { TsSpreadBIFFReader }
  TsSpreadBIFFReader = class(TsCustomSpreadReader)
  protected
    FCodepage: string; // in a format prepared for lconvencoding.ConvertEncoding
    FDateMode: TDateMode;
    constructor Create; override;
    // Here we can add reading of records which didn't change across BIFF2-8 versions
    // Workbook Globals records
    procedure ReadCodePage(AStream: TStream);
    // Figures out what the base year for dates is for this file
    procedure ReadDateMode(AStream: TStream);
  end;

  { TsSpreadBIFFWriter }

  TsSpreadBIFFWriter = class(TsCustomSpreadWriter)
  protected
    FDateMode: TDateMode;
    FLastRow: Integer;
    FLastCol: Word;
    function FPSColorToExcelPalette(AColor: TsColor): Word;
    procedure GetLastRowCallback(ACell: PCell; AStream: TStream);
    function GetLastRowIndex(AWorksheet: TsWorksheet): Integer;
    procedure GetLastColCallback(ACell: PCell; AStream: TStream);
    function GetLastColIndex(AWorksheet: TsWorksheet): Word;
    function FormulaElementKindToExcelTokenID(AElementKind: TFEKind; out ASecondaryID: Word): Byte;
    // Other records which didn't change
    // Workbook Globals records
    // Write out used codepage for character encoding
    procedure WriteCodepage(AStream: TStream; AEncoding: TsEncoding);
    // Writes out DATEMODE record depending on FDateMode
    procedure WriteDateMode(AStream: TStream);
  public
    constructor Create; override;
  end;

implementation

function ConvertExcelDateTimeToDateTime(
  const AExcelDateNum: Double; ADateMode: TDateMode): TDateTime;
begin
  // Time only:
  if (AExcelDateNum<1) and (AExcelDateNum>=0)  then
  begin
    Result:=AExcelDateNum;
  end
  else
  begin
    case ADateMode of
    dm1900:
    begin
      // Check for Lotus 1-2-3 bug with 1900 leap year
      if AExcelDateNum=61.0 then
      // 29 feb does not exist, change to 28
      // Spell out that we remove a day for ehm "clarity".
        result:=61.0-1.0+DATEMODE_1900_BASE-1.0
      else
        result:=AExcelDateNum+DATEMODE_1900_BASE-1.0;
    end;
    dm1904:
      result:=AExcelDateNum+DATEMODE_1904_BASE;
    else
      raise Exception.CreateFmt('ConvertExcelDateTimeToDateTime: unknown datemode %d. Please correct fpspreadsheet source code. ', [ADateMode]);
    end;
  end;
end;

function ConvertDateTimeToExcelDateTime(const ADateTime: TDateTime;
  ADateMode: TDateMode): Double;
begin
  // Time only:
  if (ADateTime<1) and (ADateTime>=0) then
  begin
    Result:=ADateTime;
  end
  else
  begin
    case ADateMode of
    dm1900:
      result:=ADateTime-DATEMODE_1900_BASE+1.0;
    dm1904:
      result:=ADateTime-DATEMODE_1904_BASE;
    else
      raise Exception.CreateFmt('ConvertDateTimeToExcelDateTime: unknown datemode %d. Please correct fpspreadsheet source code. ', [ADateMode]);
    end;
  end;
end;

{ TsSpreadBIFFReader }

constructor TsSpreadBIFFReader.Create;
begin
  inherited Create;
  // Initial base date in case it won't be read from file
  FDateMode := dm1900;
end;

// In BIFF 8 it seams to always use the UTF-16 codepage
procedure TsSpreadBIFFReader.ReadCodePage(AStream: TStream);
var
  lCodePage: Word;
begin
  { Codepage }
  lCodePage := WordLEToN(AStream.ReadWord());

  case lCodePage of
  // 016FH = 367 = ASCII
  // 01B5H = 437 = IBM PC CP-437 (US)
  //02D0H = 720 = IBM PC CP-720 (OEM Arabic)
  //02E1H = 737 = IBM PC CP-737 (Greek)
  //0307H = 775 = IBM PC CP-775 (Baltic)
  //0352H = 850 = IBM PC CP-850 (Latin I)
  $0352: FCodepage := 'cp850';
  //0354H = 852 = IBM PC CP-852 (Latin II (Central European))
  $0354: FCodepage := 'cp852';
  //0357H = 855 = IBM PC CP-855 (Cyrillic)
  $0357: FCodepage := 'cp855';
  //0359H = 857 = IBM PC CP-857 (Turkish)
  $0359: FCodepage := 'cp857';
  //035AH = 858 = IBM PC CP-858 (Multilingual Latin I with Euro)
  //035CH = 860 = IBM PC CP-860 (Portuguese)
  //035DH = 861 = IBM PC CP-861 (Icelandic)
  //035EH = 862 = IBM PC CP-862 (Hebrew)
  //035FH = 863 = IBM PC CP-863 (Canadian (French))
  //0360H = 864 = IBM PC CP-864 (Arabic)
  //0361H = 865 = IBM PC CP-865 (Nordic)
  //0362H = 866 = IBM PC CP-866 (Cyrillic (Russian))
  //0365H = 869 = IBM PC CP-869 (Greek (Modern))
  //036AH = 874 = Windows CP-874 (Thai)
  //03A4H = 932 = Windows CP-932 (Japanese Shift-JIS)
  //03A8H = 936 = Windows CP-936 (Chinese Simplified GBK)
  //03B5H = 949 = Windows CP-949 (Korean (Wansung))
  //03B6H = 950 = Windows CP-950 (Chinese Traditional BIG5)
  //04B0H = 1200 = UTF-16 (BIFF8)
  $04B0: FCodepage := 'utf-16';
  //04E2H = 1250 = Windows CP-1250 (Latin II) (Central European)
  //04E3H = 1251 = Windows CP-1251 (Cyrillic)
  //04E4H = 1252 = Windows CP-1252 (Latin I) (BIFF4-BIFF5)
  //04E5H = 1253 = Windows CP-1253 (Greek)
  //04E6H = 1254 = Windows CP-1254 (Turkish)
  $04E6: FCodepage := 'cp1254';
  //04E7H = 1255 = Windows CP-1255 (Hebrew)
  //04E8H = 1256 = Windows CP-1256 (Arabic)
  //04E9H = 1257 = Windows CP-1257 (Baltic)
  //04EAH = 1258 = Windows CP-1258 (Vietnamese)
  //0551H = 1361 = Windows CP-1361 (Korean (Johab))
  //2710H = 10000 = Apple Roman
  //8000H = 32768 = Apple Roman
  //8001H = 32769 = Windows CP-1252 (Latin I) (BIFF2-BIFF3)
  end;
end;

procedure TsSpreadBIFFReader.ReadDateMode(AStream: TStream);
var
  lBaseMode: Word;
begin
  //5.28 DATEMODE
  //BIFF2 BIFF3 BIFF4 BIFF5 BIFF8
  //0022H 0022H 0022H 0022H 0022H
  //This record specifies the base date for displaying date values. All dates are stored as count of days past this base date. In
  //BIFF2-BIFF4 this record is part of the Calculation Settings Block (➜4.3). In BIFF5-BIFF8 it is stored in the Workbook
  //Globals Substream.
  //Record DATEMODE, BIFF2-BIFF8:
  //Offset Size Contents
  //0 2 0 = Base date is 1899-Dec-31 (the cell value 1 represents 1900-Jan-01)
  //    1 = Base date is 1904-Jan-01 (the cell value 1 represents 1904-Jan-02)
  lBaseMode := WordLEtoN(AStream.ReadWord);
  case lBaseMode of
    0: FDateMode := dm1900;
    1: FDateMode := dm1904;
    else raise Exception.CreateFmt('Error reading file. Got unknown date mode number %d.',[lBaseMode]);
  end;
end;

function TsSpreadBIFFWriter.FPSColorToExcelPalette(AColor: TsColor): Word;
begin
  case AColor of
    scBlack: Result := BUILT_IN_COLOR_PALLETE_BLACK;
    scWhite: Result := BUILT_IN_COLOR_PALLETE_WHITE;
    scRed: Result := BUILT_IN_COLOR_PALLETE_RED;
    scGREEN: Result := BUILT_IN_COLOR_PALLETE_GREEN;
    scBLUE: Result := BUILT_IN_COLOR_PALLETE_BLUE;
    scYELLOW: Result := BUILT_IN_COLOR_PALLETE_YELLOW;
    scMAGENTA: Result := BUILT_IN_COLOR_PALLETE_MAGENTA;
    scCYAN: Result := BUILT_IN_COLOR_PALLETE_CYAN;
    scDarkRed: Result := BUILT_IN_COLOR_PALLETE_DARK_RED;
    scDarkGreen: Result := BUILT_IN_COLOR_PALLETE_DARK_GREEN;
    scDarkBlue: Result := BUILT_IN_COLOR_PALLETE_DARK_BLUE;
    scOLIVE: Result := BUILT_IN_COLOR_PALLETE_OLIVE;
    scPURPLE: Result := BUILT_IN_COLOR_PALLETE_PURPLE;
    scTEAL: Result := BUILT_IN_COLOR_PALLETE_TEAL;
    scSilver: Result := BUILT_IN_COLOR_PALLETE_SILVER;
    scGrey: Result := BUILT_IN_COLOR_PALLETE_GREY;
    //
    scGrey10pct: Result := EXTRA_COLOR_PALETTE_GREY10PCT;
    scGrey20pct: Result := EXTRA_COLOR_PALETTE_GREY20PCT;
  end;
end;

procedure TsSpreadBIFFWriter.GetLastRowCallback(ACell: PCell; AStream: TStream);
begin
  if ACell^.Row > FLastRow then FLastRow := ACell^.Row;
end;

function TsSpreadBIFFWriter.GetLastRowIndex(AWorksheet: TsWorksheet): Integer;
begin
  FLastRow := 0;
  IterateThroughCells(nil, AWorksheet.Cells, GetLastRowCallback);
  Result := FLastRow;
end;

procedure TsSpreadBIFFWriter.GetLastColCallback(ACell: PCell; AStream: TStream);
begin
  if ACell^.Col > FLastCol then FLastCol := ACell^.Col;
end;

function TsSpreadBIFFWriter.GetLastColIndex(AWorksheet: TsWorksheet): Word;
begin
  FLastCol := 0;
  IterateThroughCells(nil, AWorksheet.Cells, GetLastColCallback);
  Result := FLastCol;
end;

function TsSpreadBIFFWriter.FormulaElementKindToExcelTokenID(
  AElementKind: TFEKind; out ASecondaryID: Word): Byte;
begin
  ASecondaryID := 0;

  case AElementKind of
    { Operand Tokens }
    fekCell:  Result := INT_EXCEL_TOKEN_TREFR;
    fekCellRange: Result := INT_EXCEL_TOKEN_TAREA_R;
    fekNum:   Result := INT_EXCEL_TOKEN_TNUM;
    { Basic operations }
    fekAdd:   Result := INT_EXCEL_TOKEN_TADD;
    fekSub:   Result := INT_EXCEL_TOKEN_TSUB;
    fekDiv:   Result := INT_EXCEL_TOKEN_TDIV;
    fekMul:   Result := INT_EXCEL_TOKEN_TMUL;
    { Built-in Functions}
    fekABS:
    begin
      Result := INT_EXCEL_TOKEN_FUNC_V;
      ASecondaryID := INT_EXCEL_SHEET_FUNC_ABS;
    end;
    fekDATE:
    begin
      Result := INT_EXCEL_TOKEN_FUNC_V;
      ASecondaryID := INT_EXCEL_SHEET_FUNC_DATE;
    end;
    fekROUND:
    begin
      Result := INT_EXCEL_TOKEN_FUNC_V;
      ASecondaryID := INT_EXCEL_SHEET_FUNC_ROUND;
    end;
    fekTIME:
    begin
      Result := INT_EXCEL_TOKEN_FUNC_V;
      ASecondaryID := INT_EXCEL_SHEET_FUNC_TIME;
    end;
    { Other operations }
    fekOpSUM: Result := INT_EXCEL_TOKEN_TATTR;
  else
    Result := 0;
  end;
end;

procedure TsSpreadBIFFWriter.WriteCodepage(AStream: TStream;
  AEncoding: TsEncoding);
var
  lCodepage: Word;
begin
  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_CODEPAGE));
  AStream.WriteWord(WordToLE(2));

  { Codepage }
  case AEncoding of
    seLatin2:   lCodepage := WORD_CP_1250_Latin2;
    seCyrillic: lCodepage := WORD_CP_1251_Cyrillic;
    seGreek:    lCodepage := WORD_CP_1253_Greek;
    seTurkish:  lCodepage := WORD_CP_1254_Turkish;
    seHebrew:   lCodepage := WORD_CP_1255_Hebrew;
    seArabic:   lCodepage := WORD_CP_1256_Arabic;
  else
    // Default is Latin1
    lCodepage := WORD_CP_1252_Latin1;
  end;
  AStream.WriteWord(WordToLE(lCodepage));
end;

procedure TsSpreadBIFFWriter.WriteDateMode(AStream: TStream);
begin
  { BIFF Record header }
  // todo: check whether this is in the right place. should end up in workbook globals stream
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_DATEMODE));
  AStream.WriteWord(WordToLE(2));

  case FDateMode of
    dm1900: AStream.WriteWord(WordToLE(0));
    dm1904: AStream.WriteWord(WordToLE(1));
    else raise Exception.CreateFmt('Unknown datemode number %d. Please correct fpspreadsheet code.', [FDateMode]);
  end;
end;

constructor TsSpreadBIFFWriter.Create;
begin
  inherited Create;
  // Initial base date in case it won't be set otherwise.
  // Use 1900 to get a bit more range between 1900..1904.
  FDateMode := dm1900;
end;

end.

