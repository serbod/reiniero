Unit mp3id3tag;

{*
Windows/Linux/Mac OSX FreePascal code to read an MP3 tag (ID3V1 only).
======================================================================================
MIT X11 License: no warranties, express or implied, but all other use permitted:
Copyright (c) 2010-2013 copyright holders

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

Interface

Uses
Classes, SysUtils;

Type
  TID3v1Tag = Record {*ID3V1/ID3V1.1*}
    ID: string[3];
    Title: string[30];
    //todo: do something with null terminated string; I get results that are too long!
    Artist: string[30];
    Album: string[30];
    Year: integer;
    Comment: string[30];
    {*ID3v1.1: last byte of comment = track number (if byte before that = 0*}
    Genre: byte;
  End;

  TID3Tag = Class(TObject)
  Private
    FAlbum: string;
    FArtist: string;
    FComment: string;
    FFullPath: string;
    FGenre: string;
    FSignature: string;
    FTitle: string;
    FTrack: byte;
    FYear: integer;

    ValidTag: boolean;
    Procedure Init; //initialize variables
    Procedure ReadFromFile(); //read in tag from file and process
  Public
    constructor Create(Filename: String);
    destructor Destroy; override;

    property Album: string read FAlbum;
    property Artist: string read FArtist;
    property Comment: string read FComment;
    property Genre: string read FGenre;
    property FullPath: string read FFullPath;
    property Signature: string read FSignature;
    //Signature of tag. Should be TAG for ID3v1 at least
    property Title: string read FTitle;
    property Track: byte read FTrack;
    property Valid: boolean read ValidTag;
    property Year: integer read FYear;
  End;

Const
  //Id3V1 genres: see eg http://www.multimediasoft.com/amp3dj/help/amp3dj_00003e.htm#ss13.3
  Genres: array[0..146] Of string =
                                    ('Blues', 'Classic Rock', 'Country', 'Dance', 'Disco', 'Funk',
                                     'Grunge',
                                     'Hip-Hop', 'Jazz', 'Metal', 'New Age', 'Oldies', 'Other', 'Pop'
                                     , 'R&B',
                                     'Rap', 'Reggae', 'Rock', 'Techno', 'Industrial', 'Alternative',
                                     'Ska',
                                     'Death Metal', 'Pranks', 'Soundtrack', 'Euro-Techno', 'Ambient'
                                     ,
                                     'Trip-Hop', 'Vocal', 'Jazz+Funk', 'Fusion', 'Trance',
                                     'Classical',
                                     'Instrumental', 'Acid', 'House', 'Game', 'Sound Clip', 'Gospel'
                                     , 'Noise',
                                     'Alternative Rock', 'Bass', 'Punk', 'Space', 'Meditative',
                                     'Instrumental Pop',
                                     'Instrumental Rock', 'Ethnic', 'Gothic', 'Darkwave',
                                     'Techno-Industrial',
                                     'Electronic',
                                     'Pop-Folk', 'Eurodance', 'Dream', 'Southern Rock', 'Comedy',
                                     'Cult', 'Gangsta',
                                     'Top 40', 'Christian Rap', 'Pop/Funk', 'Jungle', 'Native US',
                                     'Cabaret', 'New Wave',
                                     'Psychadelic', 'Rave', 'Showtunes', 'Trailer', 'Lo-Fi',
                                     'Tribal', 'Acid Punk',
                                     'Acid Jazz', 'Polka', 'Retro', 'Musical', 'Rock & Roll',
                                     'Hard Rock', 'Folk',
                                     'Folk-Rock', 'National Folk', 'Swing', 'Fast Fusion', 'Bebob',
                                     'Latin', 'Revival',
                                     'Celtic', 'Bluegrass', 'Avantgarde', 'Gothic Rock',
                                     'Progressive Rock',
                                     'Psychedelic Rock', 'Symphonic Rock', 'Slow Rock', 'Big Band',
                                     'Chorus',
                                     'Easy Listening', 'Acoustic', 'Humour', 'Speech', 'Chanson',
                                     'Opera',
                                     'Chamber Music', 'Sonata', 'Symphony', 'Booty Bass', 'Primus',
                                     'Porn Groove',
                                     'Satire', 'Slow Jam', 'Club', 'Tango', 'Samba', 'Folklore',
                                     'Ballad',
                                     'Power Ballad', 'Rhytmic Soul', 'Freestyle', 'Duet',
                                     'Punk Rock', 'Drum Solo',
                                     'Acapella', 'Euro-House', 'Dance Hall', 'Goa', 'Drum & Bass',
                                     'Club-House',
                                     'Hardcore', 'Terror', 'Indie', 'BritPop', 'Negerpunk',
                                     'Polsk Punk', 'Beat',
                                     'Christian Gangsta', 'Heavy Metal', 'Black Metal', 'Crossover',
                                     'Contemporary C',
                                     'Christian Rock', 'Merengue', 'Salsa', 'Thrash Metal', 'Anime',
                                     'JPop', 'SynthPop');

implementation

Procedure Tid3tag.ReadFromFile();

Var
  FS: TFileStream;
  Buffer: array [1..128] Of char;
  GenreID: byte;
  i: integer;
Begin
  FS := TFileStream.Create(FFullPath, fmOpenRead or fmShareDenyWrite);
  Try
    FS.Seek(-128, soFromEnd);
    //initialize buffer
    for i := 1 to 128 do
      Buffer[i] := ' ';
    FS.Read(Buffer, 128);
    FSignature := Copy(Buffer, 1, 3);
    {*
    {$IFDEF CRAZYDEBUG}
    writeln(stderr, 'Debug: ', DateTimeToStr(Now),
    ': Signature for file: ', FFullPath, ' is: *', FSignature, '*');
    {$ENDIF}
    *}
    If FSignature = 'TAG' Then
      Begin
        FTitle := Trim(Copy(Buffer, 4, 30));
        FArtist := Trim(Copy(Buffer, 34, 30));
        FAlbum := Trim(Copy(Buffer, 64, 30));
        Try;
          FYear := StrToInt(Copy(Buffer, 94, 4));
        Except;
{*
We know tags sometimes have spaces instead of a year. Let's ignore it.
{$IFDEF CRAZYDEBUG}
          writeln(stderr, 'Debug: ', DateTimeToStr(Now),
          ': Error getting year for mp3 file: ', FFullPath, 'year in ascii: *',
          Copy(Buffer, 94, 4), '*');
{$ENDIF}
          *}
          FYear := 0;
        End;
        If (Ord(Buffer[127]) > 0) And (Ord(Buffer[126]) = 0) Then
          Begin
            // Assume v1.1 format with added trackid
            FTrack := Ord(Buffer[127]);
            FComment := Trim(Copy(Buffer, 98, 30));
          End
        Else
          Begin
            // Assume v1.0 format
            FTrack := 0;
            FComment := Trim(Copy(Buffer, 98, 30));
          End;
        GenreID := Ord(Buffer[128]);
        If GenreID > High(Genres) Then
          FGenre := 'Unknown'
        Else
          FGenre := Genres[GenreID];
        ValidTag := True;
      End
      // valid tag
    Else
      Begin
        ValidTag := False;
      End;
  Finally
    FS.Free;
  End;
End;

Procedure Tid3tag.Init;
Begin
  FSignature := '';
  FTitle := '';
  FArtist := '';
  FAlbum := '';
  FYear := 0;
  FComment := '';
  FGenre := '';
  ValidTag := False;
  //OurTag: TID3Tag;
  //OurFile: file;
End;


constructor Tid3tag.Create(FileName: String);
Begin
  Init;
  FFullPath := FileName;
  //Immediately read in tag info:
  ReadFromFile;
End;

destructor Tid3tag.Destroy;
Begin
{*
{$IFDEF CRAZYDEBUG}
  writeln(stderr, 'Debug: ', DateTimeToStr(Now),
  ': Tmp3id3v1tag.Destroy started for ', FFullpath);
{$ENDIF}
  *}
  inherited;
End;


End.

