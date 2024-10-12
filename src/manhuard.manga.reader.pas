unit Manhuard.Manga.Reader;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fpjson, jsonparser, Generics.Collections, Unzip, ZipUtils, LibTar,
  Manhuard.Manga;

const
  MANGA_PACKAGE_FILE_EXTS : array[mptZip..mptEPub] of string = ('.cbz', '.cba', '.cbr', '.epub');
  MANGA_BOOK_JSON_FILE_NAME = 'book.json';

type

  EMangaReaderError = class(Exception);

  TVolumeList = specialize TList<TMangaBook.TVolume>;
  TPageList = specialize TList<TMangaBook.TPage>;

  { TJSONDataHelper }

  TJSONDataHelper = class helper for TJSONData
  public
    function ReadString(Path: string; Default: string = ''): string;
    function ReadUInt(Path: string; Default: Integer = 0): Integer;
    function ReadStringArray(Path: string; Default: TStringArray = nil): TStringArray;
    function ReadVolumeArray(Path: string): TMangaBook.TVolumeArray;
    function ReadArraySize(Path: string): SizeInt;
  end;

  { TReader }

  TReader = class abstract
  private
    FPath: string;
  public
    constructor Create(APath: string);
    destructor Destroy; override;
    property Path: string read FPath;
    function GetCover(Cover: TPicture): boolean; virtual; abstract;
    function Read(MangaBook: TMangaBook; Details: TMangaBook.PDetails = nil): boolean; virtual; abstract;
    function Read(const VolumePath: string; PageList: TPageList): boolean; virtual; abstract;
    function Read(const PagePath: string; Page: TPicture): boolean; virtual; abstract;
  end;

  { TGeneralReader }

  TGeneralReader = class abstract (TReader)
  type

    { TVirtualDirectory }

    TVirtualDirectory = class
    type
      TFile = TMangaBook.TPage;
      TDirList = specialize TObjectList<TVirtualDirectory>;
      TRefDirList = specialize TList<TVirtualDirectory>;
      TFileList = specialize TList<TFile>;
    private
      FPath: string;
      FDirList: TDirList;
      FFileList: TFileList;
      FCachedDirPath: string;
      FCachedDir: TVirtualDirectory;
      function FindSubdir(SubdirName: string): TVirtualDirectory;
      function FindOrAddSubdir(SubdirName: string): TVirtualDirectory;
      function GetName: string;
      function FindOrAdd(APath: string): TVirtualDirectory;
      function Find(APath: string): TVirtualDirectory;
    public
      constructor Create(Path: string = '');
      destructor Destroy; override;
      property Path: string read FPath;
      property Name: string read GetName;
      property Files: TFileList read FFileList;
      property Dirs: TDirList read FDirList;
      procedure AddFile(FilePath: string; Size: Int64);
      function LeafNodeCount: Integer;
      function FindLeafNodes(LeafNodeList: TRefDirList): Integer;
    end;
  private
    FVDir: TVirtualDirectory;
    FScanned: boolean;
    function GetVDir: TVirtualDirectory;
  protected
    procedure AddFile(FilePath: string; Size: Int64);
    procedure ParseJSON(JSONData: TJSONData; MangaBook: TMangaBook; Details: TMangaBook.PDetails);
    procedure ScanAll; virtual; abstract;
    function ReadJSON(out Parser: TJSONParser): boolean;
    function ReadFile(FilePath: string; out Stream: TStream): boolean; virtual; abstract;
    function FileExists(FilePath: string): boolean; virtual; abstract;
    function VolumeCount: Integer;
    function FindCoverFile: string;
    property VDir: TVirtualDirectory read GetVDir;
  public
    constructor Create(APath: string);
    destructor Destroy; override;
    function GetCover(Cover: TPicture): boolean; override;
    function Read(MangaBook: TMangaBook; Details: TMangaBook.PDetails = nil): boolean; override;
    function Read(const VolumePath: string; PageList: TPageList): boolean; override;
    function Read(const PagePath: string; Page: TPicture): boolean; override;
  end;

  { TDirReader }

  TDirReader = class(TGeneralReader)
  protected
    procedure ScanAll; override;
    procedure ScanDir(BasePath: string; APath: string);
    function ReadFile(FilePath: string; out Stream: TStream): boolean; override;
    function FileExists(FilePath: string): boolean; override;
  end;

  { TZipReader }

  TZipReader = class(TGeneralReader)
  private
    FZipFile: unzFile;
  protected
    procedure ScanAll; override;
    function ReadFile(FilePath: string; out Stream: TStream): boolean; override;
    function FileExists(FilePath: string): boolean; override;
  public
    constructor Create(APath: string);
    destructor Destroy; override;
  end;

  {$ifdef WINDOWS}

  { TRarReader }

  TRarReader = class(TGeneralReader)
  protected
    procedure ScanAll; override;
    function ReadFile(FilePath: string; out Stream: TStream): boolean; override;
    function FileExists(FilePath: string): boolean; override;
  end;

  {$endif}

  { TTarReader }

  TTarReader = class(TGeneralReader)
  private
    FTarFile: TTarArchive;
  protected
    procedure ScanAll; override;
    function ReadFile(FilePath: string; out Stream: TStream): boolean; override;
    function FileExists(FilePath: string): boolean; override;
  public
    constructor Create(APath: string);
    destructor Destroy; override;
  end;

  { TEPubReader }

  TEPubReader = class(TReader)

  end;

  { TMangaBookHelper }

  TMangaBookHelper = class helper for TMangaBook
  private
    function GetCover: TPicture;
    function GetReader: TReader;
  public
    property Reader: TReader read GetReader;
    property Cover: TPicture read GetCover;
    procedure Read;
    procedure Read(AReader: TReader);
    procedure Read(out Details: TMangaBook.TDetails);
    procedure Read(AReader: TReader; out Details: TMangaBook.TDetails);
    function ReadVolume(VolumePath: string): TMangaBook.TPageArray;
    function ReadVolume(AReader: TReader; VolumePath: string): TMangaBook.TPageArray;
    function ReadPage(PagePath: string): TPicture;
    function ReadPage(AReader: TReader; PagePath: string): TPicture;
  end;

  { TMangaDetailsLoader }

  TMangaDetailsLoader = class(TMangaManager.TReadBookWork)
  private
    FBook: TMangaBook;
  public
    constructor Create(Book: TMangaBook);
    function Execute: TMangaBook.TCoverDetails; override;
  end;

  { TMangaVolumeLoader }

  TMangaVolumeLoader = class(TMangaManager.TReadVolumeWork)
  private
    FBook: TMangaBook;
    FVolume: TMangaBook.PVolume;
  public
    constructor Create(Book: TMangaBook; Volume: TMangaBook.PVolume);
    function Execute: TMangaBook.TPageArray; override;
    property Volume: TMangaBook.PVolume read FVolume;
  end;


  { TMangaPageLoader }

  TMangaPageLoader = class(TMangaManager.TReadPageWork)
  private
    FBook: TMangaBook;
    FVolume: TMangaBook.PVolume;
    FPage: TMangaBook.PPage;
  public
    constructor Create(Book: TMangaBook; Volume: TMangaBook.PVolume; Page: TMangaBook.PPage);
    function Execute: TPicture; override;
    property Volume: TMangaBook.PVolume read FVolume; 
    property Page: TMangaBook.PPage read FPage;
  end;


implementation

uses LazUTF8, LazFileUtils, DateUtils, jsonscanner,
  {$ifdef WINDOWS}
  UnRar,
  {$endif}
  Manhuard.Strings, Manhuard.Helper.Picture;

function GetFirstDir(Path: string): string; inline;
var
  Position: SizeInt;
begin
  Position := Path.IndexOfAny(['/', '\']);
  if Position > 0 then Result := Path.Substring(0, Position) else Result := Path;
end;

function ConvertToSeriesState(AValue: Integer): TMangaBook.TSeriesState; inline;
begin
  try
    Result := TMangaBook.TSeriesState(AValue);
  except
    Result := ssUnknown;
  end;
end;

{ TJSONDataHelper }

function TJSONDataHelper.ReadString(Path: string; Default: string): string;
var
  Data: TJSONData;
begin
  Data := FindPath(Path);
  if Data = nil then Exit(Default);
  try
    Result := Data.AsString;
  except
    Result := Default;
  end;
end;

function TJSONDataHelper.ReadUInt(Path: string; Default: Integer): Integer;
var
  Data: TJSONData;
begin
  if Default < 0 then raise ERangeError.Create('Default value must be a positive integer');
  Data := FindPath(Path);
  if Data = nil then Exit(Default);
  try
    Result := Data.AsInteger;
    if Result < 0 then raise ERangeError.Create(EmptyStr);
  except
    Result := Default;
  end;
end;

function TJSONDataHelper.ReadStringArray(Path: string; Default: TStringArray): TStringArray;
var
  Data: TJSONData;
  Item: TJSONEnum;
begin
  Data := FindPath(Path);
  if (Data = nil) or (Data.JSONType <> jtArray) then Exit(Default);
  SetLength(Result, Data.Count);
  try
    for Item in TJSONArray(Data) do Result[Item.KeyNum] := Item.Value.AsString;
  except
    Result := Default;
  end;
end;

function TJSONDataHelper.ReadVolumeArray(Path: string): TMangaBook.TVolumeArray;
var
  Data: TJSONData;
  Item: TJSONEnum;
begin
  Data := FindPath(Path);
  if (Data = nil) or (Data.JSONType <> jtArray) then Exit(nil);
  SetLength(Result, Data.Count);
  try
    for Item in TJSONArray(Data) do
    begin
      with Result[Item.KeyNum] do
      begin
        Path := Item.Value.ReadString('path');
        PageCount := Item.Value.ReadUInt('pages');
      end;
    end;
  except
    Result := nil;
  end;
end;

function TJSONDataHelper.ReadArraySize(Path: string): SizeInt;
var
  Data: TJSONData;
begin
  Data := FindPath(Path);
  if (Data = nil) or (Data.JSONType <> jtArray) then Exit(0);
  Result := TJSONArray(Data).Count;
end;

{ TReader }

constructor TReader.Create(APath: string);
begin
  FPath := APath;
end;

destructor TReader.Destroy;
begin
  inherited Destroy;
end;

{ TGeneralReader }

function TGeneralReader.GetVDir: TVirtualDirectory;
begin
  if not FScanned then
  begin
    ScanAll;
    FScanned := True;
  end;
  Result := FVDir;
end;

procedure TGeneralReader.AddFile(FilePath: string; Size: Int64);
begin
  FVDir.AddFile(FilePath, Size);
end;

procedure TGeneralReader.ParseJSON(JSONData: TJSONData; MangaBook: TMangaBook; Details: TMangaBook.PDetails);
var
  LastUpdated, OriginalRunFrom, OriginalRunTo: TDateTime;
begin
  if JSONData = nil then Exit;
  MangaBook.Title := JSONData.ReadString('name');
  MangaBook.Volumes := JSONData.ReadUInt('volumes');
  MangaBook.Region := JSONData.ReadString('region');
  MangaBook.Writers := JSONData.ReadStringArray('writers');
  MangaBook.Genre := JSONData.ReadStringArray('genre');
  MangaBook.ReleaseYear := JSONData.ReadUInt('releaseYear');
  TryISOStrToDate(JSONData.ReadString('lastUpdated'), LastUpdated);
  TryISOStrToDate(JSONData.ReadString('originalRun[0]'), OriginalRunFrom);
  TryISOStrToDate(JSONData.ReadString('originalRun[1]'), OriginalRunTo);
  MangaBook.OriginalRunFrom := OriginalRunFrom;
  MangaBook.OriginalRunTo := OriginalRunTo;
  MangaBook.LastUpdated := LastUpdated;
  MangaBook.SeriesState := ConvertToSeriesState(JSONData.ReadUInt('state'));
  if Details = nil then
  begin
    if MangaBook.Volumes = 0 then MangaBook.Volumes := JSONData.ReadArraySize('toc');
    Exit;
  end;
  Details^.Plot := JSONData.ReadString('plot');
  Details^.Source := JSONData.ReadString('source');
  Details^.Volumes := JSONData.ReadVolumeArray('toc');
  if MangaBook.Volumes = 0 then MangaBook.Volumes := Length(Details^.Volumes);
end;

function TGeneralReader.ReadJSON(out Parser: TJSONParser): boolean;
var
  Stream: TStream;
begin
  if not ReadFile(MANGA_BOOK_JSON_FILE_NAME, Stream) then Exit(False);
  Stream.Position := 0;
  try
    Parser := TJSONParser.Create(Stream, [joUTF8, joComments, joIgnoreTrailingComma, joIgnoreDuplicates]);
  finally
    Stream.Free;
  end;
  Result := True;
end;

function TGeneralReader.VolumeCount: Integer;
begin
  Result := VDir.LeafNodeCount;
end;

function TGeneralReader.FindCoverFile: string;
var
  Parser: TJSONParser;
  JSONData: TJSONData;
begin
  if ReadJSON(Parser) then
  begin
    try
      JSONData := Parser.Parse;
      if JSONData <> nil then
      begin
        Result := JSONData.ReadString('cover');
        if Result <> EmptyStr then Exit;
      end;
    finally
      Parser.Free;
    end;
  end;
  for Result in ['cover.png', 'cover.jpg', 'cover.jpeg', 'cover.jxl', 'cover.webp', 'cover.avif'] do if FileExists(Result) then Exit;
  Result := EmptyStr;
end;

constructor TGeneralReader.Create(APath: string);
begin
  inherited Create(APath);
  FVDir := TVirtualDirectory.Create;
  FScanned := False;
end;

destructor TGeneralReader.Destroy;
begin
  FVDir.Free;
  inherited Destroy;
end;

function TGeneralReader.GetCover(Cover: TPicture): boolean;
var
  CoverFile: String;
  Stream: TStream;
begin
  CoverFile := FindCoverFile;
  if CoverFile = EmptyStr then Exit(False);
  if not ReadFile(CoverFile, Stream) then Exit(False);
  try
    try
      Cover.Load(Stream);
    except
      on E: EPictureError do Exit(False);
    end;
  finally
    Stream.Free;
  end;
  Result := True;
end;

function TGeneralReader.Read(MangaBook: TMangaBook; Details: TMangaBook.PDetails): boolean;
var
  Parser: TJSONParser;
  LeafNodeList: TVirtualDirectory.TRefDirList;
  i: Integer;
  NoData: Boolean = False;
begin
  if ReadJSON(Parser) then
  begin
    try
      ParseJSON(Parser.Parse, MangaBook, Details);
    finally
      Parser.Free;
    end;
  end else NoData := True;
  if Details = nil then
  begin
    if MangaBook.Volumes = 0 then MangaBook.Volumes := VolumeCount;
  end
  else if Details^.Volumes = nil then
  begin
    LeafNodeList := TVirtualDirectory.TRefDirList.Create;
    try
      MangaBook.Volumes := VDir.FindLeafNodes(LeafNodeList);
      SetLength(Details^.Volumes, MangaBook.Volumes);
      for i := 0 to MangaBook.Volumes - 1 do
      begin
        with LeafNodeList[i] do
        begin
          Details^.Volumes[i].Path := Path;
          Details^.Volumes[i].PageCount := Files.Count;
        end;
      end;
    finally
      LeafNodeList.Free;
    end;
  end;
  Result := (not NoData) or (MangaBook.Volumes > 0);
end;

function TGeneralReader.Read(const VolumePath: string; PageList: TPageList): boolean;
var
  VolumeDir: TVirtualDirectory;
  FileItem: TVirtualDirectory.TFile;
begin
  VolumeDir := VDir.Find(VolumePath);
  if VolumeDir = nil then Exit(False);
  for FileItem in VolumeDir.Files do PageList.Add(FileItem);
  Result := True;
end;

function TGeneralReader.Read(const PagePath: string; Page: TPicture): boolean;
var
  Stream: TStream;
begin
  if not ReadFile(PagePath, Stream) then Exit(False);
  try
    Page.Load(Stream);
  finally
    Stream.Free;
  end;
  Result := True;
end;

{ TGeneralReader.TVirtualDirectory }

function TGeneralReader.TVirtualDirectory.FindSubdir(SubdirName: string): TVirtualDirectory;
var
  Subdir: TVirtualDirectory;
begin
  for Subdir in FDirList do if Subdir.Name = SubdirName then Exit(Subdir);
  Result := nil;
end;

function TGeneralReader.TVirtualDirectory.FindOrAddSubdir(SubdirName: string): TVirtualDirectory;
var
  Subdir: TVirtualDirectory;
begin
  for Subdir in FDirList do if Subdir.Name = SubdirName then Exit(Subdir);
  Result := TVirtualDirectory.Create(specialize IfThen<string>(FPath = EmptyStr, SubdirName, string.Join('/', [FPath, SubdirName])));
  FDirList.Add(Result);
end;

function TGeneralReader.TVirtualDirectory.GetName: string;
begin
  Result := ExtractFileName(FPath);
end;

function TGeneralReader.TVirtualDirectory.FindOrAdd(APath: string): TVirtualDirectory;
var
  CurrentPath, TailPath: String;
begin
  if APath = FPath then Exit(Self);
  CurrentPath := specialize IfThen<string>(FPath = EmptyStr, EmptyStr, FPath + '/');
  if not APath.StartsWith(CurrentPath) then raise EDirectoryNotFoundException.Create(CurrentPath);
  TailPath := APath.Substring(Length(CurrentPath));
  Result := FindOrAddSubdir(GetFirstDir(TailPath)).FindOrAdd(APath);
end;

function TGeneralReader.TVirtualDirectory.Find(APath: string): TVirtualDirectory;
var
  CurrentPath, TailPath: String;
  SubItem: TVirtualDirectory;
begin
  if APath = FPath then Exit(Self);
  CurrentPath := specialize IfThen<string>(FPath = EmptyStr, EmptyStr, FPath + '/');
  if not APath.StartsWith(CurrentPath) then Exit(nil);
  TailPath := APath.Substring(Length(CurrentPath));
  SubItem := FindSubdir(GetFirstDir(TailPath));
  if Assigned(SubItem) then
    Result := SubItem.Find(APath)
  else
    Result := nil;
end;

constructor TGeneralReader.TVirtualDirectory.Create(Path: string);
begin
  FPath := Path;
  FDirList := TDirList.Create;
  FFileList := TFileList.Create;
  FCachedDir := Self;
end;

destructor TGeneralReader.TVirtualDirectory.Destroy;
begin
  FFileList.Free;
  FDirList.Free;
  inherited Destroy;
end;

procedure TGeneralReader.TVirtualDirectory.AddFile(FilePath: string; Size: Int64);
var
  DirPath, Filename: String;
  FileItem: TFile;
begin
  DirPath := ExtractFileDir(FilePath);
  Filename := ExtractFileName(FilePath);
  if DirPath <> FCachedDirPath then
  begin
    FCachedDir := FindOrAdd(DirPath);
    FCachedDirPath := DirPath;
  end;
  FileItem.Name := Filename;
  FileItem.Size := Size;
  FCachedDir.FFileList.Add(FileItem);
end;

function TGeneralReader.TVirtualDirectory.LeafNodeCount: Integer;
var
  Subdir: TVirtualDirectory;
begin
  Result := 0;
  if FDirList.Count = 0 then Exit(1);
  for Subdir in FDirList do Inc(Result, Subdir.LeafNodeCount);
end;

function TGeneralReader.TVirtualDirectory.FindLeafNodes(LeafNodeList: TRefDirList): Integer;
var
  Subdir: TVirtualDirectory;
begin
  Result := 0;
  if FDirList.Count = 0 then
  begin
    LeafNodeList.Add(Self);
    Exit(1);
  end;
  for Subdir in FDirList do Inc(Result, Subdir.FindLeafNodes(LeafNodeList));
end;

{ TDirReader }

procedure TDirReader.ScanAll;
begin
  ScanDir(FPath, EmptyStr);
end;

procedure TDirReader.ScanDir(BasePath: string; APath: string);
var
  SearchRec: TSearchRec;
  SubItem: string;
begin
  if FindFirst(ConcatPaths([BasePath, APath, '*']), faAnyFile, SearchRec) <> 0 then Exit;
  repeat
    {$warn 5044 off}
    if string(SearchRec.Name).StartsWith('.'){$ifdef WINDOWS} or (faHidden and SearchRec.Attr > 0){$endif} then continue;
    {$warn 5044 on}
    SubItem := specialize IfThen<string>(APath = EmptyStr, SearchRec.Name, ConcatPaths([APath, SearchRec.Name]));
    if faDirectory and SearchRec.Attr = 0 then
      AddFile(SubItem.Replace(DirectorySeparator, '/'), SearchRec.Size)
    else
      ScanDir(BasePath, SubItem);
  until FindNext(SearchRec) <> 0;
  FindClose(SearchRec);
end;

function TDirReader.ReadFile(FilePath: string; out Stream: TStream): boolean;
var
  FileFullPath: string;
begin
  FileFullPath := ConcatPaths([FPath, FilePath]);
  Result := SysUtils.FileExists(FileFullPath);
  if Result then Stream := TFileStream.Create(FileFullPath, fmOpenRead or fmShareDenyWrite);
end;

function TDirReader.FileExists(FilePath: string): boolean;
begin
  Result := SysUtils.FileExists(ConcatPaths([FPath, FilePath]));
end;

{ TZipReader }

procedure TZipReader.ScanAll;
var
  UnzFileInfo: unz_file_info;
  Filename: shortstring;
begin
  if unzGoToFirstFile(FZipFile) = UNZ_OK then
  begin
    repeat
      unzGetCurrentFileInfo(FZipFile, @UnzFileInfo, @Filename[1], Sizeof(Filename) - 1, nil, 0, nil, 0);
      Filename[0] := Char(UnzFileInfo.size_filename);
      if (UnzFileInfo.crc <> 0) and (Filename[UnzFileInfo.size_filename - 1] <> '/') then
        AddFile(WinCPToUTF8(Filename), UnzFileInfo.uncompressed_size);
    until unzGoToNextFile(FZipFile) <> UNZ_OK;
  end;
end;

function TZipReader.ReadFile(FilePath: string; out Stream: TStream): boolean;
var
  UnzFileInfo: unz_file_info;
  FileName: shortstring;
begin
  if (unzLocateFile(FZipFile, PChar(UTF8ToWinCP(FilePath)), 2) <> UNZ_OK) or
     (unzGetCurrentFileInfo(FZipFile, @UnzFileInfo, @FileName[1], Sizeof(FileName) - 1, nil, 0, nil, 0) <> UNZ_OK) or
     (unzOpenCurrentFile(FZipFile) <> UNZ_OK) then Exit(False);
  try
    Stream := TMemoryStream.Create;
    Stream.Size := UnzFileInfo.uncompressed_size;
    unzReadCurrentFile(FZipFile, TMemoryStream(Stream).Memory, Stream.Size);
  finally
    unzCloseCurrentFile(FZipFile);
  end;
  Result := True;
end;

function TZipReader.FileExists(FilePath: string): boolean;
begin
  Result := unzLocateFile(FZipFile, PChar(FilePath), 2) = UNZ_OK;
end;

constructor TZipReader.Create(APath: string);
begin
  inherited;
  FZipFile := unzOpen(PChar(APath));
end;

destructor TZipReader.Destroy;
begin
  unzClose(FZipFile);
  inherited Destroy;
end;

{$ifdef WINDOWS}

function UnrarCallback(Msg: UInt32 ; UserData, P1, P2: PtrInt): Integer; stdcall;
var
  Stream: TMemoryStream;
begin
  if Msg <> UCM_PROCESSDATA then Exit(1);
  Stream := TMemoryStream(UserData);
  Stream.Write(PByte(P1)^, P2);
  Result := 1;
end;

{ TRarReader }

procedure TRarReader.ScanAll;
var                         
  RarFile: THANDLE;
  OpenData: TRAROpenArchiveData;
  HeaderData: TRARHeaderData;
  FilePath: string;
begin
  HeaderData := Default(TRARHeaderData);
  OpenData := Default(TRAROpenArchiveData);
  OpenData.ArcName := PChar(UTF8ToWinCP(Path));
  OpenData.OpenMode := RAR_OM_LIST;
  RarFile := RAROpenArchive(@OpenData);
  if RarFile = 0 then raise EFOpenError.CreateFmt('Unable to open file "%s"', [Path]);
  try
    repeat
      if RARReadHeader(RarFile, @HeaderData) > 0 then break;
      if (RHDF_DIRECTORY and HeaderData.Flags) > 0 then continue;
      FilePath := WinCPToUTF8(HeaderData.FileName).Replace('\', '/');
      AddFile(FilePath, HeaderData.UnpSize);
    until RARProcessFile(RarFile, RAR_SKIP, nil, nil) > 0;
  finally
    RARCloseArchive(RarFile);
  end;
end;

function TRarReader.ReadFile(FilePath: string; out Stream: TStream): boolean;
var
  HeaderData: TRARHeaderData;
  OpenData: TRAROpenArchiveData;
  RarFile: THANDLE;
  AFilePath: String;
begin
  HeaderData := Default(TRARHeaderData);
  OpenData := Default(TRAROpenArchiveData);
  OpenData.ArcName := PChar(UTF8ToWinCP(Path));
  OpenData.OpenMode := RAR_OM_EXTRACT;
  RarFile := RAROpenArchive(@OpenData);
  if RarFile = 0 then raise EFOpenError.CreateFmt('Unable to open file "%s"', [Path]);
  try
    while RARReadHeader(RarFile, @HeaderData) = 0 do
    begin
      AFilePath := WinCPToUTF8(HeaderData.FileName).Replace('\', '/');
      if AFilePath = FilePath then
      begin
        if (RHDF_DIRECTORY and HeaderData.Flags) > 0 then break;
        Stream := TMemoryStream.Create;
        RARSetCallback(RarFile, @UnrarCallback, PtrInt(Stream));
        if RARProcessFile(RarFile, RAR_TEST, nil, nil) > 0 then
        begin
          FreeAndNil(Stream);
          break;
        end;
        Exit(True);
      end;
      if RARProcessFile(RarFile, RAR_SKIP, nil, nil) > 0 then break;
    end;
  finally
    RARCloseArchive(RarFile);
  end;
  Result := False;
end;

function TRarReader.FileExists(FilePath: string): boolean;
var
  APath, AFileName: String;
  Dir: TVirtualDirectory;
  FileItem: TVirtualDirectory.TFile;
begin
  APath := ExtractFilePath(FilePath);
  AFileName := ExtractFileName(FilePath);
  Dir := VDir.Find(APath);
  if Dir = nil then Exit(False);
  for FileItem in Dir.Files do if FileItem.Name = AFileName then Exit(True);
  Result := False;
end;

{$endif}

{ TTarReader }

procedure TTarReader.ScanAll;
var
  DirRec: TTarDirRec;
begin
  while FTarFile.FindNext(DirRec) do if DirRec.FileType = ftNormal then AddFile(DirRec.Name, DirRec.Size);
  FTarFile.Reset;
end;

function TTarReader.ReadFile(FilePath: string; out Stream: TStream): boolean;
var
  DirRec: TTarDirRec;
begin
  try
    while FTarFile.FindNext(DirRec) do
    begin
      if DirRec.Name = FilePath then
      begin
        Stream := TMemoryStream.Create;
        FTarFile.ReadFile(Stream);
        Exit(True);
      end;
    end;
  finally
    FTarFile.Reset;
  end;
  Result := False;
end;

function TTarReader.FileExists(FilePath: string): boolean;
var
  DirRec: TTarDirRec;
begin
  try
    while FTarFile.FindNext(DirRec) do if DirRec.Name = FilePath then Exit(True);
  finally
    FTarFile.Reset;
  end;
  Result := False;
end;

constructor TTarReader.Create(APath: string);
begin
  inherited;
  FTarFile := TTarArchive.Create(APath);
end;

destructor TTarReader.Destroy;
begin
  FTarFile.Free;
  inherited Destroy;
end;

{ TMangaBookHelper }

function TMangaBookHelper.GetCover: TPicture;
var
  AReader: TReader;
begin
  AReader := Reader;
  try
    Result := TPicture.Create;
    if not AReader.GetCover(Result) then FreeAndNil(Result);
  finally
    AReader.Free;
  end;
end;

function TMangaBookHelper.GetReader: TReader;
begin
  case PackageType of
    mptDir: Result := TDirReader.Create(Path);
    mptZip: Result := TZipReader.Create(Path);
    mptTar: Result := TTarReader.Create(Path);
    {$ifdef WINDOWS}
    mptRar: Result := TRarReader.Create(Path);
    {$endif}
    mptEPub: Result := TEPubReader.Create(Path);
  else
    raise EArgumentOutOfRangeException.Create('Not implemented');
  end;
end;

procedure TMangaBookHelper.Read;
var
  AReader: TReader;
begin
  AReader := Reader;
  try
    Read(AReader);
  finally
    AReader.Free;
  end;
end;

procedure TMangaBookHelper.Read(AReader: TReader);
begin
  AReader.Read(Self);
end;

procedure TMangaBookHelper.Read(out Details: TMangaBook.TDetails);
var
  AReader: TReader;
begin
  AReader := Reader;
  try
    Read(AReader, Details);
  finally
    AReader.Free;
  end;
end;

procedure TMangaBookHelper.Read(AReader: TReader; out Details: TMangaBook.TDetails);
var
  ADetails: TMangaBook.TDetails;
begin
  AReader.Read(Self, @ADetails);
  Details := ADetails;
end;

function TMangaBookHelper.ReadVolume(VolumePath: string): TMangaBook.TPageArray;
var
  AReader: TReader;
begin
  AReader := Reader;
  try
    Result := ReadVolume(AReader, VolumePath);
  finally
    AReader.Free;
  end;
end;

function TMangaBookHelper.ReadVolume(AReader: TReader; VolumePath: string): TMangaBook.TPageArray;
var
  PageList: TPageList;
begin
  PageList := TPageList.Create;
  try
    if not AReader.Read(VolumePath, PageList) then raise EMangaReaderError.CreateFmt(MSG_FAILED_TO_READ_VOLUME, [VolumePath]);
    Result := PageList.ToArray;
  finally
    PageList.Free;
  end;
end;

function TMangaBookHelper.ReadPage(PagePath: string): TPicture;
var
  AReader: TReader;
begin
  AReader := Reader;
  try
    Result := ReadPage(AReader, PagePath);
  finally
    AReader.Free;
  end;
end;

function TMangaBookHelper.ReadPage(AReader: TReader; PagePath: string): TPicture;
begin
  Result := TPicture.Create;
  if not AReader.Read(PagePath, Result) then
  begin
    FreeAndNil(Result);
    raise EMangaReaderError.CreateFmt(MSG_FAILED_TO_READ_PAGE, [PagePath]);
  end;
end;

{ TMangaDetailsLoader }

constructor TMangaDetailsLoader.Create(Book: TMangaBook);
begin
  inherited Create;
  FBook := Book;
end;

function TMangaDetailsLoader.Execute: TMangaBook.TCoverDetails;
begin
  FBook.Read(Result.Details);
  Result.Cover := FBook.Cover;
end;

{ TMangaVolumeLoader }

constructor TMangaVolumeLoader.Create(Book: TMangaBook; Volume: TMangaBook.PVolume);
begin
  inherited Create;
  FBook := Book;
  FVolume := Volume;
end;

function TMangaVolumeLoader.Execute: TMangaBook.TPageArray;
begin
  Result := FBook.ReadVolume(FVolume^.Path);
end;

{ TMangaPageLoader }

constructor TMangaPageLoader.Create(Book: TMangaBook; Volume: TMangaBook.PVolume; Page: TMangaBook.PPage);
begin
  inherited Create;
  FBook := Book;
  FVolume := Volume;
  FPage := Page;
end;

function TMangaPageLoader.Execute: TPicture;
var
  PagePath: String;
begin
  PagePath := FVolume^.Path + '/' + FPage^.Name;
  Result := FBook.ReadPage(PagePath);
end;


end.

