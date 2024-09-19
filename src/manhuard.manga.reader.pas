unit Manhuard.Manga.Reader;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fpjson, jsonparser, Generics.Collections, Unzip, ZipUtils, LibTar,
  Manhuard.Manga, Manhuard.Helper.ListView;

const
  MANGA_PACKAGE_TYPES : array[mptZip..mptEPub] of string = ('.cbz', '.cba', '.cbr', '.epub');
  MANGA_BOOK_JSON_FILE_NAME = 'book.json';

type

  TMangaBookCoverResolutionType = (birtSmall, birtLarge);
  TMangaBookCoverManager = specialize TListViewIconManager<TMangaBookCoverResolutionType>;

  { TMangaCoverLoader }

  TMangaCoverLoader = class(TMangaBookCoverManager.TLoadIconsWork)
  type
    TIndexBookPair = specialize TPair<Integer, TMangaBook>;
    TIndexBookPairArray = array of TIndexBookPair;
  private
    FIndexBookPairArray: TIndexBookPairArray;
    FManager: TMangaBookCoverManager;
  public
    constructor Create(Manager: TMangaBookCoverManager; IndexBookPairArray: TIndexBookPairArray);
    function Execute: TMangaBookCoverManager.TIndexIconPairArray; override;
  end;

  { TMangaDetailsLoader }

  TMangaDetailsLoader = class(TMangaManager.TReadBookWork)
  private
    FBook: TMangaBook;
  public
    constructor Create(Book: TMangaBook);
    function Execute: TMangaBook.TDetails; override;
  end;

  { TMangaVolumeLoader }

  TMangaVolumeLoader = class(TMangaManager.TReadVolumeWork)
  private
    FBook: TMangaBook;
    FVolume: TMangaBook.PVolume;
  public
    constructor Create(Book: TMangaBook; Volume: TMangaBook.PVolume);
    function Execute: TMangaBook.TPageArray; override;
  end;

  TVolumeList = specialize TList<TMangaBook.TVolume>;

  { TVirtualDirectory }

  TVirtualDirectory = class
  type
    TDirList = specialize TObjectList<TVirtualDirectory>;
  private
    FPath: string;
    FDirList: TDirList;
    FFileCount: Integer;
    FCachedDirPath: string;
    FCachedDir: TVirtualDirectory;
    function GetItem(SubdirName: string): TVirtualDirectory;
    function GetName: string;
    function FindOrAdd(APath: string): TVirtualDirectory;
  public
    constructor Create(Path: string = '');
    destructor Destroy; override;
    property Path: string read FPath;
    property Item[SubdirName: string]: TVirtualDirectory read GetItem; default;
    property Name: string read GetName;
    function FindVolumes(VolumeList: TVolumeList): Integer;
    function VolumeCount: Integer;
    procedure AddFile(FilePath: string);
  end;

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
    function GetDir: TVirtualDirectory;
  protected
    FPath: string;
    FVDir: TVirtualDirectory;
    FLoaded: boolean;
    procedure LoadFiles; virtual; abstract;
  public
    constructor Create(Path: string);
    destructor Destroy; override;
    function GetFile(FilePath: string; out Stream: TStream): boolean; virtual; abstract;
    function FileExists(FilePath: string): boolean; virtual; abstract;
    property Dir: TVirtualDirectory read GetDir;
  end;

  { TMangaBookHelper }

  TMangaBookHelper = class helper for TMangaBook
  private
    function GetReader: TReader;
    function OpenMetaFile(AReader: TReader; out Parser: TJSONParser): boolean;
    function DefaultName: string;
    procedure ReadFromMetaFile(AReader: TReader; JSONData: TJSONData; Details: TMangaBook.PDetails);
  public
    property Reader: TReader read GetReader;
    procedure Read;
    procedure Read(AReader: TReader);
    procedure Read(out Details: TMangaBook.TDetails);
    procedure Read(AReader: TReader; out Details: TMangaBook.TDetails);
    //procedure Read(VolumePath: string; out PageArray: TMangaBook.TPageArray);
    //procedure Read(FilePath: string; Picture: TPicture);
  end;

  { TDirReader }

  TDirReader = class(TReader)
  public
    function GetFile(FilePath: string; out Stream: TStream): boolean; override;
    function FileExists(FilePath: string): boolean; override;
  protected
    procedure LoadFiles; override;
  end;

  { TZipReader }

  TZipReader = class(TReader)
  private
    FZipFile: unzFile;
  public
    constructor Create(Path: string);
    destructor Destroy; override;
    function GetFile(FilePath: string; out Stream: TStream): boolean; override;
    function FileExists(FilePath: string): boolean; override;
  protected
    procedure LoadFiles; override;
  end;

  { TRarReader }

  TRarReader = class(TReader)

  end;

  { TTarReader }

  TTarReader = class(TReader)

  end;

  { TEPubReader }

  TEPubReader = class(TReader)

  end;


implementation

uses LazUTF8, LazFileUtils, DateUtils, jsonscanner, Manhuard.Manga.Loader, Manhuard.Helper.Picture;

procedure LoadFilesFromDir(BasePath: string; Path: string; VDir: TVirtualDirectory);
var
  SearchRec: TSearchRec;
  SubItem: string;
begin
  if FindFirst(ConcatPaths([BasePath, Path, '*']), faAnyFile, SearchRec) <> 0 then Exit;
  repeat
    {$warn 5044 off}
    if string(SearchRec.Name).StartsWith('.'){$ifdef Windows} or (faHidden and SearchRec.Attr > 0){$endif} then continue;
    {$warn 5044 on}
    SubItem := ConcatPaths([Path, SearchRec.Name]);
    if faDirectory and SearchRec.Attr = 0 then
      VDir.AddFile(SubItem)
    else
      LoadFilesFromDir(BasePath, SubItem, VDir);
  until FindNext(SearchRec) <> 0;
  FindClose(SearchRec);
end;

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

{function FindVolumesFromDirectory(BasePath, Path: string; VolumeList: TVolumeList; FileCount: PInteger = nil): Integer;
var
  SearchRec: TSearchRec;
  SubdirCount, TheFileCount, SubFileCount: Integer;
  SubdirPath: string;
  Volume: TMangaBook.TVolume;
begin
  Result := 0;
  if FileCount = nil then FileCount := @TheFileCount;
  if FindFirst(ConcatPaths([BasePath, Path, '*']), faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if string(SearchRec.Name).StartsWith('.')
      {$warn 5044 off}
      {$ifdef Windows} or (faHidden and SearchRec.Attr > 0){$endif} then continue;
      {$warn 5044 on}
      if faDirectory and SearchRec.Attr = 0 then
      begin
        Inc(FileCount^);
        continue;
      end;
      SubdirPath := ConcatPaths([Path, SearchRec.Name]);
      SubFileCount := 0;
      SubdirCount := FindVolumesFromDirectory(BasePath, SubdirPath, VolumeList, @SubFileCount);
      if SubdirCount > 0 then
        Inc(Result, SubdirCount)
      else
      begin
        Inc(Result, 1);
        if VolumeList <> nil then
        begin
          Volume.Path := SubdirPath.TrimLeft(['/', '\']).Replace('\', '/');
          Volume.PageCount := SubFileCount;
          VolumeList.Add(Volume);
        end;
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

procedure ReadVolumeFromDirectory(Path: string; var PageArray: TMangaBook.TPageArray);
var
  SearchRec: TSearchrec;
  PageCount: Integer = 0;
  PageArrayLength: Integer;
begin
  PageArrayLength := Length(PageArray);
  if FindFirst(ConcatPaths([Path, '*']), faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if string(SearchRec.Name).StartsWith('.') or (faDirectory and SearchRec.Attr > 0)
      {$warn 5044 off}
      {$ifdef Windows} or (faHidden and SearchRec.Attr > 0){$endif} then continue;
      {$warn 5044 on}
      Inc(PageCount);
      if PageCount > PageArrayLength then SetLength(PageArray, PageCount);
      PageArray[PageCount - 1].Name := SearchRec.Name;
      PageArray[PageCount - 1].Size := SearchRec.Size;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;}

{ TMangaCoverLoader }

constructor TMangaCoverLoader.Create(Manager: TMangaBookCoverManager; IndexBookPairArray: TIndexBookPairArray);
begin
  FManager := Manager;
  FIndexBookPairArray := IndexBookPairArray;
end;

function TMangaCoverLoader.Execute: TMangaBookCoverManager.TIndexIconPairArray;
var
  IndexBook: TIndexBookPair;
  Book: TMangaBook;
  Cover, LargeCover, SmallCover: TPicture;
  i: Integer;
  Size: SizeInt;
begin
  Result := [];
  Size := Length(FIndexBookPairArray);
  SetLength(Result, Size);
  for i := 0 to Size - 1 do
  begin
    IndexBook := FIndexBookPairArray[i];
    Cover := TPicture.Create;
    try
      Book := IndexBook.Value;
      //Book.Read(Book.Cover, Cover);
      { todo: }
      Cover.LoadFromResourceName(HInstance, 'NO_COVER_LARGE');
      SmallCover := TPicture.Create;
      SmallCover.Assign(Cover);
      with FManager.Resolutions[birtSmall] do SmallCover.Resize(Width, Height);
      LargeCover := TPicture.Create;
      LargeCover.Assign(Cover);
      with FManager.Resolutions[birtLarge] do LargeCover.Resize(Width, Height);
      Result[i].Key := IndexBook.Key;
      Result[i].Value[birtSmall] := SmallCover;
      Result[i].Value[birtLarge] := LargeCover;
    finally
      Cover.Free;
    end;
  end;
end;

{ TMangaDetailsLoader }

constructor TMangaDetailsLoader.Create(Book: TMangaBook);
begin
  FBook := Book;
end;

function TMangaDetailsLoader.Execute: TMangaBook.TDetails;
begin
  FBook.Read(Result);
end;

{ TMangaVolumeLoader }

constructor TMangaVolumeLoader.Create(Book: TMangaBook; Volume: TMangaBook.PVolume);
begin
  FBook := Book;
  FVolume := Volume;
end;

function TMangaVolumeLoader.Execute: TMangaBook.TPageArray;
begin
  Result := [];
  SetLength(Result, FVolume^.PageCount);
  //FBook.Read(FVolume^.Path, Result);
  FVolume^.PageCount := Length(Result);
end;

{ TVirtualDirectory }

function TVirtualDirectory.GetItem(SubdirName: string): TVirtualDirectory;
var
  Subdir: TVirtualDirectory;
begin
  for Subdir in FDirList do if Subdir.Name = SubdirName then Exit(Subdir);
  Result := TVirtualDirectory.Create(specialize IfThen<string>(FPath = EmptyStr, SubdirName, ConcatPaths([FPath, SubdirName])));
  FDirList.Add(Result);
end;

function TVirtualDirectory.GetName: string;
begin
  Result := ExtractFileName(FPath);
end;

constructor TVirtualDirectory.Create(Path: string);
begin
  FFileCount := 0;
  FPath := Path;
  FDirList := TDirList.Create;
  FCachedDir := Self;
end;

destructor TVirtualDirectory.Destroy;
begin
  FDirList.Free;
end;

function TVirtualDirectory.FindOrAdd(APath: string): TVirtualDirectory;
var
  TailPath, CurrentPath: String;
begin
  if APath = FPath then Exit(Self);
  CurrentPath := specialize IfThen<string>(FPath = EmptyStr, EmptyStr, FPath + DirectorySeparator);
  if not APath.StartsWith(CurrentPath) then Exit(nil);
  TailPath := APath.Substring(Length(CurrentPath));
  Result := GetItem(GetFirstDir(TailPath)).FindOrAdd(APath);
end;

function TVirtualDirectory.FindVolumes(VolumeList: TVolumeList): Integer;
var
  Subdir: TVirtualDirectory;
  Volume: TMangaBook.TVolume;
begin
  if FDirList.Count = 0 then
  begin
    Volume.Path := FPath;
    Volume.PageCount := FFileCount;
    VolumeList.Add(Volume);
    Exit(1);
  end;
  Result := 0;
  for Subdir in FDirList do Inc(Result, Subdir.FindVolumes(VolumeList));
end;

function TVirtualDirectory.VolumeCount: Integer;
var
  Subdir: TVirtualDirectory;
begin
  Result := 0;
  if FDirList.Count = 0 then Exit(1);
  for Subdir in FDirList do Inc(Result, Subdir.VolumeCount);
end;

procedure TVirtualDirectory.AddFile(FilePath: string);
var
  DirPath: String;
begin
  DirPath := ExtractFileDir(FilePath);
  if DirPath = FCachedDirPath then
  begin
    Inc(FCachedDir.FFileCount);
    Exit;
  end;
  FCachedDir := FindOrAdd(DirPath);
  Inc(FCachedDir.FFileCount);
  FCachedDirPath := DirPath;
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

{ TMangaBookHelper }

function TMangaBookHelper.GetReader: TReader;
begin
  case PackageType of
    mptDir: Result := TDirReader.Create(Path);
    mptZip: Result := TZipReader.Create(Path);
    mptTar: Result := TTarReader.Create(Path);
    mptRar: Result := TRarReader.Create(Path);
    mptEPub: Result := TEPubReader.Create(Path);
  else
    raise EArgumentOutOfRangeException.Create('Not implemented');
  end;
end;

function TMangaBookHelper.OpenMetaFile(AReader: TReader; out Parser: TJSONParser): boolean;
var
  Stream: TStream;
begin
  Result := AReader.GetFile(MANGA_BOOK_JSON_FILE_NAME, Stream);
  if not Result then Exit(False);
  try
    Parser := TJSONParser.Create(Stream, [joUTF8, joComments, joIgnoreTrailingComma, joIgnoreDuplicates]);
  finally
    Stream.Free;
  end;
  Result := True;
end;

function TMangaBookHelper.DefaultName: string;
begin
  Result := specialize IfThen<string>(FPackageType = mptZip, ExtractFileNameOnly(FPath), ExtractFilename(FPath));
end;

procedure TMangaBookHelper.ReadFromMetaFile(AReader: TReader; JSONData: TJSONData; Details: TMangaBook.PDetails);

  function GetAnyFile(AReader: TReader; Paths: TStringArray): string;
  var
    Path: string;
  begin
    for Path in Paths do if AReader.FileExists(Path) then Exit(Path);
    Result := EmptyStr;
  end;

var
  CoverFilenames: TStringArray = ('cover.png', 'cover.jpg', 'cover.jpeg');
begin
  if JSONData = nil then
  begin
    FTitle := DefaultName;
    FVolumes := 0;
    FCover := GetAnyFile(AReader, CoverFilenames);
    Exit;
  end;
  FCover := JSONData.ReadString('cover');
  if FCover = EmptyStr then FCover := GetAnyFile(AReader, CoverFilenames);
  FTitle := JSONData.ReadString('name', DefaultName);
  FVolumes := JSONData.ReadUInt('volumes');
  FRegion := JSONData.ReadString('region');
  FWriters := JSONData.ReadStringArray('writers');
  FGenre := JSONData.ReadStringArray('genre');
  FReleaseYear := JSONData.ReadUInt('releaseYear');
  TryISOStrToDate(JSONData.ReadString('originalRun[0]'), FOriginalRun[rrFrom]);
  TryISOStrToDate(JSONData.ReadString('originalRun[1]'), FOriginalRun[rrTo]);
  TryISOStrToDate(JSONData.ReadString('lastUpdated'), FLastUpdated);
  FSeriesState := ConvertToSeriesState(JSONData.ReadUInt('state'));
  if Details = nil then
  begin
    if FVolumes = 0 then FVolumes := JSONData.ReadArraySize('toc');
    Exit;
  end;
  Details^.Plot := JSONData.ReadString('plot');
  Details^.Source := JSONData.ReadString('source');
  Details^.Volumes := JSONData.ReadVolumeArray('toc');
  if FVolumes = 0 then FVolumes := Length(Details^.Volumes);
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
var
  Parser: TJSONParser;
begin
  if not OpenMetaFile(AReader, Parser) then
    FTitle := DefaultName
  else
  begin
    try
      ReadFromMetaFile(AReader, Parser.Parse, nil);
    finally
      Parser.Free;
    end;
  end;
  if FVolumes = 0 then FVolumes := AReader.Dir.VolumeCount;
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
  Parser: TJSONParser;
  VolumeList: TVolumeList;
begin
  if not OpenMetaFile(AReader, Parser) then
    FTitle := DefaultName
  else
  begin
    try
      ReadFromMetaFile(AReader, Parser.Parse, @Details);
    finally
      Parser.Free;
    end;
  end;

  if (FVolumes = 0) or (Details.Volumes = nil) then
  begin
    VolumeList := TVolumeList.Create;
    try
      FVolumes := AReader.Dir.FindVolumes(VolumeList);
      Details.Volumes := VolumeList.ToArray;
    finally
      VolumeList.Free;
    end;
  end;
end;

{
procedure TMangaBookHelper.Read(VolumePath: string; out PageArray: TMangaBook.TPageArray);
begin
  case FPackageType of
    mptDir: ReadVolumeFromDirectory(ConcatPaths([FPath, VolumePath]), PageArray);
    mptZip: ;
  else
    raise EArgumentOutOfRangeException.Create('Not implemented');
  end;
end;}

{procedure TMangaBookHelper.Read(FilePath: string; Picture: TPicture);
var
  Ext: string;
  Stream: TStream;
  ZipFile: unzFile;
  UnzFileInfo: unz_file_info;
  FileName: shortstring;
begin
  Ext := ExtractFileExt(FilePath);
  Delete(Ext, 1, 1);

  case FPackageType of
    mptDir:
      begin
        try
          Stream := TFileStream.Create(ConcatPaths([FPath, FilePath]), fmOpenRead or fmShareDenyWrite);
        except
          Exit;
        end;
      end;
    mptZip:
      begin
        ZipFile := unzOpen(PChar(FPath));
        try
          if (unzLocateFile(ZipFile, PChar(FilePath), 2) <> UNZ_OK) or
             (unzGetCurrentFileInfo(ZipFile, @UnzFileInfo, @FileName[1], Sizeof(FileName) - 1, nil, 0, nil, 0) <> UNZ_OK) or
             (unzOpenCurrentFile(ZipFile) <> UNZ_OK) then Exit;
          try
            Stream := TMemoryStream.Create;
            Stream.Size := UnzFileInfo.uncompressed_size;
            unzReadCurrentFile(ZipFile, TMemoryStream(Stream).Memory, Stream.Size);
          finally
            unzCloseCurrentFile(ZipFile);
          end;
        finally
          unzClose(ZipFile);
        end;
      end;
  else
    raise EArgumentOutOfRangeException.Create('Not implemented');
  end;
  try
    if Ext <> '' then
      Picture.LoadFromStreamWithFileExt(Stream, Ext)
    else
      Picture.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;}

{ TReader }

function TReader.GetDir: TVirtualDirectory;
begin
  if not FLoaded then
  begin
    LoadFiles;
    FLoaded := True;
  end;
  Result := FVDir;
end;

constructor TReader.Create(Path: string);
begin
  FLoaded := False;
  FPath := Path;
  FVDir := TVirtualDirectory.Create;
end;

destructor TReader.Destroy;
begin
  FVDir.Free;
  inherited Destroy;
end;

{ TDirReader }

function TDirReader.GetFile(FilePath: string; out Stream: TStream): boolean;
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

procedure TDirReader.LoadFiles;
begin
  LoadFilesFromDir(FPath, '', FVDir);
end;

{ TZipReader }

constructor TZipReader.Create(Path: string);
begin
  inherited;
  FZipFile := unzOpen(PChar(Path));
end;

destructor TZipReader.Destroy;
begin
  unzClose(FZipFile);
  inherited Destroy;
end;

function TZipReader.GetFile(FilePath: string; out Stream: TStream): boolean;
var
  UnzFileInfo: unz_file_info;
  FileName: shortstring;
begin
  if (unzLocateFile(FZipFile, PChar(FilePath), 2) <> UNZ_OK) or
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

procedure TZipReader.LoadFiles;
var
  UnzFileInfo: unz_file_info;
  Filename: shortstring;
begin
  if unzGoToFirstFile(FZipFile) = UNZ_OK then
  begin
    repeat
      unzGetCurrentFileInfo(FZipFile, @UnzFileInfo, @Filename[1], Sizeof(Filename) - 1, nil, 0, nil, 0);
      Filename[0] := Char(UnzFileInfo.size_filename);
      FVDir.AddFile(WinCPToUTF8(Filename));
    until unzGoToNextFile(FZipFile) <> UNZ_OK;
  end;
end;

end.

