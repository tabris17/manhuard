unit Manhuard.Manga.Reader;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fpjson, jsonparser, Generics.Collections, Manhuard.Manga, Manhuard.Helper.ListView;

type

  TMangaBookCoverResolutionType = (birtSmall, birtLarge);
  TMangaBookCoverManager = specialize TListViewIconManager<TMangaBookCoverResolutionType>;

  { TMangaBookCoverLoader }

  TMangaBookCoverLoader = class(TMangaBookCoverManager.TLoadIconsWork)
  type
    TIndexBookPair = specialize TPair<Integer, TMangaBook>;
    TIndexBookPairArray = array of TIndexBookPair;
  private
    FIndexBookPairArray: TIndexBookPairArray;
  public
    constructor Create(IndexBookPairArray: TIndexBookPairArray);
    function Execute: TMangaBookCoverManager.TIndexIconPairArray; override;
  end;

  { TMangaDetailReader }

  TMangaDetailReader = class(TMangaManager.TReadBookWork)
  private
    FBook: TMangaBook;
  public
    constructor Create(Book: TMangaBook);
    function Execute: TMangaBook.TDetail; override;
  end;

  { TMangaVolumeReader }

  TMangaVolumeReader = class(TMangaManager.TReadVolumeWork)
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
    function FindVolumes(VolumeList: TVolumeList = nil): Integer;
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

  { TMangaBookReader }

  TMangaBookReader = class helper for TMangaBook
  type
    {TCover = class(TPicture)
    public
      procedure LoadFromBook(const Book: TMangaBook);
    end;}
  private
    function OpenJSON(out Stream: TStream; out Parser: TJSONParser): boolean;
    function DefaultName: string;
    function GetAnyFile(Paths: TStringArray): string;
    procedure FindVolumes(VolumeList: TVolumeList = nil);
    procedure ReadFromJSON(JSONData: TJSONData; Detail: TMangaBook.PDetail);
  public
    procedure Read;
    procedure Read(out Detail: TMangaBook.TDetail);
    procedure Read(VolumePath: string; out PageArray: TMangaBook.TPageArray);
    procedure Read(FilePath: string; Picture: TPicture);
  end;


implementation

uses LazUTF8, LazFileUtils, DateUtils, jsonscanner, Unzip, ZipUtils, Manhuard.Manga.Loader, Manhuard.Helper.Picture;

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

function FindVolumesFromDirectory(BasePath, Path: string; VolumeList: TVolumeList; FileCount: PInteger = nil): Integer;
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

function FindVolumesFromZipFile(Path: string; VolumeList: TVolumeList): Integer;
var
  ZipFile: unzFile;
  UnzFileInfo: unz_file_info;
  FileName: shortstring;
  VDir: TVirtualDirectory;
begin
  Result := 0;
  VDir := TVirtualDirectory.Create;
  try
    ZipFile := unzOpen(PChar(Path));
    try
      if unzGoToFirstFile(ZipFile) = UNZ_OK then
      begin
        repeat
          unzGetCurrentFileInfo(ZipFile, @UnzFileInfo, @Filename[1], Sizeof(Filename) - 1, nil, 0, nil, 0);
          Filename[0] := Char(UnzFileInfo.size_filename);
          VDir.AddFile(WinCPToUTF8(Filename));
        until unzGoToNextFile(ZipFile) <> UNZ_OK;
      end;
    finally
      unzClose(ZipFile);
    end;
    Result := VDir.FindVolumes(VolumeList);
  finally
    VDir.Free;
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
end;

{ TMangaBookCoverLoader }

constructor TMangaBookCoverLoader.Create(IndexBookPairArray: TIndexBookPairArray);
begin
  FIndexBookPairArray := IndexBookPairArray;
end;

function TMangaBookCoverLoader.Execute: TMangaBookCoverManager.TIndexIconPairArray;
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
      Book.Read(Book.Cover, Cover);
      SmallCover := TPicture.Create;
      SmallCover.Assign(Cover);
      ResizePicture(SmallCover, 60, 80);
      LargeCover := TPicture.Create;
      LargeCover.Assign(Cover);
      Result[i].Key := IndexBook.Key;
      Result[i].Value[birtSmall] := SmallCover;
      Result[i].Value[birtLarge] := LargeCover;
    finally
      Cover.Free;
    end;
  end;
end;

{ TMangaDetailReader }

constructor TMangaDetailReader.Create(Book: TMangaBook);
begin
  FBook := Book;
end;

function TMangaDetailReader.Execute: TMangaBook.TDetail;
begin
  FBook.Read(Result);
end;

{ TMangaVolumeReader }

constructor TMangaVolumeReader.Create(Book: TMangaBook; Volume: TMangaBook.PVolume);
begin
  FBook := Book;
  FVolume := Volume;
end;

function TMangaVolumeReader.Execute: TMangaBook.TPageArray;
begin
  Result := [];
  SetLength(Result, FVolume^.PageCount);
  FBook.Read(FVolume^.Path, Result);
  FVolume^.PageCount := Length(Result);
end;

{ TVirtualDirectory }

function TVirtualDirectory.GetItem(SubdirName: string): TVirtualDirectory;
var
  Subdir: TVirtualDirectory;
begin
  for Subdir in FDirList do if Subdir.Name = SubdirName then Exit(Subdir);
  Result := TVirtualDirectory.Create(specialize IfThen<string>(FPath = EmptyStr, SubdirName,ConcatPaths([FPath, SubdirName])));
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
    if VolumeList <> nil then
    begin
      Volume.Path := FPath;
      Volume.PageCount := FFileCount;
      VolumeList.Add(Volume);
    end;
    Exit(1);
  end;
  Result := 0;
  for Subdir in FDirList do Inc(Result, Subdir.FindVolumes(VolumeList));
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

{ TMangaBookReader }

function TMangaBookReader.OpenJSON(out Stream: TStream; out Parser: TJSONParser): boolean;
var
  ZipFile: unzFile;
  UnzFileInfo: unz_file_info;
  FileName: shortstring;
  TryFileName: string;
begin
  if FPackageType = mptZipped then
  begin
    ZipFile := unzOpen(PChar(FPath));
    try
      if unzLocateFile(ZipFile, PChar(BOOK_JSON_FILE_NAME), 2) = UNZ_OK then
      else if (unzGoToFirstFile(ZipFile) = UNZ_OK) and
              (unzGetCurrentFileInfo(ZipFile, @UnzFileInfo, @FileName[1], Sizeof(FileName) - 1, nil, 0, nil, 0) = UNZ_OK) then
      begin
        FileName[0] := Char(UnzFileInfo.size_filename);
        TryFileName := ConcatPaths([GetFirstDir(FileName), BOOK_JSON_FILE_NAME]);
        if (TryFileName = BOOK_JSON_FILE_NAME) or (unzLocateFile(ZipFile, PChar(TryFileName), 2) <> UNZ_OK) then Exit(False);
      end
      else Exit(False);
      if unzGetCurrentFileInfo(ZipFile, @UnzFileInfo, @FileName[1], Sizeof(FileName) - 1, nil, 0, nil, 0) <> UNZ_OK then Exit(False);
      if unzOpenCurrentFile(ZipFile) <> UNZ_OK then Exit(False);
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
  end
  else
  begin
    Stream := TFileStream.Create(ConcatPaths([FPath, BOOK_JSON_FILE_NAME]), fmOpenRead or fmShareDenyWrite);
  end;
  Parser := TJSONParser.Create(Stream, [joUTF8, joComments, joIgnoreTrailingComma, joIgnoreDuplicates]);
  Result := True;
end;

function TMangaBookReader.DefaultName: string;
begin
  Result := specialize IfThen<string>(FPackageType = mptZipped, ExtractFileNameOnly(FPath), ExtractFilename(FPath));
end;

function TMangaBookReader.GetAnyFile(Paths: TStringArray): string;
var
  Path: string;
  ZipFile: unzFile;
begin
  case FPackageType of
    mptDirectory: begin
      for Path in Paths do if FileExists(ConcatPaths([FPath, Path])) then Exit(Path);
    end;
    mptZipped: begin
      for Path in Paths do
      begin
        ZipFile := unzOpen(PChar(FPath));
        try
          if unzLocateFile(ZipFile, PChar(Path), 2) = UNZ_OK then Exit(Path);
        finally
          unzClose(ZipFile);
        end;
      end;
    end;
  end;
  Result := EmptyStr;
end;

procedure TMangaBookReader.FindVolumes(VolumeList: TVolumeList);
begin
  case FPackageType of
    mptDirectory: FVolumes := FindVolumesFromDirectory(FPath, '', VolumeList);
    mptZipped: FVolumes := FindVolumesFromZipFile(FPath, VolumeList);
  end;
end;

procedure TMangaBookReader.ReadFromJSON(JSONData: TJSONData; Detail: TMangaBook.PDetail);
var
  CoverFilenames: TStringArray = ('cover.png', 'cover.jpg', 'cover.jpeg');
begin
  if JSONData = nil then
  begin
    FTitle := DefaultName;
    FVolumes := 0;
    FCover := GetAnyFile(CoverFilenames);
    Exit;
  end;
  FCover := JSONData.ReadString('cover');
  if FCover = EmptyStr then FCover := GetAnyFile(CoverFilenames);
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
  if Detail = nil then
  begin
    if FVolumes = 0 then FVolumes := JSONData.ReadArraySize('toc');
    Exit;
  end;
  Detail^.Plot := JSONData.ReadString('plot');
  Detail^.Source := JSONData.ReadString('source');
  Detail^.Volumes := JSONData.ReadVolumeArray('toc');
  if FVolumes = 0 then FVolumes := Length(Detail^.Volumes);
end;

procedure TMangaBookReader.Read;
var
  Stream: TStream;
  Parser: TJSONParser;
begin
  if not OpenJSON(Stream, Parser) then
  begin
    FTitle := DefaultName;
    Exit;
  end;
  try
    ReadFromJSON(Parser.Parse, nil);
  finally
    Parser.Free;
    Stream.Free;
  end;
  if FVolumes = 0 then FindVolumes;
end;

procedure TMangaBookReader.Read(out Detail: TMangaBook.TDetail);
var
  Stream: TStream;
  Parser: TJSONParser;
  VolumeList: TVolumeList;
begin
  if not OpenJSON(Stream, Parser) then
  begin
    FTitle := DefaultName;
    Exit;
  end;
  try
    ReadFromJSON(Parser.Parse, @Detail);
  finally
    Parser.Free;
    Stream.Free;
  end;

  if (FVolumes = 0) or (Detail.Volumes = nil) then
  begin
    VolumeList := TVolumeList.Create;
    try
      FindVolumes(VolumeList);
      Detail.Volumes := VolumeList.ToArray;
    finally
      VolumeList.Free;
    end;
  end;
end;

procedure TMangaBookReader.Read(VolumePath: string; out PageArray: TMangaBook.TPageArray);
begin
  case FPackageType of
    mptDirectory: ReadVolumeFromDirectory(ConcatPaths([FPath, VolumePath]), PageArray);
    mptZipped: ;
  else
    raise EArgumentOutOfRangeException.Create('Not implemented');
  end;
end;

procedure TMangaBookReader.Read(FilePath: string; Picture: TPicture);
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
    mptDirectory:
      begin
        try
          Stream := TFileStream.Create(ConcatPaths([FPath, FilePath]), fmOpenRead or fmShareDenyWrite);
        except
          Exit;
        end;
      end;
    mptZipped:
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
end;


end.

