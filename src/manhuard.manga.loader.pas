unit Manhuard.Manga.Loader;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Manhuard.WorkPool, Manhuard.Manga;

type

  { TMangaLoader }

  TMangaLoader = class (TMangaManager.TLoadBooksWork)
  private
    FBooks: TMangaBooks;
    procedure ScanDir(Path: string);
  public
    constructor Create;
    destructor Destroy; override;
    function Execute: TMangaBooks; override;
  end;

const
  MANGA_BOOK_FILE_EXT = '.mhs';
  BOOK_JSON_FILE_NAME = 'book.json';

implementation

uses LazFileUtils, LazUTF8, Manhuard.Config, Manhuard.Manga.Reader;

function IsMangaDirectory(Path: string): boolean; inline;
begin
  Result := FileExists(ConcatPaths([Path, BOOK_JSON_FILE_NAME]));
end;

function IsMangaFile(Path: string): boolean; inline;
begin
  Result := ExtractFileExt(Path) = MANGA_BOOK_FILE_EXT;
end;

{ TMangaLoader }

procedure TMangaLoader.ScanDir(Path: string);
 function LoadBook(MangaPath: string; PackageType: TMangaBook.TPackageType): TMangaBook;
 begin
   Result := TMangaBook.Create(MangaPath, PackageType);
   Result.Read;
 end;

var
  SearchRec: TSearchRec;
  SubPath: string;
begin
  if FindFirst(ConcatPaths([Path, '*']), faAnyFile, SearchRec) = 0 then
  begin
    try
      repeat
        SubPath := ConcatPaths([Path, SearchRec.Name]);
        if faDirectory and SearchRec.Attr <> 0 then
        begin
          if (SearchRec.Name = '.') or (SearchRec.Name = '..') then continue;
          if IsMangaDirectory(SubPath) then
            FBooks.Add(LoadBook(SubPath, mptDirectory))
          else
            ScanDir(SubPath);
        end
        else if IsMangaFile(SubPath) then
          FBooks.Add(LoadBook(SubPath, mptZipped));
      until FindNext(SearchRec) <> 0;
      if Canceling then Cancel;
    finally
      FindClose(SearchRec);
    end;
  end;
end;

constructor TMangaLoader.Create;
begin
  inherited;
  FBooks := TMangaBooks.Create;
end;

destructor TMangaLoader.Destroy;
begin
  if not Succeeded then FBooks.Free;
end;

function TMangaLoader.Execute: TMangaBooks;
var
  MangaDir: string;
begin
  for MangaDir in Config.MangaDirs do
  begin
    if not DirectoryExists(MangaDir) then continue;
    ScanDir(MangaDir);
  end;
  Result := FBooks;
end;

end.

