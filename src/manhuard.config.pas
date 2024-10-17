unit Manhuard.Config;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, IniFiles, Manhuard.Strings, Manhuard.Types;

type

  { TIniFileHelper }

  TIniFileHelper = class helper for TIniFile
    function ReadStringArray(const Section, Ident, Separator: string; Default: TStringArray): TStringArray;
    procedure WriteStringArray(const Section, Ident, Separator: string; Value: TStringArray);
  end;

  { EConfig }

  EConfig = class(Exception);

  { TConfig }

  TConfig = object
  private
    FPath: string;
    function GetDir: string;
    function GetPath: string;
    procedure LoadFromFile(Path: string);
  public
    Filename: string;
    Database: string;
    WorkPool: record
      Capacity: SmallInt;
    end;

    Bookshelf: record
      ListView: record
        ViewStyle: TViewStyle;
      end;
    end;

    View: record
      SideBySide: Boolean;
      SizeAdaptation: TViewSizeAdaptation;
    end;

    MangaDirs: TStringArray;

    procedure Load;
    procedure Save;
    property Path: string read GetPath;
    property Dir: string read GetDir;
  end;


const
  CONFIG_DIR = 'profile';
  WORKPOOL_CAPACITY = 2;
  BOOKSHELF_LISTVIEW_VIEWSTYLE = vsReport;

var
  Config: TConfig = (
    Filename: INTERNAL_NAME + '.conf';
    Database: INTERNAL_NAME + '.db';
    WorkPool: (
      Capacity: WORKPOOL_CAPACITY
    );
    Bookshelf: (
      ListView: (
        ViewStyle: BOOKSHELF_LISTVIEW_VIEWSTYLE
      )
    );
    View: (
      SideBySide: False;
      SizeAdaptation: vsaAny;
    );
  );

implementation

uses Forms;

{ TIniFileHelper }

function TIniFileHelper.ReadStringArray(const Section, Ident, Separator: string; Default: TStringArray): TStringArray;
var
  Value: String;
begin
  Value := ReadString(Section, Ident, EmptyStr);
  if Value = EmptyStr then Exit(Default);
  Result := Value.Split([Separator]);
end;

procedure TIniFileHelper.WriteStringArray(const Section, Ident, Separator: string; Value: TStringArray);
begin
  WriteString(Section, Ident, string.Join(Separator, Value));
end;

{ TConfig }

function TConfig.GetPath: string;
begin
  if EmptyStr = FPath then
    Result := ConcatPaths([GetAppConfigDir(False), Filename])
  else
    Result := FPath;
end;

function TConfig.GetDir: string;
begin
  Result := ExtractFileDir(Path);
end;

procedure TConfig.Save;
var
  ConfigDir: String;
  IniFile: TIniFile;
begin
  if not FileExists(Path) then
  begin
    ConfigDir := ExtractFileDir(Path);
    if (not DirectoryExists(ConfigDir)) and (not ForceDirectories(ConfigDir)) then raise EConfig.CreateFmt(MSG_CONFIG_SAVE_ERROR, [Path]);
  end;
  try
    IniFile := TIniFile.Create(Path);
  except
    on E: Exception do EConfig.CreateFmt(MSG_CONFIG_SAVE_ERROR, [E.Message]);
  end;
  try
    IniFile.WriteInteger('WorkPool', 'Capacity', WorkPool.Capacity);
    IniFile.WriteInteger('Bookshelf.ListView', 'ViewStyle', Integer(Bookshelf.ListView.ViewStyle));
    IniFile.WriteStringArray(UpCase(INTERNAL_NAME), 'MangaDirs', ';', MangaDirs);
  finally
    IniFile.Free;
  end;
end;

procedure TConfig.LoadFromFile(Path: string);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(Path);
  try
    WorkPool.Capacity := IniFile.ReadInteger('WorkPool', 'Capacity', WorkPool.Capacity);
    with Bookshelf.ListView do ViewStyle := TViewStyle(IniFile.ReadInteger('Bookshelf.ListView', 'ViewStyle', Integer(ViewStyle)));
    MangaDirs := IniFile.ReadStringArray(UpCase(INTERNAL_NAME), 'MangaDirs', ';', [ConcatPaths([GetUserDir, INTERNAL_NAME])]);
  finally
    IniFile.Free;
  end;
end;

procedure TConfig.Load;
var
  ConfigFile, AppDir, CurrentDir: string;
begin
  AppDir := ExtractFilePath(Application.ExeName);
  CurrentDir := GetCurrentDir;
  for ConfigFile in [
    ConcatPaths([AppDir, Filename]),
    ConcatPaths([AppDir, CONFIG_DIR, Filename]),
    ConcatPaths([CurrentDir, Filename]),
    ConcatPaths([CurrentDir, CONFIG_DIR, Filename]),
    ConcatPaths([GetAppConfigDir(False), Filename]),
    ConcatPaths([GetAppConfigDir(True), Filename])
  ] do
  begin
    if not FileExists(ConfigFile) then Continue;
    LoadFromFile(ConfigFile);
    FPath := ConfigFile;
    Exit;
  end;
end;


initialization

Config.Load;

finalization

Config.Save;

end.

