unit Manhuard.Manga;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fgl, Contnrs, Manhuard.WorkPool;

type

  TMangaManager = class;
  TMangaBooks = class;

  { TMangaEvent }

  TMangaEvent = class
  type TEventType = (etLoading, etLoaded);
  private
    FManager: TMangaManager;
    FEventType: TEventType;
  public
    constructor Create(Sender: TMangaManager; EventType: TEventType);
    property EventType: TEventType read FEventType;
  end;

  TMangaEventListener = procedure (Event: TMangaEvent) of object;
  TMangaEventListenerList = specialize TFPGList<TMangaEventListener>;

  { TMangaBook }

  TMangaBook = class
  type
    TYear = SmallInt;
    TRunRange = (rrFrom, rrTo);
    TOriginalRun = array[TRunRange] of TDateTime;
    TSeriesState = (ssUnknown, ssCompleted, ssOngoing);
    TPackageType = (mptDir, mptZip, mptTar, mptRar, mptEPub);

    TPage = record
      Name: string;
      Size: Integer;
    end;
    TPageArray = array of TPage;
    PPage = ^TPage;

    TVolume = record
      Path: string;
      PageCount: Integer;
    end;
    PVolume = ^TVolume;
    TVolumeArray = array of TVolume;

    TDetails = record
      Plot: string;
      Source: string;
      Volumes: TVolumeArray;
    end;
    PDetails = ^TDetails;

    TCoverDetails = record
      Cover: TPicture;
      Details: TDetails;
    end;
  private
    function GetCaption: string;
    function GetName: string;
    function GetOriginalRunFrom: TDateTime;
    function GetOriginalRunTo: TDateTime;
    procedure SetOriginalRunFrom(AValue: TDateTime);
    procedure SetOriginalRunTo(AValue: TDateTime);
  protected
    FPackageType: TPackageType;
    FPath: string;
    FTitle: string;
    FWriters: TStringArray;
    FVolumes: Integer;
    FChapters: Integer;
    FReleaseYear: TYear;
    FOriginalRun: TOriginalRun;
    FRegion: string;
    FGenre: TStringArray;
    FLastUpdated: TDatetime;
    FSeriesState: TSeriesState;
  public
    constructor Create(MangaPath: string; PackageType: TPackageType);
    destructor Destroy; override;
    property PackageType: TPackageType read FPackageType;
    property Path: string read FPath;
    property Caption: string read GetCaption;
    property Name: string read GetName;
    property Title: string read FTitle write FTitle;
    property Writers: TStringArray read FWriters write FWriters;
    property Volumes: Integer read FVolumes write FVolumes;
    property Chapters: Integer read FChapters write FChapters;
    property ReleaseYear: TYear read FReleaseYear write FReleaseYear;
    property OriginalRun: TOriginalRun read FOriginalRun write FOriginalRun;
    property OriginalRunFrom: TDateTime read GetOriginalRunFrom write SetOriginalRunFrom;
    property OriginalRunTo: TDateTime read GetOriginalRunTo write SetOriginalRunTo;
    property Region: string read FRegion write FRegion;
    property Genre: TStringArray read FGenre write FGenre;
    property LastUpdated: TDatetime read FLastUpdated write FLastUpdated;
    property SeriesState: TSeriesState read FSeriesState write FSeriesState;
  end;

  { TMangaBooks }

  TMangaBooks = class
  private
    FBooks: TFPObjectList;
    function GetItem(const Index: Integer): TMangaBook;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[const Index: Integer]: TMangaBook read GetItem; default;
    function Add(Book: TMangaBook): Integer;
  end;

  { TMangaManager }

  TMangaManager = class
  type
    TLoadBooksWork = specialize TWork<TMangaBooks>;
    TReadBookWork = specialize TWork<TMangaBook.TCoverDetails>;
    TReadVolumeWork = specialize TWork<TMangaBook.TPageArray>;
  private
    FBooks: TMangaBooks;
    FListenerTable: array [TMangaEvent.TEventType] of TMangaEventListenerList;
    procedure LoadSuccess(Sender: TLoadBooksWork; Return: TMangaBooks);
    procedure LoadFailure(Sender: TLoadBooksWork; Error: TLoadBooksWork.TError);
    procedure LoadComplete(Sender: TLoadBooksWork);
    procedure DispatchEvent(Event: TMangaEvent);
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;
    property Books: TMangaBooks read FBooks;
    procedure AddEventListener(EventType: TMangaEvent.TEventType; Listener: TMangaEventListener);
    procedure RemoveEventListener(EventType: TMangaEvent.TEventType; Listener: TMangaEventListener);
    procedure Load;
    procedure ReadBook(Book: TMangaBook; OnSuccess: TReadBookWork.TOnSuccess; OnFailure: TReadBookWork.TOnFailure);
    procedure ReadVolume(Book: TMangaBook; Volume: TMangaBook.PVolume;
                         OnSuccess: TReadVolumeWork.TOnSuccess; OnFailure: TReadVolumeWork.TOnFailure);
  end;

var
  MangaManager: TMangaManager;

implementation

uses LazFileUtils, Manhuard.Manga.Loader, Manhuard.Manga.Reader;

procedure TriggerEvent(Sender: TMangaManager; EventType: TMangaEvent.TEventType);
var
  Event: TMangaEvent;
begin
  Event := TMangaEvent.Create(Sender, EventType);
  try Sender.DispatchEvent(Event) finally Event.Free end;
end;

{ TMangaEvent }

constructor TMangaEvent.Create(Sender: TMangaManager; EventType: TEventType);
begin
  FManager := Sender;
  FEventType := EventType;
end;

{ TMangaBook }

function TMangaBook.GetCaption: string;
begin
  Result := specialize IfThen<string>(Title = EmptyStr, Name, Title);
end;

function TMangaBook.GetName: string;
begin
  Result := specialize IfThen<string>(FPackageType <> mptDir, ExtractFileNameOnly(FPath), ExtractFilename(FPath));
end;

function TMangaBook.GetOriginalRunFrom: TDateTime;
begin
  Result := FOriginalRun[rrFrom];
end;

function TMangaBook.GetOriginalRunTo: TDateTime;
begin
  Result := FOriginalRun[rrTo];
end;

procedure TMangaBook.SetOriginalRunFrom(AValue: TDateTime);
begin
  FOriginalRun[rrFrom] := AValue;
end;

procedure TMangaBook.SetOriginalRunTo(AValue: TDateTime);
begin
  FOriginalRun[rrTo] := AValue;
end;

constructor TMangaBook.Create(MangaPath: string; PackageType: TPackageType);
begin
  FPath := MangaPath;
  FPackageType := PackageType;
end;

destructor TMangaBook.Destroy;
begin
  inherited Destroy;
end;

{ TMangaBooks }

function TMangaBooks.GetItem(const Index: Integer): TMangaBook;
begin
  Result := TMangaBook(FBooks[Index]);
end;

function TMangaBooks.GetCount: Integer;
begin
  Result := FBooks.Count;
end;

function TMangaBooks.Add(Book: TMangaBook): Integer;
begin
  Result := FBooks.Add(Book);
end;

constructor TMangaBooks.Create;
begin
  FBooks := TFPObjectList.Create(True);
end;

destructor TMangaBooks.Destroy;
begin
  FBooks.Free;
  inherited Destroy;
end;

{ TMangaManager }

procedure TMangaManager.LoadSuccess(Sender: TLoadBooksWork; Return: TMangaBooks);
begin
  FBooks := Return;
end;

procedure TMangaManager.LoadFailure(Sender: TLoadBooksWork; Error: TLoadBooksWork.TError);
begin
  { todo: }
end;

procedure TMangaManager.LoadComplete(Sender: TLoadBooksWork);
begin
  TriggerEvent(Self, etLoaded);
end;

procedure TMangaManager.DispatchEvent(Event: TMangaEvent);
var
  Listener: TMangaEventListener;
begin
  for Listener in FListenerTable[Event.EventType] do try Listener(Event) except end;
end;

procedure TMangaManager.Clear;
begin
  if Assigned(FBooks) then FreeAndNil(FBooks);
end;

constructor TMangaManager.Create;
var
  EventType: TMangaEvent.TEventType;
begin
  FBooks := TMangaBooks.Create;
  for EventType in TMangaEvent.TEventType do FListenerTable[EventType] := TMangaEventListenerList.Create;
end;

destructor TMangaManager.Destroy;
var
  ListenerList: TMangaEventListenerList;
begin
  FBooks.Free;
  for ListenerList in FListenerTable do ListenerList.Free;
  inherited Destroy;
end;

procedure TMangaManager.AddEventListener(EventType: TMangaEvent.TEventType; Listener: TMangaEventListener);
var
  ListenerList: TMangaEventListenerList;
begin
  ListenerList := FListenerTable[EventType];
  if ListenerList.IndexOf(Listener) < 0 then ListenerList.Add(Listener);
end;

procedure TMangaManager.RemoveEventListener(EventType: TMangaEvent.TEventType; Listener: TMangaEventListener);
begin
  FListenerTable[EventType].Remove(Listener);
end;

procedure TMangaManager.Load;
var
  Loader: TMangaLoader;
begin
  Clear;
  TriggerEvent(Self, etLoading);
  Loader := TMangaLoader.Create;
  Loader.OnSuccess := @LoadSuccess;
  Loader.OnFailure := @LoadFailure;
  Loader.OnComplete := @LoadComplete;
  WorkPool.Exec(Loader);
end;

procedure TMangaManager.ReadBook(Book: TMangaBook; OnSuccess: TReadBookWork.TOnSuccess; OnFailure: TReadBookWork.TOnFailure);
var
  Loader: TMangaDetailsLoader;
begin
  Loader := TMangaDetailsLoader.Create(Book);
  Loader.OnSuccess := OnSuccess;
  Loader.OnFailure := OnFailure;
  WorkPool.Exec(Loader);
end;

procedure TMangaManager.ReadVolume(Book: TMangaBook; Volume: TMangaBook.PVolume; OnSuccess: TReadVolumeWork.TOnSuccess;
  OnFailure: TReadVolumeWork.TOnFailure);
var
  Loader: TMangaVolumeLoader;
begin
  Loader := TMangaVolumeLoader.Create(Book, Volume);
  Loader.OnSuccess := OnSuccess;
  Loader.OnFailure := OnFailure;
  WorkPool.Exec(Loader);
end;


initialization

MangaManager := TMangaManager.Create;

finalization

MangaManager.Free;

end.

