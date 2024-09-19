unit Manhuard.Manga;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, Contnrs, Manhuard.WorkPool;

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
  protected
    FPackageType: TPackageType;
    FPath: string;
    FCover: string;
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
    property Cover: string read FCover;
    property Title: string read FTitle;
    property Writers: TStringArray read FWriters;
    property Volumes: Integer read FVolumes;
    property Chapters: Integer read FChapters;
    property ReleaseYear: TYear read FReleaseYear; 
    property OriginalRun: TOriginalRun read FOriginalRun;
    property Region: string read FRegion;
    property Genre: TStringArray read FGenre;
    property LastUpdated: TDatetime read FLastUpdated;
    property SeriesState: TSeriesState read FSeriesState;
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
    TReadBookWork = specialize TWork<TMangaBook.TDetails>;
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
    procedure ReadVolume(Book: TMangaBook; var Volume: TMangaBook.TVolume;
                         OnSuccess: TReadVolumeWork.TOnSuccess; OnFailure: TReadVolumeWork.TOnFailure);
  end;

var
  MangaManager: TMangaManager;

implementation

uses Manhuard.Manga.Loader, Manhuard.Manga.Reader;

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
  Reader: TMangaDetailsLoader;
begin
  Reader := TMangaDetailsLoader.Create(Book);
  Reader.OnSuccess := OnSuccess;
  Reader.OnFailure := OnFailure;
  WorkPool.Exec(Reader);
end;

procedure TMangaManager.ReadVolume(Book: TMangaBook; var Volume: TMangaBook.TVolume; OnSuccess: TReadVolumeWork.TOnSuccess;
  OnFailure: TReadVolumeWork.TOnFailure);
var
  Reader: TMangaVolumeLoader;
begin
  Reader := TMangaVolumeLoader.Create(Book, @Volume);
  Reader.OnSuccess := OnSuccess;
  Reader.OnFailure := OnFailure;
  WorkPool.Exec(Reader);
end;


initialization

MangaManager := TMangaManager.Create;

finalization

MangaManager.Free;

end.

