unit Manhuard.Form.View;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, ActnList, Generics.Collections,
  Manhuard.Manga, Manhuard.Frame.Book, Manhuard.Types;

type
  { TPageCache }

  TPageCache = class
  type
    TPageDict = specialize TObjectDictionary<TMangaBook.PPage, TPicture>;
    TPageQueue = specialize TQueue<TMangaBook.PPage>;
  private
    FCount: SizeInt;
    FCapacity: SizeInt;
    FPageDict: TPageDict;
    FPageQueue: TPageQueue;
  public
    constructor Create(ACapacity: SizeInt);
    destructor Destroy; override;
    procedure Clear;
    function New(Key: TMangaBook.PPage): Boolean;
    function Save(Key: TMangaBook.PPage; Value: TPicture): Boolean;
    function Load(Key: TMangaBook.PPage; out Value: TPicture): Boolean;
    property Count: SizeInt read FCount;
  end;

  { TFormView }

  TFormView = class(TForm)
    ActionResizeBoth: TAction;
    ActionResizeAny: TAction;
    ActionResizeNone: TAction;
    ActionScrollRight: TAction;
    ActionScrollLeft: TAction;
    ActionScrollDown: TAction;
    ActionScrollUp: TAction;
    ActionPreviousVolume: TAction;
    ActionNextVolume: TAction;
    ActionPreviousPage: TAction;
    ActionNextPage: TAction;
    ActionList: TActionList;
    ImageView: TImage;
    ScrollBox: TScrollBox;
    StatusBar: TStatusBar;
    Timer: TTimer;
    ToolBar: TToolBar;
    ToolButtonResizeNone: TToolButton;
    ToolButtonResizeAny: TToolButton;
    ToolButtonResizeBoth: TToolButton;
    ToolButton4: TToolButton;
    procedure ActionNextPageExecute(Sender: TObject);
    procedure ActionNextVolumeExecute(Sender: TObject);
    procedure ActionPreviousPageExecute(Sender: TObject);
    procedure ActionPreviousVolumeExecute(Sender: TObject);
    procedure ActionScrollDownExecute(Sender: TObject);
    procedure ActionScrollLeftExecute(Sender: TObject);
    procedure ActionScrollRightExecute(Sender: TObject);
    procedure ActionScrollUpExecute(Sender: TObject);
    procedure ActionResizeBothExecute(Sender: TObject);
    procedure ActionResizeAnyExecute(Sender: TObject);
    procedure ActionResizeNoneExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FLastResized: TDateTime;
    FSideBySideView: Boolean;
    FSizeAdaptation: TViewSizeAdaptation;
    FBook: TMangaBook;
    FVolumes: TMangaBook.TVolumeArray;
    FPages: TMangaBook.TPageArray;
    FVolumeIndex, FPageIndex: Integer;
    FPageCache: TPageCache;
    function GetSelectedPage: TMangaBook.PPage;
    function GetSelectedVolume: TMangaBook.PVolume;
    procedure RearrangeView;
    procedure ViewPage;
    procedure Display(const Picture: TPicture);
  protected
    procedure LoadSuccess(Sender: TMangaManager.TReadPageWork; Return: TPicture);
    procedure LoadFailure(Sender: TMangaManager.TReadPageWork; Error: TMangaManager.TReadPageWork.TError);
  public
    function Open(Source: TFrameBook): Boolean;
    function GotoNextVolume: Boolean;
    function GotoPreviousVolume: Boolean;
    property SelectedPage: TMangaBook.PPage read GetSelectedPage; 
    property SelectedVolume: TMangaBook.PVolume read GetSelectedVolume;
  end;

var
  FormView: TFormView;

const
  FORM_CAPTION = '%s/%s/%s';
  PAGE_CACHE_SIZE = 12;
  PRELOAD_COUNT = 5;

implementation

{$R *.lfm}

uses Math, DateUtils, Manhuard.Manga.Reader, Manhuard.Config, Manhuard.Helper.Picture;

{ TPageCache }

constructor TPageCache.Create(ACapacity: SizeInt);
begin
  FCount := 0;
  FCapacity := ACapacity;
  FPageDict := TPageDict.Create([doOwnsValues], ACapacity);
  FPageQueue := TPageQueue.Create;
end;

destructor TPageCache.Destroy;
begin
  FPageDict.Free;
  FPageQueue.Free;
  inherited Destroy;
end;

procedure TPageCache.Clear;
begin
  FCount := 0;
  FPageQueue.Clear;
  FPageDict.Clear;
end;

function TPageCache.New(Key: TMangaBook.PPage): Boolean;
begin
  if not FPageDict.TryAdd(Key, nil) then Exit(False);
  if FCount = FCapacity then FPageDict.Remove(FPageQueue.Dequeue) else Inc(FCount);
  FPageQueue.Enqueue(Key);
  Result := True;
end;

function TPageCache.Save(Key: TMangaBook.PPage; Value: TPicture): Boolean;
begin
  if not FPageDict.ContainsKey(Key) then Exit(False);
  FPageDict[Key] := Value;
  Result := True;
end;

function TPageCache.Load(Key: TMangaBook.PPage; out Value: TPicture): Boolean;
begin
  Result := FPageDict.TryGetValue(Key, Value);
end;


{ TFormView }

procedure TFormView.FormCreate(Sender: TObject);
begin
  FSideBySideView := Config.View.SideBySide;
  FSizeAdaptation := Config.View.SizeAdaptation;
  case FSizeAdaptation of
    vsaNone: ToolButtonResizeNone.Down := True;
    vsaAny: ToolButtonResizeAny.Down := True;
    vsaBoth: ToolButtonResizeBoth.Down := True;
  end;
  FPageCache := TPageCache.Create(PAGE_CACHE_SIZE);
end;

procedure TFormView.FormDestroy(Sender: TObject);
begin
  FPageCache.Free;
end;

procedure TFormView.FormResize(Sender: TObject);
begin
  FLastResized := Now;
  Timer.Enabled := True;
end;

procedure TFormView.TimerTimer(Sender: TObject);
begin
  if MilliSecondsBetween(FLastResized, Now) > 50 then
  begin
    RearrangeView;
    Timer.Enabled := False;
  end;
end;

procedure TFormView.ActionNextPageExecute(Sender: TObject);
begin
  if FPageIndex >= Length(FPages) - 1 then Exit;
  Inc(FPageIndex);
  ViewPage;
end;

procedure TFormView.ActionNextVolumeExecute(Sender: TObject);
begin

end;

procedure TFormView.ActionPreviousPageExecute(Sender: TObject);
begin
  if FPageIndex = 0 then Exit;
  Dec(FPageIndex);
  ViewPage;
end;

procedure TFormView.ActionPreviousVolumeExecute(Sender: TObject);
begin

end;

procedure TFormView.ActionScrollDownExecute(Sender: TObject);
var
  Y: Integer;
begin
  Y := ScrollBox.VertScrollBar.Position;
  Inc(Y, ScrollBox.VertScrollBar.Increment);
  ScrollBox.VertScrollBar.Position := Y;
end;

procedure TFormView.ActionScrollLeftExecute(Sender: TObject);
var
  X: Integer;
begin
  X := ScrollBox.HorzScrollBar.Position;
  Dec(X, ScrollBox.HorzScrollBar.Increment);
  ScrollBox.HorzScrollBar.Position := X;
end;

procedure TFormView.ActionScrollRightExecute(Sender: TObject);
var
  X: Integer;
begin
  X := ScrollBox.HorzScrollBar.Position;
  Inc(X, ScrollBox.HorzScrollBar.Increment);
  ScrollBox.HorzScrollBar.Position := X;
end;

procedure TFormView.ActionScrollUpExecute(Sender: TObject);
var
  Y: Integer;
begin
  Y := ScrollBox.VertScrollBar.Position;
  Dec(Y, ScrollBox.VertScrollBar.Increment);
  ScrollBox.VertScrollBar.Position := Y;
end;

procedure TFormView.ActionResizeBothExecute(Sender: TObject);
begin
  if FSizeAdaptation = vsaBoth then Exit;
  FSizeAdaptation := vsaBoth;
  RearrangeView;
end;

procedure TFormView.ActionResizeAnyExecute(Sender: TObject);
begin
  if FSizeAdaptation = vsaAny then Exit;
  FSizeAdaptation := vsaAny;
  RearrangeView;
end;

procedure TFormView.ActionResizeNoneExecute(Sender: TObject);
begin
  if FSizeAdaptation = vsaNone then Exit;
  FSizeAdaptation := vsaNone;
  RearrangeView;
end;

procedure TFormView.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FBook := nil;
  FVolumes := nil;
  FPages := nil;
  Caption := EmptyStr;
  FVolumeIndex := -1;
  FPageIndex := -1;
  FPageCache.Clear;
end;

procedure TFormView.RearrangeView;
var
  ImageWidth, ImageHeight, ContainerWidth, ContainerHeight: Integer;
  RawPic: TPicture;
begin
  if (not FPageCache.Load(SelectedPage, RawPic)) or (RawPic = nil) then Exit;

  ImageView.Picture := RawPic;
  ImageWidth := RawPic.Width;
  ImageHeight := RawPic.Height;
  ContainerWidth := ScrollBox.ClientWidth;
  ContainerHeight := ScrollBox.ClientHeight;

  case FSizeAdaptation of
    vsaAny:
    begin
      if ImageWidth * ScrollBox.Height > ImageHeight * ScrollBox.Width then
      begin
        ImageView.Picture.Scale(ImageWidth, ContainerHeight);
      end
      else
      begin
        ImageView.Picture.Scale(ContainerWidth, ImageHeight);
      end;
    end;
    vsaBoth:
    begin
      ScrollBox.AutoScroll := False;
      ContainerWidth := ScrollBox.ClientWidth;
      ContainerHeight := ScrollBox.ClientHeight;
      if (ImageWidth > ContainerWidth) or (ImageHeight > ContainerHeight) then
        ImageView.Picture.Scale(ContainerWidth, ContainerHeight);
    end;
  end;

  ImageWidth := ImageView.Picture.Width;
  ImageHeight := ImageView.Picture.Height;
  ImageView.Left := specialize IfThen<Integer>(ImageWidth < ContainerWidth, (ContainerWidth - ImageWidth) div 2, 0);
  ImageView.Top := specialize IfThen<Integer>(ImageHeight < ContainerHeight, (ContainerHeight - ImageHeight) div 2, 0);
  ScrollBox.AutoScroll := True;
end;

function TFormView.GetSelectedPage: TMangaBook.PPage;
begin
  if (FPageIndex < 0) or (FPageIndex >= Length(FPages)) then Exit(nil);
  Result := @FPages[FPageIndex];
end;

function TFormView.GetSelectedVolume: TMangaBook.PVolume;
begin
  if (FVolumeIndex < 0) or (FVolumeIndex >= Length(FVolumes)) then Exit(nil);
  Result := @FVolumes[FVolumeIndex];
end;

procedure TFormView.ViewPage;
var
  TheVolume: TMangaBook.PVolume;
  ThePage: TMangaBook.PPage;
  i, PageCount, VolumeCount: SizeInt;
  PagePic: TPicture;
begin
  TheVolume := SelectedVolume;
  ThePage := SelectedPage;
  if (TheVolume = nil) or (ThePage = nil) then Exit;
  PageCount := Length(FPages);
  VolumeCount := Length(FVolumes);
  Caption := Format(FORM_CAPTION, [FBook.Name, TheVolume^.Path, ThePage^.Name]);
  ImageView.Picture.Clear;
  ScrollBox.HorzScrollBar.Position := 0;
  ScrollBox.VertScrollBar.Position := 0;
  ActionPreviousPage.Enabled := FPageIndex > 0;
  ActionNextPage.Enabled := FPageIndex < PageCount - 1;
  ActionPreviousVolume.Enabled := FVolumeIndex > 0;
  ActionNextVolume.Enabled := FVolumeIndex < VolumeCount - 1;

  if FPageCache.Load(ThePage, PagePic) then
    Display(PagePic)
  else
  begin
    FPageCache.New(ThePage);
    MangaManager.ReadPage(FBook, TheVolume, ThePage, @LoadSuccess, @LoadFailure);
  end;

  for i := FPageIndex + 1 to Min(PageCount - 1, FPageIndex + PRELOAD_COUNT) do
  begin
    ThePage := @FPages[i];
    if FPageCache.New(ThePage) then MangaManager.ReadPage(FBook, TheVolume, ThePage, @LoadSuccess, @LoadFailure);
  end;
end;

procedure TFormView.Display(const Picture: TPicture);
begin
  ImageView.Picture := Picture;
  RearrangeView;
end;

procedure TFormView.LoadSuccess(Sender: TMangaManager.TReadPageWork; Return: TPicture);
var
  ReturnPage: TMangaBook.PPage;
begin
  ReturnPage := (Sender as TMangaPageLoader).Page;
  FPageCache.Save(ReturnPage, Return);
  if SelectedPage <> ReturnPage then Exit;
  Display(Return);
end;

procedure TFormView.LoadFailure(Sender: TMangaManager.TReadPageWork; Error: TMangaManager.TReadPageWork.TError);
var
  ReturnPage: TMangaBook.PPage;
begin
  ReturnPage := (Sender as TMangaPageLoader).Page;
  if SelectedPage = ReturnPage then ShowMessage(Error.Message);
end;

function TFormView.Open(Source: TFrameBook): Boolean;
var
  SelectedItem: TListItem;
  TheVolume: TMangaBook.PVolume;
  i: Integer;
begin
  SelectedItem := Source.PageListView.Selected;
  if SelectedItem = nil then Exit(False);
  FPageIndex := SelectedItem.Index;
  TheVolume := Source.SelectedVolume;
  FBook := Source.Book;
  FVolumes := Source.Volumes;
  FPages := Source.Pages[TheVolume];
  for i := 0 to Length(FVolumes) - 1 do if @FVolumes[i] = TheVolume then break;
  FVolumeIndex := i;
  FPageCache.Clear;
  ViewPage;
  Result := True;
end;

function TFormView.GotoNextVolume: Boolean;
begin
  if FVolumeIndex = Length(FVolumes) - 1 then Exit(False);
  Inc(FVolumeIndex);
  Result := False;
end;

function TFormView.GotoPreviousVolume: Boolean;
begin
  if FVolumeIndex = 0 then Exit(False);
  Dec(FVolumeIndex);
  Result := False;
end;

end.

