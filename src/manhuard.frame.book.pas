unit Manhuard.Frame.Book;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls, Graphics, Generics.Collections,
  Manhuard.Manga, Manhuard.Helper.ListView;

type

  TIconManagerDict = specialize TObjectDictionary<TMangaBook.PVolume, TListViewIconManager>;

  { TPageLoader }

  TPageLoader = class (TListViewIconManager.TLoadIconsWork)
  protected
    FBook: TMangaBook;
    FVolume: TMangaBook.PVolume;
    FDataArray: TListViewIconManager.TDataArray;
  public
    constructor Create(Book: TMangaBook; Volume: TMangaBook.PVolume; DataArray: TListViewIconManager.TDataArray);
    function Execute: TListViewIconManager.TDataIconPairArray; override;
  end;

  { TFrameBook }

  TFrameBook = class(TFrame)
    Cover: TImage;
    FlowPanel: TFlowPanel;
    PageImageList: TImageList;
    LabelOriginalRun: TLabel;
    LabelOriginalRunText: TLabel;
    LabelRegion: TLabel;
    LabelLastUpdated: TLabel;
    LabelLastUpdatedText: TLabel;
    LabelYear: TLabel;
    LabelGenre: TLabel;
    LabelStateText: TLabel;
    LabelState: TLabel;
    LabelPlotText: TLabel;
    LabelPlot: TLabel;
    LabelRegionText: TLabel;
    LabelYearText: TLabel;
    LabelGenreText: TLabel;
    LabelWriter: TLabel;
    LabelTitle: TLabel;
    PageListView: TListView;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    CoverBox: TScrollBox;
    Splitter: TSplitter;
    StatusBar: TStatusBar;
    TableOfContents: TTreeView;
    procedure FrameResize(Sender: TObject);
    procedure LabelTitleClick(Sender: TObject);
    procedure LabelTitleMouseEnter(Sender: TObject);
    procedure LabelTitleMouseLeave(Sender: TObject);
    procedure CoverBoxResize(Sender: TObject);
    procedure PageListViewCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure PageListViewData(Sender: TObject; Item: TListItem);
    procedure SplitterCanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
    procedure TableOfContentsSelectionChanged(Sender: TObject);
    procedure ListViewLoadIcon(Sender: TListViewIconManager; out LoadIconWork: TListViewIconManager.TLoadIconsWork);
  private
    FBook: TMangaBook;
    FDetail: TMangaBook.TDetails;
    FIconManagers: TIconManagerDict;
    FDefaultIcon: TPicture;
    FBusy: boolean;
    FSelectedVolume: TMangaBook.PVolume;
    FSelectedVolumePages: TMangaBook.TPageArray;
    procedure SetBook(AValue: TMangaBook);
    procedure Rearrange;
    procedure ReadSuccess(Sender: TMangaManager.TReadBookWork; Return: TMangaBook.TCoverDetails);
    procedure ReadFailure(Sender: TMangaManager.TReadBookWork; Error: TMangaManager.TReadBookWork.TError);
    procedure ReadVolumeSuccess(Sender: TMangaManager.TReadVolumeWork; Return: TMangaBook.TPageArray);
    procedure ReadVolumeFailure(Sender: TMangaManager.TReadVolumeWork; Error: TMangaManager.TReadBookWork.TError);
    procedure SetBusy(AValue: boolean);
    procedure SetLabel(LabelText, LabelValue: TLabel; Value: string); 
    procedure SetLabel(LabelText, LabelValue: TLabel; Value: TDateTime); 
    procedure SetLabel(LabelText, LabelValue: TLabel; Value: Integer);
    procedure SetLabel(LabelText, LabelValue: TLabel; Value: TMangaBook.TOriginalRun);
    procedure SetTableOfContents;
    function GetIconManager: TListViewIconManager;
    property IconManager: TListViewIconManager read GetIconManager;
  public
    property Book: TMangaBook read FBook write SetBook;
    property Busy: boolean read FBusy write SetBusy;
    procedure Initialize;
    procedure Finalize;
  end;

const
  RIGHT_PANEL_MIN_SIZE = 312;
  ORIGINAL_RUN_FROM_TO = '%s-%s';
  TOC_PATH_DEPTH = 255;

resourcestring
  WRITTEN_BY = 'By ';
  STATE_COMPLETED = 'Completed';
  STATE_ONGOING = 'Ongoing';
  NOT_ACCESS = 'N/A';
  PRESENT = 'present';
  PAGE_COUNT = '%d pages';

implementation

{$R *.lfm}

uses Dialogs, LCLIntf, Manhuard.Manga.Reader, Manhuard.Helper.Picture;

{ TPageLoader }

constructor TPageLoader.Create(Book: TMangaBook; Volume: TMangaBook.PVolume; DataArray: TListViewIconManager.TDataArray);
begin
  FBook := Book;
  FVolume := Volume;
  FDataArray := DataArray;
end;

function TPageLoader.Execute: TListViewIconManager.TDataIconPairArray;
var
  Size: SizeInt;
  i: Integer;
  Page: TMangaBook.PPage;
  Picture: TPicture;
  PagePath: String;
begin
  Size := Length(FDataArray);
  Result := [];
  SetLength(Result, Size);
  for i := 0 to Size -1 do
  begin
    Page := TMangaBook.PPage(FDataArray[i]);
    { todo: 卷未加载完成时读取 Page 出错 }
    PagePath := FVolume^.Path + '/' + Page^.Name;
    WriteLn(PagePath);
    Picture := FBook.ReadPage(PagePath);
    Picture.Scale(150, 150);
    with Result[i] do
    begin
      Key := Page;
      Value := Picture;
    end;
  end;
end;

{ TFrameBook }

procedure TFrameBook.PageListViewCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  if State = [] then Exit;
  IconManager.DrawIcon(Item, haCenter, vaBottom);
end;

procedure TFrameBook.FrameResize(Sender: TObject);
begin
  Rearrange;
end;

procedure TFrameBook.LabelTitleClick(Sender: TObject);
begin
  if LabelTitle.Hint = EmptyStr then Exit;
  OpenURL(LabelTitle.Hint);
end;

procedure TFrameBook.LabelTitleMouseEnter(Sender: TObject);
var
  FontStyle: TFontStyles;
begin
  if LabelTitle.Hint = EmptyStr then Exit;
  FontStyle := LabelTitle.Font.Style;
  Include(FontStyle, fsUnderline);
  LabelTitle.Font.Style := FontStyle;
end;

procedure TFrameBook.LabelTitleMouseLeave(Sender: TObject);
var
  FontStyle: TFontStyles;
begin
  if LabelTitle.Hint = EmptyStr then Exit;
  FontStyle := LabelTitle.Font.Style;
  Exclude(FontStyle, fsUnderline);
  LabelTitle.Font.Style := FontStyle;
end;

procedure TFrameBook.CoverBoxResize(Sender: TObject);
begin
  Rearrange;
end;

procedure TFrameBook.PageListViewData(Sender: TObject; Item: TListItem);
var
  Page: TMangaBook.PPage;
begin
  Page := @FSelectedVolumePages[Item.Index];
  Item.Caption := Page^.Name;
  Item.Data := Page;
end;

procedure TFrameBook.SplitterCanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
begin
  if NewSize > Width - RIGHT_PANEL_MIN_SIZE then Accept := False;
end;

procedure TFrameBook.TableOfContentsSelectionChanged(Sender: TObject);
  function GetFullPath(Node: TTreeNode): string;
  begin
    if Node.Parent = nil then Exit(EmptyStr);
    Result := GetFullPath(Node.Parent);
    if Result = EmptyStr then
      Result := Node.Text
    else
      Result := Result + '/' + Node.Text;
  end;

var
  Node: TTreeNode;
  Volume: TMangaBook.PVolume;
begin
  Node := TableOfContents.Selected;
  if Node = nil then Exit;

  if Node.Parent = nil then
  begin
    StatusBar.Panels[0].Text := FBook.Caption;
    StatusBar.Panels[1].Text := EmptyStr;
    CoverBox.Visible := True;
    PageListView.Visible := False;
  end
  else if Node.Data = nil then
  begin
    StatusBar.Panels[0].Text := GetFullPath(Node);
    StatusBar.Panels[1].Text := EmptyStr;
    CoverBox.Visible := False;
    PageListView.Visible := False;
  end
  else
  begin
    Volume := TMangaBook.PVolume(Node.Data);
    StatusBar.Panels[0].Text := Volume^.Path;
    StatusBar.Panels[1].Text := Format(PAGE_COUNT, [Volume^.PageCount]);
    CoverBox.Visible := False;  
    PageListView.Visible := True;
    PageListView.Clear;
    PageListView.Cursor := crAppStart;
    FSelectedVolume := Volume;
    FSelectedVolumePages := nil;
    MangaManager.ReadVolume(FBook, Volume^.Path, @ReadVolumeSuccess, @ReadVolumeFailure);
  end;
end;

procedure TFrameBook.ListViewLoadIcon(Sender: TListViewIconManager; out LoadIconWork: TListViewIconManager.TLoadIconsWork);
begin
  LoadIconWork := TPageLoader.Create(FBook, FSelectedVolume, Sender.PendingData);
end;

function TFrameBook.GetIconManager: TListViewIconManager;
begin
  if FSelectedVolume = nil then Exit(nil);

  if not FIconManagers.ContainsKey(FSelectedVolume) then
  begin
    Result := TListViewIconManager.Create(PageListView);
    Result.DefaultIcon := FDefaultIcon;
    Result.OnLoadIcon := @ListViewLoadIcon;
    FIconManagers.Add(FSelectedVolume, Result);
  end
  else
    Result := FIconManagers.Items[FSelectedVolume];
end;

procedure TFrameBook.SetBook(AValue: TMangaBook);
begin
  FBook := AValue;
  LabelTitle.Caption := FBook.Caption;
  Busy := True;
  FIconManagers.Clear;
  MangaManager.ReadBook(FBook, @ReadSuccess, @ReadFailure);
end;

procedure TFrameBook.Rearrange;
var
  LeftMinWidth, CoverBoxClientWidth, CoverBoxInnerWidth, FlowPanelBottom: Integer;
begin
  Cover.Left := (CoverBox.Width - Cover.Width) div 2;
  LabelTitle.Left := (CoverBox.Width - LabelTitle.Width) div 2;
  LabelWriter.Left := (CoverBox.Width - LabelWriter.Width) div 2;
  LeftMinWidth := Width - RIGHT_PANEL_MIN_SIZE;
  if Splitter.Left > LeftMinWidth then TableOfContents.Width := LeftMinWidth;
  CoverBoxClientWidth := CoverBox.ClientWidth;
  CoverBoxInnerWidth := CoverBoxClientWidth - 20;
  LabelPlot.Constraints.MaxWidth := CoverBoxInnerWidth;
  LabelPlot.Constraints.MinWidth := CoverBoxInnerWidth;
  FlowPanel.Constraints.MaxWidth := CoverBoxInnerWidth;
  FlowPanel.Constraints.MinWidth := CoverBoxInnerWidth;
  FlowPanelBottom := FlowPanel.Top + FlowPanel.Height;
  LabelPlotText.Top := FlowPanelBottom + 20;
  LabelPlot.Top := FlowPanelBottom + 50;
end;

procedure TFrameBook.ReadSuccess(Sender: TMangaManager.TReadBookWork; Return: TMangaBook.TCoverDetails);
begin
  Busy := False;
  if Assigned(Return.Cover) then
  begin
    try
      Cover.Picture.Assign(Return.Cover);
    finally
      Return.Cover.Free;
    end;
  end;
  FDetail := Return.Details;
  LabelWriter.Caption := WRITTEN_BY + string.Join(', ', Book.Writers);
  case FBook.SeriesState of
    ssUnknown: SetLabel(LabelStateText, LabelState, NOT_ACCESS);
    ssCompleted: SetLabel(LabelStateText, LabelState, STATE_COMPLETED);
    ssOngoing: SetLabel(LabelStateText, LabelState, STATE_ONGOING);
  end;
  SetLabel(LabelRegionText, LabelRegion, FBook.Region);
  SetLabel(LabelYearText, LabelYear, FBook.ReleaseYear);
  SetLabel(LabelGenreText, LabelGenre, string.Join(', ', FBook.Genre));
  SetLabel(LabelLastUpdatedText, LabelLastUpdated, FBook.LastUpdated);
  SetLabel(LabelOriginalRunText, LabelOriginalRun, FBook.OriginalRun);
  if Return.Details.Source = EmptyStr then
  begin
    LabelTitle.Font.Color := clDefault;
    LabelTitle.Cursor := crDefault;
    LabelTitle.Hint := '';
    LabelTitle.ShowHint := False;
  end
  else
  begin
    LabelTitle.Font.Color := clHighlight;
    LabelTitle.Cursor := crHandPoint;
    LabelTitle.Hint := Return.Details.Source;
    LabelTitle.ShowHint := True;
  end;
  LabelPlot.Caption := Return.Details.Plot;
  SetTableOfContents;
  FrameResize(Self);
end;

procedure TFrameBook.ReadFailure(Sender: TMangaManager.TReadBookWork; Error: TMangaManager.TReadBookWork.TError);
begin
  Busy := False;
  ShowMessage(Error.Message);
end;

procedure TFrameBook.ReadVolumeSuccess(Sender: TMangaManager.TReadVolumeWork; Return: TMangaBook.TPageArray);
var
  PageCount: SizeInt;
begin
  PageCount := Length(Return);
  StatusBar.Panels[1].Text := Format(PAGE_COUNT, [PageCount]);
  FSelectedVolumePages := Return;
  PageListView.Items.Count := PageCount;
  PageListView.Cursor := crDefault;
end;

procedure TFrameBook.ReadVolumeFailure(Sender: TMangaManager.TReadVolumeWork; Error: TMangaManager.TReadBookWork.TError);
begin
  PageListView.Cursor := crDefault;
  ShowMessage(Error.Message);
end;

procedure TFrameBook.SetBusy(AValue: boolean);
begin
  if FBusy = AValue then Exit;
  FBusy := AValue;
  if FBusy then
  begin
    Visible := False;
    (Owner as TControl).Cursor := crAppStart;
  end
  else
  begin
    Visible := True;
    (Owner as TControl).Cursor := crDefault;
  end;
end;

procedure TFrameBook.SetLabel(LabelText, LabelValue: TLabel; Value: string);
var
  OriginalWidth: Integer;
begin
  LabelValue.Constraints.MinWidth := 0;
  LabelValue.Left := LabelText.Width + 20;
  LabelValue.Caption := specialize IfThen<string>(Value = EmptyStr, NOT_ACCESS, Value);
  OriginalWidth := LabelValue.Width;
  LabelValue.Constraints.MinWidth := OriginalWidth + 20;
end;

procedure TFrameBook.SetLabel(LabelText, LabelValue: TLabel; Value: TDateTime);
begin
  SetLabel(LabelText, LabelValue, specialize IfThen<string>(Value > 0, DateTimeToStr(Value), 'N/A'));
end;

procedure TFrameBook.SetLabel(LabelText, LabelValue: TLabel; Value: Integer);
begin
  SetLabel(LabelText, LabelValue, specialize IfThen<string>(Value = 0, 'N/A', IntToStr(Value)));
end;

procedure TFrameBook.SetLabel(LabelText, LabelValue: TLabel; Value: TMangaBook.TOriginalRun);
var
  FromText, ToText, OriginalRunText: string;
begin
  FromText := specialize IfThen<string>(Value[rrFrom] = 0, '?', DateTimeToStr(Value[rrFrom]));
  ToText := specialize IfThen<string>(Value[rrTo] = 0,
                                      specialize IfThen<string>(FBook.SeriesState = ssOngoing, PRESENT, '?'),
                                      DateTimeToStr(Value[rrTo]));
  if (FromText = '?') and (ToText = '?') then
    OriginalRunText := 'N/A'
  else
    OriginalRunText := Format(ORIGINAL_RUN_FROM_TO, [FromText, ToText]);
  SetLabel(LabelText, LabelValue, OriginalRunText);
end;

procedure TFrameBook.SetTableOfContents;
var
  Path: string;
  RootNode, ParentNode, NextNode: TTreeNode;
  NodeText: string;
  DirectorySeparators: array of Char = ('/', '\');
  i: Integer;
begin
  TableOfContents.Items.Clear;
  RootNode := TableOfContents.Items.AddChild(nil, FBook.Caption);
  for i := 0 to Length(FDetail.Volumes) - 1 do
  begin
    Path := FDetail.Volumes[i].Path.Trim(DirectorySeparators);
    if TableOfContents.Items.FindNodeWithTextPath(Path) <> nil then continue;
    ParentNode := RootNode;
    for NodeText in Path.Split(DirectorySeparators) do
    begin
      NextNode := ParentNode.FindNode(NodeText);
      if NextNode = nil then
        ParentNode := TableOfContents.Items.AddChild(ParentNode, NodeText)
      else
        ParentNode := NextNode;
    end;
    ParentNode.Data := @FDetail.Volumes[i];
  end;
  TableOfContents.FullExpand;
  TableOfContents.Items.GetFirstNode.Selected := True;
end;

procedure TFrameBook.Initialize;
begin
  FBusy := False;
  FDefaultIcon := TPicture.Create;
  FDefaultIcon.LoadFromResourceName(HInstance, 'NO_IMAGE');
  FIconManagers := TIconManagerDict.Create([doOwnsValues]);
end;

procedure TFrameBook.Finalize;
begin
  FIconManagers.Free;
  FDefaultIcon.Free;
end;


end.

