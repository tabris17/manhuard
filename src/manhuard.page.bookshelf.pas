unit Manhuard.Page.Bookshelf;

{$mode ObjFPC}{$H+}
{$optimization autoInline}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ImgList, Menus, StdCtrls, ExtCtrls, ActnList, Graphics, Generics.Collections,
  Manhuard.Frame.Stateful, Manhuard.Manga, Manhuard.Manga.Reader, Manhuard.Helper.ListView;

type

  TFormList = specialize TList<TForm>;

  { TIconLoader }

  TIconLoader = class abstract (TListViewIconManager.TLoadIconWork)
  protected
    function Load: TPicture;
  end;

  { TSmallIconLoader }

  TSmallIconLoader = class (TIconLoader)
  public
    function Execute: TPicture; override;
  end;

  { TLargeIconLoader }

  TLargeIconLoader = class (TIconLoader)
  public
    function Execute: TPicture; override;
  end;

  { TPageBookshelf }

  TPageBookshelf = class(TStatefulPage)
    ActionOpenInNewWindow: TAction;
    ActionRefresh: TAction;
    ActionFilter: TAction;
    ActionRemove: TAction;
    ActionOpen: TAction;
    ActionLargeCoversView: TAction;
    ActionSmallCoversView: TAction;
    ActionDetailsView: TAction;
    ActionList: TActionList;
    EditFilter: TEdit;
    ImageListView: TImageList;
    ImageListToolBar: TImageList;
    ImageListToolBarGrey: TImageList;
    ListView: TListView;
    MenuItem1: TMenuItem;
    MenuItemEmpty: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    PanelFilter: TPanel;
    OpenMenu: TPopupMenu;
    ListViewMenu: TPopupMenu;
    OpenedMenu: TPopupMenu;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    ToolButtonOpened: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ViewMenu: TPopupMenu;
    ToolBar: TToolBar;
    ToolButtonView: TToolButton;
    procedure ActionFilterExecute(Sender: TObject);
    procedure ActionDetailsViewExecute(Sender: TObject);
    procedure ActionLargeCoversViewExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionOpenInNewWindowExecute(Sender: TObject);
    procedure ActionRefreshExecute(Sender: TObject);
    procedure ActionRemoveExecute(Sender: TObject);
    procedure ActionSmallCoversViewExecute(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure ListViewCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ListViewData(Sender: TObject; Item: TListItem);
    procedure ListViewDblClick(Sender: TObject);
    procedure ListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure MangaLoading(Event: TMangaEvent);
    procedure MangaLoaded(Event: TMangaEvent);
    procedure ListViewLoadIcon(Sender: TListViewIconManager; ItemData: TListViewIconManager.TItemData;
      out LoadIconWork: TListViewIconManager.TLoadIconWorkBase);
  private
    FToolBarButtonsWidth: Integer;
    FSmallIconManager, FLargeIconManager: TListViewIconManager;
    FOpenedBooks: TFormList;
    FSmallDefaultIcon, FLargeDefaultIcon: TPicture;
  protected
    procedure VisibleChanged; override;
    procedure InitStatusBar; override;
  public
    procedure Initialize; override;
    procedure Finalize; override;
    procedure FormBookClose(Sender: TObject; var CloseAction: TCloseAction);
  end;


resourcestring
  MSG_TOTAL = 'Total %d';
  MSG_SEL_TOTAL = 'Total %d/%d';
  MSG_SEL_COUNT = 'Sel %d';
  MSG_LOADING = 'Loading';
  MSG_YES = 'Yes';
  MSG_NO = 'No';
  LV_SHORT_CAPTION = '%s'#13#10'by %s';

const
  STATUS_PANEL_1_WIDTH = 150;
  STATUS_PANEL_2_WIDTH = 60;
  ICON_SMALL_WIDTH = 60;
  ICON_SMALL_HEIGHT = 80;
  ICON_LARGE_WIDTH = 180;
  ICON_LARGE_HEIGHT = 240;


implementation

{$R *.lfm}

uses LCLType, Manhuard.Form.Main, Manhuard.Types, Manhuard.Page.Book, Manhuard.WorkPool,
  Manhuard.Config, Manhuard.Form.Book, Manhuard.Helper.Picture;

{ TIconLoader }

function TIconLoader.Load: TPicture;
var
  Book: TMangaBook;
begin
  Book := TMangaBook(ItemData);
  Result := Book.Cover;
end;

{ TSmallIconLoader }

function TSmallIconLoader.Execute: TPicture;
begin
  Result := Load;
  if Assigned(Result) then Result.Scale(ICON_SMALL_WIDTH, ICON_SMALL_HEIGHT);
end;

{ TLargeIconLoader }

function TLargeIconLoader.Execute: TPicture;
begin
  Result := Load;
  if Assigned(Result) then Result.Scale(ICON_LARGE_WIDTH, ICON_LARGE_HEIGHT);
end;

{ TPageBookshelf }

procedure TPageBookshelf.FrameResize(Sender: TObject);
begin
  PanelFilter.Width := Width - FToolBarButtonsWidth;
  EditFilter.Width := PanelFilter.Width - 6;
end;

procedure TPageBookshelf.ListViewCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  if State = [] then Exit;
  if ListView.ViewStyle = vsIcon then
    FLargeIconManager.DrawIcon(Item, haCenter, vaBottom)
  else
    FSmallIconManager.DrawIcon(Item);
end;

procedure TPageBookshelf.ListViewData(Sender: TObject; Item: TListItem);
var
  Book: TMangaBook;
  Writers: String;
begin
  Book := MangaManager.Books[Item.Index];
  Item.Data := Book;
  Writers := string.Join(', ', Book.Writers);
  if ListView.ViewStyle = vsIcon then
    Item.Caption := specialize IfThen<string>(Writers = EmptyStr, Book.Caption, Format(LV_SHORT_CAPTION, [Book.Caption, Writers]))
  else
  begin
    Item.Caption := Book.Caption;
    Item.SubItems.Add(specialize IfThen<string>(Book.Volumes > 0, IntToStr(Book.Volumes), EmptyStr));
    Item.SubItems.Add(Writers);
    Item.SubItems.Add(specialize IfThen<string>(Book.ReleaseYear > 0, IntToStr(Book.ReleaseYear), EmptyStr));
    Item.SubItems.Add(Book.Region);
    Item.SubItems.Add(string.Join(', ', Book.Genre));
    Item.SubItems.Add(specialize IfThen<string>(Book.LastUpdated > 0, DateTimeToStr(Book.LastUpdated), EmptyStr));
    case Book.SeriesState of
      ssCompleted: Item.SubItems.Add(MSG_YES);
      ssOngoing: Item.SubItems.Add(MSG_NO);
    else Item.SubItems.Add(EmptyStr);
    end;
  end;
end;

procedure TPageBookshelf.ListViewDblClick(Sender: TObject);
begin
  ActionOpen.Execute;
end;

procedure TPageBookshelf.ListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  SelCount, TotalCount: Integer;
  SelOnlyOne: Boolean;
begin
  SelCount := ListView.SelCount;
  TotalCount := ListView.Items.Count;
  SelOnlyOne := SelCount = 1;

  ActionRemove.Enabled := SelCount > 0;
  ActionOpen.Enabled := SelOnlyOne;
  ActionOpenInNewWindow.Enabled := SelOnlyOne;

  StatusText[0] :=
    specialize IfThen<string>(SelOnlyOne, Format(MSG_SEL_TOTAL, [Item.Index + 1, TotalCount]), Format(MSG_TOTAL, [TotalCount]));
  StatusText[1] := Format(MSG_SEL_COUNT, [SelCount]);
end;

procedure TPageBookshelf.MangaLoading(Event: TMangaEvent);
begin
  ActionOpen.Enabled := False;
  ActionOpenInNewWindow.Enabled := False;
  ActionRemove.Enabled := False;
  ActionRefresh.Enabled := False;
  ListView.Items.Count := 0;
  ListView.Cursor := crAppStart;
  StatusText[0] := MSG_LOADING;
end;

procedure TPageBookshelf.MangaLoaded(Event: TMangaEvent);
var
  BooksCount: Integer;
begin
  ActionRefresh.Enabled := True;
  BooksCount := MangaManager.Books.Count;
  ListView.Items.Count := BooksCount;
  ListView.Cursor := crDefault;
  StatusText[0] := Format(MSG_TOTAL, [BooksCount]);
end;

procedure TPageBookshelf.ListViewLoadIcon(Sender: TListViewIconManager; ItemData: TListViewIconManager.TItemData;
  out LoadIconWork: TListViewIconManager.TLoadIconWorkBase);
begin
  if Sender = FSmallIconManager then
    LoadIconWork := TSmallIconLoader.Create(ItemData)
  else if Sender = FLargeIconManager then
    LoadIconWork := TLargeIconLoader.Create(ItemData)
  else
    LoadIconWork := nil;
end;

procedure TPageBookshelf.VisibleChanged;
begin
  inherited VisibleChanged;
  if Visible and ListView.CanFocus then ListView.SetFocus;
end;

procedure TPageBookshelf.InitStatusBar;
begin
  with StatusBar.Panels.Add do Width := STATUS_PANEL_1_WIDTH;
  with StatusBar.Panels.Add do Width := STATUS_PANEL_2_WIDTH;
end;

procedure TPageBookshelf.Initialize;
var
  Button: Pointer;
begin
  inherited;

  FSmallDefaultIcon := TPicture.Create;
  FSmallDefaultIcon.LoadFromResourceName(HInstance, 'NO_COVER_SMALL');
  FLargeDefaultIcon := TPicture.Create;
  FLargeDefaultIcon.LoadFromResourceName(HInstance, 'NO_COVER_LARGE');

  FSmallIconManager := TListViewIconManager.Create(ListView);
  FSmallIconManager.DefaultIcon := FSmallDefaultIcon;
  FSmallIconManager.OnLoadIcon := @ListViewLoadIcon;
  FLargeIconManager := TListViewIconManager.Create(ListView);
  FLargeIconManager.DefaultIcon := FLargeDefaultIcon;
  FLargeIconManager.OnLoadIcon := @ListViewLoadIcon;

  FOpenedBooks := TFormList.Create;

  FToolBarButtonsWidth := ToolBar.ButtonCount + 26;
  for Button in ToolBar.ButtonList do Inc(FToolBarButtonsWidth, TToolButton(Button).Width);

  case Config.Bookshelf.ListView.ViewStyle of
    vsReport: ActionDetailsView.Execute;
    vsIcon: ActionLargeCoversView.Execute;
    vsSmallIcon: ActionSmallCoversView.Execute;
  end;

  with MangaManager do
  begin
    AddEventListener(etLoading, @MangaLoading);
    AddEventListener(etLoaded, @MangaLoaded);
    Load;
  end;
end;

procedure TPageBookshelf.Finalize;
begin
  Config.Bookshelf.ListView.ViewStyle := ListView.ViewStyle;

  with MangaManager do
  begin
    RemoveEventListener(etLoading, @MangaLoading);
    RemoveEventListener(etLoaded, @MangaLoaded);
  end;
      
  FSmallDefaultIcon.Free;
  FLargeDefaultIcon.Free;
  FSmallIconManager.Free;
  FLargeIconManager.Free;
  FOpenedBooks.Free;
  inherited;
end;

procedure TPageBookshelf.ActionDetailsViewExecute(Sender: TObject);
begin
  ListView.ViewStyle := vsReport;
  ListView.Repaint;
  ActionDetailsView.Checked := True;
end;

procedure TPageBookshelf.ActionLargeCoversViewExecute(Sender: TObject);
begin
  ListView.ViewStyle := vsIcon;
  ListView.Repaint;
  ActionLargeCoversView.Checked := True;
end;

procedure TPageBookshelf.ActionSmallCoversViewExecute(Sender: TObject);
begin
  ListView.ViewStyle := vsSmallIcon;
  ListView.Repaint;
  ActionSmallCoversView.Checked := True;
end;

procedure TPageBookshelf.ActionFilterExecute(Sender: TObject);
begin
  ActionFilter.Checked := not ActionFilter.Checked;
end;

procedure TPageBookshelf.ActionOpenExecute(Sender: TObject);
begin
  (FormMain.Navigate(fpiBook) as TPageBook).Book := MangaManager.Books[ListView.LastSelected.Index];
end;

procedure TPageBookshelf.ActionOpenInNewWindowExecute(Sender: TObject);
var
  FormBook: TFormBook;
  MenuItem: TMenuItem;
  TheBook: TMangaBook;
  Index: Integer = 0;
  OriginalIndex: Integer;
begin
  TheBook := MangaManager.Books[ListView.LastSelected.Index];
  FormBook := TFormBook.Create(Self);
  with FormBook do
  begin
    OnClose := @FormBookClose;
    ShowOnTop;
    Book := TheBook;
  end;
  FOpenedBooks.Add(FormBook);
  MenuItemEmpty.Visible := False;
  for MenuItem in OpenedMenu.Items do
  begin
    if TheBook.Title = MenuItem.Hint then
    begin
      OriginalIndex := (MenuItem.Owner as TFormBook).Index;
      if OriginalIndex >= Index then Index := OriginalIndex + 1;
    end;
  end;
  FormBook.Index := Index;
  MenuItem := TMenuItem.Create(FormBook);
  with MenuItem do
  begin
    Caption := FormBook.Caption;
    Hint := TheBook.Title;
    OnClick := @FormBook.BringToFront;
  end;
  OpenedMenu.Items.Add(MenuItem);
end;

procedure TPageBookshelf.FormBookClose(Sender: TObject; var CloseAction: TCloseAction);
var
  FormBook: TForm;
begin
  FormBook := Sender as TForm;
  FOpenedBooks.Remove(FormBook);
  CloseAction := caFree;
  if FOpenedBooks.Count = 0 then MenuItemEmpty.Visible := True;
end;

procedure TPageBookshelf.ActionRefreshExecute(Sender: TObject);
begin
  MangaManager.Load;
end;

procedure TPageBookshelf.ActionRemoveExecute(Sender: TObject);
begin

end;


end.

