unit Manhuard.Form.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, Menus, ExtCtrls, ActnList, PopupNotifier, PythonEngine,
  Generics.Collections, SQLite3Conn, SQLDB,
  Manhuard.Strings, Manhuard.Debug, Manhuard.Python.Module, Manhuard.Types, Manhuard.Frame, Manhuard.Frame.Fullwidth;

type

  THistory = specialize TStack<TPageIndex>;

  { TFormMain }

  TFormMain = class(TForm)
    ActionAbout: TAction;
    ActionAddManga: TAction;
    ActionBookmark: TAction;
    ActionSearch: TAction;
    ActionOptions: TAction;
    ActionDownloading: TAction;
    ActionBookshelf: TAction;
    ActionHome: TAction;
    ActionExit: TAction;
    ActionList: TActionList;
    ApplicationProperties: TApplicationProperties;
    ImageListGrey: TImageList;
    ImageListSolid: TImageList;
    ImageListLight: TImageList;
    MainMenu: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    PopupNotifier: TPopupNotifier;
    PythonEngine: TPythonEngine;
    PythonIO: TPythonInputOutput;
    PythonModule: TPythonModule;
    SQLite3Connection: TSQLite3Connection;
    SQLQuery: TSQLQuery;
    SQLTransaction: TSQLTransaction;
    StatusBar: TStatusBar;
    ToolbarFilling: TLabel;
    ToolButtonAbout: TToolButton;
    ToolButton9: TToolButton;
    ToolButtonOptions: TToolButton;
    ToolButtonSearch: TToolButton;
    ToolButtonBookmark: TToolButton;
    ToolButtonAddManga: TToolButton;
    TrayMenu: TPopupMenu;
    Navbar: TToolBar;
    ToolButtonMainMenu: TToolButton;
    ToolButtonHome: TToolButton;
    ToolButtonBookshelf: TToolButton;
    ToolButtonDownloading: TToolButton;
    TrayIcon: TTrayIcon;
    procedure ActionAboutExecute(Sender: TObject);
    procedure ActionAddMangaExecute(Sender: TObject);
    procedure ActionBookmarkExecute(Sender: TObject);
    procedure ActionBookshelfExecute(Sender: TObject);
    procedure ActionDownloadingExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionHomeExecute(Sender: TObject);
    procedure ActionOptionsExecute(Sender: TObject);
    procedure ActionSearchExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure PythonIOSendData(Sender: TObject; const Data: AnsiString);
    procedure NavbarResize(Sender: TObject);
    procedure ToolButtonMainMenuClick(Sender: TObject);
  private
    FToolBarButtonsHeight: Integer;
    FPages: array[TPageIndex] of TFramePage;
    FCurrentPageIndex: TPageIndex;
    FHistory: THistory;
    procedure CalcToolBarButtonsHeight;
    procedure CreatePages;
    function GetCurrentPage: TFramePage;
    procedure SetCurrentPageIndex(AValue: TPageIndex);
  public
    property CurrentPageIndex: TPageIndex read FCurrentPageIndex write SetCurrentPageIndex;
    property CurrentPage: TFramePage read GetCurrentPage;
    function Backward: TFramePage;
    function Navigate(PageIndex: TPageIndex): TFramePage;
  end;

var
  FormMain: TFormMain;

const
  FORM_MIN_WIDTH = 480;
  FORM_MIN_HEIGHT = 360;


implementation

{$R *.lfm}

uses Manhuard.Pages;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FHistory := THistory.Create;
  Caption := APP_NAME; 
  Constraints.MinWidth := FORM_MIN_WIDTH;
  Constraints.MinHeight := FORM_MIN_HEIGHT;
  CalcToolBarButtonsHeight;
  CreatePages;
  Navigate(fpiHome);
  ActionHome.Checked:=True;
  TrayIcon.Icon := Application.Icon;
end;

procedure TFormMain.ActionExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.ActionBookshelfExecute(Sender: TObject);
begin
  Navigate(fpiBookshelf);
end;

procedure TFormMain.ActionBookmarkExecute(Sender: TObject);
begin
  Navigate(fpiBookmark);
end;

procedure TFormMain.ActionAddMangaExecute(Sender: TObject);
begin
  Navigate(fpiAddManga);
end;

procedure TFormMain.ActionAboutExecute(Sender: TObject);
begin
  Navigate(fpiAbout);
end;

procedure TFormMain.ActionDownloadingExecute(Sender: TObject);
begin
  Navigate(fpiDownloading);
end;

procedure TFormMain.ActionHomeExecute(Sender: TObject);
begin
  Navigate(fpiHome);
end;

procedure TFormMain.ActionOptionsExecute(Sender: TObject);
begin
  Navigate(fpiOptions);
end;

procedure TFormMain.ActionSearchExecute(Sender: TObject);
begin
  Navigate(fpiSearch);
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
end;

procedure TFormMain.PythonIOSendData(Sender: TObject; const Data: AnsiString);
begin
  DebugLn(Data);
end;

procedure TFormMain.NavbarResize(Sender: TObject);
begin
  ToolbarFilling.Height := Navbar.Height - FToolBarButtonsHeight;
end;

procedure TFormMain.ToolButtonMainMenuClick(Sender: TObject);
begin
  with ToolButtonMainMenu do
   PopupMenu.PopUp(ClientOrigin.X + Width, ClientOrigin.Y - ToolButtonMainMenu.Top);
end;

procedure TFormMain.CalcToolBarButtonsHeight;
var
  Button: Pointer;
begin
  FToolBarButtonsHeight := Navbar.Indent * Navbar.ButtonCount + ToolButtonMainMenu.Top;
  for Button in Navbar.ButtonList do
    Inc(FToolBarButtonsHeight, TToolButton(Button).Height);
end;

procedure TFormMain.CreatePages;
var
  PageIndex: TPageIndex;
  PageClass: TPageClass;
  Page: TFramePage;
begin
  for PageIndex := Low(PageClasses) to High(PageClasses) do
  begin
    PageClass := PageClasses[PageIndex];
    Page := PageClass.Create(Self);
    FPages[PageIndex] := Page;
    Page.Parent := Self;
    Page.Initialize;
  end;
end;

function TFormMain.GetCurrentPage: TFramePage;
begin
  Result := FPages[FCurrentPageIndex];
end;

procedure TFormMain.SetCurrentPageIndex(AValue: TPageIndex);
var
  Index: TPageIndex;
  Page: TFramePage;
  IsNotFullwidthPage: boolean;
begin
  CurrentPage.Visible := False;;
  for Index := Low(FPages) to High(FPages) do
  begin
    Page := FPages[Index];
    if Index = AValue then
    begin
      IsNotFullwidthPage := not (Page is TFullwidthPage);
      Navbar.Visible := IsNotFullwidthPage;
      StatusBar.Visible := IsNotFullwidthPage;
    end;
  end;
  FCurrentPageIndex := AValue;
  CurrentPage.Visible := True;
end;

function TFormMain.Backward: TFramePage;
begin
  if FHistory.Count > 0 then
    CurrentPageIndex := FHistory.Pop;
  Result := CurrentPage;
end;

function TFormMain.Navigate(PageIndex: TPageIndex): TFramePage;
var
  Page: TFramePage;
begin
  Page := FPages[PageIndex];
  if not (Page is TFullwidthPage) then
    FHistory.Clear;
  FHistory.Push(CurrentPageIndex);
  CurrentPageIndex := PageIndex;
  Result := CurrentPage;
end;


end.

