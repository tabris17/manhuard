unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, Menus, ActnList, Buttons;

type

  { TFormMain }

  TFormMain = class(TForm)
    ActionOpen: TAction;
    ActionList: TActionList;
    ImageList: TImageList;
    ListView1: TListView;
    MainMenu: TMainMenu;
    MenuItemFile: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemOpen: TMenuItem;
    LeftPanel: TPanel;
    RightPanel: TPanel;
    Separator1: TMenuItem;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar: TStatusBar;
    ToolBar: TToolBar;
    LeftBar: TToolBar;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure ActionOpenExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

uses fphttpclient, opensslsockets, openssl;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
end;

procedure TFormMain.ActionOpenExecute(Sender: TObject);
begin
  //
end;

end.

