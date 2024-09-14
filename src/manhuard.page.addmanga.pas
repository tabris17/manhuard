unit Manhuard.Page.AddManga;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, ComCtrls, Dialogs, ButtonPanel, StdCtrls,
  Manhuard.Frame.Fullwidth;

type

  { TPageAddManga }

  TPageAddManga = class(TFullwidthPage)
    ButtonPanel: TButtonPanel;
    EditMangaURL: TLabeledEdit;
    ImageLogo: TImage;
    PanelAddManga: TPanel;
    StaticTextHelp: TStaticText;
    procedure CancelButtonClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private

  public
    procedure Initialize; override;
  end;


const
  EXAMPLE_MANGA_URL = 'https://www.manhuagui.com/comic/1639/';

implementation

{$R *.lfm}

uses Manhuard.Strings;

{ TPageAddManga }

procedure TPageAddManga.FrameResize(Sender: TObject);
var
  DeltaX, DeltaY: Integer;
begin
  inherited;
  DeltaX := Width - PanelAddManga.Width;
  DeltaY := Height - PanelAddManga.Height;
  PanelAddManga.Width := Width - 20;
  EditMangaURL.Width := PanelAddManga.Width;
  StaticTextHelp.Width := PanelAddManga.Width;
  PanelAddManga.Left := DeltaX div 2; 
  if DeltaY > Navbar.Height then PanelAddManga.Top := DeltaY div 2 else PanelAddManga.Top := Navbar.Height;
end;

procedure TPageAddManga.CancelButtonClick(Sender: TObject);
begin
  ToolButtonBackClick(Sender);
end;

procedure TPageAddManga.Initialize;
begin
  inherited;
  EditMangaURL.TextHint := EXAMPLE_MANGA_URL;
  StaticTextHelp.Caption := TEXT_ADD_MANGA_HELP;
end;


end.

