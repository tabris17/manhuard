unit Manhuard.Frame.Fullwidth;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls, Manhuard.Frame;

type

  { TFullwidthPage }

  TFullwidthPage = class(TFramePage)
    ImageList: TImageList;
    LabelTitle: TLabel;
    Navbar: TToolBar;
    ToolButtonBack: TToolButton;
    procedure FrameResize(Sender: TObject);
    procedure ToolButtonBackClick(Sender: TObject);
  protected
    procedure SetTitle(AValue: TCaption); override;
    procedure PageBackwardQuery(var CanBackward: Boolean); virtual;
  public
    procedure Initialize; override;
  end;

implementation

{$R *.lfm}

uses Manhuard.Form.Main;

{ TFullwidthPage }

procedure TFullwidthPage.FrameResize(Sender: TObject);
var
  LabelWidth: Integer;
  Button: Pointer;
begin
  LabelWidth := Navbar.Width - Navbar.Indent * Navbar.ButtonCount;
  for Button in Navbar.ButtonList do
    Dec(LabelWidth, TToolButton(Button).Width * 2);
  LabelTitle.Width := LabelWidth;
end;

procedure TFullwidthPage.ToolButtonBackClick(Sender: TObject);
var
  CanBackward: Boolean = True;
begin
  PageBackwardQuery(CanBackward);
  if CanBackward then FormMain.Backward;
end;

procedure TFullwidthPage.SetTitle(AValue: TCaption);
begin
  inherited SetTitle(AValue);
  LabelTitle.Caption := AValue;
end;

procedure TFullwidthPage.PageBackwardQuery(var CanBackward: Boolean);
begin
  { not implemented }
end;

procedure TFullwidthPage.Initialize;
begin
  inherited;
  Title := LabelTitle.Caption;
end;

end.

