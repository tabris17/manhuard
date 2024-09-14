unit Manhuard.Page.About;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Manhuard.Frame.Fullwidth;

type

  { TPageAbout }

  TPageAbout = class(TFullwidthPage)
    ImageBrand: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelDeveloper: TLabel;
    LabelSourceCode: TLabel;
    LabelDocument: TLabel;
    LabelAppName: TLabel;
    PanelAbout: TPanel;
    StaticTextDescription: TStaticText;
    procedure FrameResize(Sender: TObject);
    procedure LabelDocumentClick(Sender: TObject);
    procedure LabelSourceCodeClick(Sender: TObject);
  private

  public
    procedure Initialize; override;
  end;

implementation

{$R *.lfm}

uses LCLIntf, FileInfo, Manhuard.Strings;

{ TPageAbout }

procedure TPageAbout.FrameResize(Sender: TObject);
var
  DeltaX, DeltaY: Integer;
begin
  inherited;
  DeltaX := Width - PanelAbout.Width;
  DeltaY := Height - PanelAbout.Height;
  if DeltaX > 0 then PanelAbout.Left := DeltaX div 2 else PanelAbout.Left := 0;
  if DeltaY > Navbar.Height then PanelAbout.Top := DeltaY div 2 else PanelAbout.Top := Navbar.Height;
end;

procedure TPageAbout.LabelDocumentClick(Sender: TObject);
begin
  OpenURL(LabelDocument.Caption);
end;

procedure TPageAbout.LabelSourceCodeClick(Sender: TObject);
begin
  OpenURL(LabelSourceCode.Caption);
end;

procedure TPageAbout.Initialize;
var
  FileVersion: String;
begin
  inherited;
  with TFileVersionInfo.Create(Self) do
  begin
    ReadFileInfo;
    FileVersion := VersionStrings.Values['FileVersion'];
  end;
  LabelAppName.Caption := Format('%s v%s', [APP_NAME, FileVersion]);
  StaticTextDescription.Caption := APP_DESCRIPTION;
  LabelSourceCode.Caption := URL_SOURCE;
  LabelDocument.Caption := URL_DOCUMENT;
  LabelDeveloper.Caption := DEVELOPER;
end;

end.

