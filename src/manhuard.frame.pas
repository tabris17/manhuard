unit Manhuard.Frame;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls;

type

  TPageClass = class of TFramePage;

  { TFramePage }

  TFramePage = class(TFrame)
  private
    FTitle: TCaption;
  protected
    procedure SetTitle(AValue: TCaption); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    property Title: TCaption read FTitle write SetTitle;
  end;

implementation

{$R *.lfm}

{ TFramePage }

procedure TFramePage.SetTitle(AValue: TCaption);
begin
  if FTitle = AValue then Exit;
  FTitle := AValue;
end;

constructor TFramePage.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Align := alClient;
  Visible := False;
end;

destructor TFramePage.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

procedure TFramePage.Initialize;
begin
  { not implemented }
end;

procedure TFramePage.Finalize;
begin
  { not implemented }
end;


end.

