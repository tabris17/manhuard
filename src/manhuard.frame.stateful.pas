unit Manhuard.Frame.Stateful;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, Graphics, Dialogs, Manhuard.Frame;

type

  { TStatefulPage }

  TStatefulPage = class(TFramePage)
  private
    FStatusStrings: array of string;
    function GetStatusBar: TStatusBar;
    function GetStatusText(Index: Integer): string;
    procedure SetStatusText(Index: Integer; AValue: string);
  protected
    property StatusBar: TStatusBar read GetStatusBar;
    property StatusText[Index: Integer]: string read GetStatusText write SetStatusText;
    procedure VisibleChanged; override;
    procedure InitStatusBar; virtual;
  public

  end;

implementation

{$R *.lfm}

uses Manhuard.Form.Main;

{ TStatefulPage }

function TStatefulPage.GetStatusBar: TStatusBar;
begin
  Result := FormMain.StatusBar;
end;

function TStatefulPage.GetStatusText(Index: Integer): string;
begin
  Result := FStatusStrings[Index];
end;

procedure TStatefulPage.SetStatusText(Index: Integer; AValue: string);
begin
  if Index >= Length(FStatusStrings) then SetLength(FStatusStrings, Index + 1);
  FStatusStrings[Index] := AValue;
  if Index < StatusBar.Panels.Count then StatusBar.Panels[Index].Text := AValue;
end;

procedure TStatefulPage.VisibleChanged;
var
  i: Integer;
begin
  inherited VisibleChanged;
  StatusBar.Panels.Clear;
  if not Visible then Exit;
  InitStatusBar;
  for i := 0 to Length(FStatusStrings) - 1 do
    if i < StatusBar.Panels.Count then
      StatusBar.Panels[i].Text := FStatusStrings[i];
end;

procedure TStatefulPage.InitStatusBar;
begin
  StatusBar.Panels.Add;
end;

end.

