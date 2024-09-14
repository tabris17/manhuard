unit Manhuard.Page.Home;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls,
  Manhuard.Frame;

type

  { TPageHome }

  TPageHome = class(TFramePage)
    ImageBanner: TImage;
    procedure FrameResize(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

{ TPageHome }

procedure TPageHome.FrameResize(Sender: TObject);
begin
  ImageBanner.Left := (Width - ImageBanner.Width) div 2;
end;

end.

