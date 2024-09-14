unit Manhuard.Page.Options;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls,
  Manhuard.Frame;

type

  { TPageOptions }

  TPageOptions = class(TFramePage)
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    ScrollBox: TScrollBox;
  private

  public

  end;

implementation

{$R *.lfm}

end.

