unit Manhuard.Page.Book;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Manhuard.Frame.Fullwidth, Manhuard.Manga, Manhuard.Frame.Book;

type

  { TPageBook }

  TPageBook = class(TFullwidthPage)
    FrameBookInPage: TFrameBook;
  private
    procedure SetBook(AValue: TMangaBook);
  protected
    procedure PageBackwardQuery(var CanBackward: Boolean); override;
  public
    property Book: TMangaBook write SetBook;
    procedure Initialize; override;
    procedure Finalize; override;
  end;

implementation

{$R *.lfm}

{ TPageBook }

procedure TPageBook.SetBook(AValue: TMangaBook);
begin
  LabelTitle.Caption := AValue.Caption;
  FrameBookInPage.Book := AValue;
end;

procedure TPageBook.PageBackwardQuery(var CanBackward: Boolean);
begin
  FrameBookInPage.Reset;
end;

procedure TPageBook.Initialize;
begin
  inherited Initialize;
  FrameBookInPage.Initialize;
end;

procedure TPageBook.Finalize;
begin
  FrameBookInPage.Finalize;
  inherited Finalize;
end;


end.

