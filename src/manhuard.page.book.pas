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
    FBook: TMangaBook;
    procedure SetBook(AValue: TMangaBook);

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
  if FBook = AValue then Exit;
  FBook := AValue;
  LabelTitle.Caption := FBook.Title;
  FrameBookInPage.Book := FBook;
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

