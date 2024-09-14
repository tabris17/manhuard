unit Manhuard.Form.Book;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Manhuard.Frame.Book, Manhuard.Manga;

type

  { TFormBook }

  TFormBook = class(TForm)
    FrameBookInWindow: TFrameBook;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FIndex: Integer;
    FBook: TMangaBook;
    procedure SetBook(AValue: TMangaBook);
    procedure SetIndex(AValue: Integer);
    procedure RenewCaption;
  public
    property Book: TMangaBook read FBook write SetBook;
    property Index: Integer read FIndex write SetIndex;
    procedure BringToFront(Sender: TObject);
  end;

var
  FormBook: TFormBook;

implementation

{$R *.lfm}

{ TFormBook }

procedure TFormBook.FormCreate(Sender: TObject);
begin
  FrameBookInWindow.Initialize;
end;

procedure TFormBook.FormDestroy(Sender: TObject);
begin
  FrameBookInWindow.Finalize;
end;

procedure TFormBook.SetBook(AValue: TMangaBook);
begin
  if FBook = AValue then Exit;
  FBook := AValue;
  FrameBookInWindow.Book := FBook;
  RenewCaption;
end;

procedure TFormBook.SetIndex(AValue: Integer);
begin
  if FIndex = AValue then Exit;
  FIndex := AValue;
  RenewCaption;
end;

procedure TFormBook.RenewCaption;
begin
  if not Assigned(FBook) then Exit;
  if FIndex > 0 then
    Caption := Format('%s #%d', [FBook.Title, FIndex])
  else
    Caption := FBook.Title;
end;

procedure TFormBook.BringToFront(Sender: TObject);
begin
  Self.SetFocus;
end;

end.

