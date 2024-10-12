unit Manhuard.Form.View;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, Manhuard.Manga, Manhuard.Frame.Book;

type

  { TFormView }

  TFormView = class(TForm)
    ImageView: TImage;
    procedure FormCreate(Sender: TObject);
  private
    FBook: TMangaBook;
    FVolumes: TMangaBook.TVolumeArray;
    FPages: TMangaBook.TPageArray;
    FVolumeIndex, FPageIndex: Integer;
    procedure Initialize;
  protected
    procedure LoadSuccess(Sender: TMangaManager.TReadPageWork; Return: TPicture);
    procedure LoadFailure(Sender: TMangaManager.TReadPageWork; Error: TMangaManager.TReadPageWork.TError);
  public
    function Open(Source: TFrameBook): Boolean;
    function GotoNextVolume: Boolean;
    function GotoPreviousVolume: Boolean;
    procedure Close;
  end;

var
  FormView: TFormView;

const
  FORM_CAPTION = '%s/%s/%s';

implementation

{$R *.lfm}

{ TFormView }

procedure TFormView.FormCreate(Sender: TObject);
begin
  Initialize;
end;

procedure TFormView.Initialize;
begin
  FBook := nil;
  FVolumes := nil;
  FPages := nil;
  Caption := EmptyStr;
  FVolumeIndex := -1;
  FPageIndex := -1;
  ImageView.Picture.Clear;
end;

procedure TFormView.LoadSuccess(Sender: TMangaManager.TReadPageWork; Return: TPicture);
begin
  ImageView.Picture.Assign(Return);
end;

procedure TFormView.LoadFailure(Sender: TMangaManager.TReadPageWork; Error: TMangaManager.TReadPageWork.TError);
begin

end;

function TFormView.Open(Source: TFrameBook): Boolean;
var
  SelectedItem: TListItem;
  SelectedVolume: TMangaBook.PVolume;
  SelectedPage: TMangaBook.PPage;
  i: Integer;
begin
  Initialize;
  SelectedItem := Source.PageListView.Selected;
  if SelectedItem = nil then Exit(False);
  FPageIndex := SelectedItem.Index;
  SelectedVolume := Source.SelectedVolume;
  FBook := Source.Book;
  FVolumes := Source.Volumes;
  FPages := Source.Pages[SelectedVolume];
  for i := 0 to Length(FVolumes) - 1 do if @FVolumes[i] = SelectedVolume then break;
  FVolumeIndex := i;
  SelectedPage := @FPages[FPageIndex];
  Caption := Format(FORM_CAPTION, [FBook.Name, SelectedVolume^.Path, SelectedPage^.Name]);
  MangaManager.ReadPage(FBook, SelectedVolume, SelectedPage, @LoadSuccess, @LoadFailure);
end;

function TFormView.GotoNextVolume: Boolean;
begin
  if FVolumeIndex = Length(FVolumes) - 1 then Exit(False);
  Inc(FVolumeIndex);
  Result := False;
end;

function TFormView.GotoPreviousVolume: Boolean;
begin
  if FVolumeIndex = 0 then Exit(False);
  Dec(FVolumeIndex);
  Result := False;
end;

procedure TFormView.Close;
begin
  Initialize;
  inherited;
end;

end.

