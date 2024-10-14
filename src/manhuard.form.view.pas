unit Manhuard.Form.View;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, ActnList,
  Manhuard.Manga, Manhuard.Frame.Book, Manhuard.Types;

type
  { TFormView }

  TFormView = class(TForm)
    ActionScrollRight: TAction;
    ActionScrollLeft: TAction;
    ActionScrollDown: TAction;
    ActionScrollUp: TAction;
    ActionPreviousVolume: TAction;
    ActionNextVolume: TAction;
    ActionPreviousPage: TAction;
    ActionNextPage: TAction;
    ActionList: TActionList;
    ImageView: TImage;
    ScrollBox: TScrollBox;
    StatusBar: TStatusBar;
    ToolBar: TToolBar;
    procedure ActionNextPageExecute(Sender: TObject);
    procedure ActionNextVolumeExecute(Sender: TObject);
    procedure ActionPreviousPageExecute(Sender: TObject);
    procedure ActionPreviousVolumeExecute(Sender: TObject);
    procedure ActionScrollDownExecute(Sender: TObject);
    procedure ActionScrollLeftExecute(Sender: TObject);
    procedure ActionScrollRightExecute(Sender: TObject);
    procedure ActionScrollUpExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ScrollBoxResize(Sender: TObject);
  private
    FSideBySideView: Boolean;
    FSizeAdaptation: TViewSizeAdaptation;
    FBook: TMangaBook;
    FVolumes: TMangaBook.TVolumeArray;
    FPages: TMangaBook.TPageArray;
    FVolumeIndex, FPageIndex: Integer;
    FPageCache: array of TPicture;
    procedure Initialize;
    procedure RearrangeView;
    procedure View;
    function Available(Worker: TMangaManager.TReadPageWork): Boolean;
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

uses Manhuard.Manga.Reader, Manhuard.Config, Manhuard.Helper.Picture;

{ TFormView }

procedure TFormView.FormCreate(Sender: TObject);
begin
  FSideBySideView := Config.View.SideBySide;
  FSizeAdaptation := Config.View.SizeAdaptation;
  //FPageCache := TPageCache.Create(PAGE_CACHE_SIZE);
  Initialize;
end;

procedure TFormView.FormDestroy(Sender: TObject);
begin
  //FPageCache.Free;
end;

procedure TFormView.ActionNextPageExecute(Sender: TObject);
begin
  if FPageIndex >= Length(FPages) - 1 then Exit;
  Inc(FPageIndex);
  View;
end;

procedure TFormView.ActionNextVolumeExecute(Sender: TObject);
begin

end;

procedure TFormView.ActionPreviousPageExecute(Sender: TObject);
begin
  if FPageIndex = 0 then Exit;
  Dec(FPageIndex);
  View;
end;

procedure TFormView.ActionPreviousVolumeExecute(Sender: TObject);
begin

end;

procedure TFormView.ActionScrollDownExecute(Sender: TObject);
var
  Y: Integer;
begin
  Y := ScrollBox.VertScrollBar.Position;
  Inc(Y, ScrollBox.VertScrollBar.Increment);
  ScrollBox.VertScrollBar.Position := Y;
end;

procedure TFormView.ActionScrollLeftExecute(Sender: TObject);
var
  X: Integer;
begin
  X := ScrollBox.HorzScrollBar.Position;
  Dec(X, ScrollBox.HorzScrollBar.Increment);
  ScrollBox.HorzScrollBar.Position := X;
end;

procedure TFormView.ActionScrollRightExecute(Sender: TObject);
var
  X: Integer;
begin
  X := ScrollBox.HorzScrollBar.Position;
  Inc(X, ScrollBox.HorzScrollBar.Increment);
  ScrollBox.HorzScrollBar.Position := X;
end;

procedure TFormView.ActionScrollUpExecute(Sender: TObject);
var
  Y: Integer;
begin
  Y := ScrollBox.VertScrollBar.Position;
  Dec(Y, ScrollBox.VertScrollBar.Increment);
  ScrollBox.VertScrollBar.Position := Y;
end;

procedure TFormView.ScrollBoxResize(Sender: TObject);
begin
  RearrangeView;
end;

procedure TFormView.Initialize;
begin
  FBook := nil;
  FVolumes := nil;
  FPages := nil;
  Caption := EmptyStr;
  FVolumeIndex := -1;
  FPageIndex := -1;
end;

procedure TFormView.RearrangeView;
var
  ContainerWidth, ContainerHeight: Integer;
begin
  ContainerWidth := ScrollBox.ClientWidth;
  ContainerHeight := ScrollBox.ClientHeight;

  case FSizeAdaptation of
    vsaOneSide: ImageView.Picture.Scale(ContainerWidth, ContainerHeight);
    vsaFull: ImageView.Picture.Scale(ContainerWidth, ContainerHeight);
  end;

  ImageView.Left := specialize IfThen<Integer>(ImageView.Width < ContainerWidth, (ContainerWidth - ImageView.Width) div 2, 0);
  ImageView.Top := specialize IfThen<Integer>(ImageView.Height < ContainerHeight, (ContainerHeight - ImageView.Height) div 2, 0);
end;

procedure TFormView.View;
var
  SelectedVolume: TMangaBook.PVolume;
  SelectedPage: TMangaBook.PPage;
begin
  SelectedVolume := @FVolumes[FVolumeIndex];
  SelectedPage := @FPages[FPageIndex];
  Caption := Format(FORM_CAPTION, [FBook.Name, SelectedVolume^.Path, SelectedPage^.Name]);
  MangaManager.ReadPage(FBook, SelectedVolume, SelectedPage, @LoadSuccess, @LoadFailure);
  ImageView.Picture.Clear;
  ScrollBox.HorzScrollBar.Position := 0;
  ScrollBox.VertScrollBar.Position := 0;
  ActionPreviousPage.Enabled := FPageIndex > 0;
  ActionNextPage.Enabled := FPageIndex < Length(FPages) - 1;
  ActionPreviousVolume.Enabled := FVolumeIndex > 0;
  ActionNextVolume.Enabled := FVolumeIndex < Length(FVolumes) - 1;
end;

function TFormView.Available(Worker: TMangaManager.TReadPageWork): Boolean;
begin
  Result := (Worker as TMangaPageLoader).Page = @FPages[FPageIndex];
end;

procedure TFormView.LoadSuccess(Sender: TMangaManager.TReadPageWork; Return: TPicture);
begin
  try
    if not Available(Sender) then Exit;
    ImageView.Picture := Return;
    RearrangeView;
  finally
    Return.Free;
  end;
end;

procedure TFormView.LoadFailure(Sender: TMangaManager.TReadPageWork; Error: TMangaManager.TReadPageWork.TError);
begin
  if Available(Sender) then ShowMessage(Error.Message);
end;

function TFormView.Open(Source: TFrameBook): Boolean;
var
  SelectedItem: TListItem;
  SelectedVolume: TMangaBook.PVolume;
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
  View;
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

