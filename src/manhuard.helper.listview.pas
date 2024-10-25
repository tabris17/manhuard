unit Manhuard.Helper.ListView;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, Graphics, Generics.Collections, Manhuard.WorkPool;

const
  DEFAULT_CACHE_SIZE_UBOUND = 512;
  DEFAULT_CACHE_SIZE_LBOUND = 256;

type

  { TListViewIconManager }

  TListViewIconManager = class(TPersistent)
  type
    TItemData = Pointer;
    THorizontalAlign = (haLeft, haCenter, haRight);
    TVerticalAlign = (vaTop, vaCenter, vaBottom);
    TIconCache = specialize TObjectDictionary<TItemData, TPicture>;
    TLoadIconWorkBase = specialize TWork<TPicture>;
    TLoadIconWork = class abstract (TLoadIconWorkBase)
    private
      FItemData: TItemData;
    public
      constructor Create(ItemData: TItemData);
      property ItemData: TItemData read FItemData;
    end;
    TPendingList = specialize TList<TItemData>;
    TLoadIconEvent = procedure (Sender: TListViewIconManager; ItemData: TItemData; out LoadIconWork: TLoadIconWorkBase) of object;
  private
    FListView: TCustomListView;
    FDefaultIcon: TPicture;
    FIconCache: TIconCache;
    FPendingList: TPendingList;
    FCacheSizeUBound, FCacheSizeLBound: SizeInt;
    FOnLoadIcon: TLoadIconEvent;
  public
    constructor Create(ListView: TCustomListView);
    destructor Destroy; override;
    procedure DrawIcon(Item: TListItem; HAlign: THorizontalAlign = haLeft; VAlign: TVerticalAlign = vaTop);
    procedure LoadSuccess(Sender: TLoadIconWorkBase; Return: TPicture);
    procedure LoadComplete(Sender: TLoadIconWorkBase);
    property Cache: TIconCache read FIconCache;
    property DefaultIcon: TPicture read FDefaultIcon write FDefaultIcon;
    property CacheSizeLBound: SizeInt read FCacheSizeLBound write FCacheSizeLBound;
    property CacheSizeUBound: SizeInt read FCacheSizeUBound write FCacheSizeUBound;
    property OnLoadIcon: TLoadIconEvent read FOnLoadIcon write FOnLoadIcon;
  end;

implementation

uses Forms;

{ TListViewIconManager }

constructor TListViewIconManager.Create(ListView: TCustomListView);
begin
  FListView := ListView;
  FIconCache := TIconCache.Create([doOwnsValues]);
  FPendingList := TPendingList.Create;
  FCacheSizeUBound := DEFAULT_CACHE_SIZE_UBOUND;
  FCacheSizeLBound := DEFAULT_CACHE_SIZE_LBOUND;
end;

destructor TListViewIconManager.Destroy;
begin
  FIconCache.Free;
  FPendingList.Free;
  inherited Destroy;
end;

procedure TListViewIconManager.DrawIcon(Item: TListItem; HAlign: THorizontalAlign; VAlign: TVerticalAlign);

  procedure HighlightFilter(Picture: TPicture);
  var
    PixelColor: TColor;
    X, Y: Integer;
  begin
    for X := 0 to Picture.Width - 1 do
    begin
      for Y := 0 to Picture.Height - 1 do
      begin
        PixelColor := Picture.Bitmap.Canvas.Pixels[X, Y];
        Picture.Bitmap.Canvas.Pixels[X, Y] := DecColor(PixelColor, $40);
      end;
    end;
  end;

  procedure LoseFocusFilter(Picture: TPicture);
  var
    PixelColor: TColor;
    X, Y: Integer;
  begin
    for X := 0 to Picture.Width - 1 do
    begin
      for Y := 0 to Picture.Height - 1 do
      begin
        PixelColor := Picture.Bitmap.Canvas.Pixels[X, Y];
        Picture.Bitmap.Canvas.Pixels[X, Y] := DecColor(PixelColor, $10);
      end;
    end;
  end;

var
  IconRect: TRect;
  Picture, FilteredPicture: TPicture;
  X, Y: Integer;
  IconLoader: TLoadIconWorkBase = nil;
begin
  IconRect := Item.DisplayRect(drIcon);
  if FIconCache.ContainsKey(Item.Data) then
  begin
    Picture := FIconCache.Items[Item.Data];
    if not Assigned(Picture) then Picture := FDefaultIcon;
  end
  else
  begin
    Picture := FDefaultIcon;
    if Assigned(FOnLoadIcon) and not FPendingList.Contains(Item.Data) then
    begin
      FOnLoadIcon(Self, Item.Data, IconLoader);
      if Assigned(IconLoader) then
      begin
        FPendingList.Add(Item.Data);
        IconLoader.OnSuccess := @LoadSuccess;
        IconLoader.OnComplete := @LoadComplete;
        WorkPool.Exec(IconLoader);
      end;
    end;
  end;
  if Picture = nil then Exit;
  case HAlign of
    haLeft: X := IconRect.Left;
    haCenter: X := IconRect.Left + (IconRect.Width - Picture.Width) div 2;
    haRight:  X := IconRect.Left + IconRect.Width - Picture.Width;
  end;
  case VAlign of
    vaTop: Y := IconRect.Top;
    vaCenter: Y := IconRect.Top + (IconRect.Height - Picture.Height) div 2;
    vaBottom: Y := IconRect.Top + IconRect.Height - Picture.Height;
  end;

  if Item.Selected then
  begin
    FilteredPicture := TPicture.Create;
    try
      FilteredPicture.Assign(Picture); 
      if Item.Owner.Owner.Focused then HighlightFilter(FilteredPicture) else LoseFocusFilter(FilteredPicture);
      FListView.Canvas.Draw(X, Y, FilteredPicture.Graphic);
    finally
      FilteredPicture.Free;
    end;
  end
  else FListView.Canvas.Draw(X, Y, Picture.Graphic);
end;

procedure TListViewIconManager.LoadSuccess(Sender: TLoadIconWorkBase; Return: TPicture);
var
  Key: TItemData;
begin
  if FIconCache.Count >= FCacheSizeUBound then
  begin
    for Key in FIconCache.Keys do
    begin
      FIconCache.Remove(Key);
      if FIconCache.Count <= FCacheSizeLBound then break;
    end;
  end;
  FIconCache.Add((Sender as TLoadIconWork).ItemData, Return);
  FListView.Repaint;
end;

procedure TListViewIconManager.LoadComplete(Sender: TLoadIconWorkBase);
begin
  FPendingList.Remove((Sender as TLoadIconWork).ItemData);
end;

{ TListViewIconManager.TLoadIconWork }

constructor TListViewIconManager.TLoadIconWork.Create(ItemData: TItemData);
begin
  inherited Create;
  FItemData := ItemData;
end;


end.

