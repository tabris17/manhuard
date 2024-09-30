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

  TListViewIconManager = class
  type
    TIconCache = specialize TObjectDictionary<Pointer, TPicture>;
    TDataIconPair = specialize TPair<Pointer, TPicture>;
    TDataIconPairArray = array of TDataIconPair;
    TLoadIconsWork = specialize TWork<TDataIconPairArray>;
    THorizontalAlign = (haLeft, haCenter, haRight);
    TVerticalAlign = (vaTop, vaCenter, vaBottom);
    TPendingQueue = specialize TQueue<Pointer>;
    TDataArray = specialize TArray<Pointer>;
    TLoadIconEvent = procedure (Sender: TListViewIconManager; out LoadIconWork: TLoadIconsWork) of object;
  private
    FListView: TCustomListView;
    FDefaultIcon: TPicture;
    FIconCache: TIconCache;
    FPendingQueue: TPendingQueue;
    FCacheSizeUBound, FCacheSizeLBound: SizeInt;
    FOnLoadIcon: TLoadIconEvent;
    function GetPendingData: TDataArray;
  public
    constructor Create(ListView: TCustomListView);
    destructor Destroy; override;
    procedure DrawIcon(Item: TListItem; HAlign: THorizontalAlign = haLeft; VAlign: TVerticalAlign = vaTop);
    procedure AsyncLoad(Data: PtrInt);
    procedure LoadSuccess(Sender: TLoadIconsWork; Return: TDataIconPairArray);
    property Cache: TIconCache read FIconCache;
    property DefaultIcon: TPicture read FDefaultIcon write FDefaultIcon;
    property CacheSizeLBound: SizeInt read FCacheSizeLBound write FCacheSizeLBound;
    property CacheSizeUBound: SizeInt read FCacheSizeUBound write FCacheSizeUBound;
    property OnLoadIcon: TLoadIconEvent read FOnLoadIcon write FOnLoadIcon;
    property PendingData: TDataArray read GetPendingData;
  end;

implementation

uses Forms;

{ TListViewIconManager }

function TListViewIconManager.GetPendingData: TDataArray;
begin
  Result := FPendingQueue.ToArray;
end;

constructor TListViewIconManager.Create(ListView: TCustomListView);
begin
  FListView := ListView;
  FIconCache := TIconCache.Create;
  FPendingQueue := TPendingQueue.Create;
  FCacheSizeUBound := DEFAULT_CACHE_SIZE_UBOUND;
  FCacheSizeLBound := DEFAULT_CACHE_SIZE_LBOUND;
end;

destructor TListViewIconManager.Destroy;
begin
  FIconCache.Free;
  FPendingQueue.Free;
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
begin
  IconRect := Item.DisplayRect(drIcon);
  if FIconCache.ContainsKey(Item.Data) then
    Picture := FIconCache.Items[Item.Data]
  else
  begin
    Picture := FDefaultIcon;
    FPendingQueue.Enqueue(Item.Data);
    Application.QueueAsyncCall(@AsyncLoad, 0);
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

procedure TListViewIconManager.AsyncLoad(Data: PtrInt);
var
  Loader: TLoadIconsWork = nil;
begin
  Application.RemoveAsyncCalls(Self);
  if Assigned(FOnLoadIcon) then FOnLoadIcon(Self, Loader);
  if Loader = nil then Exit;
  FPendingQueue.Clear;
  Loader.OnSuccess := @LoadSuccess;
  WorkPool.Exec(Loader);
end;

procedure TListViewIconManager.LoadSuccess(Sender: TLoadIconsWork; Return: TDataIconPairArray);
var
  Item: TDataIconPair;
  Key: Pointer;
begin
  if Length(Return) + FIconCache.Count >= FCacheSizeUBound then
  begin
    for Key in FIconCache.Keys do
    begin
      FIconCache.Remove(Key);
      if FIconCache.Count <= FCacheSizeLBound then break;
    end;
  end;
  for Item in Return do FIconCache.Add(Item);
  FListView.Repaint;
end;


end.

