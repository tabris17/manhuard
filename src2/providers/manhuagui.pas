unit Manhuagui;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, MTProcs;

type

  { TDownloader }

  TDownloader = class
  private
    client: TFPHTTPClient;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function Test(Url: string): boolean;
    function FetchBook(Url: string): boolean;
    function FetchVolume(Url: string): boolean;
  end;

implementation

constructor TDownloader.Create(AOwner: TComponent);
begin
  client := TFPHTTPClient.Create(AOwner);
end;

destructor TDownloader.Destroy;
begin
  FreeAndNil(client);
  inherited Destroy;
end;

end.

