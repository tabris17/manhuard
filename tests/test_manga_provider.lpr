program test_manga_provider;

{$mode objfpc}{$H+}

uses
  Classes, Crt, Manga.Types, Manga.Provider, Manga.Provider.Manhuagui;

var
  MangaProvider: TMangaProvider = nil;
  MangaHandle: TMangaHandle;

const
  MANGA_URL = 'https://www.manhuagui.com/comic/39903/';


begin
  MangaProvider := TManhuaguiProvider.Create(nil);
  MangaProvider.Test(MANGA_URL, MangaHandle);
  WriteLn('Url:', MangaHandle.Url);
  WriteLn('Id:', MangaHandle.Id);
  WriteLn('Provider:', MangaHandle.Provider);



  MangaProvider.Free;
  WriteLn('Press any key to quit');
  ReadKey;
end.

