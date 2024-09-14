unit Manhuard.Pages;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  Manhuard.Frame,
  Manhuard.Types,
  Manhuard.Page.Home,
  Manhuard.Page.Bookshelf,
  Manhuard.Page.Downloading,
  Manhuard.Page.Search,
  Manhuard.Page.Bookmark,
  Manhuard.Page.AddManga,
  Manhuard.Page.Options,
  Manhuard.Page.About,
  Manhuard.Page.Book;

var
  PageClasses: array[TPageIndex] of TPageClass = (
    TPageHome,
    TPageBookshelf,
    TPageDownloading,
    TPageSearch,
    TPageBookmark,
    TPageAddManga,
    TPageOptions,
    TPageAbout,
    TPageBook
  );

implementation

end.

