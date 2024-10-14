unit Manhuard.Types;

{$mode ObjFPC}{$H+}

interface

type
  TPageIndex = (
    fpiHome,
    fpiBookshelf,
    fpiDownloading,
    fpiSearch,
    fpiBookmark,
    fpiAddManga,
    fpiOptions,
    fpiAbout,
    fpiBook
  );

  TPtrInt = record
    case Byte of
      0: (Ptr: Pointer);
      1: (Int: PtrInt);
      2: (UInt: PtrUInt);
  end;

  TViewSizeAdaptation = (vsaRaw, vsaOneSide, vsaFull);

implementation

end.

