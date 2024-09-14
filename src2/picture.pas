unit Picture;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ctypes, Graphics;

procedure LoadMagickBitmap(FileName: string; Bmp: TBitmap); overload;
procedure LoadMagickBitmap(Strm: TMemoryStream; Bmp: TBitmap); overload;

implementation

uses magick_wand, ImageMagick, IntfGraphics, FPimage, LazUTF8;

procedure LoadMagickBitmapWand(Wand: PMagickWand; Bmp: TBitmap);
var
  Size: size_t;
  ImageBlob: PByte;
  ImageBytes: TBytes;
  ImageStream: TStream;
begin
  MagickSetImageFormat(Wand, 'BMP');
  ImageBlob := MagickGetImageBlob(Wand, @Size);
  SetLength(ImageBytes, Size);
  Move(ImageBlob^, ImageBytes[0], Size);
  ImageStream := TBytesStream.Create(ImageBytes);
  try
    Bmp.LoadFromStream(ImageStream, Size);
  finally
    ImageStream.Free;
  end;
end;

procedure LoadMagickBitmap(FileName: string; Bmp: TBitmap);
var
  wand: PMagickWand;
  status: MagickBooleanType;
  description: PChar;
  severity: ExceptionType;
begin
  wand := NewMagickWand;
  try
    status := MagickReadImage(wand, PChar(UTF8ToSys(FileName)));
    if (status = MagickFalse) then
    begin
      description := MagickGetException(wand, @severity);
      raise Exception.Create(Format('An error ocurred. Description: %s',
        [description]));
      description := MagickRelinquishMemory(description);
    end else LoadMagickBitmapWand(wand, Bmp);
  finally
    wand := DestroyMagickWand(wand);
  end;
end;

procedure LoadMagickBitmap(Strm: TMemoryStream; Bmp: TBitmap);
var
  wand: PMagickWand;
  status: MagickBooleanType;
  description: PChar;
  severity: ExceptionType;
begin
  wand := NewMagickWand;
  try
    Strm.Position := 0;
    status := MagickReadImageBlob(wand, Strm.Memory, Strm.Size);
    if (status = MagickFalse) then
    begin
      description := MagickGetException(wand, @severity);
      raise Exception.Create(Format('An error ocurred. Description: %s',
        [description]));
      description := MagickRelinquishMemory(description);
    end else LoadMagickBitmapWand(wand, Bmp);
  finally
    wand := DestroyMagickWand(wand);
  end;
end;

initialization
  MagickWandGenesis;

finalization;
  MagickWandTerminus;

end.
