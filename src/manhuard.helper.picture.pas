unit Manhuard.Helper.Picture;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  EPictureError = Exception;

procedure ResizePicture(Picture: TPicture; NewWidth, NewHeight: Integer);

implementation

uses magick_wand, ImageMagick, IntfGraphics, FPimage, LazUTF8;

procedure ThrowWandException(Wand: PMagickWand);
var
  Description: PChar;
  Severity: ExceptionType;
begin
  Description := MagickGetException(Wand, @Severity);
  try
    raise EPictureError.Create(Format('An error occurred while processing the image: (%d) %s', [Severity, Description]));
  finally
    Description := MagickRelinquishMemory(Description);
  end;
end;

procedure LoadFromPicture(Wand: PMagickWand; Picture: TPicture);
var
  Status: MagickBooleanType;
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  Picture.SaveToStream(Stream);
  try
    Status := MagickReadImageBlob(Wand, Stream.Memory, Stream.Size);
    if Status = MagickFalse then ThrowWandException(Wand);
  finally
    Stream.Free;
  end;
end;

procedure SaveToPicture(Wand: PMagickWand; Picture: TPicture);
const
  FILE_EXT = 'PNG';
var
  Size: size_t;
  ImageBlob: PByte;
  ImageBytes: TBytes;
  Status: MagickBooleanType;
  Stream: TBytesStream;
begin
  Status := MagickSetImageFormat(Wand, FILE_EXT);
  if Status = MagickFalse then ThrowWandException(Wand);
  ImageBlob := MagickGetImageBlob(Wand, @Size);
  SetLength(ImageBytes, Size);
  Move(ImageBlob^, ImageBytes[0], Size);
  Stream := TBytesStream.Create(ImageBytes);
  try
    Picture.LoadFromStreamWithFileExt(Stream, FILE_EXT);
  finally
    Stream.Free;
  end;
end;

procedure ResizePicture(Picture: TPicture; NewWidth, NewHeight: Integer);
var
  Wand: PMagickWand; 
  Status: MagickBooleanType;
begin
  Wand := NewMagickWand;
  try
    LoadFromPicture(Wand, Picture);
    Status := MagickResizeImage(Wand, NewWidth, NewHeight, LanczosFilter, 1.0);
    if Status = MagickFalse then ThrowWandException(Wand);
    SaveToPicture(Wand, Picture);
  finally
    Wand := DestroyMagickWand(Wand);
  end;
end;

initialization
  MagickWandGenesis;

finalization;
  MagickWandTerminus;

end.

