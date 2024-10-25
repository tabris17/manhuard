unit Manhuard.Helper.Picture;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ImageMagick;

type
  EPictureError = Exception;

  { TPictureHelper }

  TPictureHelper = class helper for TPicture
  public
    procedure Resize(Width, Height: Integer; const Filter: FilterTypes = LanczosFilter; const Blur: Double = 1.0);
    procedure Scale(Width, Height: Integer; Proportional: Boolean = True);
    procedure ScaleTo(Dest: TPicture; Width, Height: Integer; Proportional: Boolean = True);
    procedure Load(Stream: TStream);
  end;


implementation

uses magick_wand, IntfGraphics, FPimage, LazUTF8;

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
  try
    Picture.SaveToStream(Stream);
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
  Blob: PByte;
  Status: MagickBooleanType;
  Stream: TMemoryStream;
begin
  Status := MagickSetImageFormat(Wand, FILE_EXT);
  if Status = MagickFalse then ThrowWandException(Wand);
  Blob := MagickGetImageBlob(Wand, @Size);
  Stream := TMemoryStream.Create;
  try
    Stream.Size := Size;
    Move(Blob^, Stream.Memory^, Size);
    Picture.LoadFromStreamWithFileExt(Stream, FILE_EXT);
  finally
    Stream.Free;
  end;
end;

{ TPictureHelper }

procedure TPictureHelper.Resize(Width, Height: Integer; const Filter: FilterTypes; const Blur: Double);
var
  Wand: PMagickWand;
  Status: MagickBooleanType;
begin
  Wand := NewMagickWand;
  try
    LoadFromPicture(Wand, Self);
    Clear;
    Status := MagickResizeImage(Wand, Width, Height, Filter, Blur);
    if Status = MagickFalse then ThrowWandException(Wand);
    SaveToPicture(Wand, Self);
  finally
    Wand := DestroyMagickWand(Wand);
  end;
end;

procedure TPictureHelper.Scale(Width, Height: Integer; Proportional: Boolean);
begin                           
  ScaleTo(Self, Width, Height, Proportional);
end;

procedure TPictureHelper.ScaleTo(Dest: TPicture; Width, Height: Integer; Proportional: Boolean);
var
  Wand: PMagickWand;
  Status: MagickBooleanType;
  OriginalWidth, OriginalHeight, PropWidth: Integer;
begin
  OriginalWidth := Self.Width;
  OriginalHeight := Self.Height;
  if (OriginalWidth = 0) or (OriginalHeight = 0) then Exit;

  if Proportional then
  begin
    PropWidth := Self.Width * Height div OriginalHeight;
    if PropWidth < Width then Width := PropWidth else Height := OriginalHeight * Width div OriginalWidth;
  end;
  Wand := NewMagickWand;
  try
    LoadFromPicture(Wand, Self);
    Dest.Clear;
    Status := MagickScaleImage(Wand, Width, Height);
    if Status = MagickFalse then ThrowWandException(Wand);
    SaveToPicture(Wand, Dest);
  finally
    Wand := DestroyMagickWand(Wand);
  end;
end;

procedure TPictureHelper.Load(Stream: TStream);
var
  Wand: PMagickWand;
  MemoryStream: TMemoryStream;
  Status: MagickBooleanType;
begin
  try
    LoadFromStream(Stream);
    Exit;
  except
    on E: EInvalidGraphic do;
  end;
  Wand := NewMagickWand;
  try
    if Stream is TMemoryStream then
      Status := MagickReadImageBlob(Wand, (Stream as TMemoryStream).Memory, Stream.Size)
    else
    begin
      MemoryStream := TMemoryStream.Create;
      try
        MemoryStream.LoadFromStream(Stream);
        Status := MagickReadImageBlob(Wand, MemoryStream.Memory, Stream.Size);
      finally
        MemoryStream.Free;
      end;
    end;
    if Status = MagickFalse then ThrowWandException(Wand);
    SaveToPicture(Wand, Self);
  finally
    Wand := DestroyMagickWand(Wand);
  end;
end;

initialization
  MagickWandGenesis;

finalization;
  MagickWandTerminus;

end.

