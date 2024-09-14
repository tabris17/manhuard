unit Manhuard.Debug;

{$mode ObjFPC}{$H+}
{$inline on}

interface

uses
  Classes, SysUtils;

procedure DebugLn(const Msg: string);
procedure DebugLn(const Msg: string; const Args : Array of const); inline;
procedure DebugLn(const Msg: string; const Args: array of const; const FormatSettings: TFormatSettings); inline;

implementation

procedure DebugLn(const Msg: string);
begin
  {$IFDEF DEBUG}
  WriteLn(Msg);
  {$ENDIF}
end;

procedure DebugLn(const Msg: string; const Args: array of const); inline;
begin
  {$IFDEF DEBUG}
  WriteLn(Format(Msg, Args));
  {$ENDIF}
end;

procedure DebugLn(const Msg: string; const Args: array of const; const FormatSettings: TFormatSettings); inline;
begin
  {$IFDEF DEBUG}
  WriteLn(Format(Msg, Args, FormatSettings));
  {$ENDIF}
end;


end.


