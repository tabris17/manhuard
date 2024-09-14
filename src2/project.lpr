program Project;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Main;

{$R *.res}  

resourcestring
  APP_NAME = 'Manhuard';

begin
  RequireDerivedFormResource:=True;
  Application.Title:=APP_NAME;
  Application.Scaled:=True;
  Application.Initialize;
  Application.Run;
end.

