program Manhuard;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Manhuard.Strings,
  Forms, Manhuard.Form.Main, Manhuard.Form.View;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:=APP_NAME;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormView, FormView);
  Application.Run;
end.

