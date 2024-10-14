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
  Forms,
  {$IFDEF DEBUG}
  SysUtils,
  {$ENDIF}
  Manhuard.Strings, Manhuard.Form.Main, Manhuard.Form.View;

{$R *.res}

begin
  {$IFDEF DEBUG}
  if FileExists('heap.trc') then DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$ENDIF}
  RequireDerivedFormResource:=True;
  Application.Title:=APP_NAME;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormView, FormView);
  Application.Run;
end.

