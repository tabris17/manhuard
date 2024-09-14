unit Manhuard.Python.Module;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, PythonEngine;

function Test(Self, Args : PPyObject): PPyObject; cdecl;

implementation

function Test(Self, Args : PPyObject): PPyObject; cdecl;
begin
  with GetPythonEngine do
    Result:= PyUnicode_FromString('1.0.0');
end;

end.

