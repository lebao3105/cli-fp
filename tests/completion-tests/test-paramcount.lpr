program TestParamCount;

{$mode objfpc}{$H+}

uses
  SysUtils;

var
  i: Integer;

begin
  WriteLn('ParamCount: ', ParamCount);
  for i := 0 to ParamCount do
    WriteLn('ParamStr(', i, '): [', ParamStr(i), ']');
end.
