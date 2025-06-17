program run_tests;

{$mode objfpc}{$H+}

uses
  {$IFDEF FPC}
  fpcunit, testregistry, test_insertbuilder;
  {$ENDIF}

begin
  {$IFDEF FPC}
  RunRegisteredTests;
  {$ENDIF}
end.

