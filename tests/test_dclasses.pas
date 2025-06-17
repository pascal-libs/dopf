unit test_dclasses;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, dclasses, Classes;

type
  TTestDClasses = class(TTestCase)
  published
    procedure TestInheritances;
  end;

implementation

uses
  SysUtils
  ;

procedure TTestDClasses.TestInheritances;
begin
  AssertTrue(EdException.InheritsFrom(Exception));
  AssertTrue(TdObject.InheritsFrom(TObject));
  AssertTrue(TdComponent.InheritsFrom(TComponent));
end;

initialization
  RegisterTest(TTestDClasses);
end.
