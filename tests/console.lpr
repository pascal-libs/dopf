program console;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner,
  test_insertbuilder, test_dclasses, test_dopf, test_dutils
  ;
type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title:='FPCUnit Console test runner';
  Application.Run;
  Application.Free;
end.

