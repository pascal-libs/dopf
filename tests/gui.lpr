program gui;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner,
  test_insertbuilder, test_dclasses, test_dopf, test_dutils;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

