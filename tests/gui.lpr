program gui;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, test_insertbuilder;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

