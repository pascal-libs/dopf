program gui;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, test_insertbuilder, test_dOpfRelations
  ;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

