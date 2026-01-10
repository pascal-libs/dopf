program gui;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, test_insertbuilder, test_dclasses, test_dopf, test_dutils,
  test_dOpfRelations, test_dOpfTableRegistry, test_dOpfRelations_Extended, test_dOpfRelationsHelpers
  ;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

