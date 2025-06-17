unit test_insertbuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  dsqlbuilder;

type
  { TPerson }
  TPerson = class(TObject)
  private
    FId: Int64;
    FName: string;
  published
    property Id: Int64 read FId write FId;
    property Name: string read FName write FName;
  end;

  { TPersonTable }
  TPersonTable = class(specialize TdGTable<TPerson>)
  public
    constructor Create; override;
  end;

  { TTestInsertBuilder }
  TTestInsertBuilder = class(TTestCase)
  published
    procedure TestMakeFields;
  end;

implementation

constructor TPersonTable.Create;
begin
  inherited Create;
  Name := 'person';
end;

procedure TTestInsertBuilder.TestMakeFields;
var
  table: TPersonTable;
  fields, params: string;
begin
  table := TPersonTable.Create;
  try
    specialize TdGInsertBuilder<TPersonTable>.MakeFields(table, fields, params, True);
    AssertEquals('name', fields);
    AssertEquals(':name', params);
  finally
    table.Free;
  end;
end;

initialization
  RegisterTest(TTestInsertBuilder);
end.

