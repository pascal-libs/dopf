unit test_insertbuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, dsqlbuilder;

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
    procedure TestIgnoredFieldsGuards;
    procedure TestNoFieldsGuards;
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

procedure TTestInsertBuilder.TestIgnoredFieldsGuards;
var
  table: TPersonTable;
  fields, params: string;
begin
  // All published properties are ignored: builders must produce empty
  // strings instead of raising on negative SetLength.
  table := TPersonTable.Create;
  try
    table.IgnoredFields.Add('Id');
    table.IgnoredFields.Add('Name');
    specialize TdGSelectBuilder<TPersonTable>.MakeFields(table, fields, True);
    AssertEquals('', fields);
    specialize TdGInsertBuilder<TPersonTable>.MakeFields(table, fields, params, True);
    AssertEquals('', fields);
    AssertEquals('', params);
    // PK conditions are kept even when the PK field is ignored.
    specialize TdGUpdateBuilder<TPersonTable>.MakeFields(table, fields, params, True);
    AssertEquals('', fields);
    AssertEquals('id = :id', params);
    specialize TdGDeleteBuilder<TPersonTable>.MakeParams(table, params, True);
    AssertEquals('id = :id', params);
  finally
    table.Free;
  end;
end;

procedure TTestInsertBuilder.TestNoFieldsGuards;
var
  table: TPersonTable;
  fields, params: string;
begin
  // No primary keys and all fields ignored: every builder must produce
  // empty strings without raising on negative SetLength.
  table := TPersonTable.Create;
  try
    table.PrimaryKeys.Clear;
    table.IgnoredFields.Add('Id');
    table.IgnoredFields.Add('Name');
    specialize TdGSelectBuilder<TPersonTable>.MakeFields(table, fields, True);
    AssertEquals('', fields);
    specialize TdGInsertBuilder<TPersonTable>.MakeFields(table, fields, params, True);
    AssertEquals('', fields);
    AssertEquals('', params);
    specialize TdGUpdateBuilder<TPersonTable>.MakeFields(table, fields, params, True);
    AssertEquals('', fields);
    AssertEquals('', params);
    specialize TdGDeleteBuilder<TPersonTable>.MakeParams(table, params, True);
    AssertEquals('', params);
  finally
    table.Free;
  end;
end;

initialization
  RegisterTest(TTestInsertBuilder);
end.

