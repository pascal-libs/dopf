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
    procedure TestIgnoredFieldsRejected;
    procedure TestNoFieldsRejected;
    procedure TestBuildRejectsEmptyFields;
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

procedure TTestInsertBuilder.TestIgnoredFieldsRejected;
var
  table: TPersonTable;
  fields, params: string;
begin
  // All published properties are ignored: there is nothing to select or
  // write, so the builders must reject the configuration outright
  // instead of assembling invalid SQL.
  table := TPersonTable.Create;
  try
    table.IgnoredFields.Add('Id');
    table.IgnoredFields.Add('Name');
    try
      specialize TdGSelectBuilder<TPersonTable>.MakeFields(table, fields, True);
      Fail('Expected exception');
    except
      on E: EdSqlBuilder do
        AssertEquals('Field list must not be empty.', E.Message);
    end;
    try
      specialize TdGInsertBuilder<TPersonTable>.MakeFields(table, fields, params, True);
      Fail('Expected exception');
    except
      on E: EdSqlBuilder do
        AssertEquals('Field list must not be empty.', E.Message);
    end;
    try
      specialize TdGUpdateBuilder<TPersonTable>.MakeFields(table, fields, params, True);
      Fail('Expected exception');
    except
      on E: EdSqlBuilder do
        AssertEquals('Field list must not be empty.', E.Message);
    end;
    // PK conditions are kept even when the PK field is ignored, so the
    // delete builder still has a usable condition list.
    specialize TdGDeleteBuilder<TPersonTable>.MakeParams(table, params, True);
    AssertEquals('id = :id', params);
  finally
    table.Free;
  end;
end;

procedure TTestInsertBuilder.TestNoFieldsRejected;
var
  table: TPersonTable;
  fields, params: string;
begin
  // No primary keys and all fields ignored: every builder must reject
  // the configuration instead of raising on negative SetLength.
  table := TPersonTable.Create;
  try
    table.PrimaryKeys.Clear;
    table.IgnoredFields.Add('Id');
    table.IgnoredFields.Add('Name');
    try
      specialize TdGSelectBuilder<TPersonTable>.MakeFields(table, fields, True);
      Fail('Expected exception');
    except
      on E: EdSqlBuilder do
        AssertEquals('Field list must not be empty.', E.Message);
    end;
    try
      specialize TdGInsertBuilder<TPersonTable>.MakeFields(table, fields, params, True);
      Fail('Expected exception');
    except
      on E: EdSqlBuilder do
        AssertEquals('Field list must not be empty.', E.Message);
    end;
    try
      specialize TdGUpdateBuilder<TPersonTable>.MakeFields(table, fields, params, True);
      Fail('Expected exception');
    except
      on E: EdSqlBuilder do
        AssertEquals('Field list must not be empty.', E.Message);
    end;
    try
      specialize TdGDeleteBuilder<TPersonTable>.MakeParams(table, params, True);
      Fail('Expected exception');
    except
      on E: EdSqlBuilder do
        AssertEquals('Condition list must not be empty.', E.Message);
    end;
  finally
    table.Free;
  end;
end;

procedure TTestInsertBuilder.TestBuildRejectsEmptyFields;
var
  table: TPersonTable;
  B: specialize TdGSelectBuilder<TPersonTable>;
  sql: string;
begin
  // Build must never emit invalid SQL like 'SELECT  FROM person'.
  table := TPersonTable.Create;
  B := specialize TdGSelectBuilder<TPersonTable>.Create(nil);
  try
    table.IgnoredFields.Add('Id');
    table.IgnoredFields.Add('Name');
    B.SetTable(table);
    try
      B.Build(sql, True);
      Fail('Expected exception');
    except
      on E: EdSqlBuilder do
        AssertEquals('Field list must not be empty.', E.Message);
    end;
  finally
    B.Free;
    table.Free;
  end;
end;

initialization
  RegisterTest(TTestInsertBuilder);
end.

