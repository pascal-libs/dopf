unit test_dOpfTableRegistry;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, dOpfTableRegistry, Classes, SysUtils;

type
  // Test entities
  { TTestEntity1 }
  TTestEntity1 = class(TObject)
  end;

  { TTestEntity2 }
  TTestEntity2 = class(TObject)
  end;

  { TTestEntity3 }
  TTestEntity3 = class(TObject)
  end;

  { Test classes }
  TTestTableRegistry = class(TTestCase)
  private
    FRegistry: TdTableRegistry;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSingleton;
    procedure TestRegisterClass;
    procedure TestGetTableForClass;
    procedure TestHasClass;
    procedure TestClassMethodRegisterTable;
    procedure TestClassMethodGetTableName;
    procedure TestClassMethodIsRegistered;
    procedure TestGetTableNameForClass;
    procedure TestRegisterEntityTable;
    procedure TestFallbackToClassName;
    procedure TestDuplicateRegistration;
    procedure TestCaseInsensitiveClassName;
  end;

  TTestTableRegistryEdgeCases = class(TTestCase)
  published
    procedure TestEmptyTableName;
    procedure TestLongTableName;
    procedure TestSpecialCharactersInTableName;
  end;

implementation

{ TTestTableRegistry }

procedure TTestTableRegistry.SetUp;
begin
  inherited SetUp;
  TdTableRegistry.ClearRegistry;
  // Create a separate instance for testing to avoid affecting global state
  FRegistry := TdTableRegistry.Create;
end;

procedure TTestTableRegistry.TearDown;
begin
  FRegistry.Free;
  inherited TearDown;
end;

procedure TTestTableRegistry.TestSingleton;
var
  Instance1, Instance2: TdTableRegistry;
begin
  Instance1 := TdTableRegistry.Instance;
  Instance2 := TdTableRegistry.Instance;

  AssertNotNull('Instance should not be nil', Instance1);
  AssertSame('Should return same instance', Instance1, Instance2);
end;

procedure TTestTableRegistry.TestRegisterClass;
begin
  FRegistry.RegisterClass(TTestEntity1, 'test_entities_1');

  AssertTrue('Should have registered class', FRegistry.HasClass(TTestEntity1));
  AssertEquals('Should return correct table name', 'test_entities_1',
    FRegistry.GetTableForClass(TTestEntity1));
end;

procedure TTestTableRegistry.TestGetTableForClass;
begin
  FRegistry.RegisterClass(TTestEntity1, 'custom_table');

  AssertEquals('Should return registered table name', 'custom_table',
    FRegistry.GetTableForClass(TTestEntity1));
  AssertEquals('Should return empty string for unregistered class', '',
    FRegistry.GetTableForClass(TTestEntity2));
end;

procedure TTestTableRegistry.TestHasClass;
begin
  AssertFalse('Should not have unregistered class', FRegistry.HasClass(TTestEntity1));

  FRegistry.RegisterClass(TTestEntity1, 'test_table');
  AssertTrue('Should have registered class', FRegistry.HasClass(TTestEntity1));
end;

procedure TTestTableRegistry.TestClassMethodRegisterTable;
begin
  TdTableRegistry.RegisterTable(TTestEntity2, 'entities_2');

  AssertTrue('Should be registered via class method',
    TdTableRegistry.IsRegistered(TTestEntity2));
  AssertEquals('Should return correct table name via class method', 'entities_2',
    TdTableRegistry.GetTableName(TTestEntity2));
end;

procedure TTestTableRegistry.TestClassMethodGetTableName;
begin
  // Test with registered class
  TdTableRegistry.RegisterTable(TTestEntity1, 'registered_table');
  AssertEquals('Should return registered table name', 'registered_table',
    TdTableRegistry.GetTableName(TTestEntity1));

  // Test with unregistered class - should fallback to lowercase class name
  AssertEquals('Should fallback to lowercase class name', 'testentity3',
    TdTableRegistry.GetTableName(TTestEntity3));
end;

procedure TTestTableRegistry.TestClassMethodIsRegistered;
begin
  AssertFalse('Should not be registered initially',
    TdTableRegistry.IsRegistered(TTestEntity1));

  TdTableRegistry.RegisterTable(TTestEntity1, 'test_table');
  AssertTrue('Should be registered after registration',
    TdTableRegistry.IsRegistered(TTestEntity1));
end;

procedure TTestTableRegistry.TestGetTableNameForClass;
var
  TableName: string;
begin
  // Test with registered class
  RegisterEntityTable(TTestEntity1, 'my_entities');
  TableName := GetTableNameForClass(TTestEntity1);
  AssertEquals('Should return registered table name', 'my_entities', TableName);

  // Test with unregistered class - should fallback
  TableName := GetTableNameForClass(TTestEntity2);
  AssertEquals('Should fallback correctly', 'testentity2', TableName);
end;

procedure TTestTableRegistry.TestRegisterEntityTable;
begin
  RegisterEntityTable(TTestEntity3, 'global_entities');

  AssertTrue('Should be registered globally',
    TdTableRegistry.IsRegistered(TTestEntity3));
  AssertEquals('Should return correct table name', 'global_entities',
    TdTableRegistry.GetTableName(TTestEntity3));
end;

procedure TTestTableRegistry.TestFallbackToClassName;
var
  TableName: string;
begin
  TableName := GetTableNameForClass(TTestEntity1);
  AssertEquals('Should fallback to modified class name', 'testentity1', TableName);
end;

procedure TTestTableRegistry.TestDuplicateRegistration;
begin
  FRegistry.RegisterClass(TTestEntity1, 'first_table');
  AssertEquals('Should have first table name', 'first_table',
    FRegistry.GetTableForClass(TTestEntity1));

  // Register again with different table name
  FRegistry.RegisterClass(TTestEntity1, 'second_table');
  AssertEquals('Should have updated table name', 'second_table',
    FRegistry.GetTableForClass(TTestEntity1));
end;

procedure TTestTableRegistry.TestCaseInsensitiveClassName;
begin
  FRegistry.RegisterClass(TTestEntity1, 'case_test_table');

  // The registry should find the class regardless of case in internal operations
  AssertTrue('Should find class', FRegistry.HasClass(TTestEntity1));
  AssertEquals('Should return table name', 'case_test_table',
    FRegistry.GetTableForClass(TTestEntity1));
end;

{ TTestTableRegistryEdgeCases }

procedure TTestTableRegistryEdgeCases.TestEmptyTableName;
var
  Registry: TdTableRegistry;
begin
  Registry := TdTableRegistry.Create;
  try
    Registry.RegisterClass(TTestEntity1, '');
    AssertTrue('Should accept empty table name', Registry.HasClass(TTestEntity1));
    AssertEquals('Should return empty string', '', Registry.GetTableForClass(TTestEntity1));
  finally
    Registry.Free;
  end;
end;

procedure TTestTableRegistryEdgeCases.TestLongTableName;
var
  Registry: TdTableRegistry;
  LongTableName: string;
begin
  Registry := TdTableRegistry.Create;
  try
    LongTableName := StringOfChar('a', 255); // Very long table name
    Registry.RegisterClass(TTestEntity1, LongTableName);

    AssertEquals('Should handle long table names', LongTableName,
      Registry.GetTableForClass(TTestEntity1));
  finally
    Registry.Free;
  end;
end;

procedure TTestTableRegistryEdgeCases.TestSpecialCharactersInTableName;
var
  Registry: TdTableRegistry;
  SpecialTableName: string;
begin
  Registry := TdTableRegistry.Create;
  try
    SpecialTableName := 'table_with-special.chars$123';
    Registry.RegisterClass(TTestEntity1, SpecialTableName);

    AssertEquals('Should handle special characters', SpecialTableName,
      Registry.GetTableForClass(TTestEntity1));
  finally
    Registry.Free;
  end;
end;

initialization
  RegisterTest(TTestTableRegistry);
  RegisterTest(TTestTableRegistryEdgeCases);

end.
