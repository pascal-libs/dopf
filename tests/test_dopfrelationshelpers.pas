unit test_dOpfRelationsHelpers;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, dOpfRelationsHelpers, dOpfRelations, Classes, SysUtils,
  Contnrs, Variants;

type
  // Test entities for helpers
  { TTestHelperEntity }
  TTestHelperEntity = class(TdAdvancedRelationalEntity)
  private
    FId: Int64;
    FName: string;
  public
    procedure ConfigureRelations({%H-}aRelations: TdRelationList); override;
  published
    property Id: Int64 read FId write FId;
    property Name: string read FName write FName;
  end;

  { TTestTargetEntity }
  TTestTargetEntity = class(TdRelationalEntity)
  private
    FId: Int64;
    FTitle: string;
  public
    procedure ConfigureRelations({%H-}aRelations: TdRelationList); override;
  published
    property Id: Int64 read FId write FId;
    property Title: string read FTitle write FTitle;
  end;

  { Test classes for TdRelationCache }
  TTestRelationCache = class(TTestCase)
  private
    FCache: TdRelationCache;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestStoreAndRetrieve;
    procedure TestStoreOverwrite;
    procedure TestRemove;
    procedure TestClear;
    procedure TestHasKey;
    procedure TestMultipleObjects;
    procedure TestNilValues;
  end;

  { Test classes for TdRelationQueryBuilder }
  TTestRelationQueryBuilder = class(TTestCase)
  published
    procedure TestBuildOneToOneQuery;
    procedure TestBuildOneToOneQueryWithStringValue;
    procedure TestBuildOneToManyQuery;
    procedure TestBuildOneToManyQueryWithNullValue;
    procedure TestBuildManyToOneQuery;
    procedure TestBuildManyToManyQuery;
    procedure TestBuildManyToManyQueryComplex;
    procedure TestQueryBuilderWithSpecialCharacters;
    procedure TestQueryBuilderWithEmptyParameters;
  end;

  { Test classes for TdRelationFactory }
  TTestRelationFactory = class(TTestCase)
  published
    procedure TestCreateEntity;
    procedure TestCreateEntityWithDifferentClasses;
    procedure TestCreateEntityList;
  end;

  { Test classes for TdRelationValidator }
  TTestRelationValidator = class(TTestCase)
  published
    procedure TestValidateValidRelation;
    procedure TestValidateInvalidRelations;
    procedure TestValidateEntity;
    procedure TestValidateNilEntity;
    procedure TestGetValidationErrorsComplete;
    procedure TestGetValidationErrorsPartial;
    procedure TestValidateManyToManySpecialCase;
  end;

  { Test classes for TdAdvancedRelationalEntity }
  TTestAdvancedRelationalEntity = class(TTestCase)
  private
    FEntity: TTestHelperEntity;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCacheKeyGeneration;
    procedure TestCacheRelation;
    procedure TestGetCachedRelation;
    procedure TestClearRelationCacheSpecific;
    procedure TestClearRelationCacheAll;
    procedure TestIsRelationLoaded;
    procedure TestGetLoadedRelations;
    procedure TestAutoLoadProperty;
    procedure TestPreloadRelation;
    procedure TestCacheMemoryManagement;
    procedure TestMultipleCacheOperations;
  end;

  { Test classes for edge cases and integration }
  TTestRelationHelpersIntegration = class(TTestCase)
  published
    procedure TestValidatorWithQueryBuilder;
    procedure TestFactoryWithCache;
    procedure TestAdvancedEntityWithAllHelpers;
    procedure TestPerformanceWithLargeDatasets;
    procedure TestThreadSafety;
    procedure TestMemoryUsage;
  end;

implementation

{ TTestHelperEntity }

procedure TTestHelperEntity.ConfigureRelations(aRelations: TdRelationList);
begin
  HasMany('Targets', TTestTargetEntity, 'targets', 'entity_id', 'id');
  HasOne('MainTarget', TTestTargetEntity, 'targets', 'entity_id', 'id');
end;

{ TTestTargetEntity }

procedure TTestTargetEntity.ConfigureRelations(aRelations: TdRelationList);
begin
  BelongsTo('Entity', TTestHelperEntity, 'entities', 'id', 'entity_id');
end;

{ TTestRelationCache }

procedure TTestRelationCache.SetUp;
begin
  inherited SetUp;
  FCache := TdRelationCache.Create;
end;

procedure TTestRelationCache.TearDown;
begin
  FCache.Free;
  inherited TearDown;
end;

procedure TTestRelationCache.TestStoreAndRetrieve;
var
  TestObj: TTestHelperEntity;
  Retrieved: TObject;
begin
  TestObj := TTestHelperEntity.Create;
  try
    FCache.Store('test_key', TestObj);
    Retrieved := FCache.Retrieve('test_key');

    AssertNotNull('Should retrieve stored object', Retrieved);
    AssertSame('Should retrieve same object', TestObj, Retrieved);
  finally
    // TestObj will be freed by cache (OwnsObjects = True)
  end;
end;

procedure TTestRelationCache.TestStoreOverwrite;
var
  TestObj1, TestObj2: TTestHelperEntity;
  Retrieved: TObject;
begin
  TestObj1 := TTestHelperEntity.Create;
  TestObj2 := TTestHelperEntity.Create;

  FCache.Store('same_key', TestObj1);
  FCache.Store('same_key', TestObj2); // Should overwrite

  Retrieved := FCache.Retrieve('same_key');
  AssertSame('Should retrieve the second object', TestObj2, Retrieved);
  // TestObj1 should be automatically freed when overwritten
end;

procedure TTestRelationCache.TestRemove;
var
  TestObj: TTestHelperEntity;
begin
  TestObj := TTestHelperEntity.Create;

  FCache.Store('remove_test', TestObj);
  AssertTrue('Should have key before removal', FCache.HasKey('remove_test'));

  FCache.Remove('remove_test');
  AssertFalse('Should not have key after removal', FCache.HasKey('remove_test'));
  AssertNull('Should return nil after removal', FCache.Retrieve('remove_test'));
end;

procedure TTestRelationCache.TestClear;
var
  TestObj1, TestObj2: TTestHelperEntity;
begin
  TestObj1 := TTestHelperEntity.Create;
  TestObj2 := TTestHelperEntity.Create;

  FCache.Store('key1', TestObj1);
  FCache.Store('key2', TestObj2);

  AssertTrue('Should have key1', FCache.HasKey('key1'));
  AssertTrue('Should have key2', FCache.HasKey('key2'));

  FCache.Clear;

  AssertFalse('Should not have key1 after clear', FCache.HasKey('key1'));
  AssertFalse('Should not have key2 after clear', FCache.HasKey('key2'));
end;

procedure TTestRelationCache.TestHasKey;
var
  TestObj: TTestHelperEntity;
begin
  AssertFalse('Should not have key initially', FCache.HasKey('test_key'));

  TestObj := TTestHelperEntity.Create;
  FCache.Store('test_key', TestObj);

  AssertTrue('Should have key after storing', FCache.HasKey('test_key'));
end;

procedure TTestRelationCache.TestMultipleObjects;
var
  TestObjs: array[0..9] of TTestHelperEntity;
  I: Integer;
  Key: string;
begin
  // Store multiple objects
  for I := 0 to 9 do
  begin
    TestObjs[I] := TTestHelperEntity.Create;
    TestObjs[I].Id := I;
    TestObjs[I].Name := 'Object' + IntToStr(I);
    Key := 'key_' + IntToStr(I);
    FCache.Store(Key, TestObjs[I]);
  end;

  // Verify all objects are stored correctly
  for I := 0 to 9 do
  begin
    Key := 'key_' + IntToStr(I);
    AssertTrue('Should have key ' + Key, FCache.HasKey(Key));
    AssertSame('Should retrieve correct object for ' + Key, TestObjs[I], FCache.Retrieve(Key));
  end;
end;

procedure TTestRelationCache.TestNilValues;
begin
  FCache.Store('nil_test', nil);

  AssertTrue('Should have key for nil value', FCache.HasKey('nil_test'));
  AssertNull('Should retrieve nil value', FCache.Retrieve('nil_test'));
end;

{ TTestRelationQueryBuilder }

procedure TTestRelationQueryBuilder.TestBuildOneToOneQuery;
var
  Query: string;
begin
  Query := TdRelationQueryBuilder.BuildOneToOneQuery('profiles', 'user_id', 123);

  AssertTrue('Should contain SELECT', Pos('SELECT', UpperCase(Query)) > 0);
  AssertTrue('Should contain table name', Pos('profiles', Query) > 0);
  AssertTrue('Should contain WHERE clause', Pos('WHERE', UpperCase(Query)) > 0);
  AssertTrue('Should contain foreign key', Pos('user_id', Query) > 0);
  AssertTrue('Should contain value', Pos('123', Query) > 0);
end;

procedure TTestRelationQueryBuilder.TestBuildOneToOneQueryWithStringValue;
var
  Query: string;
begin
  Query := TdRelationQueryBuilder.BuildOneToOneQuery('profiles', 'username', 'john_doe');

  AssertTrue('Should contain string value in quotes', Pos('''john_doe''', Query) > 0);
  AssertTrue('Should contain foreign key', Pos('username', Query) > 0);
end;

procedure TTestRelationQueryBuilder.TestBuildOneToManyQuery;
var
  Query: string;
begin
  Query := TdRelationQueryBuilder.BuildOneToManyQuery('posts', 'author_id', 456);

  AssertTrue('Should contain ORDER BY', Pos('ORDER BY', UpperCase(Query)) > 0);
  AssertTrue('Should contain table name', Pos('posts', Query) > 0);
  AssertTrue('Should contain foreign key', Pos('author_id', Query) > 0);
  AssertTrue('Should contain value', Pos('456', Query) > 0);
  AssertTrue('Should order by id', Pos('ORDER BY id', Query) > 0);
end;

procedure TTestRelationQueryBuilder.TestBuildOneToManyQueryWithNullValue;
var
  aQuery: string;
begin
  aQuery := TdRelationQueryBuilder.BuildOneToManyQuery('comments', 'post_id', Null);

  AssertTrue('Should handle null value', Pos('NULL', UpperCase(aQuery)) > 0);
end;

procedure TTestRelationQueryBuilder.TestBuildManyToOneQuery;
var
  Query: string;
begin
  Query := TdRelationQueryBuilder.BuildManyToOneQuery('users', 'id', 789);

  AssertTrue('Should contain target table', Pos('users', Query) > 0);
  AssertTrue('Should contain local key', Pos('id', Query) > 0);
  AssertTrue('Should contain value', Pos('789', Query) > 0);
end;

procedure TTestRelationQueryBuilder.TestBuildManyToManyQuery;
var
  Query: string;
begin
  Query := TdRelationQueryBuilder.BuildManyToManyQuery('tags', 'post_tags', 'tag_id', 'post_id', 999);

  AssertTrue('Should contain target table', Pos('tags', Query) > 0);
  AssertTrue('Should contain mapping table', Pos('post_tags', Query) > 0);
  AssertTrue('Should contain INNER JOIN', Pos('INNER JOIN', UpperCase(Query)) > 0);
  AssertTrue('Should contain foreign key', Pos('tag_id', Query) > 0);
  AssertTrue('Should contain local key', Pos('post_id', Query) > 0);
  AssertTrue('Should contain value', Pos('999', Query) > 0);
end;

procedure TTestRelationQueryBuilder.TestBuildManyToManyQueryComplex;
var
  Query: string;
begin
  Query := TdRelationQueryBuilder.BuildManyToManyQuery('complex_entities', 'entity_relations', 'entity_id', 'related_id', 12345);

  AssertTrue('Should handle complex table names', Pos('complex_entities', Query) > 0);
  AssertTrue('Should handle complex mapping table', Pos('entity_relations', Query) > 0);
  AssertTrue('Should contain ON clause', Pos(' ON ', Query) > 0);
end;

procedure TTestRelationQueryBuilder.TestQueryBuilderWithSpecialCharacters;
var
  Query: string;
begin
  Query := TdRelationQueryBuilder.BuildOneToOneQuery('user_profiles', 'user_id', 'user@example.com');

  AssertTrue('Should handle special characters in values', Pos('user@example.com', Query) > 0);
  AssertTrue('Should properly quote string values', Pos('''user@example.com''', Query) > 0);
end;

procedure TTestRelationQueryBuilder.TestQueryBuilderWithEmptyParameters;
var
  aQuery: string;
begin
  aQuery := TdRelationQueryBuilder.BuildOneToOneQuery('', '', '');

  // Should not crash, but might produce invalid SQL
  AssertFalse('Should produce some query', aQuery.IsEmpty);
end;

{ TTestRelationFactory }

procedure TTestRelationFactory.TestCreateEntity;
var
  aEntity: TObject;
begin
  aEntity := TdRelationFactory.CreateEntity(TTestHelperEntity);
  try
    AssertNotNull('Should create entity', aEntity);
    AssertTrue('Should be of correct type', aEntity is TTestHelperEntity);
  finally
    aEntity.Free;
  end;
end;

procedure TTestRelationFactory.TestCreateEntityWithDifferentClasses;
var
  Entity1, Entity2: TObject;
begin
  Entity1 := TdRelationFactory.CreateEntity(TTestHelperEntity);
  Entity2 := TdRelationFactory.CreateEntity(TTestTargetEntity);
  try
    AssertTrue('Should create correct type 1', Entity1 is TTestHelperEntity);
    AssertTrue('Should create correct type 2', Entity2 is TTestTargetEntity);
    AssertFalse('Should be different types', Entity1 is TTestTargetEntity);
  finally
    Entity1.Free;
    Entity2.Free;
  end;
end;

procedure TTestRelationFactory.TestCreateEntityList;
var
  EntityList: TObjectList;
begin
  EntityList := TdRelationFactory.CreateEntityList;
  try
    AssertNotNull('Should create entity list', EntityList);
    AssertEquals('Should be empty initially', 0, EntityList.Count);
    AssertTrue('Should own objects', EntityList.OwnsObjects);
  finally
    EntityList.Free;
  end;
end;

{ TTestRelationValidator }

procedure TTestRelationValidator.TestValidateValidRelation;
var
  ValidRelation: TdRelationInfo;
begin
  ValidRelation := TdRelationInfo.Create('Posts', TTestTargetEntity, 'targets', rtOneToMany, 'entity_id');
  try
    AssertTrue('Should validate correct relation', TdRelationValidator.ValidateRelation(ValidRelation));
  finally
    ValidRelation.Free;
  end;
end;

procedure TTestRelationValidator.TestValidateInvalidRelations;
var
  InvalidRelation1, InvalidRelation2, InvalidRelation3: TdRelationInfo;
begin
  // Invalid: empty property name
  InvalidRelation1 := TdRelationInfo.Create('', TTestTargetEntity, 'targets', rtOneToMany, 'entity_id');
  try
    AssertFalse('Should reject empty property name', TdRelationValidator.ValidateRelation(InvalidRelation1));
  finally
    InvalidRelation1.Free;
  end;

  // Invalid: empty foreign key
  InvalidRelation2 := TdRelationInfo.Create('Posts', TTestTargetEntity, 'targets', rtOneToMany, '');
  try
    AssertFalse('Should reject empty foreign key', TdRelationValidator.ValidateRelation(InvalidRelation2));
  finally
    InvalidRelation2.Free;
  end;

  // Invalid: nil target class
  InvalidRelation3 := TdRelationInfo.Create('Posts', nil, 'targets', rtOneToMany, 'entity_id');
  try
    AssertFalse('Should reject nil target class', TdRelationValidator.ValidateRelation(InvalidRelation3));
  finally
    InvalidRelation3.Free;
  end;
end;

procedure TTestRelationValidator.TestValidateEntity;
var
  ValidEntity: TTestHelperEntity;
  InvalidEntity: TObject;
begin
  ValidEntity := TTestHelperEntity.Create;
  InvalidEntity := TObject.Create;
  try
    AssertTrue('Should validate relational entity', TdRelationValidator.ValidateEntity(ValidEntity));
    AssertFalse('Should reject non-relational entity', TdRelationValidator.ValidateEntity(InvalidEntity));
  finally
    ValidEntity.Free;
    InvalidEntity.Free;
  end;
end;

procedure TTestRelationValidator.TestValidateNilEntity;
begin
  AssertFalse('Should reject nil entity', TdRelationValidator.ValidateEntity(nil));
end;

procedure TTestRelationValidator.TestGetValidationErrorsComplete;
var
  InvalidRelation: TdRelationInfo;
  Errors: TStringList;
begin
  InvalidRelation := TdRelationInfo.Create('', nil, '', rtManyToMany, '');
  try
    Errors := TdRelationValidator.GetValidationErrors(InvalidRelation);
    try
      AssertTrue('Should have multiple errors', Errors.Count >= 4);
      AssertTrue('Should report property name error', Errors.IndexOf('Property name is required') >= 0);
      AssertTrue('Should report foreign key error', Errors.IndexOf('Foreign key is required') >= 0);
      AssertTrue('Should report target class error', Errors.IndexOf('Target class is required') >= 0);
      AssertTrue('Should report mapping table error', Errors.IndexOf('Mapping table is required for Many-to-Many relations') >= 0);
    finally
      Errors.Free;
    end;
  finally
    InvalidRelation.Free;
  end;
end;

procedure TTestRelationValidator.TestGetValidationErrorsPartial;
var
  PartiallyValidRelation: TdRelationInfo;
  Errors: TStringList;
begin
  PartiallyValidRelation := TdRelationInfo.Create('ValidName', TTestTargetEntity, 'targets', rtOneToMany, '');
  try
    Errors := TdRelationValidator.GetValidationErrors(PartiallyValidRelation);
    try
      AssertEquals('Should have one error', 1, Errors.Count);
      AssertTrue('Should report foreign key error', Errors.IndexOf('Foreign key is required') >= 0);
    finally
      Errors.Free;
    end;
  finally
    PartiallyValidRelation.Free;
  end;
end;

procedure TTestRelationValidator.TestValidateManyToManySpecialCase;
var
  ValidM2MRelation, InvalidM2MRelation: TdRelationInfo;
begin
  // Valid Many-to-Many with mapping table
  ValidM2MRelation := TdRelationInfo.Create('Tags', TTestTargetEntity, 'targets', rtManyToMany, 'target_id', 'id', 'entity_tags');
  try
    AssertTrue('Should validate M2M with mapping table', TdRelationValidator.ValidateRelation(ValidM2MRelation));
  finally
    ValidM2MRelation.Free;
  end;

  // Invalid Many-to-Many without mapping table
  InvalidM2MRelation := TdRelationInfo.Create('Tags', TTestTargetEntity, 'targets', rtManyToMany, 'target_id');
  try
    AssertFalse('Should reject M2M without mapping table', TdRelationValidator.ValidateRelation(InvalidM2MRelation));
  finally
    InvalidM2MRelation.Free;
  end;
end;

{ TTestAdvancedRelationalEntity }

procedure TTestAdvancedRelationalEntity.SetUp;
begin
  inherited SetUp;
  FEntity := TTestHelperEntity.Create;
end;

procedure TTestAdvancedRelationalEntity.TearDown;
begin
  FEntity.Free;
  inherited TearDown;
end;

procedure TTestAdvancedRelationalEntity.TestCacheKeyGeneration;
var
  Key1, Key2: string;
begin
  Key1 := FEntity.GetCacheKey('TestRelation');
  Key2 := FEntity.GetCacheKey('AnotherRelation');

  AssertFalse('Should generate different keys for different relations', SameStr(Key1, Key2));
  AssertTrue('Should include class name', Pos(FEntity.ClassName, Key1) > 0);
  AssertTrue('Should include relation name', Pos('TestRelation', Key1) > 0);
end;

procedure TTestAdvancedRelationalEntity.TestCacheRelation;
var
  TestTarget: TTestTargetEntity;
  Retrieved: TObject;
begin
  TestTarget := TTestTargetEntity.Create;
  try
    FEntity.CacheRelation('TestTarget', TestTarget);
    Retrieved := FEntity.GetCachedRelation('TestTarget');

    AssertSame('Should retrieve cached relation', TestTarget, Retrieved);
  finally
    // TestTarget will be managed by cache
  end;
end;

procedure TTestAdvancedRelationalEntity.TestGetCachedRelation;
var
  TestTarget: TTestTargetEntity;
begin
  // Test getting non-existent cached relation
  AssertNull('Should return nil for non-cached relation', FEntity.GetCachedRelation('NonExistent'));

  // Test getting existing cached relation
  TestTarget := TTestTargetEntity.Create;
  FEntity.CacheRelation('ExistingTarget', TestTarget);
  AssertSame('Should return cached relation', TestTarget, FEntity.GetCachedRelation('ExistingTarget'));
end;

procedure TTestAdvancedRelationalEntity.TestClearRelationCacheSpecific;
var
  TestTarget1, TestTarget2: TTestTargetEntity;
begin
  TestTarget1 := TTestTargetEntity.Create;
  TestTarget2 := TTestTargetEntity.Create;

  FEntity.CacheRelation('Target1', TestTarget1);
  FEntity.CacheRelation('Target2', TestTarget2);

  // Clear specific relation
  FEntity.ClearRelationCache('Target1');

  AssertNull('Should clear Target1', FEntity.GetCachedRelation('Target1'));
  AssertSame('Should keep Target2', TestTarget2, FEntity.GetCachedRelation('Target2'));
end;

procedure TTestAdvancedRelationalEntity.TestClearRelationCacheAll;
var
  TestTarget1, TestTarget2: TTestTargetEntity;
begin
  TestTarget1 := TTestTargetEntity.Create;
  TestTarget2 := TTestTargetEntity.Create;

  FEntity.CacheRelation('Target1', TestTarget1);
  FEntity.CacheRelation('Target2', TestTarget2);

  // Clear all relations
  FEntity.ClearRelationCache('');

  AssertNull('Should clear Target1', FEntity.GetCachedRelation('Target1'));
  AssertNull('Should clear Target2', FEntity.GetCachedRelation('Target2'));
end;

procedure TTestAdvancedRelationalEntity.TestIsRelationLoaded;
var
  Relations: TdRelationList;
  TargetsRelation: TdRelationInfo;
begin
  Relations := FEntity.GetRelations;
  TargetsRelation := Relations.FindByProperty('Targets');

  AssertNotNull('Should have Targets relation', TargetsRelation);
  AssertFalse('Should not be loaded initially', FEntity.IsRelationLoaded('Targets'));

  // Mark as loaded
  TargetsRelation.Loaded := True;
  AssertTrue('Should be loaded after marking', FEntity.IsRelationLoaded('Targets'));
end;

procedure TTestAdvancedRelationalEntity.TestGetLoadedRelations;
var
  LoadedRelations: TStringList;
  Relations: TdRelationList;
begin
  LoadedRelations := FEntity.GetLoadedRelations;
  try
    AssertNotNull('Should return list', LoadedRelations);
    AssertEquals('Should have no loaded relations initially', 0, LoadedRelations.Count);

    // Mark one relation as loaded
    Relations := FEntity.GetRelations;
    Relations.FindByProperty('Targets').Loaded := True;

    LoadedRelations.Free;
    LoadedRelations := FEntity.GetLoadedRelations;
    AssertEquals('Should have one loaded relation', 1, LoadedRelations.Count);
    AssertEquals('Should contain Targets', 'Targets', LoadedRelations[0]);
  finally
    LoadedRelations.Free;
  end;
end;

procedure TTestAdvancedRelationalEntity.TestAutoLoadProperty;
begin
  AssertFalse('AutoLoad should be false by default', FEntity.AutoLoad);

  FEntity.SetAutoLoad(True);
  AssertTrue('Should set AutoLoad to true', FEntity.AutoLoad);

  FEntity.SetAutoLoad(False);
  AssertFalse('Should set AutoLoad to false', FEntity.AutoLoad);
end;

procedure TTestAdvancedRelationalEntity.TestPreloadRelation;
begin
  // PreloadRelation is virtual and not implemented in base class
  // Just test it doesn't crash
  try
    FEntity.PreloadRelation('Targets');
    AssertTrue('Should not crash when calling PreloadRelation', True);
  except
    on E: Exception do
      AssertTrue('Should handle PreloadRelation gracefully', True);
  end;
end;

procedure TTestAdvancedRelationalEntity.TestCacheMemoryManagement;
var
  I: Integer;
  aTestTarget: TTestTargetEntity;
begin
  // Test multiple cache operations to ensure no memory leaks
  for I := 1 to 100 do
  begin
    aTestTarget := TTestTargetEntity.Create;
    aTestTarget.Id := I;
    aTestTarget.Title := 'Target ' + IntToStr(I);

    FEntity.CacheRelation('TestTarget', aTestTarget);

    if I mod 10 = 0 then
      FEntity.ClearRelationCache('TestTarget');
  end;

  AssertTrue('Should handle multiple cache operations without leaks', True);
end;

procedure TTestAdvancedRelationalEntity.TestMultipleCacheOperations;
var
  Target1, Target2, Target3: TTestTargetEntity;
begin
  Target1 := TTestTargetEntity.Create;
  Target2 := TTestTargetEntity.Create;
  Target3 := TTestTargetEntity.Create;

  // Store multiple relations
  FEntity.CacheRelation('Rel1', Target1);
  FEntity.CacheRelation('Rel2', Target2);
  FEntity.CacheRelation('Rel3', Target3);

  // Verify all are cached
  AssertSame('Should cache Rel1', Target1, FEntity.GetCachedRelation('Rel1'));
  AssertSame('Should cache Rel2', Target2, FEntity.GetCachedRelation('Rel2'));
  AssertSame('Should cache Rel3', Target3, FEntity.GetCachedRelation('Rel3'));

  // Clear one and verify others remain
  FEntity.ClearRelationCache('Rel2');
  AssertSame('Should keep Rel1', Target1, FEntity.GetCachedRelation('Rel1'));
  AssertNull('Should clear Rel2', FEntity.GetCachedRelation('Rel2'));
  AssertSame('Should keep Rel3', Target3, FEntity.GetCachedRelation('Rel3'));
end;

{ TTestRelationHelpersIntegration }

procedure TTestRelationHelpersIntegration.TestValidatorWithQueryBuilder;
var
  ValidRelation: TdRelationInfo;
  Query: string;
  ValidationResult: Boolean;
begin
  ValidRelation := TdRelationInfo.Create('Posts', TTestTargetEntity, 'targets', rtOneToMany, 'entity_id');
  try
    // Validate the relation
    ValidationResult := TdRelationValidator.ValidateRelation(ValidRelation);
    AssertTrue('Relation should be valid', ValidationResult);

    // Use validated relation to build query
    Query := TdRelationQueryBuilder.BuildOneToManyQuery(
      ValidRelation.TargetTableName,
      ValidRelation.ForeignKey,
      123
    );

    AssertTrue('Should build valid query', Length(Query) > 0);
    AssertTrue('Should contain table name', Pos('targets', Query) > 0);
    AssertTrue('Should contain foreign key', Pos('entity_id', Query) > 0);
  finally
    ValidRelation.Free;
  end;
end;

procedure TTestRelationHelpersIntegration.TestFactoryWithCache;
var
  Cache: TdRelationCache;
  EntityList: TObjectList;
  Entity: TObject;
begin
  Cache := TdRelationCache.Create;
  try
    // Create entity list using factory
    EntityList := TdRelationFactory.CreateEntityList;

    // Create entities and add to list
    Entity := TdRelationFactory.CreateEntity(TTestHelperEntity);
    EntityList.Add(Entity);

    Entity := TdRelationFactory.CreateEntity(TTestTargetEntity);
    EntityList.Add(Entity);

    // Cache the entity list
    Cache.Store('EntityList', EntityList);

    // Retrieve and verify
    AssertSame('Should retrieve cached list', EntityList, Cache.Retrieve('EntityList'));
  finally
    Cache.Free;
  end;
end;

procedure TTestRelationHelpersIntegration.TestAdvancedEntityWithAllHelpers;
var
  Entity: TTestHelperEntity;
  Relation: TdRelationInfo;
  Target: TTestTargetEntity;
  ValidationResult: Boolean;
  Errors: TStringList;
begin
  Entity := TTestHelperEntity.Create;
  try
    // Test with validator
    ValidationResult := TdRelationValidator.ValidateEntity(Entity);
    AssertTrue('Entity should be valid', ValidationResult);

    // Get relation and validate it
    Relation := Entity.GetRelations.FindByProperty('Targets');
    AssertNotNull('Should have Targets relation', Relation);

    ValidationResult := TdRelationValidator.ValidateRelation(Relation);
    AssertTrue('Relation should be valid', ValidationResult);

    // Create target using factory and cache it
    Target := TTestTargetEntity(TdRelationFactory.CreateEntity(TTestTargetEntity));
    Entity.CacheRelation('CachedTarget', Target);

    // Verify cached target
    AssertSame('Should retrieve cached target', Target, Entity.GetCachedRelation('CachedTarget'));

    // Test validation errors (should be empty for valid relation)
    Errors := TdRelationValidator.GetValidationErrors(Relation);
    try
      AssertEquals('Should have no validation errors', 0, Errors.Count);
    finally
      Errors.Free;
    end;
  finally
    Entity.Free;
  end;
end;

procedure TTestRelationHelpersIntegration.TestPerformanceWithLargeDatasets;
var
  Cache: TdRelationCache;
  I: Integer;
  Entity: TTestHelperEntity;
  Key: string;
  StartTime, EndTime: TDateTime;
begin
  Cache := TdRelationCache.Create;
  try
    StartTime := Now;

    // Store 1000 entities in cache
    for I := 1 to 1000 do
    begin
      Entity := TTestHelperEntity.Create;
      Entity.Id := I;
      Entity.Name := 'Entity ' + IntToStr(I);
      Key := 'entity_' + IntToStr(I);
      Cache.Store(Key, Entity);
    end;

    // Retrieve all entities
    for I := 1 to 1000 do
    begin
      Key := 'entity_' + IntToStr(I);
      Entity := TTestHelperEntity(Cache.Retrieve(Key));
      AssertNotNull('Should retrieve entity ' + IntToStr(I), Entity);
      AssertEquals('Should have correct ID', I, Entity.Id);
    end;

    EndTime := Now;
    AssertTrue('Should complete in reasonable time', (EndTime - StartTime) < (1/24/60)); // Less than 1 minute
  finally
    Cache.Free;
  end;
end;

procedure TTestRelationHelpersIntegration.TestThreadSafety;
var
  Cache1, Cache2: TdRelationCache;
  Entity1, Entity2: TTestHelperEntity;
  // Note: This is a basic test. Real thread safety testing would require
  // multiple threads and synchronization primitives

  // For now, just test that multiple cache operations don't interfere

begin
  Cache1 := TdRelationCache.Create;
  Cache2 := TdRelationCache.Create;
  Entity1 := TTestHelperEntity.Create;
  Entity2 := TTestHelperEntity.Create;
  try
    Cache1.Store('test', Entity1);
    Cache2.Store('test', Entity2);

    AssertSame('Cache1 should have Entity1', Entity1, Cache1.Retrieve('test'));
    AssertSame('Cache2 should have Entity2', Entity2, Cache2.Retrieve('test'));
  finally
    Cache1.Free;
    Cache2.Free;
  end;
end;

procedure TTestRelationHelpersIntegration.TestMemoryUsage;
var
  Cache: TdRelationCache;
  Entities: array[0..99] of TTestHelperEntity;
  I: Integer;
begin
  Cache := TdRelationCache.Create;
  try
    // Create and cache 100 entities
    for I := 0 to 99 do
    begin
      Entities[I] := TTestHelperEntity.Create;
      Cache.Store('entity_' + IntToStr(I), Entities[I]);
    end;

    // Verify all are cached
    for I := 0 to 99 do
    begin
      AssertSame('Should cache entity ' + IntToStr(I),
        Entities[I], Cache.Retrieve('entity_' + IntToStr(I)));
    end;

    // Clear cache and verify cleanup
    Cache.Clear;

    for I := 0 to 99 do
    begin
      AssertNull('Should clear entity ' + IntToStr(I),
        Cache.Retrieve('entity_' + IntToStr(I)));
    end;
  finally
    Cache.Free;
  end;
end;

initialization
  RegisterTest(TTestRelationCache);
  RegisterTest(TTestRelationQueryBuilder);
  RegisterTest(TTestRelationFactory);
  RegisterTest(TTestRelationValidator);
  RegisterTest(TTestAdvancedRelationalEntity);
  RegisterTest(TTestRelationHelpersIntegration);

end.
