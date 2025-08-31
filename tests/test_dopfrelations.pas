unit test_dOpfRelations;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, dOpfRelations, dOpfRelationsHelpers, Classes, SysUtils, fgl
  ;

type
  // Test entities
  { TTestPerson }
  TTestPerson = class(TdRelationalEntity)
  private
    FId: Int64;
    FName: string;
  public
    procedure ConfigureRelations({%H-}ARelations: TdRelationList); override;
  published
    property Id: Int64 read FId write FId;
    property Name: string read FName write FName;
  end;

  { TTestAdvancedEntity }

  TTestAdvancedEntity = class(TdAdvancedRelationalEntity)
  public
    procedure ConfigureRelations({%H-}ARelations: TdRelationList); override;
  end;

  { TTestOrder }
  TTestOrder = class(TdRelationalEntity)
  private
    FId: Int64;
    FPersonId: Int64;
    FAmount: Double;
  public
    procedure ConfigureRelations({%H-}ARelations: TdRelationList); override;
  published
    property Id: Int64 read FId write FId;
    property PersonId: Int64 read FPersonId write FPersonId;
    property Amount: Double read FAmount write FAmount;
  end;

  { Test classes }
  TTestRelationInfo = class(TTestCase)
  published
    procedure TestCreate;
    procedure TestProperties;
  end;

  TTestRelationList = class(TTestCase)
  published
    procedure TestFindByProperty;
    procedure TestAddRelation;
  end;

  TTestRelationalEntity = class(TTestCase)
  published
    procedure TestImplementsInterface;
    procedure TestConfigureRelations;
    procedure TestRelationSetupMethods;
    procedure TestRelationValueStorage;
  end;

  TTestRelationCache = class(TTestCase)
  published
    procedure TestStoreAndRetrieve;
    procedure TestRemove;
    procedure TestClear;
    procedure TestHasKey;
  end;

  TTestRelationQueryBuilder = class(TTestCase)
  published
    procedure TestBuildOneToOneQuery;
    procedure TestBuildOneToManyQuery;
    procedure TestBuildManyToOneQuery;
    procedure TestBuildManyToManyQuery;
  end;

  TTestRelationValidator = class(TTestCase)
  published
    procedure TestValidateRelation;
    procedure TestValidateEntity;
    procedure TestGetValidationErrors;
  end;

  TTestAdvancedRelationalEntity = class(TTestCase)
  published
    procedure TestCacheOperations;
    procedure TestIsRelationLoaded;
    procedure TestGetLoadedRelations;
  end;

implementation

{ TTestPerson }

procedure TTestPerson.ConfigureRelations(ARelations: TdRelationList);
begin
  HasMany('Orders', TTestOrder, 'person_id', 'id');
end;

{ TTestAdvancedEntity }

procedure TTestAdvancedEntity.ConfigureRelations(ARelations: TdRelationList);
begin
  // None actions
end;

{ TTestOrder }

procedure TTestOrder.ConfigureRelations(ARelations: TdRelationList);
begin
  BelongsTo('Person', TTestPerson, 'id', 'person_id');
end;

{ TTestRelationInfo }

procedure TTestRelationInfo.TestCreate;
var
  RelInfo: TdRelationInfo;
begin
  RelInfo := TdRelationInfo.Create('Orders', TTestOrder, rtOneToMany, 'person_id');
  try
    AssertNotNull('RelationInfo should be created', RelInfo);
    AssertEquals('Property name', 'Orders', RelInfo.PropertyName);
    AssertEquals('Target class', TTestOrder, RelInfo.TargetClass);
    AssertEquals('Relation type', Ord(rtOneToMany), Ord(RelInfo.RelationType));
    AssertEquals('Foreign key', 'person_id', RelInfo.ForeignKey);
    AssertEquals('Local key default', 'id', RelInfo.LocalKey);
    AssertFalse('Should not be loaded initially', RelInfo.Loaded);
  finally
    RelInfo.Free;
  end;
end;

procedure TTestRelationInfo.TestProperties;
var
  RelInfo: TdRelationInfo;
begin
  RelInfo := TdRelationInfo.Create('Profile', TTestPerson, rtOneToOne, 'person_id', 'user_id');
  try
    AssertEquals('Local key custom', 'user_id', RelInfo.LocalKey);
    AssertEquals('Load strategy default', Ord(lsLazy), Ord(RelInfo.LoadStrategy));

    RelInfo.LoadStrategy := lsEager;
    AssertEquals('Load strategy changed', Ord(lsEager), Ord(RelInfo.LoadStrategy));

    RelInfo.Loaded := True;
    AssertTrue('Loaded status changed', RelInfo.Loaded);
  finally
    RelInfo.Free;
  end;
end;

{ TTestRelationList }

procedure TTestRelationList.TestFindByProperty;
var
  Relations: TdRelationList;
  RelInfo: TdRelationInfo;
begin
  Relations := TdRelationList.Create;
  try
    Relations.Add(TdRelationInfo.Create('Orders', TTestOrder, rtOneToMany, 'person_id'));
    Relations.Add(TdRelationInfo.Create('Profile', TTestPerson, rtOneToOne, 'person_id'));

    RelInfo := Relations.FindByProperty('Orders');
    AssertNotNull('Should find Orders relation', RelInfo);
    AssertEquals('Should find correct relation', 'Orders', RelInfo.PropertyName);

    RelInfo := Relations.FindByProperty('NonExistent');
    AssertNull('Should not find non-existent relation', RelInfo);
  finally
    Relations.Free;
  end;
end;

procedure TTestRelationList.TestAddRelation;
var
  Relations: TdRelationList;
begin
  Relations := TdRelationList.Create;
  try
    AssertEquals('Initial count', 0, Relations.Count);

    Relations.Add(TdRelationInfo.Create('Orders', TTestOrder, rtOneToMany, 'person_id'));
    AssertEquals('Count after add', 1, Relations.Count);

    Relations.Add(TdRelationInfo.Create('Profile', TTestPerson, rtOneToOne, 'person_id'));
    AssertEquals('Count after second add', 2, Relations.Count);
  finally
    Relations.Free;
  end;
end;

{ TTestRelationalEntity }

procedure TTestRelationalEntity.TestImplementsInterface;
var
  Person: TTestPerson;
  RelEntity: IdRelationalEntity;
begin
  Person := TTestPerson.Create;
  try
    AssertTrue('Should implement IdRelationalEntity', Supports(Person, IdRelationalEntity, RelEntity));
    AssertNotNull('Interface should not be nil', RelEntity);
  finally
    Person.Free;
  end;
end;

procedure TTestRelationalEntity.TestConfigureRelations;
var
  Person: TTestPerson;
  Relations: TdRelationList;
  RelInfo: TdRelationInfo;
begin
  Person := TTestPerson.Create;
  try
    Relations := Person.GetRelations;
    AssertNotNull('Relations should be configured', Relations);
    AssertEquals('Should have one relation', 1, Relations.Count);

    RelInfo := Relations.FindByProperty('Orders');
    AssertNotNull('Should find Orders relation', RelInfo);
    AssertEquals('Relation type', Ord(rtOneToMany), Ord(RelInfo.RelationType));
    AssertEquals('Target class', TTestOrder, RelInfo.TargetClass);
    AssertEquals('Foreign key', 'person_id', RelInfo.ForeignKey);
  finally
    Person.Free;
  end;
end;

procedure TTestRelationalEntity.TestRelationSetupMethods;
var
  Person: TTestPerson;
  Relations: TdRelationList;
  RelInfo: TdRelationInfo;
begin
  Person := TTestPerson.Create;
  try
    Relations := Person.GetRelations;
    Relations.Clear; // Clear automatically created relations

    // Test HasOne
    Person.HasOne('Profile', TTestPerson, 'person_id');
    AssertEquals('Should have one relation after HasOne', 1, Relations.Count);
    RelInfo := Relations[0];
    AssertEquals('HasOne relation type', Ord(rtOneToOne), Ord(RelInfo.RelationType));

    // Test HasMany
    Person.HasMany('Orders', TTestOrder, 'person_id');
    AssertEquals('Should have two relations after HasMany', 2, Relations.Count);
    RelInfo := Relations[1];
    AssertEquals('HasMany relation type', Ord(rtOneToMany), Ord(RelInfo.RelationType));

    // Test BelongsTo
    Person.BelongsTo('Company', TTestPerson, 'company_id');
    AssertEquals('Should have three relations after BelongsTo', 3, Relations.Count);
    RelInfo := Relations[2];
    AssertEquals('BelongsTo relation type', Ord(rtManyToOne), Ord(RelInfo.RelationType));

    // Test BelongsToMany
    Person.BelongsToMany('Tags', TTestOrder, 'person_tags', 'tag_id', 'person_id');
    AssertEquals('Should have four relations after BelongsToMany', 4, Relations.Count);
    RelInfo := Relations[3];
    AssertEquals('BelongsToMany relation type', Ord(rtManyToMany), Ord(RelInfo.RelationType));
    AssertEquals('Mapping table', 'person_tags', RelInfo.MappingTable);
  finally
    Person.Free;
  end;
end;

procedure TTestRelationalEntity.TestRelationValueStorage;
var
  Person: TTestPerson;
  TestOrder: TTestOrder;
begin
  Person := TTestPerson.Create;
  TestOrder := TTestOrder.Create;
  try
    // Test setting and getting relation value
    Person.SetRelationValue('TestOrder', TestOrder);

    AssertSame('Should retrieve stored relation value', TestOrder, Person.GetRelationValue('TestOrder'));

    // Test getting non-existent relation
    AssertNull('Should return nil for non-existent relation', Person.GetRelationValue('NonExistent'));
  finally
    Person.Free;
    // TestOrder will be freed automatically via TStringList.OwnsObjects
  end;
end;

{ TTestRelationCache }

procedure TTestRelationCache.TestStoreAndRetrieve;
var
  Cache: TdRelationCache;
  TestObj: TTestPerson;
  Retrieved: TObject;
begin
  Cache := TdRelationCache.Create;
  TestObj := TTestPerson.Create;
  try
    Cache.Store('test_key', TestObj);
    Retrieved := Cache.Retrieve('test_key');

    AssertNotNull('Should retrieve stored object', Retrieved);
    AssertSame('Should retrieve same object', TestObj, Retrieved);

    AssertNull('Should return nil for non-existent key',
      Cache.Retrieve('non_existent'));
  finally
    Cache.Free;
    // TestObj will be freed automatically
  end;
end;

procedure TTestRelationCache.TestRemove;
var
  Cache: TdRelationCache;
  TestObj: TTestPerson;
begin
  Cache := TdRelationCache.Create;
  TestObj := TTestPerson.Create;
  try
    Cache.Store('test_key', TestObj);
    AssertNotNull('Should have stored object', Cache.Retrieve('test_key'));

    Cache.Remove('test_key');
    AssertNull('Should not find removed object', Cache.Retrieve('test_key'));
  finally
    Cache.Free;
  end;
end;

procedure TTestRelationCache.TestClear;
var
  Cache: TdRelationCache;
  TestObj1, TestObj2: TTestPerson;
begin
  Cache := TdRelationCache.Create;
  TestObj1 := TTestPerson.Create;
  TestObj2 := TTestPerson.Create;
  try
    Cache.Store('key1', TestObj1);
    Cache.Store('key2', TestObj2);

    AssertTrue('Should have key1', Cache.HasKey('key1'));
    AssertTrue('Should have key2', Cache.HasKey('key2'));

    Cache.Clear;

    AssertFalse('Should not have key1 after clear', Cache.HasKey('key1'));
    AssertFalse('Should not have key2 after clear', Cache.HasKey('key2'));
  finally
    Cache.Free;
  end;
end;

procedure TTestRelationCache.TestHasKey;
var
  Cache: TdRelationCache;
  TestObj: TTestPerson;
begin
  Cache := TdRelationCache.Create;
  TestObj := TTestPerson.Create;
  try
    AssertFalse('Should not have key initially', Cache.HasKey('test_key'));

    Cache.Store('test_key', TestObj);
    AssertTrue('Should have key after storing', Cache.HasKey('test_key'));
  finally
    Cache.Free;
  end;
end;

{ TTestRelationQueryBuilder }

procedure TTestRelationQueryBuilder.TestBuildOneToOneQuery;
var
  Query: string;
begin
  Query := TdRelationQueryBuilder.BuildOneToOneQuery('profiles', 'person_id', 123);
  AssertTrue('Should contain table name', Pos('profiles', Query) > 0);
  AssertTrue('Should contain foreign key', Pos('person_id', Query) > 0);
  AssertTrue('Should contain value', Pos('123', Query) > 0);
  AssertTrue('Should contain WHERE clause', Pos('WHERE', Query) > 0);
end;

procedure TTestRelationQueryBuilder.TestBuildOneToManyQuery;
var
  Query: string;
begin
  Query := TdRelationQueryBuilder.BuildOneToManyQuery('orders', 'person_id', 456);
  AssertTrue('Should contain table name', Pos('orders', Query) > 0);
  AssertTrue('Should contain foreign key', Pos('person_id', Query) > 0);
  AssertTrue('Should contain value', Pos('456', Query) > 0);
  AssertTrue('Should contain ORDER BY', Pos('ORDER BY', Query) > 0);
end;

procedure TTestRelationQueryBuilder.TestBuildManyToOneQuery;
var
  Query: string;
begin
  Query := TdRelationQueryBuilder.BuildManyToOneQuery('persons', 'id', 789);
  AssertTrue('Should contain table name', Pos('persons', Query) > 0);
  AssertTrue('Should contain local key', Pos('id', Query) > 0);
  AssertTrue('Should contain value', Pos('789', Query) > 0);
end;

procedure TTestRelationQueryBuilder.TestBuildManyToManyQuery;
var
  Query: string;
begin
  Query := TdRelationQueryBuilder.BuildManyToManyQuery('tags', 'person_tags',
    'tag_id', 'person_id', 999);
  AssertTrue('Should contain target table', Pos('tags', Query) > 0);
  AssertTrue('Should contain mapping table', Pos('person_tags', Query) > 0);
  AssertTrue('Should contain INNER JOIN', Pos('INNER JOIN', Query) > 0);
  AssertTrue('Should contain value', Pos('999', Query) > 0);
end;

{ TTestRelationValidator }

procedure TTestRelationValidator.TestValidateRelation;
var
  ValidRelation, InvalidRelation: TdRelationInfo;
begin
  // Valid relation
  ValidRelation := TdRelationInfo.Create('Orders', TTestOrder, rtOneToMany, 'person_id');
  try
    AssertTrue('Valid relation should pass validation',
      TdRelationValidator.ValidateRelation(ValidRelation));
  finally
    ValidRelation.Free;
  end;

  // Invalid relation (missing property name)
  InvalidRelation := TdRelationInfo.Create('', TTestOrder, rtOneToMany, 'person_id');
  try
    AssertFalse('Invalid relation should fail validation',
      TdRelationValidator.ValidateRelation(InvalidRelation));
  finally
    InvalidRelation.Free;
  end;
end;

procedure TTestRelationValidator.TestValidateEntity;
var
  ValidEntity: TTestPerson;
  InvalidEntity: TObject;
begin
  ValidEntity := TTestPerson.Create;
  InvalidEntity := TObject.Create;
  try
    AssertTrue('Relational entity should be valid',
      TdRelationValidator.ValidateEntity(ValidEntity));
    AssertFalse('Regular object should be invalid',
      TdRelationValidator.ValidateEntity(InvalidEntity));
    AssertFalse('Nil entity should be invalid',
      TdRelationValidator.ValidateEntity(nil));
  finally
    ValidEntity.Free;
    InvalidEntity.Free;
  end;
end;

procedure TTestRelationValidator.TestGetValidationErrors;
var
  InvalidRelation: TdRelationInfo;
  Errors: TStringList;
begin
  InvalidRelation := TdRelationInfo.Create('', nil, rtManyToMany, '');
  try
    Errors := TdRelationValidator.GetValidationErrors(InvalidRelation);
    try
      AssertTrue('Should have validation errors', Errors.Count > 0);
      AssertTrue('Should report missing property name',
        Errors.IndexOf('Property name is required') >= 0);
      AssertTrue('Should report missing foreign key',
        Errors.IndexOf('Foreign key is required') >= 0);
      AssertTrue('Should report missing target class',
        Errors.IndexOf('Target class is required') >= 0);
      AssertTrue('Should report missing mapping table for M2M',
        Errors.IndexOf('Mapping table is required for Many-to-Many relations') >= 0);
    finally
      Errors.Free;
    end;
  finally
    InvalidRelation.Free;
  end;
end;

{ TTestAdvancedRelationalEntity }

procedure TTestAdvancedRelationalEntity.TestCacheOperations;
var
  Entity: TTestAdvancedEntity;
  TestObj: TTestPerson;
begin
  Entity := TTestAdvancedEntity.Create;
  TestObj := TTestPerson.Create;
  try
    // Test caching
    Entity.CacheRelation('TestRelation', TestObj);
    AssertSame('Should retrieve cached relation', TestObj, Entity.GetCachedRelation('TestRelation'));

    // Test cache clearing
    Entity.ClearRelationCache('TestRelation');
    AssertNull('Should not find relation after cache clear',
      Entity.GetCachedRelation('TestRelation'));
  finally
    Entity.Free;
    // TestObj is managed by cache
  end;
end;

procedure TTestAdvancedRelationalEntity.TestIsRelationLoaded;
var
  Entity: TTestAdvancedEntity;
begin
  Entity := TTestAdvancedEntity.Create;
  try
    // Since ConfigureRelations is not overridden, there will be no relations
    AssertFalse('Non-existent relation should not be loaded',
      Entity.IsRelationLoaded('NonExistent'));
  finally
    Entity.Free;
  end;
end;

procedure TTestAdvancedRelationalEntity.TestGetLoadedRelations;
var
  Entity: TTestAdvancedEntity;
  LoadedRelations: TStringList;
begin
  Entity := TTestAdvancedEntity.Create;
  try
    LoadedRelations := Entity.GetLoadedRelations;
    try
      AssertNotNull('Should return loaded relations list', LoadedRelations);
      AssertEquals('Should have no loaded relations initially', 0, LoadedRelations.Count);
    finally
      LoadedRelations.Free;
    end;
  finally
    Entity.Free;
  end;
end;

initialization
  RegisterTest(TTestRelationInfo);
  RegisterTest(TTestRelationList);
  RegisterTest(TTestRelationalEntity);
  RegisterTest(TTestRelationCache);
  RegisterTest(TTestRelationQueryBuilder);
  RegisterTest(TTestRelationValidator);
  RegisterTest(TTestAdvancedRelationalEntity);

end.
