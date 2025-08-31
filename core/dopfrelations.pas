(*
  Copyright (C) 2025 Renat Suleymanov

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit dOpfRelations;

{$i dopf.inc}
{$interfaces corba}

interface

uses
  dClasses, dOpf, Classes, SysUtils, Contnrs, FGL;

type
  TdRelationType = (rtOneToOne, rtOneToMany, rtManyToOne, rtManyToMany);

  TdLoadStrategy = (lsLazy, lsEager);

  { TdRelationInfo }
  TdRelationInfo = class(TdObject)
  private
    FRelationType: TdRelationType;
    FPropertyName: string;
    FTargetClass: TClass;
    FForeignKey: string;
    FMappingTable: string; // For Many-to-Many
    FLocalKey: string;
    FLoadStrategy: TdLoadStrategy;
    FLoaded: Boolean;
  public
    constructor Create(const APropertyName: string; ATargetClass: TClass;
      ARelationType: TdRelationType; const AForeignKey: string;
      const ALocalKey: string = 'id'; const AMappingTable: string = '');
    property RelationType: TdRelationType read FRelationType;
    property PropertyName: string read FPropertyName;
    property TargetClass: TClass read FTargetClass;
    property ForeignKey: string read FForeignKey;
    property LocalKey: string read FLocalKey;
    property MappingTable: string read FMappingTable;
    property LoadStrategy: TdLoadStrategy read FLoadStrategy write FLoadStrategy;
    property Loaded: Boolean read FLoaded write FLoaded;
  end;

  { TdRelationList }
  TdRelationList = class(specialize TFPGObjectList<TdRelationInfo>)
  public
    function FindByProperty(const APropertyName: string): TdRelationInfo;
  end;

  { IdRelationalEntity - interface for entities with relations }
  IdRelationalEntity = interface
    ['{A1234567-B123-C123-D123-E12345678901}']
    procedure ConfigureRelations(ARelations: TdRelationList);
    function GetRelations: TdRelationList;
    procedure SetRelationValue(const APropertyName: string; AValue: TObject);
    function GetRelationValue(const APropertyName: string): TObject;
  end;

  { TdRelationalEntity - base class for entities with relations }
  TdRelationalEntity = class(TdObject, IdRelationalEntity)
  private
    FRelationValues: TStringList; // Stores pointers to objects
  protected
    FRelations: TdRelationList;
    procedure InitializeRelations; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // IdRelationalEntity interface methods
    procedure ConfigureRelations(ARelations: TdRelationList); virtual; abstract;
    function GetRelations: TdRelationList;
    procedure SetRelationValue(const APropertyName: string; AValue: TObject);
    function GetRelationValue(const APropertyName: string): TObject;

    // Convenience methods for configuring relations
    procedure HasOne(const APropertyName: string; ATargetClass: TClass;
      const AForeignKey: string; const ALocalKey: string = 'id');
    procedure HasMany(const APropertyName: string; ATargetClass: TClass;
      const AForeignKey: string; const ALocalKey: string = 'id');
    procedure BelongsTo(const APropertyName: string; ATargetClass: TClass;
      const AForeignKey: string; const ALocalKey: string = 'id');
    procedure BelongsToMany(const APropertyName: string; ATargetClass: TClass;
      const AMappingTable, AForeignKey, ALocalKey: string);
  end;

  { TdGRelationalOpf - extended OPF with relation support }
  generic TdGRelationalOpf<T1, T2; T3: TObject> = class(specialize TdGOpf<T1, T2, T3>)
  public type
    TRelatedEntities = specialize TFPGObjectList<TObject>;
  private
    function IsRelationalEntity(AEntity: T3): Boolean;
    function GetRelationalEntity(AEntity: T3): IdRelationalEntity;
  public
    // Loading related data
    procedure LoadRelation(AEntity: T3; const ARelationName: string);
    procedure LoadAllRelations(AEntity: T3);

    // Getting related data (returns TObject, needs type casting)
    function GetRelatedObject(AEntity: T3; const ARelationName: string): TObject;
    function GetRelatedObjectList(AEntity: T3; const ARelationName: string): TRelatedEntities;

    // Saving with relations
    procedure SaveWithRelations(AEntity: T3);

    // Cascade deletion
    procedure DeleteWithCascade(AEntity: T3);
  end;

  { TdGRelationalEntityOpf - simplified version for working with a single entity }
  generic TdGRelationalEntityOpf<T1, T2; T3: TObject> = class(specialize TdGEntityOpf<T1, T2, T3>)
  private
    function IsRelationalEntity: Boolean;
    function GetRelationalEntity: IdRelationalEntity;
  public
    procedure LoadRelation(const ARelationName: string);
    procedure LoadAllRelations;
    function GetRelatedObject(const ARelationName: string): TObject;
    function GetRelatedObjectList(const ARelationName: string): TObjectList;
    procedure SaveWithRelations;
  end;

  { Typed helper classes for safe type casting }

  { TdGRelatedObjectHelper }
  generic TdGRelatedObjectHelper<T: class> = class(TdObject)
  type
    TEntities = specialize TFPGObjectList<T>;
  public
    class function SafeCast(AObject: TObject): T;
    class function SafeCastList(AList: TObjectList): TEntities;
  end;

implementation

uses
  TypInfo, Variants;

{ TdRelationInfo }

constructor TdRelationInfo.Create(const APropertyName: string; ATargetClass: TClass;
  ARelationType: TdRelationType; const AForeignKey: string;
  const ALocalKey: string; const AMappingTable: string);
begin
  inherited Create;
  FPropertyName := APropertyName;
  FTargetClass := ATargetClass;
  FRelationType := ARelationType;
  FForeignKey := AForeignKey;
  FLocalKey := ALocalKey;
  FMappingTable := AMappingTable;
  FLoadStrategy := lsLazy; // Lazy loading by default
  FLoaded := False;
end;

{ TdRelationList }

function TdRelationList.FindByProperty(const APropertyName: string): TdRelationInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if SameText(Items[I].PropertyName, APropertyName) then
    begin
      Result := Items[I];
      Exit;
    end;
end;

{ TdRelationalEntity }

constructor TdRelationalEntity.Create;
begin
  inherited Create;
  FRelations := TdRelationList.Create;
  FRelationValues := TStringList.Create;
  FRelationValues.OwnsObjects := True; // Automatic memory management
  InitializeRelations;
end;

destructor TdRelationalEntity.Destroy;
begin
  FRelationValues.Free;
  FRelations.Free;
  inherited Destroy;
end;

procedure TdRelationalEntity.InitializeRelations;
begin
  ConfigureRelations(FRelations);
end;

function TdRelationalEntity.GetRelations: TdRelationList;
begin
  Result := FRelations;
end;

procedure TdRelationalEntity.SetRelationValue(const APropertyName: string; AValue: TObject);
var
  Index: Integer;
begin
  Index := FRelationValues.IndexOf(APropertyName);
  if Index >= 0 then
    FRelationValues.Objects[Index] := AValue
  else
    FRelationValues.AddObject(APropertyName, AValue);
end;

function TdRelationalEntity.GetRelationValue(const APropertyName: string): TObject;
var
  Index: Integer;
begin
  Index := FRelationValues.IndexOf(APropertyName);
  if Index >= 0 then
    Result := FRelationValues.Objects[Index]
  else
    Result := nil;
end;

procedure TdRelationalEntity.HasOne(const APropertyName: string; ATargetClass: TClass;
  const AForeignKey: string; const ALocalKey: string);
begin
  FRelations.Add(TdRelationInfo.Create(APropertyName, ATargetClass,
    rtOneToOne, AForeignKey, ALocalKey));
end;

procedure TdRelationalEntity.HasMany(const APropertyName: string; ATargetClass: TClass;
  const AForeignKey: string; const ALocalKey: string);
begin
  FRelations.Add(TdRelationInfo.Create(APropertyName, ATargetClass,
    rtOneToMany, AForeignKey, ALocalKey));
end;

procedure TdRelationalEntity.BelongsTo(const APropertyName: string; ATargetClass: TClass;
  const AForeignKey: string; const ALocalKey: string);
begin
  FRelations.Add(TdRelationInfo.Create(APropertyName, ATargetClass,
    rtManyToOne, AForeignKey, ALocalKey));
end;

procedure TdRelationalEntity.BelongsToMany(const APropertyName: string; ATargetClass: TClass;
  const AMappingTable, AForeignKey, ALocalKey: string);
begin
  FRelations.Add(TdRelationInfo.Create(APropertyName, ATargetClass,
    rtManyToMany, AForeignKey, ALocalKey, AMappingTable));
end;

{ TdGRelationalOpf }

function TdGRelationalOpf.IsRelationalEntity(AEntity: T3): Boolean;
begin
  Result := Supports(AEntity, IdRelationalEntity);
end;

function TdGRelationalOpf.GetRelationalEntity(AEntity: T3): IdRelationalEntity;
begin
  if not Supports(AEntity, IdRelationalEntity, Result) then
    raise EdOpf.CreateFmt('Entity "%s" does not support relations', [AEntity.ClassName]);
end;

procedure TdGRelationalOpf.LoadRelation(AEntity: T3; const ARelationName: string);
var
  RelEntity: IdRelationalEntity;
  RelInfo: TdRelationInfo;
  SQL: string;
  LocalKeyValue: Variant;
  LocalKeyProp: PPropInfo;
  TempQuery: T2;
  RelatedObj: TObject;
  RelatedList: TRelatedEntities;
begin
  if not IsRelationalEntity(AEntity) then
    raise EdOpf.CreateFmt('Entity "%s" does not support relations', [AEntity.ClassName]);

  RelEntity := GetRelationalEntity(AEntity);
  RelInfo := RelEntity.GetRelations.FindByProperty(ARelationName);

  if RelInfo = nil then
    raise EdOpf.CreateFmt('Relation "%s" not found in entity "%s"',
      [ARelationName, AEntity.ClassName]);

  if RelInfo.Loaded then
    Exit; // Already loaded

  // Get local key value
  LocalKeyProp := GetPropInfo(PTypeInfo(AEntity.ClassInfo), RelInfo.LocalKey);
  if LocalKeyProp = nil then
    raise EdOpf.CreateFmt('Local key property "%s" not found', [RelInfo.LocalKey]);

  LocalKeyValue := GetVariantProp(AEntity, LocalKeyProp);

  // Create temporary query for loading related data
  TempQuery := T2.Create(FConnection);
  try
    case RelInfo.RelationType of
      rtOneToOne, rtManyToOne:
        begin
          // SELECT * FROM target_table WHERE foreign_key = :local_value
          SQL := Format('SELECT * FROM %s WHERE %s = :local_value',
            [LowerCase(RelInfo.TargetClass.ClassName), RelInfo.ForeignKey]);
          TempQuery.SQL.Text := SQL;
          TempQuery.Param('local_value').Value := LocalKeyValue;
          TempQuery.Open;

          if not TempQuery.IsEmpty then
          begin
            RelatedObj := RelInfo.TargetClass.Create;
            TempQuery.GetFields(RelatedObj);
            RelEntity.SetRelationValue(ARelationName, RelatedObj);
          end;
        end;

      rtOneToMany:
        begin
          // SELECT * FROM target_table WHERE foreign_key = :local_value
          SQL := Format('SELECT * FROM %s WHERE %s = :local_value',
            [LowerCase(RelInfo.TargetClass.ClassName), RelInfo.ForeignKey]);
          TempQuery.SQL.Text := SQL;
          TempQuery.Param('local_value').Value := LocalKeyValue;
          TempQuery.Open;

          RelatedList := TRelatedEntities.Create;
          TempQuery.First;
          while not TempQuery.EOF do
          begin
            RelatedObj := RelInfo.TargetClass.Create;
            TempQuery.GetFields(RelatedObj);
            RelatedList.Add(RelatedObj);
            TempQuery.Next;
          end;
          RelEntity.SetRelationValue(ARelationName, RelatedList);
        end;

      rtManyToMany:
        begin
          // SELECT t.* FROM target_table t
          // JOIN mapping_table m ON t.id = m.target_id
          // WHERE m.local_id = :local_value
          SQL := Format(
            'SELECT t.* FROM %s t JOIN %s m ON t.%s = m.%s WHERE m.%s = :local_value',
            [LowerCase(RelInfo.TargetClass.ClassName), RelInfo.MappingTable,
             RelInfo.LocalKey, RelInfo.ForeignKey + '_target',
             RelInfo.LocalKey + '_local']);
          TempQuery.SQL.Text := SQL;
          TempQuery.Param('local_value').Value := LocalKeyValue;
          TempQuery.Open;

          RelatedList := TRelatedEntities.Create;
          TempQuery.First;
          while not TempQuery.EOF do
          begin
            RelatedObj := RelInfo.TargetClass.Create;
            TempQuery.GetFields(RelatedObj);
            RelatedList.Add(RelatedObj);
            TempQuery.Next;
          end;
          RelEntity.SetRelationValue(ARelationName, RelatedList);
        end;
    end;

    RelInfo.Loaded := True;
  finally
    TempQuery.Free;
  end;
end;

procedure TdGRelationalOpf.LoadAllRelations(AEntity: T3);
var
  RelEntity: IdRelationalEntity;
  I: Integer;
begin
  if not IsRelationalEntity(AEntity) then
    Exit;

  RelEntity := GetRelationalEntity(AEntity);
  for I := 0 to RelEntity.GetRelations.Count - 1 do
    LoadRelation(AEntity, RelEntity.GetRelations[I].PropertyName);
end;

function TdGRelationalOpf.GetRelatedObject(AEntity: T3; const ARelationName: string): TObject;
var
  RelEntity: IdRelationalEntity;
begin
  Result := nil;
  if not IsRelationalEntity(AEntity) then
    Exit;

  RelEntity := GetRelationalEntity(AEntity);

  // Try to load if not loaded yet
  LoadRelation(AEntity, ARelationName);

  Result := RelEntity.GetRelationValue(ARelationName);
end;

function TdGRelationalOpf.GetRelatedObjectList(AEntity: T3;
  const ARelationName: string): TRelatedEntities;
var
  RelEntity: IdRelationalEntity;
  RelObj: TObject;
begin
  Result := TRelatedEntities.Create(False); // Doesn't own objects

  if not IsRelationalEntity(AEntity) then
    Exit;

  RelEntity := GetRelationalEntity(AEntity);

  // Try to load if not loaded yet
  LoadRelation(AEntity, ARelationName);

  RelObj := RelEntity.GetRelationValue(ARelationName);
  if (RelObj <> nil) and (RelObj is TRelatedEntities) then
  begin
    Result.Free;
    Result := TRelatedEntities(RelObj);
  end;
end;

procedure TdGRelationalOpf.SaveWithRelations(AEntity: T3);
var
  RelEntity: IdRelationalEntity;
  I: Integer;
  RelInfo: TdRelationInfo;
  RelObj: TObject;
  RelList: TRelatedEntities;
  J: Integer;
begin
  // First save the main entity
  if FTable.PrimaryKeys.Count > 0 then
    Modify(AEntity)
  else
    Add(AEntity);

  if not IsRelationalEntity(AEntity) then
    Exit;

  RelEntity := GetRelationalEntity(AEntity);

  // Save related entities
  for I := 0 to RelEntity.GetRelations.Count - 1 do
  begin
    RelInfo := RelEntity.GetRelations[I];
    RelObj := RelEntity.GetRelationValue(RelInfo.PropertyName);

    if RelObj = nil then
      Continue;

    case RelInfo.RelationType of
      rtOneToOne, rtManyToOne:
        begin
          // Save related entity
          // TODO: Need to create OPF for target class and save
        end;

      rtOneToMany, rtManyToMany:
        begin
          if RelObj is TRelatedEntities then
          begin
            RelList := TRelatedEntities(RelObj);
            for J := 0 to RelList.Count - 1 do
            begin
              // Save each related entity
              // TODO: Need to create OPF for target class and save
            end;
          end;
        end;
    end;
  end;
end;

procedure TdGRelationalOpf.DeleteWithCascade(AEntity: T3);
begin
  // TODO: Implement cascade deletion
  Remove(AEntity);
end;

{ TdGRelationalEntityOpf }

function TdGRelationalEntityOpf.IsRelationalEntity: Boolean;
begin
  Result := Supports(FEntity, IdRelationalEntity);
end;

function TdGRelationalEntityOpf.GetRelationalEntity: IdRelationalEntity;
begin
  if not Supports(FEntity, IdRelationalEntity, Result) then
    raise EdOpf.CreateFmt('Entity "%s" does not support relations', [FEntity.ClassName]);
end;

procedure TdGRelationalEntityOpf.LoadRelation(const ARelationName: string);
var
  RelOpf: specialize TdGRelationalOpf<T1, T2, T3>;
begin
  RelOpf := specialize TdGRelationalOpf<T1, T2, T3>.Create(FConnection, FTable.Name);
  try
    RelOpf.LoadRelation(FEntity, ARelationName);
  finally
    RelOpf.Free;
  end;
end;

procedure TdGRelationalEntityOpf.LoadAllRelations;
var
  RelOpf: specialize TdGRelationalOpf<T1, T2, T3>;
begin
  RelOpf := specialize TdGRelationalOpf<T1, T2, T3>.Create(FConnection, FTable.Name);
  try
    RelOpf.LoadAllRelations(FEntity);
  finally
    RelOpf.Free;
  end;
end;

function TdGRelationalEntityOpf.GetRelatedObject(const ARelationName: string): TObject;
var
  RelOpf: specialize TdGRelationalOpf<T1, T2, T3>;
begin
  RelOpf := specialize TdGRelationalOpf<T1, T2, T3>.Create(FConnection, FTable.Name);
  try
    Result := RelOpf.GetRelatedObject(FEntity, ARelationName);
  finally
    RelOpf.Free;
  end;
end;

function TdGRelationalEntityOpf.GetRelatedObjectList(const ARelationName: string): TObjectList;
var
  RelOpf: specialize TdGRelationalOpf<T1, T2, T3>;
  TempList: specialize TFPGObjectList<TObject>;
  I: Integer;
begin
  Result := TObjectList.Create(False); // Doesn't own objects
  RelOpf := specialize TdGRelationalOpf<T1, T2, T3>.Create(FConnection, FTable.Name);
  try
    TempList := RelOpf.GetRelatedObjectList(FEntity, ARelationName);
    try
      for I := 0 to TempList.Count - 1 do
        Result.Add(TempList[I]);
    finally
      TempList.Free;
    end;
  finally
    RelOpf.Free;
  end;
end;

{ TdGRelatedObjectHelper }

class function TdGRelatedObjectHelper.SafeCast(AObject: TObject): T;
begin
  if (AObject <> nil) and (AObject is T) then
    Result := T(AObject)
  else
    Result := nil;
end;

class function TdGRelatedObjectHelper.SafeCastList(AList: TObjectList): TEntities;
var
  I: Integer;
  CastedItem: T;
begin
  Result := TEntities.Create(False); // Doesn't own objects
  if AList = nil then
    Exit;

  for I := 0 to AList.Count - 1 do
  begin
    CastedItem := SafeCast(AList[I]);
    if CastedItem <> nil then
      Result.Add(CastedItem);
  end;
end;

procedure TdGRelationalEntityOpf.SaveWithRelations;
var
  RelOpf: specialize TdGRelationalOpf<T1, T2, T3>;
begin
  RelOpf := specialize TdGRelationalOpf<T1, T2, T3>.Create(FConnection, FTable.Name);
  try
    RelOpf.SaveWithRelations(FEntity);
  finally
    RelOpf.Free;
  end;
end;

end.
