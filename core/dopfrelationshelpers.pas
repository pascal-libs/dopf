(*
  Copyright (C) 2025 Renat Suleymanov

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit dOpfRelationsHelpers;

{$i dopf.inc}
{$interfaces corba}

interface

uses
  dClasses, dOpfRelations, Classes, SysUtils, Contnrs, fgl;

type
  { TdRelationCache - Cache for loaded relations }
  TdRelationCache = class(TdObject)
  private
    FCache: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Store(const AKey: string; AValue: TObject);
    function Retrieve(const AKey: string): TObject;
    procedure Remove(const AKey: string);
    procedure Clear;
    function HasKey(const AKey: string): Boolean;
  end;

  { TdRelationQueryBuilder - Query builder for relations }
  TdRelationQueryBuilder = class(TdObject)
  public
    class function BuildOneToOneQuery(const ATargetTable, AForeignKey: string;
      const ALocalValue: Variant): string;
    class function BuildOneToManyQuery(const ATargetTable, AForeignKey: string;
      const ALocalValue: Variant): string;
    class function BuildManyToOneQuery(const ATargetTable, ALocalKey: string;
      const AForeignValue: Variant): string;
    class function BuildManyToManyQuery(const ATargetTable, AMappingTable,
      AForeignKey, ALocalKey: string; const ALocalValue: Variant): string;
  end;

  { TdRelationFactory - Factory for creating related objects }
  TdRelationFactory = class(TdObject)
  public
    class function CreateEntity(AClass: TClass): TObject;
    class function CreateEntityList: TObjectList;
  end;

  { TdRelationValidator - Relation validator }
  TdRelationValidator = class(TdObject)
  public
    class function ValidateRelation(ARelation: TdRelationInfo): Boolean;
    class function ValidateEntity(AEntity: TObject): Boolean;
    class function GetValidationErrors(ARelation: TdRelationInfo): TStringList;
  end;

  { TdAdvancedRelationalEntity - Extended version of the base class }
  TdAdvancedRelationalEntity = class(TdRelationalEntity)
  private
    FCache: TdRelationCache;
    FAutoLoad: Boolean;
  protected
    function GetCacheKey(const ARelationName: string): string; virtual;
    procedure CacheRelation(const ARelationName: string; AValue: TObject); virtual;
    function GetCachedRelation(const ARelationName: string): TObject; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    // Extended relation management methods
    procedure ClearRelationCache(const ARelationName: string = '');
    procedure PreloadRelation(const ARelationName: string);
    procedure SetAutoLoad(AValue: Boolean);

    // Checking relation status
    function IsRelationLoaded(const ARelationName: string): Boolean;
    function GetLoadedRelations: TStringList;

    property AutoLoad: Boolean read FAutoLoad write SetAutoLoad;
  end;

implementation

uses
  Variants, TypInfo;

{ TdRelationCache }

constructor TdRelationCache.Create;
begin
  inherited Create;
  FCache := TStringList.Create;
  FCache.OwnsObjects := True;
  FCache.Sorted := True;
  FCache.Duplicates := dupIgnore;
end;

destructor TdRelationCache.Destroy;
begin
  FCache.Free;
  inherited Destroy;
end;

procedure TdRelationCache.Store(const AKey: string; AValue: TObject);
var
  Index: Integer;
begin
  Index := FCache.IndexOf(AKey);
  if Index >= 0 then
    FCache.Objects[Index] := AValue
  else
    FCache.AddObject(AKey, AValue);
end;

function TdRelationCache.Retrieve(const AKey: string): TObject;
var
  Index: Integer;
begin
  Index := FCache.IndexOf(AKey);
  if Index >= 0 then
    Result := FCache.Objects[Index]
  else
    Result := nil;
end;

procedure TdRelationCache.Remove(const AKey: string);
var
  Index: Integer;
begin
  Index := FCache.IndexOf(AKey);
  if Index >= 0 then
    FCache.Delete(Index);
end;

procedure TdRelationCache.Clear;
begin
  FCache.Clear;
end;

function TdRelationCache.HasKey(const AKey: string): Boolean;
begin
  Result := FCache.IndexOf(AKey) >= 0;
end;

{ TdRelationQueryBuilder }

class function TdRelationQueryBuilder.BuildOneToOneQuery(const ATargetTable,
  AForeignKey: string; const ALocalValue: Variant): string;
begin
  Result := Format('SELECT * FROM %s WHERE %s = %s',
    [ATargetTable, AForeignKey, QuotedStr(VarToStr(ALocalValue))]);
end;

class function TdRelationQueryBuilder.BuildOneToManyQuery(const ATargetTable,
  AForeignKey: string; const ALocalValue: Variant): string;
begin
  Result := Format('SELECT * FROM %s WHERE %s = %s ORDER BY id',
    [ATargetTable, AForeignKey, QuotedStr(VarToStr(ALocalValue))]);
end;

class function TdRelationQueryBuilder.BuildManyToOneQuery(const ATargetTable,
  ALocalKey: string; const AForeignValue: Variant): string;
begin
  Result := Format('SELECT * FROM %s WHERE %s = %s',
    [ATargetTable, ALocalKey, QuotedStr(VarToStr(AForeignValue))]);
end;

class function TdRelationQueryBuilder.BuildManyToManyQuery(const ATargetTable,
  AMappingTable, AForeignKey, ALocalKey: string; const ALocalValue: Variant): string;
begin
  Result := Format(
    'SELECT t.* FROM %s t ' +
    'INNER JOIN %s m ON t.%s = m.%s_target ' +
    'WHERE m.%s_local = %s ' +
    'ORDER BY t.id',
    [ATargetTable, AMappingTable, ALocalKey, AForeignKey,
     ALocalKey, QuotedStr(VarToStr(ALocalValue))]);
end;

{ TdRelationFactory }

class function TdRelationFactory.CreateEntity(AClass: TClass): TObject;
begin
  Result := AClass.Create;
end;

class function TdRelationFactory.CreateEntityList: TObjectList;
begin
  Result := TObjectList.Create(True); // Owns objects
end;

{ TdRelationValidator }

class function TdRelationValidator.ValidateRelation(ARelation: TdRelationInfo): Boolean;
begin
  Result := True;

  // Check required fields
  if ARelation.PropertyName = '' then
    Result := False;

  if ARelation.ForeignKey = '' then
    Result := False;

  if ARelation.TargetClass = nil then
    Result := False;

  // Check relation type specific fields
  if (ARelation.RelationType = rtManyToMany) and (ARelation.MappingTable = '') then
    Result := False;
end;

class function TdRelationValidator.ValidateEntity(AEntity: TObject): Boolean;
begin
  Result := (AEntity <> nil) and Supports(AEntity, IdRelationalEntity);
end;

class function TdRelationValidator.GetValidationErrors(ARelation: TdRelationInfo): TStringList;
begin
  Result := TStringList.Create;

  if ARelation.PropertyName = '' then
    Result.Add('Property name is required');

  if ARelation.ForeignKey = '' then
    Result.Add('Foreign key is required');

  if ARelation.TargetClass = nil then
    Result.Add('Target class is required');

  if (ARelation.RelationType = rtManyToMany) and (ARelation.MappingTable = '') then
    Result.Add('Mapping table is required for Many-to-Many relations');
end;

{ TdAdvancedRelationalEntity }

constructor TdAdvancedRelationalEntity.Create;
begin
  inherited Create;
  FCache := TdRelationCache.Create;
  FAutoLoad := False;
end;

destructor TdAdvancedRelationalEntity.Destroy;
begin
  FCache.Free;
  inherited Destroy;
end;

function TdAdvancedRelationalEntity.GetCacheKey(const ARelationName: string): string;
begin
  Result := Format('%s_%s_%p', [Self.ClassName, ARelationName, Pointer(Self)]);
end;

procedure TdAdvancedRelationalEntity.CacheRelation(const ARelationName: string; AValue: TObject);
begin
  FCache.Store(GetCacheKey(ARelationName), AValue);
end;

function TdAdvancedRelationalEntity.GetCachedRelation(const ARelationName: string): TObject;
begin
  Result := FCache.Retrieve(GetCacheKey(ARelationName));
end;

procedure TdAdvancedRelationalEntity.ClearRelationCache(const ARelationName: string);
var
  I: Integer;
  RelInfo: TdRelationInfo;
begin
  if ARelationName = '' then
    FCache.Clear
  else
    FCache.Remove(GetCacheKey(ARelationName));

  // Update loading status in relation metadata
  if ARelationName = '' then
  begin
    for I := 0 to FRelations.Count - 1 do
      FRelations[I].Loaded := False;
  end
  else
  begin
    RelInfo := FRelations.FindByProperty(ARelationName);
    if Assigned(RelInfo) then
      RelInfo.Loaded := False;
  end;
end;

procedure TdAdvancedRelationalEntity.PreloadRelation(const ARelationName: string);
begin
  // This method will be overridden in specific OPF classes
  // to perform preloading of a specific relation
end;

procedure TdAdvancedRelationalEntity.SetAutoLoad(AValue: Boolean);
begin
  FAutoLoad := AValue;
end;

function TdAdvancedRelationalEntity.IsRelationLoaded(const ARelationName: string): Boolean;
var
  RelInfo: TdRelationInfo;
begin
  RelInfo := FRelations.FindByProperty(ARelationName);
  Result := (RelInfo <> nil) and RelInfo.Loaded;
end;

function TdAdvancedRelationalEntity.GetLoadedRelations: TStringList;
var
  I: Integer;
begin
  Result := TStringList.Create;
  for I := 0 to FRelations.Count - 1 do
    if FRelations[I].Loaded then
      Result.Add(FRelations[I].PropertyName);
end;

end.
