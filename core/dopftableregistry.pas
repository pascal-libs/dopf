(*
  Реестр таблиц - позволяет связать класс с именем таблицы
*)

unit dOpfTableRegistry;

{$i dopf.inc}

interface

uses
  dClasses, Classes, SysUtils;

type
  { TdTableRegistry - the register of correspondence of classes and tables }
  TdTableRegistry = class(TdObject)
  private
    FRegistry: TStringList;
    class var FInstance: TdTableRegistry;
  public
    constructor Create;
    destructor Destroy; override;

    class function Instance: TdTableRegistry;
    class procedure RegisterTable(AClass: TClass; const ATableName: string);
    class function GetTableName(AClass: TClass): string;
    class function IsRegistered(AClass: TClass): Boolean;

    procedure RegisterClass(AClass: TClass; const ATableName: string);
    function GetTableForClass(AClass: TClass): string;
    function HasClass(AClass: TClass): Boolean;
  end;

  { A macro for convenient registration of tables }
procedure RegisterEntityTable(AClass: TClass; const ATableName: string);

function GetTableNameForClass(AClass: TClass): string;

implementation

{ TdTableRegistry }

constructor TdTableRegistry.Create;
begin
  inherited Create;
  FRegistry := TStringList.Create;
  FRegistry.Sorted := True;
  FRegistry.Duplicates := dupIgnore;
end;

destructor TdTableRegistry.Destroy;
begin
  FRegistry.Free;
  inherited Destroy;
end;

class function TdTableRegistry.Instance: TdTableRegistry;
begin
  if not Assigned(FInstance) then
    FInstance := TdTableRegistry.Create;
  Result := FInstance;
end;

class procedure TdTableRegistry.RegisterTable(AClass: TClass; const ATableName: string);
begin
  Instance.RegisterClass(AClass, ATableName);
end;

class function TdTableRegistry.GetTableName(AClass: TClass): string;
begin
  Result := Instance.GetTableForClass(AClass);
  if Result = '' then
    Result := LowerCase(AClass.ClassName); // fallback to class name
end;

class function TdTableRegistry.IsRegistered(AClass: TClass): Boolean;
begin
  Result := Instance.HasClass(AClass);
end;

procedure TdTableRegistry.RegisterClass(AClass: TClass; const ATableName: string);
var
  Index: Integer;
begin
  Index := FRegistry.IndexOf(AClass.ClassName);
  if Index >= 0 then
    FRegistry.ValueFromIndex[Index] := ATableName
  else
    FRegistry.Values[AClass.ClassName] := ATableName;
end;

function TdTableRegistry.GetTableForClass(AClass: TClass): string;
var
  Index: Integer;
begin
  Index := FRegistry.IndexOf(AClass.ClassName);
  if Index >= 0 then
    Result := FRegistry.ValueFromIndex[Index]
  else
    Result := '';
end;

function TdTableRegistry.HasClass(AClass: TClass): Boolean;
begin
  Result := FRegistry.IndexOf(AClass.ClassName) >= 0;
end;

{ Global functions for convenience }

procedure RegisterEntityTable(AClass: TClass; const ATableName: string);
begin
  TdTableRegistry.RegisterTable(AClass, ATableName);
end;

function GetTableNameForClass(AClass: TClass): string;
begin
  Result := TdTableRegistry.GetTableName(AClass);
  if Result.IsEmpty then
  begin
    Result:=LowerCase(AClass.ClassName);
    Result := Copy(Result, 2, Length(Result) - 1); // fallback
  end;
end;

initialization

finalization
  TdTableRegistry.FInstance.Free;

end.
