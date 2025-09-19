program RelationsDemo;

{$mode objfpc}{$H+}

uses
  dOpf, dSQLdbBroker, sysutils, sqlite3conn, dOpfRelations, fgl, Classes,
  Contnrs;

type

  TOrder = class;
  TProfile = class;

  // Entities
  { TPerson }
  TPerson = class(TdRelationalEntity)
  private
    FId: Integer;
    FName: string;
    FEmail: string;
  public
    procedure ConfigureRelations({%H-}ARelations: TdRelationList); override;
    // Procedures for convenient access to relation data
    function GetOrders: TObjectList;
    function GetProfile: TProfile;
  published
    property id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Email: string read FEmail write FEmail;
  end;

  TOrderItem = class;

  { TOrder }
  TOrder = class(TdRelationalEntity)
  private
    FId: Int64;
    FPersonId: Int64;
    FAmount: Double;
    FOrderDate: TDateTime;
  public
    procedure ConfigureRelations({%H-}ARelations: TdRelationList); override;

    function GetPerson: TPerson;
    function GetOrderItems: TObjectList;
  published
    property Id: Int64 read FId write FId;
    property Person_Id: Int64 read FPersonId write FPersonId;
    property Amount: Double read FAmount write FAmount;
    property Order_Date: TDateTime read FOrderDate write FOrderDate;
  end;

  { TProfile }
  TProfile = class(TdRelationalEntity)
  private
    FId: Int64;
    FPersonId: Int64;
    FBio: string;
    FAvatar: string;
  public
    procedure ConfigureRelations({%H-}ARelations: TdRelationList); override;

    function GetPerson: TPerson;
  published
    property id: Int64 read FId write FId;
    property Person_Id: Int64 read FPersonId write FPersonId;
    property Bio: string read FBio write FBio;
    property Avatar: string read FAvatar write FAvatar;
  end;

  { TOrderItem }
  TOrderItem = class(TdRelationalEntity)
  private
    FId: Int64;
    FOrderId: Int64;
    FProductName: string;
    FQuantity: Integer;
    FPrice: Double;
  public
    procedure ConfigureRelations({%H-}ARelations: TdRelationList); override;

    function GetOrder: TOrder;
  published
    property Id: Int64 read FId write FId;
    property OrderId: Int64 read FOrderId write FOrderId;
    property ProductName: string read FProductName write FProductName;
    property Quantity: Integer read FQuantity write FQuantity;
    property Price: Double read FPrice write FPrice;
  end;

  // Types of the OPF
  TPersonOpf = specialize TdGRelationalEntityOpf<TdSQLdbConnector, TdSQLdbQuery, TPerson>;
  TOrderOpf = specialize TdGRelationalEntityOpf<TdSQLdbConnector, TdSQLdbQuery, TOrder>;
  TProfileOpf = specialize TdGRelationalEntityOpf<TdSQLdbConnector, TdSQLdbQuery, TProfile>;

var
  _con: TdSQLdbConnector = nil;

// Connect to DB
function GetConnection: TdSQLdbConnector;
begin
  if not Assigned(_con) then
  begin
    _con := TdSQLdbConnector.Create(nil);
    _con.Logger.Active := True;
    _con.Logger.FileName := 'RELATIONS_OUTPUT.LOG';
    _con.Driver := 'sqlite3';
    _con.Database := 'relations_demo.sqlite3';
  end;
  Result := _con;
end;

{ TPerson }

procedure TPerson.ConfigureRelations(ARelations: TdRelationList);
begin
  // The Person has a lot of orders (One-to-Many)
  HasMany('Orders', TOrder, 'orders', 'person_id', 'id');

  // A Person has one profile (One-to-One)
  HasOne('Profile', TProfile, 'profiles', 'person_id', 'id');
end;

function TPerson.GetOrders: TObjectList;
var
  PersonOpf: TPersonOpf;
begin
  PersonOpf := TPersonOpf.Create(GetConnection, 'person');
  try
    Result := PersonOpf.GetRelatedObjectListForEntity(Self, 'Orders');
  finally
    PersonOpf.Free;
  end;
end;

function TPerson.GetProfile: TProfile;
var
  PersonOpf: TPersonOpf;
  RelatedObject: TObject;
begin
  PersonOpf := TPersonOpf.Create(GetConnection, 'person');
  try
    RelatedObject := PersonOpf.GetRelatedObjectForEntity(Self, 'Profile');
    Result := specialize TdGRelatedObjectHelper<TProfile>.SafeCast(RelatedObject);
  finally
    PersonOpf.Free;
  end;
end;

{ TOrder }

procedure TOrder.ConfigureRelations(ARelations: TdRelationList);
begin
  // The Order belongs to a Person (Many-to-One)
  BelongsTo('Person', TPerson, 'person', 'id', 'person_id');

  // The Order has many elements (One-to-Many)
  HasMany('OrderItems', TOrderItem, 'order_items', 'order_id', 'id');
end;

function TOrder.GetPerson: TPerson;
var
  OrderOpf: TOrderOpf;
  RelatedObject: TObject;
begin
  OrderOpf := TOrderOpf.Create(GetConnection, 'orders');
  try
    RelatedObject := OrderOpf.GetRelatedObjectForEntity(Self, 'Person');
    Result := specialize TdGRelatedObjectHelper<TPerson>.SafeCast(RelatedObject);
  finally
    OrderOpf.Free;
  end;
end;

function TOrder.GetOrderItems: TObjectList;
var
  OrderOpf: TOrderOpf;
begin
  OrderOpf := TOrderOpf.Create(GetConnection, 'orders');
  try
    Result := OrderOpf.GetRelatedObjectListForEntity(Self, 'OrderItems');
  finally
    OrderOpf.Free;
  end;
end;

{ TProfile }

procedure TProfile.ConfigureRelations(ARelations: TdRelationList);
begin
  // Profile belongs Person (One-to-One reverse relation)
  BelongsTo('Person', TPerson, 'person', 'id', 'person_id');
end;

function TProfile.GetPerson: TPerson;
var
  ProfileOpf: TProfileOpf;
  RelatedObject: TObject;
begin
  ProfileOpf := TProfileOpf.Create(GetConnection, 'profiles');
  try
    RelatedObject := ProfileOpf.GetRelatedObjectForEntity(Self, 'Person');
    Result := specialize TdGRelatedObjectHelper<TPerson>.SafeCast(RelatedObject);
  finally
    ProfileOpf.Free;
  end;
end;

{ TOrderItem }

procedure TOrderItem.ConfigureRelations(ARelations: TdRelationList);
begin
  // OrderItem belongs Order (Many-to-One)
  BelongsTo('Order', TOrder, 'orders', 'id', 'order_id');
end;

function TOrderItem.GetOrder: TOrder;
var
  OrderItemOpf: specialize TdGRelationalEntityOpf<TdSQLdbConnector, TdSQLdbQuery, TOrderItem>;
  RelatedObject: TObject;
begin
  OrderItemOpf := specialize TdGRelationalEntityOpf<TdSQLdbConnector, TdSQLdbQuery, TOrderItem>.Create(GetConnection, 'order_items');
  try
    RelatedObject := OrderItemOpf.GetRelatedObjectForEntity(Self, 'Order');
    Result := specialize TdGRelatedObjectHelper<TOrder>.SafeCast(RelatedObject);
  finally
    OrderItemOpf.Free;
  end;
end;

// Procedure to create tables
procedure CreateTables;
var
  con: TdSQLdbConnector;
  qry: TdSQLdbQuery;
begin
  con := GetConnection;
  qry := TdSQLdbQuery.Create(con);
  try
    con.Connect;

    // Create tables
    WriteLn('Creating tables...');

    // Table person
    qry.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS person (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  name VARCHAR(100) NOT NULL,' +
      '  email VARCHAR(255) UNIQUE' +
      ')';
    qry.Execute;

    // Table profiles
    qry.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS profiles (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  person_id INTEGER NOT NULL,' +
      '  bio TEXT,' +
      '  avatar VARCHAR(255),' +
      '  FOREIGN KEY (person_id) REFERENCES person(id) ON DELETE CASCADE' +
      ')';
    qry.Execute;

    // Table orders
    qry.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS orders (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  person_id INTEGER NOT NULL,' +
      '  amount DECIMAL(10,2),' +
      '  order_date DATETIME DEFAULT CURRENT_TIMESTAMP,' +
      '  FOREIGN KEY (person_id) REFERENCES person(id) ON DELETE CASCADE' +
      ')';
    qry.Execute;

    // Table order_items
    qry.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS order_items (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  order_id INTEGER NOT NULL,' +
      '  product_name VARCHAR(255) NOT NULL,' +
      '  quantity INTEGER NOT NULL,' +
      '  price DECIMAL(10,2) NOT NULL,' +
      '  FOREIGN KEY (order_id) REFERENCES orders(id) ON DELETE CASCADE' +
      ')';
    qry.Execute;

    qry.Apply;
    WriteLn('Tables created successfully.');

  finally
    qry.Free;
  end;
end;

// Fill test data
procedure FillTestData;
var
  personOpf: TPersonOpf;
  orderOpf: TOrderOpf;
  profileOpf: TProfileOpf;
  personId: Int64;
begin
  WriteLn('Filling test data...');

  // Create person
  personOpf := TPersonOpf.Create(GetConnection, 'person');
  try
    personOpf.Entity.Id := 0;
    personOpf.Entity.Name := 'John Doe';
    personOpf.Entity.Email := 'john.doe@example.com';
    personOpf.Add;
    personOpf.Apply;

    personId := 1; // We assume that this is the first entry

    // Create profile
    profileOpf := TProfileOpf.Create(GetConnection, 'profiles');
    try
      profileOpf.Entity.Person_Id := personId;
      profileOpf.Entity.Bio := 'Software developer and Pascal enthusiast';
      profileOpf.Entity.Avatar := 'avatar1.jpg';
      profileOpf.Add;
      profileOpf.Apply;
    finally
      profileOpf.Free;
    end;

    // Create orders
    orderOpf := TOrderOpf.Create(GetConnection, 'orders');
    try
      // First order
      orderOpf.Entity.Person_Id := personId;
      orderOpf.Entity.Amount := 99.99;
      orderOpf.Entity.Order_Date := Now;
      orderOpf.Add;

      // Second order - creating a new instance
      orderOpf.Entity.Id := 0;
      orderOpf.Entity.Person_Id := personId;
      orderOpf.Entity.Amount := 149.50;
      orderOpf.Entity.Order_Date := Now;
      orderOpf.Add;

      orderOpf.Apply;
    finally
      orderOpf.Free;
    end;
  finally
    personOpf.Free;
  end;

  WriteLn('Test data created successfully.');
end;

// Relation work demo
procedure DemonstrateRelations;
var
  personOpf: TPersonOpf;
  orders: TObjectList;
  profile: TProfile;
  i: Integer;
  aOrder: TOrder;
begin
  WriteLn('');
  WriteLn('=== Demonstrating Relations ===');

  personOpf := TPersonOpf.Create(GetConnection, 'person');
  try
    // Load person
    personOpf.Entity.Id := 1;
    if personOpf.Get then
    begin
      WriteLn('Person: ', personOpf.Entity.Name, ' (', personOpf.Entity.Email, ')');

      // Load profile (One-to-One)
      WriteLn('Loading profile...');
      profile := personOpf.Entity.GetProfile;
      if Assigned(profile) then
      begin
        WriteLn('Profile Bio: ', profile.Bio);
        WriteLn('Profile Avatar: ', profile.Avatar);
      end
      else
        WriteLn('No profile found.');

      // Load orders (One-to-Many)
      WriteLn('Loading orders...');
      orders := personOpf.Entity.GetOrders;
      if Assigned(orders) then
      try
        WriteLn('Orders count: ', orders.Count);
        for i := 0 to orders.Count - 1 do
        begin
          aOrder := orders[i] as TOrder;
          WriteLn('  Order #', aOrder.Id, ': ', FormatFloat('0.00', aOrder.Amount), ' on ',
            DateTimeToStr(aOrder.Order_Date));
        end;
      finally
        orders.Free;
      end;
    end
    else
      WriteLn('Person not found.');

  finally
    personOpf.Free;
  end;
end;

// Reverse relations demo
procedure DemonstrateReverseRelations;
var
  orderOpf: TOrderOpf;
  person: TPerson;
begin
  WriteLn('');
  WriteLn('=== Demonstrating Reverse Relations ===');

  orderOpf := TOrderOpf.Create(GetConnection, 'orders');
  try
    // Load order
    orderOpf.Entity.Id := 1;
    if orderOpf.Get then
    begin
      WriteLn('Order: #', orderOpf.Entity.Id, ' Amount: ', FormatFloat('0.00', orderOpf.Entity.Amount));

      // Get order owner (Many-to-One)
      WriteLn('Loading order owner...');
      person := orderOpf.Entity.GetPerson;
      if Assigned(person) then
      begin
        WriteLn('Order belongs to: ', person.Name, ' (', person.Email, ')');
        // DO NOT release the person.Free; since the object is managed by the cache
      end
      else
        WriteLn('No person found for this order.');
    end
    else
      WriteLn('Order not found.');

  finally
    orderOpf.Free;
  end;
end;

// Lazy loading demo
procedure DemonstrateLazyLoading;
var
  personOpf: TPersonOpf;
begin
  WriteLn('');
  WriteLn('=== Demonstrating Lazy Loading ===');

  personOpf := TPersonOpf.Create(GetConnection, 'person');
  try
    personOpf.Entity.Id := 1;
    if personOpf.Get then
    begin
      WriteLn('Person loaded: ', personOpf.Entity.Name);
      WriteLn('Relations are not loaded yet (lazy loading)');

      // Load all relations at once
      WriteLn('Loading all relations...');
      personOpf.LoadAllRelations;
      WriteLn('All relations loaded.');

      // Now you can access all relation without additional requests
      WriteLn('Accessing cached relations...');
      // ... Here we can use cached data
    end;
  finally
    personOpf.Free;
  end;
end;

// Main program
begin
  WriteLn('dOPF Relations Demo');
  WriteLn('==================');

  try
    // Initialization
    GetConnection;

    // Create tables
    CreateTables;

    // Fill test data
    FillTestData;

    // Relation work demo
    DemonstrateRelations;

    // Reverse relations demo
    DemonstrateReverseRelations;

    // Lazy loading demo
    DemonstrateLazyLoading;

    WriteLn('');
    WriteLn('Demo completed successfully!');

  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.ClassName, ' - ', E.Message);
      ExitCode := 1;
    end;
  end;

  WriteLn('Press Enter to exit...');
  ReadLn;

  // Free
  FreeAndNil(_con);
end.
