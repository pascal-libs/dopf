# dOPF
## General description
Sleek, intuitive, and fast object persistence (OPF / ORM) for faster and easier database development.
It is designed to simplify working with databases by allowing developers to interact with data at the object level, which makes the code cleaner and more understandable.

## Main characteristics
1. Object-relational mapping: dOPF allows developers to work with objects that are automatically mapped to database tables, simplifying the process of data manipulation.
2. Support for various DBMS: The library supports many database management systems, including MySQL, PostgreSQL, SQLite, (and any others that supports in `sqldb` FPC packages), 
making it a universal tool for application development.
3. Flexibility and extensibility: dOPF provides options for customizing the behavior of the ORM, allowing developers to adapt it to their needs. 
Many classes including various generic classes.
4. It has maximum compatibility with native classes due to the use of TSQLQuery and corresponding database connections.

## IDE
Lazarus IDE / Freepascal

## Simple example

### `TdGSQLdbOpf`
This is a generic ORM (Object-Relational Mapping) operator that simplifies database operations.
By specializing it with `TPerson`, it manages CRUD operations for the Person table directly, reducing boilerplate SQL code.

### Apply:
Apply flushes the query's cached updates (if any) and commits the active transaction.
The statements themselves run immediately when `Add`, `Modify`, `Remove` or `Execute` is
called — nothing is buffered until `Apply`. If no transaction is active, `Apply` logs a
warning and does nothing, unless `StrictTransactions` is enabled (see Transactions).

### Add, Modify and Remove:
Add: Adds a new record to the database.
Modify: Updates an existing record based on the entity's ID.
Remove: Deletes a record identified by the entity's ID.

### Entities:
Individual records are represented as instances of `TPerson`.
Multiple records can be stored in a collection (Topf.TEntities), which is iterated using `for..in`.

```pascal
program demo1;

{$mode objfpc}{$H+}

uses
  dOpf, dSQLdbBroker, sqlite3conn, sysutils;

type
  TPerson = class(TObject)
  private
    FId: Int64;
    FName: string;
  published
    property Id: Int64 read FId write FId;
    property Name: string read FName write FName;
  end;

  Topf = specialize TdGSQLdbEntityOpf<TPerson>;

var
  _con: TdSQLdbConnector = nil;

function con: TdSQLdbConnector;
begin
  if not Assigned(_con) then
  begin
    _con := TdSQLdbConnector.Create(nil);
    _con.Logger.Active := True;
    _con.Logger.FileName := 'OUTPUT.LOG';
    _con.Driver := 'sqlite3';
    _con.Database := '../../data.sqlite3';
  end;
  Result := _con;
end;

var
  i: TPerson;
  pers: Topf.TEntities;
  opf: Topf;
begin
  opf := Topf.Create(con, 'person');
  pers := Topf.TEntities.Create;
  try
    con.StartTransaction;
    try
      WriteLn('Add Anonymous');
      opf.Entity.Id := 1000;
      opf.Entity.Name := 'Anonymous';
      opf.Add(False);
      WriteLn('Done.');

      WriteLn('Modify name of Waldir to Waldir Paim');
      opf.Entity.Id := 1001;
      opf.Entity.Name := 'Waldir Paim';
      opf.Modify;
      WriteLn('Done.');

      WriteLn('Remove Anonymous');
      opf.Entity.Id := 1000;
      opf.Remove;
      WriteLn('Done.');

      opf.Apply; // commits the transaction
    except
      con.Rollback;
      raise;
    end;

    WriteLn('Get Waldir Paim');
    opf.Entity.Id := 1001;
    opf.Get;
    WriteLn(opf.Entity.Id, ', ', opf.Entity.Name);
    WriteLn('Done.');
    
    WriteLn('Search for names containing "a" (order by id DESC)');
    opf.Entity.Name := '%a%';
    opf.Search(pers, nil,
      'select * from person where name like (:name) order by id desc');
    for i in pers do
      WriteLn(i.Id, ', ', i.Name);
    pers.Clear;
    WriteLn('Done.');
  finally
    pers.Free;
    opf.Free;
  end;

  ReadLn;

  FreeAndNil(_con);

end.
 
```

## Transactions

dOPF never opens or commits a transaction on its own: the caller owns the
transaction, and all queries of a connector share a single one.

- `Add`, `Modify`, `Remove` and `Query.Execute` run their SQL immediately.
  With FPC SQLdb the shared transaction is started implicitly by the first
  statement and stays open until you commit or roll it back.
- `Apply` flushes the query's cached updates (if any) and commits that active
  transaction.
- The query-side transaction methods (`Apply`, `ApplyRetaining`, `Undo`,
  `UndoRetaining`, `Commit`, `Rollback`) require an active transaction.

An explicit unit of work:

```pascal
con.StartTransaction;
try
  opf.Add;
  opf.Modify;
  opf.Apply; // commits
except
  con.Rollback;
  raise;
end;
```

A scoped helper without the try/except boilerplate:

```pascal
type
  TOrdersForm = class(TForm)
    procedure SaveAll;
  end;

procedure TOrdersForm.SaveAll;
begin
  opf.Add;
  opf.Modify;
end;

con.RunInTransaction(@SaveAll); // StartTransaction, SaveAll, Commit / Rollback
```

`RunInTransaction` raises `EdConnection` when the callback is missing or when
a transaction is already active (nesting is not supported).

### StrictTransactions

By default the query-side transaction methods are ignored with a logged
warning when no transaction is active (compatibility mode). Set

```pascal
con.StrictTransactions := True;
```

to turn them into an `EdQuery` error instead. This is the intended contract;
the default will change to `True` in dOPF 4.0.