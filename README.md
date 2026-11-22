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
      WriteLn('Empty table');
      opf.Empty;
      WriteLn('Done.');

      WriteLn('Add Waldir');
      opf.Entity.Id := 1001;
      opf.Entity.Name := 'Waldir';
      opf.Add(False);
      WriteLn('Done.');

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
    if opf.Get then
      WriteLn(opf.Entity.Id, ', ', opf.Entity.Name)
    else
      WriteLn('Not found.');
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

All queries of a connector share a single transaction, and with FPC SQLdb it
is started implicitly by the first statement: read a row and the transaction
is already open. `Add`, `Modify`, `Remove` and `Query.Execute` run their SQL
immediately inside it, and `Apply` flushes the query's cached updates (if
any) and commits. In effect that is an implicit unit of work ending at
`Apply`/`Commit` — with two traps: a forgotten commit rolls the writes back
when the connection closes, and `StartTransaction` after any statement finds
the transaction already active.

dOPF makes the model explicit instead:

- `StartTransaction` adopts an implicitly started transaction (only a
  transaction you started yourself makes it raise `EdConnection`), so an
  explicit unit of work can begin anywhere in the session.
- The query-side transaction methods (`Apply`, `ApplyRetaining`, `Undo`,
  `UndoRetaining`, `Commit`, `Rollback`) require an active transaction; with
  none active they are ignored with a logged warning (compatibility mode).
- `Connection.CommitIfActive` closes the implicit transaction a read may
  have started; it is a no-op when none is active.

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

con.RunInTransaction(@SaveAll); // atomic: Commit on success, Rollback on error
```

If a transaction is already active, `RunInTransaction` joins it: the callback
runs inside it, an error rolls back the whole transaction, and on success the
transaction is left open for its owner to commit. A missing callback raises
`EdConnection`.

### StrictTransactions

By default dOPF keeps the historical behavior: writes run inside the implicit
transaction whether you opened one or not. Set

```pascal
con.StrictTransactions := True;
```

to require an explicit transaction for every write: `Add`, `Modify`,
`Remove`, `Query.Execute`, the cached-update flushes `Query.Apply`,
`Query.ApplyRetaining` and `Query.ApplyUpdates`, and the dataset writes
`Query.Post` / `Query.Delete` (they write directly when SQLdb's
`sqoAutoApplyUpdates` is enabled) raise `EdQuery` until you call
`StartTransaction` or `RunInTransaction` (a commit without an active
transaction becomes an `EdQuery` error as well). `Query.Open` and
`Active := True` reject statements whose leading keyword is a write verb
(PostgreSQL `insert ... returning` included); the SQL text is not parsed,
so a CTE-wrapped write is not detected — route such statements through
`Query.Execute`. This is the intended contract; the default will change
to `True` in dOPF 4.0.