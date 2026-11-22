unit test_transactions;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, dopf, dSQLdbBroker, sqlite3conn, sqldb, Classes;

type
  TPerson = class(TObject)
  private
    FId: Int64;
    FName: string;
  published
    property Id: Int64 read FId write FId;
    property Name: string read FName write FName;
  end;

  TPersonOpf = specialize TdGSQLdbEntityOpf<TPerson>;

  TTestTransactions = class(TTestCase)
  private
    FCon: TdSQLdbConnector;
    FOpf: TPersonOpf;
    procedure AssertRowCount(AExpected: Integer);
  public
    procedure AddPersonProc;
    procedure AddPersonAndRaiseProc;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestApplyAfterAddCommitsAutoStartedTransaction;
    procedure TestApplyWithoutTransactionIgnored;
    procedure TestApplyStrictWithoutTransactionRaises;
    procedure TestApplyStrictRequiresExplicitTransaction;
    procedure TestStrictWriteRequiresExplicitTransaction;
    procedure TestStrictPostWithAutoApplyRequiresExplicit;
    procedure TestStrictOpenWithWriteStatementRequiresExplicit;
    procedure TestStrictRefreshWithPendingEditRequiresExplicit;
    procedure TestStrictRunInTransactionAfterRead;
    procedure TestExplicitTransactionApplyCommits;
    procedure TestRunInTransactionCommits;
    procedure TestRunInTransactionRollsBack;
    procedure TestRunInTransactionJoinsAmbientTransaction;
    procedure TestRunInTransactionJoinRollsBackAmbient;
  end;

implementation

uses
  SysUtils
  ;

procedure TTestTransactions.SetUp;
var
  Q: TdSQLdbQuery;
begin
  FCon := TdSQLdbConnector.Create(nil);
  FCon.Driver := 'sqlite3';
  FCon.Database := ':memory:';
  FCon.Connect;
  Q := TdSQLdbQuery.Create(FCon);
  try
    Q.SQL.Text :=
      'create table person (id integer primary key autoincrement, name varchar(100))';
    Q.Execute;
  finally
    Q.Free;
  end;
  // SQLdb auto-starts the transaction on Execute; close it so that every
  // test starts with no active transaction.
  if FCon.InTransaction then
    FCon.Commit;
  FOpf := TPersonOpf.Create(FCon, 'person');
end;

procedure TTestTransactions.TearDown;
begin
  FOpf.Free;
  FCon.Disconnect;
  FCon.Free;
end;

procedure TTestTransactions.AddPersonProc;
begin
  FOpf.Entity.Name := 'Committed';
  FOpf.Add;
end;

procedure TTestTransactions.AddPersonAndRaiseProc;
begin
  FOpf.Entity.Name := 'RolledBack';
  FOpf.Add;
  raise Exception.Create('boom');
end;

procedure TTestTransactions.AssertRowCount(AExpected: Integer);
var
  pers: TPersonOpf.TEntities;
begin
  pers := TPersonOpf.TEntities.Create;
  try
    FOpf.List(pers);
    AssertEquals(AExpected, pers.Count);
  finally
    pers.Free;
  end;
end;

procedure TTestTransactions.TestApplyAfterAddCommitsAutoStartedTransaction;
begin
  // SQLdb starts the shared transaction implicitly on the first statement.
  FOpf.Entity.Name := 'AutoCommit';
  FOpf.Add;
  AssertTrue(FCon.InTransaction);
  FOpf.Apply;
  AssertFalse(FCon.InTransaction);
  AssertRowCount(1);
end;

procedure TTestTransactions.TestApplyWithoutTransactionIgnored;
begin
  FOpf.Entity.Name := 'AutoCommit';
  FOpf.Add;
  FCon.Commit; // leave no active transaction
  AssertFalse(FCon.InTransaction);
  FOpf.Apply; // compatibility mode: ignored without exception
  AssertRowCount(1);
end;

procedure TTestTransactions.TestApplyStrictWithoutTransactionRaises;
begin
  FCon.StrictTransactions := True;
  try
    try
      FOpf.Apply;
      Fail('Expected exception');
    except
      on E: EdQuery do
        AssertTrue(Pos('requires an active transaction', E.Message) > 0);
    end;
  finally
    FCon.StrictTransactions := False;
  end;
  AssertRowCount(0);
end;

procedure TTestTransactions.TestApplyStrictRequiresExplicitTransaction;
var
  pers: TPersonOpf.TEntities;
begin
  FCon.StrictTransactions := True;
  try
    pers := TPersonOpf.TEntities.Create;
    try
      FOpf.List(pers); // a read auto-starts the implicit transaction
    finally
      pers.Free;
    end;
    AssertTrue(FCon.InTransaction);
    AssertFalse(FCon.ExplicitTransaction);
    try
      FOpf.Apply; // may not commit an implicit transaction in strict mode
      Fail('Expected exception');
    except
      on E: EdQuery do
        AssertTrue(Pos('explicit transaction', E.Message) > 0);
    end;
    FCon.StartTransaction; // adopts the implicit transaction
    FOpf.Apply;
    AssertFalse(FCon.InTransaction);
  finally
    FCon.StrictTransactions := False;
  end;
end;

procedure TTestTransactions.TestStrictWriteRequiresExplicitTransaction;
begin
  FCon.StrictTransactions := True;
  try
    try
      FOpf.Entity.Name := 'Blocked';
      FOpf.Add;
      Fail('Expected exception');
    except
      on E: EdQuery do
        AssertTrue(Pos('explicit transaction', E.Message) > 0);
    end;
    AssertRowCount(0);
    // with an explicit transaction the same write works
    FCon.StartTransaction;
    FOpf.Entity.Name := 'Allowed';
    FOpf.Add;
    FOpf.Apply;
    AssertRowCount(1);
  finally
    FCon.StrictTransactions := False;
  end;
end;

procedure TTestTransactions.TestStrictPostWithAutoApplyRequiresExplicit;
var
  Qy: TdSQLdbQuery;
begin
  Qy := TdSQLdbQuery.Create(FCon);
  try
    (Qy.DataSet as TSQLQuery).Options := [sqoAutoApplyUpdates];
    Qy.SQL.Text := 'select * from person';
    Qy.Open;
    FCon.StrictTransactions := True;
    try
      Qy.Append;
      Qy.Field('name').AsString := 'Blocked';
      try
        Qy.Post; // with sqoAutoApplyUpdates Post itself writes
        Fail('Expected exception');
      except
        on E: EdQuery do
          AssertTrue(Pos('explicit transaction', E.Message) > 0);
      end;
      Qy.Cancel;
    finally
      FCon.StrictTransactions := False;
    end;
  finally
    Qy.Free;
  end;
  AssertRowCount(0);
end;

procedure TTestTransactions.TestStrictOpenWithWriteStatementRequiresExplicit;
var
  Qy: TdSQLdbQuery;
begin
  FCon.StrictTransactions := True;
  try
    Qy := TdSQLdbQuery.Create(FCon);
    try
      Qy.SQL.Text := 'select * from person';
      Qy.Open; // plain reads stay allowed
      Qy.Close;
      Qy.SQL.Text := 'insert into person (name) values (''Blocked'')';
      try
        Qy.Open;
        Fail('Expected exception');
      except
        on E: EdQuery do
          AssertTrue(Pos('write statement', E.Message) > 0);
      end;
      try
        Qy.Active := True; // same path via the property
        Fail('Expected exception');
      except
        on E: EdQuery do
          AssertTrue(Pos('write statement', E.Message) > 0);
      end;
      Qy.SQL.Text := 'insert' + LineEnding + 'into person (name) ' +
        'values (''Blocked'')'; // whitespace between the verb and the rest
      try
        Qy.Open;
        Fail('Expected exception');
      except
        on E: EdQuery do
          AssertTrue(Pos('write statement', E.Message) > 0);
      end;
    finally
      Qy.Free;
    end;
  finally
    FCon.StrictTransactions := False;
  end;
  AssertRowCount(0);
end;

procedure TTestTransactions.TestStrictRefreshWithPendingEditRequiresExplicit;
var
  Qy: TdSQLdbQuery;
begin
  FOpf.Entity.Name := 'seed';
  FOpf.Add;
  FCon.Commit; // leave no transaction
  Qy := TdSQLdbQuery.Create(FCon);
  try
    (Qy.DataSet as TSQLQuery).Options := [sqoAutoApplyUpdates];
    Qy.SQL.Text := 'select * from person';
    FCon.StrictTransactions := True;
    try
      Qy.Open;
      Qy.Refresh; // no pending edits: a plain re-read stays allowed
      Qy.Edit;
      Qy.Field('name').AsString := 'Renamed';
      try
        Qy.Refresh; // would post (and write) the pending edit
        Fail('Expected exception');
      except
        on E: EdQuery do
          AssertTrue(Pos('explicit transaction', E.Message) > 0);
      end;
      Qy.Cancel;
      FCon.StartTransaction; // explicit unit (adopts the read transaction)
      Qy.Edit;
      Qy.Field('name').AsString := 'Renamed';
      Qy.Refresh; // allowed inside the explicit transaction
      Qy.Close;
      FOpf.Apply;
    finally
      FCon.StrictTransactions := False;
    end;
  finally
    Qy.Free;
  end;
end;

procedure TTestTransactions.TestStrictRunInTransactionAfterRead;
var
  pers: TPersonOpf.TEntities;
begin
  FCon.StrictTransactions := True;
  try
    pers := TPersonOpf.TEntities.Create;
    try
      FOpf.List(pers); // a read auto-starts the implicit transaction
    finally
      pers.Free;
    end;
    // joins the ambient transaction and takes responsibility for it
    FCon.RunInTransaction(@AddPersonProc);
    AssertTrue(FCon.InTransaction);
    AssertTrue(FCon.ExplicitTransaction);
    FOpf.Apply;
    AssertRowCount(1);
  finally
    FCon.StrictTransactions := False;
  end;
end;

procedure TTestTransactions.TestExplicitTransactionApplyCommits;
begin
  FCon.StartTransaction;
  try
    FOpf.Entity.Name := 'Explicit';
    FOpf.Add;
    AssertTrue(FCon.InTransaction);
    FOpf.Apply;
  except
    FCon.Rollback;
    raise;
  end;
  AssertFalse(FCon.InTransaction);
  AssertRowCount(1);
end;

procedure TTestTransactions.TestRunInTransactionCommits;
begin
  FCon.RunInTransaction(@AddPersonProc);
  AssertRowCount(1);
end;

procedure TTestTransactions.TestRunInTransactionRollsBack;
begin
  try
    FCon.RunInTransaction(@AddPersonAndRaiseProc);
    Fail('Expected exception');
  except
    on E: Exception do
      AssertTrue(E.Message = 'boom');
  end;
  AssertRowCount(0);
end;

procedure TTestTransactions.TestRunInTransactionJoinsAmbientTransaction;
begin
  FOpf.Entity.Name := 'ambient';
  FOpf.Add; // auto-starts the shared transaction
  FCon.RunInTransaction(@AddPersonProc); // joins it
  AssertTrue(FCon.InTransaction); // still open for the owner
  FOpf.Apply; // commits both writes
  AssertRowCount(2);
end;

procedure TTestTransactions.TestRunInTransactionJoinRollsBackAmbient;
begin
  FOpf.Entity.Name := 'ambient';
  FOpf.Add; // auto-starts the shared transaction
  try
    FCon.RunInTransaction(@AddPersonAndRaiseProc);
    Fail('Expected exception');
  except
    on E: Exception do
      AssertTrue(E.Message = 'boom');
  end;
  AssertFalse(FCon.InTransaction);
  AssertRowCount(0); // the ambient write was rolled back too
end;

initialization
  RegisterTest(TTestTransactions);
end.
