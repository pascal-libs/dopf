unit test_transactions;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, dopf, dSQLdbBroker, sqlite3conn, Classes;

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
    procedure TestExplicitTransactionApplyCommits;
    procedure TestRunInTransactionCommits;
    procedure TestRunInTransactionRollsBack;
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
  FOpf.Entity.Name := 'AutoCommit';
  FOpf.Add;
  FCon.Commit; // leave no active transaction
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
  // Add already executed immediately; Apply only refused to commit nothing.
  AssertRowCount(1);
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

initialization
  RegisterTest(TTestTransactions);
end.
