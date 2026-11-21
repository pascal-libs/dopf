unit test_dopf;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, dopf, Classes;

type
  TTestDOpf = class(TTestCase)
  public
    procedure NoopProc;
    procedure RaisingProc;
  published
    procedure TestConnectionBrokerConnect;
    procedure TestGConnectionCreate;
    procedure TestLoggerActivateAfterFileName;
    procedure TestGQueryNilOwner;
    procedure TestGQueryOwnerConnection;
    procedure TestGQueryInvalidOwner;
    procedure TestApplyWithoutTransactionIgnored;
    procedure TestApplyStrictWithoutTransactionRaises;
    procedure TestApplyWithTransactionCallsBroker;
    procedure TestExecuteStrictRequiresExplicitTransaction;
    procedure TestExplicitTransactionLifecycle;
    procedure TestStartTransactionAdoptsAmbient;
    procedure TestRunInTransactionCommits;
    procedure TestRunInTransactionRollsBackOnException;
    procedure TestRunInTransactionJoinsActiveTransaction;
    procedure TestRunInTransactionJoinRollsBackOnException;
    procedure TestRunInTransactionRequiresProc;
  end;

implementation

uses
  SysUtils
  ;

type
  TMyConnection = class(specialize TdGConnection<TdConnectionBroker, TdLogger>);

  { TMyConnBroker }

  TMyConnBroker = class(TdConnectionBroker)
  public
    InTx: Boolean;
    StartCount: Integer;
    CommitCount: Integer;
    RollbackCount: Integer;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
    function InTransaction: Boolean; override;
  protected
    function GetConnection: TObject; override;
  end;

  { TMyQueryBroker }

  TMyQueryBroker = class(TdQueryBroker)
  public
    ApplyCount: Integer;
    procedure Apply; override;
  protected
    function GetConnection: TObject; override;
    procedure SetConnection(AValue: TObject); override;
  end;

  TMyQueryConnection = class(specialize TdGConnection<TMyConnBroker, TdLogger>);
  TMyQuery = class(specialize TdGQuery<TMyQueryBroker, TMyQueryConnection>);

var
  GBrokerConnection: TObject;
  GBrokerConnectionSet: Boolean;

function TMyConnBroker.GetConnection: TObject;
begin
  Result := Self;
end;

procedure TMyConnBroker.StartTransaction;
begin
  Inc(StartCount);
  InTx := True;
end;

procedure TMyConnBroker.Commit;
begin
  Inc(CommitCount);
  InTx := False;
end;

procedure TMyConnBroker.Rollback;
begin
  Inc(RollbackCount);
  InTx := False;
end;

function TMyConnBroker.InTransaction: Boolean;
begin
  Result := InTx;
end;

function TMyQueryBroker.GetConnection: TObject;
begin
  Result := GBrokerConnection;
end;

procedure TMyQueryBroker.SetConnection(AValue: TObject);
begin
  GBrokerConnection := AValue;
  GBrokerConnectionSet := True;
end;

procedure TMyQueryBroker.Apply;
begin
  Inc(ApplyCount);
end;

procedure TTestDOpf.TestConnectionBrokerConnect;
var
  B: TdConnectionBroker;
begin
  B := TdConnectionBroker.Create;
  try
    try
      B.Connect;
      Fail('Expected exception');
    except
      on E: EdNotImplemented do ;
    end;
  finally
    B.Free;
  end;
end;

procedure TTestDOpf.TestGConnectionCreate;
var
  C: TMyConnection;
begin
  C := TMyConnection.Create(nil);
  try
    AssertTrue(Assigned(C.Broker));
    AssertTrue(Assigned(C.Logger));
  finally
    C.Free;
  end;
end;

procedure TTestDOpf.TestLoggerActivateAfterFileName;
const
  LogName = 'dopf_test_logger.log';
var
  L: TdLogger;
  F, Path: string;
  SL: TStringList;
begin
  // The logger appends the current date to the file name: name_YYYYMMDD.log.
  F := ChangeFileExt(LogName, '_' + FormatDateTime('yyyymmdd', Date) +
    ExtractFileExt(LogName));
  Path := ExtractFilePath(ParamStr(0)) + F;
  DeleteFile(Path);
  L := TdLogger.Create('');
  try
    L.Directory := ExtractFilePath(ParamStr(0));
    L.FileName := LogName;
    L.Active := True;
    L.Log(ltCustom, 'logger test message');
  finally
    L.Free;
  end;
  SL := TStringList.Create;
  try
    SL.LoadFromFile(Path);
    AssertTrue(Pos('logger test message', SL.Text) > 0);
  finally
    SL.Free;
    DeleteFile(Path);
  end;
end;

procedure TTestDOpf.TestGQueryNilOwner;
var
  Q: TMyQuery;
begin
  GBrokerConnection := nil;
  GBrokerConnectionSet := False;
  Q := TMyQuery.Create(nil);
  try
    AssertTrue(Assigned(Q.Broker));
    AssertTrue(Q.Connection = nil);
    AssertFalse(GBrokerConnectionSet);
  finally
    Q.Free;
  end;
end;

procedure TTestDOpf.TestGQueryOwnerConnection;
var
  C: TMyQueryConnection;
  Q: TMyQuery;
begin
  GBrokerConnection := nil;
  GBrokerConnectionSet := False;
  C := TMyQueryConnection.Create(nil);
  try
    Q := TMyQuery.Create(C);
    try
      AssertTrue(Q.Connection = C);
      AssertTrue(GBrokerConnectionSet);
      AssertTrue(GBrokerConnection = C.Broker);
    finally
      Q.Free;
    end;
  finally
    C.Free;
  end;
end;

procedure TTestDOpf.TestGQueryInvalidOwner;
var
  O: TComponent;
begin
  GBrokerConnection := nil;
  GBrokerConnectionSet := False;
  O := TComponent.Create(nil);
  try
    try
      TMyQuery.Create(O);
      Fail('Expected exception');
    except
      on E: EdQuery do
        AssertTrue(Pos('Invalid owner class', E.Message) > 0);
    end;
  finally
    O.Free;
  end;
end;

procedure TTestDOpf.NoopProc;
begin
end;

procedure TTestDOpf.RaisingProc;
begin
  raise Exception.Create('boom');
end;

procedure TTestDOpf.TestApplyWithoutTransactionIgnored;
var
  C: TMyQueryConnection;
  Q: TMyQuery;
begin
  GBrokerConnection := nil;
  GBrokerConnectionSet := False;
  C := TMyQueryConnection.Create(nil);
  try
    Q := TMyQuery.Create(C);
    try
      AssertFalse(C.InTransaction);
      Q.Apply; // compatibility mode: ignored, no exception
      AssertTrue(Q.Broker.ApplyCount = 0);
    finally
      Q.Free;
    end;
  finally
    C.Free;
  end;
end;

procedure TTestDOpf.TestApplyStrictWithoutTransactionRaises;
var
  C: TMyQueryConnection;
  Q: TMyQuery;
begin
  GBrokerConnection := nil;
  GBrokerConnectionSet := False;
  C := TMyQueryConnection.Create(nil);
  try
    C.StrictTransactions := True;
    Q := TMyQuery.Create(C);
    try
      try
        Q.Apply;
        Fail('Expected exception');
      except
        on E: EdQuery do
          AssertTrue(Pos('requires an active transaction', E.Message) > 0);
      end;
      AssertTrue(Q.Broker.ApplyCount = 0);
    finally
      Q.Free;
    end;
  finally
    C.Free;
  end;
end;

procedure TTestDOpf.TestApplyWithTransactionCallsBroker;
var
  C: TMyQueryConnection;
  Q: TMyQuery;
begin
  GBrokerConnection := nil;
  GBrokerConnectionSet := False;
  C := TMyQueryConnection.Create(nil);
  try
    C.Broker.InTx := True;
    Q := TMyQuery.Create(C);
    try
      Q.Apply;
      AssertTrue(Q.Broker.ApplyCount = 1);
    finally
      Q.Free;
    end;
  finally
    C.Free;
  end;
end;

procedure TTestDOpf.TestRunInTransactionCommits;
var
  C: TMyQueryConnection;
begin
  C := TMyQueryConnection.Create(nil);
  try
    C.RunInTransaction(@NoopProc);
    AssertTrue(C.Broker.StartCount = 1);
    AssertTrue(C.Broker.CommitCount = 1);
    AssertTrue(C.Broker.RollbackCount = 0);
    AssertFalse(C.InTransaction);
  finally
    C.Free;
  end;
end;

procedure TTestDOpf.TestRunInTransactionRollsBackOnException;
var
  C: TMyQueryConnection;
begin
  C := TMyQueryConnection.Create(nil);
  try
    try
      C.RunInTransaction(@RaisingProc);
      Fail('Expected exception');
    except
      on E: Exception do
        AssertTrue(E.Message = 'boom');
    end;
    AssertTrue(C.Broker.StartCount = 1);
    AssertTrue(C.Broker.RollbackCount = 1);
    AssertTrue(C.Broker.CommitCount = 0);
    AssertFalse(C.InTransaction);
  finally
    C.Free;
  end;
end;

procedure TTestDOpf.TestExecuteStrictRequiresExplicitTransaction;
var
  C: TMyQueryConnection;
  Q: TMyQuery;
begin
  GBrokerConnection := nil;
  GBrokerConnectionSet := False;
  C := TMyQueryConnection.Create(nil);
  try
    C.StrictTransactions := True;
    Q := TMyQuery.Create(C);
    try
      try
        Q.Execute;
        Fail('Expected exception');
      except
        on E: EdQuery do
          AssertTrue(Pos('explicit transaction', E.Message) > 0);
      end;
    finally
      Q.Free;
    end;
  finally
    C.Free;
  end;
end;

procedure TTestDOpf.TestExplicitTransactionLifecycle;
var
  C: TMyQueryConnection;
begin
  C := TMyQueryConnection.Create(nil);
  try
    AssertFalse(C.ExplicitTransaction);
    C.StartTransaction;
    AssertTrue(C.ExplicitTransaction);
    // the transaction may end elsewhere (e.g. Query.Apply): the flag follows
    C.Broker.InTx := False;
    AssertFalse(C.ExplicitTransaction);
    C.StartTransaction;
    AssertTrue(C.ExplicitTransaction);
    C.Commit;
    AssertFalse(C.ExplicitTransaction);
  finally
    C.Free;
  end;
end;

procedure TTestDOpf.TestStartTransactionAdoptsAmbient;
var
  C: TMyQueryConnection;
begin
  C := TMyQueryConnection.Create(nil);
  try
    C.Broker.InTx := True; // ambient transaction (auto-started by SQLdb)
    C.StartTransaction;    // adopts it instead of failing
    AssertTrue(C.Broker.StartCount = 0); // no second Begin
    AssertTrue(C.ExplicitTransaction);
    // a second explicit start is a clear error
    try
      C.StartTransaction;
      Fail('Expected exception');
    except
      on E: EdConnection do
        AssertTrue(Pos('already active', E.Message) > 0);
    end;
    C.Rollback;
    AssertFalse(C.ExplicitTransaction);
  finally
    C.Free;
  end;
end;

procedure TTestDOpf.TestRunInTransactionJoinsActiveTransaction;
var
  C: TMyQueryConnection;
begin
  C := TMyQueryConnection.Create(nil);
  try
    C.Broker.InTx := True; // ambient transaction (auto-started by SQLdb)
    C.RunInTransaction(@NoopProc);
    AssertTrue(C.Broker.StartCount = 0);  // did not start another one
    AssertTrue(C.Broker.CommitCount = 0); // the owner commits
    AssertTrue(C.Broker.RollbackCount = 0);
    AssertTrue(C.InTransaction); // still open
  finally
    C.Free;
  end;
end;

procedure TTestDOpf.TestRunInTransactionJoinRollsBackOnException;
var
  C: TMyQueryConnection;
begin
  C := TMyQueryConnection.Create(nil);
  try
    C.Broker.InTx := True;
    try
      C.RunInTransaction(@RaisingProc);
      Fail('Expected exception');
    except
      on E: Exception do
        AssertTrue(E.Message = 'boom');
    end;
    AssertTrue(C.Broker.RollbackCount = 1); // the whole transaction
    AssertTrue(C.Broker.CommitCount = 0);
    AssertFalse(C.InTransaction);
  finally
    C.Free;
  end;
end;

procedure TTestDOpf.TestRunInTransactionRequiresProc;
var
  C: TMyQueryConnection;
begin
  C := TMyQueryConnection.Create(nil);
  try
    try
      C.RunInTransaction(nil);
      Fail('Expected exception');
    except
      on E: EdConnection do
        AssertTrue(Pos('not assigned', E.Message) > 0);
    end;
    AssertTrue(C.Broker.StartCount = 0);
  finally
    C.Free;
  end;
end;

initialization
  RegisterTest(TTestDOpf);
end.
