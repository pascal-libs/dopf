unit test_dopf;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, dopf, Classes;

type
  TTestDOpf = class(TTestCase)
  published
    procedure TestConnectionBrokerConnect;
    procedure TestGConnectionCreate;
    procedure TestLoggerActivateAfterFileName;
    procedure TestGQueryNilOwner;
    procedure TestGQueryOwnerConnection;
    procedure TestGQueryInvalidOwner;
  end;

implementation

uses
  SysUtils
  ;

type
  TMyConnection = class(specialize TdGConnection<TdConnectionBroker, TdLogger>);

  { TMyConnBroker }

  TMyConnBroker = class(TdConnectionBroker)
  protected
    function GetConnection: TObject; override;
  end;

  { TMyQueryBroker }

  TMyQueryBroker = class(TdQueryBroker)
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

function TMyQueryBroker.GetConnection: TObject;
begin
  Result := GBrokerConnection;
end;

procedure TMyQueryBroker.SetConnection(AValue: TObject);
begin
  GBrokerConnection := AValue;
  GBrokerConnectionSet := True;
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

initialization
  RegisterTest(TTestDOpf);
end.
