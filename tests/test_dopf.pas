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
  end;

implementation

uses
  SysUtils
  ;

type
  TMyConnection = class(specialize TdGConnection<TdConnectionBroker, TdLogger>);

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

initialization
  RegisterTest(TTestDOpf);
end.
