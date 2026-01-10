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
  end;

implementation

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

initialization
  RegisterTest(TTestDOpf);
end.
