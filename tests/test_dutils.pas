unit test_dutils;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, dutils;

type
  TTestDUtils = class(TTestCase)
  published
    procedure TestParameterizeSQLNilParams;
    procedure TestGetFieldsNilObject;
    procedure TestSetFieldsNilObject;
    procedure TestGetParamsNilObject;
    procedure TestSetParamsNilObject;
  end;

implementation

procedure TTestDUtils.TestParameterizeSQLNilParams;
var
  S: string;
begin
  S := 'select * from t where id=:id';
  try
    dParameterizeSQL(S, nil);
    Fail('Expected exception');
  except
    on E: EdException do ;
  end;
end;

procedure TTestDUtils.TestGetFieldsNilObject;
begin
  try
    dGetFields(nil, nil);
    Fail('Expected exception');
  except
    on E: EdException do ;
  end;
end;

procedure TTestDUtils.TestSetFieldsNilObject;
begin
  try
    dSetFields(nil, nil);
    Fail('Expected exception');
  except
    on E: EdException do ;
  end;
end;

procedure TTestDUtils.TestGetParamsNilObject;
begin
  try
    dGetParams(nil, nil);
    Fail('Expected exception');
  except
    on E: EdException do ;
  end;
end;

procedure TTestDUtils.TestSetParamsNilObject;
begin
  try
    dSetParams(nil, nil);
    Fail('Expected exception');
  except
    on E: EdException do ;
  end;
end;

initialization
  RegisterTest(TTestDUtils);
end.
