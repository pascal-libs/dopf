unit test_dutils;

{$mode objfpc}{$H+}

interface

uses
  fpcunit, testregistry, dutils, DB;

type
  TTestDUtils = class(TTestCase)
  published
    procedure TestParameterizeSQLNilParams;
    procedure TestParameterizeSQLTokenBoundary;
    procedure TestGetFieldsNilObject;
    procedure TestSetFieldsNilObject;
    procedure TestGetParamsNilObject;
    procedure TestSetParamsNilObject;
  end;

implementation

uses
  dClasses
  ;

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

procedure TTestDUtils.TestParameterizeSQLTokenBoundary;
var
  S: string;
  P: TParams;
begin
  // A parameter whose name is a prefix of another (:user vs :username) must
  // be replaced only on token boundaries.
  P := TParams.Create(nil);
  try
    P.CreateParam(ftInteger, 'user', ptInput).AsInteger := 42;
    S := 'select * from t where user_name=:username and user_id=:user';
    dParameterizeSQL(S, P);
    AssertEquals('select * from t where user_name=:username and user_id=42', S);
  finally
    P.Free;
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
