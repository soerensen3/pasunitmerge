unit Unit2;

interface 

uses SysUtils, StrUtils, Unit1;

type
  TTestClass2 = class ( TTestClass )
    procedure Test;
  end;
  
  
implementation

uses
  SomeOtherUnit;

procedure TTestClass2.Test;
begin
  WriteLn( 'Test2' );
end;

end.
