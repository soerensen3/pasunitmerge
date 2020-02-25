unit Unit1;

interface 

uses SysUtils, StrUtils;

type
  TTestClass = class ( TObject )
    procedure Test;
  end;
  
  
{this should be copied}implementation{not this}

uses
  SomeUnit;

procedure TTestClass.Test;
begin
  WriteLn( 'Test' );
end;

{this should be copied}end.{not this}
