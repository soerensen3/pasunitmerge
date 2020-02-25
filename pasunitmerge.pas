program pasunitmerge;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  CodeToolManager, CustomCodeTool, CodeCache, CodeTree
  { you can add units after this };

type

  { TPasUnitMerge }

  TPasUnitMerge = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TPasUnitMerge }


var
  UsesIntf: TStringList;
  UsesImpl: TStringList;
  OutIntf: TStringList;
  OutImpl: TStringList;
  OutUnit: TStringList;

procedure ParseUnit( AFilename: String );
var
  Tool: TCodeTool;
  _UsesIntf: TStrings = nil;
  _UsesImpl: TStrings = nil;
  AUnit: TCodeBuffer;
  Intf, UsesSectIntf: TCodeTreeNode;
  CodeStart, CodeEnd: TCodePosition;
begin
  try
    AUnit:= CodeToolBoss.LoadFile( ExpandFileName( AFilename ), False, False );
    if ( not CodeToolBoss.Explore( AUnit, Tool, False, False )) then
      raise Exception.Create( 'The code of the unit file could not be parsed as it contains errors: ' + AUnit.Filename );

    // Uses Sections
    Tool.FindUsedUnitNames( _UsesIntf, _UsesImpl );

    // Interface Section
    Intf:= Tool.FindInterfaceNode;
    UsesSectIntf:= Tool.FindUsesNode( Intf );
    Tool.CleanPosToCodePos( UsesSectIntf.EndPos, CodeStart );
    Tool.CleanPosToCodePos( Intf.EndPos, CodeEnd );
    OutIntf.Add( Copy( AUnit.Source, CodeStart.P, CodeEnd.P - CodeStart.P ));

    // Implementation Section
    Intf:= Tool.FindImplementationNode;
    UsesSectIntf:= Tool.FindUsesNode( Intf );
    Tool.CleanPosToCodePos( UsesSectIntf.EndPos, CodeStart );
    Tool.CleanPosToCodePos( Intf.EndPos, CodeEnd );
    OutImpl.Add( Copy( AUnit.Source, CodeStart.P, CodeEnd.P - CodeStart.P ));

    UsesIntf.AddStrings( _UsesIntf );
    UsesImpl.AddStrings( _UsesImpl );
  finally
    FreeAndNil( _UsesImpl );
    FreeAndNil( _UsesIntf );
  end;
end;

procedure TPasUnitMerge.DoRun;
var
  ErrorMsg: String;
  i: Integer;
begin
  UsesImpl:= TStringList.Create;
  UsesIntf:= TStringList.Create;
  UsesIntf.Sorted:= True;
  UsesIntf.Duplicates:= dupIgnore;
  UsesImpl.Sorted:= True;
  UsesImpl.Duplicates:= dupIgnore;
  OutUnit:= TStringList.Create;
  OutIntf:= TStringList.Create;
  OutImpl:= TStringList.Create;

  // quick check parameters
  ErrorMsg:=CheckOptions('ho', 'help|outfile');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  i:= 1;
  while i <= ParamCount do begin
    if ( Params[ i ] = '-o' ) then begin
      WriteLn( 'Outfile: ', Params[ i + 1 ]);

      OutUnit:= 'unit ' + ExtractFileNameOnly( Params[ i + 1 ]) + ';';
      WriteLn( 'OutUnit: ');
      WriteLn( OutUnit.Text );

      Inc( i );
    end else
      ParseUnit( Params[ i ]);
      //WriteLn( 'Param ', i, ': ', Params[ i ]);
    Inc( i );
  end;




  // stop program loop
  FreeAndNil( UsesImpl );
  FreeAndNil( UsesIntf );
  FreeAndNil( OutUnit );
  FreeAndNil( OutIntf );
  FreeAndNil( OutImpl );
  Terminate;
end;

constructor TPasUnitMerge.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TPasUnitMerge.Destroy;
begin
  inherited Destroy;
end;

procedure TPasUnitMerge.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TPasUnitMerge;
begin
  Application:=TPasUnitMerge.Create(nil);
  Application.Title:='PasUnitMerge';
  Application.Run;
  Application.Free;
end.

