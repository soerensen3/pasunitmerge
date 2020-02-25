program pasunitmerge;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, LazFileUtils, StrUtils,
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
  UnitNames: TStringList;
  UsesIntf: TStringList;
  UsesImpl: TStringList;
  OutIntf: TStringList;
  OutImpl: TStringList;
  OutUnit: TStringList;

procedure FilterUsesSection( UsesSect: TStringList );
var
  n, i: Integer;
begin
  for i:= UsesSect.Count - 1 downto 0 do
    if ( UnitNames.Find( UsesSect[ i ], n )) then
      UsesSect.Delete( i );
end;

procedure ParseUnit( AFilename: String );
var
  Tool: TCodeTool;
  _UsesIntf: TStrings = nil;
  _UsesImpl: TStrings = nil;
  AUnit: TCodeBuffer;
  StartNode: TCodeTreeNode;
  CodeStart, CodeEnd: TCodePosition;
  UnitName: String;

  procedure AddCodeSection( OutList: TStringList; StartNode: TCodeTreeNode );
  var
    UsesSect: TCodeTreeNode;
    SrcCode: String;
  begin
    UsesSect:= Tool.FindUsesNode( StartNode );
    if ( Assigned( UsesSect )) then
      Tool.CleanPosToCodePos( UsesSect.EndPos, CodeStart )
    else
      Tool.CleanPosToCodePos( StartNode.Next.StartPos, CodeStart );

    Tool.CleanPosToCodePos( StartNode.EndPos, CodeEnd );
    SrcCode:= Copy( AUnit.Source, CodeStart.P, CodeEnd.P - CodeStart.P );
    RemoveLeadingchars( SrcCode, [ ' ', LineEnding ]);
    RemoveTrailingChars( SrcCode, [ ' ', LineEnding ]);
    OutList.Add(
      '// ' + UnitName + ' -->' + LineEnding +
      SrcCode + LineEnding +
      '// <--' + UnitName + LineEnding
    );
  end;

begin
  try
    AUnit:= CodeToolBoss.LoadFile( ExpandFileName( AFilename ), False, False );
    if ( not FileExistsUTF8( AFilename )) then
      raise Exception.Create( 'The code of the unit file could not be parsed because the file does not exist: ' + AFilename );

    if ( not CodeToolBoss.Explore( AUnit, Tool, False, False )) then
      raise Exception.Create( 'The code of the unit file could not be parsed as it contains errors: ' + AUnit.Filename );

    UnitName:= ExtractFileNameOnly( AFilename );
    UnitNames.Add( UnitName );

    // Uses Sections
    Tool.FindUsedUnitNames( _UsesIntf, _UsesImpl );

    // Interface Section
    StartNode:= Tool.FindInterfaceNode;
    AddCodeSection( OutIntf, StartNode );

    // Implementation Section
    StartNode:= Tool.FindImplementationNode;
    AddCodeSection( OutImpl, StartNode );

    UsesIntf.AddStrings( _UsesIntf );
    UsesImpl.AddStrings( _UsesImpl );
  finally
    FreeAndNil( _UsesImpl );
    FreeAndNil( _UsesIntf );
  end;
end;

procedure GenOutFile( FName: String );
  function AddUsesSection( Sect: TStringList ): String;
  var
    i: Integer;
  begin
    if ( Sect.Count > 0 ) then begin
      Result:= 'uses' + LineEnding;

      i:= 0;
      while i < Sect.Count - 1 do begin
        Result += '  ' + Sect[ i ] + ',' + LineEnding;
        Inc( i );
      end;
      Result += '  ' + Sect[ Sect.Count - 1 ] + ';' + LineEnding + LineEnding;
    end;
  end;

begin
  OutUnit.Text:= 'unit ' + ExtractFileNameOnly( FName ) + ';' + LineEnding + LineEnding;
  OutUnit.Add(
    'interface' + LineEnding +
    LineEnding +
    AddUsesSection( UsesIntf ) +
    OutIntf.Text +
    'implementation' + LineEnding +
    LineEnding +
    AddUsesSection( UsesImpl ) +
    OutImpl.Text +
    'end.'
  );

end;

procedure TPasUnitMerge.DoRun;
var
  ErrorMsg: String;
  i: Integer;
begin
  UnitNames:= TStringList.Create;
  UnitNames.Sorted:= True;
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

      FilterUsesSection( UsesIntf );
      FilterUsesSection( UsesImpl );
      GenOutFile( Params[ i + 1 ]);

      OutUnit.SaveToFile( Params[ i + 1 ]);

      Inc( i );
    end else
      ParseUnit( Params[ i ]);
      //WriteLn( 'Param ', i, ': ', Params[ i ]);
    Inc( i );
  end;




  // stop program loop
  FreeAndNil( UnitNames );
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

