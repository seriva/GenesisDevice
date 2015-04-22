{*******************************************************************************
*                            Genesis Device Engine                             *
*                   Copyright © 2007-2015 Luuk van Venrooij                    *
*                        http://www.luukvanvenrooij.nl                         *
*                         luukvanvenrooij84@gmail.com                          *
********************************************************************************
*                                                                              *
*  This file is part of the Genesis Device Engine.                             *
*                                                                              *
*  The Genesis Device Engine is free software: you can redistribute            *
*  it and/or modify it under the terms of the GNU Lesser General Public        *
*  License as published by the Free Software Foundation, either version 3      *
*  of the License, or any later version.                                       *
*                                                                              *
*  The Genesis Device Engine is distributed in the hope that                   *
*  it will be useful, but WITHOUT ANY WARRANTY; without even the               *
*  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.    *
*  See the GNU Lesser General Public License for more details.                 *
*                                                                              *
*  You should have received a copy of the GNU General Public License           *
*  along with Genesis Device.  If not, see <http://www.gnu.org/licenses/>.     *
*                                                                              *
*******************************************************************************}   
unit GDCommands;

{$MODE Delphi}
{$M+}

{******************************************************************************}
{* Holds the command class                                                    *}
{******************************************************************************}

interface

uses
  SysUtils,
  Classes,
  GDLog,
  GDConsole,
  GDConstants,
  GDRenderer,
  GDSettings,
  GDCellManager,
  GDModes;

type

{******************************************************************************}
{* Command class                                                              *}
{******************************************************************************}

  TGDCommands  = Class
  published

    procedure Help( aParams : String );
    procedure Clear( aParams : String );
    procedure ScreenShot( aParams : String );

    procedure RVSync( aParams : String );
    procedure RGamma( aParams : String );
    procedure RWireframe( aParams : String );
    procedure RStats( aParams : String );
    procedure RNormals( aParams : String );
    procedure RGrid( aParams : String );
    procedure RTerrain( aParams : String );
    procedure RSky( aParams : String );
    procedure RMeshes( aParams : String );
    procedure RWater( aParams : String );
    procedure RGrass( aParams : String );
    procedure RTreeNodes( aParams : String );
    procedure ROBJBoxes( aParams : String );
    procedure RBloom( aParams : String );
    procedure RFXAA( aParams : String );
    procedure RBloomMult( aParams : String );
    procedure RInterface( aParams : String );
    procedure CellSort( aParams : String );
  private
    FHelp : TStringList;

    procedure SeparateString(aString: String; var aCommand, aParams : String);
    procedure CallMethod( aCommand, aParams : String);
    function  CheckBoolean(aParams : String ) : boolean;
    function  CheckDouble(aParams : String; var aDouble : Double ) : boolean;
    function  CheckInteger(aParams : String; var aInteger : Integer ) : boolean;
    function  SetBoolean(aParams : String ) : boolean;
    procedure UnknownCommand();
    procedure UnknownParameter();
    procedure ParameterOutOfBound();
  public
    constructor Create();
    destructor  Destroy(); override;

    procedure ExecuteCommand();
    procedure InitHelp();
  end;
                                               
var
  Commands : TGDCommands;

implementation

{******************************************************************************}
{* Create the command class
{******************************************************************************}

constructor TGDCommands.Create();
begin
  FHelp := TStringList.Create();
  InitHelp();
end;

{******************************************************************************}
{* Destroy the command class                                                  *}
{******************************************************************************}

destructor TGDCommands.Destroy();
begin
  FreeAndNil(FHelp);
  inherited;
end;

{******************************************************************************}
{* Execute a command                                                          *}
{******************************************************************************}

procedure TGDCommands.ExecuteCommand();
var
  iName, iParams : String;
begin
  If Console.ExecuteCommand then
  begin
    Console.AddLine();
    SeparateString(Console.CommandString,iName,iParams);
    Console.CommandString := '';
    CallMethod(iName,iParams);
    Console.ExecuteCommand := false;
  end;
end;

{******************************************************************************}
{* Build the help                                                             *}
{******************************************************************************}

procedure TGDCommands.InitHelp();
begin
  FHelp.Clear();
  FHelp.Add( '' );
  FHelp.Add( '---Help---' );
  FHelp.Add( 'Clear : Clear the log' );
  FHelp.Add( 'ScreenShot <filename> : take a screenshot (can be found in the base screenshot directory)' );
  FHelp.Add( 'RVSync 0,1 : Enable or disable vertical sync' );
  FHelp.Add( 'RGamma 0.0 to 3.0 : Set the gamma value' );
  FHelp.Add( 'RWireframe 0,1 : Enable or disable wireframe' );
  FHelp.Add( 'RStats 0,1 : Show or hide stats' );
  FHelp.Add( 'RNormals 0,1 : Show or hide normals' );
  FHelp.Add( 'RGrid 0,1 : Show or hide grid' );
  FHelp.Add( 'RTerrain 0,1 : Show or hide terrain' );
  FHelp.Add( 'RSky 0,1 : Show or hide sky' );
  FHelp.Add( 'RMeshes 0,1 : Show or hide meshes' );
  FHelp.Add( 'RWater 0,1 : Show or hide water' );
  FHelp.Add( 'RGrass 0,1 : Show or hide grass' );
  FHelp.Add( 'RTreeNodes 0,1 : Show or hide treenodes' );
  FHelp.Add( 'ROBJBoxes 0,1 : Show or hide objectboxes' );
  FHelp.Add( 'RBloom 0,1 : Enable or disable bloom' );
  FHelp.Add( 'RFXAA 0,1 : Enable or disable FXAA' );
  FHelp.Add( 'RBloomMult 0.0 to 1.0 : Set the bloom multiplier value' );
  FHelp.Add( 'RInterface 0,1 : Show or hide interface' );
  FHelp.Add( 'CellSort 0,1 : Enable or disable object depth sorting' );  
  FHelp.Add( '----------' );
  FHelp.Add( '' );
end;

{******************************************************************************}
{* Seperate the command string to get the command and param string            *}
{******************************************************************************}

procedure TGDCommands.SeparateString(aString: String; var aCommand, aParams : String);
var
  iI : integer;
  iString : String;
begin
  If aString = '' then
  begin
    UnknownCommand();
    exit;
  end;

  iI := 1;
  SetLength(iString,0);
  while (aString[iI] <> ' ') and ( iI <= length(aString))   do
  begin
    iString := iString + aString[iI];
    iI := iI + 1;
  end;
  aCommand := iString;

  If ( iI > length(aString)) then
  begin
    aParams := '';
    exit;
  end;

  iI := iI + 1;
  iString := '';
  while ( iI <= length(aString) )   do
  begin
    iString := iString + aString[iI];
    iI := iI + 1;
  end;
   aParams := iString;
end;

{******************************************************************************}
{* Unknown command message                                                    *}
{******************************************************************************}

procedure TGDCommands.UnknownCommand();
begin
    Log.AddNewLine('Error Message: Unknown command!');
    Console.Row := Console.Row + 1;
end;

{******************************************************************************}
{* Unknown parameter message                                                  *}
{******************************************************************************}

procedure TGDCommands.UnknownParameter();
begin
   Log.AddNewLine('Error Message: Unknown parameter!');
   Console.Row := Console.Row + 1;
end;

{******************************************************************************}
{* Parameter message out of bound                                             *}
{******************************************************************************}

procedure TGDCommands.ParameterOutOfBound();
begin
   Log.AddNewLine('Error Message: Parameter out of bound!');
   Console.Row := Console.Row + 1;
end;

{******************************************************************************}
{* Call the method that is associated with command string                     *}
{******************************************************************************}

procedure TGDCommands.CallMethod( aCommand, aParams : String);
Var
  iCmd : procedure( aParams : String ) of object;
Begin
  TMethod(iCmd).data := self;
  TMethod(iCmd).code := MethodAddress(aCommand);
  if not Assigned(iCmd) Then
    UnknownCommand()
  else
    iCmd(aParams);
end;

{******************************************************************************}
{* Set a boolean parameter                                                    *}
{******************************************************************************}

function TGDCommands.SetBoolean(aParams : String ) : boolean;
begin
  If aParams = '1' then
    result := true
  else
    If aParams = '0' then
      result := false
end;

{******************************************************************************}
{* Check a boolean parameter                                                  *}
{******************************************************************************}

function  TGDCommands.CheckBoolean(aParams : String ) : boolean;
begin
  If (aParams = '0') or (aParams = '1') then
   result := true
  else
  begin
    result := false;
    UnknownParameter();
  end;
end;

{******************************************************************************}
{* Check a double parameter                                                   *}
{******************************************************************************}

function TGDCommands.CheckDouble(aParams : String; var aDouble : Double ) : boolean;
begin
  result  := true;
  aDouble := 0;
  try
    aDouble := StrToFloat(aParams);
  except
    UnknownParameter();
    result := false;
    aDouble := 0.0;
  end;
end;

{******************************************************************************}
{* Check a integer parameter                                                  *}
{******************************************************************************}

function TGDCommands.CheckInteger(aParams : String; var aInteger : Integer ) : boolean;
begin
  result  := true;
  aInteger := 0;
  try
    aInteger := StrToInt(aParams);
  except
    UnknownParameter();
    result   := false;
    aInteger := 0;
  end;
end;

{******************************************************************************}
{* Set vsync                                                                  *}
{******************************************************************************}

procedure TGDCommands.RVSync( aParams : String );
begin
 if CheckBoolean(aParams) then
   Settings.VerticalSync := SetBoolean(aParams);
 Renderer.VerticalSync();
end;

{******************************************************************************}
{* Set gamma                                                                  *}
{******************************************************************************}

procedure TGDCommands.RGamma( aParams : String );
var
  iGamma : Double;
begin
  if CheckDouble(aParams, iGamma) then
  begin
    if (iGamma >= 0.10) and (iGamma <= 1.1) then
    begin
      Settings.Gamma := iGamma;
    end
    else
      ParameterOutOfBound();
  end;
end;

{******************************************************************************}
{* Set the bloommultiplier                                                    *}
{******************************************************************************}

procedure TGDCommands.RBloomMult( aParams : String );
var
  iBloomMultiplier : Double;
begin
  if CheckDouble(aParams, iBloomMultiplier) then
  begin
    if (iBloomMultiplier >= 0.0) and (iBloomMultiplier <= 1.0) then
      Renderer.BloomStrengh := iBloomMultiplier
    else
      ParameterOutOfBound();
  end;
end;

{******************************************************************************}
{* Set the wireframe                                                          *}
{******************************************************************************}

procedure TGDCommands.RWireframe( aParams : String );
begin
  If aParams = '0' then
    Modes.RenderMode := RM_NORMAL
  else
    If aParams = '1' then
      Modes.RenderMode := RM_WIREFRAME
    else
      UnknownParameter();
end;

{******************************************************************************}
{* Show the command help                                                      *}
{******************************************************************************}

procedure TGDCommands.Help( aParams : String );
var
  iI : Integer;
begin
  If aParams <> '' then
  begin
     UnknownParameter();
     exit;
  end;

  Log.Save := false;
  for iI := 0 to FHelp.Count - 1 do
  begin
    Log.AddNewLine( FHelp[iI]  );
  end;
  Log.Save := true;

  Console.Row :=  Console.Row + FHelp.Count;
end;

{******************************************************************************}
{* Clear the log                                                              *}
{******************************************************************************}

procedure TGDCommands.Clear( aParams : String );
begin
  If aParams <> '' then
  begin
     UnknownParameter();
     exit;
  end;
  Console.Show := false;
  Log.ClearLog();
  Console.Row := -1;
  Console.Show := true;
end;

{******************************************************************************}
{* Create a screenshot                                                        *}
{******************************************************************************}

procedure TGDCommands.ScreenShot( aParams : String );
begin
  If aParams = '' then
  begin
     UnknownParameter();
     exit;
  end;
  Renderer.MakeScreenShot( aParams );
end;

{******************************************************************************}
{* Show or hide the engine stats                                              *}
{******************************************************************************}

procedure TGDCommands.RStats( aParams : String );
begin
 if CheckBoolean(aParams) then
   Modes.RenderStats := SetBoolean(aParams);
end;

{******************************************************************************}
{* Show or hide the engine normals                                            *}
{******************************************************************************}

procedure TGDCommands.RNormals( aParams : String );
begin
 if CheckBoolean(aParams) then
   Modes.RenderNormals := SetBoolean(aParams);
end;

{******************************************************************************}
{* Show or hide the grid                                                      *}
{******************************************************************************}

procedure TGDCommands.RGrid( aParams : String );
begin
 if CheckBoolean(aParams) then
   Modes.RenderGrid := SetBoolean(aParams );
end;

{******************************************************************************}
{* Show or hide the terrain                                                   *}
{******************************************************************************}

procedure TGDCommands.RTerrain( aParams : String );
begin
 if CheckBoolean(aParams) then
   Modes.RenderTerrain := SetBoolean(aParams );
end;

{******************************************************************************}
{* Show or hide the sky                                                       *}
{******************************************************************************}

procedure TGDCommands.RSky( aParams : String );
begin
 if CheckBoolean(aParams) then
   Modes.RenderSky := SetBoolean(aParams );
end;

{******************************************************************************}
{* Show or hide the water                                                     *}
{******************************************************************************}

procedure TGDCommands.RWater( aParams : String );
begin
 if CheckBoolean(aParams) then
   Modes.RenderWater:= SetBoolean(aParams );
end;

{******************************************************************************}
{* Show or hide the grass                                                     *}
{******************************************************************************}

procedure TGDCommands.RGrass( aParams : String );
begin
 if CheckBoolean(aParams) then
   Modes.RenderGrass:= SetBoolean(aParams );
end;

{******************************************************************************}
{* Show or hide the meshes                                                    *}
{******************************************************************************}

procedure TGDCommands.RMeshes( aParams : String );
begin
 if CheckBoolean(aParams) then
   Modes.RenderMeshes:= SetBoolean(aParams );
end;

{******************************************************************************}
{* Show or hide the tree nodes                                                *}
{******************************************************************************}

procedure TGDCommands.RTreeNodes( aParams : String );
begin
 if CheckBoolean(aParams) then
   Modes.RenderNodeBoxes := SetBoolean(aParams);
end;

{******************************************************************************}
{* Show or hide the object boxes                                              *}
{******************************************************************************}

procedure TGDCommands.ROBJBoxes( aParams : String );
begin
 if CheckBoolean(aParams) then
   Modes.RenderObjectBoxes := SetBoolean(aParams );
end;

{******************************************************************************}
{* Use or hide the bloom                                                      *}
{******************************************************************************}

procedure TGDCommands.RBloom( aParams : String );
begin
 if CheckBoolean(aParams) then
   Settings.UseBloom := SetBoolean(aParams );
end;

{******************************************************************************}
{* Use or hide the fxaa                                                       *}
{******************************************************************************}

procedure TGDCommands.RFXAA( aParams : String );
begin
 if CheckBoolean(aParams) then
   Settings.UseFXAA := SetBoolean(aParams );
end;

{******************************************************************************}
{* Show or hide the gameinterface                                             *}
{******************************************************************************}

procedure TGDCommands.RInterface( aParams : String );
begin
 if CheckBoolean(aParams) then
   Modes.RenderInterfaces := SetBoolean(aParams );
end;

{******************************************************************************}
{* Use quicksort for the octree                                               *}
{******************************************************************************}

procedure TGDCommands.CellSort( aParams : String );
begin
 if CheckBoolean(aParams) then
   CellManager.SortVisibleCells := SetBoolean(aParams );
end;

end.
