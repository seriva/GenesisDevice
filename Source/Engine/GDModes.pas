{*******************************************************************************
*                            Genesis Device Engine                             *
*                   Copyright © 2007-2015 Luuk van Venrooij                    *
*                        http://www.luukvanvenrooij.nl                         *
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
unit GDModes;

{$MODE Delphi}

interface

{******************************************************************************}
{* Holds the main modes vars of the engine                                    *}
{******************************************************************************}

uses
  LCLIntf,
  LCLType,
  SysUtils,
  dglOpenGL,
  GDConstants;

type

{******************************************************************************}
{* Main class                                                                 *}
{******************************************************************************}

  TGDModes  = Class
  private
    //modes
    FRenderWireframe   : Boolean;
    FRenderNodeBoxes   : Boolean;
    FRenderObjectBoxes : Boolean;
    FRenderNormals     : Boolean;
    FRenderStats       : Boolean;
    FRenderTerrain     : Boolean;
    FRenderSky         : Boolean;
    FRenderWater       : Boolean;
    FRenderModels      : Boolean;
    FRenderGrass       : Boolean;
    FRenderGUI         : Boolean;
  public
    property RenderWireframe : boolean read FRenderWireframe write FRenderWireframe;
    property RenderNodeBoxes : boolean read FRenderNodeBoxes write FRenderNodeBoxes;
    property RenderObjectBoxes : boolean read FRenderObjectBoxes write FRenderObjectBoxes;
    property RenderNormals : boolean read FRenderNormals write FRenderNormals;
    property RenderStats : boolean read FRenderStats write FRenderStats;
    property RenderTerrain : boolean read FRenderTerrain write FRenderTerrain;
    property RenderSky : boolean read FRenderSky write FRenderSky;
    property RenderModels : boolean read FRenderModels write FRenderModels;
    property RenderWater : boolean read FRenderWater write FRenderWater;
    property RenderGrass : boolean read FRenderGrass write FRenderGrass;
    property RenderGUI : boolean read FRenderGUI write FRenderGUI;

    constructor Create();
    destructor  Destroy(); override;

    procedure InitModes();
  end;

var
  Modes : TGDModes;

implementation

uses
  GDConsole;

constructor TGDModes.Create();
begin
  InitModes();

  Console.AddCommand('RTris', '0,1 : Enable or disable wireframe', CT_BOOLEAN, @FRenderWireframe);
  Console.AddCommand('RNorm',  '0,1 : Show or hide normals', CT_BOOLEAN, @FRenderNormals);
  Console.AddCommand('RTerrain', '0,1 : Show or hide terrain', CT_BOOLEAN, @FRenderTerrain);
  Console.AddCommand('RSky', '0,1 : Show or hide sky', CT_BOOLEAN, @FRenderSky);
  Console.AddCommand('RModels', '0,1 : Show or hide models', CT_BOOLEAN, @FRenderModels);
  Console.AddCommand('RWater', '0,1 : Show or hide water', CT_BOOLEAN, @FRenderWater);
  Console.AddCommand('RGrass', '0,1 : Show or hide grass', CT_BOOLEAN, @FRenderGrass);
  Console.AddCommand('RNodes', '0,1 : Show or hide treenodes', CT_BOOLEAN, @FRenderNodeBoxes);
  Console.AddCommand('RAABB', '0,1 : Show or hide objectboxes', CT_BOOLEAN, @FRenderObjectBoxes);
  Console.AddCommand('RStats', '0,1 : Show or hide stats', CT_BOOLEAN, @FRenderStats);
  Console.AddCommand('RGUI', '0,1 : Show or hide GUI', CT_BOOLEAN, @FRenderGUI);

  Inherited;
end;

destructor  TGDModes.Destroy();
begin
  Inherited;
end;

procedure TGDModes.InitModes();
begin
  FRenderWireframe   := false;
  FRenderNodeBoxes   := false;
  FRenderObjectBoxes := false;
  FRenderNormals     := false;
  FRenderStats       := false;
  FRenderTerrain     := true;
  FRenderSky         := true;
  FRenderWater       := true;
  FRenderGrass       := true;
  FRenderModels      := true;
  FRenderGUI         := true;
end;

end.
