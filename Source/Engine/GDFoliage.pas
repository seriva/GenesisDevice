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
unit GDFoliage;

{$MODE objfpc}

interface

uses
  FGL,
  GDBmp,
  SysUtils,
  dglOpenGL,
  IniFiles,
  GDStringParsing,
  GDTexture,
  GDTypes,
  GDConstants,
  GDMesh,
  GDResource;

type
 {******************************************************************************}
 {* Layertype class                                                            *}
 {******************************************************************************}

  TGDLayerItem = class
  private
  public
    TerrainRotation : boolean;
    Scale           : TGDVector;
    RandomScale     : TGDVector;
    CoverOfTotal    : Single;

    constructor Create(aIniFile : TIniFile; aSection : String);
    destructor  Destroy(); override;
  end;
  TGDLayerItemList = specialize TFPGObjectList<TGDLayerItem>;

{******************************************************************************}
{* GrassItem class                                                            *}
{******************************************************************************}

  TGDGrassItem = class (TGDLayerItem)
  private
  public
    Texture      : TGDTexture;

    constructor Create(aIniFile : TIniFile; aSection : String);
    destructor  Destroy(); override;
  end;

{******************************************************************************}
{* MeshItem class                                                             *}
{******************************************************************************}

  TGDMeshItem = class (TGDLayerItem)
  private
  public
    Mesh           : TGDMesh;
    MeshLOD1       : TGDMesh;
    MeshLOD2       : TGDMesh;
    OffsetPosition : TGDVector;
    Rotation       : TGDVector;
    RandomRotation : TGDVector;
 		CastShadow     : Boolean;
		ReceiveShadow  : Boolean;

    constructor Create(aIniFile : TIniFile; aSection : String);
    destructor  Destroy(); override;
  end;

{******************************************************************************}
{* Layer class                                                                *}
{******************************************************************************}

  TGDLayer = class
  private
  public
    Map : array of array of boolean;
    LayerType  : TGDLayerType;
    LayerItems : TGDLayerItemList;
    Count      : Integer;
    LowerLimit : Integer;
    UpperLimit : Integer;

    constructor Create(aIniFile : TIniFile; aSection : String);
    destructor  Destroy(); override;

    Function CheckMap( aX, aY : Integer ) : Boolean;
  end;
  TGDLayerList = specialize TFPGObjectList<TGDLayer>;

{******************************************************************************}
{* Foliage class                                                              *}
{******************************************************************************}

  TGDFoliage = class
  private
    FGrassAnimationSpeed    : Single;
    FGrassAnimationStrength : Single;
    FTreeAnimationSpeed     : Single;
    FTreeAnimationStrength  : Single;

    FLayers                 : TGDLayerList;
  public

    property GrassAnimationSpeed    : Single read FGrassAnimationSpeed;
    property GrassAnimationStrength : Single read FGrassAnimationStrength;
    property TreeAnimationSpeed     : Single read FTreeAnimationSpeed;
    property TreeAnimationStrength  : Single read FTreeAnimationStrength;

    property Layers : TGDLayerList read FLayers;

    constructor Create();
    destructor  Destroy(); override;

    Function  InitFoliage( aIniFile : TIniFile ) : boolean;
    procedure Clear();

    procedure StartRenderingGrass( aRenderAttribute : TGDRenderAttribute );
    procedure EndRenderingGrass();
  end;

implementation

uses
  GDEngine;

{******************************************************************************}
{* Create the layertype class                                                 *}
{******************************************************************************}

constructor TGDLayerItem.Create(aIniFile : TIniFile; aSection : String);
begin
  TerrainRotation := aIniFile.ReadBool( aSection, 'TerrainRotation', false );
  Scale           := ReadVector(aIniFile, aSection, 'Scale');
  RandomScale     := ReadVector(aIniFile, aSection, 'RandomScale');
  CoverOfTotal    := aIniFile.ReadFloat( aSection, 'CoverOfTotal', 100 );
end;

{******************************************************************************}
{* Create the layertype class                                                 *}
{******************************************************************************}

destructor  TGDLayerItem.Destroy();
begin
end;

{******************************************************************************}
{* Create the grasstype class                                                 *}
{******************************************************************************}

constructor TGDGrassItem.Create(aIniFile : TIniFile; aSection : String);
begin
  inherited Create(aIniFile, aSection);
  Texture := Engine.Resources.LoadTexture(aIniFile.ReadString( aSection, 'Texture', ''),
                                         TD_HIGH, Engine.Settings.TextureFilter);
end;

{******************************************************************************}
{* Destroy the grasstype                                                      *}
{******************************************************************************}

destructor  TGDGrassItem.Destroy();
begin
  Engine.Resources.RemoveResource(TGDResource(Texture));
  inherited
end;

{******************************************************************************}
{* Create the treetype class                                                  *}
{******************************************************************************}

constructor TGDMeshItem.Create(aIniFile : TIniFile; aSection : String);
begin
  inherited Create(aIniFile, aSection);
  Mesh           := Engine.Resources.Loadmesh(aIniFile.ReadString( aSection, 'Model', ''));
  MeshLOD1       := Engine.Resources.Loadmesh(aIniFile.ReadString( aSection, 'ModelLOD1', ''));
  MeshLOD2       := Engine.Resources.Loadmesh(aIniFile.ReadString( aSection, 'ModelLOD2', ''));
  OffsetPosition := ReadVector(aIniFile, aSection, 'OffsetPosition');
  Rotation       := ReadVector(aIniFile, aSection, 'Rotation');
  RandomRotation := ReadVector(aIniFile, aSection, 'RandomRotation');
 	CastShadow     := aIniFile.ReadBool( aSection, 'CastShadow', false);
	ReceiveShadow  := aIniFile.ReadBool( aSection, 'ReceiveShadow', false);
end;

{******************************************************************************}
{* Destroy the treetype                                                       *}
{******************************************************************************}

destructor  TGDMeshItem.Destroy();
begin
  Engine.Resources.RemoveResource(TGDResource(Mesh));
  Engine.Resources.RemoveResource(TGDResource(MeshLOD1));
  Engine.Resources.RemoveResource(TGDResource(MeshLOD2));
  inherited
end;

{******************************************************************************}
{* Create layer                                                               *}
{******************************************************************************}

constructor TGDLayer.Create(aIniFile : TIniFile; aSection : String);
var
  iMap : TGDBmp;
  iX, iY : integer;
begin
  LayerItems := TGDLayerItemList.Create();

  if aIniFile.ReadString( aSection, 'Type', 'MESH' ) = 'MESH' then
    LayerType := LT_MESH
  else
    LayerType := LT_GRASS;

  iMap        := TGDBmp.Create(aIniFile.ReadString( aSection, 'Map', 'map.bmp'));
  Count      := aIniFile.ReadInteger( aSection, 'Count', 0 );
  LowerLimit := aIniFile.ReadInteger( aSection, 'LowerLimit', -2147483648 );
  UpperLimit := aIniFile.ReadInteger( aSection, 'UpperLimit',  2147483647 );

  if ((iMap.Width mod 2) <> 0) or ((iMap.Height mod 2) <> 0) then
    Raise Exception.Create('Map dimensions are incorrect!');

  SetLength(Map, iMap.Width);
  for iX := 0 to (iMap.Width-1) do
  begin
    SetLength(Map[iX], iMap.Height);
    for iY := 0 to (iMap.Height-1) do
      Map[iX,iY]  := iMap.GetInt(iX,iY) > 0;
  end;
  FreeAndNil(iMap);

  //tree types
  iX := 1;
  while(aIniFile.SectionExists(aSection + '_Item' + IntToStr(iX))) do
  begin
    if LayerType = LT_MESH then
      LayerItems.Add(TGDMeshItem.Create(aIniFile, aSection + '_Item' + IntToStr(iX)))
    else
      LayerItems.Add(TGDGrassItem.Create(aIniFile, aSection + '_Item' + IntToStr(iX)));

    iX := iX + 1;
  end;
end;

{******************************************************************************}
{* Destroy layer                                                              *}
{******************************************************************************}

destructor TGDLayer.Destroy();
var
  iX : Integer;
begin
  if (Map <> nil) then
  begin
    for iX := 0 to Length(Map)-1 do
      SetLength(Map[iX], 0);
    SetLength(Map, 0);
  end;
  FreeAndNil(LayerItems);
end;

{******************************************************************************}
{* Check location on layermap                                                 *}
{******************************************************************************}

Function TGDLayer.CheckMap( aX, aY : Integer ) : Boolean;
begin
  result := Map[aX, aY];
end;

{******************************************************************************}
{* Create the foliage class                                                   *}
{******************************************************************************}

constructor TGDFoliage.Create();
begin
  FLayers := TGDLayerList.Create();
end;

{******************************************************************************}
{* Destroy the foliage class                                                  *}
{******************************************************************************}

destructor  TGDFoliage.Destroy();
begin
  inherited;
  FreeAndNil(FLayers);
end;

{******************************************************************************}
{* Init the foliage                                                           *}
{******************************************************************************}

Function TGDFoliage.InitFoliage( aIniFile : TIniFile ) : boolean;
var
  iX : integer;
  iError : String;
begin
  Clear();
  Engine.Console.Write('Loading foliage...');
  Engine.Console.Use := false;
  try
    result := true;

    FGrassAnimationSpeed    := aIniFile.ReadFloat( 'Foliage', 'GrassAnimationSpeed', 1000 );
    FGrassAnimationStrength := aIniFile.ReadFloat( 'Foliage', 'GrassAnimationStrength', 5);
    FTreeAnimationSpeed     := aIniFile.ReadFloat( 'Foliage', 'TreeAnimationSpeed', 1000 );
    FTreeAnimationStrength  := aIniFile.ReadFloat( 'Foliage', 'TreeAnimationStrength', 5 );

    iX := 1;
    while(aIniFile.SectionExists('Layer' + IntToStr(iX))) do
    begin
      FLayers.Add(TGDLayer.Create(aIniFile, 'Layer' + IntToStr(iX)));
      iX := iX + 1;
    end;
  except
    on E: Exception do
    begin
      iError := E.Message;
      result := false;
    end;
  end;

  Engine.Console.WriteOkFail(result, iError);
  Engine.Console.Use := true;
end;

{******************************************************************************}
{* Clear the foliage                                                          *}
{******************************************************************************}

procedure TGDFoliage.Clear();
begin
  FLayers.Clear();
end;

{******************************************************************************}
{* Start the rendering of a grasscell                                         *}
{******************************************************************************}

procedure TGDFoliage.StartRenderingGrass( aRenderAttribute : TGDRenderAttribute );
begin
  if Not(aRenderAttribute = RA_NORMAL) then exit;

  if Engine.Modes.RenderWireframe then
  begin
    Engine.Renderer.SetColor(0,0.25,0,1);
    glDisable(GL_CULL_FACE);
  end
  else
  begin
    glEnable(GL_ALPHA_TEST);
    glAlphaFunc(GL_GREATER, 0.7);
    glDisable(GL_CULL_FACE);
    with Engine.Renderer do
    begin
      GrassShader.Bind();
      Engine.Renderer.SetJoinedParams(GrassShader);
      GrassShader.SetInt('T_GRASSTEX', 0);
      GrassShader.SetFloat('F_ANIMATION_SPEED', Engine.Timing.ElapsedTime / FGrassAnimationSpeed);
      GrassShader.SetFloat('F_ANIMATION_STRENGTH', FGrassAnimationStrength);
    end;
  end;
end;

{******************************************************************************}
{* End the rendering of a grasscell                                           *}
{******************************************************************************}

procedure TGDFoliage.EndRenderingGrass();
begin
  glDisable(GL_ALPHA_TEST);
  glEnable(GL_CULL_FACE);
end;

end.
