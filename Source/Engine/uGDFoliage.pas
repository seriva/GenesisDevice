{*******************************************************************************
*                            Genesis Device Engine                             *
*                   Copyright © 2007-2022 Luuk van Venrooij                    *
*                        http://www.luukvanvenrooij.nl                         *
********************************************************************************
*                                                                              *
*  This file is part of the Genesis Device Engine                              *
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
unit uGDFoliage;

{$MODE objfpc}

interface

uses
  FGL,
  SysUtils,
  dglOpenGL,
  JsonTools,
  uGDBmp,
  uGDStringParsing,
  uGDTexture,
  uGDTypes,
  uGDConstants,
  uGDMesh,
  uGDResource;

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

    constructor Create(aNode : TJsonNode);
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

    constructor Create(aNode : TJsonNode);
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

    constructor Create(aNode : TJsonNode);
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

    constructor Create(aNode : TJsonNode);
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

    Function  InitFoliage( aNode : TJsonNode ) : boolean;
    procedure Clear();

    procedure StartRenderingGrass( aRenderAttribute : TGDRenderAttribute );
    procedure EndRenderingGrass();
  end;

implementation

uses
  uGDEngine;

{******************************************************************************}
{* Create the layertype class                                                 *}
{******************************************************************************}

constructor TGDLayerItem.Create(aNode : TJsonNode);
begin
  TerrainRotation := aNode.Find('TerrainRotation').AsBoolean;
  Scale           := ReadVector(aNode.Find('Scale').AsString);
  RandomScale     := ReadVector(aNode.Find('RandomScale').AsString);
  CoverOfTotal    := aNode.Find('CoverOfTotal').AsNumber;
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

constructor TGDGrassItem.Create(aNode : TJsonNode);
begin
  inherited Create(aNode);
  Texture := GDResources.LoadTexture(aNode.Find('Texture').AsString, TD_HIGH, GDSettings.TextureFilter);
end;

{******************************************************************************}
{* Destroy the grasstype                                                      *}
{******************************************************************************}

destructor  TGDGrassItem.Destroy();
begin
  GDResources.RemoveResource(TGDResource(Texture));
  inherited
end;

{******************************************************************************}
{* Create the treetype class                                                  *}
{******************************************************************************}

constructor TGDMeshItem.Create(aNode : TJsonNode);
begin
  inherited Create(aNode);
  Mesh           := GDResources.Loadmesh(aNode.Find('Model').AsString);
  MeshLOD1       := GDResources.Loadmesh(aNode.Find('ModelLOD1').AsString);
  MeshLOD2       := GDResources.Loadmesh(aNode.Find('ModelLOD2').AsString);
  OffsetPosition := ReadVector(aNode.Find('OffsetPosition').AsString);
  Rotation       := ReadVector(aNode.Find('Rotation').AsString);
  RandomRotation := ReadVector(aNode.Find('RandomRotation').AsString);
 	CastShadow     := aNode.Find('CastShadow').AsBoolean;
	ReceiveShadow  := aNode.Find('ReceiveShadow').AsBoolean;
end;

{******************************************************************************}
{* Destroy the treetype                                                       *}
{******************************************************************************}

destructor  TGDMeshItem.Destroy();
begin
  GDResources.RemoveResource(TGDResource(Mesh));
  GDResources.RemoveResource(TGDResource(MeshLOD1));
  GDResources.RemoveResource(TGDResource(MeshLOD2));
  inherited
end;

{******************************************************************************}
{* Create layer                                                               *}
{******************************************************************************}

constructor TGDLayer.Create(aNode : TJsonNode);
var
  iLayerItems : TJsonNode;
  iMap : TGDBmp;
  iX, iY : integer;
begin
  LayerItems := TGDLayerItemList.Create();

  if aNode.Find('Type').AsString = 'MESH' then
    LayerType := LT_MESH
  else
    LayerType := LT_GRASS;

  iMap       := TGDBmp.Create(aNode.Find('Map').AsString);
  Count      := Trunc(aNode.Find('Count').AsNumber); 
  LowerLimit := Trunc(aNode.Find('LowerLimit').AsNumber);
  UpperLimit := Trunc(aNode.Find('UpperLimit').AsNumber);

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

  //layer types
  iLayerItems := aNode.Find('Items');
  for iX := 0 to iLayerItems.Count-1 do
  begin
    if LayerType = LT_MESH then
      LayerItems.Add(TGDMeshItem.Create(iLayerItems.Child(iX)))
    else
      LayerItems.Add(TGDGrassItem.Create(iLayerItems.Child(iX)));    
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

Function TGDFoliage.InitFoliage( aNode : TJsonNode ) : boolean;
var
  iLayers : TJsonNode;
  iX : integer;
  iError : String;
begin
  Clear();
  GDConsole.Write('Loading foliage...');
  GDConsole.Use := false;
  try
    result := true;

    FGrassAnimationSpeed    := aNode.Find('GrassAnimationSpeed').AsNumber;
    FGrassAnimationStrength := aNode.Find('GrassAnimationStrength').AsNumber;
    FTreeAnimationSpeed     := aNode.Find('TreeAnimationSpeed').AsNumber;
    FTreeAnimationStrength  := aNode.Find('TreeAnimationStrength').AsNumber;

    iLayers := aNode.Find('Layers');
    for iX := 0 to iLayers.Count-1 do
      FLayers.Add(TGDLayer.Create(iLayers.Child(iX))); 
  except
    on E: Exception do
    begin
      iError := E.Message;
      result := false;
    end;
  end;

  GDConsole.WriteOkFail(result, iError);
  GDConsole.Use := true;
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

  if GDModes.RenderWireframe then
  begin
    GDRenderer.SetColor(0,0.25,0,1);
    glDisable(GL_CULL_FACE);
  end
  else
  begin
    glEnable(GL_ALPHA_TEST);
    glAlphaFunc(GL_GREATER, 0.7);
    glDisable(GL_CULL_FACE);
    with GDRenderer do
    begin
      GrassShader.Bind();
      GDRenderer.SetJoinedParams(GrassShader);
      GrassShader.SetInt('T_GRASSTEX', 0);
      GrassShader.SetFloat('F_ANIMATION_SPEED', GDTiming.ElapsedTime / FGrassAnimationSpeed);
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
