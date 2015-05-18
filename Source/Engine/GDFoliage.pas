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

{$MODE Delphi}

{******************************************************************************}
{* Hold the main grass classes for settings and coverage of terrain grass     *}
{* foliage.                                                                   *}
{******************************************************************************}

interface

uses
  SysUtils,
  dglOpenGL,
  Graphics,
  GDTexture,
  GDTypes,
  GDConsole,
  GDSettings,
  GDConstants,
  GDMesh,
  GDTiming,
  Contnrs,
  GDResources,
  GDResource,
  GDModes;

type

{******************************************************************************}
{* Grasstype input record                                                     *}
{******************************************************************************}

  TGDGrassTypeInput = record
    Texture       : String;
    Scale         : TGDVector;
    RandomScale   : TGDVector;
    CoverOfTotal  : Single;
  end;

{******************************************************************************}
{* Meshtype input record                                                      *}
{******************************************************************************}

  TGDMeshTypeInput = record
    Model            : String;
    ModelLOD1        : String;
    ModelLOD2        : String;
    Scale            : Single;
    RandomScale      : Single;
    StartRotation    : TGDVector;
    CoverOfTotal     : Single;
  end;

{******************************************************************************}
{* Grasstype class                                                            *}
{******************************************************************************}

  TGDGrassType = class(TObject)
  private
    FTexture : TGDTexture;
    FScale   : TGDVector;
    FRandomScale : TGDVector;
    FCoverOfTotal : Single;
  public
    property Texture : TGDTexture read FTexture;
    property Scale : TGDVector read FScale;
    property RandomScale : TGDVector read FRandomScale;
    property CoverOfTotal : Single read FCoverOfTotal;

    constructor Create(aInput : TGDGrassTypeInput);
    destructor  Destroy(); override;
  end;

{******************************************************************************}
{* Treetype class                                                             *}
{******************************************************************************}

  TGDMeshType = class(TObject)
  private
    FMesh          : TGDMesh;
    FMeshLOD1      : TGDMesh;
    FMeshLOD2      : TGDMesh;
    FStartRotation : TGDVector;
    FScale         : Single;
    FRandomScale   : Single;
    FCoverOfTotal  : Single;
  public
    property Mesh : TGDMesh read FMesh;
    property MeshLOD1 : TGDMesh read FMeshLOD1;
    property MeshLOD2 : TGDMesh read FMeshLOD2;
    property StartRotation : TGDVector read FStartRotation;
    property Scale : Single read FScale;
    property RandomScale : Single read FRandomScale;
    property CoverOfTotal : Single read FCoverOfTotal;

    constructor Create(aInput : TGDMeshTypeInput);
    destructor  Destroy(); override;
  end;

{******************************************************************************}
{* Foliage input record                                                       *}
{******************************************************************************}

  TGDFoliageInput = record
    GrassMap               : String;
    GrassCellCountX        : Integer;
    GrassCellCountY        : Integer;
    GrassAnimationSpeed    : Single;
    GrassAnimationStrength : Single;

    TreeMap                : String;
    TreeCount              : Integer;
    TreeLowerLimit         : Integer;
    TreeUpperLimit         : Integer;
    TreeAnimationSpeed     : Single;
    TreeAnimationStrength  : Single;

    RockMap                : String;
    RockCount              : Integer;
  end;

{******************************************************************************}
{* Foliage class                                                              *}
{******************************************************************************}

  TGDFoliage = class
  private
    FGrassAnimationSpeed    : Single;
    FGrassAnimationStrength : Single;
    FTreeAnimationSpeed     : Single;
    FTreeAnimationStrength  : Single;

    FGrassTypes           : TObjectList;
    FGrassCellCountX      : Integer;
    FGrassCellCountY      : Integer;

    FTreeTypes            : TObjectList;
    FTreeCount            : Integer;
    FTreeLowerLimit       : Integer;
    FTreeUpperLimit       : Integer;

    FRockTypes            : TObjectList;
    FRockCount            : Integer;
  public
    GrassMap : array of array of boolean;
    TreeMap : array of array of boolean;
    RockMap : array of array of boolean;

    property GrassAnimationSpeed    : Single read FGrassAnimationSpeed;
    property GrassAnimationStrength : Single read FGrassAnimationStrength;
    property TreeAnimationSpeed    : Single read FTreeAnimationSpeed;
    property TreeAnimationStrength : Single read FTreeAnimationStrength;

    property GrassTypes : TObjectList read FGrassTypes;
    property GrassCellCountX : Integer read FGrassCellCountX;
    property GrassCellCountY : Integer read FGrassCellCountY;

    property TreeTypes : TObjectList read FTreeTypes;
    property TreeCount : Integer read FTreeCount;
    property TreeLowerLimit : Integer read FTreeLowerLimit;
    property TreeUpperLimit : Integer read FTreeUpperLimit;

    property RockTypes : TObjectList read FRockTypes;
    property RockCount : Integer read FRockCount;

    constructor Create();
    destructor  Destroy(); override;

    Function  InitFoliage( aInput : TGDFoliageInput ) : boolean;
    procedure Clear();

    procedure StartRenderingGrass( aRenderAttribute : TGDRenderAttribute );
    procedure EndRenderingGrass();

    Function CheckTreeMap( aX, aY : Integer ) : Boolean;
    Function CheckGrassMap( aX, aY : Integer ) : Boolean;
    Function CheckRockMap( aX, aY : Integer ) : Boolean;
  end;

implementation

uses
  GDRenderer;

{******************************************************************************}
{* Create the grasstype class                                                 *}
{******************************************************************************}

constructor TGDGrassType.Create(aInput : TGDGrassTypeInput);
begin
  FTexture := Resources.LoadTexture(aInput.Texture ,TD_HIGH,Settings.TextureFilter);
  FScale := aInput.Scale.Copy();
  FRandomScale :=aInput.RandomScale.Copy();
  FCoverOfTotal := aInput.CoverOfTotal;
end;

{******************************************************************************}
{* Destroy the grasstype                                                      *}
{******************************************************************************}

destructor  TGDGrassType.Destroy();
begin
  Resources.RemoveResource(TGDResource(FTexture));
  inherited
end;

{******************************************************************************}
{* Create the treetype class                                                  *}
{******************************************************************************}

constructor TGDMeshType.Create(aInput : TGDMeshTypeInput );
begin
  FMesh := Resources.Loadmesh(aInput.Model);
  FMeshLOD1 := Resources.Loadmesh(aInput.ModelLOD1);
  FMeshLOD2 := Resources.Loadmesh(aInput.ModelLOD2);
  FStartRotation := aInput.StartRotation.Copy();
  FScale := aInput.Scale;
  FRandomScale := aInput.RandomScale;
  FCoverOfTotal := aInput.CoverOfTotal;
end;

{******************************************************************************}
{* Destroy the treetype                                                       *}
{******************************************************************************}

destructor  TGDMeshType.Destroy();
begin
  Resources.RemoveResource(TGDResource(FMesh));
  Resources.RemoveResource(TGDResource(FMeshLOD1));
  Resources.RemoveResource(TGDResource(FMeshLOD2));
  inherited
end;

{******************************************************************************}
{* Create the foliage class                                                   *}
{******************************************************************************}

constructor TGDFoliage.Create();
begin
  FGrassTypes := TObjectList.Create();
  FTreeTypes  := TObjectList.Create();
  FRockTypes  := TObjectList.Create();
end;

{******************************************************************************}
{* Destroy the foliage class                                                  *}
{******************************************************************************}

destructor  TGDFoliage.Destroy();
begin
  inherited;
  Clear();
  FreeAndNil(FTreeTypes);
  FreeAndNil(FGrassTypes);
  FreeAndNil(FRockTypes);
end;

{******************************************************************************}
{* Init the foliage                                                           *}
{******************************************************************************}

Function TGDFoliage.InitFoliage( aInput : TGDFoliageInput ) : boolean;
var
  iTreeMap, iGrassMap, iRockMap : TBitmap;
  iX, iY : integer;
  iError : String;
begin
  Clear();
  Console.Write('Loading foliage...');
  try
    result := true;

    FGrassCellCountX        := aInput.GrassCellCountX;
    FGrassCellCountY        := aInput.GrassCellCountY;
    FGrassAnimationSpeed    := aInput.GrassAnimationSpeed;
    FGrassAnimationStrength := aInput.GrassAnimationStrength;

    FTreeCount              := aInput.TreeCount;
    FTreeLowerLimit         := aInput.TreeLowerLimit;
    FTreeUpperLimit         := aInput.TreeUpperLimit;
    FTreeAnimationSpeed     := aInput.TreeAnimationSpeed;
    FTreeAnimationStrength  := aInput.TreeAnimationStrength;

    FRockCount              := aInput.RockCount;

    iTreeMap := TBitmap.Create();
    iTreeMap.pixelformat := pf24bit;
    iTreeMap.LoadFromFile( aInput.TreeMap);
    iGrassMap := TBitmap.Create();
    iGrassMap.pixelformat := pf24bit;
    iGrassMap.LoadFromFile( aInput.GrassMap);
    iRockMap := TBitmap.Create();
    iRockMap.pixelformat := pf24bit;
    iRockMap.LoadFromFile( aInput.RockMap);

    if ((iTreeMap.Width mod 2) <> 0) or ((iTreeMap.Height mod 2) <> 0) then
      Raise Exception.Create('Dimensions are incorrect!');

    if (iTreeMap.Width  <> iGrassMap.Width) or
       (iTreeMap.Height <> iGrassMap.Height) or
       (iTreeMap.Width  <> iRockMap.Width) or
       (iTreeMap.Height <> iRockMap.Height) then
      Raise Exception.Create('Dimensions are incorrect!');

    SetLength(TreeMap, iTreeMap.Width);
    SetLength(GrassMap, iTreeMap.Width);
    SetLength(RockMap, iTreeMap.Width);
    for iX := 0 to (iTreeMap.Width-1) do
    begin
      SetLength(TreeMap[iX], iTreeMap.Height);
      SetLength(GrassMap[iX], iTreeMap.Height);
      SetLength(RockMap[iX], iTreeMap.Height);
      for iY := 0 to (iTreeMap.Height-1) do
      begin
          TreeMap[iX,iY]  := iTreeMap.Canvas.Pixels[iX,iY] = clWhite;
          GrassMap[iX,iY] := iGrassMap.Canvas.Pixels[iX,iY] = clWhite;
          RockMap[iX,iY] := iRockMap.Canvas.Pixels[iX,iY] = clWhite;
      end;
    end;
    FreeAndNil(iTreeMap);
    FreeAndNil(iGrassMap);
    FreeAndNil(iRockMap);
  except
    on E: Exception do
    begin
      iError := E.Message;
      result := false;
    end;
  end;

  Console.WriteOkFail(result, iError);
end;

{******************************************************************************}
{* Clear the foliage                                                          *}
{******************************************************************************}

procedure TGDFoliage.Clear();
var
  iX : Integer;
begin
  if (GrassMap <> nil) then
  begin
    for iX := 0 to Length(GrassMap)-1 do
    begin
      Finalize(GrassMap[iX]);
      Finalize(TreeMap[iX]);
      Finalize(RockMap[iX]);
    end;
    Finalize(GrassMap);
    SetLength(GrassMap, 0);
    Finalize(TreeMap);
    SetLength(TreeMap, 0);
    Finalize(RockMap);
    SetLength(RockMap, 0);
  end;

  FGrassTypes.Clear();
  FTreeTypes.Clear();
  FRockTypes.Clear();
  FGrassCellCountX := 0;
  FGrassCellCountY := 0;
end;

{******************************************************************************}
{* Start the rendering of a grasscell                                         *}
{******************************************************************************}

procedure TGDFoliage.StartRenderingGrass( aRenderAttribute : TGDRenderAttribute );
begin
  if Not(aRenderAttribute = RA_NORMAL) then exit;

  if Modes.RenderWireframe then
  begin
    Renderer.SetColor(0,0.25,0,1);
    glDisable(GL_CULL_FACE);
  end
  else
  begin
    glEnable(GL_ALPHA_TEST);
    glAlphaFunc(GL_GREATER, 0.7);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
    glDisable(GL_CULL_FACE);
    Renderer.GrassShader.Enable();
    Renderer.SetJoinedParams(Renderer.GrassShader);
    Renderer.GrassShader.SetInt('T_GRASSTEX', 0);
    Renderer.GrassShader.SetFloat('F_ANIMATION_SPEED', Timing.ElapsedTime / FGrassAnimationSpeed);
    Renderer.GrassShader.SetFloat('F_ANIMATION_STRENGTH', FGrassAnimationStrength);
  end;
end;

{******************************************************************************}
{* End the rendering of a grasscell                                           *}
{******************************************************************************}

procedure TGDFoliage.EndRenderingGrass();
begin
  glDisable(GL_ALPHA_TEST);
  glDisable(GL_BLEND);
  glEnable(GL_CULL_FACE);
end;

{******************************************************************************}
{* Check on the grass map if grass may grow there                           *}
{******************************************************************************}

Function TGDFoliage.CheckGrassMap( aX, aY : Integer ) : Boolean;
begin
  result := GrassMap[aX, aY];
end;

{******************************************************************************}
{* Check on the tree map if tree may grow there                               *}
{******************************************************************************}

Function TGDFoliage.CheckTreeMap( aX, aY : Integer ) : Boolean;
begin
  result := TreeMap[aX, aY];
end;

{******************************************************************************}
{* Check on the rock map if rock may be there                                 *}
{******************************************************************************}

Function TGDFoliage.CheckRockMap( aX, aY : Integer ) : Boolean;
begin
  result := RockMap[aX, aY];
end;

end.
