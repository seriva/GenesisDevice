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
  GDFog,
  GDRenderer,
  GDTiming,
  GDLighting,
  Contnrs,
  GDModes;

type

{******************************************************************************}
{* Grasstype input record                                                     *}
{******************************************************************************}

  TGDGrassTypesInput = record
    FileName      : String;
    ScaleX        : Double;
    ScaleY        : Double;
    ScaleZ        : Double;
    RandomScaleX  : Double;
    RandomScaleY  : Double;
    RandomScaleZ  : Double;
    CoverOfTotal  : Double;
  end;

{******************************************************************************}
{* Treetype input record                                                      *}
{******************************************************************************}

  TGDTreeTypesInput = record
    MeshName         : String;
    StartScale       : Double;
    StartRotationX   : Double;
    StartRotationY   : Double;
    StartRotationZ   : Double;
    RandomScale      : Double;
    RandomRotationY  : Double;
    CoverOfTotal     : Double;
  end;

{******************************************************************************}
{* Grasstype class                                                            *}
{******************************************************************************}

  TGDGrassType = class(TObject)
  private
    FTexture : TGDTexture;
    FScale   : TGDVector;
    FRandomScale : TGDVector;
    FCoverOfTotal : Double;
  public
    property Texture : TGDTexture read FTexture;
    property Scale : TGDVector read FScale;
    property RandomScale : TGDVector read FRandomScale;
    property CoverOfTotal : Double read FCoverOfTotal;

    constructor Create();
    destructor  Destroy(); override;

    procedure InitGrassType( aInput : TGDGrassTypesInput );
    procedure Clear();
  end;

{******************************************************************************}
{* Treetype class                                                             *}
{******************************************************************************}

  TGDTreeType = class(TObject)
  private
    FMeshName        : String;
    FStartRotation   : TGDVector;
    FStartScale      : Double;
    FRandomScale     : Double;
    FRandomRotationY : Double;
    FCoverOfTotal    : Double;
  public
    property MeshName : String read FMeshName;
    property StartRotation : TGDVector read FStartRotation;
    property StartScale : Double read FStartScale;
    property RandomScale : Double read FRandomScale;
    property RandomRotationY : Double read FRandomRotationY;
    property CoverOfTotal : Double read FCoverOfTotal;

    constructor Create();
    destructor  Destroy(); override;

    procedure InitTreeType( aInput : TGDTreeTypesInput );
    procedure Clear();
  end;

{******************************************************************************}
{* Foliage input record                                                         *}
{******************************************************************************}

  TGDFoliageInput = record
    GrassMap          : String;
    TreeMap           : String;
    GrassCellCountX   : Integer;
    GrassCellCountY   : Integer;
    GrassWaveSpeed    : Double;
    GrassWaveStrength : Double;
    TreeCount         : Integer;
    TreeLowerLimit    : Integer;
    TreeUpperLimit    : Integer;
  end;

{******************************************************************************}
{* Foliage class                                                              *}
{******************************************************************************}

  TGDFoliage = class
  private
    FGrassTypes           : TObjectList;
    FGrassCellCountX      : Integer;
    FGrassCellCountY      : Integer;
    FGrassWaveSpeed       : Double;
    FGrassWaveStrength    : Double;

    FTreeTypes            : TObjectList;
    FTreeCount            : Integer;
    FTreeLowerLimit       : Integer;
    FTreeUpperLimit       : Integer;
  public
    GrassMap : array of array of boolean;
    TreeMap : array of array of boolean;

    property GrassTypes : TObjectList read FGrassTypes;
    property GrassCellCountX : Integer read FGrassCellCountX;
    property GrassCellCountY : Integer read FGrassCellCountY;
    property GrassWaveSpeed    : Double read FGrassWaveSpeed;
    property GrassWaveStrength : Double read FGrassWaveStrength;

    property TreeTypes : TObjectList read FTreeTypes;
    property TreeCount : Integer read FTreeCount;
    property TreeLowerLimit : Integer read FTreeLowerLimit;
    property TreeUpperLimit : Integer read FTreeUpperLimit;

    constructor Create();
    destructor  Destroy(); override;

    Function  InitFoliage( aInput : TGDFoliageInput ) : boolean;
    procedure Clear();

    procedure StartRenderingGrass( aRenderAttribute : TGDRenderAttribute; aAlphaFunction : Double );
    procedure EndRenderingGrass();

    Function CheckTreeMap( aX, aY : Integer ) : Boolean;
    Function CheckGrassMap( aX, aY : Integer ) : Boolean;
  end;

var
  Foliage : TGDFoliage;

implementation

{******************************************************************************}
{* Create the grasstype class                                                 *}
{******************************************************************************}

constructor TGDGrassType.Create();
begin
  FTexture := TGDTexture.Create();
  FScale   := TGDVector.Create(100,100,100);
  FRandomScale := TGDVector.Create();
  FCoverOfTotal := 100;
end;

{******************************************************************************}
{* Destroy the grasstype                                                      *}
{******************************************************************************}

destructor  TGDGrassType.Destroy();
begin
  FreeAndNil(FTexture);
  FreeAndNil(FScale);
  FreeAndNil(FRandomScale);
  inherited
end;

{******************************************************************************}
{* Init the grasstype class                                                   *}
{******************************************************************************}

procedure TGDGrassType.InitGrassType( aInput : TGDGrassTypesInput );
begin
  FTexture.InitTexture( aInput.FileName, TD_HIGH, Settings.TextureFilter );
  FScale.Reset(aInput.ScaleX, aInput.ScaleY, aInput.ScaleZ);
  FRandomScale.Reset(aInput.RandomScaleX, aInput.RandomScaleY, aInput.RandomScaleZ);
  FCoverOfTotal := aInput.CoverOfTotal;
end;

{******************************************************************************}
{* Clear the grasstype                                                        *}
{******************************************************************************}

procedure TGDGrassType.Clear();
begin
  FTexture.Clear();
  FScale.Reset(100,100,100);
  FRandomScale.Reset(0,0,0);
  FCoverOfTotal := 100;
end;

{******************************************************************************}
{* Create the treetype class                                                  *}
{******************************************************************************}

constructor TGDTreeType.Create();
begin
  FMeshName      := '';
  FStartRotation := TGDVector.Create(0,0,0);
  FStartScale    := 100;
  FRandomScale   := 0;
  FCoverOfTotal  := 100;
end;

{******************************************************************************}
{* Destroy the treetype                                                       *}
{******************************************************************************}

destructor  TGDTreeType.Destroy();
begin
  FreeAndNil(FStartRotation);
  inherited
end;

{******************************************************************************}
{* Init the treetype class                                                    *}
{******************************************************************************}

procedure TGDTreeType.InitTreeType( aInput : TGDTreeTypesInput );
begin
  FMeshName := aInput.MeshName;
  FStartRotation.Reset(aInput.StartRotationX, aInput.StartRotationY, aInput.StartRotationZ);
  FStartScale := aInput.StartScale;
  FRandomScale := aInput.RandomScale;
  FRandomRotationY := aInput.RandomRotationY;
  FCoverOfTotal := aInput.CoverOfTotal;
end;

{******************************************************************************}
{* Clear the treetype                                                         *}
{******************************************************************************}

procedure TGDTreeType.Clear();
begin
  FMeshName := '';
  FStartRotation.Reset(0,0,0);
  FStartScale    := 100;
  FRandomScale   := 0;
  FRandomRotationY := 0;
  FCoverOfTotal  := 100;
end;

{******************************************************************************}
{* Create the foliage class                                                   *}
{******************************************************************************}

constructor TGDFoliage.Create();
begin
  FGrassTypes := TObjectList.Create();
  FTreeTypes := TObjectList.Create();
end;

{******************************************************************************}
{* Destroy the foliage class                                                  *}
{******************************************************************************}

destructor  TGDFoliage.Destroy();
begin
  inherited;
  FreeAndNil(FTreeTypes);
  FreeAndNil(FGrassTypes);
end;

{******************************************************************************}
{* Init the foliage                                                           *}
{******************************************************************************}

Function TGDFoliage.InitFoliage( aInput : TGDFoliageInput ) : boolean;
var
  iTreeMap, iGrassMap : TBitmap;
  iX, iY : integer;
  iError : String;
begin
  Clear();
  Console.Write('Loading foliage...');
  try
    result := true;

    FGrassCellCountX   := aInput.GrassCellCountX;
    FGrassCellCountY   := aInput.GrassCellCountY;
    FGrassWaveSpeed    := aInput.GrassWaveSpeed;
    FGrassWaveStrength := aInput.GrassWaveStrength;
    FTreeCount         := aInput.TreeCount;
    FTreeLowerLimit    := aInput.TreeLowerLimit;
    FTreeUpperLimit    := aInput.TreeUpperLimit;

    iTreeMap := TBitmap.Create();
    iTreeMap.pixelformat := pf24bit;
    iTreeMap.LoadFromFile( aInput.TreeMap);
    iGrassMap := TBitmap.Create();
    iGrassMap.pixelformat := pf24bit;
    iGrassMap.LoadFromFile( aInput.GrassMap);

    if ((iTreeMap.Width mod 2) <> 0) or ((iTreeMap.Width mod 2) <> 0) then
      Raise Exception.Create('Dimensions are incorrect!');

    if (iTreeMap.Width  <> iGrassMap.Width) or (iTreeMap.Width <> iGrassMap.Width) then
      Raise Exception.Create('Dimensions are incorrect!');

    SetLength(TreeMap, iTreeMap.Width);
    SetLength(GrassMap, iTreeMap.Width);
    for iX := 0 to (iTreeMap.Width-1) do
    begin
      SetLength(TreeMap[iX], iTreeMap.Height);
      SetLength(GrassMap[iX], iTreeMap.Height);
      for iY := 0 to (iTreeMap.Height-1) do
      begin
          TreeMap[iX,iY]  := iTreeMap.Canvas.Pixels[iX,iY] = clWhite;
          GrassMap[iX,iY] := iGrassMap.Canvas.Pixels[iX,iY] = clWhite;
      end;
    end;
    FreeAndNil(iTreeMap);
    FreeAndNil(iGrassMap);
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
  if (TreeMap <> nil) then
  begin
    for iX := 0 to Length(TreeMap)-1 do
    begin
        Finalize(TreeMap[iX]);
    end;
    Finalize(TreeMap);
    SetLength(TreeMap, 0);
  end;

  if (GrassMap <> nil) then
  begin
    for iX := 0 to Length(GrassMap)-1 do
    begin
        Finalize(GrassMap[iX]);
    end;
    Finalize(GrassMap);
    SetLength(GrassMap, 0);
  end;

  FGrassTypes.Clear();
  FTreeTypes.Clear();
  FGrassCellCountX := 0;
  FGrassCellCountY := 0;
end;

{******************************************************************************}
{* Start the rendering of a grasscell                                         *}
{******************************************************************************}

procedure TGDFoliage.StartRenderingGrass( aRenderAttribute : TGDRenderAttribute; aAlphaFunction : Double );
begin
  if Not(aRenderAttribute = RA_NORMAL) then exit;

  if Modes.RenderWireframe then
  begin
    glColor4f(0,0.25,0,1);
    glDisable(GL_CULL_FACE);
  end
  else
  begin
    glAlphaFunc(GL_GREATER, 0.75 + aAlphaFunction);
    glEnable(GL_ALPHA_TEST);
    glDisable(GL_CULL_FACE);
    Renderer.GrassShader.Enable();
    Renderer.GrassShader.SetFloat3('V_LIGHT_DIR', DirectionalLight.Direction.X,
                                                   DirectionalLight.Direction.Y,
                                                   DirectionalLight.Direction.Z);
    Renderer.GrassShader.SetFloat4('V_LIGHT_AMB', DirectionalLight.Ambient.R,
                                                   DirectionalLight.Ambient.G,
                                                   DirectionalLight.Ambient.B,
                                                   DirectionalLight.Ambient.A);
    Renderer.GrassShader.SetFloat4('V_LIGHT_DIFF', DirectionalLight.Diffuse.R,
                                                    DirectionalLight.Diffuse.G,
                                                    DirectionalLight.Diffuse.B,
                                                    DirectionalLight.Diffuse.A);
    Renderer.GrassShader.SetInt('T_GRASSTEX', 0);
    Renderer.GrassShader.SetFloat('F_WAVE_SPEED', Timing.ElapsedTime / FGrassWaveSpeed);
    Renderer.GrassShader.SetFloat('F_WAVE_STRENGHT', FGrassWaveStrength);
    Renderer.GrassShader.SetFloat('F_MIN_VIEW_DISTANCE', FogManager.FogShader.MinDistance);
    Renderer.GrassShader.SetFloat('F_MAX_VIEW_DISTANCE', FogManager.FogShader.MaxDistance);
    Renderer.GrassShader.SetFloat4('V_FOG_COLOR', FogManager.FogShader.Color.R,
                                  FogManager.FogShader.Color.G, FogManager.FogShader.Color.B,
                                  FogManager.FogShader.Color.A);
    glColor4f(0,0,0,1)
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

end.
