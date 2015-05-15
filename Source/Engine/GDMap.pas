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
unit GDMap;

{$MODE Delphi}

{******************************************************************************}
{* Hold the map class that loads maps                                         *}
{******************************************************************************}

interface

uses
  SysUtils,
  IniFiles,
  dglOpenGL,
  GDTerrain,
  GDTypes,
  GDConsole,
  GDGUI,
  GDFoliage,
  GDSettings,
  GDConstants,
  GDSkyDome,
  GDWater,
  GDFog,
  GDTiming,
  GDCellManager,
  GDMeshCell;

type

{******************************************************************************}
{* Map class                                                                  *}
{******************************************************************************}

  TGDMap = class
  private
    FPlayerStart     : TGDVector;
    FPlayerViewAngle : TGDVector;

    FLightDirection  : TGDVector;
    FLightAmbient    : TGDColor;
    FLightDiffuse    : TGDColor;

    FTerrain         : TGDTerrain;
    FWater           : TGDWater;
    FFoliage         : TGDFoliage;
    FSkyDome         : TGDSkyDome;
    FFog             : TGDFog;

    FCellManager     : TGDCellManager;
  public
    property PlayerStart : TGDVector read FPlayerStart;
    property PlayerViewAngle : TGDVector read FPlayerViewAngle;

    property LightDirection : TGDVector read FLightDirection;
    property LightAmbient   : TGDColor read FLightAmbient;
    property LightDiffuse   : TGDColor read FLightDiffuse;
    property Fog            : TGDFog read FFog;

    property Terrain     : TGDTerrain read FTerrain;
    property Water       : TGDWater read FWater;
    property Foliage     : TGDFoliage read FFoliage;
    property SkyDome     : TGDSkyDome read FSkyDome;

    constructor Create();
    destructor  Destroy(); override;

    function  InitMap( aFileName : String ) : boolean;
    procedure Clear();

    function  ObjectCount(): integer;
    function  TriangleCount(): integer;

    procedure Update();
    procedure DetectVisibleCells();
    procedure RenderVisibleCells(aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor);
  end;

var
  Map : TGDMap;

implementation

uses
  GDRenderer;

{******************************************************************************}
{* Create map class                                                           *}
{******************************************************************************}

constructor TGDMap.Create();
begin
  inherited;
  FTerrain := TGDTerrain.Create();
  FWater   := TGDWater.Create();
  FFoliage := TGDFoliage.Create();
  FSkyDome := TGDSkyDome.Create();
  FFog     := TGDFog.Create();
  FCellManager := TGDCellManager.Create();
end;

{******************************************************************************}
{* Destroy map class                                                          *}
{******************************************************************************}

destructor  TGDMap.Destroy();
begin
  inherited;
  FreeAndNil(FTerrain);
  FreeAndNil(FWater);
  FreeAndNil(FFoliage);
  FreeAndNil(FSkyDome);
  FreeAndNil(FFog);
  FreeAndNil(FCellManager);
end;

{******************************************************************************}
{* Init the map                                                               *}
{******************************************************************************}

function TGDMap.InitMap( aFileName : String ) : boolean;
var
  iIniFile : TIniFile;
  iI : Integer;
  iString : String;
  iError   : String;

  //terrain
  iTerrainInput : TGDTerrainInput;

  //distance fog
  iDFR,iDFG,iDFB,iDFA : Double;

  //skydome
  iSkyDomeTexture : String;

  //water
  iWaterInput : TGDWaterInput;

  //foliage
  iFoliageInput   : TGDFoliageInput;
  iGrassTypeInput : TGDGrassTypeInput;
  iTreeTypeInput  : TGDTreeTypeInput;
  iRockTypeInput  : TGDRockTypeInput;

  //mesh
  iMeshInput : TGDMeshCellInput;
begin
  Timing.Start();
  iIniFile := TIniFile.Create(aFileName);
  Clear();
  Console.Write('......Loading map');
  Console.Write('Loading settings ' + aFileName + '...');
  try
    result := true;

    //spawnpoint
    FPlayerStart.x     := iIniFile.ReadFloat(  'SpawnPoint', 'PosX', 0 );
    FPlayerStart.y     := iIniFile.ReadFloat(  'SpawnPoint', 'PosY', 0 );
    FPlayerStart.z     := iIniFile.ReadFloat(  'SpawnPoint', 'PosZ', 0 );
    FPlayerViewAngle.x := iIniFile.ReadFloat(  'SpawnPoint', 'ViewAngleX', 0 );
    FPlayerViewAngle.Y := iIniFile.ReadFloat(  'SpawnPoint', 'ViewAngleY', 0 );
    FPlayerViewAngle.z := iIniFile.ReadFloat(  'SpawnPoint', 'ViewAngleZ', 0 );

    //terrain
    iTerrainInput.HeightMap      := iIniFile.ReadString( 'Terrain', 'HeightMap', 'heightmap.bmp' );
    iTerrainInput.ColorMap       := iIniFile.ReadString( 'Terrain', 'ColorMap', 'colormaps.bmp');
    iTerrainInput.TriangleSize   := iIniFile.ReadInteger('Terrain', 'TriangleSize', 512 );
    iTerrainInput.HeightScale    := iIniFile.ReadInteger('Terrain', 'HeightScale',  64 );
    iTerrainInput.DetailUV       := iIniFile.ReadInteger('Terrain', 'DetailMapUV', 100 );
    iTerrainInput.CausticUV      := iIniFile.ReadInteger('Terrain', 'CausticUV', 100 );
    iTerrainInput.Detail1        := iIniFile.ReadString( 'Terrain', 'DetailMap1', 'detailmap1.jpg');
    iTerrainInput.Detail2        := iIniFile.ReadString( 'Terrain', 'DetailMap2', 'detailmap2.jpg');
    iTerrainInput.Detail3        := iIniFile.ReadString( 'Terrain', 'DetailMap3', 'detailmap3.jpg');
    iTerrainInput.DetailLookup   := iIniFile.ReadString( 'Terrain', 'DetailDistribution', 'detaillookup.jpg');

    //distance fog
    iDFR := iIniFile.ReadFloat( 'Fog', 'DFR', 0.5 );
    iDFG := iIniFile.ReadFloat( 'Fog', 'DFG', 0.5 );
    iDFB := iIniFile.ReadFloat( 'Fog', 'DFB', 0.5 );
    iDFA := iIniFile.ReadFloat( 'Fog', 'DFA', 0.5 );

    //sky
    iSkyDomeTexture  := iIniFile.ReadString( 'Sky', 'TextureMap', 'sky.jpg' );

    //water
    iWaterInput.NumberOfWaterText := iIniFile.ReadInteger('Water', 'WaterTexturesCount', 10 );
    iWaterInput.WaterPath      := iIniFile.ReadString( 'Water', 'WaterMapPath', 'textures\water\');
    iWaterInput.WaterPrefix    := iIniFile.ReadString( 'Water', 'WaterMapPrefix', 'water');
    iWaterInput.WaterExtension := iIniFile.ReadString( 'Water', 'WaterMapExtension', 'bmp');
    iWaterInput.WaterDepthMap  := iIniFile.ReadString( 'Water', 'DepthMap', 'bmp');
    iWaterInput.Height := iIniFile.ReadFloat( 'Water', 'Height', 0 );
    iWaterInput.X1     := iIniFile.ReadFloat( 'Water', 'X1', 0 );
    iWaterInput.Z1     := iIniFile.ReadFloat( 'Water', 'Z1', 0 );
    iWaterInput.X2     := iIniFile.ReadFloat( 'Water', 'X2', 0 );
    iWaterInput.Z2     := iIniFile.ReadFloat( 'Water', 'Z2', 0 );
    iWaterInput.U      := iIniFile.ReadFloat( 'Water', 'WaterU', 1 );
    iWaterInput.V      := iIniFile.ReadFloat( 'Water', 'WaterV', 1 );
    iWaterInput.CellCountX   := iIniFile.ReadInteger('Water', 'CellCountX', 1 );
    iWaterInput.CellCountY   := iIniFile.ReadInteger('Water', 'CellCountY', 1 );
    iWaterInput.CellDivX     := iIniFile.ReadInteger('Water', 'CellDivX', 1 );
    iWaterInput.CellDivY     := iIniFile.ReadInteger('Water', 'CellDivY', 1 );
    iWaterInput.UnderWaterColorR := iIniFile.ReadFloat( 'Water', 'UnderWaterColorR', 1 );
    iWaterInput.UnderWaterColorG := iIniFile.ReadFloat( 'Water', 'UnderWaterColorG', 1 );
    iWaterInput.UnderWaterColorB := iIniFile.ReadFloat( 'Water', 'UnderWaterColorB', 1 );
    iWaterInput.UnderWaterColorA := iIniFile.ReadFloat( 'Water', 'UnderWaterColorA', 1 );
    iWaterInput.WaterColorCorrectionR := iIniFile.ReadFloat( 'Water', 'WaterColorCorrectionR', 1 );
    iWaterInput.WaterColorCorrectionG := iIniFile.ReadFloat( 'Water', 'WaterColorCorrectionG', 1 );
    iWaterInput.WaterColorCorrectionB := iIniFile.ReadFloat( 'Water', 'WaterColorCorrectionB', 1 );
    iWaterInput.WaterColorCorrectionA := iIniFile.ReadFloat( 'Water', 'WaterColorCorrectionA', 1 );
    iWaterInput.WaveSpeed    := iIniFile.ReadFloat( 'Water', 'WaveSpeed', 1000 );
    iWaterInput.WaveStrength := iIniFile.ReadFloat( 'Water', 'WaveStrength', 18 );
    iWaterInput.Visibility   := iIniFile.ReadInteger('Water', 'Visibility', 8 );
    iWaterInput.NumberOfCausticsText     := iIniFile.ReadInteger('Water', 'CausticTexturesCount', 10 );
    iWaterInput.CausticsPath      := iIniFile.ReadString( 'Water', 'CausticsMapPath', 'textures\water\');
    iWaterInput.CausticsPrefix    := iIniFile.ReadString( 'Water', 'CausticsMapPrefix', 'caust');
    iWaterInput.CausticsExtension := iIniFile.ReadString( 'Water', 'CausticsMapExtension', 'bmp');

    //bloom
    Renderer.BloomStrengh := iIniFile.ReadFloat( 'Bloom', 'Strengh', 0.5 );

    //foliage
    iFoliageInput.GrassMap              := iIniFile.ReadString( 'Foliage', 'GrassMap', 'grassmap.bmp' );
    iFoliageInput.GrassCellCountX        := iIniFile.ReadInteger( 'Foliage', 'GrassCellCountX', 1 );
    iFoliageInput.GrassCellCountY        := iIniFile.ReadInteger( 'Foliage', 'GrassCellCountY', 1 );
    iFoliageInput.GrassAnimationSpeed    := iIniFile.ReadFloat( 'Foliage', 'GrassAnimationSpeed', 1000 );
    iFoliageInput.GrassAnimationStrength := iIniFile.ReadFloat( 'Foliage', 'GrassAnimationStrength', 5 );
    iFoliageInput.TreeMap                := iIniFile.ReadString( 'Foliage', 'TreeMap', 'treemap.bmp' );
    iFoliageInput.TreeCount              := iIniFile.ReadInteger( 'Foliage', 'TreeCount', 0 );
    iFoliageInput.TreeLowerLimit         := iIniFile.ReadInteger( 'Foliage', 'TreeLowerLimit', 0 );
    iFoliageInput.TreeUpperLimit         := iIniFile.ReadInteger( 'Foliage', 'TreeUpperLimit', 0 );
    iFoliageInput.TreeAnimationSpeed     := iIniFile.ReadFloat( 'Foliage', 'TreeAnimationSpeed', 1000 );
    iFoliageInput.TreeAnimationStrength  := iIniFile.ReadFloat( 'Foliage', 'TreeAnimationStrength', 5 );
    iFoliageInput.RockMap                := iIniFile.ReadString( 'Foliage', 'RockMap', 'rockmap.bmp' );
    iFoliageInput.RockCount              := iIniFile.ReadInteger( 'Foliage', 'RockCount', 0 );

    //directional light
    FLightDirection.X := iIniFile.ReadFloat('DirectionalLight', 'DirX', -1.0);
    FLightDirection.Y := iIniFile.ReadFloat('DirectionalLight', 'DirY', -1.0);
    FLightDirection.Z := iIniFile.ReadFloat('DirectionalLight', 'DirZ', -1.0);
    FLightAmbient.R   := iIniFile.ReadFloat('DirectionalLight', 'AmbientR', 1.0);
    FLightAmbient.G   := iIniFile.ReadFloat('DirectionalLight', 'AmbientG', 1.0);
    FLightAmbient.B   := iIniFile.ReadFloat('DirectionalLight', 'AmbientB', 1.0);
    FLightAmbient.A   := 1;
    FLightDiffuse.R   := iIniFile.ReadFloat('DirectionalLight', 'DiffuseR', 1.0);
    FLightDiffuse.G   := iIniFile.ReadFloat('DirectionalLight', 'DiffuseB', 1.0);
    FLightDiffuse.B   := iIniFile.ReadFloat('DirectionalLight', 'DiffuseG', 1.0);
    FLightDiffuse.A   := 1;

    GUI.LoadingScreen.SetupForUse('Loading ' + StringReplace( ExtractFileName(aFileName) , ExtractFileExt(aFileName), '',  [rfReplaceAll] ) + '...',12 );
    GUI.LoadingScreen.UpdateBar();
  except
    on E: Exception do
    begin
      iError := E.Message;
      result := false;
    end;
  end;

  Console.WriteOkFail(result, iError);

  If result then
  begin

    //init terrain
    FTerrain.InitTerrain(iTerrainInput);

    //init fog
    Fog.InitDistanceFog( iDFR,iDFG,iDFB,iDFA, Settings.ViewDistance );
    Fog.UseDistanceFog();

    //init sky
    Skydome.InitSkyDome(iSkyDomeTexture,(Settings.ViewDistance * R_VIEW_DISTANCE_STEP));
    GUI.LoadingScreen.UpdateBar();

    //init water
    Water.InitWater( iWaterInput );
    Fog.InitWaterFog( iWaterInput.UnderWaterColorR,iWaterInput.UnderWaterColorG,iWaterInput.UnderWaterColorB,iWaterInput.UnderWaterColorA, iWaterInput.Visibility );
    GUI.LoadingScreen.UpdateBar();

    //foliage
    Foliage.InitFoliage( iFoliageInput );
    GUI.LoadingScreen.UpdateBar();

    //grass types
    iI := 1;
    while(iIniFile.SectionExists('GrassType' + IntToStr(iI))) do
    begin
      iString := 'GrassType' + IntToStr(iI);

      iGrassTypeInput.Texture      := iIniFile.ReadString( iString, 'Texture', '');
      iGrassTypeInput.ScaleX       := iIniFile.ReadFloat( iString, 'ScaleX', 100 );
      iGrassTypeInput.ScaleY       := iIniFile.ReadFloat( iString, 'ScaleY', 100 );
      iGrassTypeInput.ScaleZ       := iIniFile.ReadFloat( iString, 'ScaleZ', 100 );
      iGrassTypeInput.RandomScaleX := iIniFile.ReadFloat( iString, 'RandomScaleX', 100 );
      iGrassTypeInput.RandomScaleY := iIniFile.ReadFloat( iString, 'RandomScaleY', 100 );
      iGrassTypeInput.RandomScaleZ := iIniFile.ReadFloat( iString, 'RandomScaleZ', 100 );
      iGrassTypeInput.CoverOfTotal := iIniFile.ReadFloat( iString, 'CoverOfTotal', 100 );
      Foliage.GrassTypes.Add( TGDGrassType.Create(iGrassTypeInput));

      iI := iI + 1;
    end;
    GUI.LoadingScreen.UpdateBar();

    //tree types
    iI := 1;
    while(iIniFile.SectionExists('TreeType' + IntToStr(iI))) do
    begin
      iString := 'TreeType' + IntToStr(iI);

      iTreeTypeInput.Model           := iIniFile.ReadString( iString, 'Model', '');
      iTreeTypeInput.ModelLOD1       := iIniFile.ReadString( iString, 'ModelLOD1', '' );
      iTreeTypeInput.ModelLOD2        := iIniFile.ReadString( iString, 'ModelLOD2', '' );
      iTreeTypeInput.StartScale      := iIniFile.ReadFloat( iString, 'StartScale', 100 );
      iTreeTypeInput.StartRotationX  := iIniFile.ReadFloat( iString, 'StartRotationX', 0 );
      iTreeTypeInput.StartRotationY  := iIniFile.ReadFloat( iString, 'StartRotationY', 0 );
      iTreeTypeInput.StartRotationZ  := iIniFile.ReadFloat( iString, 'StartRotationZ', 0 );
      iTreeTypeInput.RandomScale     := iIniFile.ReadFloat( iString, 'RandomScale', 0 );
      iTreeTypeInput.CoverOfTotal    := iIniFile.ReadFloat( iString, 'CoverOfTotal', 100 );
      Foliage.TreeTypes.Add(TGDTreeType.Create(iTreeTypeInput));

      iI := iI + 1;
    end;
    GUI.LoadingScreen.UpdateBar();

    //rock types
    iI := 1;
    while(iIniFile.SectionExists('RockType' + IntToStr(iI))) do
    begin
      iString := 'RockType' + IntToStr(iI);

      iRockTypeInput.Model           := iIniFile.ReadString( iString, 'Model', '');
      iRockTypeInput.StartScale      := iIniFile.ReadFloat( iString, 'StartScale', 100 );
      iRockTypeInput.RandomScale     := iIniFile.ReadFloat( iString, 'RandomScale', 0 );
      iRockTypeInput.CoverOfTotal    := iIniFile.ReadFloat( iString, 'CoverOfTotal', 100 );
      Foliage.RockTypes.Add(TGDRockType.Create(iRockTypeInput));

      iI := iI + 1;
    end;
    GUI.LoadingScreen.UpdateBar();

    //mesh entities
    iI := 1;
    while (iIniFile.SectionExists('Model' + IntToStr(iI))) do
    begin
      iString := 'Model' + IntToStr(iI);

      iMeshInput.Model        := iIniFile.ReadString( iString, 'Model', '' );
      iMeshInput.ModelLOD1    := iIniFile.ReadString( iString, 'ModelLOD1', '' );
      iMeshInput.ModelLOD2    := iIniFile.ReadString( iString, 'ModelLOD2', '' );
      iMeshInput.PosX         := iIniFile.ReadFloat( iString, 'PositionX', 0);
      iMeshInput.PosY         := iIniFile.ReadFloat( iString, 'PositionY', 0);
      iMeshInput.PosZ         := iIniFile.ReadFloat( iString, 'PositionZ', 0);
      iMeshInput.RotX         := iIniFile.ReadFloat( iString, 'RotationX', 0);
      iMeshInput.RotY         := iIniFile.ReadFloat( iString, 'RotationY', 0);
      iMeshInput.RotZ         := iIniFile.ReadFloat( iString, 'RotationZ', 0);
      iMeshInput.ScaleX       := iIniFile.ReadFloat( iString, 'ScaleX', 100);
      iMeshInput.ScaleY       := iIniFile.ReadFloat( iString, 'ScaleY', 100);
      iMeshInput.ScaleZ       := iIniFile.ReadFloat( iString, 'ScaleZ', 100);
      iMeshInput.FadeDistance := 0;
      iMeshInput.FadeScale    := 0;
      FCellManager.AddMeshCell( TGDMeshCell.Create(iMeshInput)   );

      iI := iI + 1;
    end;
    GUI.LoadingScreen.UpdateBar();
  end;
  Timing.Stop();
  FreeAndNil(iIniFile);
  Console.Write('......Done loading map (' + Timing.TimeInSeconds + ' Sec)');

  FCellManager.GenerateCells(FTerrain, FWater, FFoliage);
end;

{******************************************************************************}
{* Clear the map                                                              *}
{******************************************************************************}

procedure TGDMap.Clear();
begin
  FPlayerStart.Reset(0,0,0);
  FPlayerViewAngle.Reset(0,0,0);
  FLightDirection.Reset(-1,-1,-1);
  FLightAmbient.Reset(1, 1, 1, 1);
  FLightDiffuse.Reset(1, 1, 1, 1);

  FTerrain.Clear();
  FWater.Clear();
  FFoliage.Clear();
  FSkyDome.Clear();
  FFog.Clear();

  FCellManager.Clear();
end;

{******************************************************************************}
{* Get visible object count                                                   *}
{******************************************************************************}

function TGDMap.ObjectCount(): integer;
begin
  result := FCellManager.ObjectCount();
end;

{******************************************************************************}
{* Get visible triangle count                                                 *}
{******************************************************************************}

function TGDMap.TriangleCount(): integer;
begin
  result := FCellManager.TriangleCount + FSkyDome.TriangleCount;
end;

{******************************************************************************}
{* Update the map                                                             *}
{******************************************************************************}

procedure TGDMap.Update();
begin
  FWater.Update();
end;

{******************************************************************************}
{* Detect visible cells                                                       *}
{******************************************************************************}

procedure TGDMap.DetectVisibleCells();
begin
  FCellManager.DetectVisibleCells();
end;

{******************************************************************************}
{* Render visible cells                                                       *}
{******************************************************************************}

procedure TGDMap.RenderVisibleCells(aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor);
begin
  FCellManager.RenderVisibleCells(aRenderAttribute, aRenderFor,
                                  FTerrain, FWater, FFoliage);
end;

end.
