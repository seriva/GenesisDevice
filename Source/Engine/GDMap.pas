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
  GDStringparsing,
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
  iString         : String;
  iMeshInput      : TGDMeshCellInput;
begin
  Timing.Start();
  iIniFile := TIniFile.Create(aFileName);
  Clear();
  Console.Write('......Loading map');
  GUI.LoadingScreen.SetupForUse('Loading ' + StringReplace( ExtractFileName(aFileName) , ExtractFileExt(aFileName), '',  [rfReplaceAll] ) + '...', 9 );

  //spawnpoint
  FPlayerStart := ReadVector(iIniFile, 'SpawnPoint', 'Position');
  FPlayerViewAngle := ReadVector(iIniFile, 'SpawnPoint', 'ViewAngle');

  //bloom
  Renderer.BloomStrengh := iIniFile.ReadFloat( 'Bloom', 'Strengh', 0.5 );

  //directional light
  FLightDirection := ReadVector(iIniFile, 'DirectionalLight', 'Direction');
  FLightAmbient   := ReadColor(iIniFile, 'DirectionalLight', 'Ambient');
  FLightDiffuse   := ReadColor(iIniFile, 'DirectionalLight', 'Diffuse');

  //init fog
  Fog.InitDistanceFog( ReadColor(iIniFile, 'Fog', 'Color'), Settings.ViewDistance );
  Fog.UseDistanceFog();
  GUI.LoadingScreen.UpdateBar();

  //init terrain
  FTerrain.InitTerrain(iIniFile);
  GUI.LoadingScreen.UpdateBar();

  //init sky
  Skydome.InitSkyDome(iIniFile.ReadString( 'Sky', 'TextureMap', 'sky.jpg' ),(Settings.ViewDistance * R_VIEW_DISTANCE_STEP));
  GUI.LoadingScreen.UpdateBar();

  //init water
  Water.InitWater( iIniFile );
  Fog.InitWaterFog( FWater.UnderWaterColor, iIniFile.ReadInteger('Water', 'Visibility', 8 ) );
  GUI.LoadingScreen.UpdateBar();

  //foliage
  Foliage.InitFoliage( iIniFile );
  GUI.LoadingScreen.UpdateBar();

  //grass types
  iI := 1;
  while(iIniFile.SectionExists('GrassType' + IntToStr(iI))) do
  begin
    Foliage.GrassTypes.Add( TGDGrassType.Create(iIniFile, 'GrassType' + IntToStr(iI)));
    iI := iI + 1;
  end;
  GUI.LoadingScreen.UpdateBar();

  //tree types
  iI := 1;
  while(iIniFile.SectionExists('TreeType' + IntToStr(iI))) do
  begin
    Foliage.TreeTypes.Add(TGDMeshType.Create(iIniFile, 'TreeType' + IntToStr(iI)));
    iI := iI + 1;
  end;
  GUI.LoadingScreen.UpdateBar();

  //rock types
  iI := 1;
  while(iIniFile.SectionExists('RockType' + IntToStr(iI))) do
  begin
    Foliage.RockTypes.Add(TGDMeshType.Create(iIniFile, 'RockType' + IntToStr(iI)));
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
    iMeshInput.Position     := ReadVector(iIniFile, iString, 'Position');
    iMeshInput.Rotation     := ReadVector(iIniFile, iString, 'Rotation');
    iMeshInput.Scale        := ReadVector(iIniFile, iString, 'Scale');
    iMeshInput.FadeDistance := 0;
    iMeshInput.FadeScale    := 0;
    FCellManager.AddMeshCell( TGDMeshCell.Create(iMeshInput)   );

    iI := iI + 1;
  end;
  GUI.LoadingScreen.UpdateBar();

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
