{*******************************************************************************
*                            Genesis Device Engine                             *
*                   Copyright © 2007-2015 Luuk van Venrooij                    *
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
unit uGDMap;

{$MODE Delphi}

interface

uses
  SysUtils,
  IniFiles,
  dglOpenGL,
  uGDTerrain,
  uGDTypes,
  uGDFoliage,
  uGDConstants,
  uGDSkyDome,
  uGDWater,
  uGDCellManager,
  uGDMeshManager,
  uGDStringparsing,
  uGDMeshCell;

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

    FFogDistance     : Integer;
    FFogColor        : TGDColor;
    FFogMinDistance  : double;
    FFogMaxDistance  : double;

    FTerrain         : TGDTerrain;
    FWater           : TGDWater;
    FFoliage         : TGDFoliage;
    FSkyDome         : TGDSkyDome;

    FCellManager     : TGDCellManager;
    FMeshManager		 : TGDMeshManager;
  public
    property PlayerStart : TGDVector read FPlayerStart;
    property PlayerViewAngle : TGDVector read FPlayerViewAngle;

    property LightDirection : TGDVector read FLightDirection;
    property LightAmbient   : TGDColor read FLightAmbient;
    property LightDiffuse   : TGDColor read FLightDiffuse;

    property FogDistance    : Integer read FFogDistance;
    property FogColor       : TGDColor read FFogColor;
    property FogMinDistance : double read FFogMinDistance;
    property FogMaxDistance : double read FFogMaxDistance;

    property Terrain     : TGDTerrain read FTerrain;
    property Water       : TGDWater read FWater;
    property Foliage     : TGDFoliage read FFoliage;
    property SkyDome     : TGDSkyDome read FSkyDome;
    property MeshManager : TGDMeshManager read FMeshManager;

    constructor Create();
    destructor  Destroy(); override;

    procedure Load( aFileName : String );
    procedure Clear();

    function  ObjectCount(): integer;
    function  TriangleCount(): integer;

    procedure Update();
    procedure DetectVisibleCells();
    procedure RenderVisibleCells(aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor);
  end;

implementation

uses
  uGDEngine;

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
  FCellManager := TGDCellManager.Create();
  FMeshManager := TGDMeshManager.Create();
end;

{******************************************************************************}
{* Destroy map class                                                          *}
{******************************************************************************}

destructor  TGDMap.Destroy();
begin
  inherited;
  Clear();
  FreeAndNil(FTerrain);
  FreeAndNil(FWater);
  FreeAndNil(FFoliage);
  FreeAndNil(FSkyDome);
  FreeAndNil(FCellManager);
  FreeAndNil(FMeshManager);
end;

{******************************************************************************}
{* Init the map                                                               *}
{******************************************************************************}

procedure TGDMap.Load( aFileName : String );
var
  iIniFile : TIniFile;
  iI : Integer;
  iString         : String;
  iMeshInput      : TGDMeshCellInput;
begin
  GDTiming.Start();
  iIniFile := TIniFile.Create(aFileName);
  Clear();
  GDConsole.Write('......Loading map (' + aFileName + ')');
  GDGUI.LoadingScreen.Start('Loading ' + StringReplace( ExtractFileName(aFileName), ExtractFileExt(aFileName), '',  [rfReplaceAll] ) + '...', 6 );

  //spawnpoint
  FPlayerStart := ReadVector(iIniFile, 'SpawnPoint', 'Position');
  FPlayerViewAngle := ReadVector(iIniFile, 'SpawnPoint', 'ViewAngle');

  //directional light
  FLightDirection := ReadVector(iIniFile, 'Light', 'Direction');
  FLightAmbient   := ReadColor(iIniFile, 'Light', 'Ambient');
  FLightDiffuse   := ReadColor(iIniFile, 'Light', 'Diffuse');

  //init fog
  FFogColor    := ReadColor(iIniFile, 'Fog', 'Color');
  FFogDistance := GDSettings.ViewDistance;
  FFogMinDistance := (((FFogDistance * R_VIEW_DISTANCE_STEP) / 10) * 5);
  FFogMaxDistance := (((FFogDistance * R_VIEW_DISTANCE_STEP) / 10) * 7.5);
  GDGUI.LoadingScreen.Update();

  //init terrain
  FTerrain.InitTerrain(iIniFile);
  GDGUI.LoadingScreen.Update();

  //init sky
  Skydome.InitSkyDome(iIniFile, (GDSettings.ViewDistance * R_VIEW_DISTANCE_STEP));
  GDGUI.LoadingScreen.Update();

  //foliage
  Foliage.InitFoliage( iIniFile );
  GDGUI.LoadingScreen.Update();

  //init water
  Water.InitWater(FTerrain, iIniFile );
  GDGUI.LoadingScreen.Update();

  //mesh entities
  iI := 1;
  while (iIniFile.SectionExists('Model' + IntToStr(iI))) do
  begin
    iString := 'Model' + IntToStr(iI);

    iMeshInput.Model         := iIniFile.ReadString( iString, 'Model', '' );
    iMeshInput.ModelLOD1     := iIniFile.ReadString( iString, 'ModelLOD1', '' );
    iMeshInput.ModelLOD2     := iIniFile.ReadString( iString, 'ModelLOD2', '' );
    iMeshInput.Position      := ReadVector(iIniFile, iString, 'Position');
    iMeshInput.Rotation      := ReadVector(iIniFile, iString, 'Rotation');
    iMeshInput.Scale         := ReadVector(iIniFile, iString, 'Scale');
    iMeshInput.FadeDistance  := 0;
    iMeshInput.FadeScale     := 0;
    iMeshInput.CastShadow    := iIniFile.ReadBool( iString, 'CastShadow', false );
    iMeshInput.ReceiveShadow := iIniFile.ReadBool( iString, 'ReceiveShadow', false );

    FCellManager.AddMeshCell( TGDMeshCell.Create(iMeshInput)   );

    iI := iI + 1;
  end;
  GDGUI.LoadingScreen.Update();

  GDTiming.Stop();
  FreeAndNil(iIniFile);
  GDConsole.Write('......Done loading map (' + GDTiming.TimeInSeconds + ' Sec)');

  FCellManager.GenerateCells(FTerrain, FWater, FFoliage);
  FMeshManager.CreateBuffers();

  GDCamera.Position := FPlayerStart.Copy();
  GDCamera.Rotation := FPlayerViewAngle.Copy();
  GDCamera.MouseLook(0,0,1,1,0,False);
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

  FFogMinDistance := (((GDSettings.ViewDistance * R_VIEW_DISTANCE_STEP) / 10) *5);
  FFogMaxDistance := (((GDSettings.ViewDistance * R_VIEW_DISTANCE_STEP) / 10) *8);
  FFogColor.Reset( 0.5, 0.5, 0.5, 1.0 );
  FFogDistance := 5;

  FTerrain.Clear();
  FWater.Clear();
  FFoliage.Clear();
  FSkyDome.Clear();

  FCellManager.Clear();
  FMeshManager.ClearCache();
  FMeshManager.ClearBuffers();
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
  FCellManager.RenderVisibleCells(aRenderAttribute, aRenderFor, FTerrain, FWater, FFoliage, FMeshManager);
end;

end.
