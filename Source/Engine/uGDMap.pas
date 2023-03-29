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
unit uGDMap;

{$MODE Delphi}

interface

uses
  SysUtils,
  JsonTools,
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
  iMap, iModels, iModel : TJsonNode;
  iI            : Integer;
  iString       : String;
  iMeshInput    : TGDMeshCellInput;
begin
  GDTiming.Start();
  iMap := TJsonNode.Create();
  Clear();
  GDConsole.Write('......Loading map (' + aFileName + ')');
  GDGUI.LoadingScreen.Start('Loading ' + StringReplace( ExtractFileName(aFileName), ExtractFileExt(aFileName), '',  [rfReplaceAll] ) + '...', 6 );

  //load map json
  iMap.LoadFromFile(aFileName);

  //spawnpoint
  FPlayerStart.Reset(iMap.Find('SpawnPoint/Position').AsString);
  FPlayerViewAngle.Reset(iMap.Find('SpawnPoint/ViewAngle').AsString);

  //directional light
  FLightDirection.Reset(iMap.Find('Light/Direction').AsString);
  FLightAmbient.Reset(iMap.Find('Light/Ambient').AsString);
  FLightDiffuse.Reset(iMap.Find('Light/Diffuse').AsString);

  //init fog
  FFogColor.Reset(iMap.Find('Fog/Color').AsString);
  FFogDistance := GDSettings.ViewDistance;
  FFogMinDistance := (((FFogDistance * R_VIEW_DISTANCE_STEP) / 10) * 5);
  FFogMaxDistance := (((FFogDistance * R_VIEW_DISTANCE_STEP) / 10) * 7.5);
  GDGUI.LoadingScreen.Update();

  //init terrain
  FTerrain.InitTerrain(iMap.Find('Terrain'));
  GDGUI.LoadingScreen.Update();

  //init sky
  Skydome.InitSkyDome(iMap.Find('Sky'), (GDSettings.ViewDistance * R_VIEW_DISTANCE_STEP));
  GDGUI.LoadingScreen.Update();

  //foliage
  Foliage.InitFoliage( iMap.Find('Foliage') );
  GDGUI.LoadingScreen.Update();

  //init water
  Water.InitWater(FTerrain, iMap.Find('Water') );
  GDGUI.LoadingScreen.Update();

  //mesh entities
  iModels := iMap.Find('Models');
  for iI := 0 to iModels.Count-1 do
  begin
    iModel := iModels.Child(iI);
    iMeshInput.Model         := iModel.Find('Model').AsString; 
    iMeshInput.ModelLOD1     := iModel.Find('ModelLOD1').AsString;
    iMeshInput.ModelLOD2     := iModel.Find('ModelLOD2').AsString;
    iMeshInput.Position.Reset(iModel.Find('Position').AsString);
    iMeshInput.Rotation.Reset(iModel.Find('Rotation').AsString);
    iMeshInput.Scale.Reset(iModel.Find('Scale').AsString);
    iMeshInput.FadeDistance  := 0;
    iMeshInput.FadeScale     := 0;
    iMeshInput.CastShadow    := iModel.Find('CastShadow').AsBoolean; 
    iMeshInput.ReceiveShadow := iModel.Find('ReceiveShadow').AsBoolean;
    FCellManager.AddMeshCell( TGDMeshCell.Create(iMeshInput)   );   
  end; 
  GDGUI.LoadingScreen.Update();

  GDTiming.Stop();
  FreeAndNil(iMap);
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
