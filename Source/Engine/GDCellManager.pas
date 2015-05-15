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
unit GDCellManager;

{$MODE Delphi}

{******************************************************************************}
{* This units holds the cell manager                                          *}
{******************************************************************************}

interface

uses
  SysUtils,
  dglOpenGL,
  GDConstants,
  GDBaseCell,
  GDSettings,
  GDGUI,
  GDWaterCell,
  GDFoliage,
  GDGrassCell,
  GDTerrain,
  GDTerrainCell,
  GDTiming,
  GDOctree,
  GDConsole,
  GDTypes,
  GDMeshCell,
  Contnrs,
  GDModes;

type

{******************************************************************************}
{* Cellmanager class                                                         *}
{******************************************************************************}

  TGDCellManager = class
  private
    FTriangleCount : Integer;
    FAllCells         : TObjectList;
    FTerrainCells  : TObjectList;
    FGrassCells    : TObjectList;
    FMeshCells     : TObjectList;
    FWaterCells    : TObjectList;
    FOctree        : TGDOctree;

    procedure GenerateCellsFromTerrain();
    procedure GenerateCellsFromWater();
    procedure GenerateCellsFromFoliage();
  public
    property TriangleCount     : Integer read FTriangleCount write FTriangleCount;
    property AllCells          : TObjectList read FAllCells;
    property TerrainCells      : TObjectList read FTerrainCells;
    property GrassCells        : TObjectList read FGrassCells;
    property MeshCells         : TObjectList read FMeshCells;
    property WaterCells        : TObjectList read FWaterCells;
    property Octree            : TGDOctree read FOctree;

    constructor Create();
    destructor  Destroy(); override;


    procedure GenerateCells();
    procedure Clear();

    procedure DetectVisibleCells();
    procedure RenderVisibleCells( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor );
  end;

var
  CellManager : TGDCellManager;

implementation

uses
  GDMap;

{******************************************************************************}
{* Create the cell manager class                                              *}
{******************************************************************************}

constructor TGDCellManager.Create();
begin
  FAllCells     := TObjectList.Create();
  FTerrainCells := TObjectList.Create(false);
  FGrassCells   := TObjectList.Create(false);
  FMeshCells    := TObjectList.Create(false);
  FWaterCells   := TObjectList.Create(false);
  FOctree       := TGDOctree.Create();
end;

{******************************************************************************}
{* Destroy the cell manager class                                             *}
{******************************************************************************}

destructor  TGDCellManager.Destroy();
begin
  FreeAndNil(FAllCells);
  FreeAndNil(FTerrainCells);
  FreeAndNil(FGrassCells);
  FreeAndNil(FMeshCells);
  FreeAndNil(FWaterCells);
  FreeAndNil(FOctree);
end;

{******************************************************************************}
{* Clear the cell manager                                                     *}
{******************************************************************************}

procedure TGDCellManager.Clear();
begin
  FAllCells.Clear();
  FTerrainCells.Clear();
  FGrassCells.Clear();
  FMeshCells.Clear();
  FWaterCells.Clear();
  FOctree.Clear();
end;

{******************************************************************************}
{* Generate the static terraincell objects from the terrain class             *}
{******************************************************************************}

procedure TGDCellManager.GenerateCellsFromTerrain();
var
 iI, iJ : Integer;
Begin
  If Not(Map.Terrain.TerrainLoaded) then
    exit;

  iI := 1;
  while (iI <= (Map.Terrain.TerrainWidth-CELLSIZE)) do
  begin
    iJ := 1;
    while (iJ <= (Map.Terrain.TerrainHeight-CELLSIZE)) do
    begin
      FAllCells.Add( TGDTerrainCell.Create(iI, iJ, iI+CELLSIZE, iJ+CELLSIZE) );
      iJ := iJ + CELLSIZE
    end;
    iI := iI + CELLSIZE
  end;
End;

{******************************************************************************}
{* Generate the static watercell objects from the water class                 *}
{******************************************************************************}

procedure TGDCellManager.GenerateCellsFromWater();
var
 iI, iJ, iK, iL : Double;
 iCountX, iCountY : Integer;
 iStepX1, iStepY1 : Double;
 iStepX2, iStepY2 : Double;
 iStepU1, iStepV1 : Double;
 iStepU2, iStepV2 : Double;
 iCurrentU1, iCurrentV1 : Double;
 iCurrentU2, iCurrentV2 : Double;
 iCellNotAboveTerrain : Boolean;
 iHeight : Double;
Begin
  If Not(Map.Water.WaterLoaded) then
    exit;

  iStepX1 := (Map.Water.BoundingBox.Max.X + Abs(Map.Water.BoundingBox.Min.X)) / Map.Water.CellCountX;
  iStepY1 := (Map.Water.BoundingBox.Max.Z + Abs(Map.Water.BoundingBox.Min.Z)) / Map.Water.CellCountY;
  iStepX2 := (Map.Water.BoundingBox.Max.X + Abs(Map.Water.BoundingBox.Min.X)) / (Map.Water.CellCountX * Map.Water.CellDivX);
  iStepY2 := (Map.Water.BoundingBox.Max.Z + Abs(Map.Water.BoundingBox.Min.Z)) / (Map.Water.CellCountY * Map.Water.CellDivY);
  iStepU1 := Map.Water.WaterU / (Map.Water.CellCountX * Map.Water.CellDivX);
  iStepV1 := Map.Water.WaterV / (Map.Water.CellCountY * Map.Water.CellDivY);
  iStepU2 := 1 / (Map.Water.CellCountX * Map.Water.CellDivX);
  iStepV2 := 1 / (Map.Water.CellCountY * Map.Water.CellDivY);

  iI := Map.Water.BoundingBox.Min.X-iStepX1;
  iCurrentU1 := 0;
  iCurrentU2 := 0;
  iCountX := 0;
  while (iI < (Map.Water.BoundingBox.Max.X)) do
  begin
    iJ := Map.Water.BoundingBox.Min.Z-iStepY1;
    iCurrentV1 := 0;
    iCurrentV2 := 0;
    iCountY := 0;
    while (iJ < (Map.Water.BoundingBox.Max.Z)) do
    begin
      iCellNotAboveTerrain := true;

      iK := iI;
      while ((iK <= ((iI+iStepX1))) and iCellNotAboveTerrain) do
      begin
        iL := iJ;
        while ((iL <= ((iJ+iStepY1))) and iCellNotAboveTerrain) do
        begin
          iHeight := 0;
          If Map.Terrain.GetHeight(iK, iL, iHeight) then
          begin
            if iHeight <= Map.Water.WaterHeight then
            begin
              FAllCells.Add( TGDWaterCell.Create(iI, iJ, iI+iStepX1, iJ+iStepY1, iCurrentU1, iCurrentV1, iStepU1, iStepV1,
                                                                        iCurrentU2, iCurrentV2, iStepU2, iStepV2) );
              iCellNotAboveTerrain := false;
            end;
          end;
          iL := iL + iStepY2;
        end;
        iK := iK + iStepX2;
      end;

      iCurrentV1 := iCountY * Map.Water.CellDivY * iStepV1;
      iCurrentV2 := iCountY * Map.Water.CellDivY * iStepV2;
      iJ := iJ + iStepY1;
      iCountY := iCountY + 1;
    end;
    iCurrentU1 := iCountX * Map.Water.CellDivX * iStepU1;
    iCurrentU2 := iCountX * Map.Water.CellDivX * iStepU2;
    iI := iI + iStepX1;
    iCountX := iCountX + 1;
  end;
End;

{******************************************************************************}
{* Generate the static grasscell objects from the foliage class               *}
{******************************************************************************}

procedure TGDCellManager.GenerateCellsFromFoliage();
var
 iI, iJ, iTreeCount, iRockCount : Integer;
 iK, iL : Integer;
 iX, iY : Integer;
 iCellHasGrass : boolean;
 iStepX, iStepY : Integer;
 iTreeType : TGDTreeType;
 iRockType : TGDRockType;
 iMeshInput : TGDMeshCellInput;
 iHeight : Double;
 iPos : TGDVector;
label
  RedoRandomTrees;
label
  RedoRandomRocks;
Begin
  Timing.Start();
  iStepX := Round((Map.Terrain.TerrainWidth-1) / Map.Foliage.GrassCellCountX);
  iStepY := Round((Map.Terrain.TerrainHeight-1) / Map.Foliage.GrassCellCountY);

  GUI.LoadingScreen.SetupForUse('Generating foliage...', Round(Map.Terrain.TerrainWidth/iStepX) + Map.Foliage.TreeTypes.Count + Map.Foliage.RockTypes.Count );

  //create grasscells
  iI := 1;
  while (iI <= (Map.Terrain.TerrainWidth-iStepX)) do
  begin
    iJ := 1;
    while (iJ <= (Map.Terrain.TerrainHeight-iStepY)) do
    begin

      iCellHasGrass := false;
      iK := iI-1;
      while ((iK <= (iI-2+iStepX)) and Not(iCellHasGrass)  )  do
      begin
        iL := iJ-1;
        while ((iL <= (iJ-2+iStepY)) and Not(iCellHasGrass)  )  do
        begin
          iCellHasGrass := Map.Foliage.GrassMap[iK,iL];
          iL := iL+1;
        end;
        iK := iK+1;
      end;

      If iCellHasGrass then
      begin
        FAllCells.Add( TGDGrassCell.Create(iI, iJ, iI+iStepX, iJ+iStepY) );
      end;

      iJ := iJ + iStepY;
    end;
    iI := iI + iStepX;
    GUI.LoadingScreen.UpdateBar();
  end;

  //create treecells
  for iI := 0 to Map.Foliage.TreeTypes.Count-1 do
  begin
    iTreeType := Map.Foliage.TreeTypes.Items[iI] as TGDTreeType;
    iTreeCount := Round(Map.Foliage.TreeCount * iTreeType.CoverOfTotal) div 100;
    for iJ := 1 to iTreeCount do
    begin
      RedoRandomTrees:

      iX := Random(Map.Terrain.TerrainWidth-1);
      iY := Random(Map.Terrain.TerrainHeight-1);
      iPos.Reset( Map.Terrain.TerrainPoints[ iX, iY ].FVertex.X + (Map.Terrain.TriangleSize div 2) ,
                  0,
                  Map.Terrain.TerrainPoints[ iX, iY ].FVertex.Z + (Map.Terrain.TriangleSize div 2) );

      if Map.Foliage.CheckTreeMap(iX, iY) and Map.Terrain.GetHeight(iPos.X, iPos.Z, iHeight) then
      begin
        iHeight := iHeight - 200;
        if not((iHeight > Map.Foliage.TreeLowerLimit) and (iHeight < Map.Foliage.TreeUpperLimit)) then
           goto RedoRandomTrees;

        iMeshInput.Model        := iTreeType.Mesh.Name;
        iMeshInput.ModelLOD1    := iTreeType.MeshLOD1.name;
        iMeshInput.ModelLOD2    := iTreeType.MeshLOD2.name;
        iMeshInput.PosX         := iPos.X;
        iMeshInput.PosY         := iHeight;
        iMeshInput.PosZ         := iPos.Z;
        iMeshInput.RotX         := iTreeType.StartRotation.X;
        iMeshInput.RotY         := iTreeType.StartRotation.Y + Random(Round(360));
        iMeshInput.RotZ         := iTreeType.StartRotation.Z;
        iMeshInput.ScaleX       := iTreeType.StartScale + Random(Round(iTreeType.RandomScale));
        iMeshInput.ScaleY       := iTreeType.StartScale + Random(Round(iTreeType.RandomScale));
        iMeshInput.ScaleZ       := iTreeType.StartScale + Random(Round(iTreeType.RandomScale));
        iMeshInput.FadeDistance := 0;
        iMeshInput.FadeScale    := 0;

        FAllCells.Add( TGDMeshCell.Create(iMeshInput) );
      end
      else
        goto RedoRandomTrees;
    end;
    GUI.LoadingScreen.UpdateBar();
  end;

  //create rocks
  for iI := 0 to Map.Foliage.RockTypes.Count-1 do
  begin
    iRockType  := Map.Foliage.RockTypes.Items[iI] as TGDRockType;
    iRockCount := Round(Map.Foliage.RockCount * iRockType.CoverOfTotal) div 100;
    for iJ := 1 to iRockCount do
    begin
      RedoRandomRocks:

      iX := Random(Map.Terrain.TerrainWidth-1);
      iY := Random(Map.Terrain.TerrainHeight-1);
      iPos.Reset( Map.Terrain.TerrainPoints[ iX, iY ].FVertex.X + (Map.Terrain.TriangleSize div 2) ,
                  0,
                  Map.Terrain.TerrainPoints[ iX, iY ].FVertex.Z + (Map.Terrain.TriangleSize div 2) );

      if Map.Foliage.CheckRockMap(iX, iY) and Map.Terrain.GetHeight(iPos.X, iPos.Z, iHeight) then
      begin
        iMeshInput.Model     := iRockType.Mesh.Name;
        iMeshInput.ModelLOD1 := '';
        iMeshInput.ModelLOD2 := '';
        iMeshInput.PosX      := iPos.X;
        iMeshInput.PosY      := iHeight;
        iMeshInput.PosZ      := iPos.Z;
        iMeshInput.RotX      := 0;
        iMeshInput.RotY      := Random(360);
        iMeshInput.RotZ      := 0;
        iMeshInput.ScaleX    := iRockType.StartScale + Random(Round(iRockType.RandomScale));
        iMeshInput.ScaleY    := iRockType.StartScale + Random(Round(iRockType.RandomScale));
        iMeshInput.ScaleZ    := iRockType.StartScale + Random(Round(iRockType.RandomScale));
        iMeshInput.FadeDistance := Settings.FoliageDistance * R_FOLIAGE_DISTANCE_STEP + (R_FOLIAGE_DISTANCE_STEP * 10);
        iMeshInput.FadeScale    := R_FOLIAGE_LOD_DISTANCE;
        FAllCells.Add( TGDMeshCell.Create(iMeshInput) );
      end
      else
        goto RedoRandomRocks;


    end;
    GUI.LoadingScreen.UpdateBar();
  end;
  Timing.Stop();
  Console.Write('......Generated foliage (' + Timing.TimeInSeconds + ' Sec)');
End;

{******************************************************************************}
{* Generate all the cell                                                      *}
{******************************************************************************}

procedure TGDCellManager.GenerateCells();
Begin
  GenerateCellsFromTerrain();
  GenerateCellsFromWater();
  GenerateCellsFromFoliage();
  FOctree.InitOcTree();
End;

{******************************************************************************}
{* Detect visible cells                                                       *}
{******************************************************************************}

procedure TGDCellManager.DetectVisibleCells();
var
 iK : Integer;
Begin
  //clear all the list
  FTerrainCells.Clear();
  FGrassCells.Clear();
  FMeshCells.Clear();
  FWaterCells.Clear();
  Octree.GetVisibleCells();
End;

{******************************************************************************}
{* Render the visible cells                                                   *}
{******************************************************************************}

procedure TGDCellManager.RenderVisibleCells( aRenderAttribute : TGDRenderAttribute;
                                             aRenderFor : TGDRenderFor );
var
  iI : Integer;
  iTerrainCell : TGDTerrainCell;
  iGrassCell   : TGDGrassCell;
  iWaterCell   : TGDWaterCell;
  iMeshCell    : TGDMeshCell;
Begin
  If (aRenderFor = RF_NORMAL) then TriangleCount := 0;

  //render the visible terrain cells
  if Modes.RenderTerrain then
  begin
    Map.Terrain.StartRendering( aRenderAttribute, aRenderFor );
    for iI := 0 to FTerrainCells.Count - 1 do
    begin
      iTerrainCell := TGDTerrainCell(FTerrainCells.Items[ iI ]);
      iTerrainCell.RenderTerrainCell( aRenderAttribute );
      TriangleCount := TriangleCount + TRISINCELL;
    end;
    Map.Terrain.EndRendering();
  end;

  //render the visible grass cells
  if (Modes.RenderGrass) and (aRenderFor <> RF_WATER) and (aRenderFor <> RF_BLOOM) then
  begin
    Map.Foliage.StartRenderingGrass( aRenderAttribute );
    for iI := 0 to FGrassCells.Count - 1 do
    begin
      iGrassCell := TGDGrassCell(FGrassCells.Items[ iI ]);
      iGrassCell.RenderGrassCell( aRenderAttribute );
      TriangleCount := TriangleCount + iGrassCell.TrisCount;
    end;
    Map.Foliage.EndRenderingGrass();
  end;

  //render the visible mesh cells
  if (Modes.RenderModels) then
  begin
    for iI := 0 to FMeshCells.Count - 1 do
    begin
      If (aRenderFor = RF_WATER) and (Settings.WaterReflection = WR_TERRAIN_ONLY) then break;
      iMeshCell := TGDMeshCell(FMeshCells.Items[ iI ]);
      iMeshCell.RenderMeshCell( aRenderAttribute, aRenderFor );
      TriangleCount := TriangleCount + iMeshCell.TriangleCount();
    end;
  end;

  If (Modes.RenderWater) and(aRenderFor <> RF_WATER) then
  begin
    Map.Water.StartRendering( aRenderAttribute, aRenderFor );
    for iI := 0 to FWaterCells.Count - 1 do
    begin
      iWaterCell := TGDWaterCell(FWaterCells.Items[ iI ]);
      iWaterCell.RenderWaterCell( aRenderAttribute );
      TriangleCount := TriangleCount + (Map.Water.CellDivX * Map.Water.CellDivY * 2);
    end;
    Map.Water.EndRendering();
  end;
End;

end.
