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
  GDSettings,
  GDGUI,
  GDWater,
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
{* Cellmanager class                                                          *}
{******************************************************************************}

  TGDCellManager = class
  private
    FTriangleCount       : Integer;
    FCells               : TObjectList;
    FVisibleTerrainCells : TObjectList;
    FVisibleGrassCells   : TObjectList;
    FVisibleMeshCells    : TObjectList;
    FVisibleWaterCells   : TObjectList;
    FOctree              : TGDOctree;

    procedure GenerateTerrainCells(aTerrain : TGDTerrain);
    procedure GenerateWaterCells(aWater : TGDWater; aTerrain : TGDTerrain);
    procedure GenerateFoliageCells(aFoliage : TGDFoliage; aTerrain : TGDTerrain; aWater : TGDWater);
  public
    property TriangleCount : Integer read FTriangleCount write FTriangleCount;
    function ObjectCount(): Integer;

    constructor Create();
    destructor  Destroy(); override;

    procedure AddMeshCell(aMeshCell : TGDMeshCell);
    procedure GenerateCells(aTerrain : TGDTerrain; aWater : TGDWater; aFoliage : TGDFoliage);
    procedure Clear();

    procedure DetectVisibleCells();
    procedure RenderVisibleCells( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor;
                                  aTerrain : TGDTerrain; aWater : TGDWater; aFoliage : TGDFoliage);
  end;

implementation

{******************************************************************************}
{* Create the cell manager class                                              *}
{******************************************************************************}

constructor TGDCellManager.Create();
begin
  FCells               := TObjectList.Create();
  FVisibleTerrainCells := TObjectList.Create(false);
  FVisibleGrassCells   := TObjectList.Create(false);
  FVisibleMeshCells    := TObjectList.Create(false);
  FVisibleWaterCells   := TObjectList.Create(false);
  FOctree              := TGDOctree.Create();
end;

{******************************************************************************}
{* Destroy the cell manager class                                             *}
{******************************************************************************}

destructor  TGDCellManager.Destroy();
begin
  FreeAndNil(FCells);
  FreeAndNil(FVisibleTerrainCells);
  FreeAndNil(FVisibleGrassCells);
  FreeAndNil(FVisibleMeshCells);
  FreeAndNil(FVisibleWaterCells);
  FreeAndNil(FOctree);
end;

{******************************************************************************}
{* Clear the cell manager                                                     *}
{******************************************************************************}

procedure TGDCellManager.Clear();
begin
  FCells.Clear();
  FVisibleTerrainCells.Clear();
  FVisibleGrassCells.Clear();
  FVisibleMeshCells.Clear();
  FVisibleWaterCells.Clear();
  FOctree.Clear();
end;

{******************************************************************************}
{* Get the visible object count                                               *}
{******************************************************************************}

function TGDCellManager.ObjectCount(): Integer;
begin
  result := FVisibleWaterCells.Count + FVisibleTerrainCells.Count +
            FVisibleMeshCells.Count + FVisibleGrassCells.Count
end;

{******************************************************************************}
{* Generate the static terraincell objects from the terrain class             *}
{******************************************************************************}

procedure TGDCellManager.GenerateTerrainCells(aTerrain : TGDTerrain);
var
 iI, iJ : Integer;
Begin
  If Not(aTerrain.TerrainLoaded) then
    exit;

  iI := 1;
  while (iI <= (aTerrain.TerrainWidth-CELLSIZE)) do
  begin
    iJ := 1;
    while (iJ <= (aTerrain.TerrainHeight-CELLSIZE)) do
    begin
      FCells.Add(TGDTerrainCell.Create(aTerrain, iI, iJ, iI+CELLSIZE, iJ+CELLSIZE));
      iJ := iJ + CELLSIZE
    end;
    iI := iI + CELLSIZE
  end;
End;

{******************************************************************************}
{* Generate the static watercell objects from the water class                 *}
{******************************************************************************}

procedure TGDCellManager.GenerateWaterCells(aWater : TGDWater; aTerrain : TGDTerrain);
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
  If Not(aWater.WaterLoaded) then
    exit;

  iStepX1 := (aWater.BoundingBox.Max.X + Abs(aWater.BoundingBox.Min.X)) / aWater.CellCountX;
  iStepY1 := (aWater.BoundingBox.Max.Z + Abs(aWater.BoundingBox.Min.Z)) / aWater.CellCountY;
  iStepX2 := (aWater.BoundingBox.Max.X + Abs(aWater.BoundingBox.Min.X)) / (aWater.CellCountX * aWater.CellDivX);
  iStepY2 := (aWater.BoundingBox.Max.Z + Abs(aWater.BoundingBox.Min.Z)) / (aWater.CellCountY * aWater.CellDivY);
  iStepU1 := aWater.WaterU / (aWater.CellCountX * aWater.CellDivX);
  iStepV1 := aWater.WaterV / (aWater.CellCountY * aWater.CellDivY);
  iStepU2 := 1 / (aWater.CellCountX * aWater.CellDivX);
  iStepV2 := 1 / (aWater.CellCountY * aWater.CellDivY);

  iI := aWater.BoundingBox.Min.X-iStepX1;
  iCurrentU1 := 0;
  iCurrentU2 := 0;
  iCountX := 0;
  while (iI < (aWater.BoundingBox.Max.X)) do
  begin
    iJ := aWater.BoundingBox.Min.Z-iStepY1;
    iCurrentV1 := 0;
    iCurrentV2 := 0;
    iCountY := 0;
    while (iJ < (aWater.BoundingBox.Max.Z)) do
    begin
      iCellNotAboveTerrain := true;

      iK := iI;
      while ((iK <= ((iI+iStepX1))) and iCellNotAboveTerrain) do
      begin
        iL := iJ;
        while ((iL <= ((iJ+iStepY1))) and iCellNotAboveTerrain) do
        begin
          iHeight := 0;
          If aTerrain.GetHeight(iK, iL, iHeight) then
          begin
            if iHeight <= aWater.WaterHeight then
            begin
              FCells.Add( TGDWaterCell.Create(aWater, iI, iJ, iI+iStepX1, iJ+iStepY1, iCurrentU1, iCurrentV1, iStepU1, iStepV1,
                                                                        iCurrentU2, iCurrentV2, iStepU2, iStepV2) );
              iCellNotAboveTerrain := false;
            end;
          end;
          iL := iL + iStepY2;
        end;
        iK := iK + iStepX2;
      end;

      iCurrentV1 := iCountY * aWater.CellDivY * iStepV1;
      iCurrentV2 := iCountY * aWater.CellDivY * iStepV2;
      iJ := iJ + iStepY1;
      iCountY := iCountY + 1;
    end;
    iCurrentU1 := iCountX * aWater.CellDivX * iStepU1;
    iCurrentU2 := iCountX * aWater.CellDivX * iStepU2;
    iI := iI + iStepX1;
    iCountX := iCountX + 1;
  end;
End;

{******************************************************************************}
{* Generate the static grasscell objects from the foliage class               *}
{******************************************************************************}

procedure TGDCellManager.GenerateFoliageCells(aFoliage : TGDFoliage; aTerrain : TGDTerrain; aWater : TGDWater);
var
 iI, iJ, iTreeCount, iRockCount : Integer;
 iK, iL : Integer;
 iX, iY : Integer;
 iCellHasGrass : boolean;
 iStepX, iStepY : Integer;
 iMeshType : TGDMeshType;
 iMeshInput : TGDMeshCellInput;
 iHeight : Double;
 iPos, iRot : TGDVector;
label
  RedoRandomTrees;
label
  RedoRandomRocks;
Begin
  Timing.Start();
  iStepX := Round((aTerrain.TerrainWidth-1) / aFoliage.GrassCellCountX);
  iStepY := Round((aTerrain.TerrainHeight-1) / aFoliage.GrassCellCountY);

  GUI.LoadingScreen.SetupForUse('Generating foliage...', Round(aTerrain.TerrainWidth/iStepX) + aFoliage.TreeTypes.Count + aFoliage.RockTypes.Count );

  //create grasscells
  iI := 1;
  while (iI <= (aTerrain.TerrainWidth-iStepX)) do
  begin
    iJ := 1;
    while (iJ <= (aTerrain.TerrainHeight-iStepY)) do
    begin

      iCellHasGrass := false;
      iK := iI-1;
      while ((iK <= (iI-2+iStepX)) and Not(iCellHasGrass)  )  do
      begin
        iL := iJ-1;
        while ((iL <= (iJ-2+iStepY)) and Not(iCellHasGrass)  )  do
        begin
          iCellHasGrass := aFoliage.GrassMap[iK,iL];
          iL := iL+1;
        end;
        iK := iK+1;
      end;

      If iCellHasGrass then
      begin
        FCells.Add(TGDGrassCell.Create(aTerrain, aFoliage, aWater, iI, iJ, iI+iStepX, iJ+iStepY) );
      end;

      iJ := iJ + iStepY;
    end;
    iI := iI + iStepX;
    GUI.LoadingScreen.UpdateBar();
  end;

  //create treecells
  for iI := 0 to aFoliage.TreeTypes.Count-1 do
  begin
    iMeshType := aFoliage.TreeTypes.Items[iI] as TGDMeshType;
    iTreeCount := Round(aFoliage.TreeCount * iMeshType.CoverOfTotal) div 100;
    for iJ := 1 to iTreeCount do
    begin
      RedoRandomTrees:

      iX := Random(aTerrain.TerrainWidth-1);
      iY := Random(aTerrain.TerrainHeight-1);
      iPos.Reset( aTerrain.TerrainPoints[ iX, iY ].Vertex.X + (aTerrain.TriangleSize div 2) ,
                  0,
                  aTerrain.TerrainPoints[ iX, iY ].Vertex.Z + (aTerrain.TriangleSize div 2) );

      if aFoliage.CheckTreeMap(iX, iY) and aTerrain.GetHeight(iPos.X, iPos.Z, iHeight) then
      begin
        iHeight := iHeight - 200;
        if not((iHeight > aFoliage.TreeLowerLimit) and (iHeight < aFoliage.TreeUpperLimit)) then
           goto RedoRandomTrees;

        iMeshInput.Model        := iMeshType.Mesh.Name;
        iMeshInput.ModelLOD1    := iMeshType.MeshLOD1.name;
        iMeshInput.ModelLOD2    := iMeshType.MeshLOD2.name;
        iMeshInput.Position.X   := iPos.X;
        iMeshInput.Position.Y   := iHeight;
        iMeshInput.Position.Z   := iPos.Z;
        iMeshInput.Rotation.X   := iMeshType.StartRotation.X;
        iMeshInput.Rotation.Y   := iMeshType.StartRotation.Y + Random(Round(360));
        iMeshInput.Rotation.Z   := iMeshType.StartRotation.Z;
        iMeshInput.Scale.X      := iMeshType.Scale + Random(Round(iMeshType.RandomScale));
        iMeshInput.Scale.Y      := iMeshType.Scale + Random(Round(iMeshType.RandomScale));
        iMeshInput.Scale.Z      := iMeshType.Scale + Random(Round(iMeshType.RandomScale));
        iMeshInput.FadeDistance := 0;
        iMeshInput.FadeScale    := 0;

        FCells.Add( TGDMeshCell.Create(iMeshInput) );
      end
      else
        goto RedoRandomTrees;
    end;
    GUI.LoadingScreen.UpdateBar();
  end;

  //create rocks
  for iI := 0 to aFoliage.RockTypes.Count-1 do
  begin
    iMeshType  := aFoliage.RockTypes.Items[iI] as TGDMeshType;
    iRockCount := Round(aFoliage.RockCount * iMeshType.CoverOfTotal) div 100;
    for iJ := 1 to iRockCount do
    begin
      RedoRandomRocks:

      iX := Random(aTerrain.TerrainWidth-1);
      iY := Random(aTerrain.TerrainHeight-1);
      iPos.Reset( aTerrain.TerrainPoints[ iX, iY ].Vertex.X + (aTerrain.TriangleSize div 2) ,
                  0,
                  aTerrain.TerrainPoints[ iX, iY ].Vertex.Z + (aTerrain.TriangleSize div 2) );

      if aFoliage.CheckRockMap(iX, iY) and aTerrain.GetHeight(iPos.X, iPos.Z, iHeight) then
      begin
        aTerrain.GetRotation(iPos.X, iPos.Z, iRot );
        iMeshInput.Model          := iMeshType.Mesh.Name;
        iMeshInput.ModelLOD1    := '';
        iMeshInput.ModelLOD2    := '';
        iMeshInput.Position.X   := iPos.X;
        iMeshInput.Position.Y   := iHeight;
        iMeshInput.Position.Z   := iPos.Z;
        iMeshInput.Rotation.X   := iRot.x;
        iMeshInput.Rotation.Y   := iRot.y;
        iMeshInput.Rotation.Z   := iRot.z;
        iMeshInput.Scale.X      := iMeshType.Scale + Random(Round(iMeshType.RandomScale));
        iMeshInput.Scale.Y      := iMeshType.Scale + Random(Round(iMeshType.RandomScale));
        iMeshInput.Scale.Z      := iMeshType.Scale + Random(Round(iMeshType.RandomScale));
        iMeshInput.FadeDistance := Settings.FoliageDistance * R_FOLIAGE_DISTANCE_STEP + (R_FOLIAGE_DISTANCE_STEP * 10);
        iMeshInput.FadeScale    := R_FOLIAGE_LOD_DISTANCE;
        FCells.Add( TGDMeshCell.Create(iMeshInput) );
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

procedure TGDCellManager.GenerateCells(aTerrain : TGDTerrain; aWater : TGDWater; aFoliage : TGDFoliage);
Begin
  GenerateTerrainCells(aTerrain);
  GenerateWaterCells(aWater, aTerrain);
  GenerateFoliageCells(aFoliage, aTerrain, aWater);
  FOctree.InitOcTree(FCells);
End;

{******************************************************************************}
{* Add a meshcell                                                             *}
{******************************************************************************}

procedure TGDCellManager.AddMeshCell(aMeshCell : TGDMeshCell);
begin
  FCells.Add(aMeshCell);
end;

{******************************************************************************}
{* Detect visible cells                                                       *}
{******************************************************************************}

procedure TGDCellManager.DetectVisibleCells();
var
  iVisibilityQuery : TGDVisibilityQuery;
Begin
  //clear all the list
  FVisibleTerrainCells.Clear();
  FVisibleGrassCells.Clear();
  FVisibleMeshCells.Clear();
  FVisibleWaterCells.Clear();

  //Get visible cell with the octree.
  iVisibilityQuery.Cells               := FCells;
  iVisibilityQuery.VisibleWaterCells   := FVisibleWaterCells;
  iVisibilityQuery.VisibleGrassCells   := FVisibleGrassCells;
  iVisibilityQuery.VisibleMeshCells    := FVisibleMeshCells;
  iVisibilityQuery.VisibleTerrainCells := FVisibleTerrainCells;
  FOctree.GetVisibleCells(iVisibilityQuery);
End;

{******************************************************************************}
{* Render the visible cells                                                   *}
{******************************************************************************}

procedure TGDCellManager.RenderVisibleCells( aRenderAttribute : TGDRenderAttribute;
                                             aRenderFor : TGDRenderFor;
                                             aTerrain : TGDTerrain; aWater : TGDWater; aFoliage : TGDFoliage);
var
  iI : Integer;
  iTerrainCell : TGDTerrainCell;
  iGrassCell   : TGDGrassCell;
  iWaterCell   : TGDWaterCell;
  iMeshCell    : TGDMeshCell;
Begin
  //render the octree boxes.
  if aRenderAttribute = RA_NODE_BOXES then
  begin
    FOctree.RenderTreeBoxes();
    exit;
  end;

  If (aRenderFor = RF_NORMAL) then TriangleCount := 0;

  //render the visible terrain cells
  if Modes.RenderTerrain then
  begin
    aTerrain.StartRendering( aRenderAttribute, aRenderFor );
    for iI := 0 to FVisibleTerrainCells.Count - 1 do
    begin
      iTerrainCell := TGDTerrainCell(FVisibleTerrainCells.Items[ iI ]);
      iTerrainCell.RenderTerrainCell( aRenderAttribute );
      TriangleCount := TriangleCount + TRISINCELL;
    end;
    aTerrain.EndRendering();
  end;

  //render the visible grass cells
  if (Modes.RenderGrass) and (aRenderFor <> RF_WATER) and (aRenderFor <> RF_BLOOM) then
  begin
    aFoliage.StartRenderingGrass( aRenderAttribute );
    for iI := 0 to FVisibleGrassCells.Count - 1 do
    begin
      iGrassCell := TGDGrassCell(FVisibleGrassCells.Items[ iI ]);
      iGrassCell.RenderGrassCell( aRenderAttribute );
      TriangleCount := TriangleCount + iGrassCell.TrisCount;
    end;
    aFoliage.EndRenderingGrass();
  end;

  //render the visible mesh cells
  if (Modes.RenderModels) then
  begin
    for iI := 0 to FVisibleMeshCells.Count - 1 do
    begin
      If (aRenderFor = RF_WATER) and (Settings.WaterReflection = WR_TERRAIN_ONLY) then break;
      iMeshCell := TGDMeshCell(FVisibleMeshCells.Items[ iI ]);
      iMeshCell.RenderMeshCell( aRenderAttribute, aRenderFor );
      TriangleCount := TriangleCount + iMeshCell.TriangleCount();
    end;
  end;

  If (Modes.RenderWater) and(aRenderFor <> RF_WATER) then
  begin
    aWater.StartRendering( aRenderAttribute, aRenderFor );
    for iI := 0 to FVisibleWaterCells.Count - 1 do
    begin
      iWaterCell := TGDWaterCell(FVisibleWaterCells.Items[ iI ]);
      iWaterCell.RenderWaterCell( aRenderAttribute );
      TriangleCount := TriangleCount + (aWater.CellDivX * aWater.CellDivY * 2);
    end;
    aWater.EndRendering();
  end;
End;

end.
