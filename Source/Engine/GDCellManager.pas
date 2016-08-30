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
  GDWater,
  GDWaterCell,
  GDFoliage,
  GDGrassCell,
  GDTerrain,
  GDTerrainCell,
  GDOctree,
  GDTypes,
  GDMeshCell;

type

{******************************************************************************}
{* Cellmanager class                                                          *}
{******************************************************************************}

  TGDCellManager = class
  private
    FTriangleCount       : Integer;
    FCells               : TGDBaseCellList;
    FVisibleTerrainCells : TGDBaseCellList;
    FVisibleGrassCells   : TGDBaseCellList;
    FVisibleMeshCells    : TGDBaseCellList;
    FVisibleWaterCells   : TGDBaseCellList;
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

uses
  GDEngine;

{******************************************************************************}
{* Create the cell manager class                                              *}
{******************************************************************************}

constructor TGDCellManager.Create();
begin
  FCells               := TGDBaseCellList.Create();
  FVisibleTerrainCells := TGDBaseCellList.Create(false);
  FVisibleGrassCells   := TGDBaseCellList.Create(false);
  FVisibleMeshCells    := TGDBaseCellList.Create(false);
  FVisibleWaterCells   := TGDBaseCellList.Create(false);
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
  while (iI <= (aTerrain.TerrainWidth-TERRAIN_CELLSIZE)) do
  begin
    iJ := 1;
    while (iJ <= (aTerrain.TerrainHeight-TERRAIN_CELLSIZE)) do
    begin
      FCells.Add(TGDTerrainCell.Create(aTerrain, iI, iJ, iI+TERRAIN_CELLSIZE, iJ+TERRAIN_CELLSIZE));
      iJ := iJ + TERRAIN_CELLSIZE
    end;
    iI := iI + TERRAIN_CELLSIZE
  end;
End;

{******************************************************************************}
{* Generate the static watercell objects from the water class                 *}
{******************************************************************************}

procedure TGDCellManager.GenerateWaterCells(aWater : TGDWater; aTerrain : TGDTerrain);
var
 iI, iJ, iK, iL : Double;
 iCountX, iCountY : Integer;
 iStepX, iStepY : Double;
 iStepU, iStepV : Double;
 iCurrentU, iCurrentV : Double;
 iCellNotAboveTerrain : Boolean;
 iHeight : Double;
Begin
  If Not(aWater.WaterLoaded) then
    exit;

  iStepX := (aWater.BoundingBox.Max.X + Abs(aWater.BoundingBox.Min.X)) / aWater.CellCountX;
  iStepY := (aWater.BoundingBox.Max.Z + Abs(aWater.BoundingBox.Min.Z)) / aWater.CellCountY;
  iStepU := 1 / aWater.CellCountX;
  iStepV := 1 / aWater.CellCountY;

  iI := aWater.BoundingBox.Min.X-iStepX;
  iCurrentU := 0;
  iCountX   := 0;
  while (iI < (aWater.BoundingBox.Max.X)) do
  begin
    iJ := aWater.BoundingBox.Min.Z-iStepY;
    iCurrentV := 0;
    iCountY := 0;
    while (iJ < (aWater.BoundingBox.Max.Z)) do
    begin
      iCellNotAboveTerrain := true;

      iK := iI;
      while ((iK <= ((iI+iStepX))) and iCellNotAboveTerrain) do
      begin
        iL := iJ;
        while ((iL <= ((iJ+iStepY))) and iCellNotAboveTerrain) do
        begin
          If aTerrain.GetHeight(iK, iL, iHeight) and (iHeight <= aWater.WaterHeight) then
          begin
            FCells.Add( TGDWaterCell.Create(aWater, iI, iJ, iI+iStepX, iJ+iStepY, iCurrentU, iCurrentV, iCurrentU + iStepU, iCurrentV + iStepV) );
            iCellNotAboveTerrain := false;
          end;
          iL := iL + iStepY;
        end;
        iK := iK + iStepX;
      end;

      iCurrentV := iCountY * iStepV;
      iJ := iJ + iStepY;
      iCountY := iCountY + 1;
    end;
    iCurrentU := iCountX * iStepU;
    iI := iI + iStepX;
    iCountX := iCountX + 1;
  end;

  //update the VBO array for water.
  aWater.UpdateVBO();
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
  Engine.Timing.Start();
  iStepX := aTerrain.TerrainWidth div (aTerrain.TerrainWidth div GRASS_CELLSIZE);
  iStepY := aTerrain.TerrainHeight div (aTerrain.TerrainHeight div GRASS_CELLSIZE);

  Engine.GUI.LoadingScreen.Start('Generating foliage...', Round(aTerrain.TerrainWidth/iStepX) + aFoliage.TreeTypes.Count + aFoliage.RockTypes.Count );

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
    Engine.GUI.LoadingScreen.Update();
  end;

  //create treecells
  for iI := 0 to aFoliage.TreeTypes.Count-1 do
  begin
    iMeshType := aFoliage.TreeTypes.Items[iI];
    iTreeCount := Round(aFoliage.TreeCount * iMeshType.CoverOfTotal) div 100;
    for iJ := 1 to iTreeCount do
    begin
      RedoRandomTrees:

      iX := Random(aTerrain.TerrainWidth-1);
      iY := Random(aTerrain.TerrainHeight-1);
      iPos.Reset( aTerrain.GetPoint( iX, iY ).Vertex.X + (aTerrain.TriangleSize div 2) ,
                  0,
                  aTerrain.GetPoint( iX, iY ).Vertex.Z + (aTerrain.TriangleSize div 2) );

      if aFoliage.CheckTreeMap(iX, iY) and aTerrain.GetHeight(iPos.X, iPos.Z, iHeight) then
      begin
        iHeight := iHeight - 200;
        if not((iHeight > aFoliage.TreeLowerLimit) and (iHeight < aFoliage.TreeUpperLimit)) then
           goto RedoRandomTrees;

        iMeshInput.Model         := iMeshType.Mesh.Name;
        iMeshInput.Position.X    := iPos.X;
        iMeshInput.Position.Y    := iHeight;
        iMeshInput.Position.Z    := iPos.Z;
        iMeshInput.Rotation.X    := iMeshType.StartRotation.X;
        iMeshInput.Rotation.Y    := iMeshType.StartRotation.Y + Random(Round(360));
        iMeshInput.Rotation.Z    := iMeshType.StartRotation.Z;
        iMeshInput.Scale.X       := iMeshType.Scale + Random(Round(iMeshType.RandomScale));
        iMeshInput.Scale.Y       := iMeshType.Scale + Random(Round(iMeshType.RandomScale));
        iMeshInput.Scale.Z       := iMeshType.Scale + Random(Round(iMeshType.RandomScale));
        iMeshInput.FadeDistance  := 0;
        iMeshInput.FadeScale     := 0;
        if (iMeshType.MeshLOD1 <> nil) and (iMeshType.MeshLOD2 <> nil) then
        begin
          iMeshInput.ModelLOD1     := iMeshType.MeshLOD1.name;
          iMeshInput.ModelLOD2     := iMeshType.MeshLOD2.name;
          iMeshInput.FadeDistance  := 0;
          iMeshInput.FadeScale     := 0;
        end
        else
        begin
          iMeshInput.ModelLOD1     := '';
          iMeshInput.ModelLOD2     := '';
          iMeshInput.FadeDistance  := Engine.Settings.FoliageDistance * R_FOLIAGE_DISTANCE_STEP + (R_FOLIAGE_DISTANCE_STEP * 10);
          iMeshInput.FadeScale     := R_FOLIAGE_LOD_DISTANCE;
        end;

        iMeshInput.CastShadow    := True;
        iMeshInput.ReceiveShadow := false;

        FCells.Add( TGDMeshCell.Create(iMeshInput) );
      end
      else
        goto RedoRandomTrees;
    end;
    Engine.GUI.LoadingScreen.Update();
  end;

  //create rocks
  for iI := 0 to aFoliage.RockTypes.Count-1 do
  begin
    iMeshType  := aFoliage.RockTypes.Items[iI];
    iRockCount := Round(aFoliage.RockCount * iMeshType.CoverOfTotal) div 100;
    for iJ := 1 to iRockCount do
    begin
      RedoRandomRocks:

      iX := Random(aTerrain.TerrainWidth-1);
      iY := Random(aTerrain.TerrainHeight-1);
      iPos.Reset( aTerrain.GetPoint( iX, iY ).Vertex.X + (aTerrain.TriangleSize div 2) ,
                  0,
                  aTerrain.GetPoint( iX, iY ).Vertex.Z + (aTerrain.TriangleSize div 2) );

      if aFoliage.CheckRockMap(iX, iY) and aTerrain.GetHeight(iPos.X, iPos.Z, iHeight) then
      begin
        aTerrain.GetRotation(iPos.X, iPos.Z, iRot );
        iMeshInput.Model         := iMeshType.Mesh.Name;
        iMeshInput.ModelLOD1     := '';
        iMeshInput.ModelLOD2     := '';
        iMeshInput.Position.X    := iPos.X;
        iMeshInput.Position.Y    := iHeight;
        iMeshInput.Position.Z    := iPos.Z;
        iMeshInput.Rotation.X    := iRot.x;
        iMeshInput.Rotation.Y    := iRot.y;
        iMeshInput.Rotation.Z    := iRot.z;
        iMeshInput.Scale.X       := iMeshType.Scale + Random(Round(iMeshType.RandomScale));
        iMeshInput.Scale.Y       := iMeshType.Scale + Random(Round(iMeshType.RandomScale));
        iMeshInput.Scale.Z       := iMeshType.Scale + Random(Round(iMeshType.RandomScale));
        iMeshInput.FadeDistance  := Engine.Settings.FoliageDistance * R_FOLIAGE_DISTANCE_STEP + (R_FOLIAGE_DISTANCE_STEP * 10);
        iMeshInput.FadeScale     := R_FOLIAGE_LOD_DISTANCE;
        iMeshInput.CastShadow    := False;
        iMeshInput.ReceiveShadow := True;
        FCells.Add( TGDMeshCell.Create(iMeshInput) );
      end
      else
        goto RedoRandomRocks;


    end;
    Engine.GUI.LoadingScreen.Update();
  end;
  Engine.Timing.Stop();
  Engine.Console.Write('......Generated foliage (' + Engine.Timing.TimeInSeconds + ' Sec)');
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
  if Engine.Modes.RenderTerrain and (aRenderFor <> RF_SHADOW) then
  begin
    aTerrain.StartRendering( aRenderAttribute, aRenderFor );
    for iI := 0 to FVisibleTerrainCells.Count - 1 do
    begin
      iTerrainCell := TGDTerrainCell(FVisibleTerrainCells.Items[ iI ]);
      iTerrainCell.Render( aRenderAttribute, aRenderFor );
      TriangleCount := TriangleCount + (TERRAIN_CELLSIZE * TERRAIN_CELLSIZE * 2);
    end;
    aTerrain.EndRendering();
  end;

  //render the visible grass cells
  if (Engine.Modes.RenderGrass) and (aRenderFor = RF_NORMAL) then
  begin
    aFoliage.StartRenderingGrass( aRenderAttribute );
    for iI := 0 to FVisibleGrassCells.Count - 1 do
    begin
      iGrassCell := TGDGrassCell(FVisibleGrassCells.Items[ iI ]);
      iGrassCell.Render( aRenderAttribute, aRenderFor );
      TriangleCount := TriangleCount + iGrassCell.TrisCount;
    end;
    aFoliage.EndRenderingGrass();
  end;

  //render the visible mesh cells
  if Engine.Modes.RenderModels then
  begin
    for iI := 0 to FVisibleMeshCells.Count - 1 do
    begin
      iMeshCell := TGDMeshCell(FVisibleMeshCells.Items[ iI ]);
      if (aRenderFor = RF_SHADOW) then
      begin
        if iMeshCell.CastShadow then
           iMeshCell.Render( aRenderAttribute, aRenderFor );
      end
      else
        iMeshCell.Render( aRenderAttribute, aRenderFor );
      TriangleCount := TriangleCount + iMeshCell.TriangleCount();
    end;
  end;

  //Render the sun
  if (aRenderFor = RF_NORMAL) and not(Engine.Modes.RenderWireframe) and (aRenderAttribute = RA_NORMAL) then
    Engine.Map.SkyDome.RenderSun();

  //render the visible water cells
  If (Engine.Modes.RenderWater) and (aRenderFor <> RF_WATER) and (aRenderFor <> RF_SHADOW) then
  begin
    aWater.StartRendering( aRenderAttribute, aRenderFor );
    for iI := 0 to FVisibleWaterCells.Count - 1 do
    begin
      iWaterCell := TGDWaterCell(FVisibleWaterCells.Items[ iI ]);
      iWaterCell.Render( aRenderAttribute, aRenderFor );
      TriangleCount := TriangleCount + 2;
    end;
    aWater.EndRendering();
  end;

  //Render the sunflare
  if (aRenderFor = RF_NORMAL) and not(Engine.Modes.RenderWireframe) and (aRenderAttribute = RA_NORMAL) then
    Engine.Map.SkyDome.RenderSunFlare();
End;

end.
