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
  GDWater,
  GDWaterCell,
  GDFoliage,
  GDGrassCell,
  GDTerrain,
  GDTerrainCell,
  GDTiming,
  GDConsole,
  GDTypes,
  GDMeshCell,
  Contnrs,
  GDModes;

type

{******************************************************************************}
{* Cellmanager class                                                         *}
{******************************************************************************}

  TGDCellManager = class (TObject)
  private
    FCells               : TObjectList;
    FVisibleCells        : TObjectList;
    FVisibleWaterCells   : TObjectList;
    FSortVisibleCells    : boolean;
    FTriangleCount       : Integer;

    procedure QuickSortCells( aCells : TObjectList; aLo, aHi: Integer);
  public
    property TriangleCount     : Integer read FTriangleCount write FTriangleCount;
    property Cells             : TObjectList read FCells write FCells;
    property VisibleCells      : TObjectList read FVisibleCells write FVisibleCells;
    property VisibleWaterCells : TObjectList read FVisibleWaterCells write FVisibleWaterCells;
    property SortVisibleCells  : boolean read FSortVisibleCells write FSortVisibleCells;

    constructor Create();
    destructor  Destroy(); override;

    procedure GenerateCellsFromTerrain();
    procedure GenerateCellsFromWater();
    procedure GenerateCellsFromFoliage();
    procedure GenerateAllCells();
    procedure RemoveCells( aType : TGDStaticObjectType  );
    procedure Clear();

    procedure DetectVisibleCells();
    procedure RenderVisibleCells( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor );
  end;

var
  CellManager : TGDCellManager;

implementation

uses
  GDOctree;

{******************************************************************************}
{* Quicksort a staticobject via distance                                      *}
{******************************************************************************}

procedure TGDCellManager.QuickSortCells( aCells : TObjectList; aLo, aHi: Integer);
var
  iLo, iHi : Integer;
  iMid : Double;
  iOC1, iOC2 : TGDBaseCell;
begin
  iLo := aLo;
  iHi := aHi;
  iMid :=  TGDBaseCell(aCells.Items[ (iLo + iHi) div 2 ]).Distance;
  repeat
    while TGDBaseCell( aCells.Items[ iLo ] ).Distance < iMid do Inc(iLo);
    while TGDBaseCell( aCells.Items[ iHi ] ).Distance > iMid do Dec(iHi);
    if iLo <= iHi then
    begin
      iOC1 := nil;
      iOC2 := nil;
      iOC1 := TGDBaseCell( aCells.Items[ iLo ] );
      iOC2 := TGDBaseCell( aCells.Items[ iHi ] );
      aCells.Items[iLo] := iOC2;
      aCells.Items[iHi] := iOC1;
      Inc(iLo);
      Dec(iHi);
    end;
  until iLo > iHi;
  if iHi > aLo then QuickSortCells( aCells, aLo, iHi );
  if iLo < aHi then QuickSortCells( aCells, iLo, aHi);
end;

{******************************************************************************}
{* Create the cell manager class                                              *}
{******************************************************************************}

constructor TGDCellManager.Create();
begin
  FCells               := TObjectList.Create();
  FVisibleCells        := TObjectList.Create();
  FVisibleWaterCells   := TObjectList.Create();
  FVisibleCells.OwnsObjects        := false;
  FVisibleWaterCells.OwnsObjects   := false;
  FSortVisibleCells := true;
  Console.AddCommand('CellSort', '0,1 : Enable or disable object depth sorting', CT_BOOLEAN, @FSortVisibleCells);
end;

{******************************************************************************}
{* Destroy the cell manager class                                             *}
{******************************************************************************}

destructor  TGDCellManager.Destroy();
begin
  FreeAndNil(FCells);
  FreeAndNil(FVisibleCells);
  FreeAndNil(FVisibleWaterCells);
end;

{******************************************************************************}
{* Clear the cell manager                                                     *}
{******************************************************************************}

procedure TGDCellManager.Clear();
begin
  FCells.Clear();
  FVisibleCells.Clear();
  FVisibleWaterCells.Clear();
end;

{******************************************************************************}
{* Generate the static terraincell objects from the terrain class             *}
{******************************************************************************}

procedure TGDCellManager.GenerateCellsFromTerrain();
var
 iI, iJ : Integer;
Begin
  If Not(Terrain.TerrainLoaded) then
    exit;

  iI := 1;
  while (iI <= (Terrain.TerrainWidth-CELLSIZE)) do
  begin
    iJ := 1;
    while (iJ <= (Terrain.TerrainHeight-CELLSIZE)) do
    begin
      FCells.Add( TGDTerrainCell.Create(iI, iJ, iI+CELLSIZE, iJ+CELLSIZE) );
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
  If Not(Water.WaterLoaded) then
    exit;

  iStepX1 := (Water.BoundingBox.Max.X + Abs(Water.BoundingBox.Min.X)) / Water.CellCountX;
  iStepY1 := (Water.BoundingBox.Max.Z + Abs(Water.BoundingBox.Min.Z)) / Water.CellCountY;
  iStepX2 := (Water.BoundingBox.Max.X + Abs(Water.BoundingBox.Min.X)) / (Water.CellCountX * Water.CellDivX);
  iStepY2 := (Water.BoundingBox.Max.Z + Abs(Water.BoundingBox.Min.Z)) / (Water.CellCountY * Water.CellDivY);
  iStepU1 := Water.WaterU / (Water.CellCountX * Water.CellDivX);
  iStepV1 := Water.WaterV / (Water.CellCountY * Water.CellDivY);
  iStepU2 := 1 / (Water.CellCountX * Water.CellDivX);
  iStepV2 := 1 / (Water.CellCountY * Water.CellDivY);

  iI := Water.BoundingBox.Min.X-iStepX1;
  iCurrentU1 := 0;
  iCurrentU2 := 0;
  iCountX := 0;
  while (iI < (Water.BoundingBox.Max.X)) do
  begin
    iJ := Water.BoundingBox.Min.Z-iStepY1;
    iCurrentV1 := 0;
    iCurrentV2 := 0;
    iCountY := 0;
    while (iJ < (Water.BoundingBox.Max.Z)) do
    begin
      iCellNotAboveTerrain := true;

      iK := iI;
      while ((iK <= ((iI+iStepX1))) and iCellNotAboveTerrain) do
      begin
        iL := iJ;
        while ((iL <= ((iJ+iStepY1))) and iCellNotAboveTerrain) do
        begin
          iHeight := 0;
          If Terrain.GetHeight(iK, iL, iHeight) then
          begin
            if iHeight <= Water.WaterHeight then
            begin
              FCells.Add( TGDWaterCell.Create(iI, iJ, iI+iStepX1, iJ+iStepY1, iCurrentU1, iCurrentV1, iStepU1, iStepV1,
                                                                        iCurrentU2, iCurrentV2, iStepU2, iStepV2) );
              iCellNotAboveTerrain := false;
            end;
          end;
          iL := iL + iStepY2;
        end;
        iK := iK + iStepX2;
      end;

      iCurrentV1 := iCountY * Water.CellDivY * iStepV1;
      iCurrentV2 := iCountY * Water.CellDivY * iStepV2;
      iJ := iJ + iStepY1;
      iCountY := iCountY + 1;
    end;
    iCurrentU1 := iCountX * Water.CellDivX * iStepU1;
    iCurrentU2 := iCountX * Water.CellDivX * iStepU2;
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
  iStepX := Round((Terrain.TerrainWidth-1) / Foliage.GrassCellCountX);
  iStepY := Round((Terrain.TerrainHeight-1) / Foliage.GrassCellCountY);

  GUI.LoadingScreen.SetupForUse('Generating foliage...', Round(Terrain.TerrainWidth/iStepX) + Foliage.TreeTypes.Count + Foliage.RockTypes.Count );

  //create grasscells
  iI := 1;
  while (iI <= (Terrain.TerrainWidth-iStepX)) do
  begin
    iJ := 1;
    while (iJ <= (Terrain.TerrainHeight-iStepY)) do
    begin

      iCellHasGrass := false;
      iK := iI-1;
      while ((iK <= (iI-2+iStepX)) and Not(iCellHasGrass)  )  do
      begin
        iL := iJ-1;
        while ((iL <= (iJ-2+iStepY)) and Not(iCellHasGrass)  )  do
        begin
          iCellHasGrass := Foliage.GrassMap[iK,iL];
          iL := iL+1;
        end;
        iK := iK+1;
      end;

      If iCellHasGrass then
      begin
        FCells.Add( TGDGrassCell.Create(iI, iJ, iI+iStepX, iJ+iStepY) );
      end;

      iJ := iJ + iStepY;
    end;
    iI := iI + iStepX;
    GUI.LoadingScreen.UpdateBar();
  end;

  //create treecells
  for iI := 0 to Foliage.TreeTypes.Count-1 do
  begin
    iTreeType := Foliage.TreeTypes.Items[iI] as TGDTreeType;
    iTreeCount := Round(Foliage.TreeCount * iTreeType.CoverOfTotal) div 100;
    for iJ := 1 to iTreeCount do
    begin
      RedoRandomTrees:

      iX := Random(Terrain.TerrainWidth-1);
      iY := Random(Terrain.TerrainHeight-1);
      iPos.Reset( Terrain.TerrainPoints[ iX, iY ].FVertex.X + (Terrain.TriangleSize div 2) ,
                  0,
                  Terrain.TerrainPoints[ iX, iY ].FVertex.Z + (Terrain.TriangleSize div 2) );

      if Foliage.CheckTreeMap(iX, iY) and Terrain.GetHeight(iPos.X, iPos.Z, iHeight) then
      begin
        iHeight := iHeight - 200;
        if not((iHeight > Foliage.TreeLowerLimit) and (iHeight < Foliage.TreeUpperLimit)) then
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

        CellManager.Cells.Add( TGDMeshCell.Create(iMeshInput) );
      end
      else
        goto RedoRandomTrees;
    end;
    GUI.LoadingScreen.UpdateBar();
  end;

  //create rocks
  for iI := 0 to Foliage.RockTypes.Count-1 do
  begin
    iRockType  := Foliage.RockTypes.Items[iI] as TGDRockType;
    iRockCount := Round(Foliage.RockCount * iRockType.CoverOfTotal) div 100;
    for iJ := 1 to iRockCount do
    begin
      RedoRandomRocks:

      iX := Random(Terrain.TerrainWidth-1);
      iY := Random(Terrain.TerrainHeight-1);
      iPos.Reset( Terrain.TerrainPoints[ iX, iY ].FVertex.X + (Terrain.TriangleSize div 2) ,
                  0,
                  Terrain.TerrainPoints[ iX, iY ].FVertex.Z + (Terrain.TriangleSize div 2) );

      if Foliage.CheckRockMap(iX, iY) and Terrain.GetHeight(iPos.X, iPos.Z, iHeight) then
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
        CellManager.Cells.Add( TGDMeshCell.Create(iMeshInput) );
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

procedure TGDCellManager.GenerateAllCells();
Begin
  GenerateCellsFromTerrain();
  GenerateCellsFromWater();
  GenerateCellsFromFoliage();
End;

{******************************************************************************}
{* Remove cells of a certain type                                             *}
{******************************************************************************}

procedure TGDCellManager.RemoveCells( aType : TGDStaticObjectType  );
var
  iI : Integer;
Begin
  iI := 0;
  while (iI <  FCells.Count) do
  begin

    If TGDBaseCell( FCells.Items[ iI ]).OjectType = aType then
       FCells.Delete( iI )
    else
      iI := iI + 1;
  end;
End;

{******************************************************************************}
{* Detect visible cells                                                       *}
{******************************************************************************}

procedure TGDCellManager.DetectVisibleCells();
var
 iK : Integer;
Begin
  //clear all the list
  FVisibleCells.Clear();
  FVisibleWaterCells.Clear();
  Octree.GetVisibleCells();

  //sort the list list
  If FSortVisibleCells then
  begin
    If (FVisibleWaterCells.Count > 1) then
      QuickSortCells( FVisibleWaterCells, 0, FVisibleWaterCells.Count-1 );

    If (FVisibleCells.Count > 1) then
      QuickSortCells( FVisibleCells, 0, FVisibleCells.Count-1 );
  end;

  //add the water cells to the main list
  for iK := 0 to FVisibleWaterCells.Count - 1 do FVisibleCells.Add( FVisibleWaterCells.Items[iK] );
End;

{******************************************************************************}
{* Render the visible cells                                                   *}
{******************************************************************************}

procedure TGDCellManager.RenderVisibleCells( aRenderAttribute : TGDRenderAttribute;
                                             aRenderFor : TGDRenderFor );
var
  iI : Integer;
  iDistance : Double;
  iAlphaFunction : Double;
  iTerrainCell : TGDTerrainCell;
  iGrassCell   : TGDGrassCell;
  iWaterCell   : TGDWaterCell;
  iMeshCell    : TGDMeshCell;

procedure RenderObject();
begin
  Case TGDBaseCell(FVisibleCells.Items[ iI ]).OjectType of
    SO_NONE :
    Begin
      //do nothing
    End;

    SO_TERRAINCELL :
    Begin
      if Modes.RenderTerrain then
      begin
        Terrain.StartRendering( aRenderAttribute, aRenderFor );
        iTerrainCell := TGDBaseCell(FVisibleCells.Items[ iI ]) As TGDTerrainCell;
        iTerrainCell.RenderTerrainCell( aRenderAttribute );
        Terrain.EndRendering();
        TriangleCount := TriangleCount + TRISINCELL;
      end;
    end;

    SO_GRASSCELL :
    Begin
      If (aRenderFor = RF_WATER) or (aRenderFor = RF_BLOOM) then exit;
      if Modes.RenderGrass then
      begin
        iGrassCell := TGDBaseCell(FVisibleCells.Items[ iI ]) As TGDGrassCell;
        iDistance  := Settings.FoliageDistance * R_FOLIAGE_DISTANCE_STEP;
        if iGrassCell.Distance < iDistance then iAlphaFunction := 0.0 else
        if iGrassCell.Distance < (iDistance + (R_FOLIAGE_DISTANCE_STEP * 1)) then iAlphaFunction := 0.05 else
        if iGrassCell.Distance < (iDistance + (R_FOLIAGE_DISTANCE_STEP * 2)) then iAlphaFunction := 0.1 else
        if iGrassCell.Distance < (iDistance + (R_FOLIAGE_DISTANCE_STEP * 3)) then iAlphaFunction := 0.15 else
        if iGrassCell.Distance < (iDistance + (R_FOLIAGE_DISTANCE_STEP * 4)) then iAlphaFunction := 0.2 else
        if iGrassCell.Distance < (iDistance + (R_FOLIAGE_DISTANCE_STEP * 5)) then iAlphaFunction := 0.25 else
        if iGrassCell.Distance < (iDistance + (R_FOLIAGE_DISTANCE_STEP * 6)) then iAlphaFunction := 0.3 else
        if iGrassCell.Distance < (iDistance + (R_FOLIAGE_DISTANCE_STEP * 7)) then iAlphaFunction := 0.35 else
        if iGrassCell.Distance < (iDistance + (R_FOLIAGE_DISTANCE_STEP * 8)) then iAlphaFunction := 0.40 else
        if iGrassCell.Distance < (iDistance + (R_FOLIAGE_DISTANCE_STEP * 9)) then iAlphaFunction := 0.45 else
        if iGrassCell.Distance < (iDistance + (R_FOLIAGE_DISTANCE_STEP * 10)) then iAlphaFunction := 0.5;
        Foliage.StartRenderingGrass( aRenderAttribute, iAlphaFunction );
        iGrassCell.RenderGrassCell( aRenderAttribute );
        Foliage.EndRenderingGrass();
        TriangleCount := TriangleCount + iGrassCell.TrisCount;
      end;
    end;

    SO_WATERCELL :
    Begin
      If Not(aRenderFor = RF_WATER) then
      begin
        Water.StartRendering( aRenderAttribute, aRenderFor );
        iWaterCell := TGDBaseCell(FVisibleCells.Items[ iI ]) As TGDWaterCell;
        iWaterCell.RenderWaterCell( aRenderAttribute );
        TriangleCount := TriangleCount + (Water.CellDivX * Water.CellDivY * 2);
        Water.EndRendering();
      end;
    end;

    SO_MESHCELL :
    Begin
      If (aRenderFor = RF_WATER) and (Settings.WaterReflection = WR_TERRAIN_ONLY) then exit;
      if Modes.RenderModels then
      begin
        iMeshCell := TGDBaseCell(FVisibleCells.Items[ iI ]) As TGDMeshCell;
        iMeshCell.RenderMeshCell( aRenderAttribute, aRenderFor );
        TriangleCount := TriangleCount + iMeshCell.TriangleCount();
      end;
    end;
  end;

end;

Begin
  If (aRenderFor = RF_NORMAL) then TriangleCount := 0;

  //render the visible cells
  for iI := 0 to FVisibleCells.Count - 1 do
  begin
      RenderObject();
  end;
End;

end.
