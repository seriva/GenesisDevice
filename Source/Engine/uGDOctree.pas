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
unit uGDOctree;

interface

uses
  dglOpenGL,
  SysUtils,
  uGDTypes,
  uGDConstants,
  uGDMeshCell,
  uGDBaseCell;

type

{******************************************************************************}
{* Visibility query                                                           *}
{******************************************************************************}

  TGDVisibilityQuery = record
    Cells               : TGDBaseCellList;
    VisibleTerrainCells : TGDBaseCellList;
    VisibleGrassCells   : TGDBaseCellList;
    VisibleMeshCells    : TGDBaseCellList;
    VisibleWaterCells   : TGDBaseCellList;
  end;

{******************************************************************************}
{* Octree class                                                               *}
{******************************************************************************}

  TGDOctree = class
  private
    FCellIndexes : array of integer;
    FBoundingBox : TGDBoundingBox;
    FSubNodes    : array[0..7] of TGDOctree;
    procedure InitSubOCTreeNodes(aCells : TGDBaseCellList);
  public
    constructor Create();
    destructor  Destroy(); override;

    procedure InitOcTree(aCells : TGDBaseCellList);
    procedure Clear();

    procedure GetVisibleCells(aQueryData : TGDVisibilityQuery);
    procedure RenderTreeBoxes();
  end;

implementation

uses
  uGDEngine;

{******************************************************************************}
{* Create octree class                                                        *}
{******************************************************************************}

constructor TGDOctree.Create();
begin
  SetLength(FCellIndexes,0);
end;

{******************************************************************************}
{* Destroy octree class                                                       *}
{******************************************************************************}

destructor TGDOctree.Destroy();
begin
  Clear();
  inherited;
end;

{******************************************************************************}
{* Init the octree`s subnodes                                                 *}
{******************************************************************************}

procedure TGDOctree.InitSubOCTreeNodes(aCells : TGDBaseCellList);

procedure SetupNode(iNode : integer);
var
  iI : Integer;
  iTempMeshIndexes : array of integer;
begin
  SetLength(iTempMeshIndexes,0);
  iI := 0;
  while(iI <= (length(FCellIndexes)-1)) do
  begin
    If FSubNodes[iNode].FBoundingBox.BoxInsideBox( aCells.Items[ FCellIndexes[iI] ].BoundingBox ) then
    begin
      SetLength(iTempMeshIndexes,length(iTempMeshIndexes)+1);
      iTempMeshIndexes[length(iTempMeshIndexes)-1] := FCellIndexes[iI];
      FCellIndexes[iI] := FCellIndexes[length(FCellIndexes)-1];
      SetLength(FCellIndexes,length(FCellIndexes)-1);
    end
    else
      iI := iI + 1;
  end;
  If length(iTempMeshIndexes) = 0 then
     FreeAndNil(FSubNodes[iNode])
  else
  begin
    setlength(FSubNodes[iNode].FCellIndexes, length(iTempMeshIndexes));
    for iI := 0 to length(iTempMeshIndexes)-1 do
      FSubNodes[iNode].FCellIndexes[iI] := iTempMeshIndexes[iI];
    FSubNodes[iNode].InitSubOCTreeNodes(aCells);
  end
end;

begin
  FSubNodes[0] := TGDOcTree.Create();
  FSubNodes[0].FBoundingBox.Max.Reset(FBoundingBox.max.x, FBoundingBox.max.y, FBoundingBox.max.z);
  FSubNodes[0].FBoundingBox.Min.Reset(FBoundingBox.Center.x, FBoundingBox.Center.y, FBoundingBox.Center.z);
  FSubNodes[0].FBoundingBox.CalculateCenter();
  SetupNode(0);

  FSubNodes[1] := TGDOcTree.Create();
  FSubNodes[1].FBoundingBox.Max.Reset(FBoundingBox.max.x, FBoundingBox.max.y, FBoundingBox.Center.z);
  FSubNodes[1].FBoundingBox.Min.Reset(FBoundingBox.Center.x, FBoundingBox.Center.y, FBoundingBox.min.z);
  FSubNodes[1].FBoundingBox.CalculateCenter();
  SetupNode(1);

  FSubNodes[2] := TGDOcTree.Create();
  FSubNodes[2].FBoundingBox.Max.Reset(FBoundingBox.Center.x, FBoundingBox.max.y, FBoundingBox.max.z);
  FSubNodes[2].FBoundingBox.Min.Reset(FBoundingBox.min.x, FBoundingBox.Center.y, FBoundingBox.Center.z);
  FSubNodes[2].FBoundingBox.CalculateCenter();
  SetupNode(2);

  FSubNodes[3] := TGDOcTree.Create();
  FSubNodes[3].FBoundingBox.Max.Reset(FBoundingBox.Center.x, FBoundingBox.max.y, FBoundingBox.Center.z);
  FSubNodes[3].FBoundingBox.Min.Reset(FBoundingBox.min.x, FBoundingBox.Center.y, FBoundingBox.min.z);
  FSubNodes[3].FBoundingBox.CalculateCenter();
  SetupNode(3);

  FSubNodes[4] := TGDOcTree.Create();
  FSubNodes[4].FBoundingBox.Max.Reset(FBoundingBox.max.x, FBoundingBox.Center.y, FBoundingBox.max.z);
  FSubNodes[4].FBoundingBox.Min.Reset(FBoundingBox.Center.x, FBoundingBox.min.y, FBoundingBox.Center.z);
  FSubNodes[4].FBoundingBox.CalculateCenter();
  SetupNode(4);

  FSubNodes[5] := TGDOcTree.Create();
  FSubNodes[5].FBoundingBox.Max.Reset(FBoundingBox.max.x, FBoundingBox.Center.y, FBoundingBox.Center.z);
  FSubNodes[5].FBoundingBox.Min.Reset(FBoundingBox.Center.x, FBoundingBox.min.y, FBoundingBox.min.z);
  FSubNodes[5].FBoundingBox.CalculateCenter();
  SetupNode(5);

  FSubNodes[6] := TGDOcTree.Create();
  FSubNodes[6].FBoundingBox.Max.Reset(FBoundingBox.Center.x, FBoundingBox.Center.y, FBoundingBox.max.z);
  FSubNodes[6].FBoundingBox.Min.Reset(FBoundingBox.min.x, FBoundingBox.min.y, FBoundingBox.Center.z);
  FSubNodes[6].FBoundingBox.CalculateCenter();
  SetupNode(6);

  FSubNodes[7] := TGDOcTree.Create();
  FSubNodes[7].FBoundingBox.Max.Reset(FBoundingBox.Center.x, FBoundingBox.Center.y, FBoundingBox.Center.z);
  FSubNodes[7].FBoundingBox.Min.Reset(FBoundingBox.min.x, FBoundingBox.min.y, FBoundingBox.min.z);
  FSubNodes[7].FBoundingBox.CalculateCenter();
  SetupNode(7);
end;

{******************************************************************************}
{* Init the octree                                                            *}
{******************************************************************************}

procedure TGDOctree.InitOcTree(aCells : TGDBaseCellList);
var
  iI    : Integer;
  iCell : TGDBaseCell;
begin
  FBoundingBox.Min.Reset( 9999999999, 9999999999, 9999999999);
  FBoundingBox.Max.Reset(-9999999999,-9999999999,-9999999999);

  for iI := 0 to aCells.Count-1 do
  begin
    iCell := aCells.Items[iI];

    If (iCell.BoundingBox.Min.X < FBoundingBox.Min.x) then
      FBoundingBox.Min.setX(iCell.BoundingBox.Min.X);

    If (iCell.BoundingBox.Min.Y < FBoundingBox.Min.Y) then
      FBoundingBox.Min.SetY(iCell.BoundingBox.Min.y);

    If (iCell.BoundingBox.Min.Z < FBoundingBox.Min.Z) then
      FBoundingBox.Min.SetZ(iCell.BoundingBox.Min.Z);

    If (iCell.BoundingBox.Max.X > FBoundingBox.Max.x) then
      FBoundingBox.Max.Setx(iCell.BoundingBox.Max.X);

    If (iCell.BoundingBox.Max.Y > FBoundingBox.Max.Y) then
      FBoundingBox.Max.Sety(iCell.BoundingBox.Max.y);

    If (iCell.BoundingBox.Max.Z > FBoundingBox.Max.Z) then
      FBoundingBox.Max.SetZ(iCell.BoundingBox.Max.Z);

    SetLength(FCellIndexes,Length(FCellIndexes) + 1);
    FCellIndexes[Length(FCellIndexes)-1] := iI;
  end;

  FBoundingBox.CalculateCenter();
  InitSubOctreeNodes(aCells);
end;

{******************************************************************************}
{* Clear the octree                                                           *}
{******************************************************************************}

procedure TGDOctree.Clear();
var
  iI : Integer;
begin
  FBoundingBox.Min.Reset(0,0,0);
  FBoundingBox.Max.Reset(0,0,0);
  FBoundingBox.Center.Reset(0,0,0);
  SetLength(FCellIndexes,0);
  for iI := 0 to 7 do
    FreeAndNil(FSubNodes[iI])
end;

{******************************************************************************}
{* Get the visible objects                                                    *}
{******************************************************************************}

procedure TGDOctree.GetVisibleCells(aQueryData : TGDVisibilityQuery);
var
  iCell     : TGDBaseCell;
  iMeshCell : TGDMeshCell;
  iI        : Integer;
  iVertex   : TGDVector;
begin
  If Not(GDCamera.BoxInView(FBoundingBox)) then
    exit;

  for iI := 0 to length(FCellIndexes)-1 do
  begin
    If GDCamera.BoxInView( TGDBaseCell( aQueryData.Cells.Items[  FCellIndexes[iI] ] ).BoundingBox ) then
    begin
      iCell := TGDBaseCell( aQueryData.Cells.Items[  FCellIndexes[iI] ] );
      iVertex := TGDBaseCell( aQueryData.Cells.Items[  FCellIndexes[iI] ] ).BoundingBox.Center.Copy();
      iVertex.Substract(GDCamera.Position.Copy());
      iCell.Distance := iVertex.Magnitude();

      If (iCell.OjectType = SO_WATERCELL) and GDModes.RenderWater then
        aQueryData.VisibleWaterCells.Add(iCell);

      If (iCell.OjectType = SO_GRASSCELL) and GDModes.RenderGrass then
        If (iCell.Distance < (GDSettings.FoliageDistance * R_FOLIAGE_DISTANCE_STEP + (R_FOLIAGE_DISTANCE_STEP * 10))) then
             aQueryData.VisibleGrassCells.Add(iCell);

      If (iCell.OjectType = SO_TERRAINCELL) and GDModes.RenderTerrain then
        aQueryData.VisibleTerrainCells.Add(iCell);

      If (iCell.OjectType = SO_MESHCELL) and GDModes.RenderMeshes then
      begin
        iMeshCell := TGDMeshCell(iCell);
        if iMeshCell.LODType = LT_FADE_IN then
        begin
          if iMeshCell.Distance < iMeshCell.FadeDistance then
             aQueryData.VisibleMeshCells.Add(iMeshCell);
        end
        else
          aQueryData.VisibleMeshCells.Add(iMeshCell);
      end;
    end;
  end;

  for iI := 0 to 7 do
    If FSubNodes[iI] <> nil then
      FSubNodes[iI].GetVisibleCells(aQueryData);
end;

{******************************************************************************}
{* Render the treeboxes                                                       *}
{******************************************************************************}

procedure TGDOctree.RenderTreeBoxes();
var
  iI : Integer;
begin
 if Not(GDCamera.BoxInView(FBoundingBox)) then
  exit;

 FBoundingBox.RenderWireFrame();

 for iI := 0 to 7 do
    If FSubNodes[iI] <> nil then
      FSubNodes[iI].RenderTreeBoxes();
end;

end.
