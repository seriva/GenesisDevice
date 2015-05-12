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
unit GDOctree;

{$MODE Delphi}

{******************************************************************************}
{* The octree unit is the main spacial patitioning system in the engine. It   *}
{* uses staticobjectbase oriented objects and it`s AABB to create an octree   *}
{* for visibility culling                                                     *}
{******************************************************************************}

interface

uses
  dglOpenGL,
  SysUtils,
  GDFrustum,
  GDTypes,
  GDCamera,
  GDRenderer,
  GDConstants,
  GDSettings,
  GDBaseCell,
  GDMeshCell,
  GDCellManager,
  GDModes;

Const
  TOP_LEFT_FRONT     = 0;
  TOP_LEFT_BACK      = 1;
  TOP_RIGHT_BACK     = 2;
  TOP_RIGHT_FRONT    = 3;
  BOTTOM_LEFT_FRONT  = 4;
  BOTTOM_LEFT_BACK   = 5;
  BOTTOM_RIGHT_BACK  = 6;
  BOTTOM_RIGHT_FRONT = 7;

type

{******************************************************************************}
{* Octree class                                                               *}
{******************************************************************************}

  TGDOcTree = class
  private
    FCellIndexes : array of integer;
    FBoundingBox : TGDBoundingBox;
    FSubNodes    : array[0..7] of TGDOcTree;
    procedure InitSubOCTreeNodes();
  public
    constructor Create();
    destructor  Destroy(); override;

    procedure InitOcTree();
    procedure Clear();

    procedure GetVisibleCells();
    procedure RenderTreeBoxes();
  end;

Var
  Octree : TGDOcTree;

implementation

{******************************************************************************}
{* Create octree class                                                        *}
{******************************************************************************}

constructor TGDOcTree.Create();
begin
  SetLength(FCellIndexes,0);
end;

{******************************************************************************}
{* Destroy octree class                                                       *}
{******************************************************************************}

destructor TGDOcTree.Destroy();
begin
  Clear();
  inherited;
end;

{******************************************************************************}
{* Init the octree`s subnodes                                                 *}
{******************************************************************************}

procedure TGDOcTree.InitSubOCTreeNodes();

procedure SetupNode(iNode : integer);
var
  iI : Integer;
  iTempMeshIndexes : array of integer;
begin
  SetLength(iTempMeshIndexes,0);
  iI := 0;
  while(iI <= (length(FCellIndexes)-1)) do
  begin
    If FSubNodes[iNode].FBoundingBox.BoxInsideBox( TGDBaseCell( CellManager.Cells.Items[ FCellIndexes[iI] ]).BoundingBox ) then
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
    FSubNodes[iNode].InitSubOCTreeNodes();
  end
end;

begin
  FSubNodes[TOP_LEFT_FRONT] := TGDOcTree.Create();
  FSubNodes[TOP_LEFT_FRONT].FBoundingBox.Max.Reset(FBoundingBox.max.x, FBoundingBox.max.y, FBoundingBox.max.z);
  FSubNodes[TOP_LEFT_FRONT].FBoundingBox.Min.Reset(FBoundingBox.Center.x, FBoundingBox.Center.y, FBoundingBox.Center.z);
  FSubNodes[TOP_LEFT_FRONT].FBoundingBox.CalculateCenter();
  SetupNode(TOP_LEFT_FRONT);

  FSubNodes[TOP_LEFT_BACK] := TGDOcTree.Create();
  FSubNodes[TOP_LEFT_BACK].FBoundingBox.Max.Reset(FBoundingBox.max.x, FBoundingBox.max.y, FBoundingBox.Center.z);
  FSubNodes[TOP_LEFT_BACK].FBoundingBox.Min.Reset(FBoundingBox.Center.x, FBoundingBox.Center.y, FBoundingBox.min.z);
  FSubNodes[TOP_LEFT_BACK].FBoundingBox.CalculateCenter();
  SetupNode(TOP_LEFT_BACK);

  FSubNodes[TOP_RIGHT_BACK] := TGDOcTree.Create();
  FSubNodes[TOP_RIGHT_BACK].FBoundingBox.Max.Reset(FBoundingBox.Center.x, FBoundingBox.max.y, FBoundingBox.max.z);
  FSubNodes[TOP_RIGHT_BACK].FBoundingBox.Min.Reset(FBoundingBox.min.x, FBoundingBox.Center.y, FBoundingBox.Center.z);
  FSubNodes[TOP_RIGHT_BACK].FBoundingBox.CalculateCenter();
  SetupNode(TOP_RIGHT_BACK);

  FSubNodes[TOP_RIGHT_FRONT] := TGDOcTree.Create();
  FSubNodes[TOP_RIGHT_FRONT].FBoundingBox.Max.Reset(FBoundingBox.Center.x, FBoundingBox.max.y, FBoundingBox.Center.z);
  FSubNodes[TOP_RIGHT_FRONT].FBoundingBox.Min.Reset(FBoundingBox.min.x, FBoundingBox.Center.y, FBoundingBox.min.z);
  FSubNodes[TOP_RIGHT_FRONT].FBoundingBox.CalculateCenter();
  SetupNode(TOP_RIGHT_FRONT);

  FSubNodes[BOTTOM_LEFT_FRONT] := TGDOcTree.Create();
  FSubNodes[BOTTOM_LEFT_FRONT].FBoundingBox.Max.Reset(FBoundingBox.max.x, FBoundingBox.Center.y, FBoundingBox.max.z);
  FSubNodes[BOTTOM_LEFT_FRONT].FBoundingBox.Min.Reset(FBoundingBox.Center.x, FBoundingBox.min.y, FBoundingBox.Center.z);
  FSubNodes[BOTTOM_LEFT_FRONT].FBoundingBox.CalculateCenter();
  SetupNode(BOTTOM_LEFT_FRONT);

  FSubNodes[BOTTOM_LEFT_BACK] := TGDOcTree.Create();
  FSubNodes[BOTTOM_LEFT_BACK].FBoundingBox.Max.Reset(FBoundingBox.max.x, FBoundingBox.Center.y, FBoundingBox.Center.z);
  FSubNodes[BOTTOM_LEFT_BACK].FBoundingBox.Min.Reset(FBoundingBox.Center.x, FBoundingBox.min.y, FBoundingBox.min.z);
  FSubNodes[BOTTOM_LEFT_BACK].FBoundingBox.CalculateCenter();
  SetupNode(BOTTOM_LEFT_BACK);

  FSubNodes[BOTTOM_RIGHT_BACK] := TGDOcTree.Create();
  FSubNodes[BOTTOM_RIGHT_BACK].FBoundingBox.Max.Reset(FBoundingBox.Center.x, FBoundingBox.Center.y, FBoundingBox.max.z);
  FSubNodes[BOTTOM_RIGHT_BACK].FBoundingBox.Min.Reset(FBoundingBox.min.x, FBoundingBox.min.y, FBoundingBox.Center.z);
  FSubNodes[BOTTOM_RIGHT_BACK].FBoundingBox.CalculateCenter();
  SetupNode(BOTTOM_RIGHT_BACK);

  FSubNodes[BOTTOM_RIGHT_FRONT] := TGDOcTree.Create();
  FSubNodes[BOTTOM_RIGHT_FRONT].FBoundingBox.Max.Reset(FBoundingBox.Center.x, FBoundingBox.Center.y, FBoundingBox.Center.z);
  FSubNodes[BOTTOM_RIGHT_FRONT].FBoundingBox.Min.Reset(FBoundingBox.min.x, FBoundingBox.min.y, FBoundingBox.min.z);
  FSubNodes[BOTTOM_RIGHT_FRONT].FBoundingBox.CalculateCenter();
  SetupNode(BOTTOM_RIGHT_FRONT);
end;

{******************************************************************************}
{* Init the octree                                                            *}
{******************************************************************************}

procedure TGDOcTree.InitOcTree();
var
  iI    : Integer;
  iCell : TGDBaseCell;
begin
  FBoundingBox.Min.Reset( 9999999999, 9999999999, 9999999999);
  FBoundingBox.Max.Reset(-9999999999,-9999999999,-9999999999);
  With CellManager do
  begin
    for iI := 0 to Cells.Count-1 do
    begin
      iCell :=  TGDBaseCell( Cells.Items[  iI ]);

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
  end;
  FBoundingBox.CalculateCenter();
  InitSubOCTreeNodes();
end;

{******************************************************************************}
{* Clear the octree                                                           *}
{******************************************************************************}

procedure TGDOcTree.Clear();
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

procedure TGDOcTree.GetVisibleCells();
var
  iCell   : TGDBaseCell;
  iMeshCell : TGDMeshCell;
  iI : Integer;
  iVertex : TGDVector;
begin
  If Not(Frustum.BoxInFrustum(FBoundingBox)) then
    exit;
                                              
  With CellManager do
  begin
    for iI := 0 to length(FCellIndexes)-1 do
    begin
      If Frustum.BoxInFrustum( TGDBaseCell( Cells.Items[  FCellIndexes[iI] ] ).BoundingBox ) then
      begin
          iCell := TGDBaseCell( Cells.Items[  FCellIndexes[iI] ] );
          iVertex := TGDBaseCell( Cells.Items[  FCellIndexes[iI] ] ).BoundingBox.Center.Copy();
          iVertex.Substract(Camera.Position.Copy());
          iCell.Distance := iVertex.Magnitude();

          If (iCell.OjectType = SO_WATERCELL) and Modes.RenderWater then
            VisibleWaterCells.Add(iCell);

          If (iCell.OjectType = SO_GRASSCELL) and Modes.RenderGrass then
            If iCell .Distance < (Settings.GrassDistance * R_GRASS_DISTANCE_STEP + (R_GRASS_DISTANCE_STEP * 10)) then
              VisibleCells.Add(iCell);

          If (iCell.OjectType = SO_TERRAINCELL) and Modes.RenderTerrain then
            VisibleCells.Add(iCell);

          If (iCell.OjectType = SO_MESHCELL) and Modes.RenderModels then
          begin
            iMeshCell := TGDMeshCell(iCell);
            If iMeshCell .Distance < iMeshCell.MaxDistance then
              VisibleCells.Add(iCell);
          end;
     end;
    end;
  end;

  for iI := 0 to 7 do
    If FSubNodes[iI] <> nil then
      FSubNodes[iI].GetVisibleCells();
end;

{******************************************************************************}
{* Render the treeboxes                                                       *}
{******************************************************************************}

procedure TGDOcTree.RenderTreeBoxes();
var
  iI : Integer;
begin
 if Not(Frustum.BoxInFrustum(FBoundingBox)) then
  exit;

 Renderer.SetColor(1,1,0,1);
 FBoundingBox.RenderWireFrame();

 for iI := 0 to 7 do
    If FSubNodes[iI] <> nil then
      FSubNodes[iI].RenderTreeBoxes();
end;

end.
