{*******************************************************************************
*                            Genesis Device Engine                             *
*                   Copyright © 2007-2015 Luuk van Venrooij                    *
*                        http://www.luukvanvenrooij.nl                         *
*                         luukvanvenrooij84@gmail.com                          *
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
unit GDGrassCell;

{$MODE Delphi}

{******************************************************************************}
{* This unit hold the grasscell class. A grasscell is a small patch on the    *}
{* terrain which has grass on it. Where the patches are depends on the grass  *}
{* settings.                                                                  *}
{******************************************************************************}

interface

uses
  SysUtils,
  Types,
  dglOpenGL,
  GDConstants,
  GDGLObjects,
  GDFoliage,
  GDTypes,
  GDTerrain,
  GDWater,
  GDSettings,
  GDBaseCell;

type

{******************************************************************************}
{* Grasspartical class                                                        *}
{******************************************************************************}

  TGDGrassPartical = class
  private
    FNormal : TGDVector;
    FQuad1 : TGDQuad;
    FQuad2 : TGDQuad;
    FQuad3 : TGDQuad;
  public
    constructor Create();
    destructor  Destroy();

    procedure InitGrassPartical(  aMove, aScale, aRotate : TGDVector  );
    procedure Clear();
    procedure Render();
  end;

{******************************************************************************}
{* Grasscell class                                                            *}
{******************************************************************************}

  TGDGrassCell = class (TGDBaseCell)
  private
    FStartPoint  : TPoint;
    FEndPoint    : TPoint;
    FDisplayList : TGDGLDisplayList;
    FTrisCount   : Integer;

    procedure CalculateBoundingBox();
  public
    property TrisCount : Integer Read FTrisCount;

    constructor Create();
    destructor  Destroy(); override;

    procedure InitGrassCell( aStartX, aStartY, aEndX, aEndY : Integer );
    procedure Clear();

    procedure RenderGrassCell( aRenderAttribute : TGDRenderAttribute );
  end;

implementation

{******************************************************************************}
{* Create the grasspartical class                                             *}
{******************************************************************************}

constructor TGDGrassPartical.Create();
begin
  FNormal := TGDVector.Create(0,1,0);
  FQuad1 := TGDQuad.Create(-50, -50, 0, 50, 50, 0 );
  FQuad2 := TGDQuad.Create(-25,-50,-43.30127,25,50,43.30127);
  FQuad3 := TGDQuad.Create(-25,-50,43.30127,25,50,-43.30127);
end;

{******************************************************************************}
{* Destroy the grasspartical class                                            *}
{******************************************************************************}

destructor  TGDGrassPartical.Destroy();
begin
  FreeAndNil(FNormal);
  FreeAndNil(FQuad1);
  FreeAndNil(FQuad2);
  FreeAndNil(FQuad3);
end;

{******************************************************************************}
{* Init the grasspartical                                                     *}
{******************************************************************************}

procedure TGDGrassPartical.InitGrassPartical( aMove, aScale, aRotate : TGDVector );
var
  iRandomRotation : TGDVector;
  iM : TGDMatrix;

procedure ApplyRotationToQuad(aQuad : TGDQuad);
var
  iI : integer;
begin
  for iI := 0 to length(aQuad.Vertices)-1do
    iM.ApplyToVector(aQuad.Vertices[iI]);
end;

begin
  iRandomRotation := TGDVector.Create(0,Random(360),0);
  iM := TGDMatrix.Create();
  iM.CreateRotation( iRandomRotation );
  ApplyRotationToQuad(FQuad1);
  ApplyRotationToQuad(FQuad2);
  ApplyRotationToQuad(FQuad3);
  iM.EmptyMatrix();
  iM.CreateRotation( aRotate );
  ApplyRotationToQuad(FQuad1);
  ApplyRotationToQuad(FQuad2);
  ApplyRotationToQuad(FQuad3);
  iM.ApplyToVector(FNormal);
  FreeAndNil(iM);
  FreeAndNil(iRandomRotation);
  FQuad1.Scale( aScale );
  FQuad2.Scale( aScale );
  FQuad3.Scale( aScale );
  FQuad1.Move( aMove );
  FQuad2.Move( aMove );
  FQuad3.Move( aMove );
end;

{******************************************************************************}
{* Clear the grasspartical                                                    *}
{******************************************************************************}

procedure TGDGrassPartical.Clear();
begin
  FQuad1.Reset(-50, -50, 0, 50, 50, 0 );
  FQuad2.Reset(-25,-50,-43.30127,25,50,43.30127);
  FQuad3.Reset(-25,-50,43.30127,25,50,-43.30127);
end;

{******************************************************************************}
{* Render the grasspartical                                                   *}
{******************************************************************************}

procedure TGDGrassPartical.Render();
begin
  FQuad1.Render(FNormal);
  FQuad2.Render(FNormal);
  FQuad3.Render(FNormal);
end;

{******************************************************************************}
{* Create the grasscell class                                                 *}
{******************************************************************************}

constructor TGDGrassCell.Create();
begin
  Inherited;
  OjectType    := SO_GRASSCELL;
  FTrisCount   := 0;
  FDisplayList := TGDGLDisplayList.Create();
end;

{******************************************************************************}
{* Destroy the grasscell class                                                *}
{******************************************************************************}

destructor  TGDGrassCell.Destroy();
begin
  FreeAndNil(FDisplayList);
  inherited;
end;

{******************************************************************************}
{* Init the grasscell                                                         *}
{******************************************************************************}

procedure TGDGrassCell.InitGrassCell(  aStartX, aStartY, aEndX, aEndY : Integer );
var
  iI, iJ : Integer;
  iX, iY : Integer;
  iPos, iScale, iRot : TGDVector;
  iParticalLists : array of array of TGDGrassPartical;
  iParticalCount : array of Integer;
  iHeight, iRandomHeightScale : Double;
  iTempGrassType : TGDGrassType;
begin
  randomize();

  iPos   := TGDVector.Create();
  iScale := TGDVector.Create();
  iRot   := TGDVector.Create();

  //set the boundingbox
  FStartPoint.X := aStartX;
  FStartPoint.Y := aStartY;
  FEndPoint.X   := aEndX;
  FEndPoint.Y   := aEndY;
  CalculateBoundingBox();

  //create temp arrays
  SetLength( iParticalCount, Foliage.GrassTypes.Count);
  For iI := 0 to Length(iParticalCount)-1 do
    iParticalCount[iI] :=  Round( (Settings.GrassDensity * TGDGrassType( Foliage.GrassTypes.GetObjectI(iI) ).CoverOfTotal) / 100 );

  SetLength( iParticalLists, Foliage.GrassTypes.Count);
  for iY := (FStartPoint.Y-1) to FEndPoint.Y-2 do
  begin
    iX := (FStartPoint.X-1);
    for iX := (FStartPoint.X-1) to FEndPoint.X-2 do
    begin
      If Foliage.GrassMap[iX,iY] then
      begin
        for iI := 0 to Foliage.GrassTypes.Count-1 do
        begin
          iTempGrassType := TGDGrassType(Foliage.GrassTypes.GetObjectI(iI));
          for iJ := 0 to iParticalCount[iI]-1 do
          begin
            //position
            iPos.Reset( Terrain.TerrainPoints[ iX, iY ].FVertex.X + random(Terrain.TriangleSize) , 0,
                        Terrain.TerrainPoints[ iX, iY ].FVertex.Z + random(Terrain.TriangleSize) );
            Terrain.GetHeight(iPos.X, iPos.Z, iHeight  );

            iRandomHeightScale := iTempGrassType.Scale.Y + Random( Round( iTempGrassType.RandomScale.Y ) );
            iPos.Y := iHeight + ((50 * iRandomHeightScale) / 100 );

            //scale
            iScale.Reset( iTempGrassType.Scale.X + random( Round( iTempGrassType.RandomScale.X )),
                          iRandomHeightScale,
                          iTempGrassType.Scale.Z + random( Round( iTempGrassType.RandomScale.Z )) );

            //rotation
            Terrain.GetRotation( iPos.X, iPos.Z, iRot );

            //create partical
            If iHeight > Water.WaterHeight then
            begin
              SetLength(iParticalLists[iI], Length(iParticalLists[iI]) + 1 );
              iParticalLists[iI,Length(iParticalLists[iI])-1] := TGDGrassPartical.Create();
              iParticalLists[iI,Length(iParticalLists[iI])-1].InitGrassPartical( iPos, iScale, iRot );
            end;

            FTrisCount := FTrisCount + 6;
          end;
        end;
      end;
    end;
  end;

  //create displaylist for this cell
  FDisplayList.InitDisplayList();
  FDisplayList.StartList();
  For iI := 0 to Length(iParticalLists)-1 do
  begin
    TGDGrassType(Foliage.GrassTypes.GetObjectI(iI)).Texture.BindTexture( GL_TEXTURE0 );
    for iJ := 0 to Length( iParticalLists[iI] )-1 do
      iParticalLists[iI,iJ].Render();
  end;
  FDisplayList.EndList();

  //delete temp arrays
  For iI := 0 to Length(iParticalLists)-1 do
  begin
    for iJ := 0 to Length( iParticalLists[iI] )-1 do
    begin
      iParticalLists[iI,iJ].Destroy();
    end;
    SetLength( iParticalLists[iI], 0);
  end;
  SetLength( iParticalLists, 0);
  SetLength( iParticalCount, 0);

  FreeAndNil(iPos);
  FreeAndNil(iScale);
  FreeAndNil(iRot);
end;

{******************************************************************************}
{* Calculate the grasscells AABB                                              *}
{******************************************************************************}

procedure TGDGrassCell.CalculateBoundingBox();
var
  iX,iY : Integer;
begin
  BoundingBox.Min.X :=  Terrain.TerrainPoints[ FStartPoint.X-1, FStartPoint.Y-1 ].FVertex.X;
  BoundingBox.Min.Z :=  Terrain.TerrainPoints[ FStartPoint.X-1, FStartPoint.Y-1 ].FVertex.Z;
  BoundingBox.Max.X :=  Terrain.TerrainPoints[ FEndPoint.X-1, FEndPoint.Y-1 ].FVertex.X;
  BoundingBox.Max.Z :=  Terrain.TerrainPoints[ FEndPoint.X-1, FEndPoint.Y-1 ].FVertex.Z;

  BoundingBox.Min.Y := 999999999999999;
  BoundingBox.Max.Y := -999999999999999;

  for iY := (FStartPoint.Y-1) to FEndPoint.Y-1 do
  begin
    iX := (FStartPoint.X-1);
    for iX := (FStartPoint.X-1) to FEndPoint.X-1 do
    begin
      If Terrain.TerrainPoints[ iX,iY  ].FVertex.Y > BoundingBox.Max.Y then
        BoundingBox.Max.Y := Terrain.TerrainPoints[ iX,iY  ].FVertex.Y;
      If Terrain.TerrainPoints[ iX,iY  ].FVertex.Y < BoundingBox.Min.Y then
        BoundingBox.Min.Y := Terrain.TerrainPoints[ iX,iY  ].FVertex.Y;
    end;
  end;
  BoundingBox.Max.Y := BoundingBox.Max.Y + 150;
  BoundingBox.CalculateCenter();
end;

{******************************************************************************}
{* Clear the grasscell                                                        *}
{******************************************************************************}

procedure TGDGrassCell.Clear();
begin
  FTrisCount := 0;
  FDisplayList.Clear();
end;

{******************************************************************************}
{* Render the grasscell                                                       *}
{******************************************************************************}

procedure TGDGrassCell.RenderGrassCell( aRenderAttribute : TGDRenderAttribute );
begin
  Case aRenderAttribute Of
    RA_NORMAL         : begin
                          FDisplayList.CallList();
                        end;
    RA_FRUSTUM_BOXES  : begin
                          glColor4f(1,0,0,1);
                          BoundingBox.RenderWireFrame();
                        end;
    RA_NORMALS        : begin
                          //grass doesn`t have normals
                        end;
    end;
end;

end.
