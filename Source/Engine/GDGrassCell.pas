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
  GDRenderer,
  GDFoliage,
  GDTypes,
  GDWater,
  GDSettings,
  GDTerrain,
  GDBaseCell;

type

{******************************************************************************}
{* Grasspartical class                                                        *}
{******************************************************************************}

  TGDGrassPartical = record
    Normal : TGDVector;
    Quad1 : TGDQuad;
    Quad2 : TGDQuad;
    Quad3 : TGDQuad;

    procedure InitGrassPartical(  aMove, aScale, aRotate : TGDVector  );
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

    procedure CalculateBoundingBox(aTerrain : TGDTerrain);
  public
    property TrisCount : Integer Read FTrisCount;

    constructor Create(aTerrain : TGDTerrain; aFoliage : TGDFoliage;  aWater : TGDWater; aStartX, aStartY, aEndX, aEndY : Integer);
    destructor  Destroy(); override;

    procedure RenderGrassCell( aRenderAttribute : TGDRenderAttribute );
  end;

implementation

{******************************************************************************}
{* Init the grasspartical                                                     *}
{******************************************************************************}

procedure TGDGrassPartical.InitGrassPartical( aMove, aScale, aRotate : TGDVector );
var
  iRandomRotation : TGDVector;
  iM1, iM2  : TGDMatrix;

procedure ApplyRQuad(var aQuad : TGDQuad);
begin
  aQuad.Rotate(iM1);
  aQuad.Rotate(iM2);
  aQuad.Scale( aScale );
  aQuad.Move( aMove );
  aQuad.Normal := Normal.Copy();
end;

begin
  Normal.reset(0,1,0);
  Quad1.Reset(-50, -50, 0, 50, 50, 0 );
  Quad2.Reset(-25,-50,-43.30127,25,50,43.30127);
  Quad3.Reset(-25,-50,43.30127,25,50,-43.30127);

  iRandomRotation.reset(0,Random(360),0);
  iM1.CreateRotation( iRandomRotation );
  iM2.CreateRotation( aRotate );
  iM2.ApplyToVector(Normal);

  ApplyRQuad(Quad1);
  ApplyRQuad(Quad2);
  ApplyRQuad(Quad3);
end;

{******************************************************************************}
{* Render the grasspartical                                                   *}
{******************************************************************************}

procedure TGDGrassPartical.Render();
begin
  Quad1.Render();
  Quad2.Render();
  Quad3.Render();
end;

{******************************************************************************}
{* Create the grasscell class                                                 *}
{******************************************************************************}

constructor TGDGrassCell.Create(aTerrain : TGDTerrain; aFoliage : TGDFoliage; aWater : TGDWater; aStartX, aStartY, aEndX, aEndY : Integer );
var
  iI, iJ : Integer;
  iX, iY : Integer;
  iPos, iScale, iRot : TGDVector;
  iParticalLists : array of array of TGDGrassPartical;
  iParticalCount : array of Integer;
  iHeight, iRandomHeightScale : Double;
  iTempGrassType : TGDGrassType;
  iRandomR, iRandomG, iRandomB : single;
begin
  OjectType    := SO_GRASSCELL;
  FTrisCount   := 0;
  FDisplayList := TGDGLDisplayList.Create();

  randomize();

  //set the boundingbox
  FStartPoint.X := aStartX;
  FStartPoint.Y := aStartY;
  FEndPoint.X   := aEndX;
  FEndPoint.Y   := aEndY;
  CalculateBoundingBox(aTerrain);

  //create temp arrays
  SetLength( iParticalCount, aFoliage.GrassTypes.Count);
  For iI := 0 to Length(iParticalCount)-1 do
    iParticalCount[iI] :=  Round( (Settings.FoliageDensity * TGDGrassType( aFoliage.GrassTypes.Items[iI] ).CoverOfTotal) / 100 );

  SetLength( iParticalLists, aFoliage.GrassTypes.Count);
  for iY := (FStartPoint.Y-1) to FEndPoint.Y-2 do
  begin
    iX := (FStartPoint.X-1);
    for iX := (FStartPoint.X-1) to FEndPoint.X-2 do
    begin
      If aFoliage.GrassMap[iX,iY] then
      begin
        for iI := 0 to aFoliage.GrassTypes.Count-1 do
        begin
          iTempGrassType := TGDGrassType(aFoliage.GrassTypes.Items[iI]);
          for iJ := 0 to iParticalCount[iI]-1 do
          begin
            //position
            iPos.Reset( aTerrain.TerrainPoints[ iX, iY ].FVertex.X + random(aTerrain.TriangleSize) , 0,
                        aTerrain.TerrainPoints[ iX, iY ].FVertex.Z + random(aTerrain.TriangleSize) );
            aTerrain.GetHeight(iPos.X, iPos.Z, iHeight  );

            iRandomHeightScale := iTempGrassType.Scale.Y + Random( Round( iTempGrassType.RandomScale.Y ) );
            iPos.Y := iHeight + ((50 * iRandomHeightScale) / 100 );

            //scale
            iScale.Reset( iTempGrassType.Scale.X + random( Round( iTempGrassType.RandomScale.X )),
                          iRandomHeightScale,
                          iTempGrassType.Scale.Z + random( Round( iTempGrassType.RandomScale.Z )) );

            //rotation
            aTerrain.GetRotation( iPos.X, iPos.Z, iRot );

            //create partical
            If iHeight > aWater.WaterHeight then
            begin
              SetLength(iParticalLists[iI], Length(iParticalLists[iI]) + 1 );
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
    TGDGrassType(aFoliage.GrassTypes.Items[iI]).Texture.BindTexture( GL_TEXTURE0 );
    for iJ := 0 to Length( iParticalLists[iI] )-1 do
    begin
      iRandomR := 0.75 + (Random(25)/100);
      iRandomG := 0.75 + (Random(25)/100);
      iRandomB := 0.75 + (Random(25)/100);
      glColor3f(iRandomR, iRandomG, iRandomB);
      iParticalLists[iI,iJ].Render();
    end;
  end;
  FDisplayList.EndList();

  //delete temp arrays
  For iI := 0 to Length(iParticalLists)-1 do
    SetLength( iParticalLists[iI], 0);
  SetLength( iParticalLists, 0);
  SetLength( iParticalCount, 0);
end;

{******************************************************************************}
{* Destroy the grasscell class                                                *}
{******************************************************************************}

destructor  TGDGrassCell.Destroy();
begin
  FTrisCount := 0;
  FreeAndNil(FDisplayList);
  inherited;
end;

{******************************************************************************}
{* Calculate the grasscells AABB                                              *}
{******************************************************************************}

procedure TGDGrassCell.CalculateBoundingBox(aTerrain : TGDTerrain);
var
  iX,iY : Integer;
begin
  BoundingBox.Min.Reset( aTerrain.TerrainPoints[ FStartPoint.X-1, FStartPoint.Y-1 ].FVertex.X,
                         999999999999999,
                         aTerrain.TerrainPoints[ FStartPoint.X-1, FStartPoint.Y-1 ].FVertex.Z);

  BoundingBox.Max.Reset( aTerrain.TerrainPoints[ FEndPoint.X-1, FEndPoint.Y-1 ].FVertex.X,
                         -999999999999999,
                         aTerrain.TerrainPoints[ FEndPoint.X-1, FEndPoint.Y-1 ].FVertex.Z);

  for iY := (FStartPoint.Y-1) to FEndPoint.Y-1 do
  begin
    iX := (FStartPoint.X-1);
    for iX := (FStartPoint.X-1) to FEndPoint.X-1 do
    begin
      If aTerrain.TerrainPoints[ iX,iY  ].FVertex.Y > BoundingBox.Max.Y then
        BoundingBox.Max.setY( aTerrain.TerrainPoints[ iX,iY  ].FVertex.Y);
      If aTerrain.TerrainPoints[ iX,iY  ].FVertex.Y < BoundingBox.Min.Y then
        BoundingBox.Min.setY( aTerrain.TerrainPoints[ iX,iY  ].FVertex.Y);
    end;
  end;
  BoundingBox.Max.SetY( BoundingBox.Max.Y+150 );
  BoundingBox.CalculateCenter();
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
                          Renderer.SetColor(1,0,0,1);
                          BoundingBox.RenderWireFrame();
                        end;
    RA_NORMALS        : begin
                          //grass doesn`t have normals
                        end;
    end;
end;

end.
