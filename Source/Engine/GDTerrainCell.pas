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
unit GDTerrainCell;

{$MODE Delphi}

{******************************************************************************}
{* Holds the terraincell class
{******************************************************************************}

interface

uses
  SysUtils,
  Types,
  dglOpenGL,
  GDConstants,
  GDGLObjects,
  GDTerrain,
  GDBaseCell;

type

{******************************************************************************}
{* Terraincell class                                                          *}
{******************************************************************************}

  TGDTerrainCell = class (TGDBaseCell)
  private
    FStartPoint  : TPoint;
    FEndPoint    : TPoint;
    FDisplayList : TGDGLDisplayList;

    procedure CreateDisplayList();
    procedure CalculateBoundingBox();
  public
    constructor Create();
    destructor  Destroy(); override;

    procedure InitTerrainCell(aStartX, aStartY, aEndX, aEndY : Integer);
    procedure RenderTerrainCell( aRenderAttribute : TGDRenderAttribute );
  end;

implementation

{******************************************************************************}
{* Create the terraincell class                                               *}
{******************************************************************************}

constructor TGDTerrainCell.Create();
begin
  Inherited;
  OjectType := SO_TERRAINCELL;
  FDisplayList := TGDGLDisplayList.Create();
end;

{******************************************************************************}
{* Destroy the terraincell class                                              *}
{******************************************************************************}

destructor  TGDTerrainCell.Destroy();
begin
  FreeAndNil(FDisplayList);
  Inherited;
end;

{******************************************************************************}
{* Create the terraincell displaylist                                         *}
{******************************************************************************}

procedure TGDTerrainCell.CreateDisplayList();
var
  iX, iY: Integer;
  iTempX1, iTempX2 : Double;
  iTempY1, iTempY2 : Double;

procedure SendTerrainPoint(aX,aY : integer);
begin
  With Terrain do
  begin
    glMultiTexCoord2fv(GL_TEXTURE0, TerrainPoints[aX,aY].FColorUVCoords.ArrayPointer );
    glMultiTexCoord2fv(GL_TEXTURE1, TerrainPoints[aX,aY].FDetailUVCoords.ArrayPointer );
    glMultiTexCoord2fv(GL_TEXTURE2, TerrainPoints[aX,aY].FCausticUVCoords.ArrayPointer );
    glNormal3fv( TerrainPoints[aX,aY].FNormal.ArrayPointer );
    glVertex3fv( TerrainPoints[aX,aY].FVertex.ArrayPointer );
  end;
end;

begin
  FDisplayList.InitDisplayList();
  FDisplayList.StartList();

  iTempX1 := ((FEndPoint.X-1)) / (Terrain.TerrainWidth-1);
  iTempY1 := ( (((Terrain.TerrainHeight-1) + CELLSIZE ) - (FEndPoint.Y-1))) / (Terrain.TerrainHeight-1);
  iTempX2 := Frac( iTempX1 );
  iTempY2 := Frac( iTempY1 );

  for iY := (FStartPoint.Y-1) to FEndPoint.Y-2 do
  begin
    glBegin(GL_TRIANGLE_STRIP);
      iX := (FStartPoint.X-1);

      SendTerrainPoint(iX,iY);
      SendTerrainPoint(iX,iY+1);

      for iX := (FStartPoint.X-1) to FEndPoint.X-1 do
      begin
        SendTerrainPoint(iX,iY);
        SendTerrainPoint(iX,iY+1);
      end;
    glEnd();
  end;
  FDisplayList.EndList();
end;

{******************************************************************************}
{* Calculate the terraincell AABB                                             *}
{******************************************************************************}

procedure TGDTerrainCell.CalculateBoundingBox();
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
  BoundingBox.CalculateCenter();
end;

{******************************************************************************}
{* Init the terraincell                                                       *}
{******************************************************************************}

procedure TGDTerrainCell.InitTerrainCell(aStartX, aStartY, aEndX, aEndY : Integer);
begin
  FStartPoint.X := aStartX;
  FStartPoint.Y := aStartY;
  FEndPoint.X   := aEndX;
  FEndPoint.Y   := aEndY;
  CalculateBoundingBox();
  CreateDisplayList();
end;

{******************************************************************************}
{* Render the terraincell                                                     *}
{******************************************************************************}

procedure TGDTerrainCell.RenderTerrainCell( aRenderAttribute : TGDRenderAttribute );
var
  iX,iY : Integer;
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
                          glColor4f(1,0.5,0.25,1);
                          With Terrain do
                          begin
                            for iY := (FStartPoint.Y-1) to FEndPoint.Y-1 do
                            begin
                              iX := (FStartPoint.X-1);
                              for iX := (FStartPoint.X-1) to FEndPoint.X-1 do
                              begin
                                glBegin( GL_LINES);
                                  glVertex3f( TerrainPoints[iX,iY].FVertex.X, TerrainPoints[iX,iY].FVertex.Y, TerrainPoints[iX,iY].FVertex.Z );
                                  glVertex3f((TerrainPoints[iX,iY].FNormal.X * R_NORMAL_LENGTH) + TerrainPoints[iX,iY].FVertex.X,
                                             (TerrainPoints[iX,iY].FNormal.Y * R_NORMAL_LENGTH) + TerrainPoints[iX,iY].FVertex.Y,
                                             (TerrainPoints[iX,iY].FNormal.Z * R_NORMAL_LENGTH) + TerrainPoints[iX,iY].FVertex.Z );
                                glEnd;
                              end;
                            end;
                          end;
                        end;
    end;
end;

end.
