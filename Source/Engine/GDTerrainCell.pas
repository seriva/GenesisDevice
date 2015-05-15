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
  GDRenderer,
  GDGLObjects,
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

    procedure CalculateBoundingBox();
  public
    constructor Create(aStartX, aStartY, aEndX, aEndY : Integer);
    destructor  Destroy(); override;

    procedure RenderTerrainCell( aRenderAttribute : TGDRenderAttribute );
  end;

implementation

uses
  GDMap;

{******************************************************************************}
{* Create the terraincell class                                               *}
{******************************************************************************}

constructor TGDTerrainCell.Create(aStartX, aStartY, aEndX, aEndY : Integer);
var
  iX, iY: Integer;

procedure SendTerrainPoint(aX,aY : integer);
begin
  With Map.Terrain do
  begin
    glMultiTexCoord2fv(GL_TEXTURE0, TerrainPoints[aX,aY].FColorUVCoords.ArrayPointer );
    glMultiTexCoord2fv(GL_TEXTURE1, TerrainPoints[aX,aY].FDetailUVCoords.ArrayPointer );
    glMultiTexCoord2fv(GL_TEXTURE2, TerrainPoints[aX,aY].FCausticUVCoords.ArrayPointer );
    glNormal3fv( TerrainPoints[aX,aY].FNormal.ArrayPointer );
    glVertex3fv( TerrainPoints[aX,aY].FVertex.ArrayPointer );
  end;
end;

begin
  OjectType := SO_TERRAINCELL;
  FDisplayList := TGDGLDisplayList.Create();
  FStartPoint.X := aStartX;
  FStartPoint.Y := aStartY;
  FEndPoint.X   := aEndX;
  FEndPoint.Y   := aEndY;

  CalculateBoundingBox();

  FDisplayList.InitDisplayList();
  FDisplayList.StartList();
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
{* Destroy the terraincell class                                              *}
{******************************************************************************}

destructor  TGDTerrainCell.Destroy();
begin
  FreeAndNil(FDisplayList);
  Inherited;
end;

{******************************************************************************}
{* Calculate the terraincell AABB                                             *}
{******************************************************************************}

procedure TGDTerrainCell.CalculateBoundingBox();
var
  iX,iY : Integer;
begin
  BoundingBox.Min.Reset(  Map.Terrain.TerrainPoints[ FStartPoint.X-1, FStartPoint.Y-1 ].FVertex.X,
                         999999999999999,
                         Map.Terrain.TerrainPoints[ FStartPoint.X-1, FStartPoint.Y-1 ].FVertex.Z);
  BoundingBox.Max.Reset( Map.Terrain.TerrainPoints[ FEndPoint.X-1, FEndPoint.Y-1 ].FVertex.X,
                         -999999999999999,
                         Map.Terrain.TerrainPoints[ FEndPoint.X-1, FEndPoint.Y-1 ].FVertex.Z);


  for iY := (FStartPoint.Y-1) to FEndPoint.Y-1 do
  begin
    iX := (FStartPoint.X-1);
    for iX := (FStartPoint.X-1) to FEndPoint.X-1 do
    begin
      If Map.Terrain.TerrainPoints[ iX,iY  ].FVertex.Y > BoundingBox.Max.Y then
        BoundingBox.Max.setY(Map.Terrain.TerrainPoints[ iX,iY  ].FVertex.Y);
      If Map.Terrain.TerrainPoints[ iX,iY  ].FVertex.Y < BoundingBox.Min.Y then
        BoundingBox.Min.setY(Map.Terrain.TerrainPoints[ iX,iY  ].FVertex.Y);
    end;
  end;
  BoundingBox.CalculateCenter();
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
                          Renderer.SetColor(1,0,0,1);
                          BoundingBox.RenderWireFrame();
                        end;
    RA_NORMALS        : begin
                          Renderer.SetColor(1,0.5,0.25,1);
                          With Map.Terrain do
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
