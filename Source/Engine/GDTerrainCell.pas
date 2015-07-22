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
  GDTerrain,
  GDRenderer,
  GDGLObjects,
  GDBaseCell;

type

{******************************************************************************}
{* Terraincell class                                                          *}
{******************************************************************************}

  TGDTerrainCell = class (TGDBaseCell)
  private
    FTerrain     : TGDTerrain;
    FStartPoint  : TPoint;
    FEndPoint    : TPoint;
    FDisplayList : TGDGLDisplayList;

    procedure CalculateBoundingBox();
  public
    constructor Create( aTerrain : TGDTerrain; aStartX, aStartY, aEndX, aEndY : Integer);
    destructor  Destroy(); override;

    procedure Render( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor ); override;
  end;

implementation

{******************************************************************************}
{* Create the terraincell class                                               *}
{******************************************************************************}

constructor TGDTerrainCell.Create( aTerrain : TGDTerrain; aStartX, aStartY, aEndX, aEndY : Integer);
var
  iX, iY: Integer;

procedure SendTerrainPoint(aX,aY : integer);
begin
  With FTerrain do
  begin
    glNormal3fv( GetPoint(aX,aY).Normal.ArrayPointer );
    glTexCoord2fv( GetPoint(aX,aY).UVCoords.ArrayPointer );
    glVertex3fv( GetPoint(aX,aY).Vertex.ArrayPointer );
  end;
end;

begin
  OjectType    := SO_TERRAINCELL;
  FTerrain     := aTerrain;
  FDisplayList := TGDGLDisplayList.Create();

  FStartPoint.X := aStartX;
  FStartPoint.Y := aStartY;
  FEndPoint.X   := aEndX;
  FEndPoint.Y   := aEndY;

  CalculateBoundingBox();

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
  BoundingBox.Min.Reset(  FTerrain.GetPoint( FStartPoint.X-1, FStartPoint.Y-1 ).Vertex.X,
                         999999999999999,
                         FTerrain.GetPoint( FStartPoint.X-1, FStartPoint.Y-1 ).Vertex.Z);
  BoundingBox.Max.Reset( FTerrain.GetPoint( FEndPoint.X-1, FEndPoint.Y-1 ).Vertex.X,
                         -999999999999999,
                         FTerrain.GetPoint( FEndPoint.X-1, FEndPoint.Y-1 ).Vertex.Z);


  for iY := (FStartPoint.Y-1) to FEndPoint.Y-1 do
  begin
    iX := (FStartPoint.X-1);
    for iX := (FStartPoint.X-1) to FEndPoint.X-1 do
    begin
      If FTerrain.GetPoint( iX,iY ).Vertex.Y > BoundingBox.Max.Y then
        BoundingBox.Max.setY(FTerrain.GetPoint( iX,iY ).Vertex.Y);
      If FTerrain.GetPoint( iX,iY ).Vertex.Y < BoundingBox.Min.Y then
        BoundingBox.Min.setY(FTerrain.GetPoint( iX,iY ).Vertex.Y);
    end;
  end;
  BoundingBox.CalculateCenter();
end;

{******************************************************************************}
{* Render the terraincell                                                     *}
{******************************************************************************}

procedure TGDTerrainCell.Render( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor );
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
                          With FTerrain do
                          begin
                            for iY := (FStartPoint.Y-1) to FEndPoint.Y-1 do
                            begin
                              iX := (FStartPoint.X-1);
                              for iX := (FStartPoint.X-1) to FEndPoint.X-1 do
                              begin
                                glBegin( GL_LINES);
                                  glVertex3f( GetPoint(iX,iY).Vertex.X, GetPoint(iX,iY).Vertex.Y, GetPoint(iX,iY).Vertex.Z );
                                  glVertex3f((GetPoint(iX,iY).Normal.X * R_NORMAL_LENGTH) + GetPoint(iX,iY).Vertex.X,
                                             (GetPoint(iX,iY).Normal.Y * R_NORMAL_LENGTH) + GetPoint(iX,iY).Vertex.Y,
                                             (GetPoint(iX,iY).Normal.Z * R_NORMAL_LENGTH) + GetPoint(iX,iY).Vertex.Z );
                                glEnd;
                              end;
                            end;
                          end;
                        end;
    end;
end;

end.
