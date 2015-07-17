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
unit GDWaterCell;

{$MODE Delphi}

{******************************************************************************}
{* Holds the watercell class                                                  *}
{******************************************************************************}

interface

uses
  SysUtils,
  dglOpenGL,
  GDWater,
  GDConstants,
  GDGLObjects,
  GDRenderer,
  GDBaseCell;

type

{******************************************************************************}
{* watercell class                                                            *}
{******************************************************************************}

  TGDWaterCell = class (TGDBaseCell)
  private
    FDisplayList : TGDGLDisplayList;
  public
    constructor Create(aWater : TGDWater; aStartX, aStartY, aEndX, aEndY, aStartU, aStartV, aEndU, aEndV : Double);
    destructor  Destroy(); override;

    procedure Render( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor ); override;
  end;

implementation

{******************************************************************************}
{* Create the watercell class                                                 *}
{******************************************************************************}

constructor TGDWaterCell.Create(aWater : TGDWater; aStartX, aStartY, aEndX, aEndY, aStartU, aStartV, aEndU, aEndV : Double);
begin
  OjectType := SO_WATERCELL;
  FDisplayList := TGDGLDisplayList.Create();
  BoundingBox.Min.Reset(aStartX, aWater.WaterHeight, aStartY);
  BoundingBox.Max.Reset(aEndX, aWater.WaterHeight, aEndY);

  FDisplayList.StartList();
  glBegin(GL_TRIANGLE_STRIP);
    glMultiTexCoord2f(GL_TEXTURE0, aStartU, aStartV);
    glVertex3f(aStartX, aWater.WaterHeight, aStartY);

    glMultiTexCoord2f(GL_TEXTURE0, aStartU, aEndV);
    glVertex3f(aStartX, aWater.WaterHeight, aEndY);

    glMultiTexCoord2f(GL_TEXTURE0, aEndU, aStartV);
    glVertex3f(aEndX, aWater.WaterHeight, aStartY);

    glMultiTexCoord2f(GL_TEXTURE0, aEndU, aEndV);
    glVertex3f(aEndX, aWater.WaterHeight, aEndY);
  glEnd();
  FDisplayList.EndList();
end;

{******************************************************************************}
{* Destroy the watercell class                                                *}
{******************************************************************************}

destructor  TGDWaterCell.Destroy();
begin
  FreeAndNil(FDisplayList);
  Inherited;
end;

{******************************************************************************}
{* Render the watercell                                                       *}
{******************************************************************************}

procedure TGDWaterCell.Render( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor );
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
                          //water doesn`t have normals
                        end;
    end;
end;

end.
