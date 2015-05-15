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
    constructor Create(aStartX, aStartY, aEndX, aEndY, aStartU1, aStartV1, aStepU1, aStepV1,
                       aStartU2, aStartV2, aStepU2, aStepV2 : Double);
    destructor  Destroy(); override;

    procedure RenderWaterCell( aRenderAttribute : TGDRenderAttribute );
  end;

implementation

uses
  GDMap;

{******************************************************************************}
{* Create the watercell class                                                 *}
{******************************************************************************}

constructor TGDWaterCell.Create();
var
 iI, iJ : Double;
 iStepX, iStepY : Double;
 iCurrentU1, iCurrentV1 : Double;
 iCurrentU2, iCurrentV2 : Double;

begin
  OjectType := SO_WATERCELL;
  FDisplayList := TGDGLDisplayList.Create();
  BoundingBox.Min.Reset(aStartX, Map.Water.WaterHeight, aStartY);
  BoundingBox.Max.Reset(aEndX, Map.Water.WaterHeight, aEndY);

  iStepX := (Map.Water.BoundingBox.Max.X + Abs(Map.Water.BoundingBox.Min.X)) / (Map.Water.CellCountX * Map.Water.CellDivX);
  iStepY := (Map.Water.BoundingBox.Max.Z + Abs(Map.Water.BoundingBox.Min.Z)) / (Map.Water.CellCountY * Map.Water.CellDivY);

  FDisplayList.InitDisplayList();
  FDisplayList.StartList();

  iCurrentU1 := aStartU1;
  iCurrentU2 := aStartU2;
  iI := BoundingBox.Min.X;
  while (iI <= (BoundingBox.Max.X-iStepX)) do
  begin
    iJ := BoundingBox.Min.Z;
    iCurrentV1 := aStartV1;
    iCurrentV2 := aStartV2;
    glBegin(GL_TRIANGLE_STRIP);

    glMultiTexCoord2f(GL_TEXTURE0, iCurrentU1 + aStepU1, iCurrentV1 );
    glMultiTexCoord2f(GL_TEXTURE1, iCurrentU2 + aStepU2, iCurrentV2 );
    glVertex3f(iI + iStepX, Map.Water.WaterHeight, iJ);
    glMultiTexCoord2f(GL_TEXTURE0, iCurrentU1, iCurrentV1 );
    glMultiTexCoord2f(GL_TEXTURE1, iCurrentU2, iCurrentV2 );
    glVertex3f(iI, Map.Water.WaterHeight, iJ);

    while (iJ <= (BoundingBox.Max.Z)) do
    begin
      glMultiTexCoord2f(GL_TEXTURE0, iCurrentU1 + aStepU1, iCurrentV1  );
      glMultiTexCoord2f(GL_TEXTURE1, iCurrentU2 + aStepU2, iCurrentV2  );
      glVertex3f(iI + iStepX, Map.Water.WaterHeight, iJ);
      glMultiTexCoord2f(GL_TEXTURE0, iCurrentU1, iCurrentV1  );
      glMultiTexCoord2f(GL_TEXTURE1, iCurrentU2, iCurrentV2  );
      glVertex3f(iI, Map.Water.WaterHeight, iJ);

      iJ := iJ + iStepY;
      iCurrentV1 := iCurrentV1 + aStepV1;
      iCurrentV2 := iCurrentV2 + aStepV2;
    end;

    iCurrentU1 := iCurrentU1 + aStepU1;
    iCurrentU2 := iCurrentU2 + aStepU2;
    iI := iI + iStepX;
    glEnd();
  end;

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

procedure TGDWaterCell.RenderWaterCell( aRenderAttribute : TGDRenderAttribute );
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
