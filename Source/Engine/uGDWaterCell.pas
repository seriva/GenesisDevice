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
unit uGDWaterCell;

{$MODE Delphi}

{******************************************************************************}
{* Holds the watercell class                                                  *}
{******************************************************************************}

interface

uses
  SysUtils,
  dglOpenGL,
  uGDWater,
  uGDConstants,
  uGDGLWrappers,
  uGDTypes,
  uGDTypesGenerics,
  uGDBaseCell;

type

{******************************************************************************}
{* watercell class                                                            *}
{******************************************************************************}

  TGDWaterCell = class (TGDBaseCell)
  private
    FIndexes : TGDGLIndexBuffer;
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
var
  iIdxs : TGDIndexList;
  iV : TGDVertex_V_UV;
begin
  OjectType := SO_WATERCELL;
  FIndexes := TGDGLIndexBuffer.Create();
  iIdxs := TGDIndexList.Create();

  BoundingBox.Min.Reset(aStartX, aWater.WaterHeight, aStartY);
  BoundingBox.Max.Reset(aEndX, aWater.WaterHeight, aEndY);
  BoundingBox.CalculateCenter();

  iV.Vertex.Reset(aStartX, aWater.WaterHeight, aStartY);
  iV.UV.Reset(aStartU, aStartV);
  iIdxs.Add(aWater.AddVertex(iV));

  iV.Vertex.Reset(aStartX, aWater.WaterHeight, aEndY);
  iV.UV.Reset(aStartU, aEndV);
  iIdxs.Add(aWater.AddVertex(iV));

  iV.Vertex.Reset(aEndX, aWater.WaterHeight, aStartY);
  iV.UV.Reset(aEndU, aStartV);
  iIdxs.Add(aWater.AddVertex(iV));

  iV.Vertex.Reset(aEndX, aWater.WaterHeight, aEndY);
  iV.UV.Reset(aEndU, aEndV);
  iIdxs.Add(aWater.AddVertex(iV));

  FIndexes.Bind();
  FIndexes.Update(iIdxs, GL_STATIC_DRAW);
  FIndexes.Unbind();
  FreeAndNil(iIdxs);
end;

{******************************************************************************}
{* Destroy the watercell class                                                *}
{******************************************************************************}

destructor  TGDWaterCell.Destroy();
begin
  FreeAndNil(FIndexes);
  Inherited;
end;

{******************************************************************************}
{* Render the watercell                                                       *}
{******************************************************************************}

procedure TGDWaterCell.Render( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor );
begin
  Case aRenderAttribute Of
    RA_NORMAL         : begin
                          FIndexes.Bind();
                          FIndexes.Render(GL_TRIANGLE_STRIP);
                          FIndexes.Unbind();
                        end;
    RA_FRUSTUM_BOXES  : BoundingBox.RenderWireFrame();
    RA_NORMALS        : //water doesn`t have normals
    end;
end;

end.
