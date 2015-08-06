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
unit GDSkyDome;

{$MODE Delphi}

{******************************************************************************}
{* Holds the skydome class                                                    *}
{******************************************************************************}

interface

uses
  Classes,
  SysUtils,
  dglOpenGL,
  GDTexture,
  GDTypes,
  GDGLWrappers,
  GDConstants,
  GDCamera,
  GDSettings,
  GDModes,
  GDResources,
  GDResource,
  GDTypesGenerics;

type

{******************************************************************************}
{* Skydome class                                                              *}
{******************************************************************************}

  TGDSkyDome = class
  private
    FIndexBuffer   : TGDGLIndexBuffer;
    FVertexBuffer  : TGDGLVertexBuffer;
    FSkyTexture    : TGDTexture;
    FTriangleCount : Integer;

    procedure CalculateDome(aSize : Double );
  public
    property TriangleCount : integer read FTriangleCount;

    constructor Create();
    destructor  Destroy(); override;

    procedure InitSkyDome( aSkyTexture : String; aSize : Double );
    procedure Clear();
    Procedure Render();
  end;

implementation

uses
  GDRenderer;

{******************************************************************************}
{* Create the skydome class                                                   *}
{******************************************************************************}

constructor TGDSkyDome.Create();
begin
end;

{******************************************************************************}
{* Destroy the skydome class                                                  *}
{******************************************************************************}

destructor  TGDSkyDome.Destroy();
begin
  Clear();
  Inherited;
end;

{******************************************************************************}
{* Clear the skydome                                                          *}
{******************************************************************************}

procedure TGDSkyDome.Clear();
begin
  FreeAndNil(FIndexBuffer);
  FreeAndNil(FVertexBuffer);
  FTriangleCount := 0;
  Resources.RemoveResource(TGDResource(FSkyTexture));
end;

{******************************************************************************}
{* Calculate the skydome                                                      *}
{******************************************************************************}

procedure TGDSkyDome.CalculateDome( aSize : Double );
var
  iRotationStep : double;
  iRotationZ, iRotationY : Double;
  iStartPoint : TGDVector;
  iI, iJ, iX, iY : Integer;
  iUStep, iVStep: Double;
  iMatrix : TGDMatrix;
  iIndexes : TGDIndexList;
  iVertices : TGDVertex_V_UV_List;
  iV : TGDVertex_V_UV;
begin
  iIndexes  := TGDIndexList.Create();
  iVertices := TGDVertex_V_UV_List.Create();
  FIndexBuffer  := TGDGLIndexBuffer.Create();
  FVertexBuffer := TGDGLVertexBuffer.Create();

  iRotationStep := 360/SKY_COMPLEXITY;
  iStartPoint.Reset(aSize, 0, 0 );
  iUStep := 1 / (SKY_COMPLEXITY);
  iVStep := 1 / (SKY_COMPLEXITY div 4);
  iRotationZ := 0;
  For iI := (SKY_COMPLEXITY div 4) downto 0 do
  begin
    iRotationY := 0;
    For iJ := 0 to SKY_COMPLEXITY do
    begin
      iV.Vertex := iStartPoint.Copy();
      iMatrix.CreateRotationY(iRotationY);
      iMatrix.ApplyToVector(iV.Vertex);
      iV.UV.Reset(iJ * iUStep, iI* iVStep );
      iVertices.Add(iV);
      iRotationY := iRotationY + iRotationStep;
    end;
    iStartPoint.Reset( aSize, 0, 0 );
    iRotationZ := iRotationZ - iRotationStep;
    iMatrix.CreateRotationZ(iRotationZ);
    iMatrix.ApplyToVector(iStartPoint);
  end;
  FVertexBuffer.Bind(VL_NONE);
  FVertexBuffer.Update(iVertices, GL_STATIC_DRAW);
  FVertexBuffer.Unbind();


  For iI := 0 to (SKY_COMPLEXITY div 4)-1 do
  begin
    For iJ := 0 to SKY_COMPLEXITY-1 do
    begin
      iX := iJ + (iI * (SKY_COMPLEXITY+1));
      iY := iJ + ((iI + 1) * (SKY_COMPLEXITY+1));
      iIndexes.Add(iY); iIndexes.Add(iX); iIndexes.Add(iX+1);
      iIndexes.Add(iY+1); iIndexes.Add(iY); iIndexes.Add(iX+1);
    end;
  end;
  FIndexBuffer.Bind();
  FIndexBuffer.Update(iIndexes, GL_STATIC_DRAW);
  FIndexBuffer.Unbind();

  FTriangleCount := (SKY_COMPLEXITY * (SKY_COMPLEXITY div 4)) * 2;
  FreeAndNil(iVertices);
  FreeAndNil(iIndexes);
end;

{******************************************************************************}
{* Init the skydome                                                           *}
{******************************************************************************}

procedure TGDSkyDome.InitSkyDome( aSkyTexture : String; aSize : Double );
begin
  Clear();
  FSkyTexture := Resources.LoadTexture(aSkyTexture ,Settings.TextureDetail,Settings.TextureFilter);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  CalculateDome(aSize);
end;

{******************************************************************************}
{* Render the skydome                                                         *}
{******************************************************************************}

procedure TGDSkyDome.Render();
begin
  If not(Modes.RenderSky) then exit;

  if Modes.RenderWireframe then
  begin
    Renderer.SetColor(0.2,0.2,0.8,1);
  end
  else
  begin
    FSkyTexture.BindTexture(GL_TEXTURE0);
    Renderer.SkyShader.Bind();
    Renderer.SetJoinedParams(Renderer.SkyShader);
    Renderer.SkyShader.SetInt('T_SKYTEX', 0);
  end;

  glPushMatrix();
  glDisable(GL_DEPTH_TEST);
  glDepthMask(GLboolean(FALSE));
  glTranslatef(Camera.Position.x, Camera.Position.y, Camera.Position.z);
  glScalef(1,0.20,1);
  FVertexBuffer.Bind(VL_V_UV);
  FIndexBuffer.Bind();
  FIndexBuffer.Render(GL_TRIANGLES);
  FVertexBuffer.Unbind();
  FIndexBuffer.Unbind();
  glDepthMask(GLboolean(TRUE));
  glEnable(GL_DEPTH_TEST);
  glPopMatrix();
end;

end.
