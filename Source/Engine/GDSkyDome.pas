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
  GDGLObjects,
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
    FDisplayList   : TGDGLDisplayList;
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
  FreeAndNil(FDisplayList);
  Inherited;
end;

{******************************************************************************}
{* Clear the skydome                                                          *}
{******************************************************************************}

procedure TGDSkyDome.Clear();
begin
  FTriangleCount := 0;
  FreeAndNil(FDisplayList);
  Resources.RemoveResource(TGDResource(FSkyTexture));
end;

{******************************************************************************}
{* Calculate the skydome                                                      *}
{******************************************************************************}

procedure TGDSkyDome.CalculateDome( aSize : Double );
var
  iRotationStep : double;
  iRotationZ, iRotationY : Double;
  iUV : TGDUVCoord;
  iStartPoint, iTemp : TGDVector;
  iI, iJ, iX, iY : Integer;
  iUStep, iVStep: Double;
  iMatrix : TGDMatrix;
  iVertices : TGDVertex_V_List;
  iUVCoords : TGDVertex_UV_List;
begin
  iRotationStep := 360/SKY_COMPLEXITY;
  iVertices := TGDVertex_V_List.Create();
  iUVCoords := TGDVertex_UV_List.Create();
  iStartPoint.Reset(aSize, 0, 0 );
  iUStep := 1 / (SKY_COMPLEXITY);
  iVStep := 1 / (SKY_COMPLEXITY div 4);
  iRotationZ := 0;
  For iI := (SKY_COMPLEXITY div 4) downto 0 do
  begin
    iRotationY := 0;
    For iJ := 0 to SKY_COMPLEXITY do
    begin
      iTemp := iStartPoint.Copy();
      iMatrix.EmptyMatrix();
      iMatrix.CreateRotationY(iRotationY);
      iMatrix.ApplyToVector(iTemp);
      iVertices.Add(iTemp);
      iUV.Reset(iJ * iUStep, iI* iVStep );
      iUVCoords.Add( iUV );
      iRotationY := iRotationY + iRotationStep;
    end;
    iStartPoint.Reset( aSize, 0, 0 );
    iRotationZ := iRotationZ - iRotationStep;
    iMatrix.EmptyMatrix();
    iMatrix.CreateRotationZ(iRotationZ);
    iMatrix.ApplyToVector(iStartPoint);
  end;

  FDisplayList := TGDGLDisplayList.Create();
  FDisplayList.StartList();
  FSkyTexture.BindTexture(GL_TEXTURE0);
  For iI := 0 to (SKY_COMPLEXITY div 4)-1 do
  begin
    glBegin(GL_TRIANGLE_STRIP);
    For iJ := 0 to SKY_COMPLEXITY do
    begin
      iX := iJ + (iI * (SKY_COMPLEXITY+1));
      iY := iJ + ((iI + 1) * (SKY_COMPLEXITY+1));
      glTexCoord2fv( iUVCoords.Items[ iY ].ArrayPointer );
      glVertex3fv( TGDVector( iVertices.Items[  iY ] ).ArrayPointer);
      glTexCoord2fv( iUVCoords.Items[  iX ].ArrayPointer );
      glVertex3fv( TGDVector( iVertices.Items[  iX ] ).ArrayPointer);
    end;
    glEnd;
  end;
  FDisplayList.EndList();

  FTriangleCount := (SKY_COMPLEXITY * (SKY_COMPLEXITY div 4)) * 2;
  FreeAndNil(iVertices);
  FreeAndNil(iUVCoords);
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
    Renderer.SkyShader.Bind();
    Renderer.SetJoinedParams(Renderer.SkyShader);
    Renderer.SkyShader.SetInt('T_SKYTEX', 0);
  end;

  glPushMatrix();
  glDisable(GL_DEPTH_TEST);
  glDepthMask(GLboolean(FALSE));
  glTranslatef(Camera.Position.x, Camera.Position.y, Camera.Position.z);
  glScalef(1,0.20,1);
  FDisplayList.CallList();
  glDepthMask(GLboolean(TRUE));
  glEnable(GL_DEPTH_TEST);
  glPopMatrix();
end;

end.
