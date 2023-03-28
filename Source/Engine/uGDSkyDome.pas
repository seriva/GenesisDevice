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
unit uGDSkyDome;

{$MODE Delphi}

{******************************************************************************}
{* Holds the skydome class                                                    *}
{******************************************************************************}

interface

uses
  Classes,
  SysUtils,
  IniFiles,
  dglOpenGL,
  uGDTexture,
  uGDTypes,
  uGDGLWrappers,
  uGDConstants,
  uGDResource,
  uGDTypesGenerics;

type

{******************************************************************************}
{* Skydome class                                                              *}
{******************************************************************************}

  TGDSkyDome = class
  private
    FIndexBuffer     : TGDGLIndexBuffer;
    FVertexBuffer    : TGDGLVertexBuffer;
    FCloudTexture    : TGDTexture;
    FSunFlareTexture : TGDTexture;
    FSunTexture      : TGDTexture;
    FSize            : Single;
    FIntensity       : Single;
    FCloudUV         : Single;
    FAniSpeed1, FAniSpeed2 : Single;
    FTriangleCount   : Integer;
    FSunQuery        : GLuint;
    FSunSize         : Single;

    procedure CalculateDome(aSize : Double );
  public
    property TriangleCount : integer read FTriangleCount;

    constructor Create();
    destructor  Destroy(); override;

    procedure InitSkyDome( aIniFile : TIniFile; aSize : Double );
    procedure Clear();
    procedure Render();

    procedure RenderSun();
    procedure RenderSunFlare();
  end;

implementation

uses
  uGDEngine;

{******************************************************************************}
{* Create the skydome class                                                   *}
{******************************************************************************}

constructor TGDSkyDome.Create();
begin
  inherited;
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
  GDResources.RemoveResource(TGDResource(FCloudTexture));
  GDResources.RemoveResource(TGDResource(FSunTexture));
  GDResources.RemoveResource(TGDResource(FSunFlareTexture));
  glDeleteQueries(1, @FSunQuery);
  FSunSize := 0.5;
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
  iMatrix : TGDMatrix;
  iIndexes : TGDIndexList;
  iVertices : TGDVertex_V_List;
  iV : TGDVector;
begin
  FSize := aSize;
  iIndexes  := TGDIndexList.Create();
  iVertices := TGDVertex_V_List.Create();
  FIndexBuffer  := TGDGLIndexBuffer.Create();
  FVertexBuffer := TGDGLVertexBuffer.Create();

  iRotationStep := 360/SKY_COMPLEXITY;
  iStartPoint.Reset(aSize, 0, 0 );
  iRotationZ := 0;
  For iI := (SKY_COMPLEXITY div 4) downto 0 do
  begin
    iRotationY := 0;
    For iJ := 0 to SKY_COMPLEXITY do
    begin
      iV := iStartPoint.Copy();
      iMatrix.CreateRotationY(iRotationY);
      iMatrix.ApplyToVector(iV);
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

procedure TGDSkyDome.InitSkyDome(  aIniFile : TIniFile; aSize : Double );
begin
  Clear();
  FCloudTexture := GDResources.LoadTexture(aIniFile.ReadString( 'Sky', 'CloudMap', 'sky.dds' ), GDSettings.TextureDetail, GDSettings.TextureFilter);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  FIntensity := aIniFile.ReadFloat( 'Sky', 'Intensity', 0.5 );
  FCloudUV   := aIniFile.ReadFloat( 'Sky', 'CloudUV', 1.0 );
  FAniSpeed1 := aIniFile.ReadFloat( 'Sky', 'AniSpeed1', 1000.0 );
  FAniSpeed2 := aIniFile.ReadFloat( 'Sky', 'AniSpeed2', 1000.0 );
  CalculateDome(aSize);

  FSunTexture := GDResources.LoadTexture(aIniFile.ReadString( 'Sky', 'SunMap', 'sun.dds' ), GDSettings.TextureDetail, GDSettings.TextureFilter);
  FSunFlareTexture := GDResources.LoadTexture(aIniFile.ReadString( 'Sky', 'SunFlareMap', 'sunflare.dds' ), GDSettings.TextureDetail, GDSettings.TextureFilter);
  glGenQueries(1, @FSunQuery);
  FSunSize := aIniFile.ReadFloat( 'Sky', 'SunSize', 0.5 );
end;

{******************************************************************************}
{* Render the skydome                                                         *}
{******************************************************************************}

procedure TGDSkyDome.Render();
begin
  If not(GDModes.RenderSky) then exit;

  if GDModes.RenderWireframe then
  begin
    GDRenderer.SetColor(0.2,0.2,0.8,1);
  end
  else
  begin
    FCloudTexture.BindTexture(GL_TEXTURE0);
    with GDRenderer do
    begin
      SkyShader.Bind();
      SetJoinedParams(SkyShader);
      SkyShader.SetInt('T_CLOUDTEX', 0);
      SkyShader.SetFloat('I_INTENSITY', FIntensity);
      SkyShader.SetFloat('F_SIZE', FSize / FCloudUV);
      SkyShader.SetFloat('F_ANIMATION_SPEED1', GDTiming.ElapsedTime / (FAniSpeed1*1000));
      SkyShader.SetFloat('F_ANIMATION_SPEED2', GDTiming.ElapsedTime / (FAniSpeed2*1000));
    end;
  end;

  glPushMatrix();
  glDisable(GL_DEPTH_TEST);
  glDepthMask(GLboolean(FALSE));
  glTranslatef(GDCamera.Position.x, GDCamera.Position.y, GDCamera.Position.z);
  glScalef(0.90,0.125,0.90);
  FVertexBuffer.Bind(VL_V);
  FIndexBuffer.Bind();
  FIndexBuffer.Render(GL_TRIANGLES);
  FVertexBuffer.Unbind();
  FIndexBuffer.Unbind();
  glDepthMask(GLboolean(TRUE));
  glEnable(GL_DEPTH_TEST);
  glPopMatrix();
end;

{******************************************************************************}
{* Render the sun                                                             *}
{******************************************************************************}

procedure TGDSkyDome.RenderSun();
var
  iSunPos, iTemp : TGDVector;
begin
  iSunPos := GDCamera.Position.Copy();
  iTemp := GDMap.LightDirection.inverse();
  iTemp *= 50000;
  iSunPos += iTemp;

  glEnable(GL_POINT_SPRITE);
  glEnable(GL_BLEND);
  glBlendFunc(GL_ONE, GL_ONE);
  glEnable(GL_DEPTH_TEST);
  GDRenderer.SunShader.Bind();
  GDRenderer.SunShader.SetInt('T_SUNMAP', 0);

  FSunTexture.BindTexture(GL_TEXTURE0);
  glPointSize( Round(GDWindow.Width() * FSunSize)  );
  glBegin( GL_POINTS );
    glVertex3f( iSunPos.X, iSunPos.Y, iSunPos.z);
  glEnd();
  glPointSize( 1.0 );

  GDRenderer.SunShader.Unbind();
  glDisable(GL_POINT_SPRITE);
  glDisable(GL_BLEND);
end;

{******************************************************************************}
{* Render the sunflare                                                        *}
{******************************************************************************}

procedure TGDSkyDome.RenderSunFlare();
var
  iSunPos, iTemp : TGDVector;
  iResults : Integer;
begin
  iSunPos := GDCamera.Position.Copy();
  iTemp := GDMap.LightDirection.inverse();
  iTemp *= 50000;
  iSunPos += iTemp;

  glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
  glDepthMask(GL_FALSE);

  glBeginQuery(GL_SAMPLES_PASSED, FSunQuery);
  glPointSize( 5.0 );
  glBegin( GL_POINTS );
    glVertex3f( iSunPos.X, iSunPos.Y, iSunPos.z);
  glEnd();
  glEndQuery(GL_SAMPLES_PASSED);

  glDepthMask(GL_TRUE);
  glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);

  glEnable(GL_POINT_SPRITE);
  glEnable(GL_BLEND);
  glBlendFunc(GL_ONE, GL_ONE);
  glDisable(GL_DEPTH_TEST);
  GDRenderer.SunShader.Bind();
  GDRenderer.SunShader.SetInt('T_SUNMAP', 0);

  glGetQueryObjectiv(FSunQuery, GL_QUERY_RESULT, @iResults);
  if (iResults > 5) then
  begin
    FSunFlareTexture.BindTexture(GL_TEXTURE0);
    glPointSize( Round(GDWindow.Width() * FSunSize)  );
    glBegin( GL_POINTS );
      glVertex3f( iSunPos.X, iSunPos.Y, iSunPos.z);
    glEnd();
    glPointSize( 1.0 );
  end;

  GDRenderer.SunShader.Unbind();
  glDisable(GL_POINT_SPRITE);
  glDisable(GL_BLEND);
  glEnable(GL_DEPTH_TEST);
end;

end.
