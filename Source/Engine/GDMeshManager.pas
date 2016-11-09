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
unit GDMeshManager;

{$mode objfpc}

interface

uses
  dglOpenGL,
  FGL,
  SysUtils,
  GDGLWrappers,
  GDTypesGenerics,
  GDConstants,
  GDMeshCell,
  GDMesh;

type

{******************************************************************************}
{* Surface cache                                                              *}
{******************************************************************************}

  TGDMeshCellSurfaceList  = specialize TFPGList<TGDMeshCellSurface>;
  TGDSurfaceCache = specialize TFPGMap<String,TGDMeshCellSurfaceList>;

{******************************************************************************}
{* Cellmanager class                                                          *}
{******************************************************************************}

  TGDMeshManager = class
  private
    FVertices     : TGDVertex_V_UV_N_C_List;
    FVertexBuffer : TGDGLVertexBuffer;
    FSurfaceCache : TGDSurfaceCache;
  public
    property Vertices     : TGDVertex_V_UV_N_C_List read FVertices;
    property VertexBuffer : TGDGLVertexBuffer read FVertexBuffer;
    property SurfaceCache : TGDSurfaceCache read FSurfaceCache;

    constructor Create();
    destructor  Destroy(); override;

    procedure StartRendering( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor );
    procedure EndRendering();

    procedure AddSurfaceToCache(aSurface : TGDMeshCellSurface);

    procedure CreateBuffers();
    procedure ClearBuffers();
  end;

implementation

uses
  GDEngine;

{******************************************************************************}
{* Create the mesh manager class                                              *}
{******************************************************************************}

constructor TGDMeshManager.Create();
begin
  FVertices := TGDVertex_V_UV_N_C_List.Create();
  FSurfaceCache := TGDSurfaceCache.Create();
end;

{******************************************************************************}
{* Destroy the mesh manager class                                             *}
{******************************************************************************}

destructor  TGDMeshManager.Destroy();
begin
  FreeAndNil(FSurfaceCache);
  FreeAndNil(FVertices);
end;

{******************************************************************************}
{* Start mesh rendering                                                       *}
{******************************************************************************}

procedure TGDMeshManager.StartRendering( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor );
var
  iList : TGDMeshCellSurfaceList;
  iK : integer;
begin

  for iK := FSurfaceCache.Count - 1 downto 0 do
  begin
    iList := FSurfaceCache.Data[iK] as TGDMeshCellSurfaceList;
    iList.Clear();
    FreeAndNil(iList);
    FSurfaceCache.Delete(iK);
  end;
  FSurfaceCache.Clear();

  FVertexBuffer.Bind(VL_V_UV_N_C);

  if aRenderAttribute = RA_NORMAL then
  begin
    with Engine do
    begin
      if Modes.RenderWireframe then
      begin
        Renderer.SetColor(1.0,1.0,1.0,1.0);
        Renderer.ColorShader.Bind();
        Renderer.ColorShader.SetInt('I_CUSTOM_TRANSLATE', 1);
      end
      else
      begin
        Renderer.MeshShader.Bind();
        Renderer.SetJoinedParams(Engine.Renderer.MeshShader,aRenderFor = RF_SHADOW);
        Renderer.MeshShader.SetFloat('F_ANIMATION_SPEED', Engine.Timing.ElapsedTime / Engine.Map.Foliage.TreeAnimationSpeed);
        Renderer.MeshShader.SetFloat('F_ANIMATION_STRENGTH', Engine.Map.Foliage.TreeAnimationStrength);
      end;
    end;
  end;
end;

{******************************************************************************}
{* End mesh rendering                                                         *}
{******************************************************************************}

procedure TGDMeshManager.EndRendering();
begin
	Engine.Renderer.MeshShader.UnBind();
  FVertexBuffer.Unbind();
end;

{******************************************************************************}
{* Add surface to cache                                                       *}
{******************************************************************************}

procedure TGDMeshManager.AddSurfaceToCache(aSurface : TGDMeshCellSurface);
var
  iIdx : Integer;
  iList : TGDMeshCellSurfaceList;
begin
  if FSurfaceCache.Find(aSurface.Surface.Material.Name, iIdx) then
    iList := FSurfaceCache.Data[iIdx]
  else
  begin
    iList := TGDMeshCellSurfaceList.Create();
    FSurfaceCache.Add( aSurface.Surface.Material.Name,  iList);
  end;
  iList.Add( aSurface );
end;

{******************************************************************************}
{* Create buffers                                                             *}
{******************************************************************************}

procedure TGDMeshManager.CreateBuffers();
begin
  FVertexBuffer := TGDGLVertexBuffer.Create();
  FVertexBuffer.Bind(VL_NONE);
  FVertexBuffer.Update(FVertices, GL_STATIC_DRAW);
  FVertexBuffer.Unbind();
end;

{******************************************************************************}
{* End buffers                                                                *}
{******************************************************************************}

procedure TGDMeshManager.ClearBuffers();
begin
  FreeAndNil(FVertexBuffer);
  FVertices.Clear();
end;

end.

