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
unit GDMeshCell;

{$MODE Delphi}


{******************************************************************************}
{* Holds the mesh cell class                                                  *}
{******************************************************************************}

interface

uses
  SysUtils,
  dglOpenGL,
  GDConstants,
  GDCamera,
  GDTypes,
  GDModes,
  GDMesh,
  GDWater,
  GDGLObjects,
  GDRenderer,
  GDSettings,
  GDResource,
  GDResources,
  GDBaseCell,
  GDGenerics,
  Contnrs;

type

{******************************************************************************}
{* Meshcell input record                                                      *}
{******************************************************************************}

  TGDMeshCellInput = record
    MeshName : String;
    PosX   : Double;
    PosY   : Double;
    PosZ   : Double;
    RotX   : Double;
    RotY   : Double;
    RotZ   : Double;
    ScaleX : Double;
    ScaleY : Double;
    ScaleZ : Double;
  end;

{******************************************************************************}
{* Mesh cell class                                                            *}
{******************************************************************************}

  TGDMeshCell = class (TGDBaseCell)
  private
    FMaxDistance : Single;
    FMatrix      : TGDMatrix;
    FMesh        : TGDMesh;
    FPosition    : TGDVector;
    FRotation    : TGDVector;
    FScale       : TGDVector;
    FNormalDPL   : TGDGLDisplayList;
  public
    property Mesh : TGDMesh read FMesh;
    property MaxDistance : Single read FMaxDistance write FMaxDistance;

    constructor Create(aInput : TGDMeshCellInput);
    destructor  Destroy(); override;

    procedure RenderMeshCell( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor );

    function TriangleCount() : Integer;
  end;

implementation

{******************************************************************************}
{* Create the meshcell class                                                  *}
{******************************************************************************}

constructor TGDMeshCell.Create();
var
  iVertex : TGDVector;
  iI   : Integer;
  iVertices : TGDVectorList;
begin
  FMesh := nil;
  FNormalDPL   := TGDGLDisplayList.Create();
  iVertices    := TGDVectorList.Create();
  OjectType    := SO_MESHCELL;
  FMaxDistance := Settings.ViewDistance * R_VIEW_DISTANCE_STEP;

  FMesh := Resources.LoadMesh(aInput.MeshName);
  FPosition.Reset(aInput.PosX, aInput.PosY, aInput.PosZ);
  FRotation.Reset(aInput.RotX, aInput.RotY, aInput.RotZ);
  FScale.Reset( aInput.ScaleX, aInput.ScaleY, aInput.ScaleZ );
  FMatrix.CreateRotation(FRotation);

  For iI := 0 to FMesh.Vertices.Count - 1 do
  begin
    iVertex := FMesh.Vertices.Items[iI].Copy();
    iVertex.Multiply(FScale);
    iVertex.Devide(100);
    FMatrix.ApplyToVector(iVertex);
    iVertex.Add( FPosition );
    iVertices.Add(iVertex)
  end;
  BoundingBox := iVertices.GenerateBoundingBox();

  FreeAndNil(iVertices);
end;

{******************************************************************************}
{* Destroy the  meshcell class                                                *}
{******************************************************************************}

destructor  TGDMeshCell.Destroy();
begin
  Resources.RemoveResource(TGDResource(FMesh));
  FMesh := nil;
  FreeAndNil(FNormalDPL);
  Inherited;
end;

{******************************************************************************}
{* Render the meshcell                                                        *}
{******************************************************************************}

procedure TGDMeshCell.RenderMeshCell( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor );
var
  iI  : Integer;
  iMS : TGDMaterialSegment;
  iNormal, iVertex : TGDVector;
  iNormals : TGDVectorList;
  iVertices : TGDVectorList;
  iTempPolygon : TGDMeshPolygon;

procedure RenderNormal(aP : TGDMeshPoint);
begin
  iVertex := iNormals.Items[aP.NormalID].Copy();
  iVertex.Multiply(R_NORMAL_LENGTH);
  iVertex.Add( iVertices.Items[aP.VertexID] );
  glVertex3fv( iVertices.Items[aP.VertexID].ArrayPointer );
  glVertex3fv( iVertex.ArrayPointer );
end;

begin


  Case aRenderAttribute Of
    RA_NORMAL         : begin
                          for iI := 0 to Mesh.MaterialSegmentList.Count - 1 do
                          begin
                            iMS := TGDMaterialSegment(Mesh.MaterialSegmentList.Items[iI]);

                            if Modes.RenderWireframe then
                            begin
                              Renderer.SetColor(1.0,1.0,1.0,1.0);
                              Renderer.ColorShader.SetMatrix('M_ROTATION', FMatrix);
                              Renderer.ColorShader.SetFloat3('V_POSITION', FPosition.x, FPosition.y, FPosition.z);
                              Renderer.ColorShader.SetFloat3('V_SCALE', FScale.x, FScale.y, FScale.z);
                              Renderer.ColorShader.SetInt('I_CUSTOM_TRANSLATE', 1);
                            end
                            else
                            begin
                              iMS.Material.ApplyMaterial();
                              Renderer.MeshShader.SetMatrix('M_ROTATION', FMatrix);
                              Renderer.MeshShader.SetFloat3('V_POSITION', FPosition.x, FPosition.y, FPosition.z);
                              Renderer.MeshShader.SetFloat3('V_SCALE', FScale.x, FScale.y, FScale.z);
                              Renderer.MeshShader.SetInt('I_DO_BLOOM', 1);

                              if aRenderFor = RF_BLOOM then
                              begin
                               if iMS.Material.DoBloom then
                                 Renderer.MeshShader.SetInt('I_DO_BLOOM', 1)
                               else
                                 Renderer.MeshShader.SetInt('I_DO_BLOOM', 0);
                              end;

                              If Water.WaterHeight > Camera.Position.Y then
                              begin
                               Renderer.MeshShader.SetInt('I_UNDER_WATER', 1);
                              end
                              else
                              begin
                               Renderer.MeshShader.SetInt('I_UNDER_WATER', 0);
                              end;
                            end;

                            Renderer.MeshShader.SetInt('I_FLIP_NORMAL', 0);
                            iMS.Material.BindMaterialTextures();

                            (Mesh.DPLS.Items[iI] as TGDGLDisplayList).CallList();

                            //fix for lighting with alha based surfaces
                            if iMS.Material.HasAlpha then
                            begin
                              if (aRenderFor = RF_WATER) and Not(Water.UnderWater) then
                                glCullFace(GL_BACK)
                              else
                                glCullFace(GL_FRONT);
                              Renderer.MeshShader.SetInt('I_FLIP_NORMAL', 1);
                              (Mesh.DPLS.Items[iI] as TGDGLDisplayList).CallList();
                              if (aRenderFor = RF_WATER) and Not(Water.UnderWater) then
                                glCullFace(GL_FRONT)
                              else
                                glCullFace(GL_BACK);
                            end;

                            iMS.Material.DisableMaterial();
                          end;
                        end;
    RA_FRUSTUM_BOXES  : begin
                          Renderer.SetColor(1,0,0,1);
                          BoundingBox.RenderWireFrame();
                        end;
    RA_NORMALS        : begin
                          Renderer.SetColor(1,0.5,0.25,1);
                          iNormals  := TGDVectorList.Create();
                          iVertices := TGDVectorList.Create();

                          For iI := 0 to FMesh.Vertices.Count - 1 do
                          begin
                            iVertex := FMesh.Vertices.Items[iI].Copy();
                            iVertex.Multiply(FScale);
                            iVertex.Devide(100);
                            FMatrix.ApplyToVector(iVertex);
                            iVertex.Add( FPosition );
                            iVertices.Add(iVertex)
                          end;
                          For iI := 0 to FMesh.Normals.Count - 1 do
                          begin
                            iNormal := FMesh.Normals.Items[iI].Copy();
                            FMatrix.ApplyToVector(iNormal);
                            iNormals.Add( iNormal );
                          end;

                          glBegin(GL_LINES);
                          for iI := 0 to FMesh.Polygons.Count - 1 do
                          begin
                            iTempPolygon := TGDMeshPolygon(FMesh.Polygons.Items[iI]);
                            RenderNormal(iTempPolygon.P1);
                            RenderNormal(iTempPolygon.P2);
                            RenderNormal(iTempPolygon.P3);
                          end;
                          glEnd();

                          FreeAndNil(iNormals);
                          FreeAndNil(iVertices);
                        end;
    end;
end;

{******************************************************************************}
{* Return meshcell triangle count                                             *}
{******************************************************************************}

function TGDMeshCell.TriangleCount() : Integer;
begin
  result := FMesh.Polygons.Count;
end;

end.
