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
    FMesh        : TGDMesh;
    FPosition    : TGDVector;
    FRotation    : TGDVector;
    FScale       : TGDVector;
    FDPLS        : TObjectList;
    FNormalDPL   : TGDGLDisplayList;

    procedure PrepareMeshCell();
  public
    property Mesh : TGDMesh read FMesh;

    constructor Create();
    destructor  Destroy(); override;

    procedure InitMeshCell( aInput : TGDMeshCellInput );
    procedure RenderMeshCell( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor );

    function TriangleCount() : Integer;
  end;

implementation

{******************************************************************************}
{* Create the meshcell class                                                  *}
{******************************************************************************}

constructor TGDMeshCell.Create();
begin
  Inherited;
  FMesh := nil;
  FDPLS       := TObjectList.Create();
  FNormalDPL  := TGDGLDisplayList.Create();;
  OjectType   := SO_MESHCELL;
end;

{******************************************************************************}
{* Destroy the  meshcell class                                                *}
{******************************************************************************}

destructor  TGDMeshCell.Destroy();
begin
  FMesh := nil;
  FreeAndNil(FDPLS);
  FreeAndNil(FNormalDPL);
  Inherited;
end;

{******************************************************************************}
{* Prepare the meshcell                                                       *}
{******************************************************************************}

procedure TGDMeshCell.PrepareMeshCell();
var
  iVertex, iNormal : TGDVector;
  iI : Integer;
  iMX, iMY, iMZ : TGDMatrix;
  iMS : TGDMaterialSegment;
  iDPL : TGDGLDisplayList;
  iJ  : Integer;
  iPL : TGDMeshPolygon;
  iVertices, iNormals : TGDVectorList;
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
  //Create resources
  iVertices := TGDVectorList.Create();
  iNormals  := TGDVectorList.Create();

  //Rotate vertices and normals
  iMY.CreateRotationY( FRotation.Y );
  iMZ.CreateRotationZ( FRotation.Z );
  iMX.CreateRotationX( -FRotation.X );
  For iI := 0 to FMesh.Vertices.Count - 1 do
  begin
    iVertex := FMesh.Vertices.Items[iI].Copy();
    iVertex.Multiply(FScale);
    iVertex.Devide(100);
    iMY.ApplyToVector( iVertex );
    iMZ.ApplyToVector( iVertex );
    iMX.ApplyToVector( iVertex );
    iVertex.Add( FPosition );
    iVertices.Add(iVertex)
  end;
  For iI := 0 to FMesh.Normals.Count - 1 do
  begin
    iNormal := FMesh.Normals.Items[iI].Copy();
    iMY.ApplyToVector( iNormal );
    iMZ.ApplyToVector( iNormal );
    iMX.ApplyToVector( iNormal );
    iNormals.Add( iNormal );
  end;

  //Calculate boundingbox
  BoundingBox.Generate( iVertices );

  //prepare the displaylists.
  for iI := 0 to Mesh.MaterialSegmentList.Count - 1 do
  begin
    iMS := TGDMaterialSegment(Mesh.MaterialSegmentList.Items[iI]);
    iDPL := TGDGLDisplayList.Create();
    iDPL.InitDisplayList();
    iDPL.StartList();

    glBegin(GL_TRIANGLES);
    for iJ := 0 to iMS.Polygons.Count-1 do
    begin
      iPL := TGDMeshPolygon(iMS.Polygons.Items[iJ]);
      //V1
      glNormal3fv( iNormals.Items[ iPL.P1.NormalID ].ArrayPointer );
      glTexCoord2fv( FMesh.UV.Items[ iPL.P1.UVID ].ArrayPointer );
      glVertex3fv( iVertices.Items[ iPL.P1.VertexID ].ArrayPointer );
      //V2
      glNormal3fv( iNormals.Items[ iPL.P2.NormalID ].ArrayPointer );
      glTexCoord2fv( FMesh.UV.Items[ iPL.P2.UVID ].ArrayPointer );
      glVertex3fv( iVertices.Items[ iPL.P2.VertexID ].ArrayPointer );
      //V3
      glNormal3fv( iNormals.Items[ iPL.P3.NormalID ].ArrayPointer );
      glTexCoord2fv( FMesh.UV.Items[ iPL.P3.UVID ].ArrayPointer );
      glVertex3fv( iVertices.Items[ iPL.P3.VertexID ].ArrayPointer );
    end;
    glEnd();

    iDPL.EndList();
    FDPLS.Add(iDPL);
  end;

  FNormalDPL.InitDisplayList();
  FNormalDPL.StartList();
  glBegin(GL_LINES);
  for iI := 0 to FMesh.Polygons.Count - 1 do
  begin
    iTempPolygon := TGDMeshPolygon(FMesh.Polygons.Items[iI]);
    RenderNormal(iTempPolygon.P1);
    RenderNormal(iTempPolygon.P2);
    RenderNormal(iTempPolygon.P3);
  end;
  glEnd();
  FNormalDPL.EndList();

  //Free recources
  FreeAndNil(iVertices);
  FreeAndNil(iNormals);
end;

{******************************************************************************}
{* Init the meshcell                                                          *}
{******************************************************************************}

procedure TGDMeshCell.InitMeshCell( aInput : TGDMeshCellInput );
begin
  FMesh := TGDMesh(MeshList.AddMesh( aInput.MeshName ));
  FPosition.Reset(aInput.PosX, aInput.PosY, aInput.PosZ);
  FRotation.Reset(aInput.RotX, aInput.RotY, aInput.RotZ);
  FScale.Reset( aInput.ScaleX, aInput.ScaleY, aInput.ScaleZ );
  PrepareMeshCell();
end;

{******************************************************************************}
{* Render the meshcell                                                        *}
{******************************************************************************}

procedure TGDMeshCell.RenderMeshCell( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor );
var
  iI  : Integer;
  iMS : TGDMaterialSegment;
begin
  Case aRenderAttribute Of
    RA_NORMAL         : begin
                          for iI := 0 to Mesh.MaterialSegmentList.Count - 1 do
                          begin
                            iMS := TGDMaterialSegment(Mesh.MaterialSegmentList.Items[iI]);

                            if Modes.RenderWireframe then
                            begin
                              glColor4f(1.0,1.0,1.0,1.0);
                            end
                            else
                            begin
                              iMS.Material.ApplyMaterial();
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

                            (FDPLS.Items[iI] as TGDGLDisplayList).CallList();

                            //fix for lighting with alha based surfaces
                            if iMS.Material.HasAlpha then
                            begin
                              if (aRenderFor = RF_WATER) and Not(Water.UnderWater) then
                                glCullFace(GL_BACK)
                              else
                                glCullFace(GL_FRONT);
                              Renderer.MeshShader.SetInt('I_FLIP_NORMAL', 1);
                              (FDPLS.Items[iI] as TGDGLDisplayList).CallList();
                              if (aRenderFor = RF_WATER) and Not(Water.UnderWater) then
                                glCullFace(GL_FRONT)
                              else
                                glCullFace(GL_BACK);
                            end;

                            iMS.Material.DisableMaterial();
                          end;
                        end;
    RA_FRUSTUM_BOXES  : begin
                          glColor4f(1,0,0,1);
                          BoundingBox.RenderWireFrame();
                        end;
    RA_NORMALS        : begin
                          glColor4f(1,0.5,0.25,1);
                          FNormalDPL.CallList();
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
