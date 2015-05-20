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
  GDTypes,
  GDModes,
  GDMesh,
  GDGLObjects,
  GDSettings,
  GDResource,
  GDResources,
  GDBaseCell,
  GDGenerics;

type

{******************************************************************************}
{* Meshcell input record                                                      *}
{******************************************************************************}

  TGDMeshCellInput = record
    Model        : String;
    ModelLOD1    : String;
    ModelLOD2    : String;
    FadeScale    : Double;
    FadeDistance : Double;
    Position     : TGDVector;
    Rotation     : TGDVector;
    Scale        : TGDVector;
  end;

{******************************************************************************}
{* Mesh cell class                                                            *}
{******************************************************************************}

  TGDMeshCell = class (TGDBaseCell)
  private
    FLODType       : TGDMeshLODType;
    FMesh          : TGDMesh;
    FMeshLOD1      : TGDMesh;
    FMeshLOD2      : TGDMesh;
    FFadeScale     : single;
    FFadeDistance  : single;

    FPosition      : TGDVector;
    FScale         : TGDVector;
    FRotation      : TGDMatrix;
  public
    property LODType : TGDMeshLODType read FLODType write FLODType;
    property FadeDistance : Single read FFadeDistance write FFadeDistance;
    property FadeScale : Single read FFadeScale write FFadeScale;

    constructor Create(aInput : TGDMeshCellInput);
    destructor  Destroy(); override;

    procedure Render( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor ); override;

    function TriangleCount() : Integer;
  end;

implementation

uses
  GDMap,
  GDRenderer;

{******************************************************************************}
{* Create the meshcell class                                                  *}
{******************************************************************************}

constructor TGDMeshCell.Create(aInput : TGDMeshCellInput);
var
  iVertex : TGDVector;
  iI   : Integer;
  iVertices : TGDVectorList;
begin
  OjectType   := SO_MESHCELL;
  FLODType    := LT_NONE;
  FMesh       := Resources.LoadMesh(aInput.Model);

  //set posible LOD types.
  if ((aInput.ModelLOD1 <> '') and (aInput.ModelLOD2 <> ''))then
  begin
    FLODType  := LT_STAGES;
    FMeshLOD1 := Resources.LoadMesh(aInput.ModelLOD1);
    FMeshLOD2 := Resources.LoadMesh(aInput.ModelLOD2);
  end
  else if ((aInput.FadeScale <> 0) and (aInput.FadeDistance <> 0)) then
  begin
    FLODType := LT_FADE_IN;
    FFadeScale    := aInput.FadeScale;
    FFadeDistance := aInput.FadeDistance;
  end;

  //set translation
  FPosition := aInput.Position.Copy();
  FScale    := aInput.Scale.Copy();
  FRotation.CreateRotation(aInput.Rotation);

  //calculate boundingbox
  iVertices   := TGDVectorList.Create();
  For iI := 0 to FMesh.Vertices.Count - 1 do
  begin
    iVertex := FMesh.Vertices.Items[iI].Copy();
    iVertex.Multiply(FScale);
    iVertex.Devide(100);
    FRotation.ApplyToVector(iVertex);
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
  Inherited;
end;

{******************************************************************************}
{* Render the meshcell                                                        *}
{******************************************************************************}

procedure TGDMeshCell.Render( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor );
var
  iI  : Integer;
  iMS : TGDMaterialSegment;
  iNormal, iVertex : TGDVector;
  iNormals : TGDVectorList;
  iVertices : TGDVectorList;
  iTempPolygon : TGDMeshPolygon;
  iFadeDistanceScale : Single;
  iMesh : TGDMesh;

procedure RenderNormal(aP : TGDMeshPoint);
begin
  iVertex := iNormals.Items[aP.NormalID].Copy();
  iVertex.Multiply(R_NORMAL_LENGTH);
  iVertex.Add( iVertices.Items[aP.VertexID] );
  glVertex3fv( iVertices.Items[aP.VertexID].ArrayPointer );
  glVertex3fv( iVertex.ArrayPointer );
end;

procedure SetMeshPositioning(aShader : TGDGLShader);
begin
  aShader.SetMatrix('M_ROTATION', FRotation);
  aShader.SetFloat3('V_POSITION', FPosition.x, FPosition.y, FPosition.z);
  aShader.SetFloat3('V_SCALE', FScale.x * iFadeDistanceScale, FScale.y * iFadeDistanceScale, FScale.z * iFadeDistanceScale);
end;

begin
  //Determine LOD settings for meshcell.
  iMesh := FMesh;
  iFadeDistanceScale := 1;
  Case FLODType of
    LT_NONE    : ;
    LT_FADE_IN : begin
                  if Distance < (FFadeDistance - FFadeScale) then
                    iFadeDistanceScale := 1
                  else
                    iFadeDistanceScale := 1 - ((Distance - (FFadeDistance - FFadeScale)) / FFadeScale);
                 end;
    LT_STAGES  :begin
                  if ((Distance >= 0) and (Distance < Settings.LOD0)) then
                    iMesh := FMesh
                  else if ((Distance >= Settings.LOD0) and (Distance <= Settings.LOD1)) then
                    iMesh := FMeshLOD1
                  else if ((Distance >= Settings.LOD1) and (Distance <= Settings.LOD2)) then
                    iMesh := FMeshLOD2;
                end;
  end;

  Case aRenderAttribute Of
    RA_NORMAL         : begin
                          for iI := 0 to iMesh.MaterialSegmentList.Count - 1 do
                          begin
                            iMS := TGDMaterialSegment(iMesh.MaterialSegmentList.Items[iI]);

                            if Modes.RenderWireframe then
                            begin
                              Renderer.SetColor(1.0,1.0,1.0,1.0);
                              SetMeshPositioning(Renderer.ColorShader);
                              Renderer.ColorShader.SetInt('I_CUSTOM_TRANSLATE', 1);
                              (iMesh.DPLS.Items[iI] as TGDGLDisplayList).CallList();
                            end
                            else
                            begin
                              iMS.Material.ApplyMaterial();
                              SetMeshPositioning(Renderer.MeshShader);
                              Renderer.MeshShader.SetInt('I_DO_BLOOM', 1);

                              if aRenderFor = RF_BLOOM then
                              begin
                               if iMS.Material.DoBloom then
                                 Renderer.MeshShader.SetInt('I_DO_BLOOM', 1)
                               else
                                 Renderer.MeshShader.SetInt('I_DO_BLOOM', 0);
                              end;

                              Renderer.MeshShader.SetInt('I_FLIP_NORMAL', 0);

                              (iMesh.DPLS.Items[iI] as TGDGLDisplayList).CallList();

                              //fix for lighting with alha based surfaces
                              if iMS.Material.HasAlpha then
                              begin
                                if (aRenderFor = RF_WATER) and Not(Map.Water.UnderWater) then
                                  glCullFace(GL_BACK)
                                else
                                  glCullFace(GL_FRONT);
                                Renderer.MeshShader.SetInt('I_FLIP_NORMAL', 1);
                                (iMesh.DPLS.Items[iI] as TGDGLDisplayList).CallList();
                                if (aRenderFor = RF_WATER) and Not(Map.Water.UnderWater) then
                                  glCullFace(GL_FRONT)
                                else
                                  glCullFace(GL_BACK);
                              end;

                              iMS.Material.DisableMaterial();
                            end;
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

                          For iI := 0 to iMesh.Vertices.Count - 1 do
                          begin
                            iVertex := iMesh.Vertices.Items[iI].Copy();
                            iVertex.Multiply(FScale);
                            iVertex.Devide(100);
                            FRotation.ApplyToVector(iVertex);
                            iVertex.Add( FPosition );
                            iVertices.Add(iVertex)
                          end;
                          For iI := 0 to iMesh.Normals.Count - 1 do
                          begin
                            iNormal := iMesh.Normals.Items[iI].Copy();
                            FRotation.ApplyToVector(iNormal);
                            iNormals.Add( iNormal );
                          end;

                          glBegin(GL_LINES);
                          for iI := 0 to iMesh.Polygons.Count - 1 do
                          begin
                            iTempPolygon := TGDMeshPolygon(iMesh.Polygons.Items[iI]);
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
  if FLODType = LT_STAGES then
  begin
    if ((Distance >= 0) and (Distance < Settings.LOD0)) then
      result := FMesh.Polygons.Count
    else if ((Distance >= Settings.LOD0) and (Distance <= Settings.LOD1)) then
      result := FMeshLOD1.Polygons.Count
    else if ((Distance >= Settings.LOD1) and (Distance <= Settings.LOD2)) then
      result := FMeshLOD2.Polygons.Count;
  end;
end;

end.
