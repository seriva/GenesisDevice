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
  GDGLWrappers,
  GDSettings,
  GDResource,
  GDResources,
  GDBaseCell,
  GDTypesGenerics;

type

{******************************************************************************}
{* Meshcell input record                                                      *}
{******************************************************************************}

  TGDMeshCellInput = record
    Model         : String;
    ModelLOD1     : String;
    ModelLOD2     : String;
    FadeScale     : Double;
    FadeDistance  : Double;
    Position      : TGDVector;
    Rotation      : TGDVector;
    Scale         : TGDVector;
    CastShadow    : Boolean;
    ReceiveShadow : Boolean;
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

    FCastShadow    : Boolean;
    FReceiveShadow : Boolean;
  public
    property LODType : TGDMeshLODType read FLODType write FLODType;
    property FadeDistance : Single read FFadeDistance write FFadeDistance;
    property FadeScale : Single read FFadeScale write FFadeScale;
    property CastShadow : Boolean read FCastShadow write FCastShadow;
    property ReceiveShadow : Boolean read FReceiveShadow write FReceiveShadow;

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
  iI : integer;
  iVertices : TGDVertex_V_List;
  iVector : TGDVector;
  iCenter : TGDVector;
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
  iVertices   := TGDVertex_V_List.Create();
  with iVertices do
  begin
    For iI := 0 to FMesh.Vertices.Count - 1 do
    begin
      iVector := FMesh.Vertices.Items[iI].Copy();
      iVector.Multiply(FScale);
      iVector.Devide(100);
      FRotation.ApplyToVector(iVector);
      iVector.Add( FPosition );
      iVertices.Add(iVector)
    end;

    iCenter.reset(0,0,0);
    for iI := 0 to Count-1 do iCenter.Add( Items[iI] );
    iCenter.Devide( Count );
    BoundingBox.Min.reset(iCenter.x, iCenter.y , iCenter.z);
    BoundingBox.Max.reset(iCenter.x, iCenter.y , iCenter.z);

    for iI := 0 to Count-1 do
    begin
      iVector := Items[iI].Copy();

      If (iVector.X <=  BoundingBox.Min.x) then
         BoundingBox.Min.setX(iVector.X)
      else If (iVector.X >=  BoundingBox.Max.x) then
              BoundingBox.Max.setX(iVector.X);

      If (iVector.Y <=  BoundingBox.Min.Y) then
         BoundingBox.Min.setY(iVector.Y)
      else If (iVector.Y >=  BoundingBox.Max.Y) then
              BoundingBox.Max.setY(iVector.Y);

      If (iVector.Z <=  BoundingBox.Min.Z) then
         BoundingBox.Min.setZ(iVector.Z)
      else If (iVector.Z >=  BoundingBox.Max.Z) then
              BoundingBox.Max.setZ(iVector.Z);
    end;
    BoundingBox.CalculateCenter();
  end;
  FreeAndNil(iVertices);

  //Cast and receive shadows.
  FCastShadow    := aInput.CastShadow;
  FReceiveShadow := aInput.ReceiveShadow;
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
  iI, iJ : Integer;
  iSur : TGDSurface;
  iNormal, iVertex : TGDVector;
  iNormals : TGDVertex_V_List;
  iVertices : TGDVertex_V_List;
  iTri : TGDTriangleIdxs;
  iFadeDistanceScale : Single;
  iMesh : TGDMesh;

procedure RenderNormal(aTris : TGDTriangleIdxs; aStartIdx : integer);
begin
  iVertex := iNormals.Items[aTris.Data[aStartIdx+2]].Copy();
  iVertex.Multiply(R_NORMAL_LENGTH);
  iVertex.Add( iVertices.Items[aTris.Data[aStartIdx]] );
  Renderer.AddLine(iVertices.Items[aTris.Data[aStartIdx]], iVertex);
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
                          for iI := 0 to iMesh.Surfaces.Count - 1 do
                          begin
                            iSur := iMesh.Surfaces.Items[iI];

                            if Modes.RenderWireframe then
                            begin
                              Renderer.SetColor(1.0,1.0,1.0,1.0);
                              SetMeshPositioning(Renderer.ColorShader);
                              Renderer.ColorShader.SetInt('I_CUSTOM_TRANSLATE', 1);
                              iSur.DPL.CallList();
                            end
                            else
                            begin
                              Renderer.MeshShader.Bind();
                              Renderer.SetJoinedParams(Renderer.MeshShader,aRenderFor = RF_SHADOW);
                              iSur.Material.ApplyMaterial();
                              SetMeshPositioning(Renderer.MeshShader);
                              Renderer.MeshShader.SetInt('I_DO_BLOOM', 1);

                              if self.ReceiveShadow and (aRenderFor <> RF_SHADOW) then
                                Renderer.MeshShader.SetInt('I_RECEIVE_SHADOW', 1)
                              else
                                Renderer.MeshShader.SetInt('I_RECEIVE_SHADOW', 0);

                              if aRenderFor = RF_BLOOM then
                              begin
                               if iSur.Material.DoBloom then
                                 Renderer.MeshShader.SetInt('I_DO_BLOOM', 1)
                               else
                                 Renderer.MeshShader.SetInt('I_DO_BLOOM', 0);
                              end;

                              Renderer.MeshShader.SetInt('I_FLIP_NORMAL', 0);

                              iSur.DPL.CallList();

                              //fix for lighting with alha based surfaces
                              if iSur.Material.HasAlpha then
                              begin
                                if (aRenderFor = RF_WATER) and Not(Map.Water.UnderWater) then
                                  glCullFace(GL_BACK)
                                else
                                  glCullFace(GL_FRONT);
                                Renderer.MeshShader.SetInt('I_FLIP_NORMAL', 1);
                                iSur.DPL.CallList();
                                if (aRenderFor = RF_WATER) and Not(Map.Water.UnderWater) then
                                  glCullFace(GL_FRONT)
                                else
                                  glCullFace(GL_BACK);
                              end;

                              iSur.Material.DisableMaterial();
                            end;
                          end;
                        end;
    RA_FRUSTUM_BOXES  : BoundingBox.RenderWireFrame();
    RA_NORMALS        : begin
                          iNormals  := TGDVertex_V_List.Create();
                          iVertices := TGDVertex_V_List.Create();

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

                          for iI := 0 to iMesh.Surfaces.Count - 1 do
                          begin
                            for iJ := 0 to iMesh.Surfaces[iI].Triangles.Count - 1 do
                            begin
                              iTri := iMesh.Surfaces[iI].Triangles[iJ];
                              RenderNormal(iTri, 0);
                              RenderNormal(iTri, 3);
                              RenderNormal(iTri, 6);
                            end;
                          end;

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
  result := FMesh.TriangleCount;
  if FLODType = LT_STAGES then
  begin
    if ((Distance >= 0) and (Distance < Settings.LOD0)) then
      result := FMesh.TriangleCount
    else if ((Distance >= Settings.LOD0) and (Distance <= Settings.LOD1)) then
      result := FMeshLOD1.TriangleCount
    else if ((Distance >= Settings.LOD1) and (Distance <= Settings.LOD2)) then
      result := FMeshLOD2.TriangleCount;
  end;
end;

end.
