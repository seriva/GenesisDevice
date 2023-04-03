unit uGDMeshCell;

{$MODE Delphi}

interface

uses
  SysUtils,
  dglOpenGL,
  uGDConstants,
  uGDTypes,
  uGDMesh,
  uGDGLWrappers,
  uGDResource,
  uGDBaseCell,
  uGDTypesGenerics;

type
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


  TGDMeshCell = class (TGDBaseCell)
  private
    FLODType       : TGDMeshLODType;
    FMesh          : TGDMesh;
    FMeshLOD1      : TGDMesh;
    FMeshLOD2      : TGDMesh;
    FFadeScale     : single;
    FFadeDistance  : single;
    FFadeDistanceScale : single;

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

    procedure Render( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor); override;
    procedure ApplyMeshCell(aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor);

    function TriangleCount() : Integer;
  end;


   TGDMeshCellSurface = record
    Surface  : TGDSurface;
    MeshCell : TGDMeshCell;

    class operator Equal(smc1, smc2: TGDMeshCellSurface) B: Boolean;
  end;

implementation

uses
  uGDMeshManager,
  uGDEngine;

class operator TGDMeshCellSurface.Equal(smc1, smc2: TGDMeshCellSurface) B: Boolean;
begin
  B := true;
end;


constructor TGDMeshCell.Create(aInput : TGDMeshCellInput);
var
  iI, iJ : integer;
  iVertices : TGDVertex_V_List;
  iVector : TGDVector;
  iCenter : TGDVector;
begin
  OjectType   := SO_MESHCELL;
  FLODType    := LT_NONE;
  FMesh       := GDResources.LoadMesh(aInput.Model);

  //set posible LOD types.
  if ((aInput.ModelLOD1 <> '') and (aInput.ModelLOD2 <> ''))then
  begin
    FLODType  := LT_STAGES;
    FMeshLOD1 := GDResources.LoadMesh(aInput.ModelLOD1);
    FMeshLOD2 := GDResources.LoadMesh(aInput.ModelLOD2);
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
  For iI := 0 to FMesh.Surfaces.Count - 1 do
  begin
		For iJ := 0 to FMesh.Surfaces[iI].Indexes.Count - 1 do
    begin
      iVector := GDMap.MeshManager.Vertices.Items[FMesh.Surfaces[iI].Indexes[iJ]].Vertex.Copy();
      iVector := (iVector * FScale) / 100;
      FRotation.ApplyToVector(iVector);
      iVector += FPosition;
      iVertices.Add(iVector)
    end;
  end;

  iCenter.reset(0,0,0);
  for iI := 0 to iVertices.Count-1 do iCenter += iVertices.Items[iI];
  iCenter /= iVertices.Count;
  BoundingBox.Min.reset(iCenter.x, iCenter.y , iCenter.z);
  BoundingBox.Max.reset(iCenter.x, iCenter.y , iCenter.z);

  for iI := 0 to iVertices.Count-1 do
  begin
    iVector := iVertices.Items[iI].Copy();

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

  FreeAndNil(iVertices);

  //Cast and receive shadows.
  FCastShadow    := aInput.CastShadow;
  FReceiveShadow := aInput.ReceiveShadow;
end;


destructor  TGDMeshCell.Destroy();
begin
  GDResources.RemoveResource(TGDResource(FMesh));
  FMesh := nil;
  Inherited;
end;


procedure TGDMeshCell.Render( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor );
var
  iI, iJ : Integer;
  iSur : TGDSurface;
  iV1, iV2 : TGDVector;
  iMesh : TGDMesh;
  iMCS  : TGDMeshCellSurface;

begin
  if (aRenderFor = RF_SHADOW) then
    if not(CastShadow) then
    	exit;

  //Determine LOD settings for meshcell.
  iMesh := FMesh;
  FFadeDistanceScale := 1;
  Case FLODType of
    LT_NONE    : ;
    LT_FADE_IN : begin
                  if Distance < (FFadeDistance - FFadeScale) then
                    FFadeDistanceScale := 1
                  else
                    FFadeDistanceScale := 1 - ((Distance - (FFadeDistance - FFadeScale)) / FFadeScale);
                 end;
    LT_STAGES  :begin
                  if ((Distance >= 0) and (Distance < GDSettings.LOD0)) then
                    iMesh := FMesh
                  else if ((Distance >= GDSettings.LOD0) and (Distance <= GDSettings.LOD1)) then
                    iMesh := FMeshLOD1
                  else if ((Distance >= GDSettings.LOD1) and (Distance <= GDSettings.LOD2)) then
                    iMesh := FMeshLOD2;
                end;
  end;

  Case aRenderAttribute Of
    RA_NORMAL         : begin
                          for iI := 0 to iMesh.Surfaces.Count - 1 do
                          begin
                            iMCS.Surface  := iMesh.Surfaces.Items[iI];
                            iMCS.MeshCell := self;
                            GDMap.MeshManager.AddSurfaceToCache(iMCS);
                          end;
                        end;
    RA_FRUSTUM_BOXES  : BoundingBox.RenderWireFrame();
    RA_NORMALS        : begin
                          for iI := 0 to iMesh.Surfaces.Count - 1 do
                          begin
                            iSur := iMesh.Surfaces.Items[iI];
                            for iJ := 0 to iSur.Indexes.Count-1 do
                            begin
                              iV1 := GDMap.MeshManager.Vertices.Items[iSur.Indexes.Items[iJ]].Vertex.Copy();
                              iV1 *= FScale;
                              iV1 /= 100;
                              FRotation.ApplyToVector(iV1);
                              iV1 += FPosition;
                              iV2 := GDMap.MeshManager.Vertices.Items[iSur.Indexes.Items[iJ]].Normal.Copy();
                              FRotation.ApplyToVector(iV2);
                              iV2 *= R_NORMAL_LENGTH;
                              iV2 += iV1;
                              GDRenderer.AddLine(iV1, iV2);
                            end;
                          end;
                        end;
    end;
end;


procedure TGDMeshCell.ApplyMeshCell(aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor);

procedure SetMeshPositioning(aShader : TGDGLShader);
begin
  aShader.SetMatrix('M_ROTATION', FRotation);
  aShader.SetFloat3('V_POSITION', FPosition.x, FPosition.y, FPosition.z);
  aShader.SetFloat3('V_SCALE', (FScale.x * FFadeDistanceScale) / 100, (FScale.y * FFadeDistanceScale) / 100, (FScale.z * FFadeDistanceScale) / 100);
end;

begin
  if GDModes.RenderWireframe then
    SetMeshPositioning(GDRenderer.ColorShader)
  else
    SetMeshPositioning(GDRenderer.MeshShader);

  if self.ReceiveShadow and (aRenderFor <> RF_SHADOW) then
    GDRenderer.MeshShader.SetInt('I_RECEIVE_SHADOW', 1)
  else
    GDRenderer.MeshShader.SetInt('I_RECEIVE_SHADOW', 0);
  GDRenderer.MeshShader.SetInt('I_FLIP_NORMAL', 0);
end;


function TGDMeshCell.TriangleCount() : Integer;
begin
  result := FMesh.TriangleCount;
  if FLODType = LT_STAGES then
  begin
    if ((Distance >= 0) and (Distance < GDSettings.LOD0)) then
      result := FMesh.TriangleCount
    else if ((Distance >= GDSettings.LOD0) and (Distance <= GDSettings.LOD1)) then
      result := FMeshLOD1.TriangleCount
    else if ((Distance >= GDSettings.LOD1) and (Distance <= GDSettings.LOD2)) then
      result := FMeshLOD2.TriangleCount;
  end;
end;

end.
