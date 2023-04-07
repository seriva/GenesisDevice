unit uGDMesh;

{$mode objfpc}

interface

Uses
  fgl,
  SysUtils,
  Classes,
  JsonTools,
  dglOpenGL,
  uGDTypes,
  uGDConstants,
  uGDMaterial,
  uGDGLWrappers,
  LazFileUtils,
  uGDTypesGenerics,
  uGDResource;

Type
  TGDSurface = class (TObject)
  private
    FMaterial      : TGDMaterial;
    FIndexes       : TGDIndexList;
    FIndexBuffer   : TGDGLIndexBuffer;
    FTriangleCount : Integer;
  public
    property Material      : TGDMaterial read FMaterial;
    property Indexes       : TGDIndexList read FIndexes;
    property IndexBuffer   : TGDGLIndexBuffer read FIndexBuffer;
    property TriangleCount : Integer read FTriangleCount write FTriangleCount;

    constructor Create();
    destructor  Destroy();override;

    procedure   Render(aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor);
  end;
  TGDSurfaceList = specialize TFPGObjectList<TGDSurface>;


  TGDMesh = class (TGDResource)
  private
    FSurfaces : TGDSurfaceList;
    FTriangleCount : Integer;

    procedure CreateBuffers();
  public
    property Surfaces : TGDSurfaceList read FSurfaces;
    property TriangleCount : Integer read FTriangleCount;

    constructor Create(aFileName : String);
    destructor  Destroy();override;
  end;

implementation

uses
  uGDEngine;

constructor TGDSurface.Create();
begin
  FMaterial    := Nil;
  FIndexes     := TGDIndexList.Create();
  FIndexBuffer := TGDGLIndexBuffer.Create();
  Inherited;
end;


destructor TGDSurface.Destroy();
begin
  GDResources.RemoveResource(TGDResource(FMaterial));
  FreeAndNil(FIndexes);
  FreeAndNil(FIndexBuffer);
  Inherited;
end;


procedure TGDSurface.Render(aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor);
begin
  FIndexBuffer.Bind();

  if GDModes.RenderWireframe then
 		FIndexBuffer.Render(GL_TRIANGLES)
  else
  begin
    FIndexBuffer.Render(GL_TRIANGLES);

    //fix for lighting with alha based surfaces
    if FMaterial.HasAlpha then
    begin
      if (aRenderFor = RF_WATER) and Not(GDMap.Water.UnderWater) then
        glCullFace(GL_BACK)
      else
        glCullFace(GL_FRONT);
      GDRenderer.MeshShader.SetInt('I_FLIP_NORMAL', 1);
      FIndexBuffer.Render(GL_TRIANGLES);
      if (aRenderFor = RF_WATER) and Not(GDMap.Water.UnderWater) then
        glCullFace(GL_FRONT)
      else
        glCullFace(GL_BACK);
      GDRenderer.MeshShader.SetInt('I_FLIP_NORMAL', 0);
    end;
  end;
  FIndexBuffer.Unbind();
end;


constructor TGDMesh.Create( aFileName : String );
var
  iMesh, iSurfaces, iSurface, iVerts, iNorms, iUVs, iIndices : TJsonNode;
  iError, iStr : String;
  iOffset, iCount, iI, iJ, iIdx : Integer;
  iV : TGDVertex_V_UV_N_C;
  iResult : boolean;
  iMat : TGDMaterial;
  iSur : TGDSurface;

begin
  GDConsole.Write('Loading mesh ' + aFileName + '...');
  try
    iResult := true;

    FSurfaces := TGDSurfaceList.Create();

    If Not(FileExistsUTF8(aFileName) ) then
      Raise Exception.Create('Mesh ' + aFileName + ' doesn`t excist!');

    //create the Json node
    Name := aFileName;
    iMesh := TJsonNode.Create();
    iMesh.LoadFromFile(aFileName);
    GDConsole.Use:=false;

    //load materials
    iStr := iMesh.Find('Materials/File').AsString;
    GDResources.LoadMaterials(ExtractFilePath(aFileName) + iStr);

    //load vertices
    iVerts :=  iMesh.Find('Vertices');
    iNorms :=  iMesh.Find('Normals');
    iUVs   :=  iMesh.Find('UVs');

    iCount := iVerts.count div 3;
    iOffset := GDMap.MeshManager.Vertices.count;
    for iI := 0 to iCount-1 do
    begin
      //vertex
      iV.Vertex.x := iVerts.Child((iI * 3)).AsNumber;
      iV.Vertex.y := iVerts.Child((iI * 3) + 1).AsNumber;
      iV.Vertex.z := iVerts.Child((iI * 3) + 2).AsNumber;

      //normal
      iV.Normal.x := iNorms.Child((iI * 3)).AsNumber;
      iV.Normal.y := iNorms.Child((iI * 3) + 1).AsNumber;
      iV.Normal.z := iNorms.Child((iI * 3) + 2).AsNumber;

      //uv
      iV.UV.u := iUVs.Child((iI * 2)).AsNumber;
      iV.UV.v := -iUVs.Child((iI * 2) + 1).AsNumber;

      //color
      iV.Color  := Color(1,1,1,1);
      GDMap.MeshManager.Vertices.Add(iV);
    end;

    //load surfaces
    iSurfaces := iMesh.Find('Surfaces');
    for iI := 0 to iSurfaces.Count-1 do
    begin
      iSurface := iSurfaces.Child(iI);

      //get material
      iStr := iSurface.Find('Material').AsString; 
      if not(GDResources.Find(iStr, iIdx)) then
        Raise Exception.Create('Material ' + iStr + ' doesn`t excist!');
      iMat := TGDMaterial(GDResources.Data[iIdx]);

      //create surface
      iSur := TGDSurface.Create();
      iSur.FMaterial := iMat;
      FSurfaces.Add(iSur);

      //indexes
      iIndices := iSurface.Find('Indices');
      for iJ := 0 to iIndices.count-1  do
      begin
        iIdx := Trunc(iIndices.Child(iJ).AsNumber) + iOffset;
        iSur.FIndexes.Add(iIdx);
        if iSur.Material.DoTreeAnim then
        begin
          iV := GDMap.MeshManager.Vertices[iIdx];
          iV.Color := Color(0.75 + (Random(25)/100), 0.75 + (Random(25)/100), 0.75 + (Random(25)/100), 1);
          GDMap.MeshManager.Vertices[iIdx] := iV;
        end;
      end;
    end; 

    CreateBuffers();
  except
    on E: Exception do
    begin
      iError := E.Message;
      iResult := false;
    end;
  end;

  FreeAndNil(iMesh);
  GDConsole.Use:=true;
  GDConsole.WriteOkFail(iResult, iError);
end;


destructor  TGDMesh.Destroy();
begin
  FreeAndnil(FSurfaces);
end;


procedure TGDMesh.CreateBuffers();
var
  iI : Integer;
  iSur   : TGDSurface;
begin
  for iI := 0 to FSurfaces.Count - 1 do
  begin
    iSur  := FSurfaces.Items[iI];
    iSur.IndexBuffer.Bind();
    iSur.IndexBuffer.Update(iSur.Indexes, GL_STATIC_DRAW);
    iSur.IndexBuffer.Unbind();
    FTriangleCount := FTriangleCount + iSur.TriangleCount;
  end;
end;

end.
