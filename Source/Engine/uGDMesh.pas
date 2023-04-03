unit uGDMesh;

{$mode objfpc}

interface

Uses
  fgl,
  SysUtils,
  Classes,
  dglOpenGL,
  uGDTypes,
  uGDConstants,
  uGDMaterial,
  uGDGLWrappers,
  LazFileUtils,
  uGDTypesGenerics,
  uGDResource,
  uGDStringParsing;

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
  iIdx : Integer;
  iFile : TMemoryStream;
  iSL : TStringList;
  iStr, iError : String;
  iVec, iNorm : TGDVector;
  iUV : TGDUVCoord;
  iMat : TGDMaterial;
  iResult : boolean;
  iSur : TGDSurface;
  iVertices : TGDVertex_V_List;
  iNormals  : TGDVertex_V_List;
  iUVS      : TGDVertex_UV_List;

function ParseVertex(aStr : String): TGDIdxVertex;
begin
  iSL.Clear();
  ExtractStrings(['/'], [], PChar(AnsiString(aStr)), iSL);
  result.Vertex := StrToInt(iSL.Strings[0])-1;
  result.UV     := StrToInt(iSL.Strings[1])-1;
  if iSL.Count > 2 then
  	result.Normal := StrToInt(iSL.Strings[2])-1
  else
    result.Normal := -1;
end;

function AddVertex(iIdxVertex : TGDIdxVertex): integer;
var
  iV : TGDVertex_V_UV_N_C;
begin
  iV.Vertex := iVertices.Items[iIdxVertex.Vertex].Copy();
  iV.UV     := iUVS.Items[iIdxVertex.UV].Copy();

  if -1 <> iIdxVertex.Normal then
  	iV.Normal := iNormals.Items[iIdxVertex.Normal].Copy()
  else
    iV.Normal.Reset(0,0,0);

  if iSur.Material.DoTreeAnim then
    iV.Color  := Color(0.75 + (Random(25)/100), 0.75 + (Random(25)/100), 0.75 + (Random(25)/100), 1)
  else
    iV.Color  := Color(1,1,1,1);

  result := GDMap.MeshManager.Vertices.Add(iV);
end;

begin
  GDConsole.Write('Loading mesh ' + aFileName + '...');
  try
    iResult := true;

    FSurfaces := TGDSurfaceList.Create();
    iSL       := TStringList.Create();
    iVertices := TGDVertex_V_List.Create();
    iNormals  := TGDVertex_V_List.Create();
    iUVs      := TGDVertex_UV_List.Create();

    If Not(FileExistsUTF8(aFileName) ) then
      Raise Exception.Create('Mesh ' + aFileName + ' doesn`t excist!');

    //create the filestream
    Name := aFileName;
    iFile := TMemoryStream.Create();
    iFile.LoadFromFile(aFileName);

    //set the comment string
    CommentString := '#';
    GDConsole.Use:=false;

    while (iFile.Position < iFile.Size) do
    begin
      iStr := GetNextToken(iFile);
      if iStr = 'mtllib' then //read the material lib
      begin
        iStr := GetNextToken(iFile);
        GDResources.LoadMaterials(ExtractFilePath(aFileName) + iStr);
        continue;
      end
      else if iStr = 'v' then //read a vertex
      begin
        iVec.x := StrToFloat(GetNextToken(iFile));
        iVec.y := StrToFloat(GetNextToken(iFile));
        iVec.z := StrToFloat(GetNextToken(iFile));
        iVertices.Add(iVec);
        continue;
      end
      else if iStr = 'vt' then //read a uv
      begin
        iUV.U := StrToFloat(GetNextToken(iFile));
        iUV.V := -StrToFloat(GetNextToken(iFile));
        iUVs.Add(iUV);
        continue;
      end
      else if iStr = 'vn' then //read a normal
      begin
        iNorm.x := StrToFloat(GetNextToken(iFile));
        iNorm.y := StrToFloat(GetNextToken(iFile));
        iNorm.z := StrToFloat(GetNextToken(iFile));
        iNormals.Add(iNorm);
        continue;
      end
      else if iStr = 'usemtl' then //read the current material for the faces
      begin
        iStr := GetNextToken(iFile);
        if not(GDResources.Find(iStr, iIdx)) then
           Raise Exception.Create('Material ' + iStr + ' doesn`t excist!');
        iMat := TGDMaterial(GDResources.Data[iIdx]);
        iSur := TGDSurface.Create();
        iSur.FMaterial := iMat;
        FSurfaces.Add(iSur);
        continue;
      end
      else if iStr = 'f' then //read a face (we only support triangles)
      begin
        if (iSur = nil) then
           Raise Exception.Create('No material for surface!');

        //we only support triangles now.
        iSur.Indexes.Add( AddVertex(ParseVertex(GetNextToken(iFile))));
        iSur.Indexes.Add( AddVertex(ParseVertex(GetNextToken(iFile))));
        iSur.Indexes.Add( AddVertex(ParseVertex(GetNextToken(iFile))));
        iSur.TriangleCount := iSur.TriangleCount + 1;
        continue;
      end;
    end;
    FreeAndNil(iFile);
    CreateBuffers();
  except
    on E: Exception do
    begin
      iError := E.Message;
      iResult := false;
    end;
  end;

  FreeAndNil(iSL);
  FreeAndNil(iVertices);
  FreeAndNil(iNormals);
  FreeAndNil(iUVS);

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
