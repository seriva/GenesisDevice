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
unit GDMesh;

{$mode objfpc}

interface

Uses
  fgl,
  SysUtils,
  Classes,
  dglOpenGL,
  GDTypes,
  GDConstants,
  GDMaterial,
  GDGLWrappers,
  FileUtil,
  GDTypesGenerics,
  GDResource,
  GDStringParsing;

Type

{******************************************************************************}
{* Surface class                                                              *}
{******************************************************************************}

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

    procedure   Render();
  end;
  TGDSurfaceList = specialize TFPGObjectList<TGDSurface>;

{******************************************************************************}
{* Mesh class                                                                 *}
{******************************************************************************}

  TGDMesh = class (TGDResource)
  private
    FSurfaces : TGDSurfaceList;
    FVertices : TGDVertex_V_UV_N_C_List;
    FVertexBuffer : TGDGLVertexBuffer;
    FTriangleCount : Integer;

    procedure CreateBuffers();
  public
    property Surfaces : TGDSurfaceList read FSurfaces;
    property Vertices : TGDVertex_V_UV_N_C_List read FVertices;
    property VertexBuffer  : TGDGLVertexBuffer read FVertexBuffer;
    property TriangleCount : Integer read FTriangleCount;

    constructor Create(aFileName : String);
    destructor  Destroy();override;
  end;

implementation

uses
  GDEngine;

{******************************************************************************}
{* Create surface                                                             *}
{******************************************************************************}

constructor TGDSurface.Create();
begin
  FMaterial    := Nil;
  FIndexes     := TGDIndexList.Create();
  FIndexBuffer := TGDGLIndexBuffer.Create();
  Inherited;
end;

{******************************************************************************}
{* Destroy surface                                                            *}
{******************************************************************************}

destructor TGDSurface.Destroy();
begin
  Engine.Resources.RemoveResource(TGDResource(FMaterial));
  FreeAndNil(FIndexes);
  FreeAndNil(FIndexBuffer);
  Inherited;
end;

{******************************************************************************}
{* Render surface                                                             *}
{******************************************************************************}

procedure TGDSurface.Render();
begin
  FIndexBuffer.Bind();
  FIndexBuffer.Render(GL_TRIANGLES);
  FIndexBuffer.Unbind();
end;

{******************************************************************************}
{* Create the mesh class                                                      *}
{******************************************************************************}

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
  result.Normal := StrToInt(iSL.Strings[2])-1;
end;

function AddVertex(iIdxVertex : TGDIdxVertex): integer;
var
  iV : TGDVertex_V_UV_N_C;
begin
  iV.Vertex := iVertices.Items[iIdxVertex.Vertex].Copy();
  iV.UV     := iUVS.Items[iIdxVertex.UV].Copy();
  iV.Normal := iNormals.Items[iIdxVertex.Normal].Copy();
  if iSur.Material.DoTreeAnim then
    iV.Color  := Color(0.75 + (Random(25)/100), 0.75 + (Random(25)/100), 0.75 + (Random(25)/100), 1)
  else
    iV.Color  := Color(1,1,1,1);
  result    := FVertices.Add(iV);
end;

begin
  Engine.Console.Write('Loading mesh ' + aFileName + '...');
  try
    iResult := true;

    FSurfaces := TGDSurfaceList.Create();
    FVertices := TGDVertex_V_UV_N_C_List.Create();

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
    Engine.Console.Use:=false;

    while (iFile.Position < iFile.Size) do
    begin
      iStr := GetNextToken(iFile);
      if iStr = 'mtllib' then //read the material lib
      begin
        iStr := GetNextToken(iFile);
        Engine.Resources.LoadMaterials(ExtractFilePath(aFileName) + iStr);
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
        if not(Engine.Resources.Find(iStr, iIdx)) then
           Raise Exception.Create('Material ' + iStr + ' doesn`t excist!');
        iMat := TGDMaterial(Engine.Resources.Data[iIdx]);
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

  Engine.Console.Use:=true;
  Engine.Console.WriteOkFail(iResult, iError);
end;

{******************************************************************************}
{* Destroy the mesh class                                                     *}
{******************************************************************************}

destructor  TGDMesh.Destroy();
begin
  FreeAndNil(FVertices);
  FreeAndnil(FSurfaces);
  FreeAndNil(FVertexBuffer);
end;

{******************************************************************************}
{* Create the segment dpl                                                     *}
{******************************************************************************}

procedure TGDMesh.CreateBuffers();
var
  iI, iJ, iIdx : Integer;
  iSur   : TGDSurface;
  iV : TGDVertex_V_UV_N_C;
begin
  for iI := 0 to FSurfaces.Count - 1 do
  begin
    iSur  := FSurfaces.Items[iI];
    iSur.IndexBuffer.Bind();
    iSur.IndexBuffer.Update(iSur.Indexes, GL_STATIC_DRAW);
    iSur.IndexBuffer.Unbind();
    FTriangleCount := FTriangleCount + iSur.TriangleCount;
  end;
  FVertexBuffer := TGDGLVertexBuffer.Create();
  FVertexBuffer.Bind(VL_NONE);
  FVertexBuffer.Update(FVertices, GL_STATIC_DRAW);
  FVertexBuffer.Unbind();
end;

end.
