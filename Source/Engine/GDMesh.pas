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
unit GDMesh;

{$MODE Delphi}

{******************************************************************************}
{* Holds the mesh classes                                                     *}
{******************************************************************************}

interface

Uses
  SysUtils,
  Classes,
  dglOpenGL,
  GDTypes,
  GDConsole,
  GDCamera,
  GDGLObjects,
  GDConstants,
  GDRenderer,
  GDMaterials,
  GDObjectList,
  GDModes,
  GDWater,
  FileUtil,
  GDStringParsing;

Type

{******************************************************************************}
{* Meshpoint class                                                            *}
{******************************************************************************}

  TGDMeshPoint = class
  private
    FVertexID : Integer;
    FNormalID : Integer;
    FUVID : Integer;
  public
    property VertexID : Integer read FVertexID;
    property NormalID : Integer read FNormalID;
    property UVID : Integer read FUVID;

    constructor Create();
    destructor  Destroy();override;
  end;

{******************************************************************************}
{* Meshpolygon class                                                          *}
{******************************************************************************}

  TGDMeshPolygon = class (TObject)
  private
    FMaterial   : TGDMaterial;
    FPoint1     : TGDMeshPoint;
    FPoint2     : TGDMeshPoint;
    FPoint3     : TGDMeshPoint;
  public
    property Material : TGDMaterial read FMaterial;
    property P1  : TGDMeshPoint read FPoint1;
    property P2  : TGDMeshPoint read FPoint2;
    property P3  : TGDMeshPoint read FPoint3;

    constructor Create();
    destructor  Destroy();override;
  end;

{******************************************************************************}
{* Materialsegment class                                                    *}
{******************************************************************************}

  TGDMaterialSegment = class (TObject)
  private
    FMaterial   : TGDMaterial;
    FPolygons   : TGDObjectList;
  public
    property Material    : TGDMaterial read FMaterial;
    property Polygons : TGDObjectList read FPolygons;

    constructor Create();
    destructor  Destroy();override;
  end;

{******************************************************************************}
{* Mesh class                                                                 *}
{******************************************************************************}

  TGDMesh = class
  private
    FFileName            : String;
    FVertices            : TGDObjectList;
    FNormals             : TGDObjectList;
    FUV                  : TGDObjectList;
    FPolygons            : TGDObjectList;
    FMaterialSegmentList : TGDObjectList;

    procedure CreateMaterialSegmentLists();
  public
    property FileName      : String read FFileName;
    property Vertices      : TGDObjectList read FVertices;
    property Normals       : TGDObjectList read FNormals;
    property UV            : TGDObjectList read FUV;
    property Polygons      : TGDObjectList read FPolygons;
    property MaterialSegmentList : TGDObjectList read FMaterialSegmentList;

    constructor Create();
    destructor  Destroy();override;

    function  LoadMesh( aFileName : String ) : boolean;
    procedure Clear();
  end;

{******************************************************************************}
{* Meshlist class                                                             *}
{******************************************************************************}

  TGDMeshList = class (TGDObjectList)
  private
  public
    function  AddMesh( aFileName : String ) : TGDMesh;
  end;

var
  MeshList : TGDMeshList;

implementation

{******************************************************************************}
{* Create the staticmeshpoint class                                           *}
{******************************************************************************}

constructor TGDMeshPoint.Create();
begin
  FVertexID := 0;
  FNormalID := 0;
  FUVID := 0;
  Inherited;
end;

{******************************************************************************}
{* Destroy the staticmeshpoint class                                          *}
{******************************************************************************}

destructor  TGDMeshPoint.Destroy();
begin
  Inherited;
end;

{******************************************************************************}
{* Create the staticmeshpolygon class                                         *}
{******************************************************************************}

constructor TGDMeshPolygon.Create();
begin
  FMaterial := Nil;
  FPoint1   := TGDMeshPoint.Create();
  FPoint2   := TGDMeshPoint.Create();
  FPoint3   := TGDMeshPoint.Create();
  Inherited;
end;

{******************************************************************************}
{* Destroy the staticmeshpolygon class                                        *}
{******************************************************************************}

destructor  TGDMeshPolygon.Destroy();
begin
  FMaterial := Nil;
  FreeAndNil(FPoint1);
  FreeAndNil(FPoint2);
  FreeAndNil(FPoint3);
  Inherited;
end;

{******************************************************************************}
{* Create the materialrendersegment class                                     *}
{******************************************************************************}

constructor TGDMaterialSegment.Create();
begin
  FMaterial := Nil;
  FPolygons := TGDObjectList.Create();
  FPolygons.OwnsObjects := False;
  Inherited;
end;

{******************************************************************************}
{* Destroy the materialrendersegment class                                    *}
{******************************************************************************}

destructor  TGDMaterialSegment.Destroy();
begin
  FMaterial := Nil;
  FreeAndNil(FPolygons);
  Inherited;
end;

{******************************************************************************}
{* Create the mesh class                                                      *}
{******************************************************************************}

constructor TGDMesh.Create();
begin
  FVertices            := TGDObjectList.Create();
  FNormals             := TGDObjectList.Create();
  FUV                  := TGDObjectList.Create();
  FPolygons            := TGDObjectList.Create();
  FMaterialSegmentList := TGDObjectList.Create();
end;

{******************************************************************************}
{* Destroy the mesh class                                                     *}
{******************************************************************************}

destructor  TGDMesh.Destroy();
begin
  FreeAndNil(FVertices);
  FreeAndNil(FNormals);
  FreeAndNil(FUV);
  FreeAndNil(FPolygons);
  FreeAndnil(FMaterialSegmentList);
end;

{******************************************************************************}
{* Load the mesh                                                              *}
{******************************************************************************}

function  TGDMesh.LoadMesh( aFileName : String ) : boolean;
var
  iFile : TMemoryStream;
  iStr, iError : String;
  iVec, iNorm : TGDVector;
  iUV : TGDUVCoord;
  iMat : TGDMaterial;
  iPolygon : TGDMeshPolygon;

procedure ParsePointSet(aStr : String; aPoint : TGDMeshPoint );
var
  iJ, iCount : Integer;
  iSubStr : String;
  iIdx : array[0..2] of integer;
begin
  iSubStr := '';
  iCount := 0;
  for iJ := 1 to length(aStr) do
  begin
    if aStr[iJ] <> '/' then
    begin
      iSubStr := iSubStr + aStr[iJ];
    end
    else
    begin
      if iSubStr = '' then
         iIdx[iCount] := -1
      else
      begin
        iIdx[iCount] := StrToInt(iSubStr)-1;
        iSubStr := '';
      end;
      Inc(iCount);
    end;
  end;
  iIdx[iCount] := StrToInt(iSubStr)-1;

  aPoint.FVertexID:=iIdx[0];
  aPoint.FUVID:=iIdx[1];
  aPoint.FNormalID:=iIdx[2];
end;

begin
  Console.Write('Loading mesh ' + aFileName + '...');
  try
    result := true;
    If Not(FileExistsUTF8(aFileName) ) then
      Raise Exception.Create('Mesh ' + aFileName + ' doesn`t excist!');

    //create the filestream
    FFileName := aFileName;
    iFile := TMemoryStream.Create();
    iFile.LoadFromFile(aFileName);
    //set the comment string
    CommentString := '#';
    Console.Use:=false;

    while (iFile.Position < iFile.Size) do
    begin
      iStr := GetNextToken(iFile);
      if iStr = 'mtllib' then //read the material lib
      begin
        iStr := GetNextToken(iFile);
        if not(MaterialList.LoadMaterials(ExtractFilePath(aFileName) + iStr)) then
           Raise Exception.Create('Material file ' + iStr + ' failed to load');
        continue;
      end
      else if iStr = 'v' then //read a vertex
      begin
        iVec := TGDVector.Create();
        iVec.x := StrToFloat(GetNextToken(iFile));
        iVec.y := StrToFloat(GetNextToken(iFile));
        iVec.z := StrToFloat(GetNextToken(iFile));
        FVertices.AddObjectI(iVec);
        continue;
      end
      else if iStr = 'vt' then //read a uv
      begin
        iUV := TGDUVCoord.Create();
        iUV.U := StrToFloat(GetNextToken(iFile));
        iUV.V := -StrToFloat(GetNextToken(iFile));
        FUV.AddObjectI(iUV);
        continue;
      end
      else if iStr = 'vn' then //read a normal
      begin
        iNorm := TGDVector.Create();
        iNorm.x := StrToFloat(GetNextToken(iFile));
        iNorm.y := StrToFloat(GetNextToken(iFile));
        iNorm.z := StrToFloat(GetNextToken(iFile));
        FNormals.AddObjectI(iNorm);
        continue;
      end
      else if iStr = 'usemtl' then //read the current material for the faces
      begin
        iStr := GetNextToken(iFile);
        iMat := MaterialList.GetMaterial(iStr);
        if (iMat = nil) then
           Raise Exception.Create('Material ' + iStr + ' doesn`t excist!');
        continue;
      end
      else if iStr = 'f' then //read a face (we only support triangles)
      begin
        if (iMat = nil) then
           Raise Exception.Create('No material for polygon!');

        //we only support triangles now.
        iPolygon := TGDMeshPolygon.Create();
        iPolygon.FMaterial := iMat;
        ParsePointSet(GetNextToken(iFile), iPolygon.FPoint1);
        ParsePointSet(GetNextToken(iFile), iPolygon.FPoint2);
        ParsePointSet(GetNextToken(iFile), iPolygon.FPoint3);
        FPolygons.Add(iPolygon);
        continue;
      end;
    end;
    FreeAndNil(iFile);
  except
    on E: Exception do
    begin
      iError := E.Message;
      result := false;
    end;
  end;

  Console.Use:=true;
  Console.WriteOkFail(result, iError);

  CreateMaterialSegmentLists();
end;


{******************************************************************************}
{* Clear mesh                                                                 *}
{******************************************************************************}

procedure TGDMesh.Clear();
begin
  FVertices.Clear();
  FNormals.Clear();
  FUV.Clear();
  FPolygons.Clear();
  FMaterialSegmentList.Clear();
end;

{******************************************************************************}
{* Create the mesh material segment lists                                     *}
{******************************************************************************}

procedure TGDMesh.CreateMaterialSegmentLists();
var
  iI    : Integer;
  iCount: Integer;
  iMat  : TGDMaterial;
  iMS   : TGDMaterialSegment;
  iTempPolygon : TGDMeshPolygon;

procedure StartMaterialSegment();
begin
  iMS := TGDMaterialSegment.Create();
  iMS.FMaterial := iTempPolygon.Material;
end;

procedure EndMaterialSegment();
begin
  FMaterialSegmentList.Add( iMS );
end;

begin
  iMat:= nil;
  iCount := Fpolygons.Count - 1;
  for iI := 0 to iCount do
  begin
    iTempPolygon := TGDMeshPolygon(Fpolygons.GetObjectI(iI));

    If iMat <> iTempPolygon.Material then
    begin
      if iMat = nil then
        StartMaterialSegment()
      else
      begin
        EndMaterialSegment();
        StartMaterialSegment()
      end;
    end;

    iMS.Polygons.Add(iTempPolygon);

    iMat := iTempPolygon.Material;
  end;

  EndMaterialSegment();
end;

{******************************************************************************}
{* Add mesh                                                                   *}
{******************************************************************************}

function TGDMeshList.AddMesh( aFileName : String ) : TGDMesh;
var
  iMesh    : TGDMesh;
  iI       : Integer;
begin
  result := nil;

  If Not(FileExistsUTF8(aFileName )) then exit;

  iI := 0;
  while ((iI < self.Count) and (result = nil )) do
  begin
    iMesh := TGDMesh(self.GetObjectI(iI));
    If UpperCase(iMesh.FileName) = UpperCase(aFileName) then
      result := iMesh;

    iI := iI + 1;
  end;

  If result <> nil then exit;

  iMesh := TGDMesh.Create();
  iMesh.LoadMesh( aFileName );
  result := self.AddObjectP( iMesh );
end;

end.
