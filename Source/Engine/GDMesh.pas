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
  GDConstants,
  GDMaterial,
  GDGLObjects,
  Contnrs,
  FileUtil,
  GDGenerics,
  GDResource,
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
    FPolygons   : TObjectList;
  public
    property Material    : TGDMaterial read FMaterial;
    property Polygons : TObjectList read FPolygons;

    constructor Create();
    destructor  Destroy();override;
  end;

{******************************************************************************}
{* Mesh class                                                                 *}
{******************************************************************************}

  TGDMesh = class (TGDResource)
  private
    FFileName            : String;
    FVertices            : TGDVectorList;
    FNormals             : TGDVectorList;
    FUV                  : TGDUVCoordList;
    FPolygons            : TObjectList;
    FMaterialSegmentList : TObjectList;
    FDPLS                : TObjectList;

    procedure CreateMaterialSegmentLists();
    procedure CreateDisplayLists();
  public
    property FileName      : String read FFileName;
    property Vertices      : TGDVectorList read FVertices;
    property Normals       : TGDVectorList read FNormals;
    property UV            : TGDUVCoordList read FUV;
    property Polygons      : TObjectList read FPolygons;
    property MaterialSegmentList : TObjectList read FMaterialSegmentList;
    property DPLS          : TObjectList read FDPLS;

    constructor Create(aFileName : String);
    destructor  Destroy();override;
  end;

implementation

uses
  GDResources,
  GDConsole;

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
  Resources.RemoveResource(TGDResource(FMaterial));
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
  FPolygons := TObjectList.Create();
  FPolygons.OwnsObjects := False;
  Inherited;
end;

{******************************************************************************}
{* Destroy the materialrendersegment class                                    *}
{******************************************************************************}

destructor  TGDMaterialSegment.Destroy();
begin
  Resources.RemoveResource(TGDResource(FMaterial));
  FMaterial := Nil;
  FreeAndNil(FPolygons);
  Inherited;
end;

{******************************************************************************}
{* Create the mesh class                                                      *}
{******************************************************************************}

constructor TGDMesh.Create( aFileName : String );
var
  iIdx : Integer;
  iFile : TMemoryStream;
  iStr, iError : String;
  iVec, iNorm : TGDVector;
  iUV : TGDUVCoord;
  iMat : TGDMaterial;
  iResult : boolean;
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
    iResult := true;

    FVertices            := TGDVectorList.Create();
    FNormals             := TGDVectorList.Create();
    FUV                  := TGDUVCoordList.Create();
    FPolygons            := TObjectList.Create();
    FMaterialSegmentList := TObjectList.Create();
    FDPLS                := TObjectList.Create();

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
        Resources.LoadMaterials(ExtractFilePath(aFileName) + iStr);
        continue;
      end
      else if iStr = 'v' then //read a vertex
      begin
        iVec.x := StrToFloat(GetNextToken(iFile));
        iVec.y := StrToFloat(GetNextToken(iFile));
        iVec.z := StrToFloat(GetNextToken(iFile));
        FVertices.Add(iVec);
        continue;
      end
      else if iStr = 'vt' then //read a uv
      begin
        iUV.U := StrToFloat(GetNextToken(iFile));
        iUV.V := -StrToFloat(GetNextToken(iFile));
        FUV.Add(iUV);
        continue;
      end
      else if iStr = 'vn' then //read a normal
      begin
        iNorm.x := StrToFloat(GetNextToken(iFile));
        iNorm.y := StrToFloat(GetNextToken(iFile));
        iNorm.z := StrToFloat(GetNextToken(iFile));
        FNormals.Add(iNorm);
        continue;
      end
      else if iStr = 'usemtl' then //read the current material for the faces
      begin
        iStr := GetNextToken(iFile);
        if not(Resources.Find(iStr, iIdx)) then
           Raise Exception.Create('Material ' + iStr + ' doesn`t excist!');
        iMat := TGDMaterial(Resources.Data[iIdx]);
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
      iResult := false;
    end;
  end;

  Console.Use:=true;
  Console.WriteOkFail(iResult, iError);

  CreateMaterialSegmentLists();
  CreateDisplayLists();
end;

{******************************************************************************}
{* Destroy the mesh class                                                     *}
{******************************************************************************}

destructor  TGDMesh.Destroy();
begin
  FVertices.Clear();
  FNormals.Clear();
  FUV.Clear();
  FPolygons.Clear();
  FMaterialSegmentList.Clear();
  FreeAndNil(FVertices);
  FreeAndNil(FNormals);
  FreeAndNil(FUV);
  FreeAndNil(FPolygons);
  FreeAndnil(FMaterialSegmentList);
  FreeAndNil(FDPLS);
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
    iTempPolygon := TGDMeshPolygon(Fpolygons.Items[iI]);

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
{* Create the mesh material segment lists                                     *}
{******************************************************************************}

procedure TGDMesh.CreateDisplayLists();
var
  iI   : Integer;
  iMS  : TGDMaterialSegment;
  iDPL : TGDGLDisplayList;
  iJ   : Integer;
  iPL  : TGDMeshPolygon;
begin
  //prepare the displaylists.
  for iI := 0 to FMaterialSegmentList.Count - 1 do
  begin
    iMS := TGDMaterialSegment(FMaterialSegmentList.Items[iI]);
    iDPL := TGDGLDisplayList.Create();
    iDPL.InitDisplayList();
    iDPL.StartList();

    glBegin(GL_TRIANGLES);
    for iJ := 0 to iMS.Polygons.Count-1 do
    begin
      glColor3f(0.75 + (Random(25)/100), 0.75 + (Random(25)/100), 0.75 + (Random(25)/100));
      iPL := TGDMeshPolygon(iMS.Polygons.Items[iJ]);
      //V1
      glNormal3fv( FNormals.Items[ iPL.P1.NormalID ].ArrayPointer );
      glTexCoord2fv( FUV.Items[ iPL.P1.UVID ].ArrayPointer );
      glVertex3fv( FVertices.Items[ iPL.P1.VertexID ].ArrayPointer );
      //V2
      glNormal3fv( FNormals.Items[ iPL.P2.NormalID ].ArrayPointer );
      glTexCoord2fv( FUV.Items[ iPL.P2.UVID ].ArrayPointer );
      glVertex3fv( FVertices.Items[ iPL.P2.VertexID ].ArrayPointer );
      //V3
      glNormal3fv( FNormals.Items[ iPL.P3.NormalID ].ArrayPointer );
      glTexCoord2fv( FUV.Items[ iPL.P3.UVID ].ArrayPointer );
      glVertex3fv( FVertices.Items[ iPL.P3.VertexID ].ArrayPointer );
    end;
    glEnd();

    iDPL.EndList();
    FDPLS.Add(iDPL);
  end;
end;

end.
