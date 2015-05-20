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

  TGDMeshPoint = record
    VertexID : Integer;
    NormalID : Integer;
    UVID : Integer;
  end;

{******************************************************************************}
{* Meshpolygon class                                                          *}
{******************************************************************************}

  TGDPolygon = class (TObject)
  private
    FPolygonType : TGDPolygonType;
    FMaterial    : TGDMaterial;
    FPoint1      : TGDMeshPoint;
    FPoint2      : TGDMeshPoint;
    FPoint3      : TGDMeshPoint;
    FPoint4      : TGDMeshPoint;
  public
    property PolygonType : TGDPolygonType read FPolygonType;
    property Material : TGDMaterial read FMaterial;
    property P1  : TGDMeshPoint read FPoint1;
    property P2  : TGDMeshPoint read FPoint2;
    property P3  : TGDMeshPoint read FPoint3;
    property P4  : TGDMeshPoint read FPoint4;

    constructor Create();
    destructor  Destroy();override;
  end;

{******************************************************************************}
{* Segment class                                                              *}
{******************************************************************************}

  TGDSegment = class (TObject)
  private
    FMaterial   : TGDMaterial;
    FPolygons   : TObjectList;
  public
    property Material : TGDMaterial read FMaterial;
    property Polygons : TObjectList read FPolygons;

    constructor Create();
    destructor  Destroy();override;
  end;

{******************************************************************************}
{* Mesh class                                                                 *}
{******************************************************************************}

  TGDMesh = class (TGDResource)
  private
    FFileName : String;
    FVertices : TGDVectorList;
    FNormals  : TGDVectorList;
    FUV       : TGDUVCoordList;
    FPolygons : TObjectList;
    FSegments : TObjectList;
    FDPLS     : TObjectList;

    procedure CreateSegmentLists();
    procedure CreateDisplayLists();
  public
    property FileName : String read FFileName;
    property Vertices : TGDVectorList read FVertices;
    property Normals  : TGDVectorList read FNormals;
    property UV       : TGDUVCoordList read FUV;
    property Polygons : TObjectList read FPolygons;
    property Segments : TObjectList read FSegments;
    property DPLS     : TObjectList read FDPLS;

    constructor Create(aFileName : String);
    destructor  Destroy();override;
  end;

implementation

uses
  GDResources,
  GDConsole;

{******************************************************************************}
{* Create the staticmeshpolygon class                                         *}
{******************************************************************************}

constructor TGDPolygon.Create();
begin
  FPolygonType := PT_TRIANGLE;
  FMaterial := Nil;
  Inherited;
end;

{******************************************************************************}
{* Destroy the staticmeshpolygon class                                        *}
{******************************************************************************}

destructor  TGDPolygon.Destroy();
begin
  Resources.RemoveResource(TGDResource(FMaterial));
  Inherited;
end;

{******************************************************************************}
{* Create the materialrendersegment class                                     *}
{******************************************************************************}

constructor TGDSegment.Create();
begin
  FMaterial := Nil;
  FPolygons := TObjectList.Create();
  FPolygons.OwnsObjects := False;
  Inherited;
end;

{******************************************************************************}
{* Destroy the materialrendersegment class                                    *}
{******************************************************************************}

destructor  TGDSegment.Destroy();
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
  iQuads : boolean;
  iPolygon : TGDPolygon;

procedure ParsePointSet(aStr : String; var aPoint : TGDMeshPoint );
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

  aPoint.VertexID:=iIdx[0];
  aPoint.UVID:=iIdx[1];
  aPoint.NormalID:=iIdx[2];
end;

begin
  Console.Write('Loading mesh ' + aFileName + '...');
  try
    iResult := true;

    iQuads    := false;
    FVertices := TGDVectorList.Create();
    FNormals  := TGDVectorList.Create();
    FUV       := TGDUVCoordList.Create();
    FPolygons := TObjectList.Create();
    FSegments := TObjectList.Create();
    FDPLS     := TObjectList.Create();

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
      else if iStr = 'quads' then //read a normal
      begin
        iQuads := true;
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
        iPolygon := TGDPolygon.Create();
        iPolygon.FMaterial := iMat;
        ParsePointSet(GetNextToken(iFile), iPolygon.FPoint1);
        ParsePointSet(GetNextToken(iFile), iPolygon.FPoint2);
        ParsePointSet(GetNextToken(iFile), iPolygon.FPoint3);
        if iQuads then
        begin
          iPolygon.FPolygonType := PT_QUAD;
          ParsePointSet(GetNextToken(iFile), iPolygon.FPoint4);
        end;
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

  CreateSegmentLists();
  CreateDisplayLists();
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
  FreeAndnil(FSegments);
  FreeAndNil(FDPLS);
end;

{******************************************************************************}
{* Create the mesh material segment lists                                     *}
{******************************************************************************}

procedure TGDMesh.CreateSegmentLists();
var
  iI    : Integer;
  iCount: Integer;
  iMat  : TGDMaterial;
  iMS   : TGDSegment;
  iPolygon : TGDPolygon;

procedure StartMaterialSegment();
begin
  iMS := TGDSegment.Create();
  iMS.FMaterial := iPolygon.Material;
end;

procedure EndMaterialSegment();
begin
  FSegments.Add( iMS );
end;

begin
  iMat:= nil;
  iCount := Fpolygons.Count - 1;
  for iI := 0 to iCount do
  begin
    iPolygon := TGDPolygon(Fpolygons.Items[iI]);

    If iMat <> iPolygon.Material then
    begin
      if iMat = nil then
        StartMaterialSegment()
      else
      begin
        EndMaterialSegment();
        StartMaterialSegment()
      end;
    end;

    iMS.Polygons.Add(iPolygon);

    iMat := iPolygon.Material;
  end;

  EndMaterialSegment();
end;

{******************************************************************************}
{* Create the mesh material segment lists                                     *}
{******************************************************************************}

procedure TGDMesh.CreateDisplayLists();
var
  iI   : Integer;
  iMS  : TGDSegment;
  iDPL : TGDGLDisplayList;
  iJ   : Integer;
  iPL  : TGDPolygon;
  iFirst : boolean;
begin
  //prepare the displaylists.
  for iI := 0 to FSegments.Count - 1 do
  begin
    iMS  := TGDSegment(FSegments.Items[iI]);
    iDPL := TGDGLDisplayList.Create();
    iDPL.InitDisplayList();
    iDPL.StartList();
    iFirst := true;

    for iJ := 0 to iMS.Polygons.Count-1 do
    begin
      iPL := TGDPolygon(iMS.Polygons.Items[iJ]);
      if iFirst then
      begin
        if iPL.FPolygonType = PT_TRIANGLE then
          glBegin(GL_TRIANGLES)
        else
          glBegin(GL_QUADS);
        iFirst := false;
      end;

      glColor3f(0.75 + (Random(25)/100), 0.75 + (Random(25)/100), 0.75 + (Random(25)/100));

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

      //V4
      if iPL.FPolygonType = PT_QUAD then
      begin
        glNormal3fv( FNormals.Items[ iPL.P4.NormalID ].ArrayPointer );
        glTexCoord2fv( FUV.Items[ iPL.P4.UVID ].ArrayPointer );
        glVertex3fv( FVertices.Items[ iPL.P4.VertexID ].ArrayPointer );
      end;
    end;
    glEnd();

    iDPL.EndList();
    FDPLS.Add(iDPL);
  end;
end;

end.
