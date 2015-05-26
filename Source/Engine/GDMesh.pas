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
  GDGLObjects,
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
    FMaterial  : TGDMaterial;
    FTriangles : TGDTriangleIdxList;
    FDPL       : TGDGLDisplayList;
  public
    property Material  : TGDMaterial read FMaterial;
    property Triangles : TGDTriangleIdxList read FTriangles;
    property DPL       : TGDGLDisplayList read FDPL;

    constructor Create();
    destructor  Destroy();override;
  end;
  TGDSurfaceList = specialize TFPGObjectList<TGDSurface>;

{******************************************************************************}
{* Mesh class                                                                 *}
{******************************************************************************}

  TGDMesh = class (TGDResource)
  private
    FVertices : TGDVectorList;
    FNormals  : TGDVectorList;
    FUV       : TGDUVCoordList;
    FSurfaces : TGDSurfaceList;
    FTriangleCount : Integer;

    procedure CreateDisplayList();
  public
    property Vertices : TGDVectorList read FVertices;
    property Normals  : TGDVectorList read FNormals;
    property UV       : TGDUVCoordList read FUV;
    property Surfaces : TGDSurfaceList read FSurfaces;
    property TriangleCount : Integer read FTriangleCount;

    constructor Create(aFileName : String);
    destructor  Destroy();override;
  end;

implementation

uses
  GDResources,
  GDConsole;

{******************************************************************************}
{* Create surface                                                             *}
{******************************************************************************}

constructor TGDSurface.Create();
begin
  FMaterial := Nil;
  FTriangles := TGDTriangleIdxList.Create();
  FDPL       := TGDGLDisplayList.Create();
  Inherited;
end;

{******************************************************************************}
{* Destroy surface                                                            *}
{******************************************************************************}

destructor TGDSurface.Destroy();
begin
  Resources.RemoveResource(TGDResource(FMaterial));
  FreeAndNil(FTriangles);
  FreeAndNil(FDPL);
  Inherited;
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
  iTri : TGDTriangleIdxs;
  iSur : TGDSurface;

procedure ParsePointSet(aStr : String; var aTriangle : TGDTriangleIdxs; aStartIndex : integer);
begin
  iSL.Clear();
  ExtractStrings(['/'], [], PChar(AnsiString(aStr)), iSL);
  aTriangle.Data[aStartIndex]   := StrToInt(iSL.Strings[0])-1;
  aTriangle.Data[aStartIndex+1] := StrToInt(iSL.Strings[1])-1;
  aTriangle.Data[aStartIndex+2] := StrToInt(iSL.Strings[2])-1;
end;

begin
  Console.Write('Loading mesh ' + aFileName + '...');
  try
    iResult := true;

    iSL       := TStringList.Create();
    FVertices := TGDVectorList.Create();
    FNormals  := TGDVectorList.Create();
    FUV       := TGDUVCoordList.Create();
    FSurfaces := TGDSurfaceList.Create();

    If Not(FileExistsUTF8(aFileName) ) then
      Raise Exception.Create('Mesh ' + aFileName + ' doesn`t excist!');

    //create the filestream
    Name := aFileName;
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

        iSur := TGDSurface.Create();
        iSur.FMaterial := iMat;
        FSurfaces.Add(iSur);
        continue;
      end
      else if iStr = 'f' then //read a face (we only support triangles)
      begin
        if (iSur = nil) then
           Raise Exception.Create('No material for polygon!');

        //we only support triangles now.
        ParsePointSet(GetNextToken(iFile), iTri, 0);
        ParsePointSet(GetNextToken(iFile), iTri, 3);
        ParsePointSet(GetNextToken(iFile), iTri, 6);
        iSur.FTriangles.Add(iTri);
        continue;
      end;
    end;
    FreeAndNil(iFile);
    CreateDisplayList();
  except
    on E: Exception do
    begin
      iError := E.Message;
      iResult := false;
    end;
  end;

  FreeAndNil(iSL);
  Console.Use:=true;
  Console.WriteOkFail(iResult, iError);
end;

{******************************************************************************}
{* Destroy the mesh class                                                     *}
{******************************************************************************}

destructor  TGDMesh.Destroy();
begin
  FreeAndNil(FVertices);
  FreeAndNil(FNormals);
  FreeAndNil(FUV);
  FreeAndnil(FSurfaces);
end;

{******************************************************************************}
{* Create the segment dpl                                                     *}
{******************************************************************************}

procedure TGDMesh.CreateDisplayList();
var
  iI     : Integer;
  iSur   : TGDSurface;
  iJ     : Integer;
  iTri   : TGDTriangleIdxs;
  ITrisCounter : Integer;

procedure SendPoint(aTri : TGDTriangleIdxs; aStartIdx : integer);
begin
  glNormal3fv( FNormals.Items[ aTri.Data[aStartIdx+2] ].ArrayPointer );
  glTexCoord2fv( FUV.Items[ aTri.Data[aStartIdx+1] ].ArrayPointer );
  glVertex3fv( FVertices.Items[ aTri.Data[aStartIdx] ].ArrayPointer );
end;

begin
  for iI := 0 to FSurfaces.Count - 1 do
  begin
    iSur  := FSurfaces.Items[iI];
    iSur.DPL.StartList();
    ITrisCounter := 0;
    glBegin(GL_TRIANGLES);
    for iJ := 0 to iSur.Triangles.Count-1 do
    begin
      iTri := iSur.Triangles.Items[iJ];

      //Add the random animation factor for tree animations.
      if iSur.Material.DoTreeAnim then
      begin
        if ITrisCounter = 0 then
         glColor3f(0.75 + (Random(25)/100), 0.75 + (Random(25)/100), 0.75 + (Random(25)/100));
        inc(ITrisCounter);
        if ITrisCounter = 2 then ITrisCounter := 0;
      end;

      SendPoint(iTri, 0);
      SendPoint(iTri, 3);
      SendPoint(iTri, 6);
    end;
    glEnd();
    iSur.DPL.EndList();
    FTriangleCount := FTriangleCount + iSur.Triangles.Count;
  end;
end;

end.
