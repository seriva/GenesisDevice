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
unit GDTerrain;

{$MODE Delphi}

interface

uses
  SysUtils,
  Classes,
  Math,
  Graphics,
  IniFiles,
  dglOpenGL,
  GDTexture,
  GDTypes,
  GDGLWrappers,
  GDTypesGenerics,
  GDResource,
  GDConstants;

type

{******************************************************************************}
{*  Terrain class                                                             *}
{******************************************************************************}

  TGDTerrain = class
  private
    FTerrainWidth      : Integer;
    FTerrainHeight     : Integer;
    FTerrainTop        : Double;
    FTerrainBottom     : Double;
    FColorTexture      : TGDTexture;
    FDetailTexture1    : TGDTexture;
    FDetailTexture2    : TGDTexture;
    FDetailTexture3    : TGDTexture;
    FDetailLookup      : TGDTexture;
    FTerrainLoaded     : Boolean;
    FDetailUV          : Integer;
    FCausticUV         : Integer;
    FTriangleSize      : Integer;
    FHeightScale       : Integer;
    FVertices          : TGDVertex_V_UV_N_List;
    FVertexBuffer      : TGDGLVertexBuffer;
  public
    property TerrainWidth  : Integer read FTerrainWidth;
    property TerrainHeight : Integer read FTerrainHeight;
    property TerrainLoaded : Boolean read FTerrainLoaded;
    property DetailUV      : integer read FDetailUV;
    property CausticUV     : integer read FCausticUV;
    property TriangleSize  : Integer read FTriangleSize;
    property HeightScale   : Integer read FHeightScale;

    constructor Create();
    destructor  Destroy(); override;

    function  InitTerrain( aIniFile : TIniFile ) : boolean;
    procedure Clear();

    function  GetHeight( aX, aZ : Double; var aHeight : Double ): boolean;
    function  GetRotation( aX, aZ : Double; var aRotation : TGDVector ): boolean;
    function  GetPoint(aX, aZ : Integer): TGDVertex_V_UV_N;

    procedure StartRendering( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor );
    procedure EndRendering();
  end;

implementation

uses
  GDEngine;

{******************************************************************************}
{* Create the terrain class                                                   *}
{******************************************************************************}

constructor TGDTerrain.Create();
begin
  FTerrainWidth  := 0;
  FTerrainHeight := 0;
  FTerrainTop    := 0;
  FTerrainBottom := 0;
  FTerrainLoaded := False;
  FDetailUV      := 0;
  FTriangleSize  := 0;
  FHeightScale   := 0;
  FVertices      := TGDVertex_V_UV_N_List.Create();
end;

{******************************************************************************}
{* Destroy the terrain class                                                  *}
{******************************************************************************}

destructor TGDTerrain.Destroy();
begin
  inherited;
  Clear();
  FreeAndNil(FVertices);
end;

{******************************************************************************}
{* Init terrain                                                               *}
{******************************************************************************}

function  TGDTerrain.InitTerrain( aIniFile : TIniFile ) : boolean;
var
  iMapHeight  : Byte;
  iBmp        : TBitmap;
  iX, iY, iStartWidth, iStartHeight : integer;
  iError    : String;
  iM : TGDMatrix;
  iR : TGDVector;
  iV : TGDVertex_V_UV_N;
begin
  Clear();

  Engine.Console.Write('Loading terrain...');
  Engine.Console.Use := false;
  try
    result := true;
    FTerrainLoaded := true;

    iBmp := TBitmap.Create();
    iBmp.pixelformat := pf32bit;
    iBmp.LoadFromFile(aIniFile.ReadString( 'Terrain', 'HeightMap', 'heightmap.bmp' ));
    FTriangleSize  := aIniFile.ReadInteger('Terrain', 'TriangleSize', 512 );
    FHeightScale   := aIniFile.ReadInteger('Terrain', 'HeightScale',  64 );
    FTerrainWidth  := iBmp.Width;
    FTerrainHeight := iBmp.Height;
    FDetailUV      := aIniFile.ReadInteger('Terrain', 'DetailMapUV', 100 );
    FCausticUV     := aIniFile.ReadInteger('Terrain', 'CausticUV', 100 );

    if ((FTerrainWidth mod 2) <> 1) or ((FTerrainHeight mod 2) <> 1) then
      Raise Exception.Create('Heightmap dimensions are incorrect!');

    iStartWidth    := -((FTerrainWidth * FTriangleSize) div 2);
    iStartHeight   := -((FTerrainHeight * FTriangleSize) div 2);
    FTerrainTop    := -999999999999999;
    FTerrainBottom :=  999999999999999;
    for iX := 0 to (FTerrainWidth-1) do
    begin
      for iY := 0 to (FTerrainHeight-1) do
      begin
        iMapHeight := iBmp.Canvas.Pixels[iX,iY] mod $100;
        If iMapHeight > FTerrainTop then
          FTerrainTop := iMapHeight;
        If iMapHeight < FTerrainBottom then
          FTerrainBottom := iMapHeight;
        iV.Vertex.Reset(iStartWidth + (iX * FTriangleSize),(iMapHeight)*FHeightScale, iStartHeight + (iY * FTriangleSize));
        iV.Normal.Reset(0,1,0);
        iV.UV.Reset((iX / (FTerrainWidth-1)), (iY / (FTerrainHeight-1)));
        FVertices.Add(iV);
      end;
    end;
    FTerrainTop    := (FTerrainTop) * FHeightScale;
    FTerrainBottom := (FTerrainBottom) * FHeightScale;
    FreeAndNil(iBmp);

    for iX := 0 to (FTerrainWidth-1) do
    begin
      for iY := 0 to (FTerrainHeight-1) do
      begin
        if (iX = (FTerrainWidth-1)) and (iY = FTerrainHeight-1) then
        begin
           FVertices.List^[(iX * FTerrainWidth) + iY].Normal := GetPoint(iX-1,iY-1).Normal.Copy();
           continue;
        end
        else if iX = (FTerrainWidth-1) then
        begin
           FVertices.List^[(iX * FTerrainWidth) + iY].Normal := GetPoint(iX-1,iY).Normal.Copy();
           continue;
        end
        else if iY = FTerrainHeight-1 then
        begin
          FVertices.List^[(iX * FTerrainWidth) + iY].Normal := GetPoint(iX,iY-1).Normal.Copy();
          continue;
        end;
        GetRotation(iStartWidth + iX*FTriangleSize, iStartHeight + iY*FTriangleSize, iR);
        iM.CreateRotation( iR );
        iM.ApplyToVector( FVertices.List^[(iX * FTerrainWidth) + iY].Normal );
      end;
    end;

    FColorTexture   := Engine.Resources.LoadTexture(aIniFile.ReadString( 'Terrain', 'ColorMap', 'colormaps.bmp'), Engine.Settings.TextureDetail,Engine.Settings.TextureFilter);
    FDetailTexture1 := Engine.Resources.LoadTexture(aIniFile.ReadString( 'Terrain', 'DetailMap1', 'detailmap1.jpg'), Engine.Settings.TextureDetail,Engine.Settings.TextureFilter);
    FDetailTexture2 := Engine.Resources.LoadTexture(aIniFile.ReadString( 'Terrain', 'DetailMap2', 'detailmap2.jpg'), Engine.Settings.TextureDetail,Engine.Settings.TextureFilter);
    FDetailTexture3 := Engine.Resources.LoadTexture(aIniFile.ReadString( 'Terrain', 'DetailMap3', 'detailmap3.jpg'), Engine.Settings.TextureDetail,Engine.Settings.TextureFilter);
    FDetailLookup   := Engine.Resources.LoadTexture(aIniFile.ReadString( 'Terrain', 'DetailDistribution', 'detaillookup.jpg') ,Engine.Settings.TextureDetail,Engine.Settings.TextureFilter);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    FVertexBuffer := TGDGLVertexBuffer.Create();
    FVertexBuffer.Bind(VL_NONE);
    FVertexBuffer.Update(FVertices, GL_STATIC_DRAW);
    FVertexBuffer.Unbind();
  except
    on E: Exception do
    begin
      iError := E.Message;
      result := false;
      FTerrainLoaded := false;
    end;
  end;

  Engine.Console.Use := true;

  Engine.Console.WriteOkFail(result, iError);
end;

{******************************************************************************}
{* Clear the terrain                                                          *}
{******************************************************************************}

procedure TGDTerrain.Clear();
begin
  FVertices.Clear;
  FreeAndNil(FVertexBuffer);

  Engine.Resources.RemoveResource(TGDResource(FColorTexture));
  Engine.Resources.RemoveResource(TGDResource(FDetailTexture1));
  Engine.Resources.RemoveResource(TGDResource(FDetailTexture2));
  Engine.Resources.RemoveResource(TGDResource(FDetailTexture3));
  Engine.Resources.RemoveResource(TGDResource(FDetailLookup));

  FTerrainLoaded := False;
end;

{******************************************************************************}
{* Get the heigth on a position on the terrain                                *}
{******************************************************************************}

function TGDTerrain.GetHeight( aX, aZ : Double; var aHeight : Double ): boolean;
var
  iRX, iRZ : integer;
  iFX, iFZ,
  iA, iB, iC, iD : Double;
begin
   aX := (aX + ((FTerrainWidth * FTriangleSize)/2)) / FTriangleSize;
   aZ := (aZ + ((FTerrainHeight * FTriangleSize)/2)) / FTriangleSize;
   if (aX < 0) or (aX >= FTerrainWidth-1) or
      (aZ < 0) or (aZ >= FTerrainHeight-1) or
       Not(FTerrainLoaded) then
   begin
     aHeight := 0.0;
     result := false;
     exit;
   end;
   iRX := trunc(aX);
   iRZ := trunc(aZ);
   iFX := aX - iRX;
   iFZ := aZ - iRZ;
   iA := GetPoint(iRX   ,iRZ).Vertex.Y;
   iB := GetPoint(iRX+1 ,iRZ).Vertex.Y;
   iC := GetPoint(iRX   ,iRZ+1).Vertex.Y;
   iD := GetPoint(iRX+1 ,iRZ+1).Vertex.Y;
   aHeight := (iA + (iB - iA) * iFX) + ((iC + (iD - iC) * iFX) - (iA + (iB - iA) * iFX)) * iFZ;
   result := true;
end;

{******************************************************************************}
{* Get the rotation on a position on the terrain                              *}
{******************************************************************************}

function  TGDTerrain.GetRotation( aX, aZ : Double; var aRotation : TGDVector ): boolean;
var
  iRX, iRZ : integer;
  iFX, iFZ, iA, iB, iC, iD : Double;
  iTriangle : TGDTriangle;
  iPos, iUp, iForward, iRight : TGDVector;
  iFound : boolean;
  iSecTris : boolean;

Function atan2(y : extended; x : extended): Extended;
Assembler;
asm
  fld [y]
  fld [x]
  fpatan
end;

begin
  result := false;
  aRotation.Reset(0,0,0);
  iPos.Reset(aX,0,aZ);
  aX := (aX + ((FTerrainWidth * FTriangleSize)/2)) / FTriangleSize;
  aZ := (aZ + ((FTerrainHeight * FTriangleSize)/2)) / FTriangleSize;
  if (aX < 0) or (aX > FTerrainWidth-1) or
     (aZ < 0) or (aZ > FTerrainHeight-1) or
     Not(FTerrainLoaded) then
  begin
    exit;
  end;

  iRX := trunc(aX);
  iRZ := trunc(aZ);
  iFX := aX - iRX;
  iFZ := aZ - iRZ;
  iA := GetPoint(iRX   ,iRZ).Vertex.Y;
  iB := GetPoint(iRX+1 ,iRZ).Vertex.Y;
  iC := GetPoint(iRX   ,iRZ+1).Vertex.Y;
  iD := GetPoint(iRX+1 ,iRZ+1).Vertex.Y;
  iPos.Y := (iA + (iB - iA) * iFX) + ((iC + (iD - iC) * iFX) - (iA + (iB - iA) * iFX)) * iFZ;

  iTriangle.V1 := GetPoint(iRX, iRZ).Vertex.Copy();
  iTriangle.V2 := GetPoint(iRX+1, iRZ).Vertex.Copy();
  iTriangle.V3 := GetPoint(iRX, iRZ+1).Vertex.Copy();
  iFound := true;
  iSecTris := false;;
  If Not(iTriangle.PointInTraingle( iPos )) then
  begin
    iTriangle.V1 := GetPoint(iRX+1, iRZ+1).Vertex.Copy();
    iTriangle.V2 := GetPoint(iRX, iRZ+1).Vertex.Copy();
    iTriangle.V3 := GetPoint(iRX+1, iRZ).Vertex.Copy();
    iSecTris := true;
    If Not(iTriangle.PointInTraingle( iPos )) then
      iFound := false;
  end;

  if iFound then
  begin
    if Not(iSecTris) then
    begin
      iForward := iTriangle.V2.Copy();
      iForward.Substract( iTriangle.V1 );
      iForward.Normalize();
      iRight := iTriangle.V3.Copy();
      iRight.Substract( iTriangle.V1 );
      iRight.Normalize();
    end
    else
    begin
      iForward := iTriangle.V1.Copy();
      iForward.Substract( iTriangle.V2 );
      iForward.Normalize();
      iRight := iTriangle.V1.Copy();
      iRight.Substract( iTriangle.V3 );
      iRight.Normalize();
    end;

    iUp.CrossProduct( iRight, iForward );
    iUp.Normalize();

    if iUP.X > 0.998 then
    begin
      aRotation.X := RadToDeg( aTan2( iForward.Z, iRight.Z ) );
      aRotation.Y := PI/2;
      aRotation.Z := 0;
    end
    else
      if iUP.X < -0.998 then
      begin
        aRotation.X := RadToDeg( aTan2( iForward.Z, iRight.Z ) );
        aRotation.Y := -PI/2;
        aRotation.Z := 0;
      end
      else
      begin
        aRotation.X := RadToDeg( aTan2( -iUp.Z, iUp.Y ) );
        aRotation.Y := RadToDeg( aTan2( -iRight.X, iForward.X ) );
        aRotation.Z := RadToDeg( ArcSin( iUp.X ) );
      end;
    result := true;
  end;
end;

{******************************************************************************}
{* Get terrain point.                                                         *}
{******************************************************************************}

function TGDTerrain.GetPoint(aX, aZ : Integer): TGDVertex_V_UV_N;
begin
  result := FVertices.Items[(aX * FTerrainWidth) + aZ];
end;

{******************************************************************************}
{* Start the rendering of a terraincell                                       *}
{******************************************************************************}

procedure TGDTerrain.StartRendering( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor );
begin
  if Not(aRenderAttribute = RA_NORMAL) then exit;

  FVertexBuffer.Bind(VL_V_UV_N);
  if Engine.Modes.RenderWireframe then
  begin
    Engine.Renderer.SetColor(0.2,0.8,0.2,1);
  end
  else
  begin
    case aRenderFor of
     RF_NORMAL, RF_WATER : begin
                   with Engine.Renderer do
                   begin
                     TerrainShader.Bind();
                     SetJoinedParams(TerrainShader);
                     TerrainShader.SetInt('T_COLORTEX', 0);
                     TerrainShader.SetInt('T_DETAILTEX1', 1);
                     TerrainShader.SetInt('T_DETAILTEX2', 2);
                     TerrainShader.SetInt('T_DETAILTEX3', 3);
                     TerrainShader.SetInt('T_WEIGHT_LOOKUP', 4);
                     TerrainShader.SetInt('T_CAUSTIC_TEX', 5);
                     TerrainShader.SetInt('I_DETAIL_UV', FDetailUV);
                     TerrainShader.SetInt('I_CAUSTIC_UV', FCausticUV);
                   end;

                   FColorTexture.BindTexture(GL_TEXTURE0);
                   FDetailTexture1.BindTexture(GL_TEXTURE1);
                   FDetailTexture2.BindTexture(GL_TEXTURE2);
                   FDetailTexture3.BindTexture(GL_TEXTURE3);
                   FDetailLookup.BindTexture(GL_TEXTURE4);
                   Engine.Map.Water.BindCausticTexture();
                 end;
      RF_BLOOM : begin
                   Engine.Renderer.RenderState( RS_COLOR );
                   glEnable(GL_DEPTH_TEST);
                   Engine.Renderer.SetColor(0,0,0,1)
                 end;
    end;
  end;
end;

{******************************************************************************}
{* Stop the rendering of a terrain                                            *}
{******************************************************************************}

procedure TGDTerrain.EndRendering();
begin
  FVertexBuffer.UnBind();
end;

end.
