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

{******************************************************************************}
{* Hold the terrain class                                                     *}
{******************************************************************************}

uses
  SysUtils,
  Classes,
  Math,
  Graphics,
  dglOpenGL,
  GDTexture,
  GDTypes,
  GDConsole,
  GDGUI,
  GDSettings,
  GDConstants,
  GDResource,
  GDResources,
  GDModes;

type

{******************************************************************************}
{* Terrain input record                                                       *}
{******************************************************************************}

  TGDTerrainInput = record
    HeightMap      : String;
    ColorMap       : String;
    Detail1        : String;
    Detail2        : String;
    Detail3        : String;
    DetailLookup   : String;
    TriangleSize   : Integer;
    HeightScale    : Integer;
    DetailUV       : Integer;
    CausticUV      : Integer;
  end;

{******************************************************************************}
{* Terrain point class                                                        *}
{******************************************************************************}

  TGDTerrainPoint = record
    Vertex : TGDVector;
    Normal : TGDVector;
    ColorUVCoords  : TGDUVCoord;
    DetailUVCoords : TGDUVCoord;
    CausticUVCoords : TGDUVCoord;
  end;

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
  public
    TerrainPoints : array of array of TGDTerrainPoint;

    property TerrainWidth  : Integer read FTerrainWidth;
    property TerrainHeight : Integer read FTerrainHeight;
    property TerrainLoaded : Boolean read FTerrainLoaded;
    property DetailUV     : integer read FDetailUV;
    property CausticUV    : integer read FCausticUV;
    property TriangleSize : Integer read FTriangleSize;
    property HeightScale  : Integer read FHeightScale;

    constructor Create();
    destructor  Destroy(); override;

    function  InitTerrain( aInput : TGDTerrainInput ) : boolean;
    procedure Clear();

    function  GetHeight( aX, aZ : Double; var aHeight : Double ): boolean;
    function  GetRotation( aX, aZ : Double; var aRotation : TGDVector ): boolean;

    procedure StartRendering( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor );
    procedure EndRendering();
  end;

const
  CELLSIZE = 16;
  TRISINCELL = CELLSIZE * CELLSIZE * 2;

implementation

uses
  GDMap,
  GDRenderer;

{******************************************************************************}
{* Create the terrain class                                                   *}
{******************************************************************************}

constructor TGDTerrain.Create();
begin
  TerrainPoints   := nil;
  FTerrainWidth   := 0;
  FTerrainHeight  := 0;
  FTerrainTop     := 0;
  FTerrainBottom  := 0;
  FTerrainLoaded  := False;
  FDetailUV       := 0;
  FTriangleSize   := 0;
  FHeightScale    := 0;
end;

{******************************************************************************}
{* Destroy the terrain class                                                  *}
{******************************************************************************}

destructor TGDTerrain.Destroy();
begin
  inherited;
  Clear();
end;

{******************************************************************************}
{* Init terrain                                                               *}
{******************************************************************************}

function  TGDTerrain.InitTerrain( aInput : TGDTerrainInput ) : boolean;
var
  iMapHeight  : Byte;
  iBmp1       : TBitmap;
  iJpg1       : TJPEGImage;
  iX, iY, iStartWidth, iStartHeight : integer;
  iError    : String;
  iM : TGDMatrix;
  iR : TGDVector;
begin
  Clear();

  Console.Write('Loading terrain...');
  Console.Use := false;
  try
    result := true;
    FTerrainLoaded := true;

    iJpg1 := TJPEGImage.Create();
    iBmp1 := TBitmap.Create();
    iBmp1.pixelformat := pf32bit;
    if copy(Uppercase(aInput.HeightMap), length(aInput.HeightMap)-3, 4) = '.JPG' then
    begin
      iJpg1.LoadFromFile(aInput.HeightMap);
      iBmp1.Width:=iJpg1.width;
      iBmp1.Height:=iJpg1.height;
      iBmp1.Canvas.Draw(0,0,iJpg1);
    end
    else
      iBmp1.LoadFromFile(aInput.HeightMap);

    FTriangleSize :=   aInput.TriangleSize;
    FHeightScale  :=   aInput.HeightScale;
    FTerrainWidth  :=  iBmp1.Width;
    FTerrainHeight :=  iBmp1.Height;

    FDetailUV := aInput.DetailUV;
    FCausticUV := aInput.CausticUV;

    if ((FTerrainWidth mod 2) <> 1) or ((FTerrainHeight mod 2) <> 1) then
      Raise Exception.Create('Heightmap dimensions are incorrect!');

    iStartWidth   := -((FTerrainWidth * FTriangleSize) div 2);
    iStartHeight  := -((FTerrainHeight * FTriangleSize) div 2);
    FTerrainTop          := -999999999999999;
    FTerrainBottom       :=  999999999999999;
    SetLength(TerrainPoints, FTerrainWidth);
    for iX := 0 to (FTerrainWidth-1) do
    begin
      SetLength(TerrainPoints[iX], FTerrainHeight);
      for iY := 0 to (FTerrainHeight-1) do
      begin
        iMapHeight := iBmp1.Canvas.Pixels[iX,iY] mod $100;
        If iMapHeight > FTerrainTop then
          FTerrainTop := iMapHeight;
        If iMapHeight < FTerrainBottom then
          FTerrainBottom := iMapHeight;
        TerrainPoints[iX,iY].ColorUVCoords.U := (iX / (FTerrainWidth-1));
        TerrainPoints[iX,iY].ColorUVCoords.V := (iY / (FTerrainHeight-1));
        TerrainPoints[iX,iY].DetailUVCoords.U := (FDetailUV * iX) / FTerrainWidth;
        TerrainPoints[iX,iY].DetailUVCoords.V := (FDetailUV * iY) / FTerrainHeight;
        TerrainPoints[iX,iY].CausticUVCoords.U := (FCausticUV * iX) / FTerrainWidth;
        TerrainPoints[iX,iY].CausticUVCoords.V := (FCausticUV * iY) / FTerrainHeight;
        TerrainPoints[iX,iY].Vertex.X := iStartWidth + (iX * FTriangleSize);
        TerrainPoints[iX,iY].Vertex.Y := (iMapHeight)*FHeightScale;
        TerrainPoints[iX,iY].Vertex.Z := iStartHeight + (iY * FTriangleSize);
      end;
    end;
    FTerrainTop     := (FTerrainTop) * FHeightScale;
    FTerrainBottom  := (FTerrainBottom) * FHeightScale;
    FreeAndNil(iBmp1);
    FreeAndNil(iJpg1);

    for iX := 0 to (FTerrainWidth-1) do
    begin
      for iY := 0 to (FTerrainHeight-1) do
      begin
        if (iX = (FTerrainWidth-1)) and (iY = FTerrainHeight-1) then
        begin
           TerrainPoints[iX,iY].Normal := TerrainPoints[iX-1,iY-1].Normal.Copy();
           continue;
        end
        else if iX = (FTerrainWidth-1) then
        begin
           TerrainPoints[iX,iY].Normal := TerrainPoints[iX-1,iY].Normal.Copy();
           continue;
        end
        else if iY = FTerrainHeight-1 then
        begin
          TerrainPoints[iX,iY].Normal := TerrainPoints[iX,iY-1].Normal.Copy();
          continue;
        end;
        GetRotation(iStartWidth + iX*FTriangleSize, iStartHeight + iY*FTriangleSize, iR);
        iM.EmptyMatrix();
        iM.CreateRotation( iR );
        TerrainPoints[iX,iY].Normal.reset(0,1,0);
        iM.ApplyToVector( TerrainPoints[iX,iY].Normal );
      end;
    end;
    GUI.LoadingScreen.UpdateBar();

    FColorTexture := Resources.LoadTexture(aInput.ColorMap ,Settings.TextureDetail,Settings.TextureFilter);
    GUI.LoadingScreen.UpdateBar();
    FDetailTexture1 := Resources.LoadTexture(aInput.Detail1 ,Settings.TextureDetail,Settings.TextureFilter);
    FDetailTexture2 := Resources.LoadTexture(aInput.Detail2 ,Settings.TextureDetail,Settings.TextureFilter);
    FDetailTexture3 := Resources.LoadTexture(aInput.Detail3 ,Settings.TextureDetail,Settings.TextureFilter);
    FDetailLookup   := Resources.LoadTexture(aInput.DetailLookup ,Settings.TextureDetail,Settings.TextureFilter);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    GUI.LoadingScreen.UpdateBar();
  except
    on E: Exception do
    begin
      iError := E.Message;
      result := false;
      FTerrainLoaded := false;
    end;
  end;

  Console.Use := true;

  Console.WriteOkFail(result, iError);
end;

{******************************************************************************}
{* Clear the terrain                                                          *}
{******************************************************************************}

procedure TGDTerrain.Clear();
var
  iX, iY: Integer;
begin
  if (TerrainPoints <> nil) then
  begin
    for iX := 0 to Length(TerrainPoints)-1 do
    begin
        setlength(TerrainPoints[iX],0);
    end;
    SetLength(TerrainPoints, 0);
  end;
  TerrainPoints := nil;

  Resources.RemoveResource(TGDResource(FColorTexture));
  Resources.RemoveResource(TGDResource(FDetailTexture1));
  Resources.RemoveResource(TGDResource(FDetailTexture2));
  Resources.RemoveResource(TGDResource(FDetailTexture3));
  Resources.RemoveResource(TGDResource(FDetailLookup));

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
   if (aX < 0) or (aX > FTerrainWidth-1) or
      (aZ < 0) or (aZ > FTerrainHeight-1) or
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
   iA := TerrainPoints[iRX   ,iRZ].Vertex.Y;
   iB := TerrainPoints[iRX+1 ,iRZ].Vertex.Y;
   iC := TerrainPoints[iRX   ,iRZ+1].Vertex.Y;
   iD := TerrainPoints[iRX+1 ,iRZ+1].Vertex.Y;
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
  iA := TerrainPoints[iRX   ,iRZ].Vertex.Y;
  iB := TerrainPoints[iRX+1 ,iRZ].Vertex.Y;
  iC := TerrainPoints[iRX   ,iRZ+1].Vertex.Y;
  iD := TerrainPoints[iRX+1 ,iRZ+1].Vertex.Y;
  iPos.Y := (iA + (iB - iA) * iFX) + ((iC + (iD - iC) * iFX) - (iA + (iB - iA) * iFX)) * iFZ;

  iTriangle.V1 := TerrainPoints[iRX, iRZ].Vertex.Copy();
  iTriangle.V2 := TerrainPoints[iRX+1, iRZ].Vertex.Copy();
  iTriangle.V3 := TerrainPoints[iRX, iRZ+1].Vertex.Copy();
  iFound := true;
  iSecTris := false;;
  If Not(iTriangle.PointInTraingle( iPos )) then
  begin
    iTriangle.V1 := TerrainPoints[iRX+1, iRZ+1].Vertex.Copy();
    iTriangle.V2 := TerrainPoints[iRX, iRZ+1].Vertex.Copy();
    iTriangle.V3 := TerrainPoints[iRX+1, iRZ].Vertex.Copy();
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
{* Start the rendering of a terraincell                                       *}
{******************************************************************************}

procedure TGDTerrain.StartRendering( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor );
begin
  if Not(aRenderAttribute = RA_NORMAL) then exit;

  if Modes.RenderWireframe then
  begin
    Renderer.SetColor(0.2,0.8,0.2,1);
  end
  else
  begin
    case aRenderFor of
     RF_NORMAL, RF_WATER : begin
                   Renderer.TerrainShader.Enable();
                   Renderer.SetJoinedParams(Renderer.TerrainShader);
                   Renderer.TerrainShader.SetInt('T_COLORTEX', 0);
                   Renderer.TerrainShader.SetInt('T_DETAILTEX1', 1);
                   Renderer.TerrainShader.SetInt('T_DETAILTEX2', 2);
                   Renderer.TerrainShader.SetInt('T_DETAILTEX3', 3);
                   Renderer.TerrainShader.SetInt('T_WEIGHT_LOOKUP', 4);
                   Renderer.TerrainShader.SetInt('T_CAUSTIC_TEX', 5);

                   If Map.Water.UnderWater() then
                     Renderer.TerrainShader.SetInt('I_UNDER_WATER', 1)
                   else
                     Renderer.TerrainShader.SetInt('I_UNDER_WATER', 0);
                   Renderer.TerrainShader.SetFloat('I_WATER_HEIGHT', Map.Water.WaterHeight);

                   FColorTexture.BindTexture(GL_TEXTURE0);
                   FDetailTexture1.BindTexture(GL_TEXTURE1);
                   FDetailTexture2.BindTexture(GL_TEXTURE2);
                   FDetailTexture3.BindTexture(GL_TEXTURE3);
                   FDetailLookup.BindTexture(GL_TEXTURE4);
                   Map.Water.BindCausticTexture();
                 end;
      RF_BLOOM : begin
                   Renderer.RenderState( RS_COLOR );
                   glEnable(GL_DEPTH_TEST);
                   Renderer.SetColor(0,0,0,1)
                 end;
    end;
  end;
end;

{******************************************************************************}
{* Stop the rendering of a terrain                                            *}
{******************************************************************************}

procedure TGDTerrain.EndRendering();
begin
end;

end.
