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
  GDRenderer,
  GDLighting,
  GDFog,
  GDWater,
  GDCamera,
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

  TGDTerrainPoint = class
  private
  public
    FVertex : TGDVector;
    FNormal : TGDVector;
    FColorUVCoords  : TGDUVCoord;
    FDetailUVCoords : TGDUVCoord;
    FCausticUVCoords : TGDUVCoord;
    constructor Create();
    destructor  Destroy(); override;
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
    property Top : Double read FTerrainTop;
    property Bottom : Double read FTerrainBottom;
    property DetailTexture1 : TGDTexture read FDetailTexture1;
    property DetailTexture2 : TGDTexture read FDetailTexture2;
    property DetailTexture3 : TGDTexture read FDetailTexture3;
    property DetailLookup   : TGDTexture read FDetailLookup;
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

var
  Terrain : TGDTerrain;

implementation

{******************************************************************************}
{* Create the terrainpoint class                                              *}
{******************************************************************************}

constructor TGDTerrainPoint.Create();
begin
  FVertex := TGDVector.Create();
  FNormal := TGDVector.Create();
  FColorUVCoords  := TGDUVCoord.Create();
  FDetailUVCoords := TGDUVCoord.Create();
  FCausticUVCoords := TGDUVCoord.Create();
end;

{******************************************************************************}
{* Destroy the terrainpoint class                                             *}
{******************************************************************************}

destructor  TGDTerrainPoint.Destroy();
begin
  FreeAndNil(FVertex);
  FreeAndNil(FNormal);
  FreeAndNil(FColorUVCoords);
  FreeAndNil(FDetailUVCoords);
  FreeAndNil(FCausticUVCoords);
  Inherited;
end;

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
  FColorTexture   := TGDTexture.Create();
  FDetailTexture1 := TGDTexture.Create();
  FDetailTexture2 := TGDTexture.Create();
  FDetailTexture3 := TGDTexture.Create();
  FDetailLookup   := TGDTexture.Create();
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
  FreeAndNil(FColorTexture);
  FreeAndNil(FDetailTexture1);
  FreeAndNil(FDetailTexture2);
  FreeAndNil(FDetailTexture3);
  FreeAndNil(FDetailLookup);
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
        TerrainPoints[iX,iY] := TGDTerrainPoint.Create();
        TerrainPoints[iX,iY].FColorUVCoords.U := (iX / (FTerrainWidth-1));
        TerrainPoints[iX,iY].FColorUVCoords.V := (iY / (FTerrainHeight-1));
        TerrainPoints[iX,iY].FDetailUVCoords.U := (FDetailUV * iX) / FTerrainWidth;
        TerrainPoints[iX,iY].FDetailUVCoords.V := (FDetailUV * iY) / FTerrainHeight;
        TerrainPoints[iX,iY].FCausticUVCoords.U := (FCausticUV * iX) / FTerrainWidth;
        TerrainPoints[iX,iY].FCausticUVCoords.V := (FCausticUV * iY) / FTerrainHeight;
        TerrainPoints[iX,iY].FVertex.X := iStartWidth + (iX * FTriangleSize);
        TerrainPoints[iX,iY].FVertex.Y := (iMapHeight)*FHeightScale;
        TerrainPoints[iX,iY].FVertex.Z := iStartHeight + (iY * FTriangleSize);
      end;
    end;
    FTerrainTop     := (FTerrainTop) * FHeightScale;
    FTerrainBottom  := (FTerrainBottom) * FHeightScale;
    FreeAndNil(iBmp1);
    FreeAndNil(iJpg1);

    iM := TGDMatrix.Create();
    iR := TGDVector.Create();
    for iX := 0 to (FTerrainWidth-1) do
    begin
      for iY := 0 to (FTerrainHeight-1) do
      begin
        if (iX = (FTerrainWidth-1)) and (iY = FTerrainHeight-1) then
        begin
           TerrainPoints[iX,iY].FNormal.Reset(TerrainPoints[iX-1,iY-1].FNormal);
           continue;
        end
        else if iX = (FTerrainWidth-1) then
        begin
           TerrainPoints[iX,iY].FNormal.Reset(TerrainPoints[iX-1,iY].FNormal);
           continue;
        end
        else if iY = FTerrainHeight-1 then
        begin
          TerrainPoints[iX,iY].FNormal.Reset(TerrainPoints[iX,iY-1].FNormal);
          continue;
        end;
        GetRotation(iStartWidth + iX*FTriangleSize, iStartHeight + iY*FTriangleSize, iR);
        iM.EmptyMatrix();
        iM.CreateRotation( iR );
        TerrainPoints[iX,iY].FNormal.reset(0,1,0);
        iM.ApplyToVector( TerrainPoints[iX,iY].FNormal );
      end;
    end;
    FreeAndNil(iM);
    FreeAndNil(iR);

    GUI.LoadingScreen.UpdateBar();

    if Not( FColorTexture.InitTexture(aInput.ColorMap ,Settings.TextureDetail,Settings.TextureFilter)) then
       Raise Exception.Create('Failed to load color texture!');
    GUI.LoadingScreen.UpdateBar();
    if Not( FDetailTexture1.InitTexture(aInput.Detail1 ,Settings.TextureDetail,Settings.TextureFilter)) then
       Raise Exception.Create('Failed to load detail textures!');
    if Not( FDetailTexture2.InitTexture(aInput.Detail2 ,Settings.TextureDetail,Settings.TextureFilter)) then
       Raise Exception.Create('Failed to load detail textures!');
    if Not( FDetailTexture3.InitTexture(aInput.Detail3 ,Settings.TextureDetail,Settings.TextureFilter)) then
       Raise Exception.Create('Failed to load detail textures!');
    if Not( FDetailLookup.InitTexture(aInput.DetailLookup ,TD_HIGH,Settings.TextureFilter)) then
       Raise Exception.Create('Failed to load detail textures!');
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
      for iY := 0 to Length(TerrainPoints)-1 do
      begin
        TerrainPoints[iX, iY].Destroy();
      end;
    end;
    for iX := 0 to Length(TerrainPoints)-1 do
    begin
        Finalize(TerrainPoints[iX]);
    end;
    Finalize(TerrainPoints);

    SetLength(TerrainPoints, 0);
    SetLength(TerrainPoints, 0);
  end;
  TerrainPoints := nil;

  FDetailUV := 1;
  FColorTexture.Clear();
  FDetailTexture1.Clear();
  FDetailTexture2.Clear();
  FDetailTexture3.Clear();
  FDetailLookup.Clear();

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
   iA := TerrainPoints[iRX   ,iRZ].FVertex.Y;
   iB := TerrainPoints[iRX+1 ,iRZ].FVertex.Y;
   iC := TerrainPoints[iRX   ,iRZ+1].FVertex.Y;
   iD := TerrainPoints[iRX+1 ,iRZ+1].FVertex.Y;
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
  iPos := TGDVector.Create(aX,0,aZ);
  aX := (aX + ((FTerrainWidth * FTriangleSize)/2)) / FTriangleSize;
  aZ := (aZ + ((FTerrainHeight * FTriangleSize)/2)) / FTriangleSize;
  if (aX < 0) or (aX > FTerrainWidth-1) or
     (aZ < 0) or (aZ > FTerrainHeight-1) or
     Not(FTerrainLoaded) then
  begin
    FreeAndNil(iPos);
    exit;
  end;

  iRX := trunc(aX);
  iRZ := trunc(aZ);
  iFX := aX - iRX;
  iFZ := aZ - iRZ;
  iA := TerrainPoints[iRX   ,iRZ].FVertex.Y;
  iB := TerrainPoints[iRX+1 ,iRZ].FVertex.Y;
  iC := TerrainPoints[iRX   ,iRZ+1].FVertex.Y;
  iD := TerrainPoints[iRX+1 ,iRZ+1].FVertex.Y;
  iPos.Y := (iA + (iB - iA) * iFX) + ((iC + (iD - iC) * iFX) - (iA + (iB - iA) * iFX)) * iFZ;

  iTriangle := TGDTriangle.Create();
  iTriangle.V1.Reset(TerrainPoints[iRX, iRZ].FVertex );
  iTriangle.V2.Reset(TerrainPoints[iRX+1, iRZ].FVertex );
  iTriangle.V3.Reset(TerrainPoints[iRX, iRZ+1].FVertex );
  iFound := true;
  iSecTris := false;;
  If Not(iTriangle.PointInTraingle( iPos )) then
  begin
    iTriangle.V1.Reset(TerrainPoints[iRX+1, iRZ+1].FVertex );
    iTriangle.V2.Reset(TerrainPoints[iRX, iRZ+1].FVertex );
    iTriangle.V3.Reset(TerrainPoints[iRX+1, iRZ].FVertex );
    iSecTris := true;
    If Not(iTriangle.PointInTraingle( iPos )) then iFound := false;
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

    iUp := TGDVector.Create();
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
  
  FreeAndNil(iTriangle);
  FreeAndNil(iPos);
  FreeAndNil(iUp);
  FreeAndNil(iForward);
  FreeAndNil(iRight);
end;

{******************************************************************************}
{* Start the rendering of a terraincell                                       *}
{******************************************************************************}

procedure TGDTerrain.StartRendering( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor );
begin
  if Not(aRenderAttribute = RA_NORMAL) then exit;

  if Modes.RenderWireframe then
  begin
    glColor4f(0.2,0.8,0.2,1);
  end
  else
  begin
    case aRenderFor of
     RF_NORMAL, RF_WATER : begin
                   Renderer.TerrainShader.Enable();
                   Renderer.TerrainShader.SetFloat3('V_LIGHT_DIR', DirectionalLight.Direction.X,
                                                                   DirectionalLight.Direction.Y,
                                                                   DirectionalLight.Direction.Z);
                   Renderer.TerrainShader.SetFloat4('V_LIGHT_AMB', DirectionalLight.Ambient.R,
                                                                   DirectionalLight.Ambient.G,
                                                                   DirectionalLight.Ambient.B,
                                                                   DirectionalLight.Ambient.A);
                   Renderer.TerrainShader.SetFloat4('V_LIGHT_DIFF', DirectionalLight.Diffuse.R,
                                                                    DirectionalLight.Diffuse.G,
                                                                    DirectionalLight.Diffuse.B,
                                                                    DirectionalLight.Diffuse.A);
                   Renderer.TerrainShader.SetFloat('F_MIN_VIEW_DISTANCE', FogManager.FogShader.MinDistance);
                   Renderer.TerrainShader.SetFloat('F_MAX_VIEW_DISTANCE', FogManager.FogShader.MaxDistance);
                   Renderer.TerrainShader.SetFloat4('V_FOG_COLOR', FogManager.FogShader.Color.R,
                                                     FogManager.FogShader.Color.G, FogManager.FogShader.Color.B,
                                                     FogManager.FogShader.Color.A);
                   Renderer.TerrainShader.SetInt('T_COLORTEX', 0);
                   Renderer.TerrainShader.SetInt('T_DETAILTEX1', 1);
                   Renderer.TerrainShader.SetInt('T_DETAILTEX2', 2);
                   Renderer.TerrainShader.SetInt('T_DETAILTEX3', 3);
                   Renderer.TerrainShader.SetInt('T_WEIGHT_LOOKUP', 4);
                   Renderer.TerrainShader.SetInt('T_CAUSTIC_TEX', 5);

                   FColorTexture.BindTexture(GL_TEXTURE0);
                   FDetailTexture1.BindTexture(GL_TEXTURE1);
                   FDetailTexture2.BindTexture(GL_TEXTURE2);
                   FDetailTexture3.BindTexture(GL_TEXTURE3);
                   FDetailLookup.BindTexture(GL_TEXTURE4);
                   Water.BindCausticTexture();

                   If Water.WaterHeight > Camera.Position.Y then
                   begin
                     Renderer.TerrainShader.SetInt('I_UNDER_WATER', 1);
                   end
                   else
                   begin
                     Renderer.TerrainShader.SetInt('I_UNDER_WATER', 0);
                   end;
                   Renderer.TerrainShader.SetFloat('I_WATER_HEIGHT', Water.WaterHeight);
                 end;
      RF_BLOOM : begin
                   Renderer.RenderState( RS_COLOR );
                   glEnable(GL_DEPTH_TEST);
                   glColor4f(0,0,0,1)
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
