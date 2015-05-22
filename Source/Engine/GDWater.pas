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
unit GDWater;

{$MODE Delphi}

interface

uses
  Classes,
  LCLIntf,
  LCLType,
  IniFiles,
  GDStringParsing,
  SysUtils,
  dglOpenGL,
  GDConsole,
  GDTexture,
  GDTypes,
  GDGLObjects,
  GDConstants,
  GDSettings,
  GDCamera,
  GDResource,
  GDTiming,
  GDModes;

type

{******************************************************************************}
{* water class                                                                *}
{******************************************************************************}

  TGDWater = Class
  private
    FWaterTime       : Integer;
    FLastTime        : Integer;
    FBoundingBox     : TGDBoundingBox;
    FWaterU          : Double;
    FWaterV          : Double;
    FWidth           : Integer;
    FHeight          : Integer;
    FCellCountX      : Integer;
    FCellCountY      : Integer;
    FCellDivX        : Integer;
    FCellDivY        : Integer;
    FColor           : TGDColor;
    FReflection      : TGDTexture;
    FRenderBuffer    : TGDGLRenderBufferObject;
    FFrameBuffer     : TGDGLFrameBufferObject;
    FWaterLoaded     : Boolean;
    FCausticTextures : TGDTextureList;
    FCausticCounter  : Integer;
    FWaterTextures   : TGDTextureList;
    FWaterCounter    : Integer;
    FDepth           : Double;
    FMinDistance     : Double;
    FMaxDistance     : Double;

    function GetHeight() : Double;
  public
    Property BoundingBox : TGDBoundingBox read FBoundingBox;
    Property WaterHeight : Double read GetHeight;
    property CellDivX : Integer read FCellDivX;
    property CellDivY : Integer read FCellDivY;
    property CellCountX : Integer read FCellCountX;
    property CellCountY : Integer read FCellCountY;
    property WaterU : Double read FWaterU;
    property WaterV : Double read FWaterV;
    property WaterLoaded : Boolean read FWaterLoaded;
    property Color : TGDColor read FColor;
    Property Depth : Double read FDepth;
    Property MinDistance : Double read FMinDistance;
    Property MaxDistance : Double read FMaxDistance;

    constructor Create();
    destructor  Destroy(); override;

    function    InitWater( aIniFile : TIniFile ) : boolean;
    procedure   Clear();

    procedure Resize();
    function  Visible(): boolean;
    function  UnderWater(): boolean;

    procedure StartReflection();
    procedure EndReflection();

    procedure StartRendering( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor );
    procedure EndRendering();

    procedure BindCausticTexture();
    procedure BindWaterTexture();

    procedure Update();
  end;

implementation

uses
  GDMap,
  GDRenderer,
  GDResources;

{******************************************************************************}
{* Create the water class                                                     *}
{******************************************************************************}

constructor TGDWater.Create();
begin
  FFrameBuffer     := TGDGLFrameBufferObject.Create();
  FWaterLoaded     := false;
  FCausticCounter  := 0;
  FWaterCounter    := 0;
  FCausticTextures := TGDTextureList.Create(false);
  FWaterTextures   := TGDTextureList.Create(false);
end;

{******************************************************************************}
{* Destroy the water class                                                    *}
{******************************************************************************}

destructor  TGDWater.Destroy();
begin
  Clear();
  FreeAndNil(FFrameBuffer);
  FreeAndNil(FCausticTextures);
  FreeAndNil(FWaterTextures);
  inherited;
end;

{******************************************************************************}
{* Get the water height                                                       *}
{******************************************************************************}

function TGDWater.GetHeight() : Double;
begin
  result := FBoundingBox.Max.Y;
end;

{******************************************************************************}
{* Init the water                                                             *}
{******************************************************************************}

function TGDWater.InitWater( aIniFile : TIniFile ) : boolean;
var
  iI, iCount : Integer;
  iPath, iExt : String;
  iError : string;
begin
  Console.Write('Loading water...');
  Console.Use := False;
  try
    result        := true;
    FWaterLoaded  := true;

    FCellCountX := aIniFile.ReadInteger('Water', 'CellCountX', 1 );
    FCellCountY := aIniFile.ReadInteger('Water', 'CellCountY', 1 );
    FCellDivX   := aIniFile.ReadInteger('Water', 'CellDivX', 1 );
    FCellDivY   := aIniFile.ReadInteger('Water', 'CellDivY', 1 );
    FColor := ReadColor(aIniFile, 'Water', 'Color');
    FBoundingBox.Max.Reset(aIniFile.ReadFloat( 'Water', 'X1', 0 ),
                           aIniFile.ReadFloat( 'Water', 'Height', 0 ),
                           aIniFile.ReadFloat( 'Water', 'Z1', 0 ));
    FBoundingBox.Min.Reset(aIniFile.ReadFloat( 'Water', 'X2', 0 ),
                           aIniFile.ReadFloat( 'Water', 'Height', 0 ),
                           aIniFile.ReadFloat( 'Water', 'Z2', 0 ));
    FWaterU := aIniFile.ReadFloat( 'Water', 'WaterU', 1 );
    FWaterV := aIniFile.ReadFloat( 'Water', 'WaterV', 1 );
    FDepth := aIniFile.ReadFloat( 'Water', 'Depth', 500 );;
    FMinDistance := aIniFile.ReadFloat( 'Water', 'MinDistance', 0.1 );
    FMaxDistance := aIniFile.ReadFloat( 'Water', 'MaxDistance', 0.2 );

    Resize();
    FWaterTextures.Clear();
    FCausticTextures.Clear();
    FWaterCounter := 0;

    //Water textures
    iCount := aIniFile.ReadInteger('Water', 'WaterTexturesCount', 10 );
    iPath  := aIniFile.ReadString( 'Water', 'WaterMapPath', 'textures\water\') + aIniFile.ReadString( 'Water', 'WaterMapPrefix', 'water');
    iExt   := aIniFile.ReadString( 'Water', 'WaterMapExtension', 'dds');
    for iI := 0 to iCount-1 do
      FWaterTextures.Add( Resources.LoadTexture(iPath + IntToStr(iI) + '.' + iExt ,Settings.TextureDetail,Settings.TextureFilter) );

    //Caustic textures
    iCount := aIniFile.ReadInteger('Water', 'CausticTexturesCount', 10 );
    iPath  := aIniFile.ReadString( 'Water', 'CausticsMapPath', 'textures\water\') + aIniFile.ReadString( 'Water', 'CausticsMapPrefix', 'caust');
    iExt   := aIniFile.ReadString( 'Water', 'CausticsMapExtension', 'dds');
    for iI := 0 to iCount-1 do
      FCausticTextures.Add( Resources.LoadTexture(iPath + IntToStr(iI) + '.' + iExt ,Settings.TextureDetail,Settings.TextureFilter) );
  except
    on E: Exception do
    begin
      iError := E.Message;
      result := false;
      FWaterLoaded := false;
    end;
  end;
  Console.Use := True;

  Console.WriteOkFail(result, iError);
end;

{******************************************************************************}
{* Resize the water reflection texture and buffers                            *}
{******************************************************************************}

procedure TGDWater.Resize();
begin
   case Settings.WaterDetail of
     WD_LOW    : begin
                   FWidth  := Settings.Width div 4;
                   FHeight := Settings.Height div 4;
                 end;

     WD_MEDIUM : begin
                   FWidth  := Settings.Width div 2;
                   FHeight := Settings.Height div 2;
                 end;
     WD_HIGH   : begin
                   FWidth  := Settings.Width;
                   FHeight := Settings.Height;
                 end;
  end;
  FreeAndNil(FRenderBuffer);
  FreeAndNil(FFrameBuffer);
  FreeAndNil(FReflection);
  FRenderBuffer := TGDGLRenderBufferObject.Create(FWidth,FHeight,GL_DEPTH_COMPONENT24);
  FFrameBuffer  := TGDGLFrameBufferObject.Create();
  FReflection   := TGDTexture.Create(GL_RGBA, FWidth,FHeight);
end;

{******************************************************************************}
{* Clear water                                                                *}
{******************************************************************************}

procedure TGDWater.Clear();
var
  iI : integer;
  iTex : TGDTexture;
begin
  FreeAndNil(FRenderBuffer);
  FreeAndNil(FFrameBuffer);
  FreeAndNil(FReflection);

  FBoundingBox.Min.Reset(0,0,0);
  FBoundingBox.Max.Reset(0,0,0);

  FColor.Reset(1,1,1,1);

  for iI := 0 to FCausticTextures.Count-1 do
  begin
    iTex := TGDTexture(FCausticTextures.Items[iI]);
    Resources.RemoveResource(TGDResource(iTex));
  end;
  FCausticTextures.Clear();
  FCausticCounter := 0;

  for iI := 0 to FWaterTextures.Count-1 do
  begin
    iTex := TGDTexture(FWaterTextures.Items[iI]);
    Resources.RemoveResource(TGDResource(iTex));
  end;
  FWaterTextures.Clear();
  FWaterCounter := 0;

  FWaterLoaded := false;
end;

{******************************************************************************}
{* Check if the water if visible                                              *}
{******************************************************************************}

Function TGDWater.Visible() : boolean;
begin
  result := Camera.BoxInView(FBoundingBox);
end;

{******************************************************************************}
{* Update animations                                                          *}
{******************************************************************************}

procedure TGDWater.Update();
var
  iDT, iTime : Integer;
begin
  If not(FWaterLoaded) then exit;
  iTime      := Timing.GetTime();
  iDT        := iTime - FLastTime;
  FLastTime  := iTime;
  FWaterTime := FWaterTime + iDT;
  if (FWaterTime >= 50) then
  begin
    FWaterTime := 0;

    inc(FCausticCounter);
    If FCausticCounter = FCausticTextures.Count-1 then
    FCausticCounter := 0;

    inc(FWaterCounter);
    If FWaterCounter = FWaterTextures.Count-1 then
    FWaterCounter := 0;
  end;
end;

{******************************************************************************}
{* Start the reflection rendering of the water                                *}
{******************************************************************************}

procedure TGDWater.StartReflection();
var
  FClipPlane : array [0..3] of Double;
begin
  FFrameBuffer.Bind();
  FFrameBuffer.AttachTexture(FReflection,GL_COLOR_ATTACHMENT0_EXT,GL_TEXTURE_2D);
  FFrameBuffer.AttachRenderBufferObject(FRenderBuffer,GL_DEPTH_ATTACHMENT_EXT);
  FFrameBuffer.Status();
  glViewPort(0,0,FWidth,FHeight);
  glClear(GL_DEPTH_BUFFER_BIT or GL_COLOR_BUFFER_BIT);
  If Camera.Position.Y > FBoundingBox.Max.Y then
  begin
    FClipPlane[0] := 0;
    FClipPlane[1] := -1;
    FClipPlane[2] := 0;
    FClipPlane[3] := FBoundingBox.Max.Y+50;
    glPushMatrix();
   	glClipPlane(GL_CLIP_PLANE0, @FClipPlane);
    glTranslatef(0, (FBoundingBox.Max.Y*2), 0);
    glScalef(1, -1, 1);
    glCullFace(GL_FRONT);
    glEnable(GL_CLIP_PLANE0);
    glPolygonMode(GL_BACK, GL_FILL);
  end
end;

{******************************************************************************}
{* End the reflection rendering of the water                                  *}
{******************************************************************************}

procedure TGDWater.EndReflection();
begin
  If Camera.Position.Y > FBoundingBox.Max.Y then
  begin
    glCullFace(GL_BACK);
    glPolygonMode(GL_FRONT, GL_FILL);
    glPopMatrix();
  end;
  glDisable(GL_CLIP_PLANE0);
  glViewPort(0,0,Settings.Width, Settings.Height);
  FFrameBuffer.UnBind();
end;

{******************************************************************************}
{* Start the water rendering                                                  *}
{******************************************************************************}

procedure TGDWater.StartRendering( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor );
begin
  if Not(aRenderAttribute = RA_NORMAL) then exit;
  
  if Modes.RenderWireframe then
  begin
    Renderer.SetColor(0.3,0.3,1,1);
  end
  else
  begin
    case aRenderFor of
        RF_NORMAL, RF_WATER : begin
                   Renderer.WaterShader.Enable();
                   Renderer.SetJoinedParams(Renderer.WaterShader);
                   Renderer.WaterShader.SetInt('T_REFLECTION', 0);
                   Renderer.WaterShader.SetInt('T_DUDVMAP', 1);
                   FReflection.BindTexture(GL_TEXTURE0);
                   BindWaterTexture();
                   glEnable(GL_BLEND);
                   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        end;
        RF_BLOOM : begin
                 Renderer.RenderState( RS_COLOR );
                 glEnable(GL_DEPTH_TEST);
                 Renderer.SetColor(0,0,0,1)
        end;
    end;
  end;
  glDisable(GL_CULL_FACE);
end;

{******************************************************************************}
{* End the water rendering                                                    *}
{******************************************************************************}

procedure TGDWater.EndRendering();
begin
  glEnable(GL_CULL_FACE);
  glDisable(GL_BLEND);
end;

{******************************************************************************}
{* Check if camera is underwater                                              *}
{******************************************************************************}

function TGDWater.UnderWater(): boolean;
begin
  If ( Camera.Position.Y < FBoundingBox.Max.Y ) then
    result := true
  else
    result := false;
end;

{******************************************************************************}
{* Bind the caustic texture                                                   *}
{******************************************************************************}

procedure TGDWater.BindCausticTexture();
begin
  If FWaterLoaded then
  begin
    TGDTexture(FCausticTextures.Items[ FCausticCounter ]).BindTexture(GL_TEXTURE5);
  end;
end;

{******************************************************************************}
{* Bind the water texture                                                     *}
{******************************************************************************}

procedure TGDWater.BindWaterTexture();
begin
  If FWaterLoaded then
  begin
    TGDTexture(FWaterTextures.Items[FWaterCounter]).BindTexture(GL_TEXTURE1);
  end;
end;

end.
