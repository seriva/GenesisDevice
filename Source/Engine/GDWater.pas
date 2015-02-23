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
unit GDWater;

{$MODE Delphi}

{******************************************************************************}
{* Holds the water class                                                      *}
{******************************************************************************}

interface

uses
  Classes,
  LCLIntf,
  LCLType,
  SysUtils,
  mmSystem,
  dglOpenGL,
  GDLog,
  GDTexture,
  GDTypes,
  GDFrustum,
  GDRenderer,
  GDBoundingVolumes,
  GDGLObjects,
  GDConstants,
  GDSettings,
  GDCamera,
  GDFog,
  GDLighting,
  GDTiming,
  GDObjectList,
  GDModes;

type

{******************************************************************************}
{* Water input record                                                         *}
{******************************************************************************}

  TGDWaterInput = record
    NumberOfWaterText : integer;
    WaterPath         : String;
    WaterPrefix       : String;
    WaterExtension    : String;
    WaterDepthMap     : String;
    Height            : Double;
    X1                : Double;
    Z1                : Double;
    X2                : Double;
    Z2                : Double;
    U                 : Double;
    V                 : Double;
    WaveSpeed         : Double;
    WaveStrength      : Double;
    UnderWaterColorR,UnderWaterColorG,UnderWaterColorB,UnderWaterColorA : Double;
    WaterColorCorrectionR,WaterColorCorrectionG,WaterColorCorrectionB,WaterColorCorrectionA : Double;
    CellCountX        : Integer;
    CellCountY        : Integer;
    CellDivX          : Integer;
    CellDivY          : Integer;
    Visibility        : Integer;
    NumberOfCausticsText : integer;
    CausticsPath      : String;
    CausticsPrefix    : String;
    CausticsExtension : String;
  end;

{******************************************************************************}
{* water class                                                                *}
{******************************************************************************}

  TGDWater = Class
  private
    FBoundingBox     : TGDBoundingBox;
    FWaterU          : Double;
    FWaterV          : Double;
    FWidth           : Integer;
    FHeight          : Integer;
    FWaveSpeed       : Double;
    FWaveStrength    : Double;
    FCellCountX      : Integer;
    FCellCountY      : Integer;
    FCellDivX        : Integer;
    FCellDivY        : Integer;
    FReflection      : TGDTexture;
    FDepthMap        : TGDTexture;
    FRenderBuffer    : TGDGLRenderBufferObject;
    FFrameBuffer     : TGDGLFrameBufferObject;
    FWaterLoaded     : Boolean;
    FUnderWaterColor : TGDColor;
    FWaterColorCorrection : TGDColor;
    FCausticTextures : TGDObjectList;
    FCausticCounter  : Integer;
    FWaterTextures   : TGDObjectList;
    FWaterCounter    : Integer;
    FUpdateTimer     : Integer;

    function GetHeight() : Double;
  public
    Property BoundingBox : TGDBoundingBox read FBoundingBox;
    Property WaterHeight : Double read GetHeight;
    property Reflection : TGDTexture read FReflection;
    property CellDivX : Integer read FCellDivX;
    property CellDivY : Integer read FCellDivY;
    property CellCountX : Integer read FCellCountX;
    property CellCountY : Integer read FCellCountY;
    property WaterU : Double read FWaterU;
    property WaterV : Double read FWaterV;
    property WaveSpeed : Double read FWaveSpeed;
    property WaveStrength : Double read FWaveStrength;
    property WaterLoaded : Boolean read FWaterLoaded;
    property WaterCounter : Integer read FWaterCounter;
    property UnderWaterColor : TGDColor read FUnderWaterColor;
    property CausticTextures : TGDObjectList read FCausticTextures;
    property CausticCounter : Integer read FCausticCounter;

    constructor Create();
    destructor  Destroy(); override;
    function    InitWater( aInput : TGDWaterInput ) : boolean;
    procedure   Clear();

    procedure Resize();
    function  Visible(): boolean;
    function  UnderWater(): boolean;

    procedure RenderUnderWater();

    procedure StartReflection();
    procedure EndReflection();

    procedure StartRendering( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor );
    procedure EndRendering();

    procedure BindCausticTexture();
    procedure BindWaterTexture();

    procedure Update();
  end;

  procedure UpdateWaterCallBack(TimerID, Msg: Uint; dwUser, dw1, dw2: DWORD); pascal;

var
  Water : TGDWater;

implementation

{******************************************************************************}
{* Create the water class                                                     *}
{******************************************************************************}

constructor TGDWater.Create();
begin
  FBoundingBox     := TGDBoundingBox.Create();
  FReflection      := TGDTexture.Create();
  FDepthMap        := TGDTexture.Create();
  FRenderBuffer    := TGDGLRenderBufferObject.Create();
  FFrameBuffer     := TGDGLFrameBufferObject.Create();
  FWaterLoaded     := false;
  FCausticCounter  := 0;
  FWaterCounter    := 0;
  FUnderWaterColor := TGDColor.Create();
  FWaterColorCorrection := TGDColor.Create();
  FCausticTextures := TGDObjectList.Create();
  FWaterTextures   := TGDObjectList.Create();
  FUpdateTimer     := TimeSetEvent(50, 0, @UpdateWaterCallBack, 0, TIME_PERIODIC);
end;

{******************************************************************************}
{* Destroy the water class                                                    *}
{******************************************************************************}

destructor  TGDWater.Destroy();
begin
  FreeAndNil(FBoundingBox);
  FreeAndNil(FReflection);
  FreeAndNil(FRenderBuffer);
  FreeAndNil(FFrameBuffer);
  FreeAndNil(FUnderWaterColor);
  FreeAndNil(FWaterColorCorrection);
  FreeAndNil(FCausticTextures);
  FreeAndNil(FWaterTextures);
  FreeAndNil(FDepthMap);
  TimeKillEvent(FUpdateTimer);
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

function TGDWater.InitWater( aInput : TGDWaterInput ) : boolean;
var
  iI : Integer;
  iTexture : TGDTexture;
  iFileName : String;
  iError : string;
begin
  Log.AddNewLine('Loading water...');
  Log.Use := False;
  try
    result        := true;
    FWaterLoaded  := true;

    FCellCountX := aInput.CellCountX;
    FCellCountY := aInput.CellCountY;
    FCellDivX   := aInput.CellDivX;
    FCellDivY   := aInput.CellDivY;
    FUnderWaterColor.Reset(aInput.UnderWaterColorR,aInput.UnderWaterColorG,aInput.UnderWaterColorB,aInput.UnderWaterColorA);
    FWaterColorCorrection.Reset(aInput.WaterColorCorrectionR,aInput.WaterColorCorrectionG,aInput.WaterColorCorrectionB,aInput.WaterColorCorrectionA);
    FBoundingBox.Max.Reset(aInput.X1,aInput.Height,aInput.Z1);
    FBoundingBox.Min.Reset(aInput.X2,aInput.Height,aInput.Z2);
    FWaterU        := aInput.U;
    FWaterV        := aInput.V;
    Resize();
    FWaterTextures.Clear();
    FCausticTextures.Clear();
    FWaterCounter := 0;
    FWaveSpeed := aInput.WaveSpeed;
    FWaveStrength := aInput.WaveStrength;

    If Not( FDepthMap.InitTexture( aInput.WaterDepthMap, Settings.TextureDetail, Settings.TextureFilter) ) then
      Raise Exception.Create('Failed to load depth texture!');

    For iI := 0 to aInput.NumberOfWaterText-1 do
    begin
      iFileName := '';
      iFileName := aInput.WaterPath + aInput.WaterPrefix + IntToStr(iI) + '.' + aInput.WaterExtension;
      iTexture := TGDTexture.Create();
      If Not( iTexture.InitTexture(iFileName ,Settings.TextureDetail,Settings.TextureFilter) ) then
        Raise Exception.Create('Failed to load water textures!');
      FWaterTextures.Add( iTexture );
    end;

    if Not(FWaterLoaded) then
      Raise Exception.Create('Failed to load water textures!');

    For iI := 0 to aInput.NumberOfCausticsText-1 do
    begin
      iFileName := '';
      iFileName := aInput.CausticsPath + aInput.CausticsPrefix + IntToStr(iI) + '.' + aInput.CausticsExtension;
      iTexture := TGDTexture.Create();
      If Not( iTexture.InitTexture(iFileName ,Settings.TextureDetail,Settings.TextureFilter)) then
        Raise Exception.Create('Failed to load caustics textures!');
      FCausticTextures.Add( iTexture );
    end;

  except
    on E: Exception do
    begin
      iError := E.Message;
      result := false;
      FWaterLoaded := false;
    end;
  end;
  Log.Use := True;

  If result then
  begin
    Log.AddToLastLine('Succeeded');
  end
  else
  begin
    Log.AddToLastLine('Failed');
    Log.AddNewLine('Error Message: ' + iError);
  end;
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
  FRenderBuffer.Clear();
  FFrameBuffer.Clear();
  FReflection.Clear();
  FRenderBuffer.InitRenderBuffer(FWidth,FHeight,GL_DEPTH_COMPONENT24);
  FFrameBuffer.InitFrameBuffer();
  FReflection.RenderTextureFloat(FWidth,FHeight);
end;

{******************************************************************************}
{* Clear water                                                                *}
{******************************************************************************}

procedure TGDWater.Clear();
begin
  FBoundingBox.Min.Reset(0,0,0);
  FBoundingBox.Max.Reset(0,0,0);
  FDepthMap.Clear();
  FReflection.Clear();
  FWaterTextures.Clear();
  FWaterCounter := 0;
  FUnderWaterColor.Reset(1,1,1,1);
  FWaterColorCorrection.Reset(1,1,1,1);
  FCausticTextures.Clear();
  FCausticCounter := 0;
  FWaterLoaded := false;
end;

{******************************************************************************}
{* Check if the water if visible                                              *}
{******************************************************************************}

Function TGDWater.Visible() : boolean;
begin
  result := Frustum.BoxInFrustum(FBoundingBox);
end;

{******************************************************************************}
{* Update animations                                                          *}
{******************************************************************************}

procedure TGDWater.Update();
begin
  If FWaterLoaded then
  begin
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
  
  Case Modes.RenderMode of
    RM_NORMAL    : begin
                     case aRenderFor of
                          RF_NORMAL, RF_WATER : begin
                                     Renderer.WaterShader.Enable();
                                     Renderer.WaterShader.SetFloat3('V_LIGHT_DIR', DirectionalLight.Direction.X,
                                                                                     DirectionalLight.Direction.Y,
                                                                                     DirectionalLight.Direction.Z);
                                     Renderer.WaterShader.SetFloat4('V_LIGHT_AMB', DirectionalLight.Ambient.R,
                                                                                     DirectionalLight.Ambient.G,
                                                                                     DirectionalLight.Ambient.B,
                                                                                     DirectionalLight.Ambient.A);
                                     Renderer.WaterShader.SetFloat4('V_LIGHT_DIFF', DirectionalLight.Diffuse.R,
                                                                                      DirectionalLight.Diffuse.G,
                                                                                      DirectionalLight.Diffuse.B,
                                                                                      DirectionalLight.Diffuse.A);
                                     Renderer.WaterShader.SetFloat('F_WAVE_SPEED', Timing.ElapsedTime / FWaveSpeed);
                                     Renderer.WaterShader.SetFloat('F_WAVE_STRENGHT', FWaveStrength);
                                     Renderer.WaterShader.SetInt('T_REFLECTION', 0);
                                     Renderer.WaterShader.SetInt('T_DUDVMAP', 1);
                                     Renderer.WaterShader.SetInt('T_DEPTHMAP', 2);
                                     Renderer.WaterShader.SetFloat('F_MIN_VIEW_DISTANCE', FogManager.FogShader.MinDistance);
                                     Renderer.WaterShader.SetFloat('F_MAX_VIEW_DISTANCE', FogManager.FogShader.MaxDistance);
                                     Renderer.WaterShader.SetFloat4('V_FOG_COLOR', FogManager.FogShader.Color.R,
                                     FogManager.FogShader.Color.G, FogManager.FogShader.Color.B,FogManager.FogShader.Color.A);
                                     Renderer.WaterShader.SetFloat4('V_WATER_COLOR_CORRECTION', FWaterColorCorrection.R,
                                                                                                FWaterColorCorrection.G,
                                                                                                FWaterColorCorrection.B,
                                                                                                FWaterColorCorrection.A);
                                     If Water.WaterHeight > Camera.Position.Y then
                                       Renderer.WaterShader.SetInt('I_UNDER_WATER', 1)
                                     else
                                       Renderer.WaterShader.SetInt('I_UNDER_WATER', 0);

                                     FReflection.BindTexture(TU_1);
                                     BindWaterTexture();
                                     FDepthMap.BindTexture(TU_3);
                                     glEnable(GL_BLEND);
                                     glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
                          end;
                          RF_BLOOM : begin
                                   Renderer.RenderState( RS_COLOR );
                                   glEnable(GL_DEPTH_TEST);
                                   glColor4f(0,0,0,1)
                          end;
                     end;
                   end;
    RM_WIREFRAME : glColor4f(0.3,0.3,1,1);
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
{* Render the underwater color blend                                          *}
{******************************************************************************}

procedure TGDWater.RenderUnderWater();
begin
  If (Camera.Position.Y < FBoundingBox.Max.Y ) then
  begin
    glColor4fv(FUnderWaterColor.ArrayPointer);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
    glBegin(GL_QUADS);
      glVertex2f(0, 0);
      glVertex2f(R_HUDWIDTH, 0);
      glVertex2f(R_HUDWIDTH, R_HUDHEIGHT);
      glVertex2f(0, R_HUDHEIGHT);
    glEnd;
    glDisable(GL_BLEND);
  end;
end;

{******************************************************************************}
{* Bind the caustic texture                                                   *}
{******************************************************************************}

procedure TGDWater.BindCausticTexture();
begin
  If FWaterLoaded then
  begin
    TGDTexture(FCausticTextures.GetObjectI( FCausticCounter )).BindTexture(TU_6);
  end;
end;

{******************************************************************************}
{* Bind the water texture                                                     *}
{******************************************************************************}

procedure TGDWater.BindWaterTexture();
begin
  If FWaterLoaded then
  begin
    TGDTexture(FWaterTextures.GetObjectI(FWaterCounter)).BindTexture(TU_2);
  end;
end;

{******************************************************************************}
{* Update Water Callback                                                      *}
{******************************************************************************}

procedure UpdateWaterCallBack(TimerID, Msg: Uint; dwUser, dw1, dw2: DWORD); pascal;
begin
  Water.Update();
end;

end.
