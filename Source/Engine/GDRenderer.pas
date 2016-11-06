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
unit GDRenderer;

{$MODE Delphi}

{******************************************************************************}
{* Hold the main renderer class. The render class is responsible for          *}
{* managing the opengl window, opengl states and GLSL shaders.                *}
{******************************************************************************}

interface

uses
  SysUtils,
  dglOpenGL,
  GDConstants,
  GDTexture,
  GDGLWrappers,
  GDTypesGenerics,
  GDTypes;

type

{******************************************************************************}
{* Renderer class                                                             *}
{******************************************************************************}

  TGDRenderer  = Class
  private
    FState          : TGDRenderState;
    FInitialized    : boolean;
    
    FTerrainShader  : TGDGLShader;
    FSkyShader      : TGDGLShader;
    FWaterShader    : TGDGLShader;
    FGrassShader    : TGDGLShader;
    FBlurShader     : TGDGLShader;
    FMeshShader     : TGDGLShader;
    FSunShader      : TGDGLShader;
    FPostShader     : TGDGLShader;
    FColorShader    : TGDGLShader;
    FTextureShader  : TGDGLShader;
    FClearShader    : TGDGLShader;

    FFrameFBO       : TGDGLFrameBuffer;
    FFrameTex       : TGDTexture;
    FFrameShadowTex : TGDTexture;
    FFrameDepthTex  : TGDTexture;

    FBlurFBO        : TGDGLFrameBuffer;
    FBlurTex        : TGDTexture;

    FShadowFBO      : TGDGLFrameBuffer;
    FShadowTex      : TGDTexture;

    FSSAOStrength   : Single;
    FSSAOSamples    : Integer;
    FSSAORadius     : Single;
    FSSAOOnly       : Integer;
    FShadowFilter   : Integer;

    FLinesVertices     : TGDVertex_V_List;
    FLinesVertexBuffer : TGDGLVertexBuffer;
    FQuadVertexBuffer  : TGDGLVertexBuffer;

    procedure InitShaders();
    procedure ClearShaders();
    procedure InitFrameBuffers();
    procedure ClearFrameBuffers();
    procedure InitShadowFrameBuffers();
    procedure ClearShadowFrameBuffers();
    Procedure ResizeFrameBuffers();
  public
    property    Initialized : boolean read FInitialized;

    property    TerrainShader  : TGDGLShader read FTerrainShader;
    property    SkyShader      : TGDGLShader read FSkyShader;
    property    WaterShader    : TGDGLShader read FWaterShader;
    property    GrassShader    : TGDGLShader read FGrassShader;
    property    MeshShader     : TGDGLShader read FMeshShader;
    property    ColorShader    : TGDGLShader read FColorShader;
    property    SunShader      : TGDGLShader read FSunShader;

    Constructor Create();
    Destructor  Destroy();override;

    procedure   InitViewPort();
    procedure   ClearViewPort;
    procedure   ResizeViewPort(aWidth, aHeight : integer);

    procedure   SetColor(aC : TGDColor); overload;
    procedure   SetColor(aR, aG, aB, aA : Single); overload;
    procedure   SetJoinedParams(aShader : TGDGLShader; aForShadows : boolean = false);
    procedure   RenderState( aState : TGDRenderState );

    procedure   AddLine(aV1, aV2 : TGDVector);

    procedure   ClearFrame();
    procedure   SwitchToOrtho();
    procedure   SwitchToPerspective();

    procedure   Render();
  end;

implementation

uses
  GDConsole,
  GDEngine;

{******************************************************************************}
{* Create the renderer class                                                  *}
{******************************************************************************}

constructor TGDRenderer.Create();
var
  iError      : string;
  iStr, iV    : String;
  iGLInt1, iGLInt2 : GLInt;
  iGLFLoat    : GLFLoat;
  iI : Integer;
  iQ  : TGDVertex_V_UV;
  iQL : TGDVertex_V_UV_List;

begin
  Inherited;
  Engine.Timing.Start();
  Engine.Console.Write('.....Initializing renderer');
  try
    FInitialized := true;

    //Read OpenGL properties and implementation
    InitOpenGL;
    ReadExtensions;

    //Print specs
    Engine.Console.Write('  Vendor: ' + String(AnsiString(glGetString(GL_VENDOR))));
    Engine.Console.Write('  Renderer: ' + String(AnsiString(glGetString(GL_RENDERER))));
    Engine.Console.Write('  Version: ' + String(AnsiString(glGetString(GL_VERSION))));
    Engine.Console.Write('  GLSL Version: ' + String(AnsiString(glGetString(GL_SHADING_LANGUAGE_VERSION))));

    //Check requirements
    //Version
    glGetIntegerv(GL_MAJOR_VERSION, @iGLInt1);
    glGetIntegerv(GL_MINOR_VERSION, @iGLInt2);
    iV := IntToStr(MRS_OPENGL_MAJOR_VERSION) + '.' + IntToStr(MRS_OPENGL_MINOR_VERSION);
    if iGLInt1 < MRS_OPENGL_MAJOR_VERSION then
      Raise Exception.Create('To low OpenGL version! Minimal version ' + iV + ' needed.');
    if iGLInt2 < MRS_OPENGL_MINOR_VERSION then
      Raise Exception.Create('To low OpenGL version! Minimal version ' + iV + ' needed.');

    //Texture units
    glGetIntegerv(GL_MAX_TEXTURE_IMAGE_UNITS, @iGLInt1);
    Engine.Console.Write('  Texture units: ' + IntToStr(iGLInt1));
    if iGLInt1 < MRS_TEXTURE_UNITS then
      Raise Exception.Create('Not ennough texture units! Minimal of ' + IntToStr(MRS_TEXTURE_UNITS) + ' needed.');

    //Anisotropic filtering
    glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, @iGLFLoat);
    Engine.Console.Write('  Anisotropic filtering: ' + FormatFloat('#####', iGLFLoat));
    if iGLFLoat < MRS_ANISOTROPIC_FILTERING then
      Raise Exception.Create('To low anisotropic filtering! Minimal of ' + IntToStr(MRS_ANISOTROPIC_FILTERING) + ' needed.');

    //Texture size
    glGetFloatv(GL_MAX_TEXTURE_SIZE, @iGLFLoat);
    Engine.Console.Write('  Texture size: ' + FormatFloat('#####',iGLFLoat));
    if iGLFLoat < MRS_TEXTURE_SIZE then
      Raise Exception.Create('To low texture size! Minimal of ' + IntToStr(MRS_TEXTURE_SIZE) + ' needed.');

    //Shading language extension
    iStr := glGetString(GL_EXTENSIONS);
    If ((Pos('GL_ARB_shader_objects', iStr) <= 0) or
       (Pos('GL_ARB_fragment_program', iStr) <= 0) or
       (Pos('GL_ARB_fragment_shader', iStr) <= 0) or
       (Pos('GL_ARB_vertex_program', iStr) <= 0) or
       (Pos('GL_ARB_vertex_shader', iStr) <= 0)) then
      Raise Exception.Create('Opengl Shading Language not supported!');

    //Framebuffer extension
    if Pos('GL_EXT_framebuffer_object', iStr) <= 0 then
      Raise Exception.Create('Frame Buffer Objects not supported!');

    //Set basic OpenGL settings.
    glClearColor(0.5, 0.5, 0.5, 1.0);
    glClearDepth(1.0);
    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LESS);
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);
    for iI := 0 to 7 do
    begin
      glActiveTexture(GL_TEXTURE0+iI );
      glEnable(GL_TEXTURE_2D);
    end;

    //line rendering
    FLinesVertices     := TGDVertex_V_List.Create();
    FLinesVertexBuffer := TGDGLVertexBuffer.Create();

    //Fullscreen quad.
    iQL := TGDVertex_V_UV_List.Create();
    iQ.Vertex.Reset(-1, -1, 0); iQ.UV.Reset(0, 0); iQL.Add(iQ);
    iQ.Vertex.Reset(1, -1, 0); iQ.UV.Reset(1, 0); iQL.Add(iQ);
    iQ.Vertex.Reset(1, 1, 0); iQ.UV.Reset(1, 1); iQL.Add(iQ);
    iQ.Vertex.Reset(-1, 1, 0); iQ.UV.Reset(0, 1); iQL.Add(iQ);
    FQuadVertexBuffer := TGDGLVertexBuffer.Create();
    FQuadVertexBuffer.Bind(VL_NONE);
    FQuadVertexBuffer.Update(iQL, GL_STATIC_DRAW);
    FQuadVertexBuffer.Unbind();
    FreeAndNil(iQL);

    //default values
    FSSAOStrength := 0.65;
    FSSAOSamples  := 16;
    FSSAORadius   := 2.25;
    FSSAOOnly     := 0;
    FShadowFilter := 1;

    //commands
    Engine.Console.AddCommand('RSSAOStrength', '0.0 to 1.0 : Set SSAO strength', CT_FLOAT, @FSSAOStrength);
    Engine.Console.AddCommand('RSSAOSamples', '8, 16, 32, 64 : Set SSAO sample count', CT_INTEGER, @FSSAOSamples);
    Engine.Console.AddCommand('RSSAORadius', '0.0 to 10.0 : Set SSAO radius', CT_FLOAT, @FSSAORadius);
    Engine.Console.AddCommand('RSSAOOnly', '0.0 to 1.0 : Only show SSAO', CT_INTEGER, @FSSAOOnly);
    Engine.Console.AddCommand('RShadowFilter', '0.0 to 1.0 : For shadow filtering', CT_INTEGER, @FShadowFilter);
  except
    on E: Exception do
    begin
      iError := E.Message;
      FInitialized := false;
      Engine.Console.Write('Failed to initialize renderer: ' + iError);
    end;
  end;

  If FInitialized then
  begin
    Engine.Timing.Stop();
    Engine.Console.Write('.....Done initializing renderer (' + Engine.Timing.TimeInSeconds + ' Sec)');
    InitShaders();
  end;
end;

{******************************************************************************}
{* Destroy the renderer class                                                 *}
{******************************************************************************}

Destructor TGDRenderer.Destroy();
var
  iError  : string;
  iResult : boolean;
begin
  inherited;
  Engine.Console.Write('Shutting down renderer...');
  try
    //Clear shaders.
    ClearShaders();

    //For normal rendering.
    FreeAndNil(FLinesVertices);
    FreeAndNil(FLinesVertexBuffer);
    FreeAndNil(FQuadVertexBuffer);
  except
    on E: Exception do
    begin
      iError := E.Message;
      iResult := false;
    end;
  end;
  Engine.Console.WriteOkFail(iResult, iError);
end;

{******************************************************************************}
{* Init the viewport                                                          *}
{******************************************************************************}

procedure TGDRenderer.InitViewPort();
begin
  InitFrameBuffers();
  InitShadowFrameBuffers();
end;

{******************************************************************************}
{* Shutdown the renderer                                                      *}
{******************************************************************************}

procedure TGDRenderer.ClearViewPort();
begin
  ClearFrameBuffers();
  ClearShadowFrameBuffers();
end;

{******************************************************************************}
{* Resize the windows viewport                                                *}
{******************************************************************************}

procedure TGDRenderer.ResizeViewPort(aWidth, aHeight : integer);
begin
  Engine.Settings.Width := aWidth;
  Engine.Settings.Height := aHeight;
  if (Engine.Settings.Height = 0) then
    Engine.Settings.Height := 1;
  glViewport(0, 0, Engine.Settings.Width, Engine.Settings.Height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(40.0, Engine.Settings.Width/Engine.Settings.Height, 25, Engine.Settings.ViewDistance * R_VIEW_DISTANCE_STEP);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  ResizeFrameBuffers();
end;

{******************************************************************************}
{* Set color                                                                  *}
{******************************************************************************}

procedure TGDRenderer.SetColor(aC : TGDColor);
begin
  FTextureShader.SetFloat4('V_COLOR', aC.R, aC.G, aC.B, aC.A);
  FColorShader.SetFloat4('V_COLOR',  aC.R, aC.G, aC.B, aC.A);
  FColorShader.SetInt('I_CUSTOM_TRANSLATE', 0);
end;

procedure TGDRenderer.SetColor(aR, aG, aB, aA : Single);
begin
  FTextureShader.SetFloat4('V_COLOR', aR, aG, aB, aA);
  FColorShader.SetFloat4('V_COLOR', aR, aG, aB, aA);
  FColorShader.SetInt('I_CUSTOM_TRANSLATE', 0);
end;

{******************************************************************************}
{* Set joined paramters of shaders.                                           *}
{******************************************************************************}

procedure TGDRenderer.SetJoinedParams(aShader : TGDGLShader; aForShadows : boolean = false);
begin
  //Ligthing
  aShader.Bind();
  aShader.SetFloat3('V_LIGHT_DIR',  Engine.Map.LightDirection.X,
                                    Engine.Map.LightDirection.Y,
                                    Engine.Map.LightDirection.Z);
  aShader.SetFloat4('V_LIGHT_AMB',  Engine.Map.LightAmbient.R,
                                    Engine.Map.LightAmbient.G,
                                    Engine.Map.LightAmbient.B,
                                    Engine.Map.LightAmbient.A);
  aShader.SetFloat4('V_LIGHT_DIFF', Engine.Map.LightDiffuse.R,
                                    Engine.Map.LightDiffuse.G,
                                    Engine.Map.LightDiffuse.B,
                                    Engine.Map.LightDiffuse.A);

  //Fog
  aShader.SetFloat('F_MIN_VIEW_DISTANCE', Engine.Map.FogMinDistance);
  aShader.SetFloat('F_MAX_VIEW_DISTANCE', Engine.Map.FogMaxDistance);
  aShader.SetFloat4('V_FOG_COLOR', Engine.Map.FogColor.R, Engine.Map.FogColor.G, Engine.Map.FogColor.B, Engine.Map.FogColor.A);

  //Water
  If Engine.Map.Water.UnderWater() then
    aShader.SetInt('I_UNDER_WATER', 1)
  else
    aShader.SetInt('I_UNDER_WATER', 0);
  aShader.SetFloat('I_WATER_HEIGHT', Engine.Map.Water.WaterHeight);
  aShader.SetFloat4('V_WATER_COLOR', Engine.Map.Water.Color.R,
                                     Engine.Map.Water.Color.G,
                                     Engine.Map.Water.Color.B,
                                     Engine.Map.Water.Color.A);
  aShader.SetFloat('I_WATER_DEPTH', Engine.Map.Water.Depth);
  aShader.SetFloat('I_WATER_MAX', Engine.Map.Water.MaxDistance);
  aShader.SetFloat('I_WATER_MIN', Engine.Map.Water.MinDistance);
  aShader.SetFloat3('V_CAM_POS', Engine.Camera.Position.x,  Engine.Camera.Position.Y,  Engine.Camera.Position.Z );
  aShader.SetInt('T_CAUSTICMAP', 6);
  Engine.Map.Water.BindCausticTexture();

  //Detail
  If Engine.Settings.UseDetail then
  begin
    aShader.SetInt('I_DO_DETAIL', 1);
    aShader.SetInt('T_DETAILMAP', 7);
  end
  else
    aShader.SetInt('I_DO_DETAIL', 0);

  //Shadows
  if not(aForShadows) then
  begin
    aShader.SetInt('T_SHADOWMAP', 8);
    FShadowTex.BindTexture(GL_TEXTURE8);
  end
  else
  begin
    glActiveTexture(GL_TEXTURE8);
    glBindTexture(GL_TEXTURE_2D, 0);
  end;
end;

{******************************************************************************}
{* Set the renderstate                                                        *}
{******************************************************************************}

procedure  TGDRenderer.RenderState( aState : TGDRenderState );
begin
  FState := aState;
  glPolygonMode(GL_FRONT, GL_FILL);
  glDisable(GL_BLEND);

  Case FState Of
    RS_COLOR   :   begin
                     FColorShader.Bind();
                   end;
    RS_WIREFRAME : begin
                     glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
                     glClearColor(0.3, 0.3, 0.3, 1.0);
                     FColorShader.Bind();
                   end;
    RS_TEXTS   :   begin
                     glEnable(GL_DEPTH_TEST);
                     glDepthFunc(GL_LEQUAL);
                     glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
                     glEnable(GL_BLEND);
                     FTextureShader.Bind();
                     FTextureShader.SetInt('T_COLORMAP', 0);
                   end;
    RS_TEXTURE   : begin
                     FTextureShader.Bind();
                     FTextureShader.SetInt('T_COLORMAP', 0);
                   end;
    end;
end;

{******************************************************************************}
{* Add line to render                                                         *}
{******************************************************************************}

procedure TGDRenderer.AddLine(aV1, aV2 : TGDVector);
begin
  FLinesVertices.Add( aV1.Copy() );
  FLinesVertices.Add( aV2.Copy() );
end;

{******************************************************************************}
{* Init the shaders                                                           *}
{******************************************************************************}

procedure TGDRenderer.InitShaders();
begin
  Engine.Timing.Start();
  Engine.Console.Write('.....Initializing shaders');
  FTerrainShader  := TGDGLShader.Create(SHADER_TERRAIN);
  FSkyShader      := TGDGLShader.Create(SHADER_SKY);
  FWaterShader    := TGDGLShader.Create(SHADER_WATER);
  FGrassShader    := TGDGLShader.Create(SHADER_GRASS);
  FBlurShader     := TGDGLShader.Create(SHADER_BLUR);
  FMeshShader     := TGDGLShader.Create(SHADER_MESH);
  FSunShader      := TGDGLShader.Create(SHADER_SUN);
  FPostShader     := TGDGLShader.Create(SHADER_POST);
  FColorShader    := TGDGLShader.Create(SHADER_COLOR);
  FTextureShader  := TGDGLShader.Create(SHADER_TEXTURE);
  FClearShader    := TGDGLShader.Create(SHADER_CLEAR);
  Engine.Timing.Stop();
  Engine.Console.Write('.....Done initializing shaders (' + Engine.Timing.TimeInSeconds + ' Sec)');
end;

{******************************************************************************}
{* Clear the shaders                                                          *}
{******************************************************************************}

procedure TGDRenderer.ClearShaders();
begin
  FreeAndNil(FTerrainShader);
  FreeAndNil(FSkyShader);
  FreeAndNil(FWaterShader);
  FreeAndNil(FGrassShader);
  FreeAndNil(FBlurShader);
  FreeAndNil(FMeshShader);
  FreeAndNil(FSunShader);
  FreeAndNil(FPostShader);
  FreeAndNil(FColorShader);
  FreeAndNil(FTextureShader);
  FreeAndNil(FClearShader);
end;

{******************************************************************************}
{* Init the framebuffers                                                      *}
{******************************************************************************}

procedure TGDRenderer.InitFrameBuffers();
var
  iBuffers : array[0..1] of GLEnum;
begin
  //Frame
  FFrameFBO := TGDGLFrameBuffer.Create();
  FFrameTex := TGDTexture.Create(GL_RGBA, GL_RGBA, Engine.Settings.Width, Engine.Settings.Height );
  FFrameShadowTex := TGDTexture.Create(GL_RGBA, GL_RGBA, Engine.Settings.Width, Engine.Settings.Height );
  FFrameDepthTex := TGDTexture.Create(GL_DEPTH_COMPONENT, GL_DEPTH_COMPONENT, Engine.Settings.Width, Engine.Settings.Height );
  FFrameFBO.Bind();
  FFrameFBO.AttachTexture(FFrameTex,GL_COLOR_ATTACHMENT0_EXT,GL_TEXTURE_2D);
  FFrameFBO.AttachTexture(FFrameShadowTex,GL_COLOR_ATTACHMENT1_EXT,GL_TEXTURE_2D);
  FFrameFBO.AttachTexture(FFrameDepthTex, GL_DEPTH_ATTACHMENT_EXT, GL_TEXTURE_2D );
  iBuffers[0] := GL_COLOR_ATTACHMENT0_EXT;
  iBuffers[1] := GL_COLOR_ATTACHMENT1_EXT;
	glDrawBuffers(2, @iBuffers);
	glReadBuffer(GL_NONE);
  FFrameFBO.Status();
  FFrameFBO.Unbind();

  //Bluring
  FBlurFBO := TGDGLFrameBuffer.Create();
  FBlurTex := TGDTexture.Create(GL_RGBA, GL_RGBA, Engine.Settings.Width div 4, Engine.Settings.Height div 4);
end;

procedure TGDRenderer.InitShadowFrameBuffers();
begin
  FShadowFBO := TGDGLFrameBuffer.Create();
  FShadowFBO.Bind();
  FShadowTex := TGDTexture.Create(GL_DEPTH_COMPONENT, GL_DEPTH_COMPONENT, R_SHADOW_SIZE, R_SHADOW_SIZE );
  glDrawBuffer(GL_NONE);
  glReadBuffer(GL_NONE);
  FShadowFBO.AttachTexture(FShadowTex, GL_DEPTH_ATTACHMENT_EXT, GL_TEXTURE_2D );
  FShadowFBO.Status();
  FShadowFBO.Unbind();
end;

{******************************************************************************}
{* Clear the framebuffers                                                     *}
{******************************************************************************}

procedure TGDRenderer.ClearFrameBuffers();
begin
  FreeAndNil(FFrameFBO);
  FreeAndNil(FFrameTex);
  FreeAndNil(FFrameShadowTex);
  FreeAndNil(FFrameDepthTex);

  FreeAndNil(FBlurFBO);
  FreeAndNil(FBlurTex);
end;

procedure TGDRenderer.ClearShadowFrameBuffers();
begin
  FreeAndNil(FShadowFBO);
  FreeAndNil(FShadowTex);
end;

{******************************************************************************}
{* Resize buffers                                                             *}
{******************************************************************************}

Procedure TGDRenderer.ResizeFrameBuffers();
begin
  ClearFrameBuffers();
  InitFrameBuffers();
end;

{******************************************************************************}
{* Clear frame                                                                *}
{******************************************************************************}

procedure TGDRenderer.ClearFrame();
begin
  glClear(GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;
end;

{******************************************************************************}
{* Switch to ortho view                                                       *}
{******************************************************************************}

procedure TGDRenderer.SwitchToOrtho();
begin
  glMatrixMode(GL_PROJECTION);
  glPushMatrix();
  glLoadIdentity();
  glOrtho(0, R_HUDWIDTH, 0, R_HUDHEIGHT, -1, 1);
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadIdentity();
end;

{******************************************************************************}
{* Switch to perspective view                                                 *}
{******************************************************************************}

procedure TGDRenderer.SwitchToPerspective();
begin
  glMatrixMode(GL_PROJECTION);
  glPopMatrix();
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix();
end;

procedure TGDRenderer.Render();
var
  iC : TGDColor;

{******************************************************************************}
{* Render static geometry                                                     *}
{******************************************************************************}

Procedure RenderStaticGeometry(aRenderFor : TGDRenderFor = RF_NORMAL);
begin
  If Engine.Modes.RenderSky then Engine.Map.SkyDome.Render();
  Engine.Map.RenderVisibleCells( RA_NORMAL, aRenderFor );
end;

{******************************************************************************}
{* Render the screen quad for post processing                                 *}
{******************************************************************************}

procedure RenderQuad();
begin
  FQuadVertexBuffer.Bind(VL_V_UV);
  FQuadVertexBuffer.Render(GL_QUADS);
  FQuadVertexBuffer.Unbind()
end;

{******************************************************************************}
{* Apply blur to a source image                                               *}
{******************************************************************************}

procedure ApplyBlurToImage( aSourceImage : TGDTexture; aBlurStrength : double );
begin
  glViewport(0, 0, Engine.Settings.Width div 4, Engine.Settings.Height div 4);
  glDisable(GL_DEPTH_TEST);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  FBlurShader.Bind();
  FBlurShader.SetInt( 'T_BLUR_IMAGE', 0 );

  //horizontal
  FBlurFBO.Bind();
  FBlurFBO.AttachTexture(FBlurTex,GL_COLOR_ATTACHMENT0_EXT,GL_TEXTURE_2D);
  FBlurFBO.Status();
  FBlurShader.SetFloat4('V_BLUR_OFFSET',aBlurStrength / Engine.Settings.Width, 0, 0, 1);
  aSourceImage.BindTexture( GL_TEXTURE0 );
  RenderQuad();

  //vertical
  glViewport(0, 0, Engine.Settings.Width, Engine.Settings.Height);
  FBlurFBO.AttachTexture(aSourceImage,GL_COLOR_ATTACHMENT0_EXT,GL_TEXTURE_2D);
  FBlurFBO.Status();
  FBlurShader.SetFloat4('V_BLUR_OFFSET', 0, aBlurStrength / Engine.Settings.Height, 0, 1);
  FBlurTex.BindTexture( GL_TEXTURE0 );
  RenderQuad();

  FBlurFBO.Unbind();
  FBlurShader.UnBind();
  glEnable(GL_DEPTH_TEST);
end;

{******************************************************************************}
{* Render debug                                                               *}
{******************************************************************************}

Procedure RenderDebug();

procedure RenderLines(aR, aG, aB, aA : single; aRA : TGDRenderAttribute);
begin
  FLinesVertices.Clear();
  Engine.Map.RenderVisibleCells(aRA, RF_NORMAL);
  RenderState( RS_COLOR );
  SetColor(aR, aG, aB, aA);
  FLinesVertexBuffer.Bind(VL_V);
  FLinesVertexBuffer.Update(FLinesVertices, GL_DYNAMIC_DRAW);
  FLinesVertexBuffer.Render(GL_LINES);
  FLinesVertexBuffer.Unbind();
end;

begin
  glLoadIdentity();
  Engine.Camera.Translate();
  If Engine.Modes.RenderNormals then RenderLines(1,0.5,0.25,1,RA_NORMALS);
  If Engine.Modes.RenderObjectBoxes then RenderLines(1,0,0,1, RA_FRUSTUM_BOXES);
  If Engine.Modes.RenderNodeBoxes then RenderLines(1,1,0,1, RA_NODE_BOXES);
end;

{******************************************************************************}
{* Render water reflections                                                   *}
{******************************************************************************}

procedure RenderWaterReflection();
begin
  If (Engine.Modes.RenderWireframe = false) and Engine.Map.Water.Visible() then
  begin
    //render reflection texture
    ClearFrame();
    iC := Engine.Map.FogColor.Copy();
    glClearColor(iC.R, iC.G, iC.B, iC.A);
    glClear(GL_COLOR_BUFFER_BIT);
    Engine.Camera.Translate();
    Engine.Map.Water.StartReflection();
    Engine.Camera.CalculateFrustum();
    Engine.Map.DetectVisibleCells();
    RenderStaticGeometry(RF_WATER);
    Engine.Map.Water.EndReflection();
  end;
end;

{******************************************************************************}
{* Render water reflections                                                   *}
{******************************************************************************}

procedure RenderShadowMap();
var
  iV : TGDVector;
  iM : TGDMatrix;
  iProj, iModl : array[0..15] of glDouble;
  iBias : array[0..15] of glDouble = (0.5, 0.0, 0.0, 0.0,
		                      0.0, 0.5, 0.0, 0.0,
		                      0.0, 0.0, 0.5, 0.0,
	                              0.5, 0.5, 0.5, 1.0);
begin
  If Engine.Modes.RenderWireframe = false then
  begin
    //render shadow texture
    FShadowFBO.Bind();
    glViewport(0, 0, R_SHADOW_SIZE, R_SHADOW_SIZE);
    glClear(GL_DEPTH_BUFFER_BIT);
    glLoadIdentity;

    If Engine.Settings.UseShadows = false then
    begin
      FShadowFBO.Unbind();
      glViewport(0, 0, Engine.Settings.Width, Engine.Settings.Height);
      exit;
    end;

    iM.CreateRotation( Engine.Camera.Rotation );
    iV.Reset(-1,0,0);
    iM.ApplyToVector(iV);
    iV.y := 0;
    iV.Normalize();

    glMatrixMode(GL_PROJECTION);
    glPushMatrix();
    glLoadIdentity();
    glOrtho(-10000, 10000, -10000, 10000, -1000000, 1000000);
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();
    glLoadIdentity();
    gluLookAt(Engine.Camera.Position.x+(iV.x*9800), Engine.Camera.Position.y+7500, Engine.Camera.Position.z+(iV.z*9800),
              Engine.Camera.Position.x+0.01+(iV.x*9800), Engine.Camera.Position.y+7500-1, Engine.Camera.Position.z+0.01+(iV.z*9800),
              iV.x,0,iV.z);
    Engine.Camera.CalculateFrustum();

    glColorMask(FALSE, FALSE, FALSE, FALSE);
    Engine.Map.DetectVisibleCells();
    Engine.Map.RenderVisibleCells( RA_NORMAL, RF_SHADOW );
    glColorMask(TRUE, TRUE, TRUE, TRUE);

    //Setup the light projection matrix.
    glGetDoublev(GL_MODELVIEW_MATRIX, @iModl);
    glGetDoublev(GL_PROJECTION_MATRIX, @iProj);
    glMatrixMode(GL_TEXTURE);
    glActiveTextureARB(GL_TEXTURE7);
    glLoadIdentity();
    glLoadMatrixd(@iBias);
    glMultMatrixd(@iProj);
    glMultMatrixd(@iModl);
    glMatrixMode(GL_MODELVIEW);

    glMatrixMode(GL_PROJECTION);
    glPopMatrix();
    glMatrixMode(GL_MODELVIEW);
    glPopMatrix();

    FShadowFBO.Unbind();
    glViewport(0, 0, Engine.Settings.Width, Engine.Settings.Height);
  end;
end;

{******************************************************************************}
{* Render GUI                                                               *}
{******************************************************************************}

procedure RenderGUI();
begin
  SwitchToOrtho();
    Engine.GUI.RenderScreens();
    If Engine.Modes.RenderStats then Engine.Statistics.Render();
    Engine.Console.Render();
    Engine.GUI.MouseCursor.Render();
  SwitchToPerspective();
end;

{******************************************************************************}
{* Render source image                                                        *}
{******************************************************************************}

procedure RenderSourceImage(aUnderWater : boolean);

begin
  FFrameFBO.Bind();
  glClear(GL_DEPTH_BUFFER_BIT);

  RenderStaticGeometry();

  FFrameFBO.UnBind();

  if aUnderWater then
    ApplyBlurToImage( FFrameTex, 3 );

  If Engine.Settings.UseShadows and (FShadowFilter = 1) then
    ApplyBlurToImage( FFrameShadowTex, 5);
end;

{******************************************************************************}
{* Clear buffers                                                              *}
{******************************************************************************}

procedure ClearBuffers();
begin
  glDisable(GL_DEPTH_TEST);
  FFrameFBO.Bind();
  glLoadIdentity();
  FClearShader.Bind();
  iC := Engine.Map.FogColor.Copy();
  FClearShader.SetFloat4('V_COLOR', iC.R, iC.G, iC.B, iC.A);
  RenderQuad();
  FClearShader.Unbind();
  FFrameFBO.Unbind();
  glEnable(GL_DEPTH_TEST);
end;

{******************************************************************************}
{* Render the final source image                                              *}
{******************************************************************************}

procedure RenderFinal();
begin
  glDisable(GL_DEPTH_TEST);
  FPostShader.Bind();
  FPostShader.SetInt('T_SOURCE_IMAGE',0);
  FPostShader.SetInt('T_SHADOW_IMAGE',1);

  If Engine.Settings.UseFXAA and not(Engine.Modes.RenderObjectBoxes or Engine.Modes.RenderNormals or Engine.Modes.RenderNodeBoxes) then
    FPostShader.SetInt('I_DO_FXAA',1)
  else
    FPostShader.SetInt('I_DO_FXAA',0);

  if Engine.Settings.UseSSAO and not(Engine.Map.Water.UnderWater()) then
  begin
    FPostShader.SetInt('I_DO_SSAO',1);
    FPostShader.SetInt('T_DEPTH_IMAGE',2);
    FPostShader.SetFloat('I_SSAO_NEAR',25);
    FPostShader.SetFloat('I_SSAO_FAR', Engine.Settings.ViewDistance * R_VIEW_DISTANCE_STEP);
    FPostShader.SetFloat('I_SSAO_STRENGTH',FSSAOStrength);
    FPostShader.SetInt('I_SSAO_SAMPLES',FSSAOSamples);
    FPostShader.SetFloat('I_SSAO_RADIUS',FSSAORadius);
    FPostShader.SetInt('I_SSAO_ONLY',FSSAOOnly);
    FFrameDepthTex.BindTexture( GL_TEXTURE2 );
  end
  else
    FPostShader.SetInt('I_DO_SSAO',0);

  FPostShader.SetFloat2('V_SCREEN_SIZE',Engine.Settings.Width, Engine.Settings.Height);
  FPostShader.SetFloat('I_GAMMA',Engine.Settings.Gamma);
  FFrameTex.BindTexture( GL_TEXTURE0 );
  FFrameShadowTex.BindTexture( GL_TEXTURE1 );

  RenderQuad();

  FPostShader.UnBind();
  glEnable(GL_DEPTH_TEST);
end;

begin
  //create the shadow texture
  RenderShadowMap();

  //create the water reflection texture
  RenderWaterReflection();

  //clear buffer
  ClearBuffers();

  //detect the visible objects
  Engine.Camera.Translate();
  Engine.Map.DetectVisibleCells();

  //set the current rendermode
  if not(Engine.Modes.RenderWireframe) then
  begin
    RenderSourceImage(Engine.Map.Water.UnderWater());

    //render the final image
    ClearFrame();
    RenderFinal();
  end
  else
  begin
    RenderState( RS_WIREFRAME );
    ClearFrame();
    glClear(GL_COLOR_BUFFER_BIT);
    Engine.Camera.Translate();
    RenderStaticGeometry();
  end;

  //render debug and ortho stuff
  RenderDebug();
  RenderGUI();
end;

end.
