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
  Graphics,
  Windows,
  SysUtils,
  dglOpenGL,
  GDConstants,
  GDSettings,
  GDTexture,
  GDGLWrappers,
  GDTypesGenerics,
  GDTypes,
  GDModes,
  GDTiming;

type

{******************************************************************************}
{* Renderer class                                                             *}
{******************************************************************************}

  TGDRenderer  = Class
  private
    FResourceWND   : HWND;
    FResourceDC    : HDC;
    FResourceRC    : HGLRC;

    FViewPortWND   : HWND;
    FViewPortDC    : HDC;
    FViewPortRC    : HGLRC;
    FCanResize     : boolean;
    FState         : TGDRenderState;
    FInitialized   : boolean;
    
    FTerrainShader : TGDGLShader;
    FSkyShader     : TGDGLShader;
    FWaterShader   : TGDGLShader;
    FGrassShader   : TGDGLShader;
    FBlurShader    : TGDGLShader;
    FMeshShader    : TGDGLShader;
    FFinalShader   : TGDGLShader;
    FColorShader   : TGDGLShader;
    FTextureShader : TGDGLShader;

    FFrameBuffer   : TGDGLFrameBuffer;
    FRenderBuffer1 : TGDGLRenderBuffer;
    FRenderBuffer2 : TGDGLRenderBuffer;
    FSourceImage   : TGDTexture;
    FBloomImage    : TGDTexture;
    FBlurImage     : TGDTexture;

    FShadowFrameBuffer : TGDGLFrameBuffer;
    FShadowTexture     : TGDTexture;

    FBloomStrengh  : Single;

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
    property    BloomStrengh : Single read FBloomStrengh write FBloomStrengh;

    property    TerrainShader  : TGDGLShader read FTerrainShader;
    property    SkyShader      : TGDGLShader read FSkyShader;
    property    WaterShader    : TGDGLShader read FWaterShader;
    property    GrassShader    : TGDGLShader read FGrassShader;
    property    MeshShader     : TGDGLShader read FMeshShader;
    property    ColorShader    : TGDGLShader read FColorShader;

    Constructor Create();
    Destructor  Destroy();override;

    function    InitViewPort( aWnd  : HWND ): boolean;
    function    ShutDownViewPort() : boolean;
    procedure   ResizeViewPort();

    procedure   SetColor(aC : TGDColor); overload;
    procedure   SetColor(aR, aG, aB, aA : Single); overload;
    procedure   SetJoinedParams(aShader : TGDGLShader; aForShadows : boolean = false);
    procedure   RenderState( aState : TGDRenderState );

    procedure   AddLine(aV1, aV2 : TGDVector);

    procedure   StartFrame();
    procedure   EndFrame();
    function    MakeCurrent() : boolean;
    procedure   SwitchToOrtho();
    procedure   SwitchToPerspective();
    procedure   VerticalSync();

    procedure   Render();
  end;

var
  Renderer : TGDRenderer;

implementation

uses
  GDConsole,
  GDMain,
  GDCamera,
  GDStatistics,
  GDGUI,
  GDMap;

{******************************************************************************}
{* Create the renderer class                                                  *}
{******************************************************************************}

constructor TGDRenderer.Create();
var
  iError      : string;
  iWndClass   : TWndClass;
  iDWStyle    : DWORD;
  iDWExStyle  : DWORD;
  iInstance   : HINST;
  iStr, iV    : String;
  iGLInt1, iGLInt2 : GLInt;
  iGLFLoat    : GLFLoat;
  iI : Integer;
  iQ  : TGDVertex_V_UV;
  iQL : TGDVertex_V_UV_List;

function WndProc(aWnd: HWND; aMsg: UINT;  aWParam: WPARAM;  aLParam: LPARAM): LRESULT; stdcall;
begin
  Result := 1;
end;

begin
  Inherited;
  FCanResize := false;
  Timing.Start();
  Console.Write('......Initializing renderer');
  try
    FInitialized := true;
    iInstance := GetModuleHandle(nil);
    ZeroMemory(@iWndClass, SizeOf(wndClass));

    with iWndClass do
    begin
      style         := CS_HREDRAW or CS_VREDRAW or CS_OWNDC;
      lpfnWndProc   := @WndProc;
      hInstance     := iInstance;
      hCursor       := LoadCursor(0, IDC_ARROW);
      lpszClassName := 'OpenGL';
    end;

    if (RegisterClass(iWndClass) = 0) then
      Raise Exception.Create('Failed to register reource windows class');

    iDWStyle   := WS_OVERLAPPEDWINDOW or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
    iDWExStyle := WS_EX_APPWINDOW or WS_EX_WINDOWEDGE;
    FResourceWND := CreateWindowEx(iDWExStyle,
                                      'OpenGL',
                                      'Window',
                                      iDWStyle,
                                      0, 0,
                                      50, 50,
                                      0,
                                      0,
                                      iInstance,
                                      nil);

    if FResourceWND = 0 then
      Raise Exception.Create('Failed to create resource window');

    //Get the device context
    FResourceDC := GetDC(FResourceWND);
    if (FResourceDC = 0) then
      Raise Exception.Create('Failed to get a device context');

    //Create the OpenGL rendering context
    FResourceRC := CreateRenderingContext(FResourceDC, [opDoubleBuffered, opStereo], 32, 32, 0, 0, 0, 0);;
    if (FResourceRC = 0) then
      Raise Exception.Create('Failed to create a rendering context');

    //Activate the rendering context
    ActivateRenderingContext(FResourceDC, FResourceRC);

    //Read OpenGL properties and implementation
    ReadExtensions;
    ReadImplementationProperties;

    //Print specs
    Console.Write('Vendor: ' + String(AnsiString(glGetString(GL_VENDOR))));
    Console.Write('Renderer: ' + String(AnsiString(glGetString(GL_RENDERER))));
    Console.Write('Version: ' + String(AnsiString(glGetString(GL_VERSION))));
    Console.Write('GLSL Version: ' + String(AnsiString(glGetString(GL_SHADING_LANGUAGE_VERSION))));

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
    Console.Write('Texture units: ' + IntToStr(iGLInt1));
    if iGLInt1 < MRS_TEXTURE_UNITS then
      Raise Exception.Create('Not ennough texture units! Minimal of ' + IntToStr(MRS_TEXTURE_UNITS) + ' needed.');

    //Anisotropic filtering
    glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, @iGLFLoat);
    Console.Write('Anisotropic filtering: ' + FormatFloat('#####', iGLFLoat));
    if iGLFLoat < MRS_ANISOTROPIC_FILTERING then
      Raise Exception.Create('To low anisotropic filtering! Minimal of ' + IntToStr(MRS_ANISOTROPIC_FILTERING) + ' needed.');

    //Texture size
    glGetFloatv(GL_MAX_TEXTURE_SIZE, @iGLFLoat);
    Console.Write('Texture size: ' + FormatFloat('#####',iGLFLoat));
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

    //commands
    Console.AddCommand('RBloomMult', '0.0 to 1.0 : Set the bloom multiplier value', CT_FLOAT, @FBloomStrengh);
  except
    on E: Exception do
    begin
      iError := E.Message;
      FInitialized := false;
      Console.Write('Failed to initialize renderer: ' + iError);
    end;
  end;

  If FInitialized then
  begin
    Timing.Stop();
    Console.Write('......Done initializing renderer (' + Timing.TimeInSeconds + ' Sec)');
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
  iInstance   : HINST;
begin
  inherited;
  Console.Write('Shutting down renderer...');
  try
    //Clear shaders.
    ClearShaders();

    //For normal rendering.
    FreeAndNil(FLinesVertices);
    FreeAndNil(FLinesVertexBuffer);
    FreeAndNil(FQuadVertexBuffer);

    //Destroy rendering context
    DeactivateRenderingContext();
    DestroyRenderingContext(FResourceRC);

    //destroy the window
    if ((FResourceWND <> 0) and (not DestroyWindow(FResourceWND))) then
      Raise Exception.Create('Failed to destroy window');

    iInstance := GetModuleHandle(nil);
    if (not UnRegisterClass('OpenGL', iInstance)) then
      Raise Exception.Create('Failed to unregister window class');
  except
    on E: Exception do
    begin
      iError := E.Message;
      iResult := false;
    end;
  end;
  Console.WriteOkFail(iResult, iError);
end;

{******************************************************************************}
{* Init the viewport                                                          *}
{******************************************************************************}

function TGDRenderer.InitViewPort( aWnd  : HWND ): boolean;
var
  iError    : string;
begin
  Console.Write('Initializing viewport...');
  try
    Result := true;

    //get the device context
    FViewPortWND := aWnd;
    FViewPortDC := GetDC(FViewPortWND);
    if (FViewPortDC = 0) then
      Raise Exception.Create('Failed to get a device context');

    //Create the OpenGL rendering context
    FViewPortRC := CreateRenderingContext(FViewPortDC, [opDoubleBuffered, opStereo], 32, 32, 0, 0, 0, 0);;
    if (FViewPortRC = 0) then
      Raise Exception.Create('Failed to create a rendering context');

    //Activate and share the rendering context
    ActivateRenderingContext(FViewPortDC, FViewPortRC);
    wglShareLists(FResourceRC, FViewPortRC);

    ResizeViewPort();
    VerticalSync();
    InitFrameBuffers();
    InitShadowFrameBuffers();

    FCanResize := true;
  except
    on E: Exception do
    begin
      iError := E.Message;
      result := false;
    end;
  end;
  Console.WriteOkFail(result, iError);

  if result then
    Main.InitBaseResources();
end;

{******************************************************************************}
{* Shutdown the renderer                                                      *}
{******************************************************************************}

function TGDRenderer.ShutDownViewPort() : boolean;
var
  iError    : string;
begin
  Console.Write('Shutting down viewport...');
  try
    FCanResize := false;
    result := true;
    Main.ClearBaseResources();
    ClearFrameBuffers();
    ClearShadowFrameBuffers();
    wglMakeCurrent(0, 0);
    if (not wglDeleteContext(FViewPortRC)) then
    begin
      FViewPortRC := 0;
      Raise Exception.Create('Unable to activate OpenGL rendering context!');
    end;
    if ((FViewPortDC > 1) and (ReleaseDC(FViewPortWND, FViewPortDC) = 0)) then
    begin
      FViewPortDC := 0;
      Raise Exception.Create('Release of device context failed!');
    end;
  except
    on E: Exception do
    begin
      iError := E.Message;
      result := false;
    end;
  end;

  Console.WriteOkFail(result, iError);
end;

{******************************************************************************}
{* Resize the windows viewport                                                *}
{******************************************************************************}

procedure TGDRenderer.ResizeViewPort();
begin
  if not(FCanResize) then exit;
  MakeCurrent();
  if (Settings.Height = 0) then
    Settings.Height := 1;
  glViewport(0, 0, Settings.Width, Settings.Height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(40.0, Settings.Width/Settings.Height, 25, Settings.ViewDistance * R_VIEW_DISTANCE_STEP);
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
  aShader.Bind();
  aShader.SetFloat3('V_LIGHT_DIR',  Map.LightDirection.X,
                                    Map.LightDirection.Y,
                                    Map.LightDirection.Z);
  aShader.SetFloat4('V_LIGHT_AMB',  Map.LightAmbient.R,
                                    Map.LightAmbient.G,
                                    Map.LightAmbient.B,
                                    Map.LightAmbient.A);
  aShader.SetFloat4('V_LIGHT_DIFF', Map.LightDiffuse.R,
                                    Map.LightDiffuse.G,
                                    Map.LightDiffuse.B,
                                    Map.LightDiffuse.A);
  aShader.SetFloat('F_LIGHT_SHADOW', Map.LightShadow);

  aShader.SetFloat('F_MIN_VIEW_DISTANCE', Map.FogMinDistance);
  aShader.SetFloat('F_MAX_VIEW_DISTANCE', Map.FogMaxDistance);
  aShader.SetFloat4('V_FOG_COLOR', Map.FogColor.R, Map.FogColor.G, Map.FogColor.B, Map.FogColor.A);

  If Map.Water.UnderWater() then
    aShader.SetInt('I_UNDER_WATER', 1)
  else
    aShader.SetInt('I_UNDER_WATER', 0);
  aShader.SetFloat('I_WATER_HEIGHT', Map.Water.WaterHeight);
  aShader.SetFloat4('V_WATER_COLOR', Map.Water.Color.R,
                                     Map.Water.Color.G,
                                     Map.Water.Color.B,
                                     Map.Water.Color.A);
  aShader.SetFloat('I_WATER_DEPTH', Map.Water.Depth);
  aShader.SetFloat('I_WATER_MAX', Map.Water.MaxDistance);
  aShader.SetFloat('I_WATER_MIN', Map.Water.MinDistance);

  aShader.SetFloat3('V_CAM_POS', Camera.Position.x,  Camera.Position.Y,  Camera.Position.Z );

  if not(aForShadows) then
  begin
    aShader.SetInt('T_SHADOWMAP', 7);
    FShadowTexture.BindTexture(GL_TEXTURE7);
  end
  else
  begin
    glActiveTexture(GL_TEXTURE7);
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
     RS_TEXTURE  : begin
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
  Timing.Start();
  Console.Write('......Initializing shaders');
  FTerrainShader  := TGDGLShader.Create(SHADER_TERRAIN);
  FSkyShader      := TGDGLShader.Create(SHADER_SKY);
  FWaterShader    := TGDGLShader.Create(SHADER_WATER);
  FGrassShader    := TGDGLShader.Create(SHADER_GRASS);
  FBlurShader     := TGDGLShader.Create(SHADER_BLUR);
  FMeshShader     := TGDGLShader.Create(SHADER_MESH);
  FFinalShader    := TGDGLShader.Create(SHADER_FINAL);
  FColorShader    := TGDGLShader.Create(SHADER_COLOR);
  FTextureShader  := TGDGLShader.Create(SHADER_TEXTURE);
  Timing.Stop();
  Console.Write('......Done initializing shaders (' + Timing.TimeInSeconds + ' Sec)');
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
  FreeAndNil(FFinalShader);
  FreeAndNil(FColorShader);
  FreeAndNil(FTextureShader);
end;

{******************************************************************************}
{* Init the framebuffers                                                      *}
{******************************************************************************}

procedure TGDRenderer.InitFrameBuffers();
begin
  FFrameBuffer   := TGDGLFrameBuffer.Create();
  FRenderBuffer1 := TGDGLRenderBuffer.Create(Settings.Width, Settings.Height, GL_DEPTH_COMPONENT24);
  FRenderBuffer2 := TGDGLRenderBuffer.Create(Settings.Width div 4, Settings.Height div 4, GL_DEPTH_COMPONENT24);
  FSourceImage   := TGDTexture.Create(GL_RGBA, GL_RGBA, Settings.Width, Settings.Height );
  FBloomImage    := TGDTexture.Create(GL_RGBA, GL_RGBA, Settings.Width, Settings.Height );
  FBlurImage     := TGDTexture.Create(GL_RGBA, GL_RGBA, Settings.Width div 4, Settings.Height div 4);
end;

procedure TGDRenderer.InitShadowFrameBuffers();
begin
  FShadowFrameBuffer := TGDGLFrameBuffer.Create();
  FShadowFrameBuffer.Bind();
  FShadowTexture     := TGDTexture.Create(GL_DEPTH_COMPONENT, GL_DEPTH_COMPONENT, R_SHADOW_SIZE, R_SHADOW_SIZE );
  glDrawBuffer(GL_NONE);
  glReadBuffer(GL_NONE);
  FShadowFrameBuffer.AttachTexture(FShadowTexture, GL_DEPTH_ATTACHMENT_EXT, GL_TEXTURE_2D );
  FShadowFrameBuffer.Status();
  FShadowFrameBuffer.Unbind();
end;

{******************************************************************************}
{* Clear the framebuffers                                                     *}
{******************************************************************************}

procedure TGDRenderer.ClearFrameBuffers();
begin
  FreeAndNil(FFrameBuffer);
  FreeAndNil(FRenderBuffer1);
  FreeAndNil(FRenderBuffer2);
  FreeAndNil(FSourceImage);
  FreeAndNil(FBloomImage);
  FreeAndNil(FBlurImage);
end;

procedure TGDRenderer.ClearShadowFrameBuffers();
begin
  FreeAndNil(FShadowFrameBuffer);
  FreeAndNil(FShadowTexture);
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
{* Start a frame                                                              *}
{******************************************************************************}

procedure TGDRenderer.StartFrame();
begin
  glClear(GL_DEPTH_BUFFER_BIT or GL_COLOR_BUFFER_BIT);
  glLoadIdentity;
end;

{******************************************************************************}
{* End a frame                                                                *}
{******************************************************************************}

procedure TGDRenderer.EndFrame();
begin
  SwapBuffers(FViewPortDC);
end;

{******************************************************************************}
{* Make the rendercontext current                                             *}
{******************************************************************************}

function TGDRenderer.MakeCurrent() : boolean;
begin
  Result := wglMakeCurrent(FViewPortDC, FViewPortRC);
end;

{******************************************************************************}
{* Switch to ortho view                                                       *}
{******************************************************************************}

procedure TGDRenderer.SwitchToOrtho();
begin
  glEnable(GL_DEPTH_TEST);
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
  glEnable(GL_DEPTH_TEST);
  glMatrixMode(GL_PROJECTION);
  glPopMatrix();
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix();
end;

{******************************************************************************}
{* Set vertical sync on or off                                                *}
{******************************************************************************}

procedure TGDRenderer.VerticalSync();
var
   iI : Integer;
begin
   if WGL_EXT_swap_control then
   begin
      iI := wglGetSwapIntervalEXT;

      If Settings.VerticalSync then
        if iI<>1 then
          wglSwapIntervalEXT(1);

      If not(Settings.VerticalSync) then
        if iI<>0 then
          wglSwapIntervalEXT(0);
   end;
end;

procedure TGDRenderer.Render();

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
  glViewport(0, 0, Settings.Width div 4, Settings.Height div 4);
  glDisable(GL_DEPTH_TEST);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  FBlurShader.Bind();
  FBlurShader.SetInt( 'T_BLUR_IMAGE', 0 );
  FFrameBuffer.Bind();
  FFrameBuffer.AttachRenderBuffer(FRenderBuffer2, GL_DEPTH_ATTACHMENT_EXT);

  //horizontal
  FFrameBuffer.AttachTexture(FBlurImage,GL_COLOR_ATTACHMENT0_EXT,GL_TEXTURE_2D);
  FFrameBuffer.Status();
  FBlurShader.SetFloat4('V_BLUR_OFFSET',aBlurStrength / Settings.Width, 0, 0, 1);
  aSourceImage.BindTexture( GL_TEXTURE0 );
  RenderQuad();

  //vertical
  glViewport(0, 0, Settings.Width, Settings.Height);
  FFrameBuffer.AttachTexture(aSourceImage,GL_COLOR_ATTACHMENT0_EXT,GL_TEXTURE_2D);
  FFrameBuffer.Status();
  FBlurShader.SetFloat4('V_BLUR_OFFSET', 0, aBlurStrength / Settings.Height, 0, 1);
  FBlurImage.BindTexture( GL_TEXTURE0 );
  RenderQuad();

  FFrameBuffer.Unbind();
  FBlurShader.UnBind();
  glEnable(GL_DEPTH_TEST);
end;

{******************************************************************************}
{* Render debug                                                               *}
{******************************************************************************}

Procedure RenderDebug();

procedure RenderLines();
begin
  FLinesVertexBuffer.Bind(VL_V);
  FLinesVertexBuffer.Update(FLinesVertices, GL_DYNAMIC_DRAW);
  FLinesVertexBuffer.Render(GL_LINES);
  FLinesVertexBuffer.Unbind();
end;

begin
  glLoadIdentity();
  Camera.Translate();
  RenderState( RS_COLOR );
  If Modes.RenderNormals     then
  begin
    FLinesVertices.Clear();
    Renderer.SetColor(1,0.5,0.25,1);
    Map.RenderVisibleCells( RA_NORMALS, RF_NORMAL );
    RenderLines();
  end;
  If Modes.RenderObjectBoxes then
  begin
    FLinesVertices.Clear();
    Renderer.SetColor(1,0,0,1);
    Map.RenderVisibleCells( RA_FRUSTUM_BOXES, RF_NORMAL );
    RenderLines();
  end;
  If Modes.RenderNodeBoxes then
  begin
    FLinesVertices.Clear();
    Renderer.SetColor(1,1,0,1);
    Map.RenderVisibleCells( RA_NODE_BOXES, RF_NORMAL );
    RenderLines();
  end;
end;

{******************************************************************************}
{* Render water reflections                                                   *}
{******************************************************************************}

procedure RenderWaterReflection();
begin
  If (Modes.RenderWireframe = false) and Map.Water.Visible() then
  begin
    //render reflection texture
    StartFrame();
    Camera.Translate();
    Map.Water.StartReflection();
    If Modes.RenderSky then Map.SkyDome.Render();
    Camera.CalculateFrustum();
    Map.DetectVisibleCells();
    Map.RenderVisibleCells( RA_NORMAL, RF_WATER );
    Map.Water.EndReflection();
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
  If Modes.RenderWireframe = false then
  begin
    //render shadow texture
    FShadowFrameBuffer.Bind();
    glViewport(0, 0, R_SHADOW_SIZE, R_SHADOW_SIZE);
    glClear(GL_DEPTH_BUFFER_BIT);
    glLoadIdentity;

    If Settings.UseShadows = false then
    begin
      FShadowFrameBuffer.Unbind();
      glViewport(0, 0, Settings.Width, Settings.Height);
      exit;
    end;

    iM.CreateRotation( Camera.Rotation );
    iV.Reset(-1,0,0);
    iM.ApplyToVector(iV);
    iV.y := 0;
    iV.Normalize();

    glMatrixMode(GL_PROJECTION);
    glPushMatrix();
    glLoadIdentity();
    glOrtho(-10000, 10000, -10000, 10000, -15000, 15000);
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();
    glLoadIdentity();
    gluLookAt(Camera.Position.x+(iV.x*9800), Camera.Position.y+7500, Camera.Position.z+(iV.z*9800),
              Camera.Position.x+0.01+(iV.x*9800), Camera.Position.y+7500-1, Camera.Position.z+0.01+(iV.z*9800),
              iV.x,0,iV.z);
    Camera.CalculateFrustum();

    glColorMask(FALSE, FALSE, FALSE, FALSE);
    Map.DetectVisibleCells();
    Map.RenderVisibleCells( RA_NORMAL, RF_SHADOW );
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

    FShadowFrameBuffer.Unbind();
    glViewport(0, 0, Settings.Width, Settings.Height);
  end;
end;

{******************************************************************************}
{* Render GUI                                                               *}
{******************************************************************************}

procedure RenderGUI();
begin
  SwitchToOrtho();
    GUI.RenderScreens();
    If Modes.RenderStats then Statistics.Render();
    Console.Render();
    GUI.MouseCursor.Render();
  SwitchToPerspective();
end;

{******************************************************************************}
{* Render static geometry                                                     *}
{******************************************************************************}

Procedure RenderStaticGeometry();
begin
  Map.ApplyDistanceFog();
  Map.SkyDome.Render();
  Map.RenderVisibleCells( RA_NORMAL, RF_NORMAL );
end;

{******************************************************************************}
{* Render source image                                                        *}
{******************************************************************************}

procedure RenderSourceImage();
begin
  FFrameBuffer.Bind();
  FFrameBuffer.AttachTexture(FSourceImage,GL_COLOR_ATTACHMENT0_EXT,GL_TEXTURE_2D);
  FFrameBuffer.AttachRenderBuffer(FRenderBuffer1,GL_DEPTH_ATTACHMENT_EXT);
  FFrameBuffer.Status();
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  RenderStaticGeometry();

  FFrameBuffer.UnBind();
end;

{******************************************************************************}
{* Render underwater source image                                             *}
{******************************************************************************}

procedure RenderUnderWaterSourceImage();
begin
  FFrameBuffer.Bind();
  FFrameBuffer.AttachTexture(FSourceImage,GL_COLOR_ATTACHMENT0_EXT,GL_TEXTURE_2D);
  FFrameBuffer.AttachRenderBuffer(FRenderBuffer1,GL_DEPTH_ATTACHMENT_EXT);
  FFrameBuffer.Status();
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  RenderStaticGeometry();

  FFrameBuffer.UnBind();

  ApplyBlurToImage( FSourceImage, 3 );
end;

{******************************************************************************}
{* Render bloom image                                                         *}
{******************************************************************************}

procedure RenderBloomImage();
begin
  FFrameBuffer.Bind();
  FFrameBuffer.AttachTexture(FBloomImage,GL_COLOR_ATTACHMENT0_EXT,GL_TEXTURE_2D);
  FFrameBuffer.AttachRenderBuffer(FRenderBuffer1,GL_DEPTH_ATTACHMENT_EXT);
  FFrameBuffer.Status();
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  If Modes.RenderSky then Map.SkyDome.Render();
  Map.RenderVisibleCells( RA_NORMAL, RF_BLOOM );

  FFrameBuffer.UnBind();

  ApplyBlurToImage( FBloomImage, 1.5 );
end;

{******************************************************************************}
{* Render the final source image                                              *}
{******************************************************************************}

procedure RenderFinal();
begin
  glDisable(GL_DEPTH_TEST);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  FFinalShader.Bind();
  FFinalShader.SetInt('T_SOURCE_IMAGE',0);

  If Settings.UseFXAA and not(Modes.RenderObjectBoxes or Modes.RenderNormals or Modes.RenderNodeBoxes) then
    FFinalShader.SetInt('I_DO_FXAA',1)
  else
    FFinalShader.SetInt('I_DO_FXAA',0);

  If Settings.UseBloom then
  begin
    FFinalShader.SetInt('I_DO_BLOOM',1);
    FFinalShader.SetInt('T_BLOOM_IMAGE',1);
    FFinalShader.SetFloat('I_BLOOM_STENGTH',FBloomStrengh);
    FBloomImage.BindTexture( GL_TEXTURE1 );
  end
  else
    FFinalShader.SetInt('I_DO_BLOOM',0);

  FFinalShader.SetFloat2('V_SCREEN_SIZE',Settings.Width, Settings.Height);
  FFinalShader.SetFloat('I_GAMMA',Settings.Gamma);
  FSourceImage.BindTexture( GL_TEXTURE0 );

  RenderQuad();

  FFinalShader.UnBind();
  glEnable(GL_DEPTH_TEST);
end;

begin
  //make renderer current
  MakeCurrent();

  //create the shadow texture
  RenderShadowMap();

  //create the water reflection texture
  RenderWaterReflection();

  //detect the visibel objects
  glLoadIdentity();
  Camera.Translate();
  Map.DetectVisibleCells();

  //set the current rendermode
  if not(Modes.RenderWireframe) then
  begin

    //Render the source image.
    if Map.Water.UnderWater() then
      RenderUnderWaterSourceImage()
    else
      RenderSourceImage();

    //render bloom image.
    If Settings.UseBloom then RenderBloomImage();

    //render the final image
    StartFrame();
    RenderFinal();
  end
  else
  begin
    RenderState( RS_WIREFRAME );
    StartFrame();
    Camera.Translate();
    Map.SkyDome.Render();
    Map.RenderVisibleCells( RA_NORMAL, RF_NORMAL );
  end;

  //render debug and ortho stuff
  RenderDebug();
  RenderGUI();

  //end the frame and increment the framecounter
  EndFrame();
end;

end.
