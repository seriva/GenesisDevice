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
  GDGLObjects,
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

    FFrameBuffer   : TGDGLFrameBufferObject;
    FRenderBuffer1 : TGDGLRenderBufferObject;
    FRenderBuffer2 : TGDGLRenderBufferObject;
    FSourceImage   : TGDTexture;
    FBloomImage    : TGDTexture;
    FBlurImage     : TGDTexture;
    FBloomStrengh  : Single;

    procedure InitShaders();
    procedure ClearShaders();
    procedure InitFrameBuffers();
    procedure ClearFrameBuffers();
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
    procedure   SetJoinedParams(aShader : TGDGLShader);
    procedure   RenderState( aState : TGDRenderState );

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
    glCullFace(GL_BACK);
    glEnable(GL_CULL_FACE);
    glDepthFunc(GL_LESS);
    glActiveTexture(GL_TEXTURE0);
    glEnable(GL_TEXTURE_2D);
    glActiveTexture(GL_TEXTURE1);
    glEnable(GL_TEXTURE_2D);
    glActiveTexture(GL_TEXTURE2);
    glEnable(GL_TEXTURE_2D);
    glActiveTexture(GL_TEXTURE3);
    glEnable(GL_TEXTURE_2D);
    glActiveTexture(GL_TEXTURE4);
    glEnable(GL_TEXTURE_2D);
    glActiveTexture(GL_TEXTURE5);
    glEnable(GL_TEXTURE_2D);
    glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
    glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);
    glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
    glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
    glEnable(GL_NORMALIZE);
    glDisable(GL_FOG);
    glDisable(GL_LIGHTING);

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
  gluPerspective(45.0, Settings.Width/Settings.Height, 25, Settings.ViewDistance * R_VIEW_DISTANCE_STEP);
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

procedure TGDRenderer.SetJoinedParams(aShader : TGDGLShader);
begin
  aShader.Enable();
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
  aShader.SetFloat('F_MIN_VIEW_DISTANCE', Map.Fog.MinDistance);
  aShader.SetFloat('F_MAX_VIEW_DISTANCE', Map.Fog.MaxDistance);
  aShader.SetFloat4('V_FOG_COLOR', Map.Fog.Color.R, Map.Fog.Color.G, Map.Fog.Color.B, Map.Fog.Color.A);
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
                     FColorShader.Enable();
                   end;
    RS_WIREFRAME : begin
                     glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
                     glClearColor(0.3, 0.3, 0.3, 1.0);
                     FColorShader.Enable();
                   end;
    RS_TEXTS   :   begin
                     glEnable(GL_DEPTH_TEST);
                     glDepthFunc(GL_LEQUAL);
                     glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
                     glEnable(GL_BLEND);
                     FTextureShader.Enable();
                     FTextureShader.SetInt('T_COLORMAP', 0);
                   end;
     RS_TEXTURE  : begin
                     FTextureShader.Enable();
                     FTextureShader.SetInt('T_COLORMAP', 0);
                   end;
    end;
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
  FFrameBuffer   := TGDGLFrameBufferObject.Create();
  FRenderBuffer1 := TGDGLRenderBufferObject.Create(Settings.Width, Settings.Height, GL_DEPTH_COMPONENT24);
  FRenderBuffer2 := TGDGLRenderBufferObject.Create(Settings.Width div 4, Settings.Height div 4, GL_DEPTH_COMPONENT24);
  FSourceImage   := TGDTexture.Create(GL_RGBA, Settings.Width, Settings.Height );
  FBloomImage    := TGDTexture.Create(GL_RGBA, Settings.Width, Settings.Height );
  FBlurImage     := TGDTexture.Create(GL_RGBA, Settings.Width div 4, Settings.Height div 4);
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
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0);   glVertex2f(-1, -1);
    glTexCoord2f(1, 0);   glVertex2f( 1, -1);
    glTexCoord2f(1, 1);   glVertex2f( 1, 1);
    glTexCoord2f(0, 1);   glVertex2f(-1, 1);
  glEnd;
end;

{******************************************************************************}
{* Apply blur to a source image                                               *}
{******************************************************************************}

procedure ApplyBlurToImage( aSourceImage : TGDTexture; aBlurStrength : double );
begin
  glViewport(0, 0, Settings.Width div 4, Settings.Height div 4);
  glDisable(GL_DEPTH_TEST);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  FBlurShader.Enable();
  FBlurShader.SetInt( 'T_BLUR_IMAGE', 0 );
  FFrameBuffer.Bind();
  FFrameBuffer.AttachRenderBufferObject(FRenderBuffer2, GL_DEPTH_ATTACHMENT_EXT);

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
  FBlurShader.Disable();
  glEnable(GL_DEPTH_TEST);
end;

{******************************************************************************}
{* Render debug                                                               *}
{******************************************************************************}

Procedure RenderDebug();
begin
  glLoadIdentity();
  Camera.Translate();
  RenderState( RS_COLOR );
  If Modes.RenderNormals     then Map.RenderVisibleCells( RA_NORMALS, RF_NORMAL );
  If Modes.RenderObjectBoxes then Map.RenderVisibleCells( RA_FRUSTUM_BOXES, RF_NORMAL );
  If Modes.RenderNodeBoxes   then Map.RenderVisibleCells( RA_NODE_BOXES, RF_NORMAL );
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
{* Render GUI                                                               *}
{******************************************************************************}

procedure RenderGUI();
begin
  SwitchToOrtho();
    If Modes.RenderWireframe = false then Map.Water.RenderUnderWater();
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
  //Render sky
  Map.Fog.UseDistanceFog();
  Map.SkyDome.Render();

  //Set the right fog type
  If not(Camera.Position.Y > Map.Water.WaterHeight) then
    Map.Fog.UseWaterFog();

  //Render other cells.
  Map.RenderVisibleCells( RA_NORMAL, RF_NORMAL );
end;

{******************************************************************************}
{* Render source image                                                        *}
{******************************************************************************}

procedure RenderSourceImage();
begin
  FFrameBuffer.Bind();
  FFrameBuffer.AttachTexture(FSourceImage,GL_COLOR_ATTACHMENT0_EXT,GL_TEXTURE_2D);
  FFrameBuffer.AttachRenderBufferObject(FRenderBuffer1,GL_DEPTH_ATTACHMENT_EXT);
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
  FFrameBuffer.AttachRenderBufferObject(FRenderBuffer1,GL_DEPTH_ATTACHMENT_EXT);
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
  FFrameBuffer.AttachRenderBufferObject(FRenderBuffer1,GL_DEPTH_ATTACHMENT_EXT);
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

  FFinalShader.Enable();
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

  FFinalShader.Disable();
  glEnable(GL_DEPTH_TEST);
end;

begin
  //make renderer current
  MakeCurrent();

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
