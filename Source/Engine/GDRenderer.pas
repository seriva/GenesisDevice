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
  Classes,
  dglOpenGL,
  GDConstants,
  GDSettings,
  GDTexture,
  GDGLObjects,
  GDLog,
  GDModes,
  GDTiming;

type

{******************************************************************************}
{* Renderer class                                                             *}
{******************************************************************************}

  TGDRenderer  = Class
  private
    FWND  : HWND;
    FDC   : HDC;
    FRC   : HGLRC;
    FCanResize          : boolean;
    FState              : TGDRenderState;
    
    FTerrainShader      : TGDGLShader;
    FSkyShader          : TGDGLShader;
    FWaterShader        : TGDGLShader;
    FGrassShader        : TGDGLShader;
    FBlurShader         : TGDGLShader;
    FBloomMixShader     : TGDGLShader;
    FMeshShader         : TGDGLShader;
    FFinalShader        : TGDGLShader;
    FCopyShader         : TGDGLShader;
    FColorShader        : TGDGLShader;
    FTextureShader      : TGDGLShader;

    FFrameBuffer         : TGDGLFrameBufferObject;
    FRenderBuffer1       : TGDGLRenderBufferObject;
    FRenderBuffer2       : TGDGLRenderBufferObject;
    FSourceImage1        : TGDTexture;
    FSourceImage2        : TGDTexture;
    FBloomImage          : TGDTexture;
    FHorizontalBlurImage : TGDTexture;
    FVerticalBlurImage   : TGDTexture;
    FBloomStrengh        : Double;

    procedure InitShaders();
    procedure ClearShaders();
    procedure InitFrameBuffers();
    procedure ClearFrameBuffers();
    Procedure ResizeFrameBuffers();

    procedure RenderQuad();
  public
    property    WindowHandle  : HWND read FWND write FWND;
    property    DeviceContext : HDC read FDC write FDC;
    property    RenderContext : HGLRC read FRC write FRC;
    property    State : TGDRenderState read FState write FState;

    property    TerrainShader  : TGDGLShader read FTerrainShader;
    property    SkyShader      : TGDGLShader read FSkyShader;
    property    WaterShader    : TGDGLShader read FWaterShader;
    property    GrassShader    : TGDGLShader read FGrassShader;
    property    BlurShader     : TGDGLShader read FBlurShader;
    property    BloomMixShader : TGDGLShader read FBloomMixShader;
    property    CopyShader     : TGDGLShader read FCopyShader;
    property    MeshShader     : TGDGLShader read FMeshShader;
    property    FinalShader    : TGDGLShader read FFinalShader;
    property    ColorShader    : TGDGLShader read FColorShader;
    property    TextureShader  : TGDGLShader read FTextureShader;

    property    BloomStrengh : Double read FBloomStrengh write FBloomStrengh;

    Constructor Create();
    Destructor  Destroy();override;

    function    InitRenderer( aWnd  : HWND ): boolean;
    function    ShutDownRenderer() : boolean;

    procedure   ResizeViewPort();
    procedure   RenderState( aState : TGDRenderState );
    procedure   StartFrame();
    procedure   EndFrame();
    function    MakeCurrent() : boolean;
    procedure   SwitchToOrtho();
    procedure   SwitchToPerspective();
    procedure   VerticalSync();
    function    MakeScreenShot(aFileName : string): boolean;

    function    CheckUsePostProcessing(): boolean;
    procedure   StartRenderSource();
    procedure   EndRenderSource();
    procedure   StartRenderUnderWaterSource();
    procedure   EndRenderUnderWaterSource();
    procedure   StartRenderBloom();
    procedure   EndRenderBloom();

    procedure   ApplyBlurToImage( aSourceImage : TGDTexture; aBlurStrength : double );
    procedure   RenderFinal();
  end;

var
  Renderer : TGDRenderer;

implementation

uses
  GDConsole,
  GDMain,
  GDWater,
  GDLighting;

{******************************************************************************}
{* Create the renderer class                                                  *}
{******************************************************************************}

constructor TGDRenderer.Create();
begin
  Inherited;
  FCanResize := false;
end;

{******************************************************************************}
{* Destroy the renderer class                                                 *}
{******************************************************************************}

Destructor TGDRenderer.Destroy();
begin
  Inherited;
end;

{******************************************************************************}
{* Init the renderer                                                          *}
{******************************************************************************}

function TGDRenderer.InitRenderer( aWnd  : HWND ): boolean;
var
  iPFD : TPIXELFORMATDESCRIPTOR;
  iError    : string;
  iPixelFormat : Integer;
begin
  Log.AddNewLine('Initializing renderer...');
  try
    Result := true;

    FWND := aWnd;

    FDC := GetDC(FWND);
    if (FDC = 0) then
      Raise Exception.Create('Unable to get a device context!');

    fillchar(iPFD, Sizeof(iPFD), 0);
    with iPFD do
    begin
      nSize           := SizeOf(TPIXELFORMATDESCRIPTOR);
      nVersion        := 1;
      dwFlags         := PFD_DRAW_TO_WINDOW
                      or PFD_SUPPORT_OPENGL
                      or PFD_DOUBLEBUFFER;
      iPixelType      := PFD_TYPE_RGBA;
      cColorBits      := 32;
      cRedBits := 0;                         
      cRedShift := 0;
      cGreenBits := 0;
      cGreenShift := 0;
      cBlueBits := 0;
      cBlueShift := 0;
      cAlphaBits := 0;                               
      cAlphaShift := 0;
      cAccumBits := 0;
      cAccumRedBits := 0;
      cAccumGreenBits := 0;
      cAccumBlueBits := 0;
      cAccumAlphaBits := 0;
      cDepthBits := 24;
      cStencilBits := 0;
      cAuxBuffers := 0;
      iLayerType := PFD_MAIN_PLANE;
      bReserved := 0;
      dwLayerMask := 0;
      dwVisibleMask := 0;
      dwDamageMask := 0;
    end;

    iPixelFormat := ChoosePixelFormat(FDC, @iPFD);
    if (iPixelFormat = 0) then
       Raise Exception.Create('Unable to find a suitable pixel format!');

    if (not SetPixelFormat(FDC, iPixelFormat, @iPFD)) then
       Raise Exception.Create('Unable to set the pixel format!');

    FRC := wglCreateContext(FDC);
    if (FRC = 0) then
       Raise Exception.Create('Unable to create an OpenGL rendering context!');

    if (not wglMakeCurrent(FDC, FRC)) then
       Raise Exception.Create('Unable to activate OpenGL rendering context!');

    ReadExtensions;
    ReadImplementationProperties;

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

  If result then
  begin
    Log.AddToLastLine('Succeeded');
    InitShaders();
    Main.InitBaseResources();
    DirectionalLight.Clear();
  end
  else
  begin
    Log.AddToLastLine('Failed');
    Log.AddNewLine('Error Message: ' + iError);
  end;
end;

{******************************************************************************}
{* Shutdown the renderer                                                      *}
{******************************************************************************}

function TGDRenderer.ShutDownRenderer() : boolean;
var
  iError    : string;
begin
  Log.AddNewLine('Shutting down renderer...');
  try
    FCanResize := false;
    result := true;
    ClearShaders();
    ClearFrameBuffers();
    Main.ClearBaseResources();
    wglMakeCurrent(0, 0);
    if (not wglDeleteContext(FRC)) then
    begin
      FRC := 0;
      Raise Exception.Create('Unable to activate OpenGL rendering context!');
    end;
    if ((FDC > 1) and (ReleaseDC(FWND, FDC) = 0)) then
    begin
      FDC := 0;
      Raise Exception.Create('Release of device context failed!');
    end;
  except
    on E: Exception do
    begin
      iError := E.Message;
      result := false;
    end;
  end;

  If result then
    Log.AddToLastLine('Succeeded')
  else
  begin
    Log.AddToLastLine('Failed');
    Log.AddNewLine('Error Message: ' + iError);
  end;
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
{* Set the renderstate                                                        *}
{******************************************************************************}

procedure  TGDRenderer.RenderState( aState : TGDRenderState );
begin
  FState := aState;
  glPolygonMode(GL_FRONT, GL_FILL);
  glColor4f(1,1,1,1);
  glDisable(GL_BLEND);

  Case FState Of
    RS_COLOR   :   begin
                     FColorShader.Enable();
                   end;
    RS_WIREFRAME : begin
                     glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
                     glClearColor(0.5, 0.5, 0.5, 1.0);
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
  Timer.Start();
  Log.AddNewLine('......Initializing shaders');
  FTerrainShader  := TGDGLShader.Create();
  FTerrainShader.InitShaders( SHADER_TERRAIN );
  FSkyShader      := TGDGLShader.Create();
  FSkyShader.InitShaders( SHADER_SKY );
  FWaterShader    := TGDGLShader.Create();
  FWaterShader.InitShaders( SHADER_WATER );
  FGrassShader    := TGDGLShader.Create();
  FGrassShader.InitShaders( SHADER_GRASS );
  FBlurShader     := TGDGLShader.Create();
  FBlurShader.InitShaders( SHADER_BLUR );
  FBloomMixShader := TGDGLShader.Create();
  FBloomMixShader.InitShaders( SHADER_BLOOMMIX );
  FCopyShader   := TGDGLShader.Create();
  FCopyShader.InitShaders( SHADER_COPY );
  FMeshShader := TGDGLShader.Create();
  FMeshShader.InitShaders( SHADER_MESH );
  FFinalShader := TGDGLShader.Create();
  FFinalShader.InitShaders( SHADER_FINAL );
  FColorShader := TGDGLShader.Create();
  FColorShader.InitShaders( SHADER_COLOR );
  FTextureShader := TGDGLShader.Create();
  FTextureShader.InitShaders( SHADER_TEXTURE );

  Timer.Stop();
  Log.AddNewLine('......Done initializing shaders (' + Timer.TimeInSeconds + ' Sec)');
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
  FreeAndNil(FBloomMixShader);
  FreeAndNil(FCopyShader);
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
  FFrameBuffer         := TGDGLFrameBufferObject.Create();
  FRenderBuffer1       := TGDGLRenderBufferObject.Create();
  FRenderBuffer2       := TGDGLRenderBufferObject.Create();
  FSourceImage1        := TGDTexture.Create();
  FSourceImage2        := TGDTexture.Create();
  FBloomImage          := TGDTexture.Create();
  FHorizontalBlurImage := TGDTexture.Create();
  FVerticalBlurImage   := TGDTexture.Create();
  FBloomStrengh        := 0.5;
  FFrameBuffer.InitFrameBuffer();
  FRenderBuffer1.InitRenderBuffer(Settings.Width, Settings.Height, GL_DEPTH_COMPONENT24);
  FRenderBuffer2.InitRenderBuffer(Settings.Width div 4, Settings.Height div 4, GL_DEPTH_COMPONENT24);
  FSourceImage1.RenderTextureFloat( Settings.Width, Settings.Height );
  FSourceImage2.RenderTextureFloat( Settings.Width, Settings.Height );
  FBloomImage.RenderTextureFloat( Settings.Width, Settings.Height );
  FHorizontalBlurImage.RenderTextureFloat( Settings.Width div 4, Settings.Height div 4);
  FVerticalBlurImage.RenderTextureFloat( Settings.Width div 4, Settings.Height div 4);
end;

{******************************************************************************}
{* Clear the framebuffers                                                     *}
{******************************************************************************}

procedure TGDRenderer.ClearFrameBuffers();
begin
  FreeAndNil(FFrameBuffer);
  FreeAndNil(FRenderBuffer1);
  FreeAndNil(FRenderBuffer2);
  FreeAndNil(FSourceImage1);
  FreeAndNil(FSourceImage2);
  FreeAndNil(FBloomImage);
  FreeAndNil(FHorizontalBlurImage);
  FreeAndNil(FVerticalBlurImage);
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
  SwapBuffers(FDC);
end;

{******************************************************************************}
{* Make the rendercontext current                                             *}
{******************************************************************************}

function TGDRenderer.MakeCurrent() : boolean;
begin
  Result := wglMakeCurrent(FDC, FRC);
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

{******************************************************************************}
{* Create a screenshot                                                        *}
{******************************************************************************}

function TGDRenderer.MakeScreenShot(aFileName : string): boolean;

type
  TBMPheader = packed record
    Magic          : word;
    Headersize     : longword;
    Reserved       : longword;
    Dataofs        : longword;
    Size           : longword;
    Width          : longword;
    Height         : longword;
    Planes         : word;
    BitCount       : word;
    Compression    : longword;
    SizeImage      : longword;
    XPelsPerMeter  : longword;
    YPelsPerMeter  : longword;
    ClrUsed        : longword;
    ClrImportant   : longword;
  end;

var
  iError        : string;
  iConsoleShow    : boolean;
  iViewPort     : array[0..3] of integer;
  iBuffer       : pchar;
  iBufferLength : Integer;
  iFile         : TMemoryStream;
  BMPheader     : TBMPheader;
begin
  Log.Save := false;
  Log.AddNewLine('Saving screenshot to file ' + aFileName + '...');
  try
    result := true;

    Modes.TakingScreenShot := true;
    iConsoleShow := Console.Show;
    Console.Show := false;
    glGetIntegerv(GL_VIEWPORT, @iViewPort);
    Main.RenderMain();
    Console.Show := iConsoleShow;
    glFinish();
    Modes.TakingScreenShot := false;

    iBufferLength := (iViewPort[2] * iViewPort[3]) * 3;
    getmem(iBuffer, iBufferLength);
    glPixelStorei(GL_PACK_ALIGNMENT, 1);
    glReadPixels(0, 0, iViewPort[2], iViewPort[3], GL_BGR_EXT, GL_UNSIGNED_BYTE, iBuffer);
    fillchar(BMPheader, sizeof(BMPheader), 0);

    BMPheader.magic               := 19778;
    BMPheader.headersize          := 54;
    BMPheader.reserved            := 0;
    BMPheader.dataofs             := 54;
    BMPheader.Size                := 40;
    BMPheader.Width               := iViewPort[2];
    BMPheader.Height              := iViewPort[3];
    BMPheader.Planes              := 1;
    BMPheader.BitCount            := 24;
    BMPheader.SizeImage           := iBufferLength;
    BMPheader.XPelsPerMeter       := 36100;
    BMPheader.YPelsPerMeter       := 36100;
    BMPheader.ClrUsed             := 0;
    BMPheader.ClrImportant        := 0;

    iFile := TMemoryStream.Create();
    iFile.Write(BMPheader, sizeof(BMPheader));
    iFile.Write(iBuffer^, iBufferLength);
    iFile.SaveToFile( FP_SCREENSHOTS + aFileName + '.bmp' );
    iFile.Free;

    freemem(iBuffer);
  except
    on E: Exception do
    begin
      iError := E.Message;
      result := false;
    end;
  end;

  If result then
    Log.AddToLastLine('Succeeded')
  else
  begin
    Log.AddToLastLine('Failed');
    Log.AddNewLine('Error Message: ' + iError);
  end;

  Log.Save := true;
end;

{******************************************************************************}
{* Check if post processing is used al together                               *}
{******************************************************************************}

function TGDRenderer.CheckUsePostProcessing(): boolean;
begin
  If ((Settings.UseBloom) or (Water.UnderWater) ) then
    result := true
  else
    result := false;
end;

{******************************************************************************}
{* Render the screen quad for post processing                                 *}
{******************************************************************************}

procedure TGDRenderer.RenderQuad();
begin
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0);   glVertex2f(-1, -1);
    glTexCoord2f(1, 0);   glVertex2f( 1, -1);
    glTexCoord2f(1, 1);   glVertex2f( 1, 1);
    glTexCoord2f(0, 1);   glVertex2f(-1, 1);
  glEnd;
end;

{******************************************************************************}
{* Start the rendering of the source image                                    *}
{******************************************************************************}

procedure TGDRenderer.StartRenderSource();
begin
  FFrameBuffer.Bind();
  FFrameBuffer.AttachTexture(FSourceImage1,GL_COLOR_ATTACHMENT0_EXT,GL_TEXTURE_2D);
  FFrameBuffer.AttachRenderBufferObject(FRenderBuffer1,GL_DEPTH_ATTACHMENT_EXT);
  FFrameBuffer.Status();
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
end;

{******************************************************************************}
{* End the rendering of the source image                                      *}
{******************************************************************************}

procedure TGDRenderer.EndRenderSource();
begin
  FFrameBuffer.UnBind();
end;

{******************************************************************************}
{* Start the rendering of the underwater source image                         *}
{******************************************************************************}

procedure TGDRenderer.StartRenderUnderWaterSource();
begin
  FFrameBuffer.Bind();
  FFrameBuffer.AttachTexture(FSourceImage1,GL_COLOR_ATTACHMENT0_EXT,GL_TEXTURE_2D);
  FFrameBuffer.AttachRenderBufferObject(FRenderBuffer1,GL_DEPTH_ATTACHMENT_EXT);
  FFrameBuffer.Status();
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
end;

{******************************************************************************}
{* End the rendering of the underwater source image                           *}
{******************************************************************************}

procedure TGDRenderer.EndRenderUnderWaterSource();
begin
  FFrameBuffer.UnBind();
  ApplyBlurToImage( FSourceImage1, 3 );

  glDisable(GL_DEPTH_TEST);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  FFrameBuffer.Bind();
  FFrameBuffer.AttachRenderBufferObject(FRenderBuffer1, GL_DEPTH_ATTACHMENT_EXT);
  FFrameBuffer.AttachTexture(FSourceImage2,GL_COLOR_ATTACHMENT0_EXT,GL_TEXTURE_2D);
  FFrameBuffer.Status();

  CopyShader.Enable();
  CopyShader.SetInt('T_SOURCE_IMAGE',0);
  FVerticalBlurImage.BindTexture( TU_1 );
  RenderQuad();
  CopyShader.Disable();

  FFrameBuffer.Unbind();
  glEnable(GL_DEPTH_TEST);
end;

{******************************************************************************}
{* Start the rendering of the bloom image                                     *}
{******************************************************************************}

procedure TGDRenderer.StartRenderBloom();
begin
  FFrameBuffer.Bind();
  FFrameBuffer.AttachTexture(FBloomImage,GL_COLOR_ATTACHMENT0_EXT,GL_TEXTURE_2D);
  FFrameBuffer.AttachRenderBufferObject(FRenderBuffer1,GL_DEPTH_ATTACHMENT_EXT);
  FFrameBuffer.Status();
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
end;

{******************************************************************************}
{* End the rendering of the bloom image                                       *}
{******************************************************************************}

procedure TGDRenderer.EndRenderBloom();
begin
  FFrameBuffer.UnBind();
  ApplyBlurToImage( FBloomImage, 1.5 );

  glDisable(GL_DEPTH_TEST);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  BloomMixShader.Enable();
  BloomMixShader.SetInt('T_SOURCE_IMAGE',0);
  BloomMixShader.SetInt('T_BLUR_IMAGE',1);
  BloomMixShader.SetFloat('I_BLOOM_STENGTH',FBloomStrengh);
  FFrameBuffer.Bind();
  FFrameBuffer.AttachRenderBufferObject(FRenderBuffer1, GL_DEPTH_ATTACHMENT_EXT);
  FFrameBuffer.AttachTexture(FSourceImage2,GL_COLOR_ATTACHMENT0_EXT,GL_TEXTURE_2D);
  FFrameBuffer.Status();

  FSourceImage1.BindTexture( TU_1 );
  FVerticalBlurImage.BindTexture( TU_2 );
  RenderQuad();

  FFrameBuffer.Unbind();
  BloomMixShader.Disable();
  glEnable(GL_DEPTH_TEST);
end;

{******************************************************************************}
{* Apply blur to a source image                                               *}
{******************************************************************************}

procedure TGDRenderer.ApplyBlurToImage( aSourceImage : TGDTexture; aBlurStrength : double );
begin
  glViewport(0, 0, Settings.Width div 4, Settings.Height div 4);
  glDisable(GL_DEPTH_TEST);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  BlurShader.Enable();
  BlurShader.SetInt( 'T_BLUR_IMAGE', 0 );
  FFrameBuffer.Bind();
  FFrameBuffer.AttachRenderBufferObject(FRenderBuffer2, GL_DEPTH_ATTACHMENT_EXT);

  //horizontal
  FFrameBuffer.AttachTexture(FHorizontalBlurImage,GL_COLOR_ATTACHMENT0_EXT,GL_TEXTURE_2D);
  FFrameBuffer.Status();

  BlurShader.SetFloat4('V_BLUR_OFFSET',aBlurStrength / Settings.Width, 0, 0, 1);
  aSourceImage.BindTexture( TU_1 );
  RenderQuad();

  //vertical
  FFrameBuffer.AttachTexture(FVerticalBlurImage,GL_COLOR_ATTACHMENT0_EXT,GL_TEXTURE_2D);
  FFrameBuffer.Status();
  BlurShader.SetFloat4('V_BLUR_OFFSET', 0, aBlurStrength / Settings.Height, 0, 1);
  FHorizontalBlurImage.BindTexture( TU_1 );
  RenderQuad();;

  FFrameBuffer.Unbind();
  BlurShader.Disable();

  glEnable(GL_DEPTH_TEST);
  glViewport(0, 0, Settings.Width, Settings.Height);
end;

{******************************************************************************}
{* Render the final source image                                              *}
{******************************************************************************}

procedure TGDRenderer.RenderFinal();
begin
  glDisable(GL_DEPTH_TEST);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  FinalShader.Enable();
  FinalShader.SetInt('T_SOURCE_IMAGE',0);

  If Settings.UseFXAA and not(Modes.RenderObjectBoxes or Modes.RenderNormals or Modes.RenderNodeBoxes) then
    FinalShader.SetInt('I_DO_FXAA',1)
  else
    FinalShader.SetInt('I_DO_FXAA',0);
  FinalShader.SetFloat2('V_SCREEN_SIZE',Settings.Width, Settings.Height);
  FinalShader.SetFloat('I_GAMMA',Settings.Gamma);

  if Water.UnderWater then
  begin
    FSourceImage2.BindTexture( TU_1 );
  end
  else
  begin
    If Settings.UseBloom then
      FSourceImage2.BindTexture( TU_1 )
    else
      FSourceImage1.BindTexture( TU_1 );
  end;

  RenderQuad();

  FinalShader.Disable();
  glEnable(GL_DEPTH_TEST);
end;

end.
