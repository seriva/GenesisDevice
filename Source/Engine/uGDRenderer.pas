unit uGDRenderer;

{$MODE Delphi}

interface

uses
  SysUtils,
  dglOpenGL,
  uGDConstants,
  uGDTexture,
  uGDGLWrappers,
  uGDTypes;

type
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
    procedure InitFrameBuffers(aWidth, aHeight : integer);
    procedure ClearFrameBuffers();
    procedure InitShadowFrameBuffers();
    procedure ClearShadowFrameBuffers();
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

    procedure   ResizeViewPort();

    procedure   SetColor(aC : TGDColor); overload;
    procedure   SetColor(aR, aG, aB, aA : Single); overload;
    procedure   SetJoinedParams(aShader : TGDGLShader; aForShadows : boolean = false);
    procedure   RenderState( aState : TGDRenderState );

    procedure   AddLine(aV1, aV2 : TGDVector);

    procedure   ClearFrame();
    procedure   SwitchToOrtho(aWidth, aHeight : integer);
    procedure   SwitchToPerspective();

    procedure   Render();
  end;

implementation

uses
  uGDConsole,
  uGDEngine;

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
  GDTiming.Start();
  GDConsole.Write('.....Initializing renderer');
  try
    FInitialized := true;

    //Read OpenGL properties and implementation
    InitOpenGL;
    ReadExtensions;

    //Print specs
    GDConsole.Write('  Vendor: ' + String(AnsiString(glGetString(GL_VENDOR))));
    GDConsole.Write('  Renderer: ' + String(AnsiString(glGetString(GL_RENDERER))));
    GDConsole.Write('  Version: ' + String(AnsiString(glGetString(GL_VERSION))));
    GDConsole.Write('  GLSL Version: ' + String(AnsiString(glGetString(GL_SHADING_LANGUAGE_VERSION))));

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
    GDConsole.Write('  Texture units: ' + IntToStr(iGLInt1));
    if iGLInt1 < MRS_TEXTURE_UNITS then
      Raise Exception.Create('Not ennough texture units! Minimal of ' + IntToStr(MRS_TEXTURE_UNITS) + ' needed.');

    //Anisotropic filtering
    glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, @iGLFLoat);
    GDConsole.Write('  Anisotropic filtering: ' + FormatFloat('#####', iGLFLoat));
    if iGLFLoat < MRS_ANISOTROPIC_FILTERING then
      Raise Exception.Create('To low anisotropic filtering! Minimal of ' + IntToStr(MRS_ANISOTROPIC_FILTERING) + ' needed.');

    //Texture size
    glGetFloatv(GL_MAX_TEXTURE_SIZE, @iGLFLoat);
    GDConsole.Write('  Texture size: ' + FormatFloat('#####',iGLFLoat));
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
    GDConsole.AddCommand('RSSAOStrength', '0.0 to 1.0 : Set SSAO strength', CT_FLOAT, @FSSAOStrength);
    GDConsole.AddCommand('RSSAOSamples', '8, 16, 32, 64 : Set SSAO sample count', CT_INTEGER, @FSSAOSamples);
    GDConsole.AddCommand('RSSAORadius', '0.0 to 10.0 : Set SSAO radius', CT_FLOAT, @FSSAORadius);
    GDConsole.AddCommand('RSSAOOnly', '0.0 to 1.0 : Only show SSAO', CT_INTEGER, @FSSAOOnly);
    GDConsole.AddCommand('RShadowFilter', '0.0 to 1.0 : For shadow filtering', CT_INTEGER, @FShadowFilter);

    InitFrameBuffers(GDWindow.ScaledWidth(), GDWindow.ScaledHeight());
    InitShadowFrameBuffers();
  except
    on E: Exception do
    begin
      iError := E.Message;
      FInitialized := false;
      GDConsole.Write('Failed to initialize renderer: ' + iError);
    end;
  end;

  If FInitialized then
  begin
    GDTiming.Stop();
    GDConsole.Write('.....Done initializing renderer (' + GDTiming.TimeInSeconds + ' Sec)');
    InitShaders();
  end;
end;


Destructor TGDRenderer.Destroy();
var
  iError  : string;
  iResult : boolean;
begin
  inherited;
  GDConsole.Write('Shutting down renderer...');
  try
    //Clear shaders.
    ClearShaders();

    //Clear framebuffers
    ClearFrameBuffers();
    ClearShadowFrameBuffers();

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
  GDConsole.WriteOkFail(iResult, iError);
end;


procedure TGDRenderer.ResizeViewPort();
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(40.0, GDWindow.Width/GDWindow.Height, 25, GDSettings.ViewDistance * R_VIEW_DISTANCE_STEP);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  ClearFrameBuffers();
  InitFrameBuffers(GDWindow.ScaledWidth, GDWindow.ScaledHeight);
end;


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


procedure TGDRenderer.SetJoinedParams(aShader : TGDGLShader; aForShadows : boolean = false);
begin
  //Ligthing
  aShader.Bind();
  aShader.SetFloat3('V_LIGHT_DIR',  GDMap.LightDirection.X,
                                    GDMap.LightDirection.Y,
                                    GDMap.LightDirection.Z);
  aShader.SetFloat4('V_LIGHT_AMB',  GDMap.LightAmbient.R,
                                    GDMap.LightAmbient.G,
                                    GDMap.LightAmbient.B,
                                    GDMap.LightAmbient.A);
  aShader.SetFloat4('V_LIGHT_DIFF', GDMap.LightDiffuse.R,
                                    GDMap.LightDiffuse.G,
                                    GDMap.LightDiffuse.B,
                                    GDMap.LightDiffuse.A);

  //Fog
  aShader.SetFloat('F_MIN_VIEW_DISTANCE', GDMap.FogMinDistance);
  aShader.SetFloat('F_MAX_VIEW_DISTANCE', GDMap.FogMaxDistance);
  aShader.SetFloat4('V_FOG_COLOR', GDMap.FogColor.R, GDMap.FogColor.G, GDMap.FogColor.B, GDMap.FogColor.A);

  //Water
  If GDMap.Water.UnderWater() then
    aShader.SetInt('I_UNDER_WATER', 1)
  else
    aShader.SetInt('I_UNDER_WATER', 0);
  aShader.SetFloat('I_WATER_HEIGHT', GDMap.Water.WaterHeight);
  aShader.SetFloat4('V_WATER_COLOR', GDMap.Water.Color.R,
                                     GDMap.Water.Color.G,
                                     GDMap.Water.Color.B,
                                     GDMap.Water.Color.A);
  aShader.SetFloat('I_WATER_DEPTH', GDMap.Water.Depth);
  aShader.SetFloat('I_WATER_MAX', GDMap.Water.MaxDistance);
  aShader.SetFloat('I_WATER_MIN', GDMap.Water.MinDistance);
  aShader.SetFloat3('V_CAM_POS', GDCamera.Position.x,  GDCamera.Position.Y,  GDCamera.Position.Z );
  aShader.SetInt('T_CAUSTICMAP', 6);
  GDMap.Water.BindCausticTexture();

  //Detail
  If GDSettings.UseDetail then
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


procedure TGDRenderer.AddLine(aV1, aV2 : TGDVector);
begin
  FLinesVertices.Add( aV1.Copy() );
  FLinesVertices.Add( aV2.Copy() );
end;


procedure TGDRenderer.InitShaders();
begin
  GDTiming.Start();
  GDConsole.Write('.....Initializing shaders');
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
  GDTiming.Stop();
  GDConsole.Write('.....Done initializing shaders (' + GDTiming.TimeInSeconds + ' Sec)');
end;


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


procedure TGDRenderer.InitFrameBuffers(aWidth, aHeight : integer);
var
  iBuffers : array[0..1] of GLEnum;
begin
  //Frame
  FFrameFBO := TGDGLFrameBuffer.Create();
  FFrameTex := TGDTexture.Create(GL_RGBA, GL_RGBA, aWidth, aHeight );
  FFrameShadowTex := TGDTexture.Create(GL_RGBA, GL_RGBA, aWidth, aHeight );
  FFrameDepthTex := TGDTexture.Create(GL_DEPTH_COMPONENT, GL_DEPTH_COMPONENT, aWidth, aHeight );
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
  FBlurTex := TGDTexture.Create(GL_RGBA, GL_RGBA, aWidth div 4, aHeight div 4);
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


procedure TGDRenderer.ClearFrame();
begin
  glClear(GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;
end;


procedure TGDRenderer.SwitchToOrtho(aWidth, aHeight : integer);
begin
  glViewport(0, 0, GDWindow.Width(), GDWindow.Height());
  glMatrixMode(GL_PROJECTION);
  glPushMatrix();
  glLoadIdentity();
  glOrtho(0, aWidth, 0, aHeight, -1, 1);
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadIdentity();
end;


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

Procedure RenderStaticGeometry(aRenderFor : TGDRenderFor = RF_NORMAL);
begin
  If GDModes.RenderSky then GDMap.SkyDome.Render();
  GDMap.RenderVisibleCells( RA_NORMAL, aRenderFor );
end;

procedure RenderQuad();
begin
  FQuadVertexBuffer.Bind(VL_V_UV);
  FQuadVertexBuffer.Render(GL_QUADS);
  FQuadVertexBuffer.Unbind()
end;

procedure ApplyBlurToImage( aSourceImage : TGDTexture; aBlurStrength : double );
begin
  glViewport(0, 0, GDWindow.ScaledWidth() div 4, GDWindow.ScaledHeight() div 4);
  glDisable(GL_DEPTH_TEST);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  FBlurShader.Bind();
  FBlurShader.SetInt( 'T_BLUR_IMAGE', 0 );

  //horizontal
  FBlurFBO.Bind();
  FBlurFBO.AttachTexture(FBlurTex,GL_COLOR_ATTACHMENT0_EXT,GL_TEXTURE_2D);
  FBlurFBO.Status();
  FBlurShader.SetFloat4('V_BLUR_OFFSET',aBlurStrength / GDWindow.ScaledWidth(), 0, 0, 1);
  aSourceImage.BindTexture( GL_TEXTURE0 );
  RenderQuad();

  //vertical
  glViewport(0, 0, GDWindow.ScaledWidth(), GDWindow.ScaledHeight());
  FBlurFBO.AttachTexture(aSourceImage,GL_COLOR_ATTACHMENT0_EXT,GL_TEXTURE_2D);
  FBlurFBO.Status();
  FBlurShader.SetFloat4('V_BLUR_OFFSET', 0, aBlurStrength / GDWindow.ScaledHeight(), 0, 1);
  FBlurTex.BindTexture( GL_TEXTURE0 );
  RenderQuad();

  FBlurFBO.Unbind();
  FBlurShader.UnBind();
  glEnable(GL_DEPTH_TEST);
end;


Procedure RenderDebug();

procedure RenderLines(aR, aG, aB, aA : single; aRA : TGDRenderAttribute);
begin
  FLinesVertices.Clear();
  GDMap.RenderVisibleCells(aRA, RF_NORMAL);
  RenderState( RS_COLOR );
  SetColor(aR, aG, aB, aA);
  FLinesVertexBuffer.Bind(VL_V);
  FLinesVertexBuffer.Update(FLinesVertices, GL_DYNAMIC_DRAW);
  FLinesVertexBuffer.Render(GL_LINES);
  FLinesVertexBuffer.Unbind();
end;

begin
  glViewport(0, 0, GDWindow.Width(), GDWindow.Height());
  glLoadIdentity();
  GDCamera.Translate();
  If GDModes.RenderNormals then RenderLines(1,0.5,0.25,1,RA_NORMALS);
  If GDModes.RenderObjectBoxes then RenderLines(1,0,0,1, RA_FRUSTUM_BOXES);
  If GDModes.RenderNodeBoxes then RenderLines(1,1,0,1, RA_NODE_BOXES);
end;

procedure RenderWaterReflection();
begin
  If (GDModes.RenderWireframe = false) and GDMap.Water.Visible() then
  begin
    //render reflection texture
    ClearFrame();
    iC := GDMap.FogColor.Copy();
    glClearColor(iC.R, iC.G, iC.B, iC.A);
    glClear(GL_COLOR_BUFFER_BIT);
    GDCamera.Translate();
    GDMap.Water.StartReflection();
    GDCamera.CalculateFrustum();
    GDMap.DetectVisibleCells();
    RenderStaticGeometry(RF_WATER);
    GDMap.Water.EndReflection();
  end;
end;

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
  If GDModes.RenderWireframe = false then
  begin
    //render shadow texture
    FShadowFBO.Bind();
    glViewport(0, 0, R_SHADOW_SIZE, R_SHADOW_SIZE);
    glClear(GL_DEPTH_BUFFER_BIT);
    glLoadIdentity;

    If GDSettings.UseShadows = false then
    begin
      FShadowFBO.Unbind();
      exit;
    end;

    iM.CreateRotation( GDCamera.Rotation );
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
    gluLookAt(GDCamera.Position.x+(iV.x*9800), GDCamera.Position.y+7500, GDCamera.Position.z+(iV.z*9800),
              GDCamera.Position.x+0.01+(iV.x*9800), GDCamera.Position.y+7500-1, GDCamera.Position.z+0.01+(iV.z*9800),
              iV.x,0,iV.z);
    GDCamera.CalculateFrustum();

    glColorMask(FALSE, FALSE, FALSE, FALSE);
    GDMap.DetectVisibleCells();
    GDMap.RenderVisibleCells( RA_NORMAL, RF_SHADOW );
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
  end;
end;

procedure RenderGUI();
begin
  SwitchToOrtho(R_HUD_WIDTH, R_HUD_HEIGHT);
    GDGUI.RenderScreens();
    If GDModes.RenderStats then GDStatistics.Render();
    GDConsole.Render();
    GDGUI.MouseCursor.Render();
  SwitchToPerspective();
end;

procedure RenderSourceImage(aUnderWater : boolean);

begin
  glViewport(0, 0, GDWindow.ScaledWidth(), GDWindow.ScaledHeight());
  FFrameFBO.Bind();
  glClear(GL_DEPTH_BUFFER_BIT);

  RenderStaticGeometry();

  FFrameFBO.UnBind();

  if aUnderWater then
    ApplyBlurToImage( FFrameTex, 3 );

  If GDSettings.UseShadows and (FShadowFilter = 1) then
    ApplyBlurToImage( FFrameShadowTex, 8);
end;

procedure ClearBuffers();
begin
  glDisable(GL_DEPTH_TEST);
  FFrameFBO.Bind();
  glLoadIdentity();
  FClearShader.Bind();
  iC := GDMap.FogColor.Copy();
  FClearShader.SetFloat4('V_COLOR', iC.R, iC.G, iC.B, iC.A);
  RenderQuad();
  FClearShader.Unbind();
  FFrameFBO.Unbind();
  glEnable(GL_DEPTH_TEST);
end;

procedure RenderFinal();
begin
  glViewport(0, 0, GDWindow.Width(), GDWindow.Height());
  glDisable(GL_DEPTH_TEST);
  FPostShader.Bind();
  FPostShader.SetInt('T_SOURCE_IMAGE',0);
  FPostShader.SetInt('T_SHADOW_IMAGE',1);

  If GDSettings.UseFXAA and not(GDModes.RenderObjectBoxes or GDModes.RenderNormals or GDModes.RenderNodeBoxes) then
    FPostShader.SetInt('I_DO_FXAA',1)
  else
    FPostShader.SetInt('I_DO_FXAA',0);

  if GDSettings.UseSSAO and not(GDMap.Water.UnderWater()) then
  begin
    FPostShader.SetInt('I_DO_SSAO',1);
    FPostShader.SetInt('T_DEPTH_IMAGE',2);
    FPostShader.SetFloat('I_SSAO_NEAR',25);
    FPostShader.SetFloat('I_SSAO_FAR', GDSettings.ViewDistance * R_VIEW_DISTANCE_STEP);
    FPostShader.SetFloat('I_SSAO_STRENGTH',FSSAOStrength);
    FPostShader.SetInt('I_SSAO_SAMPLES',FSSAOSamples);
    FPostShader.SetFloat('I_SSAO_RADIUS',FSSAORadius);
    FPostShader.SetInt('I_SSAO_ONLY',FSSAOOnly);
    FFrameDepthTex.BindTexture( GL_TEXTURE2 );
  end
  else
    FPostShader.SetInt('I_DO_SSAO',0);

  FPostShader.SetFloat2('V_SCREEN_SIZE',GDWindow.Width(), GDWindow.Height());
  FPostShader.SetFloat('I_GAMMA',GDSettings.Gamma);
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
  GDCamera.Translate();
  GDMap.DetectVisibleCells();

  //set the current rendermode
  if not(GDModes.RenderWireframe) then
  begin
    RenderSourceImage(GDMap.Water.UnderWater());

    //render the final image
    ClearFrame();
    RenderFinal();
  end
  else
  begin
    RenderState( RS_WIREFRAME );
    ClearFrame();
    glClear(GL_COLOR_BUFFER_BIT);
    GDCamera.Translate();
    RenderStaticGeometry();
  end;

  //render debug and ortho stuff
  RenderDebug();
  RenderGUI();
end;

end.
