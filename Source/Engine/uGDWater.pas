 unit uGDWater;

{$MODE Delphi}

interface

uses
  SDL2,
  Classes,
  LCLIntf,
  LCLType,
  JsonTools,
  SysUtils,
  dglOpenGL,
  uGDTexture,
  uGDTypes,
  uGDTypesGenerics,
  uGDGLWrappers,
  uGDConstants,
  uGDResource,
  uGDTerrain;

type
  TGDWater = Class
  private
    FLastTime        : Integer;
    FBoundingBox     : TGDBoundingBox;
    FRefractionUV    : Integer;
    FWavesUV         : Integer;
    FWidth           : Integer;
    FHeight          : Integer;
    FCellCountX      : Integer;
    FCellCountY      : Integer;
    FColor           : TGDColor;
    FReflection      : TGDTexture;
    FRenderBuffer    : TGDGLRenderBuffer;
    FFrameBuffer     : TGDGLFrameBuffer;
    FWaterLoaded     : Boolean;
    FCausticTextures : TGDTextureList;
    FCausticCounter  : Integer;
    FWaterTextures   : TGDTextureList;
    FWaterCounter    : Integer;
    FDepth           : Double;
    FMinDistance     : Double;
    FMaxDistance     : Double;
    FVertices        : TGDVertex_V_UV_List;
    FVertexBuffer    : TGDGLVertexBuffer;

    function GetHeight() : Double;
  public
    Property BoundingBox : TGDBoundingBox read FBoundingBox;
    Property WaterHeight : Double read GetHeight;
    property CellCountX : Integer read FCellCountX;
    property CellCountY : Integer read FCellCountY;
    property RefractionUV : Integer read FRefractionUV;
    property WavesUV : Integer read FWavesUV;
    property WaterLoaded : Boolean read FWaterLoaded;
    property Color : TGDColor read FColor;
    Property Depth : Double read FDepth;
    Property MinDistance : Double read FMinDistance;
    Property MaxDistance : Double read FMaxDistance;

    constructor Create();
    destructor  Destroy(); override;

    function    InitWater(aTerrain : TGDTerrain; aNode : TJsonNode ) : boolean;
    procedure   Clear();

    procedure Resize();
    function  Visible(): boolean;
    function  UnderWater(): boolean;
    function  AddVertex(aV : TGDVertex_V_UV): integer;
    procedure UpdateVBO();

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
  uGDEngine;

constructor TGDWater.Create();
begin
  FFrameBuffer     := TGDGLFrameBuffer.Create();
  FWaterLoaded     := false;
  FCausticCounter  := 0;
  FWaterCounter    := 0;
  FCausticTextures := TGDTextureList.Create(false);
  FWaterTextures   := TGDTextureList.Create(false);
  FVertices        := TGDVertex_V_UV_List.Create();
end;


destructor  TGDWater.Destroy();
begin
  Clear();
  FreeAndNil(FFrameBuffer);
  FreeAndNil(FCausticTextures);
  FreeAndNil(FWaterTextures);
  FreeAndNil(FVertices);
  inherited;
end;


function TGDWater.GetHeight() : Double;
begin
  result := FBoundingBox.Max.Y;
end;


function TGDWater.InitWater(aTerrain : TGDTerrain; aNode : TJsonNode ) : boolean;
var
  iI, iCount : Integer;
  iPath, iExt : String;
  iError : string;
begin
  GDConsole.Write('Loading water...');
  GDConsole.Use := False;
  try
    result        := true;
    FWaterLoaded  := true;

    FCellCountX := (aTerrain.TerrainWidth-1) div TERRAIN_CELLSIZE;
    FCellCountY := (aTerrain.TerrainHeight-1) div TERRAIN_CELLSIZE;
    FColor.Reset( aNode.Find('Color') );

    FBoundingBox.Max.Reset(aTerrain.GetPoint(aTerrain.TerrainWidth-1, 0).Vertex.x,
                           aNode.Find('Height').AsNumber,
                           aTerrain.GetPoint(0, aTerrain.TerrainHeight-1).Vertex.z);
    FBoundingBox.Min.Reset(aTerrain.GetPoint(0, 0).Vertex.x,
                           aNode.Find('Height').AsNumber,
                           aTerrain.GetPoint(0, 0).Vertex.z);

    FRefractionUV := Trunc(aNode.Find('RefractionUV').AsNumber);
    FWavesUV      := Trunc(aNode.Find('WavesUV').AsNumber);
    FDepth        := Trunc(aNode.Find('Depth').AsNumber);
    FMinDistance  := aNode.Find('MinDistance').AsNumber;
    FMaxDistance  := aNode.Find('MaxDistance').AsNumber;

    Resize();
    FWaterTextures.Clear();
    FCausticTextures.Clear();
    FWaterCounter := 0;

    //Water textures
    iCount := Trunc(aNode.Find('WaterTexturesCount').AsNumber);
    iPath  := aNode.Find('WaterMapPath').AsString + aNode.Find('WaterMapPrefix').AsString;
    iExt   := aNode.Find('WaterMapExtension').AsString;
    for iI := 0 to iCount-1 do
      FWaterTextures.Add( GDResources.LoadTexture(iPath + IntToStr(iI) + '.' + iExt ,GDSettings.TextureDetail,GDSettings.TextureFilter) );

    //Caustic textures
    iCount := Trunc(aNode.Find('CausticTexturesCount').AsNumber);
    iPath  := aNode.Find('CausticsMapPath').AsString;
    iExt   := aNode.Find('CausticsMapExtension').AsString;
    for iI := 0 to iCount-1 do
      FCausticTextures.Add( GDResources.LoadTexture(iPath + IntToStr(iI) + '.' + iExt ,GDSettings.TextureDetail,GDSettings.TextureFilter) );

    //create VBO
    FVertexBuffer := TGDGLVertexBuffer.Create();

    //timing
    FLastTime     := GDTiming.GetTime()+50;
  except
    on E: Exception do
    begin
      iError := E.Message;
      result := false;
      FWaterLoaded := false;
    end;
  end;
  GDConsole.Use := True;

  GDConsole.WriteOkFail(result, iError);
end;


procedure TGDWater.Resize();
begin
   case GDSettings.WaterDetail of
     WD_LOW    : begin
                   FWidth  := GDWindow.ScaledWidth() div 4;
                   FHeight := GDWindow.ScaledHeight() div 4;
                 end;

     WD_MEDIUM : begin
                   FWidth  := GDWindow.ScaledWidth() div 2;
                   FHeight := GDWindow.ScaledHeight() div 2;
                 end;
     WD_HIGH   : begin
                   FWidth  := GDWindow.ScaledWidth();
                   FHeight := GDWindow.ScaledHeight();
                 end;
  end;
  FreeAndNil(FRenderBuffer);
  FreeAndNil(FFrameBuffer);
  FreeAndNil(FReflection);
  FRenderBuffer := TGDGLRenderBuffer.Create(FWidth,FHeight,GL_DEPTH_COMPONENT24);
  FFrameBuffer  := TGDGLFrameBuffer.Create();
  FReflection   := TGDTexture.Create(GL_RGBA, GL_RGBA, FWidth,FHeight);
end;


procedure TGDWater.Clear();
var
  iI : integer;
  iTex : TGDTexture;
begin
  FreeAndNil(FVertexBuffer);
  FreeAndNil(FRenderBuffer);
  FreeAndNil(FFrameBuffer);
  FreeAndNil(FReflection);

  FBoundingBox.Min.Reset(0,0,0);
  FBoundingBox.Max.Reset(0,0,0);

  FVertices.Clear();
  FColor.Reset(1,1,1,1);

  for iI := 0 to FCausticTextures.Count-1 do
  begin
    iTex := TGDTexture(FCausticTextures.Items[iI]);
    GDResources.RemoveResource(TGDResource(iTex));
  end;
  FCausticTextures.Clear();
  FCausticCounter := 0;

  for iI := 0 to FWaterTextures.Count-1 do
  begin
    iTex := TGDTexture(FWaterTextures.Items[iI]);
    GDResources.RemoveResource(TGDResource(iTex));
  end;
  FWaterTextures.Clear();
  FWaterCounter := 0;

  FWaterLoaded := false;
end;


Function TGDWater.Visible() : boolean;
begin
  result := GDCamera.BoxInView(FBoundingBox);
end;


procedure TGDWater.Update();
begin
  If not(FWaterLoaded) then exit;

  if SDL_TICKS_PASSED(GDTiming.GetTime(), FLastTime) then
  begin
    FLastTime := GDTiming.GetTime()+50;

    inc(FCausticCounter);
    If FCausticCounter = FCausticTextures.Count-1 then
    FCausticCounter := 0;

    inc(FWaterCounter);
    If FWaterCounter = FWaterTextures.Count-1 then
    FWaterCounter := 0;
  end;
end;


procedure TGDWater.StartReflection();
var
  FClipPlane : array [0..3] of Double;
begin
  FFrameBuffer.Bind();
  FFrameBuffer.AttachTexture(FReflection,GL_COLOR_ATTACHMENT0_EXT,GL_TEXTURE_2D);
  FFrameBuffer.AttachRenderBuffer(FRenderBuffer,GL_DEPTH_ATTACHMENT_EXT);
  FFrameBuffer.Status();
  glViewPort(0,0,FWidth,FHeight);
  glClear(GL_DEPTH_BUFFER_BIT or GL_COLOR_BUFFER_BIT);
  If GDCamera.Position.Y > FBoundingBox.Max.Y then
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


procedure TGDWater.EndReflection();
begin
  If GDCamera.Position.Y > FBoundingBox.Max.Y then
  begin
    glCullFace(GL_BACK);
    glPolygonMode(GL_FRONT, GL_FILL);
    glPopMatrix();
  end;
  glDisable(GL_CLIP_PLANE0);
  FFrameBuffer.UnBind();
end;


procedure TGDWater.StartRendering( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor );
begin
  if Not(aRenderAttribute = RA_NORMAL) then exit;

  FVertexBuffer.Bind(VL_V_UV);
  if GDModes.RenderWireframe then
  begin
    GDRenderer.ColorShader.Bind();
    GDRenderer.SetColor(0.3,0.3,1,1);
  end
  else
  begin
    case aRenderFor of
        RF_NORMAL, RF_WATER : begin
                   with GDRenderer do
                   begin
                     WaterShader.Bind();
                     WaterShader.SetInt('T_REFLECTION', 0);
                     WaterShader.SetInt('T_DUDVMAP', 1);
                     WaterShader.SetInt('T_CAUSTICMAP', 5);
                     WaterShader.SetInt('I_REFRACTION_UV', RefractionUV);
                     WaterShader.SetInt('I_WAVES_UV', WavesUV);
                     GDRenderer.SetJoinedParams(WaterShader);
                   end;

                   FReflection.BindTexture(GL_TEXTURE0);
                   BindWaterTexture();
                   BindCausticTexture();
                   glEnable(GL_BLEND);
                   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        end;
    end;
  end;
  glDisable(GL_CULL_FACE);
end;


procedure TGDWater.EndRendering();
begin
  FVertexBuffer.Unbind();
  glEnable(GL_CULL_FACE);
  glDisable(GL_BLEND);
end;


function TGDWater.UnderWater(): boolean;
begin
  If ( GDCamera.Position.Y < FBoundingBox.Max.Y ) then
    result := true
  else
    result := false;
end;


function  TGDWater.AddVertex(aV : TGDVertex_V_UV): integer;
begin
  result := FVertices.add(aV);
end;


procedure TGDWater.UpdateVBO();
begin
  FVertexBuffer.Bind(VL_NONE);
  FVertexBuffer.Update(FVertices, GL_STATIC_DRAW);
  FVertexBuffer.Unbind()
end;


procedure TGDWater.BindCausticTexture();
begin
  If FWaterLoaded then
  begin
    TGDTexture(FCausticTextures.Items[ FCausticCounter ]).BindTexture(GL_TEXTURE6);
  end;
end;


procedure TGDWater.BindWaterTexture();
begin
  If FWaterLoaded then
  begin
    TGDTexture(FWaterTextures.Items[FWaterCounter]).BindTexture(GL_TEXTURE1);
  end;
end;

end.
