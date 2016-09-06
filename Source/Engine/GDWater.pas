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
  SDL2,
  Classes,
  LCLIntf,
  LCLType,
  IniFiles,
  GDStringParsing,
  SysUtils,
  dglOpenGL,
  GDTexture,
  GDTypes,
  GDTypesGenerics,
  GDGLWrappers,
  GDConstants,
  GDResource,
  GDTerrain;

type

{******************************************************************************}
{* water class                                                                *}
{******************************************************************************}

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

    function    InitWater(aTerrain : TGDTerrain; aIniFile : TIniFile ) : boolean;
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
  GDEngine;

{******************************************************************************}
{* Create the water class                                                     *}
{******************************************************************************}

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

{******************************************************************************}
{* Destroy the water class                                                    *}
{******************************************************************************}

destructor  TGDWater.Destroy();
begin
  Clear();
  FreeAndNil(FFrameBuffer);
  FreeAndNil(FCausticTextures);
  FreeAndNil(FWaterTextures);
  FreeAndNil(FVertices);
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

function TGDWater.InitWater(aTerrain : TGDTerrain; aIniFile : TIniFile ) : boolean;
var
  iI, iCount : Integer;
  iPath, iExt : String;
  iError : string;
begin
  Engine.Console.Write('Loading water...');
  Engine.Console.Use := False;
  try
    result        := true;
    FWaterLoaded  := true;

    FCellCountX := (aTerrain.TerrainWidth-1) div TERRAIN_CELLSIZE;
    FCellCountY := (aTerrain.TerrainHeight-1) div TERRAIN_CELLSIZE;
    FColor      := ReadColor(aIniFile, 'Water', 'Color');

    FBoundingBox.Max.Reset(aTerrain.GetPoint(aTerrain.TerrainWidth-1, 0).Vertex.x,
                           aIniFile.ReadFloat( 'Water', 'Height', 0 ),
                           aTerrain.GetPoint(0, aTerrain.TerrainHeight-1).Vertex.z);
    FBoundingBox.Min.Reset(aTerrain.GetPoint(0, 0).Vertex.x,
                           aIniFile.ReadFloat( 'Water', 'Height', 0 ),
                           aTerrain.GetPoint(0, 0).Vertex.z);

    FRefractionUV := aIniFile.ReadInteger( 'Water', 'RefractionUV', 1 );
    FWavesUV      := aIniFile.ReadInteger( 'Water', 'WavesUV', 1 );
    FDepth        := aIniFile.ReadFloat( 'Water', 'Depth', 500 );
    FMinDistance  := aIniFile.ReadFloat( 'Water', 'MinDistance', 0.1 );
    FMaxDistance  := aIniFile.ReadFloat( 'Water', 'MaxDistance', 0.2 );

    Resize();
    FWaterTextures.Clear();
    FCausticTextures.Clear();
    FWaterCounter := 0;

    //Water textures
    iCount := aIniFile.ReadInteger('Water', 'WaterTexturesCount', 10 );
    iPath  := aIniFile.ReadString( 'Water', 'WaterMapPath', 'textures\water\') + aIniFile.ReadString( 'Water', 'WaterMapPrefix', 'water');
    iExt   := aIniFile.ReadString( 'Water', 'WaterMapExtension', 'dds');
    for iI := 0 to iCount-1 do
      FWaterTextures.Add( Engine.Resources.LoadTexture(iPath + IntToStr(iI) + '.' + iExt ,Engine.Settings.TextureDetail,Engine.Settings.TextureFilter) );

    //Caustic textures
    iCount := aIniFile.ReadInteger('Water', 'CausticTexturesCount', 10 );
    iPath  := aIniFile.ReadString( 'Water', 'CausticsMapPath', 'textures\water\') + aIniFile.ReadString( 'Water', 'CausticsMapPrefix', 'caust');
    iExt   := aIniFile.ReadString( 'Water', 'CausticsMapExtension', 'dds');
    for iI := 0 to iCount-1 do
      FCausticTextures.Add( Engine.Resources.LoadTexture(iPath + IntToStr(iI) + '.' + iExt ,Engine.Settings.TextureDetail,Engine.Settings.TextureFilter) );

    //create VBO
    FVertexBuffer := TGDGLVertexBuffer.Create();

    //timing
    FLastTime     := Engine.Timing.GetTime()+50;
  except
    on E: Exception do
    begin
      iError := E.Message;
      result := false;
      FWaterLoaded := false;
    end;
  end;
  Engine.Console.Use := True;

  Engine.Console.WriteOkFail(result, iError);
end;

{******************************************************************************}
{* Resize the water reflection texture and buffers                            *}
{******************************************************************************}

procedure TGDWater.Resize();
begin
   case Engine.Settings.WaterDetail of
     WD_LOW    : begin
                   FWidth  := Engine.Settings.Width div 4;
                   FHeight := Engine.Settings.Height div 4;
                 end;

     WD_MEDIUM : begin
                   FWidth  := Engine.Settings.Width div 2;
                   FHeight := Engine.Settings.Height div 2;
                 end;
     WD_HIGH   : begin
                   FWidth  := Engine.Settings.Width;
                   FHeight := Engine.Settings.Height;
                 end;
  end;
  FreeAndNil(FRenderBuffer);
  FreeAndNil(FFrameBuffer);
  FreeAndNil(FReflection);
  FRenderBuffer := TGDGLRenderBuffer.Create(FWidth,FHeight,GL_DEPTH_COMPONENT24);
  FFrameBuffer  := TGDGLFrameBuffer.Create();
  FReflection   := TGDTexture.Create(GL_RGBA, GL_RGBA, FWidth,FHeight);
end;

{******************************************************************************}
{* Clear water                                                                *}
{******************************************************************************}

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
    Engine.Resources.RemoveResource(TGDResource(iTex));
  end;
  FCausticTextures.Clear();
  FCausticCounter := 0;

  for iI := 0 to FWaterTextures.Count-1 do
  begin
    iTex := TGDTexture(FWaterTextures.Items[iI]);
    Engine.Resources.RemoveResource(TGDResource(iTex));
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
  result := Engine.Camera.BoxInView(FBoundingBox);
end;

{******************************************************************************}
{* Update animations                                                          *}
{******************************************************************************}

procedure TGDWater.Update();
begin
  If not(FWaterLoaded) then exit;

  if SDL_TICKS_PASSED(Engine.Timing.GetTime(), FLastTime) then
  begin
    FLastTime := Engine.Timing.GetTime()+50;

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
  FFrameBuffer.AttachRenderBuffer(FRenderBuffer,GL_DEPTH_ATTACHMENT_EXT);
  FFrameBuffer.Status();
  glViewPort(0,0,FWidth,FHeight);
  glClear(GL_DEPTH_BUFFER_BIT or GL_COLOR_BUFFER_BIT);
  If Engine.Camera.Position.Y > FBoundingBox.Max.Y then
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
  If Engine.Camera.Position.Y > FBoundingBox.Max.Y then
  begin
    glCullFace(GL_BACK);
    glPolygonMode(GL_FRONT, GL_FILL);
    glPopMatrix();
  end;
  glDisable(GL_CLIP_PLANE0);
  glViewPort(0,0,Engine.Settings.Width, Engine.Settings.Height);
  FFrameBuffer.UnBind();
end;

{******************************************************************************}
{* Start the water rendering                                                  *}
{******************************************************************************}

procedure TGDWater.StartRendering( aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor );
begin
  if Not(aRenderAttribute = RA_NORMAL) then exit;

  FVertexBuffer.Bind(VL_V_UV);
  if Engine.Modes.RenderWireframe then
  begin
    Engine.Renderer.SetColor(0.3,0.3,1,1);
  end
  else
  begin
    case aRenderFor of
        RF_NORMAL, RF_WATER : begin
                   with Engine.Renderer do
                   begin
                     WaterShader.Bind();
                     WaterShader.SetInt('T_REFLECTION', 0);
                     WaterShader.SetInt('T_DUDVMAP', 1);
                     WaterShader.SetInt('T_CAUSTICMAP', 5);
                     WaterShader.SetInt('I_REFRACTION_UV', RefractionUV);
                     WaterShader.SetInt('I_WAVES_UV', WavesUV);
                     Engine.Renderer.SetJoinedParams(WaterShader);
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

{******************************************************************************}
{* End the water rendering                                                    *}
{******************************************************************************}

procedure TGDWater.EndRendering();
begin
  FVertexBuffer.Unbind();
  glEnable(GL_CULL_FACE);
  glDisable(GL_BLEND);
end;

{******************************************************************************}
{* Check if camera is underwater                                              *}
{******************************************************************************}

function TGDWater.UnderWater(): boolean;
begin
  If ( Engine.Camera.Position.Y < FBoundingBox.Max.Y ) then
    result := true
  else
    result := false;
end;

{******************************************************************************}
{* Add vertex to water vertexlist                                             *}
{******************************************************************************}

function  TGDWater.AddVertex(aV : TGDVertex_V_UV): integer;
begin
  result := FVertices.add(aV);
end;

{******************************************************************************}
{* Update water VBO                                                           *}
{******************************************************************************}

procedure TGDWater.UpdateVBO();
begin
  FVertexBuffer.Bind(VL_NONE);
  FVertexBuffer.Update(FVertices, GL_STATIC_DRAW);
  FVertexBuffer.Unbind()
end;

{******************************************************************************}
{* Bind the caustic texture                                                   *}
{******************************************************************************}

procedure TGDWater.BindCausticTexture();
begin
  If FWaterLoaded then
  begin
    TGDTexture(FCausticTextures.Items[ FCausticCounter ]).BindTexture(GL_TEXTURE6);
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
