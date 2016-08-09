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
unit GDGUI;

{$MODE objfpc}

interface

{******************************************************************************}
{* This contains the main GUI class for ingame/inengine UI:                   *}
{* - Loadingscreen                                                            *}
{* - Mousecursor                                                              *}
{* - Font                                                                     *}
{******************************************************************************}

uses
  fgl,
  sdl2,
  LCLIntf,
  LCLType,
  Classes,
  IniFiles,
  SysUtils,
  dglOpenGL,
  GDTexture,
  GDStringParsing,
  GDConstants,
  GDWindow,
  GDTypes;

const
  FONT_TEXHEIGHT = 512;
  FONT_HEIGHT    = 64;
  FONT_CHARCOORDS : array[0..93,0..3] of Word = (
      (0,0,25,64),        //!
      (25,0,54,64),       //"
      (54,0,107,64),      //#
      (107,0,148,64),     //dollar
      (148,0,217,64),     //%
      (217,0,263,64),     //&
      (263,0,280,64),     //'
      (280,0,309,64),     //(
      (309,0,338,64),     //)
      (338,0,379,64),     //*
      (379,0,432,64),     //+
      (432,0,455,64),     //,
      (455,0,484,64),     //-
      (0,64,21,128),      //.
      (23,64,52,128),     ///
      (52,64,93,128),     //0
      (93,64,133,128),    //1
      (133,64,174,128),   //2
      (174,64,215,128),   //3
      (215,64,256,128),   //4
      (256,64,296,128),   //5
      (296,64,337,128),   //6
      (337,64,378,128),   //7
      (378,64,419,128),   //8
      (419,64,459,128),   //9
      (459,64,488,128),   //:
      (0,128,29,192),     //;
      (29,128,81,192),    //<
      (81,128,134,192),   //=
      (134,128,186,192),  //>
      (186,128,221,192),  //?
      (221,128,285,192),  //@
      (285,128,329,192),  //A
      (329,128,373,192),  //B
      (373,128,418,192),  //C
      (418,128,467,192),  //D
      (0,192,40,256),     //E
      (40,192,77,256),    //F
      (77,192,127,256),   //G
      (127,192,175,256),  //H
      (175,192,202,256),  //I
      (202,192,231,256),  //J
      (231,192,275,256),  //K
      (275,192,311,256),  //L
      (311,192,365,256),  //M
      (365,192,413,256),  //N
      (413,192,463,256),  //O
      (1,256,38,320),     //P
      (38,256,89,320),    //Q
      (89,256,133,320),   //R
      (133,256,176,320),  //S
      (177,256,216,320),  //T
      (217,256,263,320),  //U
      (263,256,307,320),  //V
      (307,256,370,320),  //W
      (370,256,414,320),  //X
      (414,256,453,320),  //Y
      (453,256,497,320),  //Z
      (0,320,29,384),     //[
      (29,320,58,384),    //"\"
      (59,320,87,384),    //]
      (87,320,139,384),   //^
      (139,320,180,384),  //_
      (180,320,221,384),  //`
      (221,320,259,384),  //a
      (259,320,299,384),  //b
      (299,320,332,384),  //c
      (332,320,372,384),  //d
      (372,320,411,384),  //e
      (411,320,433,384),  //f
      (435,320,473,384),  //g
      (0,384,40,448),     //h
      (40,384,56,448),    //i
      (58,384,80,448),    //j
      (80,384,118,448),   //k
      (118,384,135,448),  //l
      (135,384,197,448),  //m
      (197,384,238,448),  //n
      (238,384,277,448),  //o
      (277,384,317,448),  //p
      (317,384,356,448),  //q
      (357,384,384,448),  //r
      (385,384,417,448),  //s
      (417,384,442,448),  //t
      (443,384,483,448),  //u
      (0,448,38,512),     //v
      (38,448,90,512),    //w
      (90,448,128,512),   //x
      (128,448,166,512),  //y
      (166,448,200,512),  //z
      (200,448,241,512),  //{
      (241,448,270,512),  //|
      (270,448,310,512),  //}
      (310,448,363,512)   //~
  );

type

{******************************************************************************}
{* GUI component classes                                                      *}
{******************************************************************************}

  TGDComponent = class
  private
    FDepth : Integer;
    FX : Integer;
    FY : Integer;
  public
    property Depth : Integer read FDepth write FDepth;
    property X : Integer read FX write FX;
    property Y : Integer read FY write FY;

    procedure Render(); virtual;
  end;
  TGDComponentList = specialize TFPGObjectList<TGDComponent>;

  TGDPanel = class (TGDComponent)
  private
    FWidth : Integer;
    FHeight : Integer;
    FTexture : TGDTexture;
  public
    constructor Create(aIniFile : TIniFile; aSection : String);
    destructor  Destroy(); override;
    procedure   Render();  override;
  end;

  TGDLabel = class (TGDComponent)
  private
    FScale : Single;
    FText : String;
    FColor : TGDColor;
  public
    constructor Create(aIniFile : TIniFile; aSection : String);
    destructor  Destroy(); override;
    procedure   Render();  override;
  end;

  TGDScreen = class
  private
    FVisible : Boolean;
    FComponents : TGDComponentList;
  public
    property Visible : Boolean read FVisible write FVisible;

    constructor Create(aFileName : String);
    destructor  Destroy(); override;
    procedure   Render();
  end;
  TGDScreenList = specialize TFPGObjectList<TGDScreen>;

{******************************************************************************}
{* Font class                                                                 *}
{******************************************************************************}

  TGDFont = class
  private
    FTexture : TGDTexture;
    FColor   : TGDColor;
  public
    property Color : TGDColor read FColor write FColor;

    constructor Create(aTexture : string);
    destructor  Destroy(); override;
    procedure   Render( aLeft, aTop, aScale : Double; aString : string);
    function    TextWidth(const str : String; const scale : Single = 1): Integer;
  end;

{******************************************************************************}
{* Mousecursor class                                                          *}
{******************************************************************************}

  TGDMouseCursor = class
  private
    FCursorTexture : TGDTexture;
    FVisible     : Boolean;
    FPosition      : TPoint;
    FCursorSize    : Integer;
  public
    property Visible : boolean read FVisible write FVisible;
    property Position : TPoint read FPosition write FPosition;

    constructor Create(aFileName: String; aCursorSize : Integer );
    destructor  Destroy(); override;
    procedure Render();
  end;

{******************************************************************************}
{* Loadingscreen class                                                        *}
{******************************************************************************}

  TGDLoadingScreen = class
  private
    FUse             : boolean;
    FX, FY           : Integer;
    FMax             : integer;
    FPosition        : integer;
    FBarColor        : TGDColor;
    FProcesName      : String;
    FBarOnly         : boolean;

    Procedure   Render();
  public
    property Max : integer read FMax write FMax;
    property Position : integer read FPosition write FPosition;
    property Use : boolean read FUse write FUse;

    constructor Create(aIniFile : TIniFile);
    destructor  Destroy();override;

    procedure   Start( aProcesName : String; aMax : integer );
    procedure   Update();
  end;

{******************************************************************************}
{* Main GUIManager class                                                      *}
{******************************************************************************}

  TGDGUI = class
  private
    //Base components
    FFont          : TGDFont;
    FMouseCursor   : TGDMouseCursor;
    FLoadingScreen : TGDLoadingScreen;

    //Base colors
    FFontColor     : TGDColor;
    FOutlineColor  : TGDColor;
    FFillColor     : TGDColor;

    //Screens
    FScreens : TGDScreenList;
  public
    property Font : TGDFont read FFont;
    property MouseCursor : TGDMouseCursor read FMouseCursor;
    property LoadingScreen : TGDLoadingScreen read FLoadingScreen;

    property FontColor : TGDColor read FFontColor;
    property OutlineColor : TGDColor read FOutlineColor;
    property FillColor : TGDColor read FFillColor;

    constructor Create();
    destructor  Destroy(); override;

    function  InitScreen(aFileName : String): TGDScreen;
    function  ScreenGetVisible(aScreen : TGDScreen): Boolean;
    procedure ScreenSetVisible(aScreen : TGDScreen; aVisible : Boolean);
    procedure RenderScreens();
  end;

procedure RenderTexturedQuad(x, y, width, height : Integer;
                             u1 : Single=1; v1 : Single=1;
                             u2 : Single=0; v2 : Single=1;
                             u3 : Single=0; v3 : Single=0;
                             u4 : Single=1; v4 : Single=0);
procedure RenderFlatQuad(x, y, width, height : Integer; inside : boolean = true; outline : boolean = true);

implementation

uses
  GDEngine,
  GDResource;

procedure RenderTexturedQuad(x, y, width, height : Integer;
                             u1 : Single=1; v1 : Single=1;
                             u2 : Single=0; v2 : Single=1;
                             u3 : Single=0; v3 : Single=0;
                             u4 : Single=1; v4 : Single=0);
begin
  glBegin(GL_QUADS);
    glTexCoord2f(u1, v1); glVertex2f( x,       y);
    glTexCoord2f(u2, v2); glVertex2f( x+width, y);
    glTexCoord2f(u3, v3); glVertex2f( x+width, y+height);
    glTexCoord2f(u4, v4); glVertex2f( x,       y+height);
  glEnd;
end;


procedure RenderFlatQuad(x, y, width, height : Integer; inside : boolean = true; outline : boolean = true);

procedure SendQuad(mode: GLenum);
begin
  glBegin(mode);
    glVertex2f( x,       y);
    glVertex2f( x+width, y);
    glVertex2f( x+width, y+height);
    glVertex2f( x,       y+height);
  glEnd;
end;

begin
  Engine.Renderer.RenderState( RS_COLOR );
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);
  if inside then
  begin
    Engine.Renderer.SetColor(Engine.GUI.FillColor);
    SendQuad(GL_QUADS);
  end;
  if outline then
  begin
    Engine.Renderer.SetColor(Engine.GUI.OutlineColor);
    SendQuad(GL_LINE_LOOP);
  end;
end;

{******************************************************************************}
{* GUI component classes                                                      *}
{******************************************************************************}

procedure TGDComponent.Render();
begin
  //Do nothing
end;

constructor TGDPanel.Create(aIniFile : TIniFile; aSection : String);
var
  iStr : String;
begin
  Depth := aIniFile.ReadInteger( aSection, 'Depth', 0 );;
  X := aIniFile.ReadInteger( aSection, 'X', 0 );
  Y := aIniFile.ReadInteger( aSection, 'Y', 0 );
  FWidth  := aIniFile.ReadInteger( aSection, 'Width', 0 );
  FHeight := aIniFile.ReadInteger( aSection, 'Height', 0 );
  iStr := aIniFile.ReadString( aSection, 'Texture', '' );
  if iStr <> '' then
    FTexture := Engine.Resources.LoadTexture(iStr, TD_HIGH, TF_TRILINEAR)
  else
    FTexture := nil;
end;

destructor  TGDPanel.Destroy();
begin
   inherited;
   Engine.Resources.RemoveResource(TGDResource(FTexture));
end;

procedure   TGDPanel.Render();
begin
  if FTexture = nil then
    RenderFlatQuad(X, Y, FWidth, FHeight)
  else
  begin
    Engine.Renderer.RenderState(RS_TEXTURE);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
    FTexture.BindTexture( GL_TEXTURE0 );
    Engine.Renderer.SetColor(1,1,1,1);
    RenderTexturedQuad(X, Y, FWidth, FHeight);
    glDisable(GL_BLEND);
  end;
end;

constructor TGDLabel.Create(aIniFile : TIniFile; aSection : String);
begin
  Depth  := aIniFile.ReadInteger( aSection, 'Depth', 0 );
  FScale := aIniFile.ReadFloat( aSection, 'Scale', 1 );
  FText  := aIniFile.ReadString( aSection, 'Text', '' );
  X      := aIniFile.ReadInteger( aSection, 'X', 0 );
  Y      := aIniFile.ReadInteger( aSection, 'Y', 0 );
  FColor := ReadColor(aIniFile, aSection, 'Color');;
end;

destructor  TGDLabel.Destroy();
begin
   inherited;
end;

procedure   TGDLabel.Render();
begin
  Engine.Renderer.RenderState( RS_TEXTS );
  Engine.GUI.Font.Color := FColor.Copy();
  Engine.GUI.Font.Render(X,Y,FScale,FText);
end;

constructor TGDScreen.Create(aFileName : String);
var
  iIniFile : TIniFile;
  iI       : Integer;
begin
  FComponents := TGDComponentList.create();
  FVisible := false;
  iIniFile := TIniFile.Create( aFileName );

  //Panels
  iI := 1;
  while(iIniFile.SectionExists('Panel' + IntToStr(iI))) do
  begin
    FComponents.Add( TGDPanel.Create(iIniFile, 'Panel' + IntToStr(iI)));
    iI := iI + 1;
  end;

  //Labels
  iI := 1;
  while(iIniFile.SectionExists('Label' + IntToStr(iI))) do
  begin
    FComponents.Add(TGDLabel.Create(iIniFile, 'Label' + IntToStr(iI)));
    iI := iI + 1;
  end;

  FreeAndNil(iIniFile);
end;

destructor  TGDScreen.Destroy();
begin
  inherited;
  FreeAndNil(FComponents);
end;

procedure TGDScreen.Render();
var
  iI : Integer;
begin
  for iI := 0 to FComponents.Count - 1 do
    FComponents.Items[iI].Render();
end;

{******************************************************************************}
{* Create the font class                                                      *}
{******************************************************************************}

constructor TGDFont.Create(aTexture : string);
begin
  FColor.White();
  FTexture := Engine.Resources.LoadTexture(aTexture, TD_HIGH, TF_TRILINEAR);
end;

{******************************************************************************}
{* Destroy the font class                                                     *}
{******************************************************************************}

destructor TGDFont.Destroy();
begin
  Engine.Resources.RemoveResource(TGDResource(FTexture));
  inherited;
end;

{******************************************************************************}
{* Text width the font                                                        *}
{******************************************************************************}

function TGDFont.TextWidth(const str : String; const scale : Single = 1): Integer;
var
  i, x, c, inwidth : integer;
begin
  x := 0;
  for i := 1 to length(str) do
  begin
    if(str[i] = ' ') then begin x := x + round((FONT_HEIGHT/2) * scale); continue; end;
    c := Ord(str[i])-33;
    if(c < 0) or (c >= 95) then continue;
    inwidth := round((FONT_CHARCOORDS[c][2] - FONT_CHARCOORDS[c][0]) * scale);
    x := x + inwidth + 1;
  end;
  result := x;
end;

{******************************************************************************}
{* Render a string                                                            *}
{******************************************************************************}

Procedure TGDFont.Render( aLeft, aTop, aScale : Double; aString : string);
var
  i, x, y, c, inwidth, inheight : integer;
  inleft, intop, inright, inbottom : Single;
begin
  FTexture.BindTexture(GL_TEXTURE0);
  Engine.Renderer.SetColor(FColor);
  x := Round(aLeft);
  y := Round(aTop);
  for i := 1 to length(aString) do
  begin
    if(aString[i] = ' ') then begin x := x + round((FONT_HEIGHT/2) * aScale); continue; end;
    c := Ord(aString[i])-33;
    if(c < 0) or (c >= 95) then continue;
    inleft   := FONT_CHARCOORDS[c][0]   / FONT_TEXHEIGHT;
    intop    := ((FONT_CHARCOORDS[c][1+2]) / FONT_TEXHEIGHT);
    inright  := FONT_CHARCOORDS[c][2]   / FONT_TEXHEIGHT;
    inbottom := ((FONT_CHARCOORDS[c][3-2]) / FONT_TEXHEIGHT);
    inwidth  := Round((FONT_CHARCOORDS[c][2] - FONT_CHARCOORDS[c][0]) * aScale);
    inheight := Round((FONT_CHARCOORDS[c][3] - FONT_CHARCOORDS[c][1]) * aScale);
    RenderTexturedQuad(x,y,inwidth,inheight,inleft,intop,inright,intop,inright,inbottom,inleft,inbottom);
    x := x + inwidth + 1;
  end;
end;

{******************************************************************************}
{* Create the mousecursor class                                               *}
{******************************************************************************}

constructor TGDMouseCursor.Create(aFileName: String; aCursorSize : Integer );
begin
  FVisible := false;
  FCursorSize := aCursorSize;
  FCursorTexture := Engine.Resources.LoadTexture(aFileName, TD_HIGH, TF_TRILINEAR);
end;

{******************************************************************************}
{* Destroy the mousecursor class                                              *}
{******************************************************************************}

destructor  TGDMouseCursor.Destroy();
begin
  inherited;
  Engine.Resources.RemoveResource(TGDResource(FCursorTexture));
  FVisible := false;
end;

{******************************************************************************}
{* Render the mousecursor                                                     *}
{******************************************************************************}

procedure TGDMouseCursor.Render();
var
 iCurPos     : TPoint;

procedure CalculateScreenPosition( aX, aY : Integer);
var
  iViewPort   : TGLVectori4;
  iModelView  : TGLMatrixd4;
  iProjection : TGLMatrixd4;
  iOglPoint   : TGLVectord3;
  iWinZ       : Single;
begin
  glGetDoublev( GL_MODELVIEW_MATRIX, @iModelView );
  glGetDoublev( GL_PROJECTION_MATRIX, @iProjection );
  glGetIntegerv( GL_VIEWPORT, @iViewPort );
  if( aY = 0 )then aY := 1;
  glReadPixels(	aX, -aY, 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, @iWinZ );
  gluUnProject(	aX, iViewPort[3]-aY, iWinZ,
		iModelView, iProjection, iViewPort,
		@iOglPoint[0], @iOglPoint[1], @iOglPoint[2]);
  FPosition.X := round(iOglPoint[0]);
  FPosition.Y := round(iOglPoint[1]);
end;

begin
  Engine.Renderer.RenderState( RS_TEXTS );
  SDL_GetMouseState(@iCurPos.x, @iCurPos.y);
  CalculateScreenPosition(iCurPos.X, iCurPos.Y);
  glDisable(GL_BLEND);

  If FVisible or Engine.Console.Show then
  begin
    Engine.Renderer.RenderState(RS_TEXTURE);
    FCursorTexture.BindTexture( GL_TEXTURE0 );
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
    Engine.Renderer.SetColor(1,1,1,1);
    RenderTexturedQuad(FPosition.x,FPosition.y-FCursorSize,FCursorSize,FCursorSize);
    glDisable(GL_BLEND);
  end;
end;

{******************************************************************************}
{* Create the loadingscreen class                                             *}
{******************************************************************************}

constructor TGDLoadingScreen.Create(aIniFile : TIniFile);
begin
  FUse := true;
  FMax  := 100;
  FPosition := 0;
  FX := aIniFile.ReadInteger('Loading', 'X', 1);
  FY := aIniFile.ReadInteger('Loading', 'Y', 1);
  FBarOnly := false;
  FBarColor := ReadColor(aIniFile, 'Loading', 'Bar');
end;

{******************************************************************************}
{* Destroy the loadingscreen class                                            *}
{******************************************************************************}

destructor TGDLoadingScreen.Destroy();
begin
  inherited;
end;

{******************************************************************************}
{* Update the loadingscreen                                                   *}
{******************************************************************************}

procedure TGDLoadingScreen.Update();
begin
  If FUse = false then exit;
  FPosition := FPosition + 1;
  Render();
end;

{******************************************************************************}
{* Setup the loadingscreen                                                    *}
{******************************************************************************}

procedure TGDLoadingScreen.Start( aProcesName : String; aMax : integer );
begin
  If FUse = false then exit;
  FMax := aMax;
  FProcesName := aProcesName;
  FPosition := 0;
  Render();
end;

{******************************************************************************}
{* Render the loadingscreen                                                   *}
{******************************************************************************}

Procedure TGDLoadingScreen.Render();
var
  iProgress : Double;
  iPercent : Double;
begin
  glClearColor(0.0, 0.0, 0.0, 1.0);
  Engine.Window.MakeCurrent();
  Engine.Renderer.ClearFrame();
  Engine.Renderer.SwitchToOrtho();
  glClear(GL_COLOR_BUFFER_BIT);

  Engine.Renderer.RenderState( RS_COLOR );
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);
  glPushMatrix();
  glTranslatef(FX, FY, 0);

  If Not(FBarOnly) then
    RenderFlatQuad(0, 0, 600, 100);

  iProgress := 580/FMax;
  iProgress := iProgress * FPosition;
  Engine.Renderer.SetColor(FBarColor);
  glBegin(GL_QUADS);
    glVertex2f(10 + iProgress ,10 );
    glVertex2f(10 + iProgress ,60 );
    glVertex2f(10 ,60 );
    glVertex2f(10 ,10 );
  glEnd;
  RenderFlatQuad(10, 10, 580, 50, false);

  glPopMatrix();
  iPercent := (FPosition * 100) / FMax;

  If Not(FBarOnly) then
  begin
    Engine.Renderer.RenderState( RS_TEXTS );
    Engine.GUI.Font.Color := Engine.GUI.FontColor.Copy();
    Engine.GUI.Font.Render(7+FX,65+FY,0.5,FProcesName);
    Engine.GUI.Font.Render(270+FX,16+FY,0.5,IntToStr(round(iPercent)) + '%');
  end;
  glDisable(GL_BLEND);

  Engine.Renderer.SwitchToPerspective();
  Engine.Window.Swap();
end;

{******************************************************************************}
{* Create the GUI class                                                       *}
{******************************************************************************}

constructor TGDGUI.Create();
var
  iIniFile      : TIniFile;
begin
  Engine.Console.Use:=false;
  iIniFile := TIniFile.Create( PATH_INITS + GUI_INI );

  FFontColor    := ReadColor(iIniFile,'DefaultColors', 'Font');
  FOutlineColor := ReadColor(iIniFile, 'DefaultColors', 'Outline');
  FFillColor    := ReadColor(iIniFile, 'DefaultColors', 'Fill');
  FFont          := TGDFont.Create(iIniFile.ReadString('Font', 'Texture', 'console.fnt'));
  FMouseCursor   := TGDMouseCursor.Create(iIniFile.ReadString('Mouse', 'Texture', 'mouse.dds'),
                                          iIniFile.ReadInteger('Mouse', 'Size', 40));
  FLoadingScreen := TGDLoadingScreen.Create(iIniFile);
  FScreens       := TGDScreenList.Create();

  FreeAndNil(iIniFile);
  Engine.Console.Use:=true;
end;

{******************************************************************************}
{* Destroy the GUI class                                                      *}
{******************************************************************************}

destructor  TGDGUI.Destroy();
begin
  FreeAndNil(FFont);
  FreeAndNil(FMouseCursor);
  FreeAndNil(FLoadingScreen);
  FreeAndNil(FScreens);
end;

{******************************************************************************}
{* Init screen                                                                *}
{******************************************************************************}

function TGDGUI.InitScreen(aFileName : String): TGDScreen;
begin
  result := TGDScreen.Create(aFileName);
  FScreens.Add(result);
end;

{******************************************************************************}
{* Get screen visible                                                         *}
{******************************************************************************}

function TGDGUI.ScreenGetVisible(aScreen : TGDScreen): Boolean;
begin
  result := aScreen.Visible;
end;

{******************************************************************************}
{* Set screen visible                                                         *}
{******************************************************************************}

procedure TGDGUI.ScreenSetVisible(aScreen : TGDScreen; aVisible : Boolean);
begin
  aScreen.Visible := aVisible;
end;

{******************************************************************************}
{* Set screen visible                                                         *}
{******************************************************************************}

procedure TGDGUI.RenderScreens();
var
  iI : Integer;
  iScreen : TGDScreen;
begin
  for iI := 0 to FScreens.Count - 1 do
  begin
    iScreen := FScreens.Items[iI];
    if iScreen.Visible then
      iScreen.Render();
  end;
end;

end.
