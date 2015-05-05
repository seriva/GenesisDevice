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
unit GDGUI;

{$MODE Delphi}

interface

{******************************************************************************}
{* This contains the main GUI class for ingame/inengine UI:                   *}
{* - Loadingscreen                                                            *}
{* - Mousecursor                                                              *}
{* - Font                                                                     *}
{******************************************************************************}

uses
  LCLIntf,
  LCLType,
  Windows,
  Classes,
  IniFiles,
  SysUtils,
  dglOpenGL,
  GDTexture,
  GDConstants,
  GDTypes;

const
  FONT_TEXHEIGHT = 512;
  FONT_HEIGHT    = 64;
  FONT_CHARCOORDS : array[0..93,0..3] of Word = (
      (0,0,25,64),        //!
      (25,0,54,64),       //"
      (54,0,107,64),      //#
      (107,0,148,64),     //$
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
{* Font class                                                                 *}
{******************************************************************************}

  TGDFont = class
  private
    FTexture : TGDTexture;
    FColor   : TGDColor;
  public
    property Color : TGDColor read FColor write FColor;

    constructor Create();
    destructor  Destroy(); override;
    function    InitFont( aTexture : string) : boolean;
    procedure   Clear();
    procedure   Render( aLeft, aTop, aScale : Double; aString : string);
    function    TextWidth(const str : String; const scale : Single = 1): Integer;
  end;

{******************************************************************************}
{* Mousecursor class                                                          *}
{******************************************************************************}

  TGDMouseCursor = class
  private
    FCursorTexture : TGDTexture;
    FShowMouse     : Boolean;
    FPosition      : TPoint;
    FCursorSize    : Integer;
  public
    property ShowMouse : boolean read FShowMouse write FShowMouse;
    property Position : TPoint read FPosition write FPosition;

    constructor Create();
    destructor  Destroy(); override;

    procedure InitMouse(aFileName: String; aCursorSize : Integer );
    procedure Clear();

    procedure Render();
  end;

{******************************************************************************}
{* Loadingscreen input record                                                 *}
{******************************************************************************}

  TGDLoadingInput = record
    X : integer;
    Y : integer;
    BarR, BarG, BarB, BarA : Double;
    BackGroundR, BackGroundG, BackGroundB, BackGroundA : Double;
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
    FBackgroundColor : TGDColor;
    FProcesName      : String;
    FBarOnly         : boolean;

    Procedure   Render();
  public
    property Max : integer read FMax write FMax;
    property Position : integer read FPosition write FPosition;
    property Use : boolean read FUse write FUse;

    constructor Create();
    destructor  Destroy();override;

    procedure   InitLoadingScreen( aInput : TGDLoadingInput );
    procedure   Clear();
    procedure   SetupForUse( aProcesName : String; aMax : integer );
    procedure   UpdateBar();
  end;

{******************************************************************************}
{* Main GUIManager class                                                      *}
{******************************************************************************}

  TGDGUI = class
  private
    FFont          : TGDFont;
    FMouseCursor   : TGDMouseCursor;
    FLoadingScreen : TGDLoadingScreen;

    FFontColor     : TGDColor;
    FOutlineColor  : TGDColor;
    FFillColor     : TGDColor;
  public
    property Font : TGDFont read FFont;
    property MouseCursor : TGDMouseCursor read FMouseCursor;
    property LoadingScreen : TGDLoadingScreen read FLoadingScreen;

    property FontColor : TGDColor read FFontColor;
    property OutlineColor : TGDColor read FOutlineColor;
    property FillColor : TGDColor read FFillColor;

    constructor Create();
    destructor  Destroy(); override;

    procedure InitGUI();
    procedure Clear();
  end;

var
  GUI : TGDGUI;

implementation

uses
  GDRenderer,
  GDSettings;

{******************************************************************************}
{* Create the font class                                                      *}
{******************************************************************************}

constructor TGDFont.Create();
begin
  FColor.White();
  FTexture := TGDTexture.Create();
end;

{******************************************************************************}
{* Destroy the font class                                                     *}
{******************************************************************************}

destructor TGDFont.Destroy();
begin
  Clear();
  FreeAndNil(FTexture);
  inherited;
end;


{******************************************************************************}
{* Init the font                                                              *}
{******************************************************************************}

function TGDFont.InitFont( aTexture : string) : boolean;
begin
  result := FTexture.InitTexture(aTexture, TD_HIGH, TF_TRILINEAR);
end;

{******************************************************************************}
{* Clear the font                                                             *}
{******************************************************************************}

Procedure TGDFont.Clear();
begin
  FTexture.Clear();
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

procedure RenderTexturedQuad(x,y,width,height: Single;
                               u1 : Single=0; v1 : Single=0;
                               u2 : Single=1; v2 : Single=0;
                               u3 : Single=1; v3 : Single=1;
                               u4 : Single=0; v4 : Single=1);
begin
  glBegin(GL_QUADS);
    glTexCoord2f(u1, v1); glVertex2f( x,       y);
    glTexCoord2f(u2, v2); glVertex2f( x+width, y);
    glTexCoord2f(u3, v3); glVertex2f( x+width, y+height);
    glTexCoord2f(u4, v4); glVertex2f( x,       y+height);
  glEnd();
end;

begin
  FTexture.BindTexture(GL_TEXTURE0);
  glColor4fv(FColor.ArrayPointer());
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

constructor TGDMouseCursor.Create();
begin
  FCursorTexture := TGDTexture.Create();
  Clear();
end;

{******************************************************************************}
{* Destroy the mousecursor class                                              *}
{******************************************************************************}

destructor  TGDMouseCursor.Destroy();
begin
  FreeAndNil(FCursorTexture);
  Clear();
end;

{******************************************************************************}
{* Init the mousecursor                                                       *}
{******************************************************************************}

procedure TGDMouseCursor.InitMouse(aFileName: String; aCursorSize : Integer );
begin
  Clear();
  FShowMouse := false;
  FCursorSize := aCursorSize;
  FCursorTexture.InitTexture( aFileName, TD_HIGH, TF_TRILINEAR );
end;

{******************************************************************************}
{* Clear the mousecursor                                                      *}
{******************************************************************************}

procedure TGDMouseCursor.Clear();
begin
  FCursorTexture.Clear();
  FShowMouse := false;
end;

{******************************************************************************}
{* Render the mousecursor                                                     *}
{******************************************************************************}

procedure TGDMouseCursor.Render();
var
 iCurPos     : TPoint;

procedure RenderQuad(aX, aY, aWidth, aHeight : Integer);
begin
  glBegin(GL_QUADS);
    glTexCoord2f(0.0, 1.0); glVertex2f(aX,  aY-aHeight);
    glTexCoord2f(1.0, 1.0); glVertex2f(aX+aWidth, aY-aHeight);
    glTexCoord2f(1.0, 0.0); glVertex2f(aX+aWidth, aY);
    glTexCoord2f(0.0, 0.0); glVertex2f(aX, aY);
  glEnd();
end;

{******************************************************************************}
{* Calculate the mouse screenposition                                         *}
{******************************************************************************}

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
  GetCursorPos(iCurPos);
  CalculateScreenPosition(iCurPos.X-Settings.Left, iCurPos.Y-Settings.Top);

  If ShowMouse then
  begin
    FCursorTexture.BindTexture( GL_TEXTURE0 );
    
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
    glColor4f(1,1,1,1);
    RenderQuad(FPosition.x,FPosition.y,FCursorSize,FCursorSize);
    glDisable(GL_BLEND);
  end;
end;

{******************************************************************************}
{* Create the loadingscreen class                                             *}
{******************************************************************************}

constructor TGDLoadingScreen.Create();
begin
  FUse := true;
  FBarOnly := true;
  FMax  := 100;
  FPosition := 0;
  FX := 0;
  FY := 0;
end;

{******************************************************************************}
{* Destroy the loadingscreen class                                            *}
{******************************************************************************}

destructor TGDLoadingScreen.Destroy();
begin
  inherited;
end;

{******************************************************************************}
{* Init the loadingscreen                                                     *}
{******************************************************************************}

procedure TGDLoadingScreen.InitLoadingScreen( aInput : TGDLoadingInput );
begin
  Clear();
  FX := aInput.X;
  FY := aInput.Y;
  FBarOnly := false;
  FBarColor.Reset( aInput.BarR,  aInput.BarG,  aInput.BarB,  aInput.BarA);
  FBackgroundColor.Reset( aInput.BackGroundR,  aInput.BackGroundG, aInput.BackGroundB, aInput.BackGroundA);
end;

{******************************************************************************}
{* Clear the loadingscreen                                                    *}
{******************************************************************************}

procedure TGDLoadingScreen.Clear();
begin
  FBarColor.Reset(1,1,1,1);
  FBackgroundColor.Reset(1,1,1,1);
  FUse := true;
  FBarOnly := false;
  FMax  := 100;
  FPosition := 0;
  FX := 0;
  FY := 0;
end;

{******************************************************************************}
{* Update the loadingscreen                                                   *}
{******************************************************************************}

procedure TGDLoadingScreen.UpdateBar();
begin
  If FUse = false then exit;
  FPosition := FPosition + 1;
  Render();
end;

{******************************************************************************}
{* Setup the loadingscreen                                                    *}
{******************************************************************************}

procedure TGDLoadingScreen.SetupForUse( aProcesName : String; aMax : integer );
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
  Renderer.MakeCurrent();
  Renderer.StartFrame();
  Renderer.SwitchToOrtho();

  Renderer.RenderState( RS_COLOR );
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  glColor4fv( FBackgroundColor.ArrayPointer() );
  glBegin(GL_QUADS);
    glVertex2f(0, 0);
    glVertex2f(R_HUDWIDTH, 0);
    glVertex2f(R_HUDWIDTH, R_HUDHEIGHT);
    glVertex2f(0, R_HUDHEIGHT);
  glEnd();

  glPushMatrix();
  glTranslatef(FX, FY, 0);

  If Not(FBarOnly) then
  begin
    glColor4fv( GUI.FillColor.ArrayPointer );
    glBegin(GL_QUADS);
      glVertex2f( 600 ,0  );
      glVertex2f( 600 ,100 );
      glVertex2f( 0   ,100 );
      glVertex2f( 0   ,0  );
    glEnd;
  end;

  iProgress := 580/FMax;
  iProgress := iProgress * FPosition;
  glColor4fv(FBarColor.ArrayPointer);
  glBegin(GL_QUADS);
    glVertex2f(10 + iProgress ,10 );
    glVertex2f(10 + iProgress ,60 );
    glVertex2f(10 ,60 );
    glVertex2f(10 ,10 );
  glEnd;

  glColor4fv( GUI.OutLineColor.ArrayPointer );
  If Not(FBarOnly) then
  begin
    glBegin(GL_LINE_LOOP);
      glVertex2f(600 ,0  );
      glVertex2f(600 ,100 );
      glVertex2f(0   ,100 );
      glVertex2f(0   ,0  );
    glEnd;
  end;
  glBegin(GL_LINE_LOOP);
    glVertex2f(590 ,10 );
    glVertex2f(590 ,60 );
    glVertex2f(10  ,60 );
    glVertex2f(10  ,10 );
  glEnd;

  glPopMatrix();
  iPercent := (FPosition * 100) / FMax;

  If Not(FBarOnly) then
  begin
    Renderer.RenderState( RS_TEXTS );
    GUI.Font.Color := GUI.FontColor.Copy();
    GUI.Font.Render(7+FX,65+FY,0.5,FProcesName);
    GUI.Font.Render(270+FX,16+FY,0.5,IntToStr(round(iPercent)) + '%');
  end;
  glDisable(GL_BLEND);

  Renderer.SwitchToPerspective();
  Renderer.EndFrame();
end;

{******************************************************************************}
{* Create the GUI class                                                       *}
{******************************************************************************}

constructor TGDGUI.Create();
begin
  FFont          := TGDFont.Create();
  FMouseCursor   := TGDMouseCursor.Create();
  FLoadingScreen := TGDLoadingScreen.Create();
end;

{******************************************************************************}
{* Destroy the GUI class                                                      *}
{******************************************************************************}

destructor  TGDGUI.Destroy();
begin
  FreeAndNil(FFont);
  FreeAndNil(FMouseCursor);
  FreeAndNil(FLoadingScreen);
end;

{******************************************************************************}
{* Clear the GUI class                                                        *}
{******************************************************************************}

procedure TGDGUI.InitGUI();
var
  iIniFile      : TIniFile;
  iLoadingInput : TGDLoadingInput;
begin
  ShowCursor(false);
  iIniFile := TIniFile.Create( FP_INITS + GUI_INI );

  //Default colors
  FFontColor.R := iIniFile.ReadFloat('DefaultColors', 'FontR', 1);
  FFontColor.G := iIniFile.ReadFloat('DefaultColors', 'FontG', 1);
  FFontColor.B := iIniFile.ReadFloat('DefaultColors', 'FontB', 1);
  FFontColor.A := iIniFile.ReadFloat('DefaultColors', 'FontA', 1);
  FOutlineColor.R := iIniFile.ReadFloat('DefaultColors', 'OutlineR', 1);
  FOutlineColor.G := iIniFile.ReadFloat('DefaultColors', 'OutlineG', 1);
  FOutlineColor.B := iIniFile.ReadFloat('DefaultColors', 'OutlineB', 1);
  FOutlineColor.A := iIniFile.ReadFloat('DefaultColors', 'OutlineA', 1);
  FFillColor.R := iIniFile.ReadFloat('DefaultColors', 'FillR', 1);
  FFillColor.G := iIniFile.ReadFloat('DefaultColors', 'FillG', 1);
  FFillColor.B := iIniFile.ReadFloat('DefaultColors', 'FillB', 1);
  FFillColor.A := iIniFile.ReadFloat('DefaultColors', 'FillA', 1);

  //Font
  Font.InitFont( iIniFile.ReadString('Font', 'Texture', 'console.fnt') );

  //Mouse
  GUI.MouseCursor.InitMouse( iIniFile.ReadString('Mouse', 'Texture', 'mouse.dds'),
                             iIniFile.ReadInteger('Mouse', 'Size', 40) );

  //Loading
  iLoadingInput.X := iIniFile.ReadInteger('Loading', 'X', 1); ;
  iLoadingInput.Y := iIniFile.ReadInteger('Loading', 'Y', 1); ;
  iLoadingInput.BarR := iIniFile.ReadFloat('Loading', 'BarR', 1);
  iLoadingInput.BarG := iIniFile.ReadFloat('Loading', 'BarG', 1);
  iLoadingInput.BarB := iIniFile.ReadFloat('Loading', 'BarB', 1);
  iLoadingInput.BarA := iIniFile.ReadFloat('Loading', 'BarA', 1);
  iLoadingInput.BackGroundR := iIniFile.ReadFloat('Loading', 'BackGroundR', 1);
  iLoadingInput.BackGroundG := iIniFile.ReadFloat('Loading', 'BackGroundG', 1);
  iLoadingInput.BackGroundB := iIniFile.ReadFloat('Loading', 'BackGroundB', 1);
  iLoadingInput.BackGroundA := iIniFile.ReadFloat('Loading', 'BackGroundA', 1);
  GUI.LoadingScreen.InitLoadingScreen( iLoadingInput );

  FreeAndNil(iIniFile);
end;

{******************************************************************************}
{* Clear the GUI class                                                        *}
{******************************************************************************}

procedure TGDGUI.Clear();
begin
  ShowCursor(true);
  FFont.Clear();
  FMouseCursor.Clear();
  FLoadingScreen.Clear();
end;

end.
