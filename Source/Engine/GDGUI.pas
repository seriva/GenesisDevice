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
{* This is the main GUI class for ingame/inengine forms. For now there are:   *}
{* - Loadingscreen                                                            *}
{* - Mousecursor                                                              *}
{* In feature releases this will be extended to contain:                      *}
{* - Forms                                                                    *}
{* - Buttons                                                                  *}
{* - Labels                                                                   *}
{* - Editboxes                                                                *}
{* - Comboboxes                                                               *}
{* - Sliders                                                                  *}
{* - And more form components                                                 *}
{******************************************************************************}

uses
  LCLIntf, LCLType, LMessages,
  Classes,
  SysUtils,
  dglOpenGL,
  GDTexture,
  GDRenderer,
  GDConstants,
  GDSettings,
  GDTypes,
  GDFont,
  GDInput;

type

{******************************************************************************}
{* Loadingscreen input record                                                 *}
{******************************************************************************}

  TGDLoadingInput = record
    X : integer;
    Y : integer;
    BarR,  BarG, BarB, BarA    : Double;
    LineR, LineG, LineB, LineA : Double;
    BackR, BackG, BackB, BackA : Double;
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
{* Loadingscreen class                                                        *}
{******************************************************************************}

  TGDLoadingScreen = class
  private
    FUse             : boolean;
    FX, FY           : Integer;
    FMax             : integer;
    FPosition        : integer;
    FBarColor        : TGDColor;
    FLineColor       : TGDColor;
    FBackGroundColor : TGDColor;
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

  TGDGUIManager = class
  private
    FMouseCursor : TGDMouseCursor;
    FLoadingScreen : TGDLoadingScreen;
  public
    property MouseCursor : TGDMouseCursor read FMouseCursor write FMouseCursor;
    property LoadingScreen : TGDLoadingScreen read FLoadingScreen write FLoadingScreen;

    constructor Create();
    destructor  Destroy(); override;

    procedure Clear();
  end;

var
  GUIManager : TGDGUIManager;

implementation

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
    FCursorTexture.BindTexture( TU_1 );
    
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
  FBarColor        := TGDColor.Create();
  FLineColor       := TGDColor.Create();
  FBackGroundColor := TGDColor.Create();
end;

{******************************************************************************}
{* Destroy the loadingscreen class                                            *}
{******************************************************************************}

destructor TGDLoadingScreen.Destroy();
begin
  inherited;
  FreeAndNil(FBarColor);
  FreeAndNil(FLineColor);
  FreeAndNil(FBackGroundColor);
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
  FLineColor.Reset( aInput.LineR, aInput.LineG, aInput.LineB, aInput.LineA);
  FBackGroundColor.Reset( aInput.BackR, aInput.BackG, aInput.BackB, aInput.BackA);
end;

{******************************************************************************}
{* Clear the loadingscreen                                                    *}
{******************************************************************************}

procedure TGDLoadingScreen.Clear();
begin
  FBarColor.Reset(1,1,1,1);
  FLineColor.Reset(1,1,1,1);
  FBackGroundColor.Reset(1,1,1,1);
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
  glColor4f(0.2, 0.2, 0.2, 1);
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
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
    glColor4fv( FBackGroundColor.ArrayPointer );
    glBegin(GL_QUADS);
      glVertex2f( 600 ,0  );
      glVertex2f( 600 ,100 );
      glVertex2f( 0   ,100 );
      glVertex2f( 0   ,0  );
    glEnd;
    glDisable(GL_BLEND);
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

  glColor4fv( FLineColor.ArrayPointer );
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
    SystemFont.Render(7+FX,105+FY,1,FProcesName);
    SystemFont.Render(270+FX,56+FY,1,IntToStr(round(iPercent)) + '%');
  end;

  Renderer.SwitchToPerspective();
  Renderer.EndFrame();
end;

{******************************************************************************}
{* Create the fontmanager class                                               *}
{******************************************************************************}

constructor TGDGUIManager.Create();
begin
  FMouseCursor := TGDMouseCursor.Create();
  FLoadingScreen := TGDLoadingScreen.Create();
end;

{******************************************************************************}
{* Destroy the fontmanager class                                              *}
{******************************************************************************}

destructor  TGDGUIManager.Destroy();
begin
  FreeAndNil(FMouseCursor);
  FreeAndNil(FLoadingScreen);
end;

{******************************************************************************}
{* Clear the fontmanager                                                      *}
{******************************************************************************}

procedure TGDGUIManager.Clear();
begin
  FMouseCursor.Clear();
  FLoadingScreen.Clear();
end;

end.
