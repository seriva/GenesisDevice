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
unit Intro;

{$MODE Delphi}

{******************************************************************************}
{* This units holds the intro                                                 *}
{******************************************************************************}

interface

uses
  LCLIntf,
  LCLType,
  Types,
  IniFiles,
  GDInterface,
  GDConstants,
  SysUtils,
  dglOpenGL;

type

{******************************************************************************}
{* The intro class                                                            *}
{******************************************************************************}

  TIntro = class
  private
  public
    FFontId          : pointer;
    FPaperTexId      : pointer;
    FRenderIntroText : Boolean;

    Constructor Create();
    Destructor  Destroy(); override;

    procedure InitializeIntro(aIniPath : String );
    procedure ClearIntro();

    procedure Render();
    procedure ToggleIntroText();
    function  CheckInsideButton() : boolean;
  end;

implementation

uses
  Main;

{******************************************************************************}
{* Create the intro class                                                     *}
{******************************************************************************}

Constructor TIntro.Create();
begin
  inherited;
  ClearIntro();
end;

{******************************************************************************}
{* Destroy the intro class                                                    *}
{******************************************************************************}

Destructor TIntro.Destroy();
begin
  inherited;
  ClearIntro();
end;

{******************************************************************************}
{* Init the intro                                                             *}
{******************************************************************************}

procedure TIntro.InitializeIntro( aIniPath : String );
var
  iIniFile : TIniFile;
begin
  iIniFile := TIniFile.Create( aIniPath );

  FRenderIntroText := true;
  FFontId          := gdFontsLoad( Pchar( iIniFile.ReadString('Intro', 'Font', '')) );
  FPaperTexId      := gdTexturesLoad( PChar( iIniFile.ReadString('Intro', 'Paper', '')) );
  gdInputSystemUseMouseLook(False);
  gdGUIMouseCursorShow(true);

  FreeAndNil( iIniFile );
end;

{******************************************************************************}
{* Clear the intro                                                            *}
{******************************************************************************}

procedure TIntro.ClearIntro();
begin
  gdFontsRemove(FFontId);
  gdTexturesRemove(FPaperTexId);
  FFontId     := nil;
  FPaperTexId := nil;
end;

{******************************************************************************}
{* Toggle the introtext                                                       *}
{******************************************************************************}

procedure TIntro.ToggleIntroText();
begin
  FRenderIntroText := not(FRenderIntroText);
  if FRenderIntroText then
  begin
    gdInputSystemUseMouseLook(false);
    gdGUIMouseCursorShow(true);
  end
  else
  begin
    gdInputSystemUseMouseLook(true);
    gdGUIMouseCursorShow(false);
  end;
end;

{******************************************************************************}
{* Render the introscreen                                                     *}
{******************************************************************************}

procedure TIntro.Render();

procedure RenderQuad(aX, aY, aWidth, aHeight : Integer);
begin
  glBegin(GL_QUADS);
    glTexCoord2f(1.0, 1.0); glVertex2f(aX, aY);
    glTexCoord2f(0.0, 1.0); glVertex2f(aX+aWidth, aY);
    glTexCoord2f(0.0, 0.0); glVertex2f(aX+aWidth, aY+aHeight);
    glTexCoord2f(1.0, 0.0); glVertex2f(aX,  aY+aHeight);
  glEnd;
end;

procedure RenderIntroScreen();
begin
  if Not(FRenderIntroText) then Exit;

  gdRenderSystemSetState(RS_TEXTURE);
  gdTexturesBind( FPaperTexId, TU_1 );
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);
  glColor4f(1,1,1,1);
  RenderQuad(300,200,1000,800);
  glDisable(GL_BLEND);

  gdRenderSystemSetState(RS_TEXTS);
  gdFontsSetColor(FFontId,0,0,0,1);
  gdFontsRenderText( FFontId, 400, 875, 0.9, 'Welcome to the Genesis Device Engine Demo');
  gdFontsRenderText( FFontId, 400, 845, 0.9, 'This demo shows the following capabilities:');
  gdFontsRenderText( FFontId, 430, 815, 0.9, '- Terrain');
  gdFontsRenderText( FFontId, 430, 785, 0.9, '- Sky');
  gdFontsRenderText( FFontId, 430, 755, 0.9, '- Water with reflections, distortion and waves');
  gdFontsRenderText( FFontId, 430, 725, 0.9, '- Meshes');
  gdFontsRenderText( FFontId, 430, 695, 0.9, '- Foliage');
  gdFontsRenderText( FFontId, 430, 665, 0.9, '- Bloom and post processing effects');
  gdFontsRenderText( FFontId, 430, 635, 0.9, 'Some tips:');
  gdFontsRenderText( FFontId, 430, 605, 0.9, '- WASD keys (default) for movement');
  gdFontsRenderText( FFontId, 430, 575, 0.9, '- Mouse to look arround');
  gdFontsRenderText( FFontId, 430, 545, 0.9, '- Tilde for console, type help for commands');
  gdFontsRenderText( FFontId, 430, 485, 0.9, '- F1 to F5 toggle some debug functions');
  gdFontsRenderText( FFontId, 430, 515, 0.9, '- F6 to create a screenshot');
  gdFontsRenderText( FFontId, 430, 455, 0.9, 'Contact information:');
  gdFontsRenderText( FFontId, 430, 425, 0.9, 'www.disapproval.net');
  gdFontsRenderText( FFontId, 430, 395, 0.9, 'luukvanvenrooij84@gmail.com');

  If CheckInsideButton() then
    gdFontsSetColor(FFontId,1,1,1,1)
  else
    gdFontsSetColor(FFontId,0,0,0,1);
    
  gdFontsRenderText( FFontId, 1150, 300, 1.25, 'Hide');
end;

begin
  if GamePause then
  begin
    gdRenderSystemSetState(RS_TEXTS);
    gdFontsSetColor(FFontId,1,1,1,1);
    gdFontsRenderText( FFontId, 710, 645, 2.5, 'Pause');
    exit;
  end;

  RenderIntroScreen();
end;

{******************************************************************************}
{* Check the hide button                                                      *}
{******************************************************************************}

function TIntro.CheckInsideButton() : boolean;
var
  iPoint : TPoint;
begin
  iPoint := gdGUIMouseCursorGetPosition();

  if (iPoint.X > 1150) and (iPoint.X < 1225) and (iPoint.Y < 300) and (iPoint.Y > 250) then
    result := true
  else
    result := false;
end;

end.
