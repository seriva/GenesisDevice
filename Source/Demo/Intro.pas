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
  FPaperTexId      := gdTexturesLoad( PChar( iIniFile.ReadString('Intro', 'Paper', '')) );
  gdInputUseMouseLook(False);
  gdGUIMouseCursorShow(true);

  FreeAndNil( iIniFile );
end;

{******************************************************************************}
{* Clear the intro                                                            *}
{******************************************************************************}

procedure TIntro.ClearIntro();
begin
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
    gdInputUseMouseLook(false);
    gdGUIMouseCursorShow(true);
  end
  else
  begin
    gdInputUseMouseLook(true);
    gdGUIMouseCursorShow(false);
  end;
end;

{******************************************************************************}
{* Render the introscreen                                                     *}
{******************************************************************************}

procedure TIntro.Render();
begin
  if Not(FRenderIntroText) then Exit;

  gdRendererState(RS_TEXTURE);
  gdTexturesBind( FPaperTexId, GL_TEXTURE0 );
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);
  glColor4f(1,1,1,1);
  glBegin(GL_QUADS);
    glTexCoord2f(1.0, 1.0); glVertex2f(300, 200);
    glTexCoord2f(0.0, 1.0); glVertex2f(1300, 200);
    glTexCoord2f(0.0, 0.0); glVertex2f(1300, 1000);
    glTexCoord2f(1.0, 0.0); glVertex2f(300,  1000);
  glEnd;
  glDisable(GL_BLEND);

  gdRendererState(RS_TEXTS);
  gdGUITextColor(0,0,0);
  gdGUITextRender( 400, 845-50, 0.45, 'Welcome to the Genesis Device Engine Demo');
  gdGUITextRender( 400, 815-50, 0.45, 'This demo shows the following capabilities:');
  gdGUITextRender( 430, 785-50, 0.45, '- Terrain');
  gdGUITextRender( 430, 755-50, 0.45, '- Sky');
  gdGUITextRender( 430, 725-50, 0.45, '- Water with reflections, distortion and waves');
  gdGUITextRender( 430, 695-50, 0.45, '- Meshes');
  gdGUITextRender( 430, 665-50, 0.45, '- Foliage');
  gdGUITextRender( 430, 635-50, 0.45, '- Bloom and post processing effects');
  gdGUITextRender( 430, 605-50, 0.45, 'Some tips:');
  gdGUITextRender( 430, 575-50, 0.45, '- WASD keys (default) for movement');
  gdGUITextRender( 430, 545-50, 0.45, '- Mouse to look arround');
  gdGUITextRender( 430, 515-50, 0.45, '- Tilde for console, type help for commands');
  gdGUITextRender( 430, 485-50, 0.45, '- F1 to F5 toggle some debug functions');
  gdGUITextRender( 430, 455-50, 0.45, 'Contact information:');
  gdGUITextRender( 430, 425-50, 0.45, 'www.luukvanvenrooij.nl');
  gdGUITextRender( 430, 395-50, 0.45, 'luukvanvenrooij84@gmail.com');

  if CheckInsideButton() then
    gdGUITextColor(1,1,1)
  else
    gdGUITextColor(0,0,0);

  gdGUITextRender( 1150, 250, 0.6, 'Hide');
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
