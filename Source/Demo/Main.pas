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
unit Main;

{$MODE Delphi}

{******************************************************************************}
{* This units holds the main game procedures                                  *}
{******************************************************************************}

interface

uses
  SysUtils,
  Intro,
  Player,
  Sounds,
  GDInterface;

var
  //main
  Stats      : boolean;
  WireFrame  : boolean;
  TreeNodes  : boolean;
  ObjBoxes   : boolean;
  Clip       : boolean;
  OCPlanes   : boolean;

  //main classes
  Intro      : TIntro;
  Player     : TPlayer;
  Sounds     : TSounds;

procedure InitGame();
procedure ClearGame();

implementation

uses
  Configuration;

{******************************************************************************}
{* Init the main                                                              *}
{******************************************************************************}

procedure InitGame();
begin
  //initialize the resources
  Stats      := false;
  WireFrame  := false;
  TreeNodes  := false;
  ObjBoxes   := false;
  Clip       := true;
  OCPlanes   := false;

  //initialize the gamerecources
  gdTimerStart();
  gdGUILoadingScreenSetup( 'Loading game...', 3);
  gdLogAddNewLine('......Initializing game resources');
  //intro
  Intro   := TIntro.Create();
  Intro.InitializeIntro('Inits\Intro.ini');
  gdGUILoadingScreenUpdate();
  //sounds
  Sounds  := TSounds.Create();
  Sounds.InitializeSounds( 'Inits\Sounds.ini' );
  gdGUILoadingScreenUpdate();
  //player
  Player  := TPlayer.Create();
  Player.InitializePlayer('');
  gdGUILoadingScreenUpdate();
  gdTimerStop();
  gdLogAddNewLine( PChar('......Done initializing game resources (' + gdTimerGetInSeconds() + ' Sec)') );

  //load the gameworld
  gdMapLoad( PChar( 'Maps\' + ConfigurationForm.Map + '\map.ini') );

  gdInputSystemEnable(true);
end;

{******************************************************************************}
{* Clear the game                                                             *}
{******************************************************************************}

procedure ClearGame();
begin
  FreeAndNil(Intro);
  FreeAndNil(Player);
  FreeAndNil(Sounds);
end;

end.
