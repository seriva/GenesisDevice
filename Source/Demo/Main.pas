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
unit Main;

{$MODE Delphi}

{******************************************************************************}
{* This units holds the main game procedures                                  *}
{******************************************************************************}

interface

uses
  GDConstants,
  SysUtils,
  LCLIntf,
  LCLType,
  Forms,
  ExtCtrls,
  Controls,
  Player,
  GDInterface;

var
  //main
  Stats      : boolean;
  WireFrame  : boolean;
  TreeNodes  : boolean;
  ObjBoxes   : boolean;
  Clip       : boolean;

  //main classes
  Player     : TPlayer;

  //Sounds
  AmbientId    : pointer;
  UnderWaterId : pointer;
  MusicId      : pointer;

  //Screens
  IntroId    : pointer;

procedure InitGame();
procedure ClearGame();

implementation

uses
  Configuration;

{******************************************************************************}
{* Exit thee engine                                                           *}
{******************************************************************************}

procedure ExitCallback();
begin
  Configuration.ViewPortForm.Close();
end;

{******************************************************************************}
{* Move the player forward                                                    *}
{******************************************************************************}

procedure PlayerForward();
begin
  if not(gdGUIScreenGetVisible(IntroId)) then
    Player.MoveForward();
end;

{******************************************************************************}
{* Move the player backwards                                                  *}
{******************************************************************************}

procedure PlayerBackward();
begin
  if not(gdGUIScreenGetVisible(IntroId)) then
    Player.MoveBackWard();
end;

{******************************************************************************}
{* Move the player right                                                      *}
{******************************************************************************}

procedure PlayerRight();
begin
  if not(gdGUIScreenGetVisible(IntroId)) then
    Player.MoveRight();
end;

{******************************************************************************}
{* Move the player left                                                       *}
{******************************************************************************}

procedure PlayerLeft();
begin
  if not(gdGUIScreenGetVisible(IntroId)) then
    Player.MoveLeft();
end;

{******************************************************************************}
{* Set the walkspeed                                                          *}
{******************************************************************************}

procedure SetWalk();
begin
  if not(gdGUIScreenGetVisible(IntroId)) then
    Player.Walk();
end;

{******************************************************************************}
{* Set the runspeed                                                           *}
{******************************************************************************}

procedure SetRun();
begin
  if not(gdGUIScreenGetVisible(IntroId)) then
    PLayer.Run();
end;

{******************************************************************************}
{* Bool to string                                                             *}
{******************************************************************************}

function BoolToStr(b : boolean): String;
begin
  If b then
    result := '1'
  else
    result := '0';
end;

{******************************************************************************}
{* Toggle the stats                                                           *}
{******************************************************************************}

procedure ToggleStats();
begin
  Stats := not(Stats);
  gdConsoleCommand('RStats ' + BoolToStr(Stats));
end;

{******************************************************************************}
{* Toggle wireframe rendering                                                 *}
{******************************************************************************}

procedure ToggleWireFrame();
begin
  WireFrame := not(WireFrame);
  gdConsoleCommand('RTris ' + BoolToStr(WireFrame));
end;

{******************************************************************************}
{* Toggle the octree nodes                                                    *}
{******************************************************************************}

procedure ToggleOctreeNodes(); stdcall;
begin
  TreeNodes := not(TreeNodes);
  gdConsoleCommand('RNodes ' + BoolToStr(TreeNodes));
end;

{******************************************************************************}
{* Toggle the OBJ boxes                                                       *}
{******************************************************************************}

procedure ToggleOBJBoxes(); stdcall;
begin
  ObjBoxes := not(ObjBoxes);
  gdConsoleCommand('RAABB ' + BoolToStr(ObjBoxes));
end;

{******************************************************************************}
{* Toggle the collision                                                       *}
{******************************************************************************}

procedure ToggleClipping();
begin
  Clip := not(Clip);
end;

{******************************************************************************}
{* Toggle the intro text                                                      *}
{******************************************************************************}

procedure ToggleIntroText();
begin
  gdGUIScreenSetVisible(IntroId, not(gdGUIScreenGetVisible(IntroId)));
  gdInputUseMouseLook(not(gdGUIScreenGetVisible(IntroId)));
  gdGUIMouseCursorShow(gdGUIScreenGetVisible(IntroId));
end;

{******************************************************************************}
{* Beforce render callback                                                    *}
{******************************************************************************}

procedure Loop();
begin
  //do soms sound stuff
  if Player.PlayerUnderWater() then
  begin
    gdSoundResume( UnderWaterID );
    gdSoundPause( AmbientId);
  end
  else
  begin
    gdSoundPause( UnderWaterID);
    gdSoundResume( AmbientID);
  end;

  If (ViewPortForm.Focused = False) or (ViewPortForm.WindowState = wsMinimized) then
    Sleep(50);

  //calculate player collision and response
  Player.DoPlayerCollisionAndPhysics();
end;

{******************************************************************************}
{* Init the main                                                              *}
{******************************************************************************}

procedure InitGame();
begin
  //initialize game vars
  Stats      := false;
  WireFrame  := false;
  TreeNodes  := false;
  ObjBoxes   := false;
  Clip       := true;

  //initialize the gamerecources
  gdTimingStart();
  gdGUILoadingScreenSetup( 'Loading game...', 3);
  gdConsoleLog('......Initializing game resources');

  //sounds
  AmbientId    := gdSoundLoad( 'Sounds\ambient.wav', ST_LOOP);
  UnderWaterId := gdSoundLoad( 'Sounds\underwater.wav', ST_LOOP);
  //MusicId     := gdSoundLoad( 'Sounds\music.mp3', ST_LOOP);
  gdSoundPlay(AmbientId);
  gdSoundPause(AmbientId);
  gdSoundPlay(UnderWaterId);
  gdSoundPause(UnderWaterId);
  //gdSoundPlay( FMusicId );
  gdGUILoadingScreenUpdate();

  //intro
  IntroId := gdGUIInitScreen('Ini\Intro.ini');
  gdGUILoadingScreenUpdate();

  //player
  Player  := TPlayer.Create();
  gdGUILoadingScreenUpdate();
  gdTimingStop();
  gdConsoleLog( PChar('......Done initializing game resources (' + gdTimingInSeconds() + ' Sec)') );

  //map
  gdMapLoad( PChar( 'Maps\' + ConfigurationForm.Map + '\map.ini') );

  //final settings.
  gdGUIScreenSetVisible(IntroId, true);
  gdInputEnable(true);
  gdGUIMouseCursorShow(true);
  gdInputUseMouseLook(false);

  //main callback functions
  gdEngineLoopCallback( @Loop );

  //input funtions
  gdInputRegisterAction(IT_SINGLE,VK_ESCAPE,@ExitCallback, false );
  gdInputRegisterAction(IT_DIRECT,VK_W,@PlayerForward, true );
  gdInputRegisterAction(IT_DIRECT,VK_S,@PlayerBackward, true );
  gdInputRegisterAction(IT_DIRECT,VK_A,@PlayerLeft, true );
  gdInputRegisterAction(IT_DIRECT,VK_D,@PlayerRight, true );
  gdInputRegisterAction(IT_DOWN,VK_LSHIFT,@SetRun, true );
  gdInputRegisterAction(IT_UP,VK_LSHIFT,@SetWalk, true );
  gdInputRegisterAction(IT_SINGLE,VK_F1,@ToggleStats, false  );
  gdInputRegisterAction(IT_SINGLE,VK_F2,@ToggleWireFrame, false  );
  gdInputRegisterAction(IT_SINGLE,VK_F3,@ToggleOctreeNodes, false  );
  gdInputRegisterAction(IT_SINGLE,VK_F4,@ToggleOBJBoxes, false  );
  gdInputRegisterAction(IT_SINGLE,VK_F5,@ToggleClipping, false  );
  gdInputRegisterAction(IT_SINGLE,VK_P,@ToggleIntroText, true  );
end;

{******************************************************************************}
{* Clear the game                                                             *}
{******************************************************************************}

procedure ClearGame();
begin
  FreeAndNil(Player);
  gdSoundRemove(AmbientId);
  gdSoundRemove(UnderWaterId);
  //gdSoundRemove(MusicId);
end;

end.
