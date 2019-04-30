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
unit uMain;

{$MODE Delphi}

{******************************************************************************}
{* This units holds the main game procedures                                  *}
{******************************************************************************}

interface

uses
  uGDConstants,
  SysUtils,
  SDL2,
  uPlayer,
  uGDEngine,
  uGDSound,
  uGDGUI;

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
  AmbientBuffer    : TGDSoundResource;
  AmbientSource    : integer;
  UnderWaterBuffer : TGDSoundResource;
  UnderWaterSource : integer;
  MusicBuffer      : TGDSoundResource;
  MusicSource      : integer;

  //Screens
  IntroScreen    : TGDScreen;

procedure InitGame();
procedure ClearGame();
procedure GameLoop();

implementation

{******************************************************************************}
{* Exit thee engine                                                           *}
{******************************************************************************}

procedure ExitCallback();
begin
  GDEngine.Done := true;
end;

{******************************************************************************}
{* Move the player forward                                                    *}
{******************************************************************************}

procedure PlayerForward();
begin
  if not(IntroScreen.Visible) then
    Player.MoveForward();
end;

{******************************************************************************}
{* Move the player backwards                                                  *}
{******************************************************************************}

procedure PlayerBackward();
begin
  if not(IntroScreen.Visible) then
    Player.MoveBackWard();
end;

{******************************************************************************}
{* Move the player right                                                      *}
{******************************************************************************}

procedure PlayerRight();
begin
  if not(IntroScreen.Visible) then
    Player.MoveRight();
end;

{******************************************************************************}
{* Move the player left                                                       *}
{******************************************************************************}

procedure PlayerLeft();
begin
  if not(IntroScreen.Visible) then
    Player.MoveLeft();
end;

{******************************************************************************}
{* Set the walkspeed                                                          *}
{******************************************************************************}

procedure SetWalk();
begin
  if not(IntroScreen.Visible) then
    Player.Walk();
end;

{******************************************************************************}
{* Set the runspeed                                                           *}
{******************************************************************************}

procedure SetRun();
begin
  if not(IntroScreen.Visible) then
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
  GDConsole.ExecuteCommand ('RStats ' + BoolToStr(Stats));
end;

{******************************************************************************}
{* Toggle wireframe rendering                                                 *}
{******************************************************************************}

procedure ToggleWireFrame();
begin
  WireFrame := not(WireFrame);
  GDConsole.ExecuteCommand ('RTris ' + BoolToStr(WireFrame));
end;

{******************************************************************************}
{* Toggle the octree nodes                                                    *}
{******************************************************************************}

procedure ToggleOctreeNodes(); stdcall;
begin
  TreeNodes := not(TreeNodes);
  GDConsole.ExecuteCommand ('RNodes ' + BoolToStr(TreeNodes));
end;

{******************************************************************************}
{* Toggle the OBJ boxes                                                       *}
{******************************************************************************}

procedure ToggleOBJBoxes(); stdcall;
begin
  ObjBoxes := not(ObjBoxes);
  GDConsole.ExecuteCommand ('RAABB ' + BoolToStr(ObjBoxes));
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
  IntroScreen.Visible := not(IntroScreen.Visible);
  GDInput.MouseLook := not(IntroScreen.Visible);
  GDGUI.MouseCursor.Visible := IntroScreen.Visible;
end;

{******************************************************************************}
{* Main gameloop                                                              *}
{******************************************************************************}

procedure GameLoop();
begin
  //do soms sound stuff
  if Player.PlayerUnderWater() then
  begin
    GDSound.Resume(UnderWaterSource);
    GDSound.Pause( AmbientSource );
  end
  else
  begin
    GDSound.Pause( UnderWaterSource );
    GDSound.Resume(AmbientSource);
  end;

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
  GDTiming.Start();
  GDWindow.SetTitle('Demo');
  GDGUI.LoadingScreen.Start('Loading game...', 3);
  GDConsole.Write('.....Initializing game resources');

  //sounds
  AmbientBuffer    := GDSound.Load( 'Sounds/ambient.wav');
  UnderWaterBuffer := GDSound.Load( 'Sounds/underwater.wav');
  MusicBuffer      := GDSound.Load( 'Sounds/music.mp3');
  AmbientSource    := GDSound.Play(AmbientBuffer, true);
  GDSound.Pause(AmbientSource);
  UnderWaterSource := GDSound.Play(UnderWaterBuffer, true);
  GDSound.Pause(UnderWaterSource);
  GDGUI.LoadingScreen.Update();

  //intro
  IntroScreen := GDGUI.InitScreen('Ini/Intro.ini');
  GDGUI.LoadingScreen.Update();

  //player
  Player  := TPlayer.Create();
  GDGUI.LoadingScreen.Update();
  GDTiming.Stop();
  GDConsole.Write( '.....Done initializing game resources (' +  GDTiming.TimeInSeconds() + ' Sec)' );

  //map
  GDMap.Load( 'Maps/Demo/map.ini' );

  //final settings.
  IntroScreen.Visible := true;
  GDInput.EnableInput := true;
  GDInput.MouseLook :=  false;
  GDGUI.MouseCursor.Visible := true;

  //input functions
  GDInput.AddAction(IT_SINGLE,SDL_SCANCODE_ESCAPE,@ExitCallback, false );
  GDInput.AddAction(IT_DIRECT,SDL_SCANCODE_W,@PlayerForward, true );
  GDInput.AddAction(IT_DIRECT,SDL_SCANCODE_S,@PlayerBackward, true );
  GDInput.AddAction(IT_DIRECT,SDL_SCANCODE_A,@PlayerLeft, true );
  GDInput.AddAction(IT_DIRECT,SDL_SCANCODE_D,@PlayerRight, true );
  GDInput.AddAction(IT_DOWN, SDL_SCANCODE_LSHIFT,@SetRun, true );
  GDInput.AddAction(IT_UP,SDL_SCANCODE_LSHIFT,@SetWalk, true );
  GDInput.AddAction(IT_SINGLE,SDL_SCANCODE_F1,@ToggleStats, false  );
  GDInput.AddAction(IT_SINGLE,SDL_SCANCODE_F2,@ToggleWireFrame, false  );
  GDInput.AddAction(IT_SINGLE,SDL_SCANCODE_F3,@ToggleOctreeNodes, false  );
  GDInput.AddAction(IT_SINGLE,SDL_SCANCODE_F4,@ToggleOBJBoxes, false  );
  GDInput.AddAction(IT_SINGLE,SDL_SCANCODE_F5,@ToggleClipping, false  );
  GDInput.AddAction(IT_SINGLE,SDL_SCANCODE_P,@ToggleIntroText, true  );

  MusicSource := GDSound.Play( MusicBuffer, true );
end;

{******************************************************************************}
{* Clear the game                                                             *}
{******************************************************************************}

procedure ClearGame();
begin
  FreeAndNil(Player);
  GDSound.Stop(AmbientSource);
  GDSound.Remove(AmbientBuffer);
  GDSound.Stop(UnderWaterSource);
  GDSound.Remove(UnderWaterBuffer);
  GDSound.Stop(MusicSource);
  GDSound.Remove(MusicBuffer);
end;

end.
