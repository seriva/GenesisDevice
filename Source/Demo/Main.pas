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
  Configuration,
  GDConstants,
  SysUtils,
  LCLIntf,
  LCLType,
  Forms,
  SDL2,
  ExtCtrls,
  Controls,
  Player,
  GDEngine,
  GDSound,
  GDGUI;

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
  Engine.Done := true;
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
  Engine.Console.ExecuteCommand ('RStats ' + BoolToStr(Stats));
end;

{******************************************************************************}
{* Toggle wireframe rendering                                                 *}
{******************************************************************************}

procedure ToggleWireFrame();
begin
  WireFrame := not(WireFrame);
  Engine.Console.ExecuteCommand ('RTris ' + BoolToStr(WireFrame));
end;

{******************************************************************************}
{* Toggle the octree nodes                                                    *}
{******************************************************************************}

procedure ToggleOctreeNodes(); stdcall;
begin
  TreeNodes := not(TreeNodes);
  Engine.Console.ExecuteCommand ('RNodes ' + BoolToStr(TreeNodes));
end;

{******************************************************************************}
{* Toggle the OBJ boxes                                                       *}
{******************************************************************************}

procedure ToggleOBJBoxes(); stdcall;
begin
  ObjBoxes := not(ObjBoxes);
  Engine.Console.ExecuteCommand ('RAABB ' + BoolToStr(ObjBoxes));
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
  Engine.Input.MouseLook := not(IntroScreen.Visible);
  Engine.GUI.MouseCursor.Visible := IntroScreen.Visible;
end;

{******************************************************************************}
{* Main gameloop                                                              *}
{******************************************************************************}

procedure GameLoop();
begin
  //do soms sound stuff
  if Player.PlayerUnderWater() then
  begin
    Engine.Sound.Resume(UnderWaterSource);
    Engine.Sound.Pause( AmbientSource );
  end
  else
  begin
    Engine.Sound.Pause( UnderWaterSource );
    Engine.Sound.Resume(AmbientSource);
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
  Engine.Timing.Start();
  Engine.Window.SetTitle('Demo');
  Engine.GUI.LoadingScreen.Start('Loading game...', 3);
  Engine.Console.Write('.....Initializing game resources');

  //sounds
  AmbientBuffer    := Engine.Sound.Load( 'Sounds/ambient.wav');
  UnderWaterBuffer := Engine.Sound.Load( 'Sounds/underwater.wav');
  MusicBuffer      := Engine.Sound.Load( 'Sounds/music.mp3');
  AmbientSource := Engine.Sound.Play(AmbientBuffer, true);
  Engine.Sound.Pause(AmbientSource);
  UnderWaterSource := Engine.Sound.Play(UnderWaterBuffer, true);
  Engine.Sound.Pause(UnderWaterSource);
  Engine.GUI.LoadingScreen.Update();

  //intro
  IntroScreen := Engine.GUI.InitScreen('Ini/Intro.ini');
  Engine.GUI.LoadingScreen.Update();

  //player
  Player  := TPlayer.Create();
  Engine.GUI.LoadingScreen.Update();
  Engine.Timing.Stop();
  Engine.Console.Write( '.....Done initializing game resources (' +  Engine.Timing.TimeInSeconds() + ' Sec)' );

  //map
  Engine.Map.Load( 'Maps/Demo/map.ini' );

  //final settings.
  IntroScreen.Visible := true;
  Engine.Input.EnableInput := true;
  Engine.Input.MouseLook :=  false;
  Engine.GUI.MouseCursor.Visible := true;

  //input functions
  Engine.Input.AddAction(IT_SINGLE,SDL_SCANCODE_ESCAPE,@ExitCallback, false );
  Engine.Input.AddAction(IT_DIRECT,SDL_SCANCODE_W,@PlayerForward, true );
  Engine.Input.AddAction(IT_DIRECT,SDL_SCANCODE_S,@PlayerBackward, true );
  Engine.Input.AddAction(IT_DIRECT,SDL_SCANCODE_A,@PlayerLeft, true );
  Engine.Input.AddAction(IT_DIRECT,SDL_SCANCODE_D,@PlayerRight, true );
  Engine.Input.AddAction(IT_DOWN, SDL_SCANCODE_LSHIFT,@SetRun, true );
  Engine.Input.AddAction(IT_UP,SDL_SCANCODE_LSHIFT,@SetWalk, true );
  Engine.Input.AddAction(IT_SINGLE,SDL_SCANCODE_F1,@ToggleStats, false  );
  Engine.Input.AddAction(IT_SINGLE,SDL_SCANCODE_F2,@ToggleWireFrame, false  );
  Engine.Input.AddAction(IT_SINGLE,SDL_SCANCODE_F3,@ToggleOctreeNodes, false  );
  Engine.Input.AddAction(IT_SINGLE,SDL_SCANCODE_F4,@ToggleOBJBoxes, false  );
  Engine.Input.AddAction(IT_SINGLE,SDL_SCANCODE_F5,@ToggleClipping, false  );
  Engine.Input.AddAction(IT_SINGLE,SDL_SCANCODE_P,@ToggleIntroText, true  );

  MusicSource := Engine.Sound.Play( MusicBuffer, true );

  Application.EnableIdleHandler;
end;

{******************************************************************************}
{* Clear the game                                                             *}
{******************************************************************************}

procedure ClearGame();
begin
  Application.DisableIdleHandler;

  FreeAndNil(Player);
  Engine.Sound.Stop(AmbientSource);
  Engine.Sound.Stop(UnderWaterSource);
  Engine.Sound.Stop(MusicSource);
  Engine.Sound.Remove(AmbientBuffer);
  Engine.Sound.Remove(UnderWaterBuffer);
  Engine.Sound.Remove(MusicBuffer);

  ConfigurationForm.SettingsToInterface();
  ConfigurationForm.Visible := true;
  ConfigurationForm.Repaint();
end;

end.
