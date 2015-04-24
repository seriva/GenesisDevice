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
unit CallBack;

{$MODE Delphi}

{******************************************************************************}
{* This units holds various callback functions that are used internaly in the *}
{* engine. These functions basicly are steps between the fixed pipeline of the*}
{* engine to control the game or application build upon it.                   *}
{******************************************************************************}

interface

uses
  SysUtils,
  LCLIntf,
  LCLType,
  Forms,
  ExtCtrls,
  Controls,
  FileUtil,
  Configuration,
  Main,
  GDInterface,
  GDConstants;

procedure InitCallBackFunctions();
procedure ClearCallBackFunctions();

implementation

{******************************************************************************}
{* Set the interface render callback                                          *}
{******************************************************************************}

procedure RenderInterfaceCallBack(); stdcall;
begin
  Intro.Render();
end;

{******************************************************************************}
{* Exit thee engine                                                           *}
{******************************************************************************}

procedure ExitCallback(); stdcall;
begin
  Configuration.ViewPortForm.Close();
end;

{******************************************************************************}
{* Move the player forward                                                    *}
{******************************************************************************}

procedure PlayerForward(); stdcall;
begin
  Player.MoveForward();
end;

{******************************************************************************}
{* Move the player backwards                                                  *}
{******************************************************************************}

procedure PlayerBackward(); stdcall;
begin
  Player.MoveBackWard();
end;

{******************************************************************************}
{* Move the player right                                                      *}
{******************************************************************************}

procedure PlayerRight(); stdcall;
begin
  Player.MoveRight();
end;

{******************************************************************************}
{* Move the player left                                                       *}
{******************************************************************************}

procedure PlayerLeft(); stdcall;
begin
  Player.MoveLeft();
end;

{******************************************************************************}
{* Set the walkspeed                                                          *}
{******************************************************************************}

procedure SetWalk(); stdcall;
begin
  Player.Walk();
end;

{******************************************************************************}
{* Set the runspeed                                                           *}
{******************************************************************************}

procedure SetRun(); stdcall;
begin
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

procedure ToggleStats(); stdcall;
begin
  Stats := not(Stats);
  gdConsoleCommand('RStats ' + BoolToStr(Stats));
end;

{******************************************************************************}
{* Toggle wireframe rendering                                                 *}
{******************************************************************************}

procedure ToggleWireFrame(); stdcall;
begin
  WireFrame := not(WireFrame);
  gdConsoleCommand('RWireframe ' + BoolToStr(WireFrame));
end;

{******************************************************************************}
{* Toggle the octree nodes                                                    *}
{******************************************************************************}

procedure ToggleOctreeNodes(); stdcall;
begin
  TreeNodes := not(TreeNodes);
  gdConsoleCommand('RTreeNodes ' + BoolToStr(TreeNodes));
end;

{******************************************************************************}
{* Toggle the OBJ boxes                                                       *}
{******************************************************************************}

procedure ToggleOBJBoxes(); stdcall;
begin
  ObjBoxes := not(ObjBoxes);
  gdConsoleCommand('ROBJBoxes ' + BoolToStr(ObjBoxes));
end;

{******************************************************************************}
{* Toggle the collision                                                       *}
{******************************************************************************}

procedure ToggleClipping(); stdcall;
begin
  Clip := not(Clip);
end;

{******************************************************************************}
{* Beforce render callback                                                    *}
{******************************************************************************}

procedure BeforeRender() stdcall;
begin
  //do soms sound stuff
  if Player.PlayerUnderWater() then
  begin
    gdSoundFilesResume( Sounds.FUnderWaterID );
    gdSoundFilesPause(Sounds.FAmbientId);
  end
  else
  begin
    gdSoundFilesPause(Sounds.FUnderWaterID);
    gdSoundFilesResume(Sounds.FAmbientID);
  end;

  //if the window is not selected sleep for a bit and only render the scene
  //this way we can move the mouse if needed and the scene will still refresh
  If (ViewPortForm.Focused = False) or (ViewPortForm.WindowState = wsMinimized) then
  begin
    gdInputUseMouseLook(false);
    gdGUIMouseCursorShow(true);
    gdInputEnable(false);
    Sleep(10)
  end
  else
  begin
    if Not(Intro.FRenderIntroText) then
    begin
      gdInputUseMouseLook(true);
      gdGUIMouseCursorShow(false);
      gdInputEnable(true);
    end;
  end;

  //calculate player collision and response
  Player.DoPlayerCollisionAndPhysics();
end;

{******************************************************************************}
{* Static render callback                                                    *}
{******************************************************************************}

procedure StaticRender() stdcall;
begin
end;

{******************************************************************************}
{* Init the callback functions                                                *}
{******************************************************************************}

procedure InitCallBackFunctions();
begin

  //main callback functions
  gdCallBackSetInterfaceRenderer( @RenderInterfaceCallBack );
  gdCallBackSetBeforeRender( @BeforeRender );
  
  //input funtions
  gdInputRegisterAction(IT_SINGLE,'ESCAPE',@ExitCallback, false );
  gdInputRegisterAction(IT_DIRECT,ConfigurationForm.AForwards,@PlayerForward, true );
  gdInputRegisterAction(IT_DIRECT,ConfigurationForm.ABackwards,@PlayerBackward, true );
  gdInputRegisterAction(IT_DIRECT,ConfigurationForm.ALeft,@PlayerLeft, true );
  gdInputRegisterAction(IT_DIRECT,ConfigurationForm.ARight,@PlayerRight, true );
  gdInputRegisterAction(IT_DOWN,ConfigurationForm.ARun,@SetRun, true );
  gdInputRegisterAction(IT_UP,ConfigurationForm.ARun,@SetWalk, true );
  gdInputRegisterAction(IT_SINGLE,'F1',@ToggleStats, true  );
  gdInputRegisterAction(IT_SINGLE,'F2',@ToggleWireFrame, true  );
  gdInputRegisterAction(IT_SINGLE,'F3',@ToggleOctreeNodes, true  );
  gdInputRegisterAction(IT_SINGLE,'F4',@ToggleOBJBoxes, true  );
  gdInputRegisterAction(IT_SINGLE,'F5',@ToggleClipping, true  );
end;

{******************************************************************************}
{* Clear the callback functions                                               *}
{******************************************************************************}

procedure ClearCallBackFunctions();
begin
  //main callback functions
  gdCallBackSetInterfaceRenderer( nil );
  gdCallBackSetBeforeRender( nil );
end;

end.
