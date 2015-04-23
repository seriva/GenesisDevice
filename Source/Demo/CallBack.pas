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
{* Create a screenshot                                                        *}
{******************************************************************************}

procedure ScreenShot(); stdcall;
var
  iI : Integer;
begin
  iI := 0;
  while FileExistsUTF8(ExtractFilePath(Application.ExeName) + 'Screenshots\' + IntTOStr( iI ) + '.bmp') { *Converted from FileExists* } do
    Inc(iI);
  gdRenderSystemScreenShot( PChar(IntTOStr( iI ) ) );
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
  gdCommandExecute('RStats ' + BoolToStr(Stats));
end;

{******************************************************************************}
{* Toggle wireframe rendering                                                 *}
{******************************************************************************}

procedure ToggleWireFrame(); stdcall;
begin
  WireFrame := not(WireFrame);
  gdCommandExecute('RWireframe ' + BoolToStr(WireFrame));
end;

{******************************************************************************}
{* Toggle the octree nodes                                                    *}
{******************************************************************************}

procedure ToggleOctreeNodes(); stdcall;
begin
  TreeNodes := not(TreeNodes);
  gdCommandExecute('RTreeNodes ' + BoolToStr(TreeNodes));
end;

{******************************************************************************}
{* Toggle the OBJ boxes                                                       *}
{******************************************************************************}

procedure ToggleOBJBoxes(); stdcall;
begin
  ObjBoxes := not(ObjBoxes);
  gdCommandExecute('ROBJBoxes ' + BoolToStr(ObjBoxes));
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
    gdInputSystemUseMouseLook(false);
    gdGUIMouseCursorShow(true);
    gdInputSystemEnable(false);
    Sleep(10)
  end
  else
  begin
    if Not(Intro.FRenderIntroText) then
    begin
      gdInputSystemUseMouseLook(true);
      gdGUIMouseCursorShow(false);
      gdInputSystemEnable(true);
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
  gdInputSystemRegisterAction(IT_SINGLE,'ExitGame','ESCAPE',@ExitCallback, false );
  gdInputSystemRegisterAction(IT_DIRECT,'Forward',ConfigurationForm.AForwards,@PlayerForward, true );
  gdInputSystemRegisterAction(IT_DIRECT,'Backward',ConfigurationForm.ABackwards,@PlayerBackward, true );
  gdInputSystemRegisterAction(IT_DIRECT,'Left',ConfigurationForm.ALeft,@PlayerLeft, true );
  gdInputSystemRegisterAction(IT_DIRECT,'Right',ConfigurationForm.ARight,@PlayerRight, true );
  gdInputSystemRegisterAction(IT_DOWN,'SetRun',ConfigurationForm.ARun,@SetRun, true );
  gdInputSystemRegisterAction(IT_UP,'SetWalk',ConfigurationForm.ARun,@SetWalk, true );
  gdInputSystemRegisterAction(IT_SINGLE,'Toggle Stats','F1',@ToggleStats, true  );
  gdInputSystemRegisterAction(IT_SINGLE,'Toggle Wireframe','F2',@ToggleWireFrame, true  );
  gdInputSystemRegisterAction(IT_SINGLE,'Toggle Octree Nodes','F3',@ToggleOctreeNodes, true  );
  gdInputSystemRegisterAction(IT_SINGLE,'Toggle Object Bounding Boxes','F4',@ToggleOBJBoxes, true  );
  gdInputSystemRegisterAction(IT_SINGLE,'Toggle Collision','F5',@ToggleClipping, true  );
  gdInputSystemRegisterAction(IT_SINGLE,'Make Screenshot','F6',@ScreenShot, true  );
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
