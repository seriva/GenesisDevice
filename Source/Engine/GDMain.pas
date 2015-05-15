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
unit GDMain;

{$MODE Delphi}

{******************************************************************************}
{* This is the main unit of the engine. It holds the main variables,          *}
{* callbacks and loops controling the engine. This unit will later be extended*}
{* with multible threads for the different systems                            *}
{******************************************************************************}

interface

uses
  LCLIntf,
  LCLType,
  SysUtils,
  dglOpenGL,
  GDRenderer,
  GDConsole,
  GDTiming,
  GDConstants,
  GDInput,
  GDGUI,
  GDMap,
  GDResources,
  GDSound,
  GDOctree,
  GDCellManager,
  GDModes,
  GDStatistics;

type

{******************************************************************************}
{* Main class                                                                 *}
{******************************************************************************}

  TGDMain  = Class
  private
  public
    LoopCallBack : TGDCallback;

    constructor Create();
    destructor  Destroy(); override;

    procedure InitBaseResources();
    procedure ClearBaseResources();

    procedure Main();
  end;

var
  Main : TGDMain;

implementation

{******************************************************************************}
{* Create main class                                                          *}
{******************************************************************************}

constructor TGDMain.Create();
begin
  LoopCallBack := nil;
end;

{******************************************************************************}
{* Destroy main class                                                         *}
{******************************************************************************}

destructor TGDMain.Destroy();
begin
  inherited;
end;

{******************************************************************************}
{* Init the base resources of the engine using the base.ini                   *}
{******************************************************************************}

procedure TGDMain.InitBaseResources();
begin
  Timing.Start();
  Console.Write('......Initializing engine recources');

  GUI.InitGUI();
  Statistics.InitStatistics();
  Modes.InitModes();
  Console.InitConsole();

  Timing.Stop();
  Console.Write('......Done initializing engine resources (' + Timing.TimeInSeconds + ' Sec)');
end;

{******************************************************************************}
{* Clear the base resources                                                   *}
{******************************************************************************}

procedure TGDMain.ClearBaseResources();
begin
  LoopCallBack := nil;
  Input.ClearInputActions();
  Console.Clear();
  GUI.Clear();
  Map.Clear();
  Resources.Clear();
end;

{******************************************************************************}
{* Main loop of the engine                                                    *}
{******************************************************************************}

procedure TGDMain.Main();
begin
  //start timing
  Statistics.FrameStart();
  Timing.CalculateFrameTime();

  Input.ExecuteInput();
  Sound.Update();
  Map.Update();
  if assigned(LoopCallBack) then LoopCallBack();
  Renderer.Render();

  //end timing
  Statistics.FrameStop();
  Statistics.Update();
end;

end.
