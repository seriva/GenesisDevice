{*******************************************************************************
*                            Genesis Device Engine                             *
*                   Copyright © 2007-2022 Luuk van Venrooij                    *
*                        http://www.luukvanvenrooij.nl                         *
********************************************************************************
*                                                                              *
*  This file is part of the Genesis Device                              *
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
unit uGDEngine;

{******************************************************************************}
{* This is the main unit of the  It holds the main variables,          *}
{* callbacks and loops controling the  This unit will later be extended*}
{* with multible threads for the different systems                            *}
{******************************************************************************}

interface

uses
  SysUtils,
  sdl2,
  dglOpenGL,
  uGDRenderer,
  uGDConsole,
  uGDTiming,
  uGDConstants,
  uGDInput,
  uGDGUI,
  uGDMap,
  uGDWindow,
  uGDResources,
  uGDSound,
  uGDModes,
  uGDPhysics,
  uGDCamera,
  uGDSettings,
  uGDStatistics;

type

{******************************************************************************}
{* Engine class                                                               *}
{******************************************************************************}

  TGDEngine = Class
  private
    FDone : boolean;
  public
    property Done : Boolean read FDone write FDone;

    constructor Create();
    destructor  Destroy(); override;

    procedure Init(aInit : TGDCallback);
    procedure Clear(aClear : TGDCallback);
    procedure Loop(aLoop : TGDCallback);
  end;

var
  GDEngine     : TGDEngine;
  GDTiming     : TGDTiming;
  GDConsole    : TGDConsole;
  GDSettings   : TGDSettings;
  GDWindow     : TGDWindow;
  GDInput      : TGDInput;
  GDSound      : TGDSound;
  GDPhysics    : TGDPhysics;
  GDRenderer   : TGDRenderer;
  GDStatistics : TGDStatistics;
  GDModes      : TGDModes;
  GDResources  : TGDResources;
  GDCamera     : TGDCamera;
  GDMap        : TGDMap;
  GDGUI        : TGDGUI;

implementation

{******************************************************************************}
{* Create engine class                                                        *}
{******************************************************************************}

constructor TGDEngine.Create();
begin
  DefaultFormatSettings.DecimalSeparator := '.';
  GDConsole    := TGDConsole.Create();
  GDSettings   := TGDSettings.Create();
end;

{******************************************************************************}
{* Destroy main class                                                         *}
{******************************************************************************}

destructor TGDEngine.Destroy();
begin
  inherited;
  FreeAndNil(GDConsole);
  FreeAndNil(GDSettings);
end;

{******************************************************************************}
{* Init                                                                       *}
{******************************************************************************}

procedure TGDEngine.Init(aInit : TGDCallback);
var
  iSDLInit : boolean;
  iVersion : TSDL_Version;
  iV : String;
begin
  GDSettings.load();

  GDConsole.Write('.....Initializing SDL');
  try
    iSDLInit := not(SDL_Init(SDL_INIT_VIDEO or SDL_INIT_TIMER) < 0);
    if not(iSDLInit) then
       Raise Exception.Create(SDL_GetError());
    SDL_GetVersion(@iVersion);
    iV :=  IntToStr(iVersion.major) + '.' + IntToStr(iVersion.minor) + '.' + IntToStr(iVersion.patch);
    GDConsole.Write('  Version: ' + iV);
    GDConsole.Write('.....Done initializing SDL');
  except
    on E: Exception do
    begin
      iSDLInit := false;
      GDConsole.Write('Failed to initialize SDL: ' + E.Message);
    end;
  end;

  GDTiming     := TGDTiming.Create();
  GDWindow     := TGDWindow.Create();
  GDInput      := TGDInput.Create();
  GDRenderer   := TGDRenderer.Create();
  GDSound      := TGDSound.Create();
  GDPhysics    := TGDPhysics.Create();

  If not(GDInput.Initialized and GDRenderer.Initialized and GDWindow.Initialized and GDPhysics.Initialized and iSDLInit) then
  begin
    halt;
  end;

  GDStatistics := TGDStatistics.Create();
  GDModes      := TGDModes.Create();
  GDResources  := TGDResources.Create();
  GDResources.Sorted := True;
  GDCamera     := TGDCamera.Create();
  GDMap        := TGDMap.Create();
  GDGUI        := TGDGUI.Create();
  GDWindow.Show();
  SDL_ShowCursor(0);
  Done := false;
  if assigned(aInit) then aInit();
end;

{******************************************************************************}
{* Clear                                                                      *}
{******************************************************************************}

procedure TGDEngine.Clear(aClear : TGDCallback);
begin
  GDWindow.Hide();
  if assigned(aClear) then aClear();
  SDL_ShowCursor(1);
  FreeAndNil(GDStatistics);
  FreeAndNil(GDModes);
  FreeAndNil(GDInput);
  FreeAndNil(GDSound);
  FreeAndNil(GDPhysics);
  FreeAndNil(GDRenderer);
  FreeAndNil(GDGUI);
  FreeAndNil(GDMap);
  FreeAndNil(GDResources);
  FreeAndNil(GDWindow);
  FreeAndNil(GDTiming);
  FreeAndNil(GDCamera);
  SDL_Quit();
  GDConsole.Write('Shutting down SDL...Ok');
end;

{******************************************************************************}
{* Loop                                                                       *}
{******************************************************************************}

procedure TGDEngine.Loop(aLoop : TGDCallback);
begin
  //start timing
  GDStatistics.FrameStart();
  GDTiming.CalculateFrameTime();

  //Update all systems
  GDWindow.Update();
  GDInput.Update();
  GDSound.Update();
  GDMap.Update();
  GDPhysics.Update();

  if assigned(aLoop) then aLoop();

  //Render the scene
  GDRenderer.Render();
  GDWindow.Swap();

  //end timing
  GDStatistics.FrameStop();
  GDStatistics.Update();
end;

initialization
  GDEngine := TGDEngine.Create();
finalization
  FreeAndNil(GDEngine);
end.
