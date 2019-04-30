{*******************************************************************************
*                            Genesis Device Engine                             *
*                   Copyright © 2007-2015 Luuk van Venrooij                    *
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

{$MODE Delphi}

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
    FDone 	: boolean;

    function  InitSDL(): Boolean;
    function  InitSystems(): boolean;
    procedure ClearSystems();
  public
    property Done : Boolean read FDone write FDone;

    constructor Create();
    destructor  Destroy(); override;

    procedure Reset();

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
end;

{******************************************************************************}
{* Destroy main class                                                         *}
{******************************************************************************}

destructor TGDEngine.Destroy();
begin
  inherited;
end;

{******************************************************************************}
{* Init SDL                                                                   *}
{******************************************************************************}

function  TGDEngine.InitSDL(): Boolean;
var
  iVersion : TSDL_Version;
  iV1, iV2 : String;
begin
  Inherited;
  GDConsole.Write('.....Initializing SDL');
  try
    result := not(SDL_Init(SDL_INIT_VIDEO or SDL_INIT_TIMER) < 0);
    if result then
    begin
      SDL_GetVersion(@iVersion);
      iV1 :=  IntToStr(iVersion.major) + '.' + IntToStr(iVersion.minor) + '.' + IntToStr(iVersion.patch);
      iV2 :=  IntToStr(MRS_SDL_MAJOR_VERSION) + '.' + IntToStr(MRS_SDL_MINOR_VERSION) + '.' + IntToStr(MRS_SDL_PATCH_VERSION);
      GDConsole.Write('  Version: ' + iV1);
      if (iV1 <> iV2) then
        Raise Exception.Create('SDL version ' + iV2 + ' required.');
    end
    else
    	Raise Exception.Create(SDL_GetError());
    GDConsole.Write('.....Done initializing SDL');
  except
    on E: Exception do
    begin
      result := false;
      GDConsole.Write('Failed to initialize SDL: ' + E.Message);
    end;
  end;
end;

{******************************************************************************}
{* Init engine Systems                                                        *}
{******************************************************************************}

function TGDEngine.InitSystems(): boolean;
var
  iSDLInit : boolean;
begin
  GDConsole    := TGDConsole.Create();
  GDSettings   := TGDSettings.Create();
  iSDLInit    := InitSDL();
  GDTiming     := TGDTiming.Create();
  GDWindow     := TGDWindow.Create();
  GDInput      := TGDInput.Create();
  GDRenderer   := TGDRenderer.Create();
  GDSound      := TGDSound.Create();
  GDPhysics    := TGDPhysics.Create();
  result       := GDInput.Initialized and GDRenderer.Initialized and GDWindow.Initialized and GDPhysics.Initialized and iSDLInit ;
  GDStatistics := TGDStatistics.Create();
  GDModes      := TGDModes.Create();
  GDResources  := TGDResources.Create();
  GDResources.Sorted := True;
  GDCamera     := TGDCamera.Create();
  GDMap        := TGDMap.Create();
  GDGUI        := TGDGUI.Create();
end;

{******************************************************************************}
{* Clear engine Systems                                                       *}
{******************************************************************************}

procedure TGDEngine.ClearSystems();
begin
  FreeAndNil(GDStatistics);
  FreeAndNil(GDModes);
  FreeAndNil(GDInput);
  FreeAndNil(GDSound);
  FreeAndNil(GDPhysics);
  FreeAndNil(GDRenderer);
  FreeAndNil(GDGUI);
  FreeAndNil(GDMap);
  FreeAndNil(GDWindow);

  SDL_Quit();
  GDConsole.Write('Shutting down SDL...Ok');

  FreeAndNil(GDTiming);
  FreeAndNil(GDConsole);
  FreeAndNil(GDSettings);
  FreeAndNil(GDCamera);

  FreeAndNil(GDResources);
end;

{******************************************************************************}
{* Clear the base resources                                                   *}
{******************************************************************************}

procedure TGDEngine.Reset();
begin
  GDConsole.Reset();
  GDModes.Reset();
  GDInput.Clear();
  GDMap.Clear();
  GDGUI.ClearScreens();
  GDResources.Clear();
end;

{******************************************************************************}
{* Init                                                                       *}
{******************************************************************************}

procedure TGDEngine.Init(aInit : TGDCallback);
begin
  GDWindow.Show();
  GDRenderer.InitViewPort();
  GDRenderer.ResizeViewPort(GDSettings.Width, GDSettings.Height);
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
  GDRenderer.ClearViewPort();
  Reset();
  SDL_ShowCursor(1);
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
  If not(GDEngine.InitSystems()) then
  begin
    halt;
  end;
finalization
  GDEngine.ClearSystems();
  FreeAndNil(GDEngine);
end.
