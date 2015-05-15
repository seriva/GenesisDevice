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
unit GDInterface;

{$MODE Delphi}

interface

uses
  Dialogs,
  Windows,
  dglOPengl,
  SysUtils,
  GDRenderer,
  GDInput,
  GDSound,
  GDMain,
  GDConstants,
  GDConsole,
  GDSettings,
  GDTiming,
  GDTypes,
  GDGUI,
  GDCamera,
  GDOctree,
  GDMap,
  GDCellManager,
  GDResources,
  GDResource,
  GDStatistics,
  GDModes;

//Engine Functions
function  gdEngineInit() : Boolean;
procedure gdEngineShutDown(); 
function  gdEngineBuildInfo() : String;
procedure gdEngineLoop();
procedure gdEngineLoopCallBack( aFunction : TGDCallback );

//settings functions
procedure gdSettingsLoad();
procedure gdSettingsSave();
function  gdSettingsGetCurrent() : TSettings; 
procedure gdSettingsSetCurrent(aSettings : TSettings);

//log functions
procedure gdConsoleLog(aText : String; aNewLine : boolean = true);
procedure gdConsoleCommand( aCommand : String );
procedure gdConsoleAddChar( aChar : Char );
procedure gdConsoleControl( aKey : Integer );

//timing functions
procedure gdTimingStart();
procedure gdTimingStop();
function  gdTimingInSeconds() : String;
function  gdTimingInMilliSeconds() : String;
function  gdTimingFrameTime() : Integer;

//renderer functions
function  gdRendererInitViewPort( aWnd  : HWND ) : boolean;
function  gdRendererShutDownViewPort() : boolean;
procedure gdRendererResizeViewPort(aTop, aLeft, aWidth, aHeight : integer);
procedure gdRendererState(aState : TGDRenderState);

//sound functions
function  gdSoundLoad( aFileName : String; aType : TGDSoundTypes ) : pointer;
procedure gdSoundRemove( aPointer : pointer );
procedure gdSoundPlay( aPointer : pointer );
procedure gdSoundPause( aPointer : pointer );
procedure gdSoundResume( aPointer  : pointer );
procedure gdSoundStop( aPointer  : pointer );

//input functions
procedure gdInputEnable( aEnable : boolean );
procedure gdInputUseMouseLook( aUse : boolean );
procedure gdInputRegisterAction(aType : TGDInputTypes; aKey : Integer ; aAction : TGDCallback;  aConsoleDisabled : boolean );

//gui functions
procedure gdGUIMouseCursorShow(aShow : boolean); 
function  gdGUIMouseCursorGetPosition() : TPoint;
procedure gdGUILoadingScreenSetup( aProcessName : String; aMax : Integer ); 
procedure gdGUILoadingScreenUpdate();
function  gdGUIInitScreen(aFileName : String) : pointer;
function  gdGUIScreenGetVisible(aPointer : pointer ): Boolean;
procedure gdGUIScreenSetVisible(aPointer : pointer ; aVisible : Boolean);

//camera functions
procedure gdCameraSetPosition(aX,aY,aZ : double); 
function  gdCameraGetPosition() : TGDVector;
procedure gdCameraSetDirection(aX,aY,aZ : double); 
function  gdCameraGetDirection() : TGDVector;
procedure gdCameraMove(aStep : double); 
procedure gdCameraStrafe(aStep : double);

//map functions
function  gdMapLoad( aFileName : String ) : boolean; 
procedure gdMapClear();
function  gdMapTerrainHeight(aX, aZ : Double) : Double; 
function  gdMapTerrainRotation(aX, aZ : Double) : TGDVector;
function  gdMapWaterHeight(): Double;

implementation

{******************************************************************************}
{* Initialize the engine`s core                                               *}
{******************************************************************************}

function gdEngineInit() : Boolean;
begin
  DefaultFormatSettings.DecimalSeparator := '.';

  //Init engine base systems.
  Timing   := TGDTiming.Create();
  Console  := TGDConsole.Create();
  Settings := TGDSettings.Create();

  //Create engine main and subsystem classes
  Main             := TGDMain.Create();
  Input            := TGDInput.Create();
  Sound            := TGDSound.Create();
  Renderer         := TGDRenderer.Create();

  //Check if subsystems where initialized properly.
  If not(Sound.Initialized) or not(Input.Initialized) then
  begin
     result := false;
     exit;
  end;

  //Create engine classes
  Resources        := TGDResources.Create();
  Camera           := TGDCamera.Create();
  Map              := TGDMap.Create();
  Statistics       := TGDStatistics.Create();
  Modes            := TGDModes.Create();
  CellManager      := TGDCellManager.Create();
  GUI              := TGDGUI.Create();

  result := true;
end;

{******************************************************************************}
{* Shutdown the engine`s core                                                 *}
{******************************************************************************}

procedure gdEngineShutDown(); 
begin
  //Clear engine classes
  FreeAndNil(Input);
  FreeAndNil(Camera);
  FreeAndNil(Renderer);
  FreeAndNil(Sound);
  FreeAndNil(GUI);
  FreeAndNil(Map);
  FreeAndNil(Timing);
  FreeAndNil(CellManager);
  FreeAndNil(Main);
  FreeAndNil(Settings);
  FreeAndNil(Statistics);
  FreeAndNil(Modes);
  FreeAndNil(Console);

  FreeAndNil(Resources)
end;

{******************************************************************************}
{* Get the builddate of the engine                                            *}
{******************************************************************************}

function gdEngineBuildInfo() : String;
begin
   result := ENGINE_INFO;
end;

{******************************************************************************}
{* Main loop of the engine                                                    *}
{******************************************************************************}

procedure gdEngineLoop();
begin
  Main.Main();
end;

{******************************************************************************}
{* Sets the beforerender callback function                                    *}
{******************************************************************************}

procedure gdEngineLoopCallBack( aFunction : TGDCallback );
begin
  Main.LoopCallBack := aFunction;
end;

{******************************************************************************}
{* Initialize the renderer                                                    *}
{******************************************************************************}

function gdRendererInitViewPort( aWnd  : HWND ) : boolean;
begin
  result := Renderer.InitViewPort( aWnd );
end;

{******************************************************************************}
{* Shutdown the renderer                                                      *}
{******************************************************************************}

function gdRendererShutDownViewPort() : boolean;
begin
  result := Renderer.ShutDownViewPort();
end;

{******************************************************************************}
{* Resize the viewport of the engine                                          *}
{******************************************************************************}

procedure gdRendererResizeViewPort(aTop, aLeft, aWidth, aHeight : integer);
begin
  Settings.Top := aTop;
  Settings.Left := aLeft;
  Settings.Width := aWidth;
  Settings.Height := aHeight;
  Input.CalculateMousePosStart();
  Renderer.ResizeViewPort();
end;

{******************************************************************************}
{* Set the current renderstate                                                *}
{******************************************************************************}

procedure gdRendererState(aState : TGDRenderState);
begin
  Renderer.RenderState(aState);
end;

{******************************************************************************}
{* Init a soundfile                                                           *}
{******************************************************************************}

function  gdSoundLoad( aFileName : String;  aType : TGDSoundTypes  ) : pointer;
begin
  result := Resources.LoadSound(aFileName,aType );
end;

{******************************************************************************}
{* Remove sound                                                               *}
{******************************************************************************}

procedure gdSoundRemove( aPointer : pointer );
begin
  Resources.RemoveResource(TGDResource(aPointer));
  aPointer := nil
end;

{******************************************************************************}
{* Play a sound                                                               *}
{******************************************************************************}

procedure gdSoundPlay( aPointer  : pointer );
begin
  TGDSoundFile(aPointer).Play();
end;

{******************************************************************************}
{* Pause a sound                                                              *}
{******************************************************************************}

procedure gdSoundPause( aPointer  : pointer );

begin
  TGDSoundFile(aPointer).Pause();
end;

{******************************************************************************}
{* Resume a sound                                                             *}
{******************************************************************************}

procedure gdSoundResume( aPointer  : pointer );
begin
  TGDSoundFile(aPointer).Resume();
end;

{******************************************************************************}
{* Stop a sound                                                               *}
{******************************************************************************}

procedure gdSoundStop( aPointer  : pointer );
begin
  TGDSoundFile(aPointer).Stop();
end;

{******************************************************************************}
{* Register a keyaction                                                       *}
{******************************************************************************}

procedure gdInputRegisterAction(aType : TGDInputTypes; aKey : Integer ; aAction : TGDCallback;  aConsoleDisabled : boolean );
begin
  Input.RegisterInputAction(aType, aKey, aAction, aConsoleDisabled );
end;

{******************************************************************************}
{* Toggle use of mouselook                                                    *}
{******************************************************************************}

procedure gdInputUseMouseLook( aUse : boolean );
begin
  Input.MouseLook := aUse;
end;

{******************************************************************************}
{* Enable or disable input                                                    *}
{******************************************************************************}

procedure gdInputEnable( aEnable : boolean );
begin
  Input.EnableInput := aEnable;
end;

{******************************************************************************}
{* Show or hide the mouse cursor                                              *}
{******************************************************************************}

procedure gdGUIMouseCursorShow(aShow : boolean); 
begin
  Input.CalculateMousePosStart();
  GUI.MouseCursor.ShowMouse := aShow;
end;

{******************************************************************************}
{* retrieve mouse cursor position                                             *}
{******************************************************************************}

function gdGUIMouseCursorGetPosition() : TPoint; 
begin
  result.X := GUI.MouseCursor.Position.X;
  result.Y := GUI.MouseCursor.Position.Y;
end;

{******************************************************************************}
{* Setup the loadingscreen                                                    *}
{******************************************************************************}

procedure gdGUILoadingScreenSetup( aProcessName : String; aMax : Integer ); 
begin
  GUI.LoadingScreen.SetupForUse(String(aProcessName),aMax);
end;

{******************************************************************************}
{* Update the loadingscreen                                                   *}
{******************************************************************************}

procedure gdGUILoadingScreenUpdate(); 
begin
  GUI.LoadingScreen.UpdateBar();
end;

{******************************************************************************}
{* Init a screen                                                              *}
{******************************************************************************}

function  gdGUIInitScreen(aFileName : String) : pointer;
begin
  result := GUI.InitScreen(aFileName);
end;

{******************************************************************************}
{* Get screen visible                                                         *}
{******************************************************************************}

function  gdGUIScreenGetVisible(aPointer : pointer ): Boolean;
begin
  result := TGDScreen(aPointer).Visible;
end;

{******************************************************************************}
{* Set screen visible                                                         *}
{******************************************************************************}

procedure gdGUIScreenSetVisible(aPointer : pointer ; aVisible : Boolean);
begin
  TGDScreen(aPointer).Visible := aVisible;
end;

{******************************************************************************}
{* Set the camera position                                                    *}
{******************************************************************************}

procedure gdCameraSetPosition(aX,aY,aZ : double); 
begin
  Camera.Position.Reset(aX,aY,aZ);
end;

{******************************************************************************}
{* Get the camera position                                                    *}
{******************************************************************************}

function gdCameraGetPosition() : TGDVector;
begin
  result := Camera.Position.Copy();
end;

{******************************************************************************}
{* Set the camera direction                                                   *}
{******************************************************************************}

procedure gdCameraSetDirection(aX,aY,aZ : double); 
begin
  Camera.Direction.Reset(aX, aY, aZ);
end;

{******************************************************************************}
{* Get the camera direction                                                   *}
{******************************************************************************}

function gdCameraGetDirection() : TGDVector;
begin
  result := Camera.Direction.Copy();
end;

{******************************************************************************}
{* Move camera forward or backwards                                           *}
{******************************************************************************}

procedure gdCameraMove(aStep : double); 
begin
  Camera.Move(aStep);
end;

{******************************************************************************}
{* Strafe camera right or left                                                *}
{******************************************************************************}

procedure gdCameraStrafe(aStep : double); 
begin
  Camera.Strafe(aStep);
end;


{******************************************************************************}
{* Load settings out an ini-file                                              *}
{******************************************************************************}

procedure gdSettingsLoad();
begin
  Settings.LoadIniFile();
end;

{******************************************************************************}
{* Save settings to an ini-file                                               *}
{******************************************************************************}

procedure gdSettingsSave();
begin
  Settings.SaveIniFile();
end;

{******************************************************************************}
{* Retrieve the current settings                                              *}
{******************************************************************************}

function  gdSettingsGetCurrent() : TSettings; 
begin
  result := Settings.GetSettings();
end;

{******************************************************************************}
{* Set the current settings                                                   *}
{******************************************************************************}

procedure gdSettingsSetCurrent(aSettings : TSettings); 
begin
  Settings.SetSettings(aSettings);
end;

{******************************************************************************}
{* Excute console command                                                     *}
{******************************************************************************}

procedure gdConsoleCommand(aCommand : String );
begin
  Console.ExecuteCommand(aCommand);
end;

{******************************************************************************}
{* Add a text to the console log                                              *}
{******************************************************************************}

procedure gdConsoleLog(aText : String; aNewLine : boolean = true);
begin
  If aText = '' then exit;
  Console.Write(aText, aNewLine);
end;

{******************************************************************************}
{* Pas down a char to the console                                             *}
{******************************************************************************}

procedure gdConsoleAddChar( aChar : Char );
begin
  Console.AddChar(aChar);
end;

procedure gdConsoleControl( aKey : Integer );
begin
  Console.Control(aKey);
end;

{******************************************************************************}
{* Start timer                                                                *}
{******************************************************************************}

procedure gdTimingStart();
begin
  Timing.Start();
end;

{******************************************************************************}
{* Stop timer                                                                 *}
{******************************************************************************}

procedure gdTimingStop();
begin
  Timing.Stop();
end;

{******************************************************************************}
{* Get time in seconds                                                        *}
{******************************************************************************}

function gdTimingInSeconds() : String;
begin
  result := Timing.TimeInSeconds();
end;

{******************************************************************************}
{* Get time in millids                                                        *}
{******************************************************************************}

function gdTimingInMilliSeconds() : String;
begin
  result := Timing.TimeInMilliSeconds();
end;

{******************************************************************************}
{* Get frame time                                                             *}
{******************************************************************************}

function gdTimingFrameTime() : Integer;
begin
  result := Timing.FrameTime;
end;

{******************************************************************************}
{* Load map                                                                   *}
{******************************************************************************}

function gdMapLoad( aFileName : String ) : boolean; 
begin
  result := false;
  result := Map.InitMap( aFileName );
  Camera.Position := Map.PlayerStart.Copy();
  Camera.Rotation := Map.PlayerViewAngle.Copy();
  Camera.MouseLook(0,0,1,1,0,False);
end;

{******************************************************************************}
{* Clear map                                                                  *}
{******************************************************************************}

procedure gdMapClear(); 
begin
  Map.Clear();
end;

{******************************************************************************}
{* Return the height of the terrain at a point                                *}
{******************************************************************************}

function gdMapTerrainHeight(aX, aZ  : Double) : Double; 
var
  iHeight : Double;
begin
  Map.Terrain.GetHeight( aX, aZ, iHeight );
  result := iHeight;
end;

{******************************************************************************}
{* Return the rotation of the terrain at a point                              *}
{******************************************************************************}

function  gdMapTerrainRotation(aX, aZ : Double) : TGDVector;
var
  iRotation : TGDVector;
begin
  Map.Terrain.GetRotation( aX, aZ, iRotation );
  result := iRotation;
end;

{******************************************************************************}
{* return the waterheight                                                     *}
{******************************************************************************}

function gdMapWaterHeight(): Double; 
begin
  result := Map.Water.WaterHeight;
end;

end.
