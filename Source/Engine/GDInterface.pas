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
  GDFrustum,
  GDMap,
  GDTerrain,
  GDSkyDome,
  GDWater,
  GDFoliage,
  GDFog,
  GDCellManager,
  GDTexture,
  GDMesh,
  GDMaterials,
  contnrs,
  GDLighting,
  GDStatistics,
  GDModes,
  GDCallBack; 
  
Var
  FEngineInitialized   : Boolean = false;

 //some functions to check the engine 
function CheckEngineInitialized(): boolean;

//Engine Functions
function  gdEngineInit() : Boolean;
procedure gdEngineShutDown(); 
function  gdEngineBuildInfo() : String;

//settings functions
procedure gdSettingsLoad();
procedure gdSettingsSave();
function  gdSettingsGetCurrent() : TSettings; 
procedure gdSettingsSetCurrent(aSettings : TSettings);

//log functions
procedure gdConsoleToggle();
procedure gdConsoleLog(aText : String; aNewLine : boolean = true);
procedure gdConsoleCommand( aCommand : String );

//timing functions
procedure gdTimingStart();
procedure gdTimingStop();
function  gdTimingInSeconds() : String;
function  gdTimingInMilliSeconds() : String;
function  gdTimingFrameTime() : Integer;

//callback functions
procedure gdCallBackSetInterfaceRenderer( aFunction : TGDProcEngineCallback );
procedure gdCallBackSetBeforeRender( aFunction : TGDProcEngineCallback ); 
procedure gdCallBackSetAfterRender( aFunction : TGDProcEngineCallback ); 

//loop functions
procedure gdLoopMain();

//renderer functions
function  gdRendererInitViewPort( aWnd  : HWND ) : boolean;
function  gdRendererShutDownViewPort() : boolean;
procedure gdRendererResizeViewPort(aTop, aLeft, aWidth, aHeight : integer);
procedure gdRendererState(aState : TGDRenderState);

//sound functions
function  gdSoundNumberOfDrivers() : Integer;
function  gdSoundGetDriverName( aDriverNumber : Integer ) : String;
function  gdSoundInitDriver() : boolean;
function  gdSoundShutDownDriver() : boolean;

//input functions
function  gdInputStringToKey( aName : String ) : Integer ;
function  gdInputKeyToString( aKey : Integer ) : String ;
function  gdInputDetectCurrentKeyString() : String ;
function  gdInputDetectCurrentKey() : Integer ;
procedure gdInputRegisterAction(aType : TGDInputTypes; aKeyString : String ; aAction : TGDProcEngineCallback;  aConsoleDisabled : boolean );
procedure gdInputHandleChar( aChar : Char );
procedure gdInputUseMouseLook( aUse : boolean );
procedure gdInputEnable( aEnable : boolean );

//gui functions
procedure gdGUIMouseCursorShow(aShow : boolean); 
function  gdGUIMouseCursorGetPosition() : TPoint;
procedure gdGUILoadingScreenSetup( aProcessName : String; aMax : Integer ); 
procedure gdGUILoadingScreenUpdate();
procedure gdGUITextColor(aR,aG,aB : Double);
procedure gdGUITextRender(aX,aY,aScale : Double; aText : String);

//camera functions
procedure gdCameraInit(aX,aY,aZ : double); 
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

//texture functions
function  gdTexturesLoad( aFileName : String ) : pointer; 
procedure gdTexturesBind( aPointer : pointer; aTextureUnit : GLEnum );
procedure gdTexturesClear(); 
procedure gdTexturesRemove( aPointer : pointer );

//sound functions
function  gdSoundFilesLoad( aFileName : String; aType : TGDSoundTypes ) : pointer; 
procedure gdSoundFilesRemove( aPointer : pointer ); 
procedure gdSoundFilesClear(); 
procedure gdSoundFilesPlay( aPointer : pointer ); 
procedure gdSoundFilesPause( aPointer : pointer ); 
procedure gdSoundFilesResume( aPointer  : pointer ); 

implementation

{******************************************************************************}
{* Check if the engine is initialized                                         *}
{******************************************************************************}

function CheckEngineInitialized(): boolean;
begin
  If not(FEngineInitialized) then MessageBox(0, 'Engine is not initialized!', 'Error', MB_OK or MB_ICONERROR);
  result := FEngineInitialized;
end;

{******************************************************************************}
{* Initialize the engine`s core                                               *}
{******************************************************************************}

function gdEngineInit() : Boolean;
begin
  If FEngineInitialized then
  begin
     ShowMessage('Engine is already initialized!');
     result := false;
     exit;
  end;
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
  Camera           := TGDCamera.Create();
  Frustum          := TGDFrustum.Create();
  Map              := TGDMap.Create();
  Terrain          := TGDTerrain.Create();
  SkyDome          := TGDSkyDome.Create();
  Water            := TGDWater.Create();
  Octree           := TGDOcTree.Create();
  Foliage          := TGDFoliage.Create();
  Statistics       := TGDStatistics.Create();
  Modes            := TGDModes.Create();
  CallBack         := TGDCallBack.Create();
  FogManager       := TGDFogManager.Create();
  CellManager      := TGDCellManager.Create();
  GUI              := TGDGUI.Create();
  DirectionalLight := TGDDirectionalLight.Create();


  SoundList        := TObjectList.Create();
  TextureList      := TObjectList.Create();
  MaterialList     := TGDMaterialList.Create();
  MeshList         := TGDMeshList.Create();


  result := true;
  FEngineInitialized := true;
end;

{******************************************************************************}
{* Shutdown the engine`s core                                                 *}
{******************************************************************************}

procedure gdEngineShutDown(); 
begin
  If Not(FEngineInitialized) then exit;

  //Clear engine classes
  FreeAndNil(Input);
  FreeAndNil(Camera);
  FreeAndNil(Renderer);
  FreeAndNil(SoundList);
  FreeAndNil(Sound);
  FreeAndNil(GUI);
  FreeAndNil(Frustum);
  FreeAndNil(Map);
  FreeAndNil(Terrain);
  FreeAndNil(Timing);
  FreeAndNil(Foliage);
  FreeAndNil(SkyDome);
  FreeAndNil(Water);
  FreeAndNil(FogManager);
  FreeAndNil(CellManager);
  FreeAndNil(MeshList);
  FreeAndNil(Octree);
  FreeAndNil(TextureList);
  FreeAndNil(Main);
  FreeAndNil(Settings);
  FreeAndNil(MaterialList);
  FreeAndNil(Statistics);
  FreeAndNil(CallBack);
  FreeAndNil(DirectionalLight);
  FreeAndNil(Modes);
  FreeAndNil(Console);
end;

{******************************************************************************}
{* Get the builddate of the engine                                            *}
{******************************************************************************}

function gdEngineBuildInfo() : String;
begin
   result := ENGINE_INFO;
end;

{******************************************************************************}
{* Initialize the renderer                                                    *}
{******************************************************************************}

function gdRendererInitViewPort( aWnd  : HWND ) : boolean;
begin
  If Not(FEngineInitialized) then exit;
  result := Renderer.InitViewPort( aWnd );
end;

{******************************************************************************}
{* Shutdown the renderer                                                      *}
{******************************************************************************}

function gdRendererShutDownViewPort() : boolean;
begin
  If Not(FEngineInitialized) then exit;
  result := Renderer.ShutDownViewPort();
end;

{******************************************************************************}
{* Resize the viewport of the engine                                          *}
{******************************************************************************}

procedure gdRendererResizeViewPort(aTop, aLeft, aWidth, aHeight : integer);
begin
  If Not(FEngineInitialized) then exit;
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
  If Not(FEngineInitialized) then exit;
  Renderer.RenderState(aState);
end;

{******************************************************************************}
{* Get the number of sounddrivers the soundengine supports                    *}
{******************************************************************************}

function gdSoundNumberOfDrivers() : Integer;
begin
  If Not(FEngineInitialized) then exit;
  result := Sound.GetNumberOfDrivers();
end;

{******************************************************************************}
{* Get the name string of a driver                                            *}
{******************************************************************************}

function gdSoundGetDriverName( aDriverNumber : Integer ) : String;
begin
  If Not(FEngineInitialized) then exit;
  result :=  String(Sound.GetDriverName( aDriverNumber ));
end;

{******************************************************************************}
{* Initialize the soundsystem                                                 *}
{******************************************************************************}

function gdSoundInitDriver() : boolean;
begin
  result := Sound.InitSoundDriver()
end;

{******************************************************************************}
{* Shutdown the soundengine                                                    *}
{******************************************************************************}

function gdSoundShutDownDriver() : boolean;
begin
  result := Sound.ShutDownSoundDriver();
end;

{******************************************************************************}
{* Keystring to key                                                           *}
{******************************************************************************}

function gdInputStringToKey( aName : String ) : Integer ;
begin
  If Not(FEngineInitialized) then exit;
  result := Input.StringToKey( aName );
end;

{******************************************************************************}
{* Key to keytring                                                            *}
{******************************************************************************}

function gdInputKeyToString( aKey : Integer ) : String ;
begin
  If Not(FEngineInitialized) then exit;
  result := String(Input.KeyToString( aKey ));
end;

{******************************************************************************}
{* Detect which Key is currently pressed and returns the keystring            *}
{******************************************************************************}

function gdInputDetectCurrentKeyString() : String ;
begin
  If Not(FEngineInitialized) then exit;
  result := String(Input.DetectCurrentKeyString());
end;

{******************************************************************************}
{* Detect which Key is currently pressed and returns it                       *}
{******************************************************************************}

function gdInputDetectCurrentKey() : Integer ;
begin
  If Not(FEngineInitialized) then exit;
  result := Input.DetectCurrentKey();
end;

{******************************************************************************}
{* Register a keyaction                                                       *}
{******************************************************************************}

procedure gdInputRegisterAction(aType : TGDInputTypes; aKeyString : String ; aAction : TGDProcEngineCallback;  aConsoleDisabled : boolean );
begin
  If Not(FEngineInitialized) then exit;
  Input.RegisterInputAction(aType, aKeyString, aAction, aConsoleDisabled );
end;

{******************************************************************************}
{* Pas down a char the engine (used for typing and such)                      *}
{******************************************************************************}

procedure gdInputHandleChar( aChar : Char );
begin
  If Not(FEngineInitialized) then exit;
  Input.ExecuteCharInput( aChar );
end;

{******************************************************************************}
{* Toggle use of mouselook                                                    *}
{******************************************************************************}

procedure gdInputUseMouseLook( aUse : boolean );
begin
  If Not(FEngineInitialized) then exit;
  Input.MouseLook := aUse;
end;

{******************************************************************************}
{* Enable or disable input                                                    *}
{******************************************************************************}

procedure gdInputEnable( aEnable : boolean );
begin
  If Not(FEngineInitialized) then exit;
  Input.EnableInput := aEnable;
end;

{******************************************************************************}
{* Show or hide the mouse cursor                                              *}
{******************************************************************************}

procedure gdGUIMouseCursorShow(aShow : boolean); 
begin
  If Not(FEngineInitialized) then exit;
  Input.CalculateMousePosStart();
  GUI.MouseCursor.ShowMouse := aShow;
end;

{******************************************************************************}
{* retrieve mouse cursor position                                             *}
{******************************************************************************}

function gdGUIMouseCursorGetPosition() : TPoint; 
begin
  If Not(FEngineInitialized) then exit;
  result.X := GUI.MouseCursor.Position.X;
  result.Y := GUI.MouseCursor.Position.Y;
end;

{******************************************************************************}
{* Setup the loadingscreen                                                    *}
{******************************************************************************}

procedure gdGUILoadingScreenSetup( aProcessName : String; aMax : Integer ); 
begin
  If Not(FEngineInitialized) then exit;
  GUI.LoadingScreen.SetupForUse(String(aProcessName),aMax);
end;

{******************************************************************************}
{* Update the loadingscreen                                                   *}
{******************************************************************************}

procedure gdGUILoadingScreenUpdate(); 
begin
  If Not(FEngineInitialized) then exit;
  GUI.LoadingScreen.UpdateBar();
end;

{******************************************************************************}
{* Set text color                                                             *}
{******************************************************************************}

procedure gdGUITextColor(aR,aG,aB : Double);
begin
  If Not(FEngineInitialized) then exit;
  GUI.Font.Color.Reset(aR,aG,aB, 1);
end;

{******************************************************************************}
{* Render a text                                                              *}
{******************************************************************************}

procedure gdGUITextRender(aX,aY,aScale : Double; aText : String);
begin
  If Not(FEngineInitialized) then exit;
  GUI.Font.Render(aX,aY,aScale,aText);
end;

{******************************************************************************}
{* Initialize the camera                                                      *}
{******************************************************************************}

procedure gdCameraInit(aX,aY,aZ : double); 
begin
  If Not(FEngineInitialized) then exit;
  Camera.InitCamera(aX,aY,aZ);
end;

{******************************************************************************}
{* Set the camera position                                                     *}
{******************************************************************************}

procedure gdCameraSetPosition(aX,aY,aZ : double); 
begin
  If Not(FEngineInitialized) then exit;
  Camera.Position.X := aX;
  Camera.Position.Y := aY;
  Camera.Position.Z := aZ;
end;

{******************************************************************************}
{* Get the camera position                                                     *}
{******************************************************************************}

function gdCameraGetPosition() : TGDVector;
begin
  If Not(FEngineInitialized) then exit;
  result := Camera.Position.Copy();
end;

{******************************************************************************}
{* Set the camera direction                                                   *}
{******************************************************************************}

procedure gdCameraSetDirection(aX,aY,aZ : double); 
begin
  If Not(FEngineInitialized) then exit;
  Camera.Direction.Reset(aX, aY, aZ);
end;

{******************************************************************************}
{* Get the camera direction                                                   *}
{******************************************************************************}

function gdCameraGetDirection() : TGDVector;
begin
  If Not(FEngineInitialized) then exit;
  result := Camera.Direction.Copy();
end;

{******************************************************************************}
{* Move camera forward or backwards                                           *}
{******************************************************************************}

procedure gdCameraMove(aStep : double); 
begin
  If Not(FEngineInitialized) then exit;
  Camera.Move(aStep);
end;

{******************************************************************************}
{* Strafe camera right or left                                                *}
{******************************************************************************}

procedure gdCameraStrafe(aStep : double); 
begin
  If Not(FEngineInitialized) then exit;
  Camera.Strafe(aStep);
end;


{******************************************************************************}
{* Load settings out an ini-file                                              *}
{******************************************************************************}

procedure gdSettingsLoad();
begin
  If Not(FEngineInitialized) then exit;
  Settings.LoadIniFile();
end;

{******************************************************************************}
{* Save settings to an ini-file                                               *}
{******************************************************************************}

procedure gdSettingsSave();
begin
  If Not(FEngineInitialized) then exit;
  Settings.SaveIniFile();
end;

{******************************************************************************}
{* Retrieve the current settings                                              *}
{******************************************************************************}

function  gdSettingsGetCurrent() : TSettings; 
begin
  If Not(FEngineInitialized) then exit;
  result := Settings.GetSettings();
end;

{******************************************************************************}
{* Set the current settings                                                   *}
{******************************************************************************}

procedure gdSettingsSetCurrent(aSettings : TSettings); 
begin
  If Not(FEngineInitialized) then exit;
  Settings.SetSettings(aSettings);
end;

{******************************************************************************}
{* Toggle the console                                                         *}
{******************************************************************************}

procedure gdConsoleToggle();
begin
  If Not(FEngineInitialized) then exit;
  Console.Show := Not(Console.Show);
end;

{******************************************************************************}
{* Excute console command                                                     *}
{******************************************************************************}

procedure gdConsoleCommand(aCommand : String );
begin
  If Not(FEngineInitialized) then exit;
  Console.Command := aCommand;
  Console.ExecuteCommand();
end;

{******************************************************************************}
{* Add a text to the console log                                              *}
{******************************************************************************}

procedure gdConsoleLog(aText : String; aNewLine : boolean = true);
begin
  If Not(FEngineInitialized) then exit;
  If aText = '' then exit;
  Console.Write(aText, aNewLine);
end;

{******************************************************************************}
{* Start timer                                                                *}
{******************************************************************************}

procedure gdTimingStart();
begin
  If Not(FEngineInitialized) then exit;
  Timing.Start();
end;

{******************************************************************************}
{* Stop timer                                                                 *}
{******************************************************************************}

procedure gdTimingStop();
begin
  If Not(FEngineInitialized) then exit;
  Timing.Stop();
end;

{******************************************************************************}
{* Get time in seconds                                                        *}
{******************************************************************************}

function gdTimingInSeconds() : String;
begin
  If Not(FEngineInitialized) then exit;
  result := Timing.TimeInSeconds();
end;

{******************************************************************************}
{* Get time in millids                                                        *}
{******************************************************************************}

function gdTimingInMilliSeconds() : String;
begin
  If Not(FEngineInitialized) then exit;
  result := Timing.TimeInMilliSeconds();
end;

{******************************************************************************}
{* Main render loop of the engine                                             *}
{******************************************************************************}

function gdTimingFrameTime() : Integer;
begin
  result := 0;
  If Not(FEngineInitialized) then exit;
  result := Timing.FrameTime;
end;

{******************************************************************************}
{* Sets the RenderInterface callback function                                 *}
{******************************************************************************}

procedure gdCallBackSetInterfaceRenderer( aFunction : TGDProcEngineCallback ); 
begin
  If Not(FEngineInitialized) then exit;
  RenderInterfaceCallBack := aFunction;
end;

{******************************************************************************}
{* Sets the beforerender callback function                                    *}
{******************************************************************************}

procedure gdCallBackSetBeforeRender( aFunction : TGDProcEngineCallback ); 
begin
  If Not(FEngineInitialized) then exit;
  AfterRenderCallBack := aFunction;
end;

{******************************************************************************}
{* Sets the afterrender callback function                                     *}
{******************************************************************************}

procedure gdCallBackSetAfterRender( aFunction : TGDProcEngineCallback ); 
begin
  If Not(FEngineInitialized) then exit;
  BeforeRenderCallBack := aFunction;
end;

{******************************************************************************}
{* Main loop of the engine                                                    *}
{******************************************************************************}

procedure gdLoopMain(); 
begin
  If Not(FEngineInitialized) then exit;
  Main.Main();
end;

{******************************************************************************}
{* Load map                                                                   *}
{******************************************************************************}

function gdMapLoad( aFileName : String ) : boolean; 
begin
  result := false;
  If Not(FEngineInitialized) then exit;
  Octree.Clear();
  CellManager.Cells.Clear();
  result := Map.InitMap( aFileName );
  CellManager.GenerateAllCells();
  Octree.InitOcTree();
  Camera.InitCamera(Map.PlayerStart.X,Map.PlayerStart.Y,Map.PlayerStart.Z);
  Camera.Rotation.x := Map.PlayerViewAngle.X;
  Camera.Rotation.y := Map.PlayerViewAngle.y;
  Camera.Rotation.z := Map.PlayerViewAngle.z;
  Camera.MouseLook(0,0,1,1,0,False);
end;

{******************************************************************************}
{* Clear map                                                                  *}
{******************************************************************************}

procedure gdMapClear(); 
begin
  If Not(FEngineInitialized) then exit;
  Map.Clear();
  CellManager.Clear();
  Octree.Clear();
end;

{******************************************************************************}
{* Return the height of the terrain at a point                                *}
{******************************************************************************}

function gdMapTerrainHeight(aX, aZ  : Double) : Double; 
var
  iHeight : Double;
begin
  result := 0;
  If Not(FEngineInitialized) then exit;
  Terrain.GetHeight( aX, aZ, iHeight );
  result := iHeight;
end;

{******************************************************************************}
{* Return the rotation of the terrain at a point                              *}
{******************************************************************************}

function  gdMapTerrainRotation(aX, aZ : Double) : TGDVector;
var
  iRotation : TGDVector;
begin
  iRotation := TGDVector.Create();
  If Not(FEngineInitialized) then exit;
  Terrain.GetRotation( aX, aZ, iRotation );
  result := iRotation;
end;

{******************************************************************************}
{* return the waterheight                                                     *}
{******************************************************************************}

function gdMapWaterHeight(): Double; 
begin
  result := 0;
  If Not(FEngineInitialized) then exit;
  result := Water.WaterHeight;
end;

{******************************************************************************}
{* Load a texture from a file and return the retrievel index                  *}
{******************************************************************************}

function gdTexturesLoad( aFileName : String ) : pointer; 
var
  iTempTexture : TGDTexture;
begin
  result := nil;
  If Not(FEngineInitialized) then exit;
  iTempTexture := TGDTexture.Create();
  If Not(iTempTexture.InitTexture( aFileName, Settings.TextureDetail,
          Settings.TextureFilter)) then exit;
  TextureList.Add( iTempTexture );
  result := iTempTexture;
end;

{******************************************************************************}
{* Bind texture to a texture unit                                             *}
{******************************************************************************}

procedure gdTexturesBind(aPointer : pointer; aTextureUnit : GLEnum );
begin
  If Not(FEngineInitialized) then exit;
  TGDTexture(aPointer).BindTexture(aTextureUnit); ;
end;

{******************************************************************************}
{* remove a texture in the engine                                             *}
{******************************************************************************}

procedure gdTexturesRemove( aPointer : pointer ); 
begin
  If Not(FEngineInitialized) then exit;
  TextureList.Remove(aPointer);
  aPointer := nil
end;

{******************************************************************************}
{* Bind texture to a texture unit                                             *}
{******************************************************************************}

procedure gdTexturesClear(); 
begin
  If Not(FEngineInitialized) then exit;
  TextureList.Clear();
end;

{******************************************************************************}
{* Init a soundfile                                                           *}
{******************************************************************************}

function  gdSoundFilesLoad( aFileName : String;  aType : TGDSoundTypes  ) : pointer; 
var
  iTempSoundFile : TGDSoundFile;
begin
  result := nil;
  If Not(FEngineInitialized) then exit;
  iTempSoundFile := TGDSoundFile.Create();
  If Not(iTempSoundFile.InitSoundFile( aFileName, aType )) then exit;
  SoundList.Add( iTempSoundFile );
  result := iTempSoundFile;
end;

{******************************************************************************}
{* Clear all soundfiles                                                       *}
{******************************************************************************}

procedure gdSoundFilesClear(); 
begin
  If Not(FEngineInitialized) then exit;
  SoundList.Clear();
end;

{******************************************************************************}
{* Remove soundfile                                                           *}
{******************************************************************************}

procedure gdSoundFilesRemove( aPointer : pointer ); 
begin
  If Not(FEngineInitialized) then exit;
  SoundList.Remove(aPointer);
  aPointer := nil
end;

{******************************************************************************}
{* Play a soundfile                                                           *}
{******************************************************************************}

procedure gdSoundFilesPlay( aPointer  : pointer ); 
var
  iTempSoundFile : TGDSoundFile;
begin
  If Not(FEngineInitialized) then exit;
  iTempSoundFile := TGDSoundFile(aPointer);
  If iTempSoundFile <> nil then
    iTempSoundFile.Play();
  ;
end;

{******************************************************************************}
{* Pause a soundfile                                                          *}
{******************************************************************************}

procedure gdSoundFilesPause( aPointer  : pointer ); 
var
  iTempSoundFile : TGDSoundFile;
begin
  If Not(FEngineInitialized) then exit;
  iTempSoundFile := TGDSoundFile(aPointer);
  If iTempSoundFile <> nil then
    iTempSoundFile.Pause();
  ;
end;

{******************************************************************************}
{* Resume playing of a soundfile                                              *}
{******************************************************************************}

procedure gdSoundFilesResume( aPointer  : pointer ); 
var
  iTempSoundFile : TGDSoundFile;
begin
  If Not(FEngineInitialized) then exit;
  iTempSoundFile := TGDSoundFile(aPointer);
  If iTempSoundFile <> nil then
    iTempSoundFile.Resume();
  ;
end;

end.
