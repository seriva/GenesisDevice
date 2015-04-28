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
unit GDMain;

{$MODE Delphi}

{******************************************************************************}
{* This is the main unit of the engine. It holds the main variables,          *}
{* callbacks and loops controling the engine. This unit will later be extended*}
{* with multible threads for the different systems                            *}
{******************************************************************************}

interface

uses
  LCLIntf, LCLType,
  SysUtils,
  IniFiles,
  Windows,
  dglOpenGL,
  GDSettings,
  GDFont,
  GDRenderer,
  GDConsole,
  GDTiming,
  GDConstants,
  GDFog,
  GDWater,
  GDCamera,
  GDInput,
  GDGUI,
  GDMap,
  GDSkyDome,
  GDFrustum,
  GDSound,
  GDOctree,
  GDCellManager,
  GDModes,
  GDCallBack,
  GDStatistics;

type

{******************************************************************************}
{* Main class                                                                 *}
{******************************************************************************}

  TGDMain  = Class
  private
  public
    constructor Create();
    destructor  Destroy(); override;

    procedure InitBaseResources();
    procedure ClearBaseResources();

    procedure Main();
    procedure RenderMain();
    procedure InputMain();
    procedure SoundMain();
  end;

var
  Main : TGDMain;

implementation

{******************************************************************************}
{* Create main class                                                          *}
{******************************************************************************}

constructor TGDMain.Create();
begin
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
var
  iIniFile      : TIniFile;
  iPathString   : String;
  iLoadingInput : TGDLoadingInput;

begin
  Timing.Start();
  Console.Write('......Initializing base recources');
  iPathString := FP_INITS + ENGINE_INI;
  iIniFile := TIniFile.Create( iPathString );

  //init system font
  Font.InitFont( iIniFile.ReadString('Base', 'SystemFont', 'console.fnt') );

  //init the mouse
  GUIManager.MouseCursor.InitMouse( iIniFile.ReadString('Base', 'MouseCursor', 'mouse.bmp'),40);
  ShowCursor(false);

  //init the console
  Console.InitConsole();

  //init the loadingscreen
  iLoadingInput.X := 500;
  iLoadingInput.Y := 575;
  iLoadingInput.BarR := 0.0;
  iLoadingInput.BarG := 0.0;
  iLoadingInput.BarB := 0.7;
  iLoadingInput.BarA := 1.0;
  iLoadingInput.LineR := 1.0;
  iLoadingInput.LineG := 1.0;
  iLoadingInput.LineB := 1.0;
  iLoadingInput.LineA := 1.0;
  iLoadingInput.BackR := 0.3;
  iLoadingInput.BackG := 0.3;
  iLoadingInput.BackB := 0.3;
  iLoadingInput.BackA := 1.0;
  GUIManager.LoadingScreen.InitLoadingScreen( iLoadingInput );

  FreeAndNil(iIniFile);

  //init the camera
  Camera.InitCamera(0, 8192, 32768);

  //init and timing
  Statistics.InitStatistics();
  Modes.InitModes();

  Timing.Stop();
  Console.Write('......Done initializing base resources (' + Timing.TimeInSeconds + ' Sec)');
end;

{******************************************************************************}
{* Clear the base resources                                                   *}
{******************************************************************************}

procedure TGDMain.ClearBaseResources();
begin
  Input.ClearInputActions();
  Font.Clear();
  Console.Clear();
  Octree.Clear();
  ShowCursor(true);
  CellManager.Clear();
  GUIManager.Clear();
  Map.Clear();
  SoundList.Clear();
end;

{******************************************************************************}
{* Main loop of the engine                                                    *}
{******************************************************************************}

procedure TGDMain.Main();
begin
  //start timing
  Statistics.FrameStart();
  Timing.CalculateFrameTime();

  //input
  InputMain();

  //sound
  SoundMain();

  //rendering
  RenderMain();

  //end timing
  Statistics.FrameStop();
end;

{******************************************************************************}
{* The main renderloop                                                        *}
{******************************************************************************}

procedure TGDMain.RenderMain();

{******************************************************************************}
{* Render debug                                                               *}
{******************************************************************************}

Procedure RenderDebug();
begin
  glLoadIdentity();
  Camera.Translate();
  Renderer.RenderState( RS_COLOR );
  If Modes.RenderNormals     then CellManager.RenderVisibleCells( RA_NORMALS, RF_NORMAL );
  If Modes.RenderObjectBoxes then CellManager.RenderVisibleCells( RA_FRUSTUM_BOXES, RF_NORMAL );
  If Modes.RenderNodeBoxes   then Octree.RenderTreeBoxes();
end;

{******************************************************************************}
{* Render water reflections                                                   *}
{******************************************************************************}

procedure RenderWaterReflection();
begin
  If (Modes.RenderWireframe = false) and Water.Visible() then
  begin
    //render reflection texture
    Renderer.StartFrame();
    Camera.Translate();
    Water.StartReflection();
    If Modes.RenderSky then SkyDome.Render();
    Frustum.CalculateFrustum();
    CellManager.DetectVisibleCells();
    CellManager.RenderVisibleCells( RA_NORMAL, RF_WATER );
    Water.EndReflection();
  end;
end;

{******************************************************************************}
{* Render ortho                                                               *}
{******************************************************************************}

procedure RenderOrtho();
begin
  //rendering 2d stuff (console,stats,interfaces, menus enz)
  Renderer.SwitchToOrtho();
    If Modes.RenderWireframe = false then Water.RenderUnderWater();
    CallBack.RenderInterface();
    If Modes.RenderStats then Statistics.Render();
    Console.Render();
    GUIManager.MouseCursor.Render();
  Renderer.SwitchToPerspective();
end;

{******************************************************************************}
{* Render static geometry                                                     *}
{******************************************************************************}

Procedure RenderStaticGeometry();
begin
 //Render sky
 FogManager.UseDistanceFog();
 SkyDome.Render();

 //Set the right fog type
 If not(Camera.Position.Y > Water.WaterHeight) then
   FogManager.UseWaterFog();

 //Render other cells.
 CellManager.RenderVisibleCells( RA_NORMAL, RF_NORMAL );
end;

{******************************************************************************}
{* Render source image                                                        *}
{******************************************************************************}

procedure RenderSourceImage();
begin
  Renderer.StartRenderSource();
  RenderStaticGeometry();
  Renderer.EndRenderSource();
end;

{******************************************************************************}
{* Render underwater source image                                             *}
{******************************************************************************}

procedure RenderUnderWaterSourceImage();
begin
  Renderer.StartRenderUnderWaterSource();
  RenderStaticGeometry();
  Renderer.EndRenderUnderWaterSource();
end;

{******************************************************************************}
{* Render bloom image                                                         *}
{******************************************************************************}

procedure RenderBloomImage();
begin
  Renderer.StartRenderBloom();
  If Modes.RenderSky then SkyDome.Render();
  CellManager.RenderVisibleCells( RA_NORMAL, RF_BLOOM );
  Renderer.EndRenderBloom();
end;

begin
  //do before render and start timing
  CallBack.BeforeRender();

  //make renderer current
  Renderer.MakeCurrent();

  //create the water reflection texture
  RenderWaterReflection();

  //detect the visibel objects
  glLoadIdentity();
  Camera.Translate();
  Frustum.CalculateFrustum();
  CellManager.DetectVisibleCells();

  //set the current rendermode
  if not(Modes.RenderWireframe) then
  begin
    FogManager.FogShader.ApplyFog();

    //check if where underwater
    if Water.UnderWater() then
    begin
      //render the underwater source image
      RenderUnderWaterSourceImage();
    end
    else
    begin
      //render the source image
      RenderSourceImage();

      //render bloom image and apply bloom shader
      If Settings.UseBloom then RenderBloomImage();
    end;

    //render the final image
    Renderer.StartFrame();
    Renderer.RenderFinal();
  end
  else
  begin
    Renderer.RenderState( RS_WIREFRAME );
    Renderer.StartFrame();
    Camera.Translate();
    SkyDome.Render();
    CellManager.RenderVisibleCells( RA_NORMAL, RF_NORMAL );
  end;

  //render debug and ortho stuff
  RenderDebug();
  RenderOrtho();

  //end the frame and increment the framecounter
  Renderer.EndFrame();
  Statistics.FrameCount:= Statistics.FrameCount + 1;

  //do after rendering and end timing
  CallBack.AfterRender();
end;

{******************************************************************************}
{* The main input loop                                                        *}
{******************************************************************************}

procedure TGDMain.InputMain();
begin
  Input.ExecuteMouseMove();
  Input.ExecuteDirectInput();
end;

{******************************************************************************}
{* The main sound loop                                                        *}
{******************************************************************************}

procedure TGDMain.SoundMain();
begin
  If Not(Settings.MuteSound) then
  begin
    Sound.UpdateSound();
  end;
end;

end.
