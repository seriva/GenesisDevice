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
unit GDSettings;

{$MODE Delphi}

{******************************************************************************}
{* Holds the settings class. It detects the current system and checks if it   *}
{* meets the requirements. It also has the settings for the engine.           *}
{******************************************************************************}

interface

uses
  Dialogs,
  Windows,
  Forms,
  Classes,
  SysUtils,
  IniFiles,
  dglOpenGL,
  GDConstants,
  GDLog,
  FileUtil;

type

{******************************************************************************}
{* Record that holds the maximum system settings                              *}
{******************************************************************************}

  TMaximumSettings = record
    MaxTextureUnits       : Integer;
    MaxAnisotropicFilter  : Extended;
    MaxtextureSize        : Extended;
    SupportGLSL           : Boolean;
    SupportFBO            : Boolean;
  end;

{******************************************************************************}
{* Records that holds the current settings                                    *}
{******************************************************************************}

  TSettings = record
    //viewport settings
    Width            : Integer;
    Height           : Integer;
    FullScreen       : Boolean;
    VerticalSync     : Boolean;
    Gamma            : Double;

    //render settings
    ViewDistance     : Integer;
    GrassDistance    : Integer;
    GrassDensity     : Integer;
    TextureDetail    : String;
    WaterDetail      : String;
    WaterReflection  : String;
    TextureFilter    : String;
    UseBloom         : Boolean;
    UseFXAA          : Boolean;

    //input settings
    InvertMouse : Boolean;
    MouseSensitivity : Integer;

    //sound settings
    SoundDriver : Integer;
    MuteSound   : Boolean;
    MusicVolume : Double;
    SoundVolume : Double;
  end;

{******************************************************************************}
{* Settings class                                                             *}
{******************************************************************************}

  TGDSettings = class
  private
    FApplicationFilePath : String;
    FApplicationFileName : String;

    //max supported render settings
    FMaxTextureUnits       : GLInt;
    FMaxAnisotropicFilter  : GLFLoat;
    FMaxtextureSize        : GLFLoat;
    FSupportGLSL           : Boolean;
    FSupportFBO            : Boolean;

    //viewport settings
    FTop              : Integer;
    FLeft             : Integer;
    FWidth            : Integer;
    FHeight           : Integer;
    FFullScreen       : Boolean;
    FVerticalSync     : Boolean;
    FGamma            : Double;

    //render settings
    FViewDistance     : Integer;
    FGrassDistance    : Integer;
    FGrassDensity     : Integer;
    FTextureDetail    : TGDTextureDetail;
    FWaterDetail      : TGDWaterDetail;
    FWaterReflection  : TGDWaterReflection;
    FTextureFilter    : TGDTextureFilter;
    FUseBloom         : Boolean;
    FUseFXAA          : Boolean;

    //input settings
    FInvertMouse      : Boolean;
    FMouseSensitivity : Integer;

    //sound settings
    FSoundDriver : Integer;
    FMuteSound   : boolean;
    FMusicVolume : Double;
    FSoundVolume : Double;

    procedure SetTextureDetail(aStr : String);
    function  GetTextureDetail() : String;
    procedure SetWaterDetail(aStr : String);
    function  GetWaterDetail() : String;
    procedure SetWaterReflectionDetail(aStr : String);
    function  GetWaterReflectionDetail() : String;
    procedure SetTextureFilter(aStr : String);
    function  GetTextureFilter() : String;
  public
    property ApplicationFilePath : String read FApplicationFilePath write FApplicationFilePath;
    property ApplicationFileName : String read FApplicationFileName write FApplicationFileName;

    //max supported render settings
    property MaxTextureUnits : GLInt read FMaxTextureUnits;
    property MaxAnisotropicFilter : GLFLoat read FMaxAnisotropicFilter;
    property MaxtextureSize : GLFLoat read FMaxtextureSize;

    //viewport settings
    property Top : Integer read FTop write FTop;
    property Left : Integer read FLeft write FLeft;
    property Width : Integer read FWidth write FWidth;
    property Height : Integer read FHeight write FHeight;
    property FullScreen : Boolean read FFullScreen write FFullScreen;
    property VerticalSync : Boolean read FVerticalSync write FVerticalSync;
    property Gamma : double read FGamma write FGamma;

    //render settings
    property ViewDistance : Integer read FViewDistance write FViewDistance;
    property GrassDistance : Integer read FGrassDistance write FGrassDistance;
    property GrassDensity : Integer read FGrassDensity write FGrassDensity;
    property WaterDetail : TGDWaterDetail read FWaterDetail write FWaterDetail;
    property WaterReflection : TGDWaterReflection read FWaterReflection write FWaterReflection;
    property TextureDetail : TGDTextureDetail read FTextureDetail write FTextureDetail;
    property TextureFilter : TGDTextureFilter read FTextureFilter write FTextureFilter;
    property UseBloom : Boolean read FUseBloom write FUseBloom;
    property UseFXAA : Boolean read FUseFXAA write FUseFXAA;

    //input settings
    property InvertMouse : Boolean read FInvertMouse write FInvertMouse;
    property MouseSensitivity : Integer read FMouseSensitivity write FMouseSensitivity;

    //sound settings
    property SoundDriver : Integer read FSoundDriver write FSoundDriver;
    property MuteSound : Boolean read FMuteSound write FMuteSound;
    property MusicVolume : Double read FMusicVolume write FMusicVolume;
    property SoundVolume : Double read FSoundVolume write FSoundVolume;

    constructor Create();
    destructor  Destroy(); override;

    procedure SafeSettings();
    procedure LowSettings();
    procedure MediumSettings();
    procedure HighSettings();
    procedure LoadIniFile( aIniFile : String );
    procedure SaveIniFile( aIniFile : String );
    function  GetSettings() : TSettings;
    procedure SetSettings(aSettings : TSettings);
    function  GetMaximumSettings() : TMaximumSettings;

    function DetectOpengl(): boolean;

    function CheckCapabilities() : boolean;
    function CheckFileSystem(): boolean;
  end;

var
  Settings : TGDSettings;

implementation

{******************************************************************************}
{* Create the settings class                                                  *}
{******************************************************************************}

constructor TGDSettings.Create();
begin
  FApplicationFilePath := '';
  FApplicationFileName := '';

  SafeSettings();

  //set right en left
  FTop := 0;
  FLeft := 0;
  FGamma := 0.60;

  //input
  FInvertMouse      := False;
  FMouseSensitivity := 5;

  //sound
  FSoundDriver := 0;
  FMuteSound   := false;
  FMusicVolume := 0.3;
  FSoundVolume := 0.7;
end;

{******************************************************************************}
{* Destroy the settings class                                                 *}
{******************************************************************************}

destructor  TGDSettings.Destroy();
begin
  inherited;
end;

{******************************************************************************}
{* Set the settings to high                                                   *}
{******************************************************************************}

procedure TGDSettings.HighSettings();
begin
  //viewport settings
  FWidth            := 800;
  FHeight           := 600;
  FFullScreen       := true;
  FVerticalSync     := true;

  //renderer
  FViewDistance     := 9;
  FGrassDistance    := 5;
  FGrassDensity     := 20;
  FTextureDetail    := TD_HIGH;
  FWaterDetail      := WD_HIGH;
  FWaterReflection  := WR_ALL;
  FTextureFilter    := TF_AF16;
  FUseBloom         := true;
  FUseFXAA          := true;
end;

{******************************************************************************}
{* Set the settings to medium                                                 *}
{******************************************************************************}

procedure TGDSettings.MediumSettings();
begin
  //viewport settings
  FWidth            := 800;
  FHeight           := 600;
  FFullScreen       := True;
  FVerticalSync     := True;

  //renderer
  FViewDistance     := 7;
  FGrassDistance    := 3;
  FGrassDensity     := 15;
  FTextureDetail    := TD_MEDIUM;
  FWaterDetail      := WD_MEDIUM;
  FWaterReflection  := WR_TERRAIN_ONLY;
  FTextureFilter    := TF_AF8;
  FUseBloom         := true;
  FUseFXAA          := true;
end;

{******************************************************************************}
{* Set the settings to low                                                    *}
{******************************************************************************}

procedure TGDSettings.LowSettings();
begin
  //viewport settings
  FWidth            := 800;
  FHeight           := 600;
  FFullScreen       := true;
  FVerticalSync     := true;

  //renderer
  FViewDistance     := 5;
  FGrassDistance    := 1;
  FGrassDensity     := 10;
  FTextureDetail    := TD_LOW;
  FWaterDetail      := WD_LOW;
  FWaterReflection  := WR_TERRAIN_ONLY;
  FTextureFilter    := TF_TRILINEAR;
  FUseBloom         := false;
  FUseFXAA          := false;
end;

{******************************************************************************}
{* Set the settings to safe                                                   *}
{******************************************************************************}

procedure TGDSettings.SafeSettings();
begin
  //viewport settings
  FWidth            := 800;
  FHeight           := 600;
  FFullScreen       := false;
  FVerticalSync     := false;

  //renderer
  FViewDistance     := 5;
  FGrassDistance    := 1;
  FGrassDensity     := 10;
  FTextureDetail    := TD_LOW;
  FWaterDetail      := WD_LOW;
  FWaterReflection  := WR_TERRAIN_ONLY;
  FTextureFilter    := TF_BILINEAR;
  FUseBloom         := false;
  FUseFXAA          := false;
end;

{******************************************************************************}
{* Load the settings from an ini-file                                         *}
{******************************************************************************}

procedure TGDSettings.LoadIniFile( aIniFile : String );
var
  iIniFile : TIniFile;
  iStr : String;
begin
  iIniFile := TIniFile.Create( FApplicationFilePath + aIniFile );

  //viewport settings
  FWidth :=        iIniFile.ReadInteger('ViewPort', 'Width', 800);
  FHeight :=       iIniFile.ReadInteger('ViewPort', 'Height', 600);
  FFullScreen :=   iIniFile.ReadBool('ViewPort', 'Fullscreen', False);
  FVerticalSync := iIniFile.ReadBool('ViewPort', 'VerticalSync', False);
  FGamma :=        iIniFile.ReadFloat('ViewPort', 'Gamma', 0.60);

  //render settings
  FViewDistance  := iIniFile.ReadInteger('Renderer', 'ViewDistance', 5);
  FGrassDistance := iIniFile.ReadInteger('Renderer', 'GrassDistance', 1);
  FGrassDensity := iIniFile.ReadInteger('Renderer',  'GrassDensity', 5);
  iStr := iIniFile.ReadString('Renderer', 'TextureDetail', TGDTextureDetailStrings[1]);
  SetTextureDetail( iStr );
  iStr := iIniFile.ReadString('Renderer', 'TextureFilter', TGDTextureFilterStrings[1]);
  SetTextureFilter(iStr);
  iStr := iIniFile.ReadString('Renderer', 'WaterDetail', TGDWaterDetailStrings[1]);
  SetWaterDetail(iStr);
  iStr := iIniFile.ReadString('Renderer', 'WaterReflection', TGDWaterReflectionStrings[1]);
  SetWaterReflectionDetail(iStr);
  FUseBloom         := iIniFile.ReadBool( 'Renderer', 'UseBloom', False);
  FUseFXAA          := iIniFile.ReadBool( 'Renderer', 'UseFXAA', False);

  //input settings
  FInvertMouse      := iIniFile.ReadBool( 'Input', 'InvertMouse', False);
  FMouseSensitivity := iIniFile.ReadInteger('Input', 'MouseSensitivity ', 5);

  //sound settings
  FSoundDriver := iIniFile.ReadInteger( 'Sound', 'Driver', 0);
  FMuteSound   := iIniFile.ReadBool( 'Sound', 'Mute', False);
  FMusicVolume := iIniFile.ReadFloat( 'Sound', 'MusicVolume', 0.3);
  FSoundVolume := iIniFile.ReadFloat( 'Sound', 'SoundVolume', 0.7);
  
  FreeAndNil(iIniFile)
end;

{******************************************************************************}
{* Saves the settings to an ini-files                                         *}
{******************************************************************************}

procedure TGDSettings.SaveIniFile( aIniFile : String );
var
  iIniFile : TIniFile;
begin
  iIniFile := TIniFile.Create( FApplicationFilePath + aIniFile);

  //viewport
  iIniFile.WriteInteger('ViewPort', 'Width', FWidth);
  iIniFile.WriteInteger('ViewPort', 'Height', FHeight);
  iIniFile.WriteBool('ViewPort', 'Fullscreen', FFullScreen);
  iIniFile.WriteBool('ViewPort', 'VerticalSync', FVerticalSync);
  iIniFile.WriteFloat('ViewPort', 'Gamma', FGamma);

  //render settings
  iIniFile.WriteInteger('Renderer', 'ViewDistance', FViewDistance);
  iIniFile.WriteInteger('Renderer', 'GrassDistance', FGrassDistance);
  iIniFile.WriteInteger('Renderer', 'GrassDensity', FGrassDensity);
  iIniFile.WriteString('Renderer', 'TextureDetail', GetTextureDetail());
  iIniFile.WriteString('Renderer', 'TextureFilter', GetTextureFilter());
  iIniFile.WriteString('Renderer', 'WaterDetail', GetWaterDetail());
  iIniFile.WriteString('Renderer', 'WaterReflection', GetWaterReflectionDetail());
  iIniFile.WriteBool( 'Renderer', 'UseBloom', FUseBloom );
  iIniFile.WriteBool( 'Renderer', 'UseFXAA', FUseFXAA );

  //input settings
  iIniFile.WriteBool( 'Input', 'InvertMouse', FInvertMouse);
  iIniFile.WriteInteger('Input', 'MouseSensitivity ', FMouseSensitivity);

  //sound settings
  iIniFile.WriteInteger( 'Sound', 'Driver', FSoundDriver);
  iIniFile.WriteBool( 'Sound', 'Mute', FMuteSound);
  iIniFile.WriteFloat( 'Sound', 'MusicVolume', FMusicVolume);
  iIniFile.WriteFloat( 'Sound', 'SoundVolume', FSoundVolume);

  FreeAndNil(iIniFile)
end;

{******************************************************************************}
{* Get the current settings                                                   *}
{******************************************************************************}

function  TGDSettings.GetSettings() : TSettings;
begin
  //viewport settings
  Result.Width            := FWidth;
  Result.Height           := FHeight;
  Result.FullScreen       := FFullScreen;
  Result.VerticalSync     := FVerticalSync;
  Result.Gamma            := FGamma;

  //render settings
  Result.ViewDistance     := FViewDistance;
  Result.GrassDistance    := FGrassDistance;
  Result.GrassDensity     := FGrassDensity;
  Result.TextureDetail    := GetTextureDetail();
  Result.WaterDetail      := GetWaterDetail();
  Result.WaterReflection  := GetWaterReflectionDetail();
  Result.TextureFilter    := GetTextureFilter();
  Result.UseBloom         := FUseBloom;
  Result.UseFXAA          := FUseFXAA;

  //input settings
  Result.InvertMouse      := FInvertMouse;
  Result.MouseSensitivity := FMouseSensitivity;

  //sound settings
  Result.SoundDriver := FSoundDriver;
  Result.MuteSound   := FMuteSound;
  Result.MusicVolume := FMusicVolume;
  Result.SoundVolume := FSoundVolume;
end;

{******************************************************************************}
{* Set the current settings                                                   *}
{******************************************************************************}

procedure TGDSettings.SetSettings(aSettings : TSettings);
begin
  //viewport settings
  FWidth            := aSettings.Width;
  FHeight           := aSettings.Height;
  FFullScreen       := aSettings.FullScreen;
  FVerticalSync     := aSettings.VerticalSync;
  FGamma            := aSettings.Gamma;

  //render settings
  FViewDistance     := aSettings.ViewDistance;
  FGrassDistance    := aSettings.GrassDistance;
  FGrassDensity     := aSettings.GrassDensity;
  SetTextureDetail( aSettings.TextureDetail );
  SetWaterDetail( aSettings.WaterDetail );
  SetWaterReflectionDetail( aSettings.WaterReflection );
  SetTextureFilter( aSettings.TextureFilter );
  FUseBloom         := aSettings.UseBloom;
  FUseFXAA          := aSettings.UseFXAA;

  //input settings
  FInvertMouse      := aSettings.InvertMouse;
  FMouseSensitivity := aSettings.MouseSensitivity;

  //sound settings
  FSoundDriver := aSettings.SoundDriver;
  FMuteSound   := aSettings.MuteSound;
  FMusicVolume := aSettings.MusicVolume;
  FSoundVolume := aSettings.SoundVolume;
end;

{******************************************************************************}
{* Set texture detail                                                         *}
{******************************************************************************}

procedure TGDSettings.SetTextureDetail(aStr : String);
begin
  If  aStr = TGDTextureDetailStrings[1] then FTextureDetail := TD_LOW    else
  If  aStr = TGDTextureDetailStrings[2] then FTextureDetail := TD_MEDIUM else
  If  aStr = TGDTextureDetailStrings[3] then FTextureDetail := TD_HIGH   else
  FTextureDetail := TD_LOW;
end;

{******************************************************************************}
{* Get texture detail                                                         *}
{******************************************************************************}

function TGDSettings.GetTextureDetail() : String;
begin
  case FTextureDetail of
     TD_LOW    : result := TGDTextureDetailStrings[1];
     TD_MEDIUM : result := TGDTextureDetailStrings[2];
     TD_HIGH   : result := TGDTextureDetailStrings[3];
     else result := TGDTextureDetailStrings[1];
  end;
end;

{******************************************************************************}
{* Set texture filter                                                         *}
{******************************************************************************}

procedure TGDSettings.SetTextureFilter(aStr : String);
begin
  If  aStr = TGDTextureFilterStrings[1] then FTextureFilter := TF_BILINEAR  else
  If  aStr = TGDTextureFilterStrings[2] then FTextureFilter := TF_TRILINEAR else
  If  aStr = TGDTextureFilterStrings[3] then FTextureFilter := TF_AF2  else
  If  aStr = TGDTextureFilterStrings[4] then FTextureFilter := TF_AF4  else
  If  aStr = TGDTextureFilterStrings[5] then FTextureFilter := TF_AF8  else
  If  aStr = TGDTextureFilterStrings[6] then FTextureFilter := TF_AF16 else
  FTextureFilter := TF_BILINEAR;
end;

{******************************************************************************}
{* Get texture filter                                                         *}
{******************************************************************************}

function TGDSettings.GetTextureFilter() : String;
begin
  case FTextureFilter of
     TF_BILINEAR  : Result := TGDTextureFilterStrings[1] ;
     TF_TRILINEAR : Result := TGDTextureFilterStrings[2] ;
     TF_AF2       : Result := TGDTextureFilterStrings[3] ;
     TF_AF4       : Result := TGDTextureFilterStrings[4] ;
     TF_AF8       : Result := TGDTextureFilterStrings[5] ;
     TF_AF16      : Result := TGDTextureFilterStrings[6] ;
     else result := TGDTextureFilterStrings[1];
  end;
end;

{******************************************************************************}
{* Set water detail                                                           *}
{******************************************************************************}

procedure TGDSettings.SetWaterDetail(aStr : String);
begin
  If  aStr = TGDWaterDetailStrings[1] then FWaterDetail := WD_LOW    else
  If  aStr = TGDWaterDetailStrings[2] then FWaterDetail := WD_MEDIUM else
  If  aStr = TGDWaterDetailStrings[3] then FWaterDetail := WD_HIGH   else
  FWaterDetail := WD_LOW;
end;

{******************************************************************************}
{* Get the water detail                                                       *}
{******************************************************************************}

function TGDSettings.GetWaterDetail() : String;
begin
  case FWaterDetail of
     WD_LOW    : Result := TGDWaterDetailStrings[1];
     WD_MEDIUM : Result := TGDWaterDetailStrings[2];
     WD_HIGH   : Result := TGDWaterDetailStrings[3];
     else result := 'Low';
  end;
end;

{******************************************************************************}
{* Set water reflection detail                                                *}
{******************************************************************************}

procedure TGDSettings.SetWaterReflectionDetail(aStr : String);
begin
  If  aStr = TGDWaterReflectionStrings[1] then WaterReflection := WR_TERRAIN_ONLY else
  If  aStr = TGDWaterReflectionStrings[2] then WaterReflection := WR_ALL else
  WaterReflection := WR_TERRAIN_ONLY;
end;

{******************************************************************************}
{* Get water reflection detail                                                *}
{******************************************************************************}

function TGDSettings.GetWaterReflectionDetail() : String;
begin
  case FWaterReflection of
     WR_TERRAIN_ONLY : Result := TGDWaterReflectionStrings[1];
     WR_ALL          : Result := TGDWaterReflectionStrings[2];
     else result := TGDWaterReflectionStrings[1];
  end;
end;

{******************************************************************************}
{* Get the maximum settings                                                   *}
{******************************************************************************}

function TGDSettings.GetMaximumSettings() : TMaximumSettings;
begin
    Result.MaxTextureUnits       := FMaxTextureUnits;
    Result.MaxAnisotropicFilter  := FMaxAnisotropicFilter;
    Result.MaxtextureSize        := FMaxtextureSize;
    Result.SupportGLSL           := FSupportGLSL;
    Result.SupportFBO            := FSupportFBO;
end;


{******************************************************************************}
{* Detect OpenGL                                                              *}
{******************************************************************************}

function TGDSettings.DetectOpengl(): boolean;
var
  iString1, iString2 : String;
  iI : Integer;
  iForm : TForm;
  iDC   : HDC;
  iRC   : HGLRC;
  iError    : string;

procedure glKillWnd();
begin
  wglMakeCurrent(iDC, 0);
  if (not wglDeleteContext(iRC)) then
    iRC := 0;


  if ((iDC > 0) and (ReleaseDC(iForm.Handle, iDC) = 0)) then
    iDC := 0;

  FreeAndNil(iForm);
end;

function glCreateWnd() : Boolean;
var
  iPixelFormat : GLuint;
  iPFD         : TPIXELFORMATDESCRIPTOR;
begin
  iForm := TForm.Create(nil);
  iDC := GetDC(iForm.Handle);
  if (iDC = 0) then
  begin
    glKillWnd();
    Result := False;
    Exit;
  end;

  with iPFD do
  begin
    nSize           := SizeOf(TPIXELFORMATDESCRIPTOR);
    nVersion        := 1;
    dwFlags         := PFD_DRAW_TO_WINDOW
                       or PFD_SUPPORT_OPENGL
                       or PFD_DOUBLEBUFFER;
    iPixelType      := PFD_TYPE_RGBA;
    cColorBits      := 32;
    cRedBits        := 0;
    cRedShift       := 0;
    cGreenBits      := 0;
    cGreenShift     := 0;
    cBlueBits       := 0;
    cBlueShift      := 0;
    cAlphaBits      := 0;
    cAlphaShift     := 0;
    cAccumBits      := 0;
    cAccumRedBits   := 0;
    cAccumGreenBits := 0;
    cAccumBlueBits  := 0;
    cAccumAlphaBits := 0;
    cDepthBits      := 16;
    cStencilBits    := 0;
    cAuxBuffers     := 0;
    iLayerType      := PFD_MAIN_PLANE;
    bReserved       := 0;
    dwLayerMask     := 0;
    dwVisibleMask   := 0;
    dwDamageMask    := 0;
  end;

  iPixelFormat := ChoosePixelFormat(iDC, @iPFD);
  if (iPixelFormat = 0) then
  begin
    glKillWnd();
    Result := False;
    Exit;
  end;

  if (not SetPixelFormat(iDC, iPixelFormat, @iPFD)) then
  begin
    glKillWnd();
    Result := False;
    Exit;
  end;

  iRC := wglCreateContext(iDC);
  if (iRC = 0) then
  begin
    glKillWnd();
    Result := False;
    Exit;
  end;

  if (not wglMakeCurrent(iDC, iRC)) then
  begin
    glKillWnd();
    Result := False;
    Exit;
  end;

  ReadExtensions;
  ReadImplementationProperties;
end;

begin
  Log.AddNewLine('---OpenGL Detection---');
  Try
    result := true;
    if not(glCreateWnd()) then
      Raise Exception.Create('Failed to create test window!');

    Log.Save := False;;
    Log.AddNewLine('GL_VENDOR: ' + glGetString(GL_VENDOR));
    Log.AddNewLine('GL_RENDERER: ' + glGetString(GL_RENDERER));
    Log.AddNewLine('GL_VERSION: ' + glGetString(GL_VERSION));

    iString1 := glGetString(GL_EXTENSIONS);
    Log.AddNewLine('List of Supported Extensions: ');
    iString2 := '   ';
    For iI := 1 to  length(iString1)-1 do
    begin
      If iString1[iI] <> ' ' then
        iString2 := iString2 + iString1[iI]
      else
      begin
        Log.AddNewLine(iString2);
        iString2 := '   ';
      end;
    end;
    Log.Save := True;

    glGetIntegerv(GL_MAX_TEXTURE_UNITS_ARB, @FMaxTextureUnits);
    Log.AddNewLine('Number of Texture Units: ' + IntToStr(FMaxTextureUnits) );

    glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, @FMaxAnisotropicFilter);
    Log.AddNewLine('Maximum Anisotropic Filter: ' + FloatToStr(FMaxAnisotropicFilter));

    glGetFloatv(GL_MAX_TEXTURE_SIZE, @FMaxtextureSize);
    Log.AddNewLine('Maximum Texture Size : ' + FloatToStr(FMaxtextureSize));

    If ((Pos('GL_ARB_shader_objects', iString1) <= 0) or
       (Pos('GL_ARB_fragment_program', iString1) <= 0) or
       (Pos('GL_ARB_fragment_shader', iString1) <= 0) or
       (Pos('GL_ARB_vertex_program', iString1) <= 0) or
       (Pos('GL_ARB_vertex_shader', iString1) <= 0)) then
    begin
       FSupportGLSL := false;
       Log.AddNewLine('OpenGL Shading Language: not supported');
    end
    else
    begin
       FSupportGLSL := true;
       Log.AddNewLine('OpenGL Shading Language: supported');
    end;

    if Pos('GL_EXT_framebuffer_object', iString1) <= 0 then
    begin
       FSupportFBO := false;
       Log.AddNewLine('Frame Buffer Objects: not supported');
    end
    else
    begin
       FSupportFBO := true;
       Log.AddNewLine('Frame Buffer Objects: supported');
    end;
       
    glKillWnd();
  except
    on E: Exception do
    begin
      iError := E.Message;
      result := false;
    end;
  end;

  If not(result) then
  begin
    Log.AddNewLine('Failed to complete OpenGL detection');
    Log.AddNewLine('Error Message: ' + iError);
  end;
  Log.AddNewLine('----------------------');
  Log.AddNewLine('');
end;

{******************************************************************************}
{* Check the engine filesystem                                                *}
{******************************************************************************}

function TGDSettings.CheckFileSystem(): boolean;

function CheckDir(aPath : String) : bool;
begin
  Log.AddNewLine('Checking ' + aPath + '...');
  if not DirectoryExistsUTF8(aPath) then
  begin
    Log.AddToLastLine('Failed');
    result := false;
    exit
  end
  else
    Log.AddToLastLine('Succeeded');
end;

begin
  result := true;

  If not(CheckDir(FApplicationFilePath + FP_MAPS)) then
  begin
    result := false;
    exit;
  end;

  If not(CheckDir(FApplicationFilePath + FP_SCREENSHOTS)) then
  begin
    result := false;
    exit;
  end;

  If not(CheckDir(FApplicationFilePath + FP_SHADERS)) then
  begin
    result := false;
    exit;
  end;

  If not(CheckDir(FApplicationFilePath + FP_SOUNDS)) then
  begin
    result := false;
    exit;
  end;

  If not(CheckDir(FApplicationFilePath + FP_MESHES)) then
  begin
    result := false;
    exit;
  end;

  If not(CheckDir(FApplicationFilePath + FP_TEXTURES)) then
  begin
    result := false;
    exit;
  end;

  If not(CheckDir(FApplicationFilePath + FP_INITS)) then
  begin
    result := false;
    exit;
  end;
end;

{******************************************************************************}
{* Check the engine capabilities                                              *}
{******************************************************************************}

Function  TGDSettings.CheckCapabilities() : boolean;
begin
  Log.AddNewLine('Checking capabilities...');
  result := false;

  If FMaxTextureUnits < MRS_TEXTURE_UNITS then
  begin
    Log.AddToLastLine('Failed');
    Log.AddNewLine('Error Message: Not ennough texture units!');
    exit;
  end;

  if Not(FSupportGLSL) then
  begin
    Log.AddToLastLine('Failed');
    Log.AddNewLine('Error Message: Opengl Shading Language not supported!');
    exit;
  end;

  if Not(FSupportFBO) then
  begin
    Log.AddToLastLine('Failed');
    Log.AddNewLine('Error Message: Frame Buffer Objects not supported!');
    exit;
  end;

  Log.AddToLastLine('Succeeded');
  result := true;
end;

end.
