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
  SysUtils,
  IniFiles,
  dglOpenGL,
  GDConstants,
  FileUtil;

type

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
    //viewport settings
    FTop              : Integer;
    FLeft             : Integer;
    FWidth            : Integer;
    FHeight           : Integer;
    FFullScreen       : Boolean;
    FVerticalSync     : Boolean;
    FGamma            : Single;

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
    //viewport settings
    property Top : Integer read FTop write FTop;
    property Left : Integer read FLeft write FLeft;
    property Width : Integer read FWidth write FWidth;
    property Height : Integer read FHeight write FHeight;
    property FullScreen : Boolean read FFullScreen write FFullScreen;
    property VerticalSync : Boolean read FVerticalSync write FVerticalSync;
    property Gamma : Single read FGamma write FGamma;

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

    procedure LoadIniFile();
    procedure SaveIniFile();
    function  GetSettings() : TSettings;
    procedure SetSettings(aSettings : TSettings);
  end;

var
  Settings : TGDSettings;

implementation

uses
  GDConsole;

{******************************************************************************}
{* Create the settings class                                                  *}
{******************************************************************************}

constructor TGDSettings.Create();
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

  //console commands
  Console.AddCommand('RBloom', '0,1 : Enable or disable bloom', CT_BOOLEAN, @FUseBloom);
  Console.AddCommand('RFXAA', '0,1 : Enable or disable bloom', CT_BOOLEAN, @FUseFXAA);
  Console.AddCommand('RVSync', '0,1 : Enable or disable vertical sync', CT_BOOLEAN, @FVerticalSync);
  Console.AddCommand('RGamma', '0.0 to 1.0 : Set the gamma value', CT_FLOAT, @FGamma);
end;

{******************************************************************************}
{* Destroy the settings class                                                 *}
{******************************************************************************}

destructor  TGDSettings.Destroy();
begin
  inherited;
end;

{******************************************************************************}
{* Load the settings from an ini-file                                         *}
{******************************************************************************}

procedure TGDSettings.LoadIniFile();
var
  iIniFile : TIniFile;
  iStr : String;
begin
  iIniFile := TIniFile.Create( FP_INITS + ENGINE_INI );

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
  FInvertMouse      := iIniFile.ReadBool( 'Controls', 'InvertMouse', False);
  FMouseSensitivity := iIniFile.ReadInteger('Controls', 'MouseSensitivity', 5);

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

procedure TGDSettings.SaveIniFile();
var
  iIniFile : TIniFile;
begin
  iIniFile := TIniFile.Create(FP_INITS + ENGINE_INI);

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
  iIniFile.WriteBool( 'Controls', 'InvertMouse', FInvertMouse);
  iIniFile.WriteInteger('Controls', 'MouseSensitivity', FMouseSensitivity);

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



end.
