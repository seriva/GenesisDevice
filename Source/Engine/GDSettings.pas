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
    FoliageDistance  : Integer;
    FoliageDensity   : Integer;
    TextureDetail    : String;
    WaterDetail      : String;
    TextureFilter    : String;
    UseBloom         : Boolean;
    UseFXAA          : Boolean;
    UseShadows       : Boolean;

    //input settings
    InvertMouse : Boolean;
    MouseSensitivity : Integer;

    //sound settings
    MuteSound   : Boolean;
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
    FFoliageDistance  : Integer;
    FFoliageDensity   : Integer;
    FTextureDetail    : TGDTextureDetail;
    FWaterDetail      : TGDWaterDetail;
    FTextureFilter    : TGDTextureFilter;
    FUseBloom         : Boolean;
    FUseFXAA          : Boolean;
    FUseShadows       : Boolean;

    //input settings
    FInvertMouse      : Boolean;
    FMouseSensitivity : Integer;

    //sound settings
    FMuteSound   : boolean;
    FSoundVolume : Double;

    //Model global LOD settings.
    FLOD0, FLOD1, FLOD2 : Single;

    procedure SetTextureDetail(aStr : String);
    function  GetTextureDetail() : String;
    procedure SetWaterDetail(aStr : String);
    function  GetWaterDetail() : String;
    procedure SetTextureFilter(aStr : String);
    function  GetTextureFilter() : String;
    procedure SetViewDistance(aDistance : Integer);
  public
    //viewport settings
    property Top : Integer read FTop write FTop;
    property Left : Integer read FLeft write FLeft;
    property Width : Integer read FWidth write FWidth;
    property Height : Integer read FHeight write FHeight;
    property FullScreen : Boolean read FFullScreen write FFullScreen;
    property VerticalSync : Boolean read FVerticalSync write FVerticalSync;
    property Gamma : Single read FGamma write FGamma;
    property LOD0: Single read FLOD0;
    property LOD1: Single read FLOD1;
    property LOD2: Single read FLOD2;

    //render settings
    property ViewDistance : Integer read FViewDistance write SetViewDistance;
    property FoliageDistance : Integer read FFoliageDistance write FFoliageDistance;
    property FoliageDensity : Integer read FFoliageDensity write FFoliageDensity;
    property WaterDetail : TGDWaterDetail read FWaterDetail write FWaterDetail;
    property TextureDetail : TGDTextureDetail read FTextureDetail write FTextureDetail;
    property TextureFilter : TGDTextureFilter read FTextureFilter write FTextureFilter;
    property UseBloom : Boolean read FUseBloom write FUseBloom;
    property UseFXAA : Boolean read FUseFXAA write FUseFXAA;
    property UseShadows : Boolean read FUseShadows write FUseShadows;


    //input settings
    property InvertMouse : Boolean read FInvertMouse write FInvertMouse;
    property MouseSensitivity : Integer read FMouseSensitivity write FMouseSensitivity;

    //sound settings
    property MuteSound : Boolean read FMuteSound write FMuteSound;
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
  ViewDistance      := 5;
  FFoliageDistance  := 1;
  FFoliageDensity   := 10;
  FTextureDetail    := TD_LOW;
  FWaterDetail      := WD_LOW;
  FTextureFilter    := TF_BILINEAR;
  FUseBloom         := false;
  FUseFXAA          := false;
  FUseShadows       := false;

  //set right en left
  FTop := 0;
  FLeft := 0;
  FGamma := 0.50;

  //input
  FInvertMouse      := False;
  FMouseSensitivity := 5;

  //sound
  FMuteSound   := false;
  FSoundVolume := 0.5;

  //console commands
  Console.AddCommand('RBloom', '0,1 : Enable or disable bloom', CT_BOOLEAN, @FUseBloom);
  Console.AddCommand('RFXAA', '0,1 : Enable or disable bloom', CT_BOOLEAN, @FUseFXAA);
  Console.AddCommand('RVSync', '0,1 : Enable or disable vertical sync', CT_BOOLEAN, @FVerticalSync);
  Console.AddCommand('RGamma', '0.0 to 1.0 : Set the gamma value', CT_FLOAT, @FGamma);
  Console.AddCommand('RShadows', '0,1 : Enable or disable bloom', CT_BOOLEAN, @FUseShadows);
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
begin
  iIniFile := TIniFile.Create( PATH_INITS + ENGINE_INI );

  //viewport settings
  FWidth :=        iIniFile.ReadInteger('ViewPort', 'Width', 800);
  FHeight :=       iIniFile.ReadInteger('ViewPort', 'Height', 600);
  FFullScreen :=   iIniFile.ReadBool('ViewPort', 'Fullscreen', False);
  FVerticalSync := iIniFile.ReadBool('ViewPort', 'VerticalSync', False);
  FGamma :=        iIniFile.ReadFloat('ViewPort', 'Gamma', 0.60);

  //render settings
  ViewDistance     := iIniFile.ReadInteger('Renderer', 'ViewDistance', 5);
  FFoliageDistance := iIniFile.ReadInteger('Renderer', 'FoliageDistance', 1);
  FFoliageDensity  := iIniFile.ReadInteger('Renderer',  'FoliageDensity', 5);
  SetTextureDetail( iIniFile.ReadString('Renderer', 'TextureDetail', TGDTextureDetailStrings[1]) );
  SetTextureFilter(iIniFile.ReadString('Renderer', 'TextureFilter', TGDTextureFilterStrings[1]));
  SetWaterDetail(iIniFile.ReadString('Renderer', 'WaterDetail', TGDWaterDetailStrings[1]));
  FUseBloom         := iIniFile.ReadBool( 'Renderer', 'UseBloom', False);
  FUseFXAA          := iIniFile.ReadBool( 'Renderer', 'UseFXAA', False);
  FUseShadows       := iIniFile.ReadBool( 'Renderer', 'UseShadows', False);

  //input settings
  FInvertMouse      := iIniFile.ReadBool( 'Controls', 'InvertMouse', False);
  FMouseSensitivity := iIniFile.ReadInteger('Controls', 'MouseSensitivity', 5);

  //sound settings
  FMuteSound   := iIniFile.ReadBool( 'Sound', 'Mute', False);
  FSoundVolume := iIniFile.ReadFloat( 'Sound', 'SoundVolume', 0.5);
  
  FreeAndNil(iIniFile)
end;

{******************************************************************************}
{* Saves the settings to an ini-files                                         *}
{******************************************************************************}

procedure TGDSettings.SaveIniFile();
var
  iIniFile : TIniFile;
begin
  iIniFile := TIniFile.Create(PATH_INITS + ENGINE_INI);

  //viewport
  iIniFile.WriteInteger('ViewPort', 'Width', FWidth);
  iIniFile.WriteInteger('ViewPort', 'Height', FHeight);
  iIniFile.WriteBool('ViewPort', 'Fullscreen', FFullScreen);
  iIniFile.WriteBool('ViewPort', 'VerticalSync', FVerticalSync);
  iIniFile.WriteFloat('ViewPort', 'Gamma', FGamma);

  //render settings
  iIniFile.WriteInteger('Renderer', 'ViewDistance', FViewDistance);
  iIniFile.WriteInteger('Renderer', 'FoliageDistance', FFoliageDistance);
  iIniFile.WriteInteger('Renderer', 'FoliageDensity', FFoliageDensity);
  iIniFile.WriteString('Renderer', 'TextureDetail', GetTextureDetail());
  iIniFile.WriteString('Renderer', 'TextureFilter', GetTextureFilter());
  iIniFile.WriteString('Renderer', 'WaterDetail', GetWaterDetail());
  iIniFile.WriteBool( 'Renderer', 'UseBloom', FUseBloom );
  iIniFile.WriteBool( 'Renderer', 'UseFXAA', FUseFXAA );
  iIniFile.WriteBool( 'Renderer', 'UseShadows', FUseShadows );

  //input settings
  iIniFile.WriteBool( 'Controls', 'InvertMouse', FInvertMouse);
  iIniFile.WriteInteger('Controls', 'MouseSensitivity', FMouseSensitivity);

  //sound settings
  iIniFile.WriteBool( 'Sound', 'Mute', FMuteSound);
  iIniFile.WriteFloat( 'Sound', 'SoundVolume', FSoundVolume);

  FreeAndNil(iIniFile)
end;

{******************************************************************************}
{* Get the current settings                                                   *}
{******************************************************************************}

function  TGDSettings.GetSettings() : TSettings;
begin
  //viewport settings
  Result.Width           := FWidth;
  Result.Height          := FHeight;
  Result.FullScreen      := FFullScreen;
  Result.VerticalSync    := FVerticalSync;
  Result.Gamma           := FGamma;

  //render settings
  Result.ViewDistance    := FViewDistance;
  Result.FoliageDistance := FFoliageDistance;
  Result.FoliageDensity  := FFoliageDensity;
  Result.TextureDetail   := GetTextureDetail();
  Result.WaterDetail     := GetWaterDetail();
  Result.TextureFilter   := GetTextureFilter();
  Result.UseBloom        := FUseBloom;
  Result.UseFXAA         := FUseFXAA;
  Result.UseShadows      := FUseShadows;

  //input settings
  Result.InvertMouse      := FInvertMouse;
  Result.MouseSensitivity := FMouseSensitivity;

  //sound settings
  Result.MuteSound   := FMuteSound;
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
  FFoliageDistance  := aSettings.FoliageDistance;
  FFoliageDensity   := aSettings.FoliageDensity;
  SetTextureDetail( aSettings.TextureDetail );
  SetWaterDetail( aSettings.WaterDetail );
  SetTextureFilter( aSettings.TextureFilter );
  FUseBloom         := aSettings.UseBloom;
  FUseFXAA          := aSettings.UseFXAA;
  FUseShadows       := aSettings.UseShadows;

  //input settings
  FInvertMouse      := aSettings.InvertMouse;
  FMouseSensitivity := aSettings.MouseSensitivity;

  //sound settings
  FMuteSound   := aSettings.MuteSound;
  FSoundVolume := aSettings.SoundVolume;
end;

{******************************************************************************}
{* Set view distance                                                          *}
{******************************************************************************}

procedure TGDSettings.SetViewDistance(aDistance : Integer);
var
  iStep : Single;
begin
  FViewDistance := aDistance;

  //set the new LOD levels.
  iStep := (FViewDistance * R_VIEW_DISTANCE_STEP) / 100;
  FLOD0 := iStep * R_LOD0_DISTANCE;
  FLOD1 := iStep * R_LOD1_DISTANCE;
  FLOD2 := iStep * R_LOD2_DISTANCE;
end;

{******************************************************************************}
{* Set texture detail                                                         *}
{******************************************************************************}

procedure TGDSettings.SetTextureDetail(aStr : String);
var
  iI : Integer;
begin
  for iI := 1 to 3 do
    If  aStr = TGDTextureDetailStrings[iI] then FTextureDetail := TGDTextureDetail(iI);
end;

{******************************************************************************}
{* Get texture detail                                                         *}
{******************************************************************************}

function TGDSettings.GetTextureDetail() : String;
begin
  result := TGDTextureDetailStrings[ Integer(FTextureDetail) ];
end;

{******************************************************************************}
{* Set texture filter                                                         *}
{******************************************************************************}

procedure TGDSettings.SetTextureFilter(aStr : String);
var
  iI : Integer;
begin
  for iI := 1 to 6 do
    If  aStr = TGDTextureFilterStrings[iI] then FTextureFilter := TGDTextureFilter(iI);
end;

{******************************************************************************}
{* Get texture filter                                                         *}
{******************************************************************************}

function TGDSettings.GetTextureFilter() : String;
begin
  result := TGDTextureFilterStrings[ Integer(FTextureFilter) ];
end;

{******************************************************************************}
{* Set water detail                                                           *}
{******************************************************************************}

procedure TGDSettings.SetWaterDetail(aStr : String);
var
  iI : Integer;
begin
  for iI := 1 to 3 do
    If  aStr = TGDWaterDetailStrings[iI] then FWaterDetail := TGDWaterDetail(iI);
end;

{******************************************************************************}
{* Get the water detail                                                       *}
{******************************************************************************}

function TGDSettings.GetWaterDetail() : String;
begin
  result := TGDWaterDetailStrings[ Integer(FWaterDetail) ];
end;

end.
