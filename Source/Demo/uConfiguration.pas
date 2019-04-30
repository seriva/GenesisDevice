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
unit uConfiguration;

{$MODE Delphi}

{******************************************************************************}
{* This units holds the configurationform of the demoapplication              *}
{******************************************************************************}

interface

uses
  SDL2,
  LCLIntf,
  LCLType,
  Forms,
  ComCtrls,
  ExtCtrls,
  Controls,
  StdCtrls,
  Classes,
  Dialogs,
  SysUtils,
  uGDConstants,
  uGDEngine,
  LazFileUtils;

type

{******************************************************************************}
{* COnfiguration form                                                         *}
{******************************************************************************}

  { TConfigurationForm }

  TConfigurationForm = class(TForm)
    ApplicationProperties: TApplicationProperties;
    DetailCheckbox: TCheckBox;
    FoliageDensityLabel: TLabel;
    FoliageDensityTrackBar: TTrackBar;
    FoliageDistanceTrackBar: TTrackBar;
    FullScreenCheckBox: TCheckBox;
    FXAACheckBox: TCheckBox;
    GammaLabel: TLabel;
    GammaTrackBar: TTrackBar;
    GrassDistanceLabel: TLabel;
    InputPanel: TPanel;
    InputTabSheet: TTabSheet;
    InvertMouseCheckBox: TCheckBox;
    MonitorComboBox: TComboBox;
    MonitorLabel: TLabel;
    MouseSensitivityLabel: TLabel;
    MouseSensitivityTrackBar: TTrackBar;
    MuteSoundCheckBox: TCheckBox;
    PageControl: TPageControl;
    RenderingPanel: TPanel;
    RenderingTabSheet: TTabSheet;
    ResolutionLabel: TLabel;
    ResolutionsComboBox: TComboBox;
    ShadowCheckBox: TCheckBox;
    RunButton: TButton;
    ShadowsCheckbox: TCheckBox;
    SoundPanel: TPanel;
    SoundTabSheet: TTabSheet;
    SoundVolumeLabel: TLabel;
    SoundVolumeTrackBar: TTrackBar;
    SSAOCheckbox: TCheckBox;
    TextureDetailComboBox: TComboBox;
    TextureDetailLabel: TLabel;
    TextureFilterComboBox: TComboBox;
    TextureFilteringLabel: TLabel;
    VerticalSyncCheckBox: TCheckBox;
    ViewDistanceLabel: TLabel;
    ViewDistanceTrackBar: TTrackBar;
    WaterDetailComboBox: TComboBox;
    WaterDetailLabel: TLabel;
    WaterReflectionLabel: TLabel;
    WaterReflectionComboBox: TComboBox;
    WindowPanel: TPanel;
    WindowTabSheet: TTabSheet;
    procedure ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
    procedure ApplicationPropertiesIdleEnd(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MonitorComboBoxChange(Sender: TObject);

    procedure RunButtonClick(Sender: TObject);
  private
    FDisplayModes : array of TSDL_DisplayMode;

    procedure FillComboboxes();
    procedure FillDisplays();
    procedure FillDisplayModi();
  public
    procedure SettingsToInterface();
    procedure SettingsFromInterface();
  end;

var
  ConfigurationForm: TConfigurationForm;

implementation

{$R *.lfm}

uses
  uMain;

{******************************************************************************}
{* Form create                                                                *}
{******************************************************************************}

procedure TConfigurationForm.FormCreate(Sender: TObject);
begin
  //set some form basics
  Application.Title := 'Demo';
  self.Caption := 'Demo';

  //fill settingscombos
  FillComboboxes();

  //load the settings and update interface
  GDSettings.Load();
  SettingsToInterface();

  //show the form and set the focus
  self.Show();
  RunButton.SetFocus();
  Application.DisableIdleHandler;
end;

{******************************************************************************}
{* Form destroy                                                               *}
{******************************************************************************}

procedure TConfigurationForm.FormDestroy(Sender: TObject);
begin
  //save settings
  SettingsFromInterface();
  GDSettings.Save();
end;

{******************************************************************************}
{* Set position on configurationform show                                     *}
{******************************************************************************}

procedure TConfigurationForm.FormShow(Sender: TObject);
begin
  self.Left:=5;
  self.Top:=5;
end;

{******************************************************************************}
{* Refill resolutions when we change the monitor                              *}
{******************************************************************************}

procedure TConfigurationForm.MonitorComboBoxChange(Sender: TObject);
begin
  FillDisplayModi();
end;

{******************************************************************************}
{* Fill the settings comboboxes on the configurationform                      *}
{******************************************************************************}

procedure TConfigurationForm.FillComboboxes();
var
  iI: LongInt;
begin
  //fill monitors combobox
  FillDisplays();

  //fill resolutions combobox
  FillDisplayModi();

  //fill texturedetail combobox
  TextureDetailComboBox.Clear;
  for iI := Low(TTextureDetail) to High(TTextureDetail) do
    TextureDetailComboBox.Items.Add(TTextureDetail[iI]);

  //fill texturefilter combobox
  TextureFilterComboBox.Clear;
  for iI := Low(TTextureFilter) to High(TTextureFilter) do
    TextureFilterComboBox.Items.Add(TTextureFilter[iI]);

  //fill waterdetail combobox
  WaterDetailComboBox.Clear;
  for iI := Low(TWaterDetail) to High(TWaterDetail) do
    WaterDetailComboBox.Items.Add(TWaterDetail[iI]);
end;

{******************************************************************************}
{* Set the engines current settings on the interface                          *}
{******************************************************************************}

procedure TConfigurationForm.SettingsToInterface();
var
  iI: Integer;
begin
  //window settings
  MonitorComboBox.ItemIndex := GDSettings.Display;
  ResolutionsComboBox.ItemIndex := GDSettings.DisplayMode;
  FullScreenCheckBox.Checked := GDSettings.FullScreen;
  VerticalSyncCheckBox.Checked := GDSettings.VerticalSync;
  GammaTrackBar.Position := Round(GDSettings.Gamma * 100);

  //rendering settings
  ViewDistanceTrackBar.Position := GDSettings.ViewDistance;
  FoliageDistanceTrackBar.Position := GDSettings.FoliageDistance;
  FoliageDensityTrackBar.Position := GDSettings.FoliageDensity;
  For  iI := 0 to TextureDetailComboBox.Items.Count-1 do
    if  TextureDetailComboBox.Items[iI] = GDSettings.GetTextureDetail() then
      TextureDetailComboBox.ItemIndex := iI;

  For  iI := 0 to TextureFilterComboBox.Items.Count-1 do
    if  TextureFilterComboBox.Items[iI] = GDSettings.GetTextureFilter() then
      TextureFilterComboBox.ItemIndex := iI;

  For  iI := 0 to WaterDetailComboBox.Items.Count-1 do
    if  WaterDetailComboBox.Items[iI] = GDSettings.GetWaterDetail() then
      WaterDetailComboBox.ItemIndex := iI;

  FXAACheckBox.Checked := GDSettings.UseFXAA;
  ShadowsCheckbox.Checked := GDSettings.UseShadows;
  SSAOCheckbox.Checked := GDSettings.UseSSAO;
  DetailCheckbox.Checked:=GDSettings.UseDetail;

  //input settings
  InvertMouseCheckBox.Checked := GDSettings.InvertMouse;
  MouseSensitivityTrackBar.Position := GDSettings.MouseSensitivity;

  //sound settings
  MuteSoundCheckBox.Checked := GDSettings.MuteSound;
  SoundVolumeTrackBar.Position := Round(100 * GDSettings.SoundVolume );
end;

{******************************************************************************}
{* Get the settings from the interface and pas them to the engine             *}
{******************************************************************************}

procedure TConfigurationForm.SettingsFromInterface();
begin
  //window settings
  GDSettings.Display          := MonitorComboBox.ItemIndex;
  GDSettings.DisplayMode      := ResolutionsComboBox.ItemIndex;
  GDSettings.Width            := FDisplayModes[ResolutionsComboBox.ItemIndex].w;
  GDSettings.Height           := FDisplayModes[ResolutionsComboBox.ItemIndex].h;
  GDSettings.FullScreen       := FullScreenCheckBox.Checked;
  GDSettings.VerticalSync     := VerticalSyncCheckBox.Checked;
  GDSettings.Gamma            := GammaTrackBar.Position / 100;

  //rendering
  GDSettings.ViewDistance     := ViewDistanceTrackBar.Position;
  GDSettings.FoliageDistance  := FoliageDistanceTrackBar.Position;
  GDSettings.FoliageDensity   := FoliageDensityTrackBar.Position;
  GDSettings.SetTextureDetail(TextureDetailComboBox.Text);
  GDSettings.SetTextureFilter(TextureFilterComboBox.Text);
  GDSettings.SetWaterDetail(WaterDetailComboBox.Text);
  GDSettings.UseFXAA          := FXAACheckBox.Checked;
  GDSettings.UseShadows       := ShadowsCheckbox.Checked;
  GDSettings.UseSSAO          := SSAOCheckbox.Checked;
  GDSettings.UseDetail        := DetailCheckbox.Checked;

  //input settings
  GDSettings.InvertMouse      := InvertMouseCheckBox.Checked;
  GDSettings.MouseSensitivity := MouseSensitivityTrackBar.Position;

  //sound settings
  GDSettings.MuteSound        := MuteSoundCheckBox.Checked;
  GDSettings.SoundVolume      := SoundVolumeTrackBar.Position / 100;
end;

{******************************************************************************}
{* Run the engine                                                             *}
{******************************************************************************}

procedure TConfigurationForm.RunButtonClick(Sender: TObject);
begin
  SettingsFromInterface();
  GDSettings.Save();
  Visible := false;
  GDEngine.Init(@InitGame);
  Application.EnableIdleHandler;
end;

procedure TConfigurationForm.ApplicationPropertiesIdle(Sender: TObject;
  var Done: Boolean);
begin
  Done := GDEngine.Done;
  GDEngine.Loop(@GameLoop);
end;

procedure TConfigurationForm.ApplicationPropertiesIdleEnd(Sender: TObject);
begin
  Application.DisableIdleHandler;
  GDEngine.Clear(@ClearGame);
  ConfigurationForm.SettingsToInterface();
  ConfigurationForm.Visible := true;
  ConfigurationForm.Repaint();
end;

{******************************************************************************}
{* Fill Displays                                                              *}
{******************************************************************************}

procedure TConfigurationForm.FillDisplays();
var
  iI : Integer;
begin
  for iI := 0 to SDL_GetNumVideoDisplays()-1 do
  begin
    MonitorComboBox.Items.Add(IntToStr(iI) + ' - ' +SDL_GetDisplayName(iI));
  end;
  MonitorComboBox.ItemIndex := 0;
end;

{******************************************************************************}
{* Fill Display Modi                                                          *}
{******************************************************************************}

procedure TConfigurationForm.FillDisplayModi();
var
  iI, iJ   : Integer;
begin
  ResolutionsComboBox.Items.Clear;
  setLength(FDisplayModes, SDL_GetNumDisplayModes(MonitorComboBox.ItemIndex));
  iJ := SDL_GetNumDisplayModes(MonitorComboBox.ItemIndex)-1;
  for iI := iJ downto 0 do
  begin
    SDL_GetDisplayMode(MonitorComboBox.ItemIndex, iI, @FDisplayModes[iJ-iI]);
    ResolutionsComboBox.Items.Add(IntToStr(FDisplayModes[iJ-iI].w) + ' x ' + IntToStr(FDisplayModes[iJ-iI].h) + ' @ ' + IntToStr(FDisplayModes[iJ-iI].refresh_rate) + 'Hz');
  end;
  ResolutionsComboBox.ItemIndex := 0;
end;

end.
