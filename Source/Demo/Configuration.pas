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
unit Configuration;

{$MODE Delphi}

{******************************************************************************}
{* This units holds the configurationform of the demoapplication              *}
{******************************************************************************}

interface

uses
  LCLIntf,
  LCLType,
  Windows,
  Forms,
  ComCtrls,
  ExtCtrls,
  Controls,
  StdCtrls,
  Classes,
  Dialogs,
  ViewPort,
  SysUtils,
  GDInterface,
  IniFiles,
  multimon,
  GDConstants,
  GDSettings,
  FileUtil;

type
{******************************************************************************}
{* Records that holds a displaymode                                           *}
{******************************************************************************}

  TDisplayMode = record
    Width: LongInt;
    Height: LongInt;
  end;

{******************************************************************************}
{* COnfiguration form                                                         *}
{******************************************************************************}

  { TConfigurationForm }

  TConfigurationForm = class(TForm)
    ShadowCheckBox: TCheckBox;
    FXAACheckBox: TCheckBox;
    MonitorLabel: TLabel;
    MonitorComboBox: TComboBox;
    RunButton: TButton;
    PageControl: TPageControl;
    ShadowsCheckbox: TCheckBox;
    WindowTabSheet: TTabSheet;
    WindowPanel: TPanel;
    ResolutionLabel: TLabel;
    GammaLabel: TLabel;
    ResolutionsComboBox: TComboBox;
    FullScreenCheckBox: TCheckBox;
    VerticalSyncCheckBox: TCheckBox;
    GammaTrackBar: TTrackBar;
    RenderingTabSheet: TTabSheet;
    RenderingPanel: TPanel;
    WaterDetailLabel: TLabel;
    ViewDistanceLabel: TLabel;
    GrassDistanceLabel: TLabel;
    FoliageDensityLabel: TLabel;
    TextureDetailLabel: TLabel;
    TextureFilteringLabel: TLabel;
    WaterReflectionLabel: TLabel;
    ViewDistanceTrackBar: TTrackBar;
    FoliageDistanceTrackBar: TTrackBar;
    FoliageDensityTrackBar: TTrackBar;
    TextureDetailComboBox: TComboBox;
    TextureFilterComboBox: TComboBox;
    WaterReflectionComboBox: TComboBox;
    WaterDetailComboBox: TComboBox;
    BloomCheckBox: TCheckBox;
    InputTabSheet: TTabSheet;
    InputPanel: TPanel;
    MouseSensitivityLabel: TLabel;
    MouseSensitivityTrackBar: TTrackBar;
    InvertMouseCheckBox: TCheckBox;
    SoundTabSheet: TTabSheet;
    SoundPanel: TPanel;
    SoundVolumeLabel: TLabel;
    MuteSoundCheckBox: TCheckBox;
    SoundVolumeTrackBar: TTrackBar;
    DemoTabSheet: TTabSheet;
    DemoPanel: TPanel;
    MapLabel: TLabel;
    MapComboBox: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MonitorComboBoxChange(Sender: TObject);

    procedure RunButtonClick(Sender: TObject);
  private
    FMap : String;

    procedure FillComboboxes();
    procedure FillDisplays();
    procedure FillDisplayModi();
  public
    FMonitorInfos      : array of TMonitorInfoEx;
    FAvailableModi     : array of TDisplayMode;

    property Map         : String read FMap;

    procedure SettingsToInterface();
    procedure SettingsFromInterface();
    procedure LoadConfig();
    procedure SaveConfig();
  end;

var
  ConfigurationForm: TConfigurationForm;
  ViewPortForm : TViewPortForm;

implementation

{$R *.lfm}

uses
  Main;

{******************************************************************************}
{* Form create                                                                *}
{******************************************************************************}

procedure TConfigurationForm.FormCreate(Sender: TObject);
begin
  //set some form basics
  Application.Title := 'Demo';
  self.Caption := 'Configuration';

  //initialize the engine`s core systems
  If Not( gdEngineInit()) then
  begin
    MessageBox(0, 'Error starting engine! See log for details.', 'Error', MB_OK);
    Application.Terminate();
  end;

  //fill settingscombos
  FillComboboxes();

  //load the settings and update interface
  gdSettingsLoad();
  LoadConfig();
  SettingsToInterface();

  //show the form and set the focus
  self.Show();
  RunButton.SetFocus();
end;

{******************************************************************************}
{* Form destroy                                                               *}
{******************************************************************************}

procedure TConfigurationForm.FormDestroy(Sender: TObject);
begin
  //save settings
  SettingsFromInterface();
  gdSettingsSave();
  SaveConfig();

  //shutdown engine`s core
  gdEngineShutDown();
end;

{******************************************************************************}
{* Set position on configurationform show                                     *}
{******************************************************************************}

procedure TConfigurationForm.FormShow(Sender: TObject);
begin
  self.Left:=10;
  self.Top:=10;
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
 iStr: string;
 iResult : TStringList;

Procedure FindFiles(aPath, aFindMask: String; aWithSub: Boolean; aResult: tStrings);
var
FindRec: tSearchRec;
Begin
    If (aPath = '') or (aFindMask = '') or Not Assigned (aResult) Then
      Exit;
    If aPath[Length (aPath)] <> '\' Then
      aPath := aPath + '\';
    If FindFirstUTF8(aPath + aFindMask,faAnyFile,FindRec) = 0 Then
      Repeat
        If (FindRec.Name <> '.') and (FindRec.Name <> '..') Then
          aResult.Add (aPath + FindRec.Name);
      Until FindNextUTF8(FindRec) <> 0;
    FindCloseUTF8(FindRec);
    If Not aWithSub Then
      Exit;
    If FindFirstUTF8(aPath + '*.*',faAnyFile,FindRec) = 0 Then
      Repeat
        If (FindRec.Name <> '.') and (FindRec.Name <> '..') Then
          If Boolean (FindRec.Attr and faDirectory) Then
            FindFiles (aPath + FindRec.Name, aFindMask, aWithSub, aResult);
      Until FindNextUTF8(FindRec) <> 0;
    FindCloseUTF8(FindRec);
End;

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

  //detect maps
  MapComboBox.Items.Clear();
  iResult := TStringList.Create();
  FindFiles ('Maps\', '*.*', false , iResult);
  For iI := 0 to iResult.Count-1 do
  begin
    iResult.Strings[iI] := copy(iResult.Strings[iI], length('Maps\')+1, length(iResult.Strings[iI]));
    iStr := 'Maps\' + iResult.Strings[iI] + '\map.ini';
    If FileExistsUTF8(iStr ) then
    begin
      MapComboBox.Items.Add( iResult.Strings[iI] );
    end;
  end;
end;

{******************************************************************************}
{* Set the engines current settings on the interface                          *}
{******************************************************************************}

procedure TConfigurationForm.SettingsToInterface();
var
  iSettings : TSettings;
  iI: Integer;
begin
  //get the current engine settings and set them on the interface
  iSettings := gdSettingsGetCurrent();

  //viewport settings
  FullScreenCheckBox.Checked := iSettings.FullScreen;
  VerticalSyncCheckBox.Checked := iSettings.VerticalSync;
  GammaTrackBar.Position := Round(iSettings.Gamma * 100);

  //rendering settings
  ViewDistanceTrackBar.Position := iSettings.ViewDistance;
  FoliageDistanceTrackBar.Position := iSettings.FoliageDistance;
  FoliageDensityTrackBar.Position := iSettings.FoliageDensity;
  For  iI := 0 to TextureDetailComboBox.Items.Count-1 do
    if  TextureDetailComboBox.Items[iI] = iSettings.TextureDetail then
      TextureDetailComboBox.ItemIndex := iI;

  For  iI := 0 to TextureFilterComboBox.Items.Count-1 do
    if  TextureFilterComboBox.Items[iI] = iSettings.TextureFilter then
      TextureFilterComboBox.ItemIndex := iI;

  For  iI := 0 to WaterDetailComboBox.Items.Count-1 do
    if  WaterDetailComboBox.Items[iI] = iSettings.WaterDetail then
      WaterDetailComboBox.ItemIndex := iI;

  BloomCheckBox.Checked := iSettings.UseBloom;
  FXAACheckBox.Checked := iSettings.UseFXAA;
  ShadowsCheckbox.Checked := iSettings.UseShadows;

  //input settings
  InvertMouseCheckBox.Checked := iSettings.InvertMouse;
  MouseSensitivityTrackBar.Position := iSettings.MouseSensitivity;

  //sound settings
  MuteSoundCheckBox.Checked := iSettings.MuteSound;
  SoundVolumeTrackBar.Position := Round(100 * iSettings.SoundVolume );

  //map
  For  iI := 0 to MapComboBox.Items.Count-1 do
    if  MapComboBox.Items[iI] = FMap then
      MapComboBox.ItemIndex := iI;
end;

{******************************************************************************}
{* Get the settings from the interface and pas them to the engine             *}
{******************************************************************************}

procedure TConfigurationForm.SettingsFromInterface();
var
  Mode: TDisplayMode;
  iSettings : TSettings;
begin
  //viewport settings
  if ResolutionsComboBox.ItemIndex >= 0 then
  begin
    Mode := FAvailableModi[ResolutionsComboBox.ItemIndex];
    iSettings.Width      := Mode.Width;
    iSettings.Height     := Mode.Height;
  end;
  iSettings.FullScreen   := FullScreenCheckBox.Checked;
  iSettings.VerticalSync := VerticalSyncCheckBox.Checked;
  iSettings.Gamma        := GammaTrackBar.Position / 100;

  //rendering
  iSettings.ViewDistance     := ViewDistanceTrackBar.Position;
  iSettings.FoliageDistance  := FoliageDistanceTrackBar.Position;
  iSettings.FoliageDensity   := FoliageDensityTrackBar.Position;
  iSettings.TextureDetail    := TextureDetailComboBox.Text;
  iSettings.TextureFilter    := TextureFilterComboBox.Text;
  iSettings.WaterDetail      := WaterDetailComboBox.Text;
  iSettings.UseBloom         := BloomCheckBox.Checked;
  iSettings.UseFXAA          := FXAACheckBox.Checked;
  iSettings.UseShadows       := ShadowsCheckbox.Checked;

  //input settings
  iSettings.InvertMouse      := InvertMouseCheckBox.Checked;
  iSettings.MouseSensitivity := MouseSensitivityTrackBar.Position;

  //sound settings
  iSettings.MuteSound   := MuteSoundCheckBox.Checked;
  iSettings.SoundVolume := SoundVolumeTrackBar.Position / 100;

  //demo
  FMap := MapComboBox.Text;

  //pas the settings to the engine
  gdSettingsSetCurrent(iSettings);
end;

{******************************************************************************}
{* Run the engine                                                             *}
{******************************************************************************}

procedure TConfigurationForm.RunButtonClick(Sender: TObject);
begin
  SettingsFromInterface();
  gdSettingsSave();
  SaveConfig();
  if FMap = '' then
  begin
    ShowMessage('Select a map first.');
    exit
  end;
  Visible := false;
  Application.CreateForm(TViewPortForm, ViewPortForm);
  ViewPortForm.ShowOnTop();
  ViewPortForm.Visible := true;
  ViewPortForm.Repaint();

  //initialize the gameresources
  InitGame();
end;

{******************************************************************************}
{* Load the input from the config file (IS NOT DONE IN THE ENGINE BECAUSE     *}
{* ACTIONS CAN BE DIFFERENT FROM GAME TO GAME))                               *}
{******************************************************************************}

procedure TConfigurationForm.LoadConfig();
var
  iIniFile : TIniFile;
begin
  iIniFile := TIniFile.Create( ExtractFilePath(Application.ExeName) + 'Ini\Game.ini' );

  //monitor
  MonitorComboBox.ItemIndex     := iIniFile.ReadInteger('Monitor', 'MonitorId', 0);
  ResolutionsComboBox.ItemIndex := iIniFile.ReadInteger('Monitor', 'ResolutionId', 0);

  //maps
  FMap := iIniFile.ReadString('Demo', 'SelectedMap', '');

  FreeAndNil(iIniFile);
end;

{******************************************************************************}
{* Save the input to the config file (IS NOT DONE IN THE ENGINE BECAUSE       *}
{* ACTIONS CAN BE DIFFERENT FROM GAME TO GAME))                               *}
{******************************************************************************}

procedure TConfigurationForm.SaveConfig();
var
  iIniFile : TIniFile;
begin
  iIniFile := TIniFile.Create( ExtractFilePath(Application.ExeName) + 'Ini\Game.ini');

   //monitor
  iIniFile.WriteInteger('Monitor', 'MonitorId', MonitorComboBox.ItemIndex);
  iIniFile.WriteInteger('Monitor', 'ResolutionId', ResolutionsComboBox.ItemIndex);

  //map
  iIniFile.WriteString('Demo', 'SelectedMap', FMap);

  FreeAndNil(iIniFile);
end;

{******************************************************************************}
{* Fill Displays                                                              *}
{******************************************************************************}

procedure TConfigurationForm.FillDisplays();
var
  iI : Integer;
begin
  for iI := 0 to Screen.MonitorCount-1 do
  begin
    if Screen.Monitors[iI].Primary then
      MonitorComboBox.Items.Add('Monitor ' + IntToStr(iI+1) + ' (Primary)')
    else
      MonitorComboBox.Items.Add('Monitor ' + IntToStr(iI+1));

    SetLength(FMonitorInfos, Length(FMonitorInfos) + 1);
    FMonitorInfos[High(FMonitorInfos)].cbSize := SizeOf(TMonitorInfoEx);
    GetMonitorInfo(Screen.Monitors[iI].Handle, @FMonitorInfos[High(FMonitorInfos)]);
  end;
  MonitorComboBox.ItemIndex := 0;
end;

{******************************************************************************}
{* Fill Display Modi                                                          *}
{******************************************************************************}

procedure TConfigurationForm.FillDisplayModi();
var
 iDevMode: TDeviceMode;
 iModes: array of TDisplayMode;
 iModeIdx, iI: LongInt;
 iStr: string;
begin
  //fill resolutions combobox
  SetLength(iModes, 0);
  SetLength(FAvailableModi, 0);
  iModeIdx := 0;
  while EnumDisplaySettings( @FMonitorInfos[MonitorComboBox.ItemIndex].szDevice[0], iModeIdx, iDevMode) do
  begin
    if (iDevMode.dmBitsPerPel = 32) and (iDevMode.dmPelsWidth >= 800) and (iDevMode.dmPelsHeight >= 600) then
    begin
      SetLength(iModes, Length(iModes) + 1);
      with iModes[High(iModes)] do
      begin
        Width        := iDevMode.dmPelsWidth;
        Height       := iDevMode.dmPelsHeight;
      end;
    end;
    Inc(iModeIdx);
  end;

  ResolutionsComboBox.Items.Clear;
  for iI := Low(iModes) to High(iModes) do
  with iModes[iI] do
  begin
    iStr := IntToStr(Width) + ' x ' + IntToStr(Height);
    if ResolutionsComboBox.Items.IndexOf(iStr) < 0 then
    begin
      ResolutionsComboBox.Items.Add(iStr);
      SetLength(FAvailableModi, Length(FAvailableModi) + 1);
      FAvailableModi[High(FAvailableModi)] := iModes[iI];
    end;
  end;
  SetLength(iModes, 0);

  ResolutionsComboBox.ItemIndex := 0;
end;

end.
