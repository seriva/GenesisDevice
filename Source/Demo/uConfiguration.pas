unit uConfiguration;

{$MODE Delphi}

interface

uses
  SysUtils,
  LCLIntf,
  LCLType,
  Forms,
  ComCtrls,
  ExtCtrls,
  Controls,
  StdCtrls,
  Classes,
  Dialogs,
  uGDConstants,
  uGDEngine,
  LazFileUtils;

type

  { TConfigurationForm }

  TConfigurationForm = class(TForm)
    DetailCheckbox: TCheckBox;
    FoliageDensityLabel: TLabel;
    FoliageDensityTrackBar: TTrackBar;
    FoliageDistanceTrackBar: TTrackBar;
    FullScreenCheckBox: TCheckBox;
    FXAACheckBox: TCheckBox;
    GammaLabel: TLabel;
    ScaleLabel: TLabel;
    ScaledLabel: TLabel;
    ScaledValueLabel: TLabel;
    ScalePanel: TPanel;
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
    ScaleTrackBar: TTrackBar;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FullScreenCheckBoxChange(Sender: TObject);
    procedure MonitorComboBoxChange(Sender: TObject);
    procedure RunButtonClick(Sender: TObject);
    procedure ScaleTrackBarChange(Sender: TObject);
  private
    procedure FillComboboxes();
    procedure FillDisplays();
  public
    procedure SettingsToInterface();
    procedure SettingsFromInterface();
  end;

function SettingsExecute: boolean;

implementation

function SettingsExecute: boolean;
var
  iSettings : TConfigurationForm;
begin
  try
    iSettings := TConfigurationForm.Create(nil);
    Result := iSettings.ShowModal = mrOk;
  finally
    FreeAndNil(iSettings);
  end;
end;

{$R *.lfm}

procedure TConfigurationForm.FormCreate(Sender: TObject);
begin
  FillComboboxes();
  GDSettings.Load();
  SettingsToInterface();
  ScalePanel.Visible := FullScreenCheckBox.Checked;
end;


procedure TConfigurationForm.FormDestroy(Sender: TObject);
begin
  SettingsFromInterface();
  GDSettings.Save();
end;


procedure TConfigurationForm.FormShow(Sender: TObject);
begin
  self.Left := 5;
  self.Top  := 5;
end;


procedure TConfigurationForm.FullScreenCheckBoxChange(Sender: TObject);
begin
  ScalePanel.Visible := FullScreenCheckBox.Checked;
end;


procedure TConfigurationForm.MonitorComboBoxChange(Sender: TObject);
begin
  ScaleTrackBarChange(nil);
end;


procedure TConfigurationForm.ScaleTrackBarChange(Sender: TObject);
var
  iM : TMonitor;
begin
  GDSettings.DisplayScale := ScaleTrackBar.Position / 100;
  iM :=  Screen.Monitors[GDSettings.Display];
  ScaledValueLabel.Caption := IntToStr(Round(iM.Width * GDSettings.DisplayScale)) +
                            ' x ' + IntToStr(Round(iM.Height * GDSettings.DisplayScale))
end;


procedure TConfigurationForm.FillComboboxes();
var
  iI: LongInt;
begin
  //fill monitors combobox
  FillDisplays();

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


procedure TConfigurationForm.SettingsToInterface();
var
  iI: Integer;
begin
  //window settings
  MonitorComboBox.ItemIndex := GDSettings.Display;
  ScaleTrackBar.Position    := Round(GDSettings.DisplayScale * 100);
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
  SoundVolumeTrackBar.Position := Round(100 * GDSettings.SoundVolume);

  ScaleTrackBarChange(nil);
end;


procedure TConfigurationForm.SettingsFromInterface();
begin
  //window settings
  GDSettings.Display          := MonitorComboBox.ItemIndex;
  GDSettings.FullScreen       := FullScreenCheckBox.Checked;
  GDSettings.VerticalSync     := VerticalSyncCheckBox.Checked;
  GDSettings.Gamma            := GammaTrackBar.Position / 100;
  GDSettings.DisplayScale     := ScaleTrackBar.Position / 100;

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


procedure TConfigurationForm.RunButtonClick(Sender: TObject);
begin
  SettingsFromInterface();
  GDSettings.Save();
  Visible := false;
  Repaint();
  ModalResult := mrOK;
end;


procedure TConfigurationForm.FillDisplays();
var
  iI : Integer;
begin
  for iI := 0 to Screen.MonitorCount-1 do
  begin
    if Screen.Monitors[iI].Primary then
      MonitorComboBox.Items.Add(IntToStr(iI) + ' - Primary')
    else
      MonitorComboBox.Items.Add(IntToStr(iI));
  end;
  MonitorComboBox.ItemIndex := 0;
end;

end.
