  unit uGDSettings;

{$MODE Delphi}

interface

uses
  SysUtils,
  IniFiles,
  dglOpenGL,
  uGDConstants,
  FileUtil;

type
  TGDSettings = class
  private
    //window settings
    FDisplay	      : Integer;
    FDisplayScale     : Single;
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
    FUseFXAA          : Boolean;
    FUseShadows       : Boolean;
    FUseSSAO          : Boolean;
    FUseDetail        : Boolean;

    //input settings
    FInvertMouse      : Boolean;
    FMouseSensitivity : Integer;

    //sound settings
    FMuteSound   : boolean;
    FSoundVolume : Double;

    //Model global LOD settings.
    FLOD0, FLOD1, FLOD2 : Single;

    procedure SetViewDistance(aDistance : Integer);
  public
    procedure SetTextureDetail(aStr : String);
    function  GetTextureDetail() : String;
    procedure SetWaterDetail(aStr : String);
    function  GetWaterDetail() : String;
    procedure SetTextureFilter(aStr : String);
    function  GetTextureFilter() : String;

    //window settings
    property Display : Integer read FDisplay write FDisplay;
    property DisplayScale : Single read FDisplayScale write FDisplayScale;
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
    property UseFXAA : Boolean read FUseFXAA write FUseFXAA;
    property UseShadows : Boolean read FUseShadows write FUseShadows;
    property UseSSAO : Boolean read FUseSSAO write FUseSSAO;

    property UseDetail : Boolean read FUseDetail write FUseDetail;

    //input settings
    property InvertMouse : Boolean read FInvertMouse write FInvertMouse;
    property MouseSensitivity : Integer read FMouseSensitivity write FMouseSensitivity;

    //sound settings
    property MuteSound : Boolean read FMuteSound write FMuteSound;
    property SoundVolume : Double read FSoundVolume write FSoundVolume;

    constructor Create();
    destructor  Destroy(); override;

    procedure Load();
    procedure Save();
  end;

implementation

uses
  uGDEngine,
  uGDConsole;

constructor TGDSettings.Create();
begin
  //window settings
  FDisplay	    := 0;
  FDisplayScale     := 1.0;
  FFullScreen       := false;
  FVerticalSync     := false;
  FGamma            := 0.50;

  //renderer
  ViewDistance      := 5;
  FFoliageDistance  := 1;
  FFoliageDensity   := 10;
  FTextureDetail    := TD_LOW;
  FWaterDetail      := WD_LOW;
  FTextureFilter    := TF_BILINEAR;
  FUseFXAA          := false;
  FUseShadows       := false;
  FUseDetail        := false;

  //input
  FInvertMouse      := False;
  FMouseSensitivity := 5;

  //sound
  FMuteSound   := false;
  FSoundVolume := 0.5;

  //console commands
  GDConsole.AddCommand('RFXAA', '0,1 : Enable or disable FXAA', CT_BOOLEAN, @FUseFXAA);
  GDConsole.AddCommand('RVSync', '0,1 : Enable or disable vertical sync', CT_BOOLEAN, @FVerticalSync);
  GDConsole.AddCommand('RGamma', '0.0 to 1.0 : Set the gamma value', CT_FLOAT, @FGamma);
  GDConsole.AddCommand('RShadows', '0,1 : Enable or disable shadows', CT_BOOLEAN, @FUseShadows);
  GDConsole.AddCommand('RSSAO', '0,1 : Enable or disable SSAO', CT_BOOLEAN, @FUseSSAO);
  GDConsole.AddCommand('RDetail', '0,1 : Enable or disable detail texturing', CT_BOOLEAN, @FUseDetail);
end;


destructor  TGDSettings.Destroy();
begin
  inherited;
end;


procedure TGDSettings.Load();
var
  iIniFile : TIniFile;
begin
  iIniFile := TIniFile.Create(ENGINE_INI);

  //window settings
  FDisplay :=      iIniFile.ReadInteger('Window', 'Display', 0);
  FDisplayScale := iIniFile.ReadFloat('Window', 'DisplayScale', 1.0);
  FFullScreen :=   iIniFile.ReadBool('Window', 'Fullscreen', False);
  FVerticalSync := iIniFile.ReadBool('Window', 'VerticalSync', False);
  FGamma :=        iIniFile.ReadFloat('Window', 'Gamma', 0.60);

  //render settings
  ViewDistance     := iIniFile.ReadInteger('Renderer', 'ViewDistance', 5);
  FFoliageDistance := iIniFile.ReadInteger('Renderer', 'FoliageDistance', 1);
  FFoliageDensity  := iIniFile.ReadInteger('Renderer',  'FoliageDensity', 5);
  SetTextureDetail(iIniFile.ReadString('Renderer', 'TextureDetail', TGDTextureDetailStrings[1]));
  SetTextureFilter(iIniFile.ReadString('Renderer', 'TextureFilter', TGDTextureFilterStrings[1]));
  SetWaterDetail(iIniFile.ReadString('Renderer', 'WaterDetail', TGDWaterDetailStrings[1]));
  FUseFXAA          := iIniFile.ReadBool( 'Renderer', 'UseFXAA', False);
  FUseShadows       := iIniFile.ReadBool( 'Renderer', 'UseShadows', False);
  FUseSSAO          := iIniFile.ReadBool( 'Renderer', 'UseSSAO', False);
  FUseDetail        := iIniFile.ReadBool( 'Renderer', 'UseDetail', False);

  //input settings
  FInvertMouse      := iIniFile.ReadBool( 'Controls', 'InvertMouse', False);
  FMouseSensitivity := iIniFile.ReadInteger('Controls', 'MouseSensitivity', 5);

  //sound settings
  FMuteSound   := iIniFile.ReadBool( 'Sound', 'Mute', False);
  FSoundVolume := iIniFile.ReadFloat( 'Sound', 'SoundVolume', 0.5);
  
  FreeAndNil(iIniFile);
  Save();
end;


procedure TGDSettings.Save();
var
  iIniFile : TIniFile;
begin
  iIniFile := TIniFile.Create(ENGINE_INI);

  //window
  iIniFile.WriteInteger('Window', 'Display', FDisplay);
  iIniFile.WriteFloat('Window', 'DisplayScale', DisplayScale);
  iIniFile.WriteBool('Window', 'Fullscreen', FFullScreen);
  iIniFile.WriteBool('Window', 'VerticalSync', FVerticalSync);
  iIniFile.WriteFloat('Window', 'Gamma', FGamma);

  //render settings
  iIniFile.WriteInteger('Renderer', 'ViewDistance', FViewDistance);
  iIniFile.WriteInteger('Renderer', 'FoliageDistance', FFoliageDistance);
  iIniFile.WriteInteger('Renderer', 'FoliageDensity', FFoliageDensity);
  iIniFile.WriteString('Renderer', 'TextureDetail', GetTextureDetail());
  iIniFile.WriteString('Renderer', 'TextureFilter', GetTextureFilter());
  iIniFile.WriteString('Renderer', 'WaterDetail', GetWaterDetail());
  iIniFile.WriteBool( 'Renderer', 'UseFXAA', FUseFXAA );
  iIniFile.WriteBool( 'Renderer', 'UseShadows', FUseShadows );
  iIniFile.WriteBool( 'Renderer', 'UseSSAO', FUseSSAO );
  iIniFile.WriteBool( 'Renderer', 'UseDetail', FUseDetail );

  //input settings
  iIniFile.WriteBool( 'Controls', 'InvertMouse', FInvertMouse);
  iIniFile.WriteInteger('Controls', 'MouseSensitivity', FMouseSensitivity);

  //sound settings
  iIniFile.WriteBool( 'Sound', 'Mute', FMuteSound);
  iIniFile.WriteFloat( 'Sound', 'SoundVolume', FSoundVolume);

  FreeAndNil(iIniFile)
end;


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


procedure TGDSettings.SetTextureDetail(aStr : String);
var
  iI : Integer;
begin
  for iI := 1 to 3 do
    If  aStr = TGDTextureDetailStrings[iI] then FTextureDetail := TGDTextureDetail(iI);
end;


function TGDSettings.GetTextureDetail() : String;
begin
  result := TGDTextureDetailStrings[ Integer(FTextureDetail) ];
end;


procedure TGDSettings.SetTextureFilter(aStr : String);
var
  iI : Integer;
begin
  for iI := 1 to 6 do
    If  aStr = TGDTextureFilterStrings[iI] then FTextureFilter := TGDTextureFilter(iI);
end;


function TGDSettings.GetTextureFilter() : String;
begin
  result := TGDTextureFilterStrings[ Integer(FTextureFilter) ];
end;


procedure TGDSettings.SetWaterDetail(aStr : String);
var
  iI : Integer;
begin
  for iI := 1 to 3 do
    If  aStr = TGDWaterDetailStrings[iI] then FWaterDetail := TGDWaterDetail(iI);
end;


function TGDSettings.GetWaterDetail() : String;
begin
  result := TGDWaterDetailStrings[ Integer(FWaterDetail) ];
end;

end.
