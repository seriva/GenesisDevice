unit uGDMap;

{$MODE Delphi}

interface

uses
  SysUtils,
  JsonTools,
  uGDTerrain,
  uGDTypes,
  uGDFoliage,
  uGDConstants,
  uGDSkyDome,
  uGDWater,
  uGDCellManager,
  uGDStaticMeshCache,
  uGDMeshCell;

type
  TGDMap = class
  private
    FPlayerStart     : TGDVector;
    FPlayerViewAngle : TGDVector;

    FLightDirection  : TGDVector;
    FLightAmbient    : TGDColor;
    FLightDiffuse    : TGDColor;

    FFogDistance     : Integer;
    FFogColor        : TGDColor;
    FFogMinDistance  : double;
    FFogMaxDistance  : double;

    FTerrain         : TGDTerrain;
    FWater           : TGDWater;
    FFoliage         : TGDFoliage;
    FSkyDome         : TGDSkyDome;

    FCellManager     : TGDCellManager;
    FStaticMeshCache : TGDStaticMeshCache;
  public
    property PlayerStart : TGDVector read FPlayerStart;
    property PlayerViewAngle : TGDVector read FPlayerViewAngle;

    property LightDirection : TGDVector read FLightDirection;
    property LightAmbient   : TGDColor read FLightAmbient;
    property LightDiffuse   : TGDColor read FLightDiffuse;

    property FogDistance    : Integer read FFogDistance;
    property FogColor       : TGDColor read FFogColor;
    property FogMinDistance : double read FFogMinDistance;
    property FogMaxDistance : double read FFogMaxDistance;

    property Terrain     : TGDTerrain read FTerrain;
    property Water       : TGDWater read FWater;
    property Foliage     : TGDFoliage read FFoliage;
    property SkyDome     : TGDSkyDome read FSkyDome;
    property StaticMeshCache : TGDStaticMeshCache read FStaticMeshCache;

    constructor Create();
    destructor  Destroy(); override;

    procedure Load( aFileName : String );
    procedure Clear();

    function  ObjectCount(): integer;
    function  TriangleCount(): integer;

    procedure Update();
    procedure DetectVisibleCells();
    procedure RenderVisibleCells(aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor);
  end;

implementation

uses
  uGDEngine;

constructor TGDMap.Create();
begin
  inherited;
  FTerrain := TGDTerrain.Create();
  FWater   := TGDWater.Create();
  FFoliage := TGDFoliage.Create();
  FSkyDome := TGDSkyDome.Create();
  FCellManager := TGDCellManager.Create();
  FStaticMeshCache := TGDStaticMeshCache.Create();
end;


destructor  TGDMap.Destroy();
begin
  inherited;
  Clear();
  FreeAndNil(FTerrain);
  FreeAndNil(FWater);
  FreeAndNil(FFoliage);
  FreeAndNil(FSkyDome);
  FreeAndNil(FCellManager);
  FreeAndNil(FStaticMeshCache);
end;


procedure TGDMap.Load( aFileName : String );
var
  iMap, iModels, iModel : TJsonNode;
  iI            : Integer;
  iString       : String;
  iMeshInput    : TGDMeshCellInput;
begin
  GDTiming.Start();
  iMap := TJsonNode.Create();
  Clear();
  GDConsole.Write('......Loading map (' + aFileName + ')');
  GDGUI.LoadingScreen.Start('Loading ' + StringReplace( ExtractFileName(aFileName), ExtractFileExt(aFileName), '',  [rfReplaceAll] ) + '...', 6 );

  //load map json
  iMap.LoadFromFile(aFileName);

  //spawnpoint
  FPlayerStart.Reset(iMap.Find('SpawnPoint/Position'));
  FPlayerViewAngle.Reset(iMap.Find('SpawnPoint/ViewAngle'));

  //directional light
  FLightDirection.Reset(iMap.Find('Light/Direction'));
  FLightAmbient.Reset(iMap.Find('Light/Ambient'));
  FLightDiffuse.Reset(iMap.Find('Light/Diffuse'));

  //init fog
  FFogColor.Reset(iMap.Find('Fog/Color'));
  FFogDistance := GDSettings.ViewDistance;
  FFogMinDistance := (((FFogDistance * R_VIEW_DISTANCE_STEP) / 10) * 5);
  FFogMaxDistance := (((FFogDistance * R_VIEW_DISTANCE_STEP) / 10) * 7.5);
  GDGUI.LoadingScreen.Update();

  //init terrain
  FTerrain.InitTerrain(iMap.Find('Terrain'));
  GDGUI.LoadingScreen.Update();

  //init sky
  Skydome.InitSkyDome(iMap.Find('Sky'), (GDSettings.ViewDistance * R_VIEW_DISTANCE_STEP));
  GDGUI.LoadingScreen.Update();

  //foliage
  Foliage.InitFoliage( iMap.Find('Foliage') );
  GDGUI.LoadingScreen.Update();

  //init water
  Water.InitWater(FTerrain, iMap.Find('Water') );
  GDGUI.LoadingScreen.Update();

  //mesh entities
  iModels := iMap.Find('Models');
  for iI := 0 to iModels.Count-1 do
  begin
    iModel := iModels.Child(iI);
    iMeshInput.Model         := iModel.Find('Model').AsString; 
    iMeshInput.ModelLOD1     := iModel.Find('ModelLOD1').AsString;
    iMeshInput.ModelLOD2     := iModel.Find('ModelLOD2').AsString;
    iMeshInput.Position.Reset(iModel.Find('Position'));
    iMeshInput.Rotation.Reset(iModel.Find('Rotation'));
    iMeshInput.Scale.Reset(iModel.Find('Scale'));
    iMeshInput.FadeDistance  := 0;
    iMeshInput.FadeScale     := 0;
    iMeshInput.CastShadow    := iModel.Find('CastShadow').AsBoolean; 
    iMeshInput.ReceiveShadow := iModel.Find('ReceiveShadow').AsBoolean;
    FCellManager.AddMeshCell( TGDMeshCell.Create(iMeshInput)   );   
  end; 
  GDGUI.LoadingScreen.Update();

  GDTiming.Stop();
  FreeAndNil(iMap);
  GDConsole.Write('......Done loading map (' + GDTiming.TimeInSeconds + ' Sec)');

  FCellManager.GenerateCells(FTerrain, FWater, FFoliage);
  FStaticMeshCache.CreateBuffers();

  GDCamera.Position := FPlayerStart.Copy();
  GDCamera.Rotation := FPlayerViewAngle.Copy();
  GDCamera.MouseLook(0,0,1,1,0,False);
end;


procedure TGDMap.Clear();
begin
  FPlayerStart.Reset(0,0,0);
  FPlayerViewAngle.Reset(0,0,0);

  FLightDirection.Reset(-1,-1,-1);
  FLightAmbient.Reset(1, 1, 1, 1);
  FLightDiffuse.Reset(1, 1, 1, 1);

  FFogMinDistance := (((GDSettings.ViewDistance * R_VIEW_DISTANCE_STEP) / 10) *5);
  FFogMaxDistance := (((GDSettings.ViewDistance * R_VIEW_DISTANCE_STEP) / 10) *8);
  FFogColor.Reset( 0.5, 0.5, 0.5, 1.0 );
  FFogDistance := 5;

  FTerrain.Clear();
  FWater.Clear();
  FFoliage.Clear();
  FSkyDome.Clear();

  FCellManager.Clear();
  FStaticMeshCache.ClearCache();
  FStaticMeshCache.ClearBuffers();
end;


function TGDMap.ObjectCount(): integer;
begin
  result := FCellManager.ObjectCount();
end;


function TGDMap.TriangleCount(): integer;
begin
  result := FCellManager.TriangleCount + FSkyDome.TriangleCount;
end;


procedure TGDMap.Update();
begin
  FWater.Update();
end;


procedure TGDMap.DetectVisibleCells();
begin
  FCellManager.DetectVisibleCells();
end;


procedure TGDMap.RenderVisibleCells(aRenderAttribute : TGDRenderAttribute; aRenderFor : TGDRenderFor);
begin
  FCellManager.RenderVisibleCells(aRenderAttribute, aRenderFor, FTerrain, FWater, FFoliage, FStaticMeshCache);
end;

end.
