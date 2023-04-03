unit uPlayer;

{$MODE Delphi}

interface

uses
  uGDEngine,
  SysUtils;

type

  TPlayer = class
  private
    FMovementSpeed : Double;
    FMovementMultiplier : Integer;
    FSinkSpeed : Double;
    FSinkMultiplier : Integer;
    FRunning        : boolean;
  public
    property  MovementSpeed : Double read FMovementSpeed write FMovementSpeed;
    property  MovementMultiplier : Integer read FMovementMultiplier write FMovementMultiplier;
    property  SinkSpeed : Double read FSinkSpeed write FSinkSpeed;
    property  SinkMultiplier : Integer read FSinkMultiplier write FSinkMultiplier;
    property  Running : Boolean read FRunning write FRunning;

    Constructor Create();
    Destructor  Destroy(); override;

    procedure MoveForward();
    procedure MoveBackWard();
    procedure MoveLeft();
    procedure MoveRight();

    Procedure Run();
    Procedure Walk();

    Function  PlayerInWater() : boolean;
    Function  PlayerUnderWater() : boolean;
    procedure DoPlayerCollisionAndPhysics();
  end;

implementation

uses
  uMain;

Constructor TPlayer.Create();
begin
  inherited;
  FMovementMultiplier := 400;
  FSinkMultiplier     := 50;
end;


Destructor TPlayer.Destroy();
begin
  inherited;
end;


procedure TPlayer.MoveForward();
begin
  GDCamera.Move( MovementSpeed );
end;


procedure TPlayer.MoveBackWard();
begin
  GDCamera.Move( -MovementSpeed );
end;


procedure TPlayer.MoveLeft();
begin
  GDCamera.Strafe(MovementSpeed);
end;


procedure TPlayer.MoveRight();
begin
  GDCamera.Strafe(-MovementSpeed);
end;


Procedure TPlayer.Run();
begin
  FRunning := true;
  if PlayerInWater() then
    FMovementMultiplier := 600
  else
    FMovementMultiplier := 800;

  if not(Clip) then FMovementMultiplier := 2000;
end;


Procedure TPlayer.Walk();
begin
  FRunning := false;
  if PlayerInWater() then
    FMovementMultiplier := 400
  else
    FMovementMultiplier := 600;

  if not(Clip) then FMovementMultiplier := 2000;
end;


Function TPlayer.PlayerInWater() : boolean;
begin
  if GDCamera.Position.y < GDMap.Water.WaterHeight + 100 then
    result := true
  else
    result := false;
end;


Function TPlayer.PlayerUnderWater() : boolean;
begin
  if GDCamera.Position.y < GDMap.Water.WaterHeight then
    result := true
  else
    result := false;
end;


procedure TPlayer.DoPlayerCollisionAndPhysics();
var
  iTerrainHeight : Double;
begin
  FMovementSpeed := FMovementMultiplier * GDTiming.FrameTime / 1000;
  FSinkSpeed     := FSinkMultiplier * GDTiming.FrameTime / 1000;

  If Not(Clip) then exit;

  if PlayerInWater() then
    GDCamera.Position.SetY(GDCamera.Position.y - FSinkSpeed);

  iTerrainHeight := 0;
  GDMap.Terrain.GetHeight(GDCamera.Position.X, GDCamera.Position.Z, iTerrainHeight);

  if iTerrainHeight > GDMap.Water.WaterHeight then
  begin
    GDCamera.Position.SetY(iTerrainHeight + 256);
    exit;
  end
  else
  begin
    if iTerrainHeight + 170 > GDMap.Water.WaterHeight then
    begin
      GDCamera.Position.SetY(iTerrainHeight + 256);
      exit;
    end;

    if GDCamera.Position.Y < iTerrainHeight+170 then
    begin
      GDCamera.Position.SetY(iTerrainHeight+170);
    end
    else
      if GDCamera.Position.Y > GDMap.Water.WaterHeight+96 then
      begin
        GDCamera.Position.SetY(GDMap.Water.WaterHeight+96);
      end;
  end;
End;

end.
