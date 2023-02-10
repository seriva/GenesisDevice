{*******************************************************************************
*                            Genesis Device Engine                             *
*                   Copyright © 2007-2022 Luuk van Venrooij                    *
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
unit uPlayer;

{******************************************************************************}
{* This units hold the playerobject for the game                              *}
{******************************************************************************}

interface

uses
  uGDEngine,
  SysUtils;

type

{******************************************************************************}
{* Player class                                                               *}
{******************************************************************************}

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

{******************************************************************************}
{* Create player class                                                        *}
{******************************************************************************}

Constructor TPlayer.Create();
begin
  inherited;
  FMovementMultiplier := 400;
  FSinkMultiplier     := 50;
end;

{******************************************************************************}
{* Destroy player class                                                       *}
{******************************************************************************}

Destructor TPlayer.Destroy();
begin
  inherited;
end;

{******************************************************************************}
{*  Move forwards                                                             *}
{******************************************************************************}

procedure TPlayer.MoveForward();
begin
  GDCamera.Move( MovementSpeed );
end;

{******************************************************************************}
{* Move backwards                                                             *}
{******************************************************************************}

procedure TPlayer.MoveBackWard();
begin
  GDCamera.Move( -MovementSpeed );
end;

{******************************************************************************}
{* Move left                                                                  *}
{******************************************************************************}

procedure TPlayer.MoveLeft();
begin
  GDCamera.Strafe(MovementSpeed);
end;

{******************************************************************************}
{* Move right                                                                 *}
{******************************************************************************}

procedure TPlayer.MoveRight();
begin
  GDCamera.Strafe(-MovementSpeed);
end;

{******************************************************************************}
{* Set player running mode                                                    *}
{******************************************************************************}

Procedure TPlayer.Run();
begin
  FRunning := true;
  if PlayerInWater() then
    FMovementMultiplier := 600
  else
    FMovementMultiplier := 800;

  if not(Clip) then FMovementMultiplier := 2000;
end;

{******************************************************************************}
{* Set player walking mode                                                    *}
{******************************************************************************}

Procedure TPlayer.Walk();
begin
  FRunning := false;
  if PlayerInWater() then
    FMovementMultiplier := 400
  else
    FMovementMultiplier := 600;

  if not(Clip) then FMovementMultiplier := 2000;
end;

{******************************************************************************}
{* Check if the player is in water                                            *}
{******************************************************************************}

Function TPlayer.PlayerInWater() : boolean;
begin
  if GDCamera.Position.y < GDMap.Water.WaterHeight + 100 then
    result := true
  else
    result := false;
end;

{******************************************************************************}
{* Check if the under water                                                   *}
{******************************************************************************}

Function TPlayer.PlayerUnderWater() : boolean;
begin
  if GDCamera.Position.y < GDMap.Water.WaterHeight then
    result := true
  else
    result := false;
end;

{******************************************************************************}
{* Do player collision and physics                                            *}
{******************************************************************************}

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
